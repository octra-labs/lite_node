// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <atomic>
#include <memory>
#include "generators.hpp"
#include "inner_product.hpp"

namespace pvac {
namespace bp {

class ReadyPoints {
    const std::vector<ProverPoint> g_;
    const std::vector<ProverPoint> h_;

    static std::vector<ProverPoint> decode(const std::vector<RistrettoPoint>& bytes) {
        std::vector<ProverPoint> points(bytes.size());
        generator_run(0, bytes.size(), [&](size_t first, size_t last) {
            for (size_t i = first; i < last; ++i) points[i] = prover_point(bytes[i]);
        });
        return points;
    }

    explicit ReadyPoints(size_t n)
        : g_(decode(generators().G_vec(n))), h_(decode(generators().H_vec(n))) {}

    friend class ReadyTable;

public:
    size_t size() const { return g_.size(); }
    const ProverPoint& G(size_t i) const { return g_.at(i); }
    const ProverPoint& H(size_t i) const { return h_.at(i); }
};

class ReadyTable {
    std::mutex build_;
    std::shared_ptr<const ReadyPoints> points_;
    std::atomic<size_t> sums_{0};
    std::atomic<size_t> rejected_{0};

public:
    void build(size_t n) {
        if (n == 0 || n > BP_MAX_VECTOR_SIZE)
            throw std::runtime_error("pvac: ready size rejected");
        std::lock_guard<std::mutex> lock(build_);
        if (std::atomic_load(&points_))
            throw std::runtime_error("pvac: ready table already exists");
        std::shared_ptr<const ReadyPoints> points(new ReadyPoints(n));
        std::atomic_store(&points_, std::move(points));
    }

    std::shared_ptr<const ReadyPoints> get(size_t n) const {
        auto points = std::atomic_load(&points_);
        return points && n > 0 && n <= points->size() ? points : nullptr;
    }

    size_t size() const {
        const auto points = std::atomic_load(&points_);
        return points ? points->size() : 0;
    }

    size_t sums() const { return sums_.load(std::memory_order_relaxed); }
    void summed() { sums_.fetch_add(1, std::memory_order_relaxed); }
    size_t rejected() const { return rejected_.load(std::memory_order_relaxed); }
    void reject() { rejected_.fetch_add(1, std::memory_order_relaxed); }
};

inline ReadyTable& ready_table() {
    static ReadyTable table;
    return table;
}

inline size_t ready_width(size_t count) {
    size_t width = 1;
    while (count >>= 1) ++width;
    return std::min(width, size_t{16});
}

inline RistrettoPoint ready_sum(
    const ReadyPoints& points,
    const std::vector<Scalar>& scalars,
    size_t n,
    const std::vector<RistrettoPoint>& rest
) {
    if (n == 0 || n > points.size() || rest.size() > scalars.size()
        || scalars.size() - rest.size() != 2 * n || scalars.size() < 3)
        throw std::runtime_error("pvac: ready point count rejected");
    std::vector<ProverPoint> decoded;
    decoded.reserve(rest.size());
    for (const auto& point : rest) decoded.push_back(prover_point(point));
    std::vector<std::array<uint8_t, 32>> bytes(scalars.size());
    for (size_t i = 0; i < scalars.size(); ++i) sc_tobytes(bytes[i].data(), scalars[i]);
    const size_t width = ready_width(scalars.size());
    const size_t count = (size_t{1} << width) - 1;
    const size_t windows = (256 + width - 1) / width;
    std::vector<ExtPoint> buckets(count);
    ExtPoint result = ext_identity();
    for (size_t window = windows; window > 0; --window) {
        for (size_t bit = 0; bit < width; ++bit) result = ext_double(result);
        std::fill(buckets.begin(), buckets.end(), ext_identity());
        const size_t bit = (window - 1) * width;
        for (size_t i = 0; i < scalars.size(); ++i) {
            const size_t index = msm_digit(bytes[i], bit, width);
            if (index == 0) continue;
            const auto& point = i < n ? points.G(i)
                : i < 2 * n ? points.H(i - n) : decoded[i - 2 * n];
            buckets[index - 1] = prover_add(buckets[index - 1], point);
        }
        ExtPoint running = ext_identity();
        ExtPoint sum = ext_identity();
        for (size_t i = count; i > 0; --i) {
            running = ext_add(running, buckets[i - 1]);
            sum = ext_add(sum, running);
        }
        result = ext_add(result, sum);
    }
    const auto encoded = rist_encode(result);
    ready_table().summed();
    return encoded;
}

inline std::array<uint8_t, 32> ready_digest(const ReadyPoints& points) {
    Sha256 digest;
    digest.init();
    constexpr char domain[] = "pvac.ready.points";
    digest.update(reinterpret_cast<const uint8_t*>(domain), sizeof(domain) - 1);
    uint8_t size[8];
    for (size_t i = 0; i < 8; ++i) size[i] = uint8_t(uint64_t{points.size()} >> (8 * i));
    digest.update(size, sizeof(size));
    for (bool second : {false, true}) {
        for (size_t i = 0; i < points.size(); ++i) {
            const auto& point = second ? points.H(i) : points.G(i);
            for (const auto& field : {point.minus, point.plus, point.product}) {
                uint8_t bytes[32];
                fe_tobytes(bytes, field);
                digest.update(bytes, sizeof(bytes));
            }
        }
    }
    std::array<uint8_t, 32> result;
    digest.finish(result.data());
    return result;
}

inline constexpr size_t ready_capacity = 1048576;

inline bool ready_check() {
    const auto points = ready_table().get(ready_capacity);
    if (!points || points->size() != ready_capacity) return false;
    constexpr std::array<uint8_t, 32> expected{
        0xec, 0x99, 0x23, 0xec, 0x13, 0x41, 0x51, 0xb1,
        0x33, 0x5d, 0xcb, 0xda, 0xe7, 0x82, 0x49, 0x0e,
        0xa2, 0x24, 0x4f, 0xf2, 0xe2, 0x3d, 0x07, 0xa2,
        0x59, 0xb8, 0xb5, 0xa3, 0x4c, 0x79, 0x2f, 0xd0
    };
    return ready_digest(*points) == expected;
}

}
}