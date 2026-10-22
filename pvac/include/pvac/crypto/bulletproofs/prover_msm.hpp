// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <algorithm>
#include <array>
#include <cstdint>
#include <limits>
#include <stdexcept>
#include <vector>
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

struct ProverPoint {
    Fe25519 minus, plus, product;
};

inline ProverPoint prover_point(const RistrettoPoint& bytes) {
    const auto point = rist_decode_or_throw(bytes);
    return {
        fe_sub(point.Y, point.X),
        fe_add(point.Y, point.X),
        fe_mul(point.T, ed_2d())
    };
}

inline ExtPoint prover_add(const ExtPoint& point, const ProverPoint& added) {
    const auto a = fe_mul(fe_sub(point.Y, point.X), added.minus);
    const auto b = fe_mul(fe_add(point.Y, point.X), added.plus);
    const auto c = fe_mul(point.T, added.product);
    const auto d = fe_add(point.Z, point.Z);
    const auto e = fe_sub(b, a);
    const auto f = fe_sub(d, c);
    const auto g = fe_add(d, c);
    const auto h = fe_add(b, a);
    return {fe_mul(e, f), fe_mul(g, h), fe_mul(f, g), fe_mul(e, h)};
}

inline size_t prover_window(size_t count) {
    size_t width = 1;
    size_t score = std::numeric_limits<size_t>::max();
    for (size_t bits = 1; bits <= 16; ++bits) {
        const size_t buckets = (size_t{1} << bits) - 1;
        if (buckets > (256 * 1024) / sizeof(ExtPoint)) break;
        const size_t windows = (256 + bits - 1) / bits;
        if (count > std::numeric_limits<size_t>::max() / windows - 2 * buckets)
            continue;
        const size_t work = windows * (count + 2 * buckets);
        if (work < score) {
            width = bits;
            score = work;
        }
    }
    return width;
}

inline RistrettoPoint prover_msm(
    const std::vector<Scalar>& scalars,
    const std::vector<RistrettoPoint>& points
) {
    if (scalars.size() != points.size())
        throw std::runtime_error("pvac: scalar and point counts differ");
    const size_t count = scalars.size();
    if (count == 0) return rist_identity();
    if (count == 1)
        return rist_encode(ext_scalarmul(rist_decode_or_throw(points[0]), scalars[0]));
    if (count == 2) {
        const auto left = rist_decode_or_throw(points[0]);
        const auto right = rist_decode_or_throw(points[1]);
        std::array<std::array<uint8_t, 32>, 2> bytes;
        for (size_t i = 0; i < 2; ++i) sc_tobytes(bytes[i].data(), scalars[i]);
        auto result = ext_identity();
        for (size_t bit = 256; bit > 0; --bit) {
            result = ext_double(result);
            const size_t index = bit - 1;
            if ((bytes[0][index / 8] >> (index % 8)) & 1)
                result = ext_add(result, left);
            if ((bytes[1][index / 8] >> (index % 8)) & 1)
                result = ext_add(result, right);
        }
        return rist_encode(result);
    }
    std::vector<ProverPoint> decoded;
    decoded.reserve(count);
    for (const auto& point : points) decoded.push_back(prover_point(point));
    std::vector<std::array<uint8_t, 32>> bytes(count);
    for (size_t i = 0; i < count; ++i) sc_tobytes(bytes[i].data(), scalars[i]);
    const size_t width = prover_window(count);
    const size_t mask = (size_t{1} << width) - 1;
    const size_t windows = (256 + width - 1) / width;
    std::vector<ExtPoint> buckets(mask);
    auto result = ext_identity();
    for (size_t window = windows; window > 0; --window) {
        for (size_t i = 0; i < width; ++i) result = ext_double(result);
        std::fill(buckets.begin(), buckets.end(), ext_identity());
        const size_t bit = (window - 1) * width;
        const size_t byte = bit / 8;
        for (size_t i = 0; i < count; ++i) {
            uint32_t word = 0;
            for (size_t j = 0; j < 3 && byte + j < 32; ++j)
                word |= uint32_t{bytes[i][byte + j]} << (8 * j);
            const size_t digit = (word >> (bit % 8)) & mask;
            if (digit != 0) buckets[digit - 1] = prover_add(buckets[digit - 1], decoded[i]);
        }
        auto sum = ext_identity();
        auto total = ext_identity();
        for (size_t i = buckets.size(); i > 0; --i) {
            sum = ext_add(sum, buckets[i - 1]);
            total = ext_add(total, sum);
        }
        result = ext_add(result, total);
    }
    return rist_encode(result);
}

}
}