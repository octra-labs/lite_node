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

inline std::array<int8_t, 257> prover_digits(const Scalar& scalar) {
    std::array<uint64_t, 5> words{scalar.v[0], scalar.v[1], scalar.v[2], scalar.v[3], 0};
    std::array<int8_t, 257> digits{};
    for (size_t bit = 0; bit < digits.size(); ++bit) {
        if ((words[0] & 1) != 0) {
            const int low = static_cast<int>(words[0] & 31);
            const int digit = low > 16 ? low - 32 : low;
            digits[bit] = static_cast<int8_t>(digit);
            if (digit > 0) {
                words[0] -= static_cast<uint64_t>(digit);
            } else {
                uint64_t carry = static_cast<uint64_t>(-digit);
                for (size_t i = 0; i < words.size() && carry != 0; ++i) {
                    const auto next = words[i] + carry;
                    carry = next < words[i] ? 1 : 0;
                    words[i] = next;
                }
            }
        }
        for (size_t i = 0; i + 1 < words.size(); ++i)
            words[i] = (words[i] >> 1) | (words[i + 1] << 63);
        words.back() >>= 1;
    }
    return digits;
}

struct ProverTerm {
    Fe25519 minus, plus, product, z;
};

inline std::array<ProverTerm, 8> prover_terms(const ExtPoint& point) {
    std::array<ProverTerm, 8> terms;
    const auto twice = ext_double(point);
    auto odd = point;
    for (size_t i = 0; i < terms.size(); ++i) {
        terms[i] = {fe_sub(odd.Y, odd.X), fe_add(odd.Y, odd.X),
            fe_mul(odd.T, ed_2d()), fe_add(odd.Z, odd.Z)};
        if (i + 1 < terms.size()) odd = ext_add(odd, twice);
    }
    return terms;
}

inline ExtPoint prover_term_add(
    const ExtPoint& point, const ProverTerm& term, bool negative
) {
    const auto a = fe_mul(fe_sub(point.Y, point.X), negative ? term.plus : term.minus);
    const auto b = fe_mul(fe_add(point.Y, point.X), negative ? term.minus : term.plus);
    const auto c = fe_mul(point.T, negative ? fe_neg(term.product) : term.product);
    const auto d = fe_mul(point.Z, term.z);
    const auto e = fe_sub(b, a), f = fe_sub(d, c);
    const auto g = fe_add(d, c), h = fe_add(b, a);
    return {fe_mul(e, f), fe_mul(g, h), fe_mul(f, g), fe_mul(e, h)};
}

inline ExtPoint prover_pair(
    const ExtPoint& left, const Scalar& a,
    const ExtPoint& right, const Scalar& b
) {
    const auto a_digits = prover_digits(a), b_digits = prover_digits(b);
    const auto left_table = prover_terms(left), right_table = prover_terms(right);
    auto result = ext_identity();
    for (size_t bit = a_digits.size(); bit > 0; --bit) {
        result = ext_double(result);
        const int x = a_digits[bit - 1], y = b_digits[bit - 1];
        if (x != 0)
            result = prover_term_add(result, left_table[(x < 0 ? -x : x) / 2], x < 0);
        if (y != 0)
            result = prover_term_add(result, right_table[(y < 0 ? -y : y) / 2], y < 0);
    }
    return result;
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
        return rist_encode(prover_pair(left, scalars[0], right, scalars[1]));
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