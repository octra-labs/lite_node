// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <cassert>
#include "transcript.hpp"
#include "generators.hpp"
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

struct InnerProductProof {
    std::vector<RistrettoPoint> L;
    std::vector<RistrettoPoint> R;
    Scalar a;
    Scalar b;
};

inline RistrettoPoint multi_scalar_mul(
    const std::vector<Scalar>& scalars,
    const std::vector<RistrettoPoint>& points
) {
    assert(scalars.size() == points.size());
    size_t n = scalars.size();
    if (n == 0) return rist_identity();

    std::vector<ExtPoint> pts(n);
    for (size_t i = 0; i < n; i++)
        pts[i] = rist_decode_or_throw(points[i]);

    std::vector<std::array<uint8_t, 32>> sbytes(n);
    for (size_t i = 0; i < n; i++)
        sc_tobytes(sbytes[i].data(), scalars[i]);

    if (n <= 2) {
        ExtPoint acc = ext_identity();
        for (size_t i = 0; i < n; i++)
            acc = ext_add(acc, ext_scalarmul(pts[i], scalars[i]));
        return rist_encode(acc);
    }

    size_t w = 1;
    { size_t tmp = n; while (tmp >>= 1) w++; }
    if (w > 16) w = 16;

    size_t num_buckets = (1ULL << w) - 1;

    size_t num_windows = (256 + w - 1) / w;

    ExtPoint result = ext_identity();

    for (size_t win_idx = num_windows; win_idx > 0; win_idx--) {

        for (size_t d = 0; d < w; d++)
            result = ext_double(result);

        std::vector<ExtPoint> buckets(num_buckets, ext_identity());

        size_t bit_start = (win_idx - 1) * w;

        for (size_t i = 0; i < n; i++) {

            uint32_t bucket_idx = 0;
            for (size_t b = 0; b < w; b++) {
                size_t bit = bit_start + b;
                if (bit >= 256) break;
                size_t byte_idx = bit >> 3;
                size_t bit_in_byte = bit & 7;
                if ((sbytes[i][byte_idx] >> bit_in_byte) & 1)
                    bucket_idx |= (1U << b);
            }

            if (bucket_idx == 0) continue;
            buckets[bucket_idx - 1] = ext_add(buckets[bucket_idx - 1], pts[i]);
        }

        ExtPoint running_sum = ext_identity();
        ExtPoint window_sum = ext_identity();

        for (size_t k = num_buckets; k > 0; k--) {
            running_sum = ext_add(running_sum, buckets[k - 1]);
            window_sum = ext_add(window_sum, running_sum);
        }

        result = ext_add(result, window_sum);
    }

    return rist_encode(result);
}

inline Scalar inner_product(const std::vector<Scalar>& a, const std::vector<Scalar>& b) {
    assert(a.size() == b.size());
    Scalar result = sc_zero();
    for (size_t i = 0; i < a.size(); i++)
        result = sc_add(result, sc_mul(a[i], b[i]));
    return result;
}

inline InnerProductProof ipp_prove(
    Transcript& transcript,
    const RistrettoPoint& Q,
    std::vector<Scalar> a,
    std::vector<Scalar> b,
    std::vector<RistrettoPoint> G_vec,
    std::vector<RistrettoPoint> H_vec
) {
    size_t n = a.size();
    assert(n == b.size());
    assert(n == G_vec.size());
    assert(n == H_vec.size());
    assert(n > 0 && (n & (n - 1)) == 0);

    size_t lg = log2_size(n);
    InnerProductProof proof;
    proof.L.resize(lg);
    proof.R.resize(lg);

    for (size_t k = 0; k < lg; k++) {
        size_t half = n / 2;

        Scalar c_L = sc_zero();
        Scalar c_R = sc_zero();
        for (size_t i = 0; i < half; i++) {
            c_L = sc_add(c_L, sc_mul(a[i], b[half + i]));
            c_R = sc_add(c_R, sc_mul(a[half + i], b[i]));
        }

        {
            std::vector<Scalar> scalars;
            std::vector<RistrettoPoint> points;
            scalars.reserve(n + 1);
            points.reserve(n + 1);

            for (size_t i = 0; i < half; i++) {
                scalars.push_back(a[i]);
                points.push_back(G_vec[half + i]);
            }
            for (size_t i = 0; i < half; i++) {
                scalars.push_back(b[half + i]);
                points.push_back(H_vec[i]);
            }
            scalars.push_back(c_L);
            points.push_back(Q);

            proof.L[k] = multi_scalar_mul(scalars, points);
        }

        {
            std::vector<Scalar> scalars;
            std::vector<RistrettoPoint> points;
            scalars.reserve(n + 1);
            points.reserve(n + 1);

            for (size_t i = 0; i < half; i++) {
                scalars.push_back(a[half + i]);
                points.push_back(G_vec[i]);
            }
            for (size_t i = 0; i < half; i++) {
                scalars.push_back(b[i]);
                points.push_back(H_vec[half + i]);
            }
            scalars.push_back(c_R);
            points.push_back(Q);

            proof.R[k] = multi_scalar_mul(scalars, points);
        }

        transcript.append_point("L", proof.L[k]);
        transcript.append_point("R", proof.R[k]);
        Scalar u_k = transcript.challenge_scalar("u");
        Scalar u_k_inv = sc_inv(u_k);

        std::vector<Scalar> a_new(half);
        std::vector<Scalar> b_new(half);
        std::vector<RistrettoPoint> G_new(half);
        std::vector<RistrettoPoint> H_new(half);

        for (size_t i = 0; i < half; i++) {
            a_new[i] = sc_add(sc_mul(a[i], u_k), sc_mul(a[half + i], u_k_inv));
            b_new[i] = sc_add(sc_mul(b[i], u_k_inv), sc_mul(b[half + i], u_k));

            G_new[i] = multi_scalar_mul(
                {u_k_inv, u_k},
                {G_vec[i], G_vec[half + i]}
            );

            H_new[i] = multi_scalar_mul(
                {u_k, u_k_inv},
                {H_vec[i], H_vec[half + i]}
            );
        }

        a = std::move(a_new);
        b = std::move(b_new);
        G_vec = std::move(G_new);
        H_vec = std::move(H_new);
        n = half;
    }

    assert(a.size() == 1 && b.size() == 1);
    proof.a = a[0];
    proof.b = b[0];
    return proof;
}

inline std::vector<Scalar> ipp_verification_scalars(
    const std::vector<Scalar>& challenges
) {
    size_t lg = challenges.size();
    size_t n = 1ULL << lg;

    std::vector<Scalar> u_inv(lg);
    for (size_t k = 0; k < lg; k++)
        u_inv[k] = sc_inv(challenges[k]);

    std::vector<Scalar> s(n);
    s[0] = Scalar{{1, 0, 0, 0}};
    for (size_t k = 0; k < lg; k++)
        s[0] = sc_mul(s[0], u_inv[k]);

    for (size_t i = 1; i < n; i++) {

        size_t k = 0;
        size_t tmp = i;
        while (tmp >>= 1) k++;

        size_t challenge_idx = lg - 1 - k;
        Scalar u_sq = sc_mul(challenges[challenge_idx], challenges[challenge_idx]);
        s[i] = sc_mul(s[i ^ (1ULL << k)], u_sq);
    }

    return s;
}

inline std::vector<Scalar> batch_sc_inv(const std::vector<Scalar>& vals) {
    size_t n = vals.size();
    if (n == 0) return {};

    std::vector<Scalar> prefix(n);
    prefix[0] = vals[0];
    for (size_t i = 1; i < n; i++)
        prefix[i] = sc_mul(prefix[i-1], vals[i]);

    Scalar inv_all = sc_inv(prefix[n-1]);

    std::vector<Scalar> result(n);
    for (size_t i = n - 1; i > 0; i--) {
        result[i] = sc_mul(inv_all, prefix[i-1]);
        inv_all = sc_mul(inv_all, vals[i]);
    }
    result[0] = inv_all;

    return result;
}

inline bool ipp_verify(
    Transcript& transcript,
    const RistrettoPoint& P,
    const RistrettoPoint& Q,
    const InnerProductProof& proof,
    size_t n
) {
    size_t lg = proof.L.size();
    if (proof.R.size() != lg) return false;
    if ((1ULL << lg) != n) return false;

    std::vector<Scalar> challenges(lg);
    for (size_t k = 0; k < lg; k++) {
        transcript.append_point("L", proof.L[k]);
        transcript.append_point("R", proof.R[k]);
        challenges[k] = transcript.challenge_scalar("u");
    }

    auto s = ipp_verification_scalars(challenges);

    auto s_inv = batch_sc_inv(s);

    std::vector<Scalar> scalars;
    std::vector<RistrettoPoint> points;
    scalars.reserve(2 * n + 2 * lg + 2);
    points.reserve(2 * n + 2 * lg + 2);

    Scalar ab = sc_mul(proof.a, proof.b);

    const auto& gen = generators();
    for (size_t i = 0; i < n; i++) {
        scalars.push_back(sc_mul(proof.a, s[i]));
        points.push_back(gen.G(i));
    }

    for (size_t i = 0; i < n; i++) {
        scalars.push_back(sc_mul(proof.b, s_inv[i]));
        points.push_back(gen.H(i));
    }

    scalars.push_back(ab);
    points.push_back(Q);

    std::vector<Scalar> u_sqs(lg);
    for (size_t k = 0; k < lg; k++)
        u_sqs[k] = sc_mul(challenges[k], challenges[k]);
    auto u_inv_sqs = batch_sc_inv(u_sqs);

    for (size_t k = 0; k < lg; k++) {
        scalars.push_back(sc_neg(u_sqs[k]));
        points.push_back(proof.L[k]);

        scalars.push_back(sc_neg(u_inv_sqs[k]));
        points.push_back(proof.R[k]);
    }

    RistrettoPoint expected = multi_scalar_mul(scalars, points);
    return expected == P;
}

}
}