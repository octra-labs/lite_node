// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <cassert>
#include "transcript.hpp"
#include "generators.hpp"
#include "prover_msm.hpp"
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

struct InnerProductProof {
    std::vector<RistrettoPoint> L;
    std::vector<RistrettoPoint> R;
    Scalar a;
    Scalar b;
};

inline size_t msm_digit(const std::array<uint8_t, 32>& bytes, size_t bit, size_t width) {
    if (bit >= 256 || width == 0 || width > 16)
        throw std::runtime_error("pvac: scalar window rejected");
    const size_t byte = bit / 8;
    uint32_t word = 0;
    for (size_t index = 0; index < 3 && byte + index < bytes.size(); ++index)
        word |= uint32_t{bytes[byte + index]} << (8 * index);
    return (word >> (bit % 8)) & ((size_t{1} << width) - 1);
}

inline RistrettoPoint multi_scalar_mul(
    const std::vector<Scalar>& scalars,
    const std::vector<RistrettoPoint>& points
) {
    if (scalars.size() != points.size())
        throw std::runtime_error("pvac: scalar and point counts differ");
    size_t n = scalars.size();
    if (n == 0) return rist_identity();

    if (n <= 2) {
        std::vector<ExtPoint> pts(n);
        for (size_t i = 0; i < n; i++)
            pts[i] = rist_decode_or_throw(points[i]);
        ExtPoint acc = ext_identity();
        for (size_t i = 0; i < n; i++)
            acc = ext_add(acc, ext_scalarmul(pts[i], scalars[i]));
        return rist_encode(acc);
    }

    std::vector<ProverPoint> pts;
    pts.reserve(n);
    for (const auto& point : points) pts.push_back(prover_point(point));
    std::vector<std::array<uint8_t, 32>> sbytes(n);
    for (size_t i = 0; i < n; i++)
        sc_tobytes(sbytes[i].data(), scalars[i]);

    size_t w = 1;
    { size_t count = n; while (count >>= 1) w++; }
    if (w > 16) w = 16;

    size_t num_buckets = (1ULL << w) - 1;

    size_t num_windows = (256 + w - 1) / w;

    ExtPoint result = ext_identity();
    std::vector<ExtPoint> buckets(num_buckets);

    for (size_t win_idx = num_windows; win_idx > 0; win_idx--) {

        for (size_t d = 0; d < w; d++)
            result = ext_double(result);

        std::fill(buckets.begin(), buckets.end(), ext_identity());

        size_t bit_start = (win_idx - 1) * w;

        for (size_t i = 0; i < n; i++) {

            const auto bucket_idx = msm_digit(sbytes[i], bit_start, w);

            if (bucket_idx == 0) continue;
            buckets[bucket_idx - 1] = prover_add(buckets[bucket_idx - 1], pts[i]);
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

inline Scalar inner_product(const std::vector<Scalar>& a, const std::vector<Scalar>& b, const ScalarOps& ops = ScalarOps()) {
    assert(a.size() == b.size());
    Scalar result = sc_zero();
    for (size_t i = 0; i < a.size(); i++)
        result = sc_add(result, ops.mul(a[i], b[i]));
    return result;
}

inline InnerProductProof ipp_prove(
    Transcript& transcript,
    const RistrettoPoint& Q,
    std::vector<Scalar> a,
    std::vector<Scalar> b,
    std::vector<RistrettoPoint> G_vec,
    std::vector<RistrettoPoint> H_vec,
    const std::vector<Scalar>& h_scale = {}
) {
    const auto& ops = transcript.ops;
    size_t n = a.size();
    assert(n == b.size());
    assert(n == G_vec.size());
    assert(n == H_vec.size());
    assert(n > 0 && (n & (n - 1)) == 0);
    if (!h_scale.empty() && h_scale.size() != n)
        throw std::runtime_error("pvac: generator scale count differs");

    size_t lg = log2_size(n);
    InnerProductProof proof;
    proof.L.resize(lg);
    proof.R.resize(lg);

    for (size_t k = 0; k < lg; k++) {
        size_t half = n / 2;

        Scalar c_L = sc_zero();
        Scalar c_R = sc_zero();
        for (size_t i = 0; i < half; i++) {
            c_L = sc_add(c_L, ops.mul(a[i], b[half + i]));
            c_R = sc_add(c_R, ops.mul(a[half + i], b[i]));
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
                scalars.push_back(k == 0 && !h_scale.empty()
                    ? sc_mul(b[half + i], h_scale[i], ScalarRule::Wide)
                    : b[half + i]);
                points.push_back(H_vec[i]);
            }
            scalars.push_back(c_L);
            points.push_back(Q);

            proof.L[k] = prover_msm(scalars, points);
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
                scalars.push_back(k == 0 && !h_scale.empty()
                    ? sc_mul(b[i], h_scale[half + i], ScalarRule::Wide)
                    : b[i]);
                points.push_back(H_vec[half + i]);
            }
            scalars.push_back(c_R);
            points.push_back(Q);

            proof.R[k] = prover_msm(scalars, points);
        }

        transcript.append_point("L", proof.L[k]);
        transcript.append_point("R", proof.R[k]);
        Scalar u_k = transcript.challenge_scalar("u");
        Scalar u_k_inv = ops.inv(u_k);

        std::vector<Scalar> a_new(half);
        std::vector<Scalar> b_new(half);
        std::vector<RistrettoPoint> G_new(half);
        std::vector<RistrettoPoint> H_new(half);

        for (size_t i = 0; i < half; i++) {
            a_new[i] = sc_add(ops.mul(a[i], u_k), ops.mul(a[half + i], u_k_inv));
            b_new[i] = sc_add(ops.mul(b[i], u_k_inv), ops.mul(b[half + i], u_k));

            G_new[i] = prover_msm(
                {u_k_inv, u_k},
                {G_vec[i], G_vec[half + i]}
            );

            const auto h_left = k == 0 && !h_scale.empty()
                ? sc_mul(u_k, h_scale[i], ScalarRule::Wide) : u_k;
            const auto h_right = k == 0 && !h_scale.empty()
                ? sc_mul(u_k_inv, h_scale[half + i], ScalarRule::Wide) : u_k_inv;
            H_new[i] = prover_msm(
                {h_left, h_right},
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
    const std::vector<Scalar>& challenges,
    const ScalarOps& ops = ScalarOps()
) {
    size_t lg = challenges.size();
    size_t n = 1ULL << lg;

    std::vector<Scalar> u_inv(lg);
    for (size_t k = 0; k < lg; k++)
        u_inv[k] = ops.inv(challenges[k]);

    std::vector<Scalar> s(n);
    s[0] = Scalar{{1, 0, 0, 0}};
    for (size_t k = 0; k < lg; k++)
        s[0] = ops.mul(s[0], u_inv[k]);

    for (size_t i = 1; i < n; i++) {

        size_t k = 0;
        size_t tmp = i;
        while (tmp >>= 1) k++;

        size_t challenge_idx = lg - 1 - k;
        Scalar u_sq = ops.mul(challenges[challenge_idx], challenges[challenge_idx]);
        s[i] = ops.mul(s[i ^ (1ULL << k)], u_sq);
    }

    return s;
}

inline std::vector<Scalar> batch_sc_inv(const std::vector<Scalar>& vals, const ScalarOps& ops = ScalarOps()) {
    size_t n = vals.size();
    if (n == 0) return {};

    std::vector<Scalar> prefix(n);
    prefix[0] = vals[0];
    for (size_t i = 1; i < n; i++)
        prefix[i] = ops.mul(prefix[i-1], vals[i]);

    Scalar inv_all = ops.inv(prefix[n-1]);

    std::vector<Scalar> result(n);
    for (size_t i = n - 1; i > 0; i--) {
        result[i] = ops.mul(inv_all, prefix[i-1]);
        inv_all = ops.mul(inv_all, vals[i]);
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
    const auto& ops = transcript.ops;
    size_t lg = proof.L.size();
    if (proof.R.size() != lg) return false;
    if ((1ULL << lg) != n) return false;

    std::vector<Scalar> challenges(lg);
    for (size_t k = 0; k < lg; k++) {
        transcript.append_point("L", proof.L[k]);
        transcript.append_point("R", proof.R[k]);
        challenges[k] = transcript.challenge_scalar("u");
    }

    auto s = ipp_verification_scalars(challenges, ops);

    auto s_inv = batch_sc_inv(s, ops);

    std::vector<Scalar> scalars;
    std::vector<RistrettoPoint> points;
    scalars.reserve(2 * n + 2 * lg + 2);
    points.reserve(2 * n + 2 * lg + 2);

    Scalar ab = ops.mul(proof.a, proof.b);

    const auto& gen = generators();
    for (size_t i = 0; i < n; i++) {
        scalars.push_back(ops.mul(proof.a, s[i]));
        points.push_back(gen.G(i));
    }

    for (size_t i = 0; i < n; i++) {
        scalars.push_back(ops.mul(proof.b, s_inv[i]));
        points.push_back(gen.H(i));
    }

    scalars.push_back(ab);
    points.push_back(Q);

    std::vector<Scalar> u_sqs(lg);
    for (size_t k = 0; k < lg; k++)
        u_sqs[k] = ops.mul(challenges[k], challenges[k]);
    auto u_inv_sqs = batch_sc_inv(u_sqs, ops);

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