// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <cassert>
#include <limits>
#include "transcript.hpp"
#include "generators.hpp"
#include "inner_product.hpp"
#include "r1cs_limits.hpp"
#include "r1cs_types.hpp"
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

static_assert(R1CS_MAX_GATES == BP_MAX_VECTOR_SIZE);

struct ConstraintSystem {
    size_t num_gates;
    size_t num_committed;
    std::vector<Constraint> constraints;

    size_t padded_gates() const {
        return next_power_of_2(num_gates > 0 ? num_gates : 1);
    }

    bool padded_gates_checked(size_t& out) const {
        if (num_gates > R1CS_MAX_GATES)
            return false;
        try {
            out = padded_gates();
        } catch (...) {
            return false;
        }
        return out != 0 && out <= R1CS_MAX_GATES;
    }
};

inline bool r1cs_var_ok(const Variable& var, size_t gates, size_t committed) {
    switch (var.type) {
        case VarType::ONE:
            return var.index == 0;
        case VarType::COMMITTED:
            return var.index < committed;
        case VarType::MULT_LEFT:
        case VarType::MULT_RIGHT:
        case VarType::MULT_OUT:
            return var.index < gates;
    }
    return false;
}

inline bool r1cs_system_ok(
    const ConstraintSystem& cs,
    size_t N,
    R1CSLimitProfile profile = R1CSLimitProfile::Default
) {
    const size_t max_constraints = r1cs_constraint_limit(profile);
    if (max_constraints == 0)
        return false;
    if (N == 0 || N > R1CS_MAX_GATES)
        return false;
    if (cs.num_gates > R1CS_MAX_GATES)
        return false;
    if (cs.num_committed > R1CS_MAX_COMMITTED)
        return false;
    if (cs.constraints.size() > max_constraints)
        return false;
    size_t terms = 0;
    for (const auto& constraint : cs.constraints) {
        if (constraint.lc.terms.size() > R1CS_MAX_TERMS - terms)
            return false;
        terms += constraint.lc.terms.size();
        for (const auto& term : constraint.lc.terms) {
            if (!r1cs_var_ok(term.first, cs.num_gates, cs.num_committed))
                return false;
        }
    }
    return true;
}

inline bool rist_valid(const RistrettoPoint& point) {
    ExtPoint decoded;
    return rist_decode(decoded, point);
}

inline bool ipp_points_valid(const InnerProductProof& proof) {
    if (proof.L.size() != proof.R.size()) return false;
    for (const auto& point : proof.L)
        if (!rist_valid(point))
            return false;
    for (const auto& point : proof.R)
        if (!rist_valid(point))
            return false;
    return true;
}

inline bool r1cs_points_valid(const R1CSProof& proof) {
    if (!rist_valid(proof.A_I1)) return false;
    if (!rist_valid(proof.A_O1)) return false;
    if (!rist_valid(proof.S1)) return false;
    if (!rist_valid(proof.T_1)) return false;
    if (!rist_valid(proof.T_3)) return false;
    if (!rist_valid(proof.T_4)) return false;
    if (!rist_valid(proof.T_5)) return false;
    if (!rist_valid(proof.T_6)) return false;
    for (const auto& point : proof.V)
        if (!rist_valid(point))
            return false;
    return ipp_points_valid(proof.ipp);
}

inline bool ipp_verify_with_y(
    Transcript& transcript,
    const RistrettoPoint& P,
    const RistrettoPoint& Q,
    const InnerProductProof& proof,
    size_t n,
    const std::vector<Scalar>& y_inv_n
) {
    if (!rist_valid(P)) return false;
    if (!rist_valid(Q)) return false;
    if (!ipp_points_valid(proof)) return false;
    if (n == 0 || n > BP_MAX_VECTOR_SIZE || y_inv_n.size() != n) return false;

    size_t lg = proof.L.size();
    if (proof.R.size() != lg) return false;
    if (lg >= std::numeric_limits<size_t>::digits) return false;
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
        scalars.push_back(sc_mul(sc_mul(proof.b, s_inv[i]), y_inv_n[i]));
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

inline bool r1cs_verify(
    Transcript& transcript,
    const ConstraintSystem& cs,
    const R1CSProof& proof,
    R1CSLimitProfile profile = R1CSLimitProfile::Default
) {
    const size_t m = cs.num_committed;
    const size_t q = cs.constraints.size();
    size_t N = 0;
    if (!cs.padded_gates_checked(N)) return false;
    if (!r1cs_system_ok(cs, N, profile)) return false;

    if (proof.V.size() != m) return false;
    if (!r1cs_points_valid(proof)) return false;

    transcript.append_u64("n", N);
    transcript.append_u64("m", m);
    transcript.append_u64("q", q);
    for (size_t j = 0; j < m; j++)
        transcript.append_point("V", proof.V[j]);

    transcript.append_point("A_I", proof.A_I1);
    transcript.append_point("A_O", proof.A_O1);
    transcript.append_point("S", proof.S1);

    Scalar y = transcript.challenge_scalar("y");
    Scalar z = transcript.challenge_scalar("z");

    transcript.append_point("T_1", proof.T_1);
    transcript.append_point("T_3", proof.T_3);
    transcript.append_point("T_4", proof.T_4);
    transcript.append_point("T_5", proof.T_5);
    transcript.append_point("T_6", proof.T_6);

    Scalar xc = transcript.challenge_scalar("x");

    transcript.append_scalar("t_x", proof.t_x);
    transcript.append_scalar("t_x_blinding", proof.t_x_blinding);
    transcript.append_scalar("e_blinding", proof.e_blinding);

    Scalar w_ch = transcript.challenge_scalar("w");

    Scalar x2 = sc_mul(xc, xc);
    Scalar x3 = sc_mul(x2, xc);
    Scalar x4 = sc_mul(x3, xc);
    Scalar x5 = sc_mul(x4, xc);
    Scalar x6 = sc_mul(x5, xc);

    Scalar y_inv = sc_inv(y);
    std::vector<Scalar> y_n(N), y_inv_n(N);
    y_n[0] = Scalar{{1,0,0,0}};
    y_inv_n[0] = Scalar{{1,0,0,0}};
    for (size_t i = 1; i < N; i++) {
        y_n[i] = sc_mul(y_n[i-1], y);
        y_inv_n[i] = sc_mul(y_inv_n[i-1], y_inv);
    }

    std::vector<Scalar> wL(N, sc_zero()), wR(N, sc_zero()), wO(N, sc_zero());
    std::vector<Scalar> wV(m, sc_zero());
    Scalar wc = sc_zero();

    {
        Scalar zp = Scalar{{1,0,0,0}};
        for (size_t qi = 0; qi < q; qi++) {
            for (const auto& [var, coeff] : cs.constraints[qi].lc.terms) {
                Scalar zcoeff = sc_mul(zp, coeff);
                switch (var.type) {
                    case VarType::MULT_LEFT:
                        if (var.index < N) wL[var.index] = sc_add(wL[var.index], zcoeff);
                        break;
                    case VarType::MULT_RIGHT:
                        if (var.index < N) wR[var.index] = sc_add(wR[var.index], zcoeff);
                        break;
                    case VarType::MULT_OUT:
                        if (var.index < N) wO[var.index] = sc_add(wO[var.index], zcoeff);
                        break;
                    case VarType::COMMITTED:
                        if (var.index < m) wV[var.index] = sc_add(wV[var.index], zcoeff);
                        break;
                    case VarType::ONE:
                        wc = sc_add(wc, zcoeff);
                        break;
                }
            }
            zp = sc_mul(zp, z);
        }
    }

    Scalar delta = sc_zero();
    for (size_t i = 0; i < N; i++)
        delta = sc_add(delta, sc_mul(sc_mul(y_inv_n[i], wR[i]), wL[i]));

    RistrettoPoint T_lhs = pedersen_commit(proof.t_x, proof.t_x_blinding);

    {
        std::vector<Scalar> sc;
        std::vector<RistrettoPoint> pt;
        sc.reserve(m + 6);
        pt.reserve(m + 6);

        sc.push_back(sc_mul(x2, sc_sub(delta, wc)));
        pt.push_back(pedersen_B());

        for (size_t j = 0; j < m; j++) {
            sc.push_back(sc_neg(sc_mul(x2, wV[j])));
            pt.push_back(proof.V[j]);
        }

        sc.push_back(xc);        pt.push_back(proof.T_1);

        sc.push_back(x3);        pt.push_back(proof.T_3);

        sc.push_back(x4);        pt.push_back(proof.T_4);

        sc.push_back(x5);        pt.push_back(proof.T_5);

        sc.push_back(x6);        pt.push_back(proof.T_6);

        RistrettoPoint T_rhs = multi_scalar_mul(sc, pt);

        if (T_lhs != T_rhs) return false;
    }

    RistrettoPoint Q = rist_scalarmul(pedersen_B(), w_ch);

    {
        const auto& gen = generators();
        gen.precompute(N);

        std::vector<Scalar> sc;
        std::vector<RistrettoPoint> pt;
        sc.reserve(2*N + 5);
        pt.reserve(2*N + 5);

        for (size_t i = 0; i < N; i++) {
            sc.push_back(sc_mul(xc, sc_mul(y_inv_n[i], wR[i])));
            pt.push_back(gen.G(i));
        }

        for (size_t i = 0; i < N; i++) {
            Scalar h_coeff = sc_sub(
                sc_mul(y_inv_n[i], sc_add(wO[i], sc_mul(wL[i], xc))),
                Scalar{{1,0,0,0}}
            );
            sc.push_back(h_coeff);
            pt.push_back(gen.H(i));
        }

        sc.push_back(xc);
        pt.push_back(proof.A_I1);

        sc.push_back(x2);
        pt.push_back(proof.A_O1);

        sc.push_back(x3);
        pt.push_back(proof.S1);

        sc.push_back(sc_neg(proof.e_blinding));
        pt.push_back(pedersen_B_blinding());

        sc.push_back(sc_mul(proof.t_x, w_ch));
        pt.push_back(pedersen_B());

        RistrettoPoint P = multi_scalar_mul(sc, pt);

        if (!ipp_verify_with_y(transcript, P, Q, proof.ipp, N, y_inv_n))
            return false;
    }

    return true;
}

}
}