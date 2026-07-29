// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <tuple>
#include <cassert>
#include <stdexcept>
#include "transcript.hpp"
#include "generators.hpp"
#include "inner_product.hpp"
#include "r1cs_limits.hpp"
#include "r1cs_types.hpp"
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

class R1CSProver {
    struct CommittedVar { Scalar value, blinding; };
    struct Gate { Scalar a_L, a_R, a_O; };

    std::vector<CommittedVar> committed_;
    std::vector<Gate> gates_;
    std::vector<Constraint> constraints_;

public:
    R1CSProver() = default;

    Variable commit(const Scalar& value, const Scalar& blinding) {
        size_t j = committed_.size();
        committed_.push_back({value, blinding});
        return Variable::committed(j);
    }

    std::tuple<Variable, Variable, Variable> allocate(
        const Scalar& a_L_val,
        const Scalar& a_R_val
    ) {
        size_t i = gates_.size();
        gates_.push_back({a_L_val, a_R_val, sc_mul(a_L_val, a_R_val)});
        return {Variable::mult_left(i), Variable::mult_right(i), Variable::mult_out(i)};
    }

    std::tuple<Variable, Variable, Variable> multiply(
        const LinearCombination& lc_left,
        const LinearCombination& lc_right
    ) {
        size_t i = gates_.size();
        Scalar aL = eval_lc(lc_left);
        Scalar aR = eval_lc(lc_right);
        gates_.push_back({aL, aR, sc_mul(aL, aR)});

        LinearCombination cl = lc_left;
        cl -= LinearCombination(Variable::mult_left(i));
        constraints_.push_back({cl});

        LinearCombination cr = lc_right;
        cr -= LinearCombination(Variable::mult_right(i));
        constraints_.push_back({cr});

        return {Variable::mult_left(i), Variable::mult_right(i), Variable::mult_out(i)};
    }

    void constrain(const LinearCombination& lc) {
        constraints_.push_back({lc});
    }

    size_t num_gates() const { return gates_.size(); }
    size_t num_constraints() const { return constraints_.size(); }
    size_t num_committed() const { return committed_.size(); }
    size_t constraint_terms() const {
        size_t count = 0;
        for (const auto& constraint : constraints_) {
            if (constraint.lc.terms.size() > R1CS_MAX_TERMS - count)
                return R1CS_MAX_TERMS + 1;
            count += constraint.lc.terms.size();
        }
        return count;
    }
    std::vector<Constraint> get_constraints() const { return constraints_; }

    R1CSProof prove(
        Transcript& transcript,
        R1CSLimitProfile profile = R1CSLimitProfile::Default
    ) {
        R1CSProof proof;
        const size_t n = gates_.size();
        const size_t m = committed_.size();
        const size_t q = constraints_.size();
        const size_t terms = constraint_terms();
        const size_t max_constraints = r1cs_constraint_limit(profile);
        if (max_constraints == 0)
            throw std::runtime_error("pvac: invalid r1cs constraint limit");
        if (n > R1CS_MAX_GATES || m > R1CS_MAX_COMMITTED ||
            q > max_constraints || terms > R1CS_MAX_TERMS)
            throw std::runtime_error("pvac: r1cs prover limits exceeded");
        const size_t N = next_power_of_2(n > 0 ? n : 1);

        proof.V.resize(m);
        for (size_t j = 0; j < m; j++)
            proof.V[j] = pedersen_commit(committed_[j].value, committed_[j].blinding);

        transcript.append_u64("n", N);
        transcript.append_u64("m", m);
        transcript.append_u64("q", q);
        for (size_t j = 0; j < m; j++)
            transcript.append_point("V", proof.V[j]);

        std::vector<Scalar> aL(N, sc_zero()), aR(N, sc_zero()), aO(N, sc_zero());
        for (size_t i = 0; i < n; i++) {
            aL[i] = gates_[i].a_L;
            aR[i] = gates_[i].a_R;
            aO[i] = gates_[i].a_O;
        }

        std::vector<Scalar> sL(N), sR(N);
        for (size_t i = 0; i < N; i++) { sL[i] = sc_random(); sR[i] = sc_random(); }

        Scalar alpha = sc_random(), beta = sc_random(), rho = sc_random();

        const auto& gen = generators();
        gen.precompute(N);

        proof.A_I1 = msm_GH_blind(aL, aR, alpha, N);

        proof.A_O1 = msm_G_blind(aO, beta, N);

        proof.S1 = msm_GH_blind(sL, sR, rho, N);

        transcript.append_point("A_I", proof.A_I1);
        transcript.append_point("A_O", proof.A_O1);
        transcript.append_point("S", proof.S1);

        Scalar y = transcript.challenge_scalar("y");
        Scalar z = transcript.challenge_scalar("z");

        std::vector<Scalar> wL(N, sc_zero()), wR(N, sc_zero()), wO(N, sc_zero());
        std::vector<Scalar> wV(m, sc_zero());
        Scalar wc = sc_zero();
        z_aggregate(wL, wR, wO, wV, wc, z, N, m);

        std::vector<Scalar> y_n(N), y_inv_n(N);
        Scalar y_inv = sc_inv(y);
        y_n[0] = Scalar{{1,0,0,0}};
        y_inv_n[0] = Scalar{{1,0,0,0}};
        for (size_t i = 1; i < N; i++) {
            y_n[i] = sc_mul(y_n[i-1], y);
            y_inv_n[i] = sc_mul(y_inv_n[i-1], y_inv);
        }

        std::vector<Scalar> l1(N), l2(N), l3(N);
        std::vector<Scalar> r0(N), r1(N), r3(N);

        for (size_t i = 0; i < N; i++) {
            l1[i] = sc_add(aL[i], sc_mul(y_inv_n[i], wR[i]));
            l2[i] = aO[i];
            l3[i] = sL[i];

            r0[i] = sc_sub(wO[i], y_n[i]);
            r1[i] = sc_add(sc_mul(y_n[i], aR[i]), wL[i]);
            r3[i] = sc_mul(y_n[i], sR[i]);
        }

        Scalar t1 = inner_product(l1, r0);

        Scalar t3 = sc_add(inner_product(l2, r1), inner_product(l3, r0));
        Scalar t4 = sc_add(inner_product(l1, r3), inner_product(l3, r1));
        Scalar t5 = inner_product(l2, r3);
        Scalar t6 = inner_product(l3, r3);

        Scalar tau1 = sc_random();
        Scalar tau3 = sc_random();
        Scalar tau4 = sc_random();
        Scalar tau5 = sc_random();
        Scalar tau6 = sc_random();

        proof.T_1 = pedersen_commit(t1, tau1);
        proof.T_3 = pedersen_commit(t3, tau3);
        proof.T_4 = pedersen_commit(t4, tau4);
        proof.T_5 = pedersen_commit(t5, tau5);
        proof.T_6 = pedersen_commit(t6, tau6);

        transcript.append_point("T_1", proof.T_1);
        transcript.append_point("T_3", proof.T_3);
        transcript.append_point("T_4", proof.T_4);
        transcript.append_point("T_5", proof.T_5);
        transcript.append_point("T_6", proof.T_6);

        Scalar xc = transcript.challenge_scalar("x");

        Scalar x2 = sc_mul(xc, xc);
        Scalar x3 = sc_mul(x2, xc);
        Scalar x4 = sc_mul(x3, xc);
        Scalar x5 = sc_mul(x4, xc);
        Scalar x6 = sc_mul(x5, xc);

        std::vector<Scalar> l_eval(N), r_eval(N);
        for (size_t i = 0; i < N; i++) {
            l_eval[i] = sc_add(sc_add(
                sc_mul(l1[i], xc),
                sc_mul(l2[i], x2)),
                sc_mul(l3[i], x3));

            r_eval[i] = sc_add(sc_add(
                r0[i],
                sc_mul(r1[i], xc)),
                sc_mul(r3[i], x3));
        }

        Scalar t_hat = inner_product(l_eval, r_eval);
        proof.t_x = t_hat;

        Scalar tau_x = sc_mul(tau1, xc);

        Scalar wv_blind = sc_zero();
        for (size_t j = 0; j < m; j++)
            wv_blind = sc_add(wv_blind, sc_mul(wV[j], committed_[j].blinding));
        tau_x = sc_sub(tau_x, sc_mul(wv_blind, x2));
        tau_x = sc_add(tau_x, sc_mul(tau3, x3));
        tau_x = sc_add(tau_x, sc_mul(tau4, x4));
        tau_x = sc_add(tau_x, sc_mul(tau5, x5));
        tau_x = sc_add(tau_x, sc_mul(tau6, x6));
        proof.t_x_blinding = tau_x;

        proof.e_blinding = sc_add(sc_add(
            sc_mul(xc, alpha),
            sc_mul(x2, beta)),
            sc_mul(x3, rho));

        transcript.append_scalar("t_x", proof.t_x);
        transcript.append_scalar("t_x_blinding", proof.t_x_blinding);
        transcript.append_scalar("e_blinding", proof.e_blinding);

        Scalar w_ch = transcript.challenge_scalar("w");
        RistrettoPoint Q = rist_scalarmul(pedersen_B(), w_ch);

        auto Gv = gen.G_vec(N);
        auto Hv = gen.H_vec(N);

        std::vector<RistrettoPoint> H_prime(N);
        for (size_t i = 0; i < N; i++) {
            H_prime[i] = rist_scalarmul(Hv[i], y_inv_n[i]);
        }

        proof.ipp = ipp_prove(transcript, Q, l_eval, r_eval, Gv, H_prime);
        return proof;
    }

private:
    Scalar eval_lc(const LinearCombination& lc) const {
        Scalar r = sc_zero();
        for (const auto& [var, coeff] : lc.terms) {
            Scalar v;
            switch (var.type) {
                case VarType::ONE:       v = Scalar{{1,0,0,0}}; break;
                case VarType::COMMITTED: v = committed_[var.index].value; break;
                case VarType::MULT_LEFT: v = gates_[var.index].a_L; break;
                case VarType::MULT_RIGHT:v = gates_[var.index].a_R; break;
                case VarType::MULT_OUT:  v = gates_[var.index].a_O; break;
            }
            r = sc_add(r, sc_mul(coeff, v));
        }
        return r;
    }

    void z_aggregate(
        std::vector<Scalar>& wL, std::vector<Scalar>& wR,
        std::vector<Scalar>& wO, std::vector<Scalar>& wV,
        Scalar& wc, const Scalar& z, size_t N, size_t m
    ) const {
        Scalar zp = Scalar{{1,0,0,0}};
        for (const auto& c : constraints_) {
            for (const auto& [var, coeff] : c.lc.terms) {
                Scalar zc = sc_mul(zp, coeff);
                switch (var.type) {
                    case VarType::MULT_LEFT:  if (var.index<N) wL[var.index]=sc_add(wL[var.index],zc); break;
                    case VarType::MULT_RIGHT: if (var.index<N) wR[var.index]=sc_add(wR[var.index],zc); break;
                    case VarType::MULT_OUT:   if (var.index<N) wO[var.index]=sc_add(wO[var.index],zc); break;
                    case VarType::COMMITTED:  if (var.index<m) wV[var.index]=sc_add(wV[var.index],zc); break;
                    case VarType::ONE:        wc = sc_add(wc, zc); break;
                }
            }
            zp = sc_mul(zp, z);
        }
    }

    RistrettoPoint msm_GH_blind(
        const std::vector<Scalar>& a, const std::vector<Scalar>& b,
        const Scalar& blind, size_t N
    ) const {
        const auto& gen = generators();
        std::vector<Scalar> sc; std::vector<RistrettoPoint> pt;
        sc.reserve(2*N+1); pt.reserve(2*N+1);
        for (size_t i=0;i<N;i++) { sc.push_back(a[i]); pt.push_back(gen.G(i)); }
        for (size_t i=0;i<N;i++) { sc.push_back(b[i]); pt.push_back(gen.H(i)); }
        sc.push_back(blind); pt.push_back(pedersen_B_blinding());
        return multi_scalar_mul(sc, pt);
    }

    RistrettoPoint msm_G_blind(
        const std::vector<Scalar>& a, const Scalar& blind, size_t N
    ) const {
        const auto& gen = generators();
        std::vector<Scalar> sc; std::vector<RistrettoPoint> pt;
        sc.reserve(N+1); pt.reserve(N+1);
        for (size_t i=0;i<N;i++) { sc.push_back(a[i]); pt.push_back(gen.G(i)); }
        sc.push_back(blind); pt.push_back(pedersen_B_blinding());
        return multi_scalar_mul(sc, pt);
    }
};

}
}