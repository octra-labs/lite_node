// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <cassert>
#include <cstring>

#include "../crypto/bulletproofs/r1cs.hpp"
#include "../crypto/bulletproofs/gadgets.hpp"
#include "commit.hpp"
#include "verify_zero.hpp"

namespace pvac {

struct ZeroProof {
    bp::R1CSProof proof;

    bool is_bound = false;
};

namespace detail {

struct CircuitWiring {

    struct SlotVar {
        bp::Variable x_var;
        Scalar x_val;
        bp::FpLimbs limbs;
    };
    std::vector<std::vector<SlotVar>> layer_vars;
};

struct KeyBoundCircuitWiring {
    struct SlotVar {
        bp::Variable r_var;
        Scalar r_val;
        bp::Variable rinv_var;
        Scalar rinv_val;
        bp::FpLimbs rinv_limbs;
    };
    std::vector<std::vector<SlotVar>> layer_vars;
};

struct AmountBinding {
    uint64_t amount = 0;
    Scalar blinding = sc_zero();
    size_t range_bits = 0;
};

inline void append_key_bound_transcript_context(
    bp::Transcript& transcript,
    const PubKey& pk,
    const Cipher& ct,
    const char* proof_kind,
    const RistrettoPoint* amount_commitment = nullptr
) {
    const char* proof_version = circuit_prf_proof_version(pk.circuit_prf_profile);
    transcript.append_message(
        "proof_version",
        reinterpret_cast<const uint8_t*>(proof_version),
        std::strlen(proof_version));
    transcript.append_message("proof_kind", reinterpret_cast<const uint8_t*>(proof_kind), std::strlen(proof_kind));
    if (pk.circuit_prf_profile == CircuitPrfProfile::MIMC_X5_V7)
        transcript.append_u64("circuit_prf_profile", static_cast<uint64_t>(pk.circuit_prf_profile));
    transcript.append_point("registered_key_commitment", pk.circuit_prf_key_commit);
    auto ct_hash = commit_ct(pk, ct);
    transcript.append_message("commit_ct", ct_hash.data(), ct_hash.size());
    if (amount_commitment)
        transcript.append_point("amount_commitment", *amount_commitment);
}

inline CircuitWiring build_circuit(
    bp::R1CSProver& prover,
    const Cipher& ct,
    const std::vector<std::vector<Fp>>& A,
    const std::vector<size_t>& bases,

    const std::vector<std::vector<Fp>>* rinv_ptr,
    const SecKey* sk_ptr,

    const AmountBinding* amount_bind = nullptr
) {
    size_t nL = ct.L.size();
    size_t S = ct.slots;
    size_t nB = bases.size();

    if (A.size() != nL)
        throw std::runtime_error("pvac: coefficient/layer size mismatch");
    for (size_t lid = 0; lid < nL; ++lid) {
        if (A[lid].size() != S)
            throw std::runtime_error("pvac: coefficient/slots size mismatch");
        const auto& layer = ct.L[lid];
        if (layer.rule == RRule::PROD &&
            (layer.pa >= lid || layer.pb >= lid))
            throw std::runtime_error("pvac: invalid product parent");
        if (layer.rule == RRule::PROD && !layer.PC.empty())
            throw std::runtime_error("pvac: product layer must not contain PC");
        if (layer.rule != RRule::BASE && layer.rule != RRule::PROD)
            throw std::runtime_error("pvac: invalid layer rule");
    }

    CircuitWiring w;
    w.layer_vars.resize(nL);
    for (size_t lid = 0; lid < nL; lid++)
        w.layer_vars[lid].resize(S);

    auto get_rinv = [&](size_t lid, size_t j) -> Fp {
        if (rinv_ptr) return (*rinv_ptr)[lid][j];
        return Fp{0, 0};
    };

    for (size_t bi = 0; bi < nB; bi++) {
        size_t lid = bases[bi];
        for (size_t j = 0; j < S; j++) {
            Fp rinv_fp = get_rinv(lid, j);

            Scalar signed_val = rinv_ptr ? sc_from_fp_signed(rinv_fp) : sc_zero();
            Scalar rho_j = (rinv_ptr && sk_ptr) ? derive_rho(*sk_ptr, ct.L[lid], j) : sc_zero();

            bp::Variable v_signed = prover.commit(signed_val, rho_j);

            auto bound = bp::bind_pc_value(prover, v_signed, rinv_fp);

            w.layer_vars[lid][j] = {bound.x_var, bound.x_val, bound.limbs};
        }
    }

    for (size_t lid = 0; lid < nL; lid++) {
        if (ct.L[lid].rule != RRule::PROD) continue;
        uint32_t pa = ct.L[lid].pa;
        uint32_t pb = ct.L[lid].pb;

        for (size_t j = 0; j < S; j++) {

            auto& va = w.layer_vars[pa][j];
            auto& vb = w.layer_vars[pb][j];
            auto result = bp::fp_mul_gadget(prover, va.x_var, va.x_val,
                                                     vb.x_var, vb.x_val);

            auto limbs = bp::fp127_decompose(prover, result.var, result.val);
            w.layer_vars[lid][j] = {result.var, result.val, limbs};
        }
    }

    bp::Variable amount_var;
    Scalar amount_scalar = sc_zero();
    if (amount_bind) {
        amount_scalar = bp::sc_from_u64(amount_bind->amount);
        Scalar blind = amount_bind->blinding;
        amount_var = prover.commit(amount_scalar, blind);
    }

    for (size_t j = 0; j < S; j++) {
        Fp c0_fp = ct.c0.empty() ? Fp{0, 0} : ct.c0[j];
        Scalar c0_scalar = Scalar{{c0_fp.lo, c0_fp.hi, 0, 0}};

        bp::LinearCombination sum_lc;
        if (c0_fp.lo != 0 || c0_fp.hi != 0)
            sum_lc += bp::LinearCombination(bp::Variable::one(), c0_scalar);

        if (amount_bind) {

            sum_lc -= bp::LinearCombination(amount_var);
        }

        Scalar total = amount_bind
            ? sc_sub(c0_scalar, amount_scalar)
            : c0_scalar;

        for (size_t lid = 0; lid < nL; lid++) {
            Fp a_coeff = A[lid][j];
            if (a_coeff.lo == 0 && a_coeff.hi == 0) continue;

            auto folded = bp::fp_mul_const_var_folded(a_coeff, w.layer_vars[lid][j].limbs);
            sum_lc += folded.lc;
            total = sc_add(total, folded.val);
        }

        Scalar p_sc = bp::sc_mersenne_p();
        Scalar k_val = sc_mul(total, sc_inv(p_sc));

        auto [k_var, k_r, k_o] = prover.allocate(k_val, bp::sc_from_u64(1));

        auto [kl, kr, k_out] = prover.multiply(
            bp::LinearCombination(k_var),
            bp::LinearCombination(bp::Variable::one(), p_sc)
        );

        bp::LinearCombination final_lc(k_out);
        final_lc -= sum_lc;
        prover.constrain(final_lc);

        bp::range_check(prover, k_var, k_val, 73);
    }

    return w;
}

inline KeyBoundCircuitWiring build_key_bound_circuit(
    bp::R1CSProver& prover,
    const PubKey& pk,
    const SecKey* sk_ptr,
    const Cipher& ct,
    const std::vector<std::vector<Fp>>& A,
    const std::vector<size_t>& bases,
    const std::vector<std::vector<Fp>>* r_ptr,
    const std::vector<std::vector<Fp>>* rinv_ptr,
    const AmountBinding* amount_bind = nullptr
) {
    size_t nL = ct.L.size();
    size_t S = ct.slots;
    size_t nB = bases.size();
    const bool canonical = pk.circuit_prf_profile == CircuitPrfProfile::MIMC_X5_V7;

    if (A.size() != nL)
        throw std::runtime_error("pvac: coefficient/layer size mismatch");
    for (size_t lid = 0; lid < nL; ++lid) {
        if (A[lid].size() != S)
            throw std::runtime_error("pvac: coefficient/slots size mismatch");
        const auto& layer = ct.L[lid];
        if (layer.rule == RRule::PROD &&
            (layer.pa >= lid || layer.pb >= lid))
            throw std::runtime_error("pvac: invalid product parent");
        if (layer.rule == RRule::PROD && (!layer.PC.empty() || !layer.R_PC.empty()))
            throw std::runtime_error("pvac: product layer must not contain proof commitments");
        if (layer.rule != RRule::BASE && layer.rule != RRule::PROD)
            throw std::runtime_error("pvac: invalid layer rule");
    }

    KeyBoundCircuitWiring w;
    w.layer_vars.resize(nL);
    for (size_t lid = 0; lid < nL; lid++)
        w.layer_vars[lid].resize(S);

    Fp key_fp = sk_ptr
        ? circuit_prf_key_for_profile(*sk_ptr, pk.circuit_prf_profile)
        : Fp{0, 0};
    Scalar key_signed = sk_ptr ? sc_from_fp_signed(key_fp) : sc_zero();
    Scalar key_blind = sk_ptr
        ? sc_reduce256(circuit_prf_blind_for_profile(*sk_ptr, pk.circuit_prf_profile).data())
        : sc_zero();
    bp::Variable key_signed_var = prover.commit(key_signed, key_blind);
    auto key_bound = bp::bind_pc_value(prover, key_signed_var, key_fp, canonical);

    auto constrain_add_key_const = [&](
        const bp::LinearCombination& state_lc,
        Scalar state_val,
        Fp state_fp,
        Fp c_fp,
        const bp::Variable* out_existing,
        const Scalar* out_existing_val,
        const Fp* out_existing_fp
    ) -> std::pair<bp::Variable, Scalar> {
        Fp out_fp = out_existing_fp
            ? *out_existing_fp
            : fp_add(fp_add(state_fp, key_fp), c_fp);
        Scalar c_sc = Scalar{{c_fp.lo, c_fp.hi, 0, 0}};
        Scalar out_val = out_existing_val
            ? *out_existing_val
            : Scalar{{out_fp.lo, out_fp.hi, 0, 0}};

        bp::Variable out_var;
        if (out_existing) {
            out_var = *out_existing;
        } else {
            auto [allocated, one, ignored] = prover.allocate(out_val, bp::sc_from_u64(1));
            bp::LinearCombination one_lc(one);
            one_lc -= bp::LinearCombination(bp::Variable::one());
            prover.constrain(one_lc);
            out_var = allocated;
        }

        Scalar total = sc_sub(sc_add(sc_add(state_val, key_bound.x_val), c_sc), out_val);
        Scalar p_sc = bp::sc_mersenne_p();
        Scalar q_val = sc_mul(total, bp::sc_mersenne_p_inv());
        auto [q_var, q_r, q_out] = prover.allocate(q_val, bp::sc_from_u64(1));
        auto [ql, qr, qp_out] = prover.multiply(
            bp::LinearCombination(q_var),
            bp::LinearCombination(bp::Variable::one(), p_sc)
        );

        bp::LinearCombination add_lc = state_lc;
        add_lc += bp::LinearCombination(key_bound.x_var);
        add_lc += bp::LinearCombination(bp::Variable::one(), c_sc);
        add_lc -= bp::LinearCombination(out_var);
        add_lc -= bp::LinearCombination(qp_out);
        prover.constrain(add_lc);
        bp::range_check(prover, q_var, q_val, 3);
        return {out_var, out_val};
    };

    auto get_r = [&](size_t lid, size_t j) -> Fp {
        if (r_ptr) return (*r_ptr)[lid][j];
        return Fp{0, 0};
    };
    auto get_rinv = [&](size_t lid, size_t j) -> Fp {
        if (rinv_ptr) return (*rinv_ptr)[lid][j];
        return Fp{0, 0};
    };

    for (size_t bi = 0; bi < nB; bi++) {
        size_t lid = bases[bi];
        for (size_t j = 0; j < S; j++) {
            Fp r_fp = get_r(lid, j);
            Fp rinv_fp = get_rinv(lid, j);

            Scalar r_signed_val = r_ptr ? sc_from_fp_signed(r_fp) : sc_zero();
            Scalar rinv_signed_val = rinv_ptr ? sc_from_fp_signed(rinv_fp) : sc_zero();
            Scalar alpha_j = (r_ptr && sk_ptr) ? derive_r_alpha(*sk_ptr, ct.L[lid], j) : sc_zero();
            Scalar rho_j = (rinv_ptr && sk_ptr) ? derive_rho(*sk_ptr, ct.L[lid], j) : sc_zero();

            bp::Variable r_signed = prover.commit(r_signed_val, alpha_j);
            bp::Variable rinv_signed = prover.commit(rinv_signed_val, rho_j);

            auto r_bound = bp::bind_pc_value(prover, r_signed, r_fp, canonical);
            auto rinv_bound = bp::bind_pc_value(prover, rinv_signed, rinv_fp, canonical);

            Fp state_fp = circuit_prf_challenge(pk, ct.L[lid].seed, j);
            Scalar state_val = Scalar{{state_fp.lo, state_fp.hi, 0, 0}};
            bp::LinearCombination state_lc(bp::Variable::one(), state_val);

            for (size_t round = 0; round < circuit_prf_rounds(pk.circuit_prf_profile); ++round) {
                Fp c_fp = circuit_prf_round_constant(pk.circuit_prf_profile, round);
                Fp t_fp = fp_add(fp_add(state_fp, key_fp), c_fp);
                auto [t_var, t_val] = constrain_add_key_const(
                    state_lc, state_val, state_fp, c_fp,
                    nullptr, nullptr, nullptr
                );
                auto t_power = pk.circuit_prf_profile == CircuitPrfProfile::MIMC_X3_V6
                    ? bp::fp_cube_gadget(prover, t_var, t_val, canonical)
                    : bp::fp_fifth_gadget(prover, t_var, t_val, canonical);
                state_fp = circuit_prf_power(pk.circuit_prf_profile, t_fp);
                state_val = t_power.val;
                state_lc = bp::LinearCombination(t_power.var);
            }

            Fp one_fp = fp_from_u64(1);
            constrain_add_key_const(
                state_lc, state_val, state_fp, one_fp,
                &r_bound.x_var, &r_bound.x_val, &r_fp
            );

            auto inv_prod = bp::fp_mul_gadget(prover, r_bound.x_var, r_bound.x_val,
                                                      rinv_bound.x_var, rinv_bound.x_val,
                                                      canonical);
            bp::LinearCombination inv_lc(inv_prod.var);
            inv_lc -= bp::LinearCombination(bp::Variable::one());
            prover.constrain(inv_lc);

            w.layer_vars[lid][j] = {
                r_bound.x_var,
                r_bound.x_val,
                rinv_bound.x_var,
                rinv_bound.x_val,
                rinv_bound.limbs
            };
        }
    }

    for (size_t lid = 0; lid < nL; lid++) {
        if (ct.L[lid].rule != RRule::PROD) continue;
        uint32_t pa = ct.L[lid].pa;
        uint32_t pb = ct.L[lid].pb;

        for (size_t j = 0; j < S; j++) {
            auto& ra = w.layer_vars[pa][j];
            auto& rb = w.layer_vars[pb][j];

            auto r_prod = bp::fp_mul_gadget(prover, ra.r_var, ra.r_val,
                                                    rb.r_var, rb.r_val,
                                                    canonical);
            auto rinv_prod = bp::fp_mul_gadget(prover, ra.rinv_var, ra.rinv_val,
                                                       rb.rinv_var, rb.rinv_val,
                                                       canonical);

            auto inv_prod = bp::fp_mul_gadget(prover, r_prod.var, r_prod.val,
                                                      rinv_prod.var, rinv_prod.val,
                                                      canonical);
            bp::LinearCombination inv_lc(inv_prod.var);
            inv_lc -= bp::LinearCombination(bp::Variable::one());
            prover.constrain(inv_lc);

            auto rinv_limbs = bp::fp127_decompose(
                prover, rinv_prod.var, rinv_prod.val, canonical);
            w.layer_vars[lid][j] = {
                r_prod.var,
                r_prod.val,
                rinv_prod.var,
                rinv_prod.val,
                rinv_limbs
            };
        }
    }

    bp::Variable amount_var;
    Scalar amount_scalar = sc_zero();
    if (amount_bind) {
        amount_scalar = bp::sc_from_u64(amount_bind->amount);
        amount_var = prover.commit(amount_scalar, amount_bind->blinding);
        if (amount_bind->range_bits > 0)
            bp::range_check(prover, amount_var, amount_scalar, amount_bind->range_bits);
    }

    for (size_t j = 0; j < S; j++) {
        Fp c0_fp = ct.c0.empty() ? Fp{0, 0} : ct.c0[j];
        Scalar c0_scalar = Scalar{{c0_fp.lo, c0_fp.hi, 0, 0}};

        bp::LinearCombination sum_lc;
        if (c0_fp.lo != 0 || c0_fp.hi != 0)
            sum_lc += bp::LinearCombination(bp::Variable::one(), c0_scalar);

        if (amount_bind)
            sum_lc -= bp::LinearCombination(amount_var);

        Scalar total = amount_bind
            ? sc_sub(c0_scalar, amount_scalar)
            : c0_scalar;

        for (size_t lid = 0; lid < nL; lid++) {
            Fp a_coeff = A[lid][j];
            if (a_coeff.lo == 0 && a_coeff.hi == 0) continue;

            auto folded = bp::fp_mul_const_var_folded(a_coeff, w.layer_vars[lid][j].rinv_limbs);
            sum_lc += folded.lc;
            total = sc_add(total, folded.val);
        }

        Scalar p_sc = bp::sc_mersenne_p();
        Scalar k_val = sc_mul(total, bp::sc_mersenne_p_inv());
        auto [k_var, k_r, k_o] = prover.allocate(k_val, bp::sc_from_u64(1));
        auto [kl, kr, k_out] = prover.multiply(
            bp::LinearCombination(k_var),
            bp::LinearCombination(bp::Variable::one(), p_sc)
        );

        bp::LinearCombination final_lc(k_out);
        final_lc -= sum_lc;
        prover.constrain(final_lc);
        bp::range_check(prover, k_var, k_val, 73);
    }

    return w;
}

}

inline size_t key_bound_base_layer_limit(CircuitPrfProfile profile) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return 8;
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return 5;
    return 0;
}

inline size_t key_bound_key_switch_base_layer_limit(CircuitPrfProfile profile) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return 8;
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return 6;
    return 0;
}

inline bool key_bound_verify_shape_ok_with_limit(
    const PubKey& pk,
    const Cipher& ct,
    size_t base_layer_limit
) {
    if (ct.slots != 1 || ct.L.empty() || ct.L.size() > 16 || ct.E.size() > 4096)
        return false;
    size_t base_layers = 0;
    for (const auto& layer : ct.L) {
        if (layer.rule == RRule::BASE)
            ++base_layers;
        else if (layer.rule != RRule::PROD)
            return false;
    }
    return base_layers > 0 && base_layers <= base_layer_limit;
}

inline bool key_bound_verify_shape_ok(const PubKey& pk, const Cipher& ct) {
    return key_bound_verify_shape_ok_with_limit(
        pk,
        ct,
        key_bound_base_layer_limit(pk.circuit_prf_profile));
}

inline bool key_bound_key_switch_shape_ok(const PubKey& pk, const Cipher& ct) {
    return key_bound_verify_shape_ok_with_limit(
        pk,
        ct,
        key_bound_key_switch_base_layer_limit(pk.circuit_prf_profile));
}

inline ZeroProof make_zero_proof(
    const PubKey& pk, const SecKey& sk, const Cipher& ct
) {
    if (!key_bound_verify_shape_ok(pk, ct))
        throw std::runtime_error("pvac: key-bound proof shape rejected");
    size_t nL = ct.L.size();
    size_t S = ct.slots;

    std::vector<std::vector<Fp>> cache(nL);
    std::vector<uint8_t> st(nL, 0);
    for (size_t lid = 0; lid < nL; lid++)
        layer_R_cached(pk, sk, ct, (uint32_t)lid, st, cache);

    std::vector<std::vector<Fp>> rinv(nL);
    for (size_t lid = 0; lid < nL; lid++) {
        rinv[lid].resize(S);
        for (size_t j = 0; j < S; j++)
            rinv[lid][j] = fp_inv(cache[lid][j]);
    }

    auto A = compute_layer_coeffs(pk, ct);
    auto bases = base_layer_indices(ct);
    size_t nB = bases.size();

    bp::R1CSProver prover;
    auto wiring = detail::build_key_bound_circuit(prover, pk, &sk, ct, A, bases, &cache, &rinv);

    bp::Transcript transcript("pvac.verify_zero.key_bound");
    transcript.append_u64("nL", nL);
    transcript.append_u64("S", S);
    transcript.append_u64("nB", nB);
    detail::append_key_bound_transcript_context(transcript, pk, ct, "zero");

    ZeroProof result;
    result.proof = prover.prove(transcript);
    result.is_bound = false;
    return result;
}

inline bool verify_zero(
    const PubKey& pk, const Cipher& ct,
    const ZeroProof& proof
) {
    if (!is_cipher_compatible_with_pubkey(pk, ct)) return false;
    if (!key_bound_verify_shape_ok(pk, ct)) return false;
    if (pk.circuit_prf_key_commit == std::array<uint8_t, 32>{}) return false;
    size_t nL = ct.L.size();
    size_t S = ct.slots;

    auto bases = base_layer_indices(ct);
    size_t nB = bases.size();

    if (proof.proof.V.size() != 1 + nB * S * 2) return false;
    if (proof.proof.V[0] != pk.circuit_prf_key_commit) return false;

    for (size_t bi = 0; bi < nB; bi++) {
        size_t lid = bases[bi];
        if (ct.L[lid].R_PC.size() != S) return false;
        if (ct.L[lid].PC.size() != S) return false;
        for (size_t j = 0; j < S; j++) {
            size_t idx = 1 + 2 * (bi * S + j);
            if (proof.proof.V[idx] != ct.L[lid].R_PC[j])
                return false;
            if (proof.proof.V[idx + 1] != ct.L[lid].PC[j])
                return false;
        }
    }

    auto A = compute_layer_coeffs(pk, ct);
    bp::R1CSProver dummy;
    detail::build_key_bound_circuit(dummy, pk, nullptr, ct, A, bases, nullptr, nullptr);

    bp::ConstraintSystem cs;
    cs.num_gates = dummy.num_gates();
    cs.num_committed = dummy.num_committed();
    cs.constraints = dummy.get_constraints();

    bp::Transcript transcript("pvac.verify_zero.key_bound");
    transcript.append_u64("nL", nL);
    transcript.append_u64("S", S);
    transcript.append_u64("nB", nB);
    detail::append_key_bound_transcript_context(transcript, pk, ct, "zero");

    return bp::r1cs_verify(transcript, cs, proof.proof);
}

inline ZeroProof make_zero_proof_bound_checked(
    const PubKey& pk, const SecKey& sk, const Cipher& ct,
    uint64_t amount, const Scalar& amount_blinding,
    size_t base_layer_limit, bp::R1CSLimitProfile limit_profile
) {
    if (!key_bound_verify_shape_ok_with_limit(pk, ct, base_layer_limit))
        throw std::runtime_error("pvac: key-bound proof shape rejected");
    size_t nL = ct.L.size();
    size_t S = ct.slots;

    std::vector<std::vector<Fp>> cache(nL);
    std::vector<uint8_t> st(nL, 0);
    for (size_t lid = 0; lid < nL; lid++)
        layer_R_cached(pk, sk, ct, (uint32_t)lid, st, cache);

    std::vector<std::vector<Fp>> rinv(nL);
    for (size_t lid = 0; lid < nL; lid++) {
        rinv[lid].resize(S);
        for (size_t j = 0; j < S; j++)
            rinv[lid][j] = fp_inv(cache[lid][j]);
    }

    auto A = compute_layer_coeffs(pk, ct);
    auto bases = base_layer_indices(ct);
    size_t nB = bases.size();

    detail::AmountBinding bind;
    bind.amount = amount;
    bind.blinding = amount_blinding;

    bp::R1CSProver prover;
    auto wiring = detail::build_key_bound_circuit(prover, pk, &sk, ct, A, bases, &cache, &rinv, &bind);

    RistrettoPoint amount_commitment = pedersen_commit(bp::sc_from_u64(amount), amount_blinding);
    bp::Transcript transcript("pvac.verify_zero_bound.key_bound");
    transcript.append_u64("nL", nL);
    transcript.append_u64("S", S);
    transcript.append_u64("nB", nB);
    detail::append_key_bound_transcript_context(transcript, pk, ct, "bound", &amount_commitment);

    ZeroProof result;
    result.proof = prover.prove(transcript, limit_profile);
    result.is_bound = true;
    return result;
}

inline ZeroProof make_zero_proof_bound(
    const PubKey& pk, const SecKey& sk, const Cipher& ct,
    uint64_t amount, const Scalar& amount_blinding
) {
    return make_zero_proof_bound_checked(
        pk,
        sk,
        ct,
        amount,
        amount_blinding,
        key_bound_base_layer_limit(pk.circuit_prf_profile),
        bp::R1CSLimitProfile::Default);
}

inline ZeroProof make_zero_proof_bound_key_switch(
    const PubKey& pk, const SecKey& sk, const Cipher& ct,
    uint64_t amount, const Scalar& amount_blinding
) {
    return make_zero_proof_bound_checked(
        pk,
        sk,
        ct,
        amount,
        amount_blinding,
        key_bound_key_switch_base_layer_limit(pk.circuit_prf_profile),
        bp::R1CSLimitProfile::KeySwitchRefresh);
}

inline bool verify_zero_bound_checked(
    const PubKey& pk, const Cipher& ct,
    const ZeroProof& proof, const RistrettoPoint& amount_commitment,
    size_t base_layer_limit, bp::R1CSLimitProfile limit_profile
) {
    if (!is_cipher_compatible_with_pubkey(pk, ct)) return false;
    if (!key_bound_verify_shape_ok_with_limit(pk, ct, base_layer_limit)) return false;
    if (pk.circuit_prf_key_commit == std::array<uint8_t, 32>{}) return false;
    size_t nL = ct.L.size();
    size_t S = ct.slots;

    auto bases = base_layer_indices(ct);
    size_t nB = bases.size();

    size_t amount_idx = 1 + nB * S * 2;
    if (proof.proof.V.size() != amount_idx + 1) return false;
    if (proof.proof.V[0] != pk.circuit_prf_key_commit) return false;

    for (size_t bi = 0; bi < nB; bi++) {
        size_t lid = bases[bi];
        if (ct.L[lid].R_PC.size() != S) return false;
        if (ct.L[lid].PC.size() != S) return false;
        for (size_t j = 0; j < S; j++) {
            size_t idx = 1 + 2 * (bi * S + j);
            if (proof.proof.V[idx] != ct.L[lid].R_PC[j])
                return false;
            if (proof.proof.V[idx + 1] != ct.L[lid].PC[j])
                return false;
        }
    }

    if (proof.proof.V[amount_idx] != amount_commitment) return false;

    auto A = compute_layer_coeffs(pk, ct);
    detail::AmountBinding dummy_bind;
    bp::R1CSProver dummy;
    detail::build_key_bound_circuit(dummy, pk, nullptr, ct, A, bases, nullptr, nullptr, &dummy_bind);

    bp::ConstraintSystem cs;
    cs.num_gates = dummy.num_gates();
    cs.num_committed = dummy.num_committed();
    cs.constraints = dummy.get_constraints();

    bp::Transcript transcript("pvac.verify_zero_bound.key_bound");
    transcript.append_u64("nL", nL);
    transcript.append_u64("S", S);
    transcript.append_u64("nB", nB);
    detail::append_key_bound_transcript_context(transcript, pk, ct, "bound", &amount_commitment);

    return bp::r1cs_verify(transcript, cs, proof.proof, limit_profile);
}

inline bool verify_zero_bound(
    const PubKey& pk, const Cipher& ct,
    const ZeroProof& proof,
    const RistrettoPoint& amount_commitment
) {
    return verify_zero_bound_checked(
        pk,
        ct,
        proof,
        amount_commitment,
        key_bound_base_layer_limit(pk.circuit_prf_profile),
        bp::R1CSLimitProfile::Default);
}

inline bool verify_zero_bound_key_switch(
    const PubKey& pk, const Cipher& ct,
    const ZeroProof& proof,
    const RistrettoPoint& amount_commitment
) {
    return verify_zero_bound_checked(
        pk,
        ct,
        proof,
        amount_commitment,
        key_bound_key_switch_base_layer_limit(pk.circuit_prf_profile),
        bp::R1CSLimitProfile::KeySwitchRefresh);
}

inline ZeroProof make_zero_proof_bound_range(
    const PubKey& pk, const SecKey& sk, const Cipher& ct,
    uint64_t amount, const Scalar& amount_blinding
) {
    if (!key_bound_verify_shape_ok(pk, ct))
        throw std::runtime_error("pvac: key-bound proof shape rejected");
    size_t nL = ct.L.size();
    size_t S = ct.slots;

    std::vector<std::vector<Fp>> cache(nL);
    std::vector<uint8_t> st(nL, 0);
    for (size_t lid = 0; lid < nL; lid++)
        layer_R_cached(pk, sk, ct, (uint32_t)lid, st, cache);

    std::vector<std::vector<Fp>> rinv(nL);
    for (size_t lid = 0; lid < nL; lid++) {
        rinv[lid].resize(S);
        for (size_t j = 0; j < S; j++)
            rinv[lid][j] = fp_inv(cache[lid][j]);
    }

    auto A = compute_layer_coeffs(pk, ct);
    auto bases = base_layer_indices(ct);
    size_t nB = bases.size();

    detail::AmountBinding bind;
    bind.amount = amount;
    bind.blinding = amount_blinding;
    bind.range_bits = 64;

    bp::R1CSProver prover;
    detail::build_key_bound_circuit(prover, pk, &sk, ct, A, bases, &cache, &rinv, &bind);

    RistrettoPoint amount_commitment = pedersen_commit(bp::sc_from_u64(amount), amount_blinding);
    bp::Transcript transcript("pvac.verify_zero_bound_range.key_bound");
    transcript.append_u64("nL", nL);
    transcript.append_u64("S", S);
    transcript.append_u64("nB", nB);
    detail::append_key_bound_transcript_context(transcript, pk, ct, "bound_range", &amount_commitment);

    ZeroProof result;
    result.proof = prover.prove(transcript);
    result.is_bound = true;
    return result;
}

inline bool verify_zero_bound_range(
    const PubKey& pk, const Cipher& ct,
    const ZeroProof& proof,
    const RistrettoPoint& amount_commitment
) {
    if (!is_cipher_compatible_with_pubkey(pk, ct)) return false;
    if (!key_bound_verify_shape_ok(pk, ct)) return false;
    if (pk.circuit_prf_key_commit == std::array<uint8_t, 32>{}) return false;
    size_t nL = ct.L.size();
    size_t S = ct.slots;

    auto bases = base_layer_indices(ct);
    size_t nB = bases.size();

    size_t amount_idx = 1 + nB * S * 2;
    if (proof.proof.V.size() != amount_idx + 1) return false;
    if (proof.proof.V[0] != pk.circuit_prf_key_commit) return false;

    for (size_t bi = 0; bi < nB; bi++) {
        size_t lid = bases[bi];
        if (ct.L[lid].R_PC.size() != S) return false;
        if (ct.L[lid].PC.size() != S) return false;
        for (size_t j = 0; j < S; j++) {
            size_t idx = 1 + 2 * (bi * S + j);
            if (proof.proof.V[idx] != ct.L[lid].R_PC[j])
                return false;
            if (proof.proof.V[idx + 1] != ct.L[lid].PC[j])
                return false;
        }
    }

    if (proof.proof.V[amount_idx] != amount_commitment) return false;

    auto A = compute_layer_coeffs(pk, ct);
    detail::AmountBinding dummy_bind;
    dummy_bind.range_bits = 64;
    bp::R1CSProver dummy;
    detail::build_key_bound_circuit(dummy, pk, nullptr, ct, A, bases, nullptr, nullptr, &dummy_bind);

    bp::ConstraintSystem cs;
    cs.num_gates = dummy.num_gates();
    cs.num_committed = dummy.num_committed();
    cs.constraints = dummy.get_constraints();

    bp::Transcript transcript("pvac.verify_zero_bound_range.key_bound");
    transcript.append_u64("nL", nL);
    transcript.append_u64("S", S);
    transcript.append_u64("nB", nB);
    detail::append_key_bound_transcript_context(transcript, pk, ct, "bound_range", &amount_commitment);

    return bp::r1cs_verify(transcript, cs, proof.proof);
}

inline bool verify_zero_bound_range(
    const PubKey& pk, const Cipher& ct,
    const ZeroProof& proof
) {
    if (!is_cipher_compatible_with_pubkey(pk, ct)) return false;
    if (!key_bound_verify_shape_ok(pk, ct)) return false;
    const auto bases = base_layer_indices(ct);
    const size_t amount_idx = 1 + bases.size() * ct.slots * 2;
    if (proof.proof.V.size() != amount_idx + 1) return false;
    return verify_zero_bound_range(
        pk,
        ct,
        proof,
        proof.proof.V[amount_idx]);
}

}