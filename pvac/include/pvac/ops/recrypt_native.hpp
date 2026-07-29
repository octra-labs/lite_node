// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <cstddef>
#include <cstring>
#include <stdexcept>

#include "recrypt_field_native_cipher.hpp"
#include "recrypt_runtime_eval.hpp"

namespace pvac {

struct NativeRecryptParams {
    size_t out_layers = 2;
    size_t edges_per_layer = 8;
    size_t max_bytes = 1 << 20;
    NativeRuntimeReusableBudget budget;
};

struct NativeRecryptBundle {
    Cipher output;
    NativeResetStmt stmt;
    NativeResetOutput reset_out;
    NativeResetPublicSpec spec;
    NativeResetCert cert;
    NativeResetProofPayload proof;
    NativeResetPublicEvalMaterial material;
    NativeRuntimeHiddenCarrier source;
    NativeRuntimeHiddenCarrier carrier;
    NativeRuntimeResetHandoff handoff;
    NativeRuntimeResetHandoffDecision handoff_decision;
    NativeRuntimeEqProof eq;
    NativeRuntimeEqDecision eq_decision;
    NativeRuntimePerfProfile perf;
    NativeRuntimeReusableDecision reusable;
    NativeRuntimeClaim claim;
};

inline std::array<uint8_t, 32> native_recrypt_tag(const PubKey& pk, const Cipher& input, const Cipher& output, const uint8_t seed[32]) {
    Sha256 h;
    h.init();
    h.update("pvac.native.recrypt.tag", std::strlen("pvac.native.recrypt.tag"));
    sha256_acc_u64(h, pk.canon_tag);
    h.update(seed, 32);
    auto in_digest = native_runtime_cipher_digest(pk, input);
    auto out_digest = native_runtime_cipher_digest(pk, output);
    h.update(in_digest.data(), in_digest.size());
    h.update(out_digest.data(), out_digest.size());
    std::array<uint8_t, 32> out{};
    h.finish(out.data());
    return out;
}

inline bool native_recrypt_spec_eq(const NativeResetPublicSpec& a, const NativeResetPublicSpec& b) {
    return
        a.bind == b.bind &&
        a.stmt_bound == b.stmt_bound &&
        a.output_bound == b.output_bound &&
        a.material_bound == b.material_bound &&
        a.relation_bound == b.relation_bound &&
        a.no_basis_oracle == b.no_basis_oracle &&
        a.native_backend == b.native_backend;
}

inline NativeRecryptBundle native_recrypt_seeded(const PubKey& pk, const SecKey& sk, const Cipher& input, const uint8_t seed[32], const NativeRecryptParams& params = NativeRecryptParams{}) {
    if (params.out_layers == 0)
        throw std::runtime_error("pvac: native recrypt target rejected");
    auto reset = make_field_native_reset_bundle_seeded(pk, sk, input, seed, params.out_layers, params.edges_per_layer);
    NativeRecryptBundle bundle;
    bundle.output = reset.output;
    bundle.stmt = native_reset_stmt_from_cipher(pk, input, bundle.output.L.size());
    bundle.reset_out = native_reset_output_from_cipher(pk, bundle.output);
    bundle.spec = native_reset_field_spec();
    bundle.cert = reset.cert;
    bundle.proof = reset.proof;
    bundle.material = reset.material;
    if (!verify_native_reset_proof_public_cert(bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof))
        throw std::runtime_error("pvac: native recrypt proof rejected");
    if (!verify_native_reset_public_eval_material(bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof, bundle.material))
        throw std::runtime_error("pvac: native recrypt material rejected");
    bundle.source = make_native_runtime_source_carrier(pk, input);
    bundle.carrier = make_native_runtime_com_reset_carrier(bundle.reset_out, bundle.cert, bundle.spec);
    auto tag = native_recrypt_tag(pk, input, bundle.output, seed);
    bundle.handoff = make_native_runtime_reset_handoff(pk, bundle.source, input, bundle.output, tag, bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof);
    bundle.handoff_decision = decide_native_runtime_reset_handoff(pk, bundle.handoff, input, bundle.output, bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof);
    bundle.eq = bundle.handoff.output_eq;
    bundle.eq_decision = decide_native_runtime_eq_proof(pk, bundle.carrier, bundle.output, bundle.eq, bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof);
    bundle.perf = plan_native_runtime_perf_profile(bundle.handoff, bundle.eq, params.max_bytes);
    bundle.reusable = decide_native_runtime_reusable(bundle.carrier, params.budget);
    bundle.claim = decide_native_runtime_claim(bundle.eq_decision, bundle.perf, bundle.carrier, bundle.reusable);
    if (!bundle.handoff_decision.admitted || !bundle.claim.production)
        throw std::runtime_error("pvac: native recrypt production gate rejected");
    return bundle;
}

inline bool verify_native_recrypt(const PubKey& pk, const Cipher& input, const NativeRecryptBundle& bundle, const NativeRecryptParams& params = NativeRecryptParams{}) {
    try {
        if (!is_cipher_compatible_with_pubkey(pk, input))
            return false;
        if (!is_cipher_compatible_with_pubkey(pk, bundle.output))
            return false;
        if (input.slots != bundle.output.slots)
            return false;
        if (!native_recrypt_spec_eq(bundle.spec, native_reset_field_spec()))
            return false;
        if (!native_runtime_stmt_matches_cipher(pk, input, bundle.stmt, bundle.output.L.size()))
            return false;
        if (!native_runtime_output_matches_cipher(pk, bundle.output, bundle.reset_out))
            return false;
        if (!verify_native_reset_proof_public_cert(bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof))
            return false;
        if (!verify_native_reset_public_eval_material(bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof, bundle.material))
            return false;
        auto source = make_native_runtime_source_carrier(pk, input);
        auto carrier = make_native_runtime_com_reset_carrier(bundle.reset_out, bundle.cert, bundle.spec);
        if (!native_recrypt_carrier_eq(bundle.source, source))
            return false;
        if (!native_recrypt_carrier_eq(bundle.carrier, carrier))
            return false;
        auto handoff_decision = decide_native_runtime_reset_handoff(pk, bundle.handoff, input, bundle.output, bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof);
        auto eq_decision = decide_native_runtime_eq_proof(pk, carrier, bundle.output, bundle.eq, bundle.stmt, bundle.reset_out, bundle.cert, bundle.spec, bundle.proof);
        auto perf = plan_native_runtime_perf_profile(bundle.handoff, bundle.eq, params.max_bytes);
        auto reusable = decide_native_runtime_reusable(carrier, params.budget);
        auto claim = decide_native_runtime_claim(eq_decision, perf, carrier, reusable);
        return handoff_decision.admitted && claim.production;
    } catch (...) {
        return false;
    }
}

}