// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include "recrypt_runtime_eval.hpp"
#include "recrypt_ru.hpp"

namespace pvac {

struct Mat {
    NativeRuntimeOpening open;
    NativeRuntimeEqProof eq;
    NativeRuntimeReusableDecision reuse;
    NativeRuntimePerfProfile perf;
    NativeRuntimeClaim claim;
    std::array<uint8_t, 32> digest = {};
    bool safe = false;
};

struct MatGate {
    bool admitted = false;
    bool key = false;
    bool source = false;
    bool cipher = false;
    bool open = false;
    bool eq = false;
    bool value = false;
    bool reuse = false;
    bool perf = false;
    bool claim = false;
    bool digest = false;
    bool rejects_value = false;
    bool rejects_reuse = false;
    bool rejects_claim = false;
};

inline NativeRuntimePerfProfile mat_perf(const NativeRuntimeHiddenCarrier& source, const NativeRuntimeEqProof& eq, size_t max_bytes) {
    NativeRuntimePerfProfile perf;
    perf.stages = 1;
    perf.tags = source.out.coeff_tags.size();
    perf.openings = eq.coeff_openings.size();
    perf.digest_bytes = (perf.tags + perf.openings + 8) * 32;
    perf.coeff_bytes = perf.openings * 16;
    perf.total_bytes = perf.digest_bytes + perf.coeff_bytes;
    perf.compact = perf.total_bytes <= max_bytes;
    perf.admitted = perf.compact && perf.openings == source.out.coeff_tags.size() && perf.openings != 0;
    return perf;
}

inline std::array<uint8_t, 32> mat_digest(const Mat& mat) {
    Sha256 h;
    h.init();
    h.update("pvac.native.mat", std::strlen("pvac.native.mat"));
    h.update(mat.open.opening_digest.data(), mat.open.opening_digest.size());
    h.update(mat.eq.proof_digest.data(), mat.eq.proof_digest.size());
    h.update(mat.reuse.output_digest.data(), mat.reuse.output_digest.size());
    sha256_acc_u64(h, mat.perf.total_bytes);
    sha256_acc_u64(h, mat.claim.production ? 1 : 0);
    sha256_acc_u64(h, mat.claim.value_commit ? 1 : 0);
    sha256_acc_u64(h, mat.safe ? 1 : 0);
    std::array<uint8_t, 32> out{};
    h.finish(out.data());
    return out;
}

inline Mat make_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const std::array<uint8_t, 32>& tag) {
    if (!rku_safe(pk, rk))
        throw std::runtime_error("pvac: mat key rejected");
    Mat mat;
    mat.open = make_native_runtime_sealed_opening(pk, source, clean, tag);
    mat.eq = make_native_runtime_eq_proof(pk, source, clean, tag);
    auto eq_decision = decide_native_runtime_eq_proof(pk, source, clean, mat.eq);
    mat.reuse = decide_native_runtime_reusable(source, rk.budget);
    mat.perf = mat_perf(source, mat.eq, rk.budget.max_proof_bytes);
    mat.claim = decide_native_runtime_claim(eq_decision, mat.perf, source, mat.reuse);
    mat.safe = mat.claim.production;
    mat.digest = mat_digest(mat);
    return mat;
}

inline Mat make_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const std::array<uint8_t, 32>& tag, const NativeResetStmt& stmt, const NativeResetOutput& out, const NativeResetCert& cert, const NativeResetPublicSpec& spec, const NativeResetProofPayload& proof) {
    if (!rku_safe(pk, rk))
        throw std::runtime_error("pvac: mat key rejected");
    Mat mat;
    mat.open = make_native_runtime_sealed_opening(pk, source, clean, tag);
    mat.eq = make_native_runtime_eq_proof(pk, source, clean, tag, stmt, out, cert, spec, proof);
    auto eq_decision = decide_native_runtime_eq_proof(pk, source, clean, mat.eq, stmt, out, cert, spec, proof);
    mat.reuse = decide_native_runtime_reusable(source, rk.budget);
    mat.perf = mat_perf(source, mat.eq, rk.budget.max_proof_bytes);
    mat.claim = decide_native_runtime_claim(eq_decision, mat.perf, source, mat.reuse);
    mat.safe = mat.claim.production;
    mat.digest = mat_digest(mat);
    return mat;
}

inline MatGate decide_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const Mat& mat) {
    MatGate gate;
    gate.key = rku_safe(pk, rk);
    gate.source = native_runtime_carrier_ok(source);
    gate.cipher = is_cipher_compatible_with_pubkey(pk, clean);
    auto open = decide_native_runtime_sealed_opening(pk, source, clean, mat.open);
    auto eq = decide_native_runtime_eq_proof(pk, source, clean, mat.eq);
    auto reuse = decide_native_runtime_reusable(source, rk.budget);
    auto perf = mat_perf(source, mat.eq, rk.budget.max_proof_bytes);
    auto claim = decide_native_runtime_claim(eq, perf, source, reuse);
    gate.open = open.admitted;
    gate.eq = eq.admitted;
    gate.value = eq.value_commit;
    gate.reuse = reuse.admitted;
    gate.perf = perf.admitted;
    gate.claim = claim.production;
    gate.digest = native_chosen_digest_eq(mat.digest, mat_digest(mat));
    gate.rejects_value = !gate.value;
    gate.rejects_reuse = !gate.reuse;
    gate.rejects_claim = !gate.claim;
    gate.admitted =
        gate.key &&
        gate.source &&
        gate.cipher &&
        gate.open &&
        gate.eq &&
        gate.value &&
        gate.reuse &&
        gate.perf &&
        gate.claim &&
        gate.digest;
    return gate;
}

inline MatGate decide_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const Mat& mat, const NativeResetStmt& stmt, const NativeResetOutput& out, const NativeResetCert& cert, const NativeResetPublicSpec& spec, const NativeResetProofPayload& proof) {
    MatGate gate;
    gate.key = rku_safe(pk, rk);
    gate.source = native_runtime_carrier_ok(source);
    gate.cipher = is_cipher_compatible_with_pubkey(pk, clean);
    auto open = decide_native_runtime_sealed_opening(pk, source, clean, mat.open);
    auto eq = decide_native_runtime_eq_proof(pk, source, clean, mat.eq, stmt, out, cert, spec, proof);
    auto reuse = decide_native_runtime_reusable(source, rk.budget);
    auto perf = mat_perf(source, mat.eq, rk.budget.max_proof_bytes);
    auto claim = decide_native_runtime_claim(eq, perf, source, reuse);
    gate.open = open.admitted;
    gate.eq = eq.admitted;
    gate.value = eq.value_commit;
    gate.reuse = reuse.admitted;
    gate.perf = perf.admitted;
    gate.claim = claim.production;
    gate.digest = native_chosen_digest_eq(mat.digest, mat_digest(mat));
    gate.rejects_value = !gate.value;
    gate.rejects_reuse = !gate.reuse;
    gate.rejects_claim = !gate.claim;
    gate.admitted =
        gate.key &&
        gate.source &&
        gate.cipher &&
        gate.open &&
        gate.eq &&
        gate.value &&
        gate.reuse &&
        gate.perf &&
        gate.claim &&
        gate.digest;
    return gate;
}

inline bool verify_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const Mat& mat) {
    return decide_mat(pk, rk, source, clean, mat).admitted;
}

inline bool verify_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const Mat& mat, const NativeResetStmt& stmt, const NativeResetOutput& out, const NativeResetCert& cert, const NativeResetPublicSpec& spec, const NativeResetProofPayload& proof) {
    return decide_mat(pk, rk, source, clean, mat, stmt, out, cert, spec, proof).admitted;
}

inline Cipher accept_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const Mat& mat) {
    if (!verify_mat(pk, rk, source, clean, mat))
        throw std::runtime_error("pvac: materialization rejected");
    return clean;
}

inline Cipher accept_mat(const PubKey& pk, const Rku& rk, const NativeRuntimeHiddenCarrier& source, const Cipher& clean, const Mat& mat, const NativeResetStmt& stmt, const NativeResetOutput& out, const NativeResetCert& cert, const NativeResetPublicSpec& spec, const NativeResetProofPayload& proof) {
    if (!verify_mat(pk, rk, source, clean, mat, stmt, out, cert, spec, proof))
        throw std::runtime_error("pvac: materialization rejected");
    return clean;
}

}