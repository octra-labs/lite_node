// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <stdexcept>
#include <string>
#include <vector>

#include "../crypto/ristretto255.hpp"
#include "commit.hpp"
#include "decrypt.hpp"
#include "layer_coeffs.hpp"
#include "recrypt_legacy.hpp"
#include "recrypt_field_native.hpp"

namespace pvac {

struct FieldNativeResetKey {
    RecryptKey key;
    NativeResetRows rows;
};

struct FieldNativeResetBundle {
    Cipher output;
    NativeResetCert cert;
    NativeResetProofPayload proof;
    NativeResetPublicEvalMaterial material;
};

inline NativeResetStmt native_reset_stmt_from_cipher(const PubKey& pk, const Cipher& input, size_t target_terms) {
    if (!is_cipher_compatible_with_pubkey(pk, input))
        throw std::runtime_error("pvac: field native reset input rejected");
    NativeResetStmt stmt;
    stmt.c = input.c0.empty() ? field::Op::zeros(input.slots) : input.c0;
    stmt.alpha = compute_layer_coeffs(pk, input);
    stmt.target_terms = target_terms;
    stmt.bound = HiddenCoeffBound{stmt.alpha.size(), 127, 127};
    return stmt;
}

inline NativeResetOutput native_reset_output_from_cipher(const PubKey& pk, const Cipher& output) {
    if (!is_cipher_compatible_with_pubkey(pk, output))
        throw std::runtime_error("pvac: field native reset output rejected");
    NativeResetOutput out;
    out.c = output.c0.empty() ? field::Op::zeros(output.slots) : output.c0;
    out.beta = compute_layer_coeffs(pk, output);
    return out;
}

inline NativeResetLayerWitness native_reset_layer_witness_from_key(const PubKey& pk, const SecKey& sk, const Cipher& input, const RecryptKey& key, const NativeResetSecrets& sec) {
    NativeResetLayerWitness wit;
    wit.canon_tag = pk.canon_tag;
    wit.old_layers = input.L;
    wit.new_layers = key.layers;
    wit.old_r.assign(input.L.size(), field::Op::zeros(input.slots));
    wit.new_r.assign(key.layers.size(), field::Op::zeros(input.slots));
    std::vector<std::vector<Fp>> cache(input.L.size());
    std::vector<uint8_t> state(input.L.size(), 0);
    for (size_t i = 0; i < input.L.size(); ++i) {
        auto r = layer_R_cached(pk, sk, input, static_cast<uint32_t>(i), state, cache);
        for (size_t j = 0; j < input.slots; ++j) {
            wit.old_r[i][j] = fp_inv(sec.old_x[i][j]);
            if (!native_fp_vec_eq({wit.old_r[i][j]}, {r[j]}))
                throw std::runtime_error("pvac: field native reset old inverse mismatch");
        }
    }
    auto new_inv = recrypt_new_inv(pk, sk, key.layers, input.slots);
    for (size_t i = 0; i < key.layers.size(); ++i) {
        for (size_t j = 0; j < input.slots; ++j) {
            wit.new_r[i][j] = fp_inv(sec.new_y[i][j]);
            if (!native_fp_vec_eq({sec.new_y[i][j]}, {new_inv[i][j]}))
                throw std::runtime_error("pvac: field native reset new inverse mismatch");
        }
    }
    return wit;
}

inline FieldNativeResetKey make_field_native_reset_key_seeded(const PubKey& pk, const SecKey& sk, const Cipher& input, size_t out_layers, const uint8_t seed[32]) {
    FieldNativeResetKey out;
    out.key = make_recrypt_key_seeded(pk, sk, input, out_layers, seed);
    out.rows.bias = out.key.bias;
    out.rows.mix = out.key.mix;
    return out;
}

inline FieldNativeResetBundle make_field_native_reset_bundle_seeded(const PubKey& pk, const SecKey& sk, const Cipher& input, const uint8_t seed[32], size_t out_layers = 2, size_t edges_per_layer = 8) {
    auto reset = make_field_native_reset_key_seeded(pk, sk, input, out_layers, seed);
    FieldNativeResetBundle bundle;
    bundle.output = ct_recrypt_apply_seeded(pk, reset.key, input, seed, edges_per_layer);
    if (bundle.output.L.size() != reset.key.layers.size())
        throw std::runtime_error("pvac: field native reset compacted target layers rejected");
    std::array<uint8_t, 32> proof_seed{};
    for (size_t i = 0; i < proof_seed.size(); ++i) {
        proof_seed[i] = seed[i];
    }
    auto stmt = native_reset_stmt_from_cipher(pk, input, reset.key.layers.size());
    auto out = native_reset_output_from_cipher(pk, bundle.output);
    auto new_inv = recrypt_new_inv(pk, sk, reset.key.layers, input.slots);
    auto sec = native_reset_secrets_from_rows(reset.rows, new_inv);
    auto check = check_native_reset_relation(stmt, reset.rows, sec, out);
    if (!check.admitted) {
        throw std::runtime_error(
            std::string("pvac: field native reset relation rejected shape = ") +
            (check.shape ? "1" : "0") +
            " aggregation = " + (check.aggregation ? "1" : "0") +
            " witness = " + (check.witness ? "1" : "0") +
            " value = " + (check.value ? "1" : "0") +
            " support = " + (check.support ? "1" : "0")
        );
    }
    auto local_cert = make_native_reset_witness_cert(stmt, reset.rows, sec, out);
    auto public_cert = make_native_hidden_reset_cert(stmt, reset.rows, sec, out);
    auto wit = native_reset_layer_witness_from_key(pk, sk, input, reset.key, sec);
    auto spec = native_reset_field_spec();
    bundle.cert = public_cert;
    bundle.proof = make_native_reset_proof_payload(stmt, out, public_cert, spec);
    bundle.proof.arith = make_native_reset_arith_proof(stmt, reset.rows, sec, out, local_cert, bundle.proof.params, proof_seed);
    bundle.proof.layer = make_native_reset_layer_field_proof(stmt, out, wit, sec, bundle.proof.params, proof_seed);
    bundle.material = make_native_reset_public_eval_material(stmt, out, public_cert, spec, bundle.proof);
    return bundle;
}

inline bool verify_field_native_reset_bundle(const PubKey& pk, const Cipher& input, const FieldNativeResetBundle& bundle) {
    try {
        if (!is_cipher_compatible_with_pubkey(pk, input))
            return false;
        if (!is_cipher_compatible_with_pubkey(pk, bundle.output))
            return false;
        if (input.slots != bundle.output.slots)
            return false;
        auto stmt = native_reset_stmt_from_cipher(pk, input, bundle.output.L.size());
        auto out = native_reset_output_from_cipher(pk, bundle.output);
        auto spec = native_reset_field_spec();
        if (!verify_native_reset_proof_public_cert(stmt, out, bundle.cert, spec, bundle.proof))
            return false;
        return verify_native_reset_public_eval_material(stmt, out, bundle.cert, spec, bundle.proof, bundle.material);
    } catch (...) {
        return false;
    }
}

}