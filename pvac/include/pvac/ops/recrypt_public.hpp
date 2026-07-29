// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <stdexcept>

#include "../core/types.hpp"
#include "recrypt_legacy.hpp"
#include "verify_zero_circuit.hpp"

namespace pvac {

struct PublicRecryptKey {
    size_t max_output_layers = 0;
    size_t max_output_edges = 0;
    size_t max_slots = 0;
    bool require_smaller = true;
};

struct PublicRecryptProof {
    ZeroProof equality;
    size_t output_layers = 0;
    size_t output_edges = 0;
};

struct PublicRecryptBundle {
    Cipher output;
    PublicRecryptProof proof;
};

inline PublicRecryptKey make_public_recrypt_key(size_t max_output_layers, size_t max_output_edges, size_t max_slots = 0) {
    if (max_output_layers == 0)
        throw std::runtime_error("pvac: make_public_recrypt_key: max_output_layers is zero");
    if (max_output_edges == 0)
        throw std::runtime_error("pvac: make_public_recrypt_key: max_output_edges is zero");
    PublicRecryptKey key;
    key.max_output_layers = max_output_layers;
    key.max_output_edges = max_output_edges;
    key.max_slots = max_slots;
    return key;
}

inline bool is_public_recrypt_shape(const PubKey& pk, const PublicRecryptKey& key, const Cipher& input, const Cipher& output) {
    if (!is_cipher_compatible_with_pubkey(pk, input))
        return false;
    if (!is_cipher_compatible_with_pubkey(pk, output))
        return false;
    if (input.slots != output.slots)
        return false;
    if (key.max_slots != 0 && output.slots > key.max_slots)
        return false;
    if (output.L.size() > key.max_output_layers)
        return false;
    if (output.E.size() > key.max_output_edges)
        return false;
    if (key.require_smaller && output.L.size() >= input.L.size() && output.E.size() >= input.E.size())
        return false;
    return true;
}

inline PublicRecryptProof prove_public_recrypt(const PubKey& pk, const SecKey& sk, const Cipher& input, const Cipher& output) {
    Cipher diff = ct_sub(pk, input, output);
    PublicRecryptProof proof;
    proof.equality = make_zero_proof(pk, sk, diff);
    proof.output_layers = output.L.size();
    proof.output_edges = output.E.size();
    return proof;
}

inline bool verify_public_recrypt(const PubKey& pk, const PublicRecryptKey& key, const Cipher& input, const Cipher& output, const PublicRecryptProof& proof) {
    if (!is_public_recrypt_shape(pk, key, input, output))
        return false;
    if (proof.equality.is_bound)
        return false;
    if (proof.output_layers != output.L.size())
        return false;
    if (proof.output_edges != output.E.size())
        return false;
    Cipher diff = ct_sub(pk, input, output);
    return verify_zero(pk, diff, proof.equality);
}

inline PublicRecryptBundle make_public_recrypt_bundle_seeded(const PubKey& pk, const SecKey& sk, const PublicRecryptKey& public_key, const Cipher& input, const uint8_t seed[32], size_t out_layers = 2, size_t edges_per_layer = 8) {
    PublicRecryptBundle bundle;
    bundle.output = ct_recrypt_reset_seeded(pk, sk, input, seed, out_layers, edges_per_layer);
    if (!is_public_recrypt_shape(pk, public_key, input, bundle.output))
        throw std::runtime_error("pvac: make_public_recrypt_bundle_seeded: output rejected by public profile");
    bundle.proof = prove_public_recrypt(pk, sk, input, bundle.output);
    return bundle;
}

}