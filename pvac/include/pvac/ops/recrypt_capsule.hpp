// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <cstring>
#include <stdexcept>
#include <vector>

#include "../core/hash.hpp"
#include "../core/types.hpp"
#include "commit.hpp"
#include "recrypt_public.hpp"

namespace pvac {

struct RecryptCapsuleProfile {
    size_t max_output_layers = 0;
    size_t max_output_edges = 0;
    size_t max_slots = 0;
    bool require_smaller = true;
};

struct RecryptCapsuleStatement {
    std::array<uint8_t, 32> label = {};
    std::array<uint8_t, 32> input_commit = {};
    std::array<uint8_t, 32> output_commit = {};
    uint64_t canon_tag = 0;
    size_t input_layers = 0;
    size_t input_edges = 0;
    size_t input_slots = 0;
    size_t max_output_layers = 0;
    size_t max_output_edges = 0;
    size_t max_slots = 0;
    bool require_smaller = true;
};

struct RecryptCapsule {
    RecryptCapsuleStatement statement;
    PublicRecryptBundle bundle;
    std::array<uint8_t, 32> material_digest = {};
};

struct RecryptCapsuleRegistry {
    std::vector<std::array<uint8_t, 32>> spent;
};

inline void recrypt_capsule_hash_bytes(Sha256& h, const std::array<uint8_t, 32>& bytes) {
    h.update(bytes.data(), bytes.size());
}

inline void recrypt_capsule_hash_bool(Sha256& h, bool value) {
    uint8_t b[1] = {static_cast<uint8_t>(value ? 1 : 0)};
    h.update(b, 1);
}

inline void recrypt_capsule_hash_profile(Sha256& h, const RecryptCapsuleProfile& profile) {
    sha256_acc_u64(h, profile.max_output_layers);
    sha256_acc_u64(h, profile.max_output_edges);
    sha256_acc_u64(h, profile.max_slots);
    recrypt_capsule_hash_bool(h, profile.require_smaller);
}

inline void recrypt_capsule_hash_domain(Sha256& h, const char* domain) {
    h.update(domain, std::strlen(domain));
}

inline RecryptCapsuleProfile make_recrypt_capsule_profile(size_t max_output_layers, size_t max_output_edges, size_t max_slots = 0) {
    if (max_output_layers == 0)
        throw std::runtime_error("pvac: make_recrypt_capsule_profile: max_output_layers is zero");
    if (max_output_edges == 0)
        throw std::runtime_error("pvac: make_recrypt_capsule_profile: max_output_edges is zero");
    RecryptCapsuleProfile profile;
    profile.max_output_layers = max_output_layers;
    profile.max_output_edges = max_output_edges;
    profile.max_slots = max_slots;
    return profile;
}

inline PublicRecryptKey public_recrypt_key_from_capsule_profile(const RecryptCapsuleProfile& profile) {
    PublicRecryptKey key = make_public_recrypt_key(profile.max_output_layers, profile.max_output_edges, profile.max_slots);
    key.require_smaller = profile.require_smaller;
    return key;
}

inline std::array<uint8_t, 32> make_recrypt_capsule_label(const PubKey& pk, const RecryptCapsuleProfile& profile, const std::array<uint8_t, 32>& input_commit, const std::array<uint8_t, 32>& output_commit) {
    Sha256 h;
    h.init();
    recrypt_capsule_hash_domain(h, "pvac.recrypt.capsule.label.v1");
    h.update(pk.H_digest.data(), pk.H_digest.size());
    sha256_acc_u64(h, pk.canon_tag);
    recrypt_capsule_hash_profile(h, profile);
    recrypt_capsule_hash_bytes(h, input_commit);
    recrypt_capsule_hash_bytes(h, output_commit);
    std::array<uint8_t, 32> out = {};
    h.finish(out.data());
    return out;
}

inline RecryptCapsuleStatement make_recrypt_capsule_statement(const PubKey& pk, const RecryptCapsuleProfile& profile, const Cipher& input, const Cipher& output) {
    if (!is_cipher_compatible_with_pubkey(pk, input))
        throw std::runtime_error("pvac: make_recrypt_capsule_statement: input rejected");
    if (!is_cipher_compatible_with_pubkey(pk, output))
        throw std::runtime_error("pvac: make_recrypt_capsule_statement: output rejected");
    RecryptCapsuleStatement stmt;
    stmt.input_commit = commit_ct(pk, input);
    stmt.output_commit = commit_ct(pk, output);
    stmt.label = make_recrypt_capsule_label(pk, profile, stmt.input_commit, stmt.output_commit);
    stmt.canon_tag = pk.canon_tag;
    stmt.input_layers = input.L.size();
    stmt.input_edges = input.E.size();
    stmt.input_slots = input.slots;
    stmt.max_output_layers = profile.max_output_layers;
    stmt.max_output_edges = profile.max_output_edges;
    stmt.max_slots = profile.max_slots;
    stmt.require_smaller = profile.require_smaller;
    return stmt;
}

inline std::array<uint8_t, 32> recrypt_capsule_statement_digest(const RecryptCapsuleStatement& stmt) {
    Sha256 h;
    h.init();
    recrypt_capsule_hash_domain(h, "pvac.recrypt.capsule.statement.v1");
    recrypt_capsule_hash_bytes(h, stmt.label);
    recrypt_capsule_hash_bytes(h, stmt.input_commit);
    recrypt_capsule_hash_bytes(h, stmt.output_commit);
    sha256_acc_u64(h, stmt.canon_tag);
    sha256_acc_u64(h, stmt.input_layers);
    sha256_acc_u64(h, stmt.input_edges);
    sha256_acc_u64(h, stmt.input_slots);
    RecryptCapsuleProfile profile;
    profile.max_output_layers = stmt.max_output_layers;
    profile.max_output_edges = stmt.max_output_edges;
    profile.max_slots = stmt.max_slots;
    profile.require_smaller = stmt.require_smaller;
    recrypt_capsule_hash_profile(h, profile);
    std::array<uint8_t, 32> out = {};
    h.finish(out.data());
    return out;
}

inline std::array<uint8_t, 32> recrypt_capsule_material_digest(const PubKey& pk, const RecryptCapsuleStatement& stmt, const Cipher& output) {
    Sha256 h;
    h.init();
    recrypt_capsule_hash_domain(h, "pvac.recrypt.capsule.material.v1");
    auto stmt_digest = recrypt_capsule_statement_digest(stmt);
    auto output_commit = commit_ct(pk, output);
    recrypt_capsule_hash_bytes(h, stmt_digest);
    recrypt_capsule_hash_bytes(h, output_commit);
    sha256_acc_u64(h, output.L.size());
    sha256_acc_u64(h, output.E.size());
    sha256_acc_u64(h, output.slots);
    std::array<uint8_t, 32> out = {};
    h.finish(out.data());
    return out;
}

inline bool is_recrypt_capsule_statement(const PubKey& pk, const RecryptCapsuleProfile& profile, const Cipher& input, const Cipher& output, const RecryptCapsuleStatement& stmt) {
    auto expected = make_recrypt_capsule_statement(pk, profile, input, output);
    return expected.label == stmt.label &&
        expected.input_commit == stmt.input_commit &&
        expected.output_commit == stmt.output_commit &&
        expected.canon_tag == stmt.canon_tag &&
        expected.input_layers == stmt.input_layers &&
        expected.input_edges == stmt.input_edges &&
        expected.input_slots == stmt.input_slots &&
        expected.max_output_layers == stmt.max_output_layers &&
        expected.max_output_edges == stmt.max_output_edges &&
        expected.max_slots == stmt.max_slots &&
        expected.require_smaller == stmt.require_smaller;
}

inline RecryptCapsule make_recrypt_capsule_seeded(const PubKey& pk, const SecKey& sk, const RecryptCapsuleProfile& profile, const Cipher& input, const uint8_t seed[32], size_t out_layers = 2, size_t edges_per_layer = 8) {
    RecryptCapsule cap;
    auto public_key = public_recrypt_key_from_capsule_profile(profile);
    cap.bundle = make_public_recrypt_bundle_seeded(pk, sk, public_key, input, seed, out_layers, edges_per_layer);
    cap.statement = make_recrypt_capsule_statement(pk, profile, input, cap.bundle.output);
    cap.material_digest = recrypt_capsule_material_digest(pk, cap.statement, cap.bundle.output);
    return cap;
}

inline bool verify_recrypt_capsule(const PubKey& pk, const RecryptCapsuleProfile& profile, const Cipher& input, const RecryptCapsule& cap) {
    if (!is_recrypt_capsule_statement(pk, profile, input, cap.bundle.output, cap.statement))
        return false;
    auto digest = recrypt_capsule_material_digest(pk, cap.statement, cap.bundle.output);
    if (digest != cap.material_digest)
        return false;
    auto public_key = public_recrypt_key_from_capsule_profile(profile);
    return verify_public_recrypt(pk, public_key, input, cap.bundle.output, cap.bundle.proof);
}

inline bool is_recrypt_capsule_spent(const RecryptCapsuleRegistry& registry, const RecryptCapsule& cap) {
    auto digest = recrypt_capsule_statement_digest(cap.statement);
    for (const auto& spent : registry.spent) {
        if (spent == digest) {
            return true;
        }
    }
    return false;
}

inline bool accept_recrypt_capsule(const PubKey& pk, const RecryptCapsuleProfile& profile, const Cipher& input, const RecryptCapsule& cap, RecryptCapsuleRegistry& registry, Cipher& output) {
    if (!verify_recrypt_capsule(pk, profile, input, cap))
        return false;
    if (is_recrypt_capsule_spent(registry, cap))
        return false;
    registry.spent.push_back(recrypt_capsule_statement_digest(cap.statement));
    output = cap.bundle.output;
    return true;
}

}