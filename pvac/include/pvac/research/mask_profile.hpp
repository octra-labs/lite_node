// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <cstdint>
#include <cstring>
#include <optional>

#include "../core/hash.hpp"
#include "../core/types.hpp"

namespace pvac::research::mask_profile {

enum class Id : uint8_t {
    invalid = 0,
    legacy_lpn_prefix = 1,
    circuit_prf = 2,
    lpn_v2 = 3,
    native = 4
};

enum class Stage : uint8_t {
    base = 1,
    product = 2,
    wrapper = 3,
    recrypt = 4
};

struct Descriptor {
    Id id;
    uint8_t version;
};

struct Binding {
    Descriptor profile;
    Stage stage;
    std::array<uint8_t, 32> digest;
};

inline std::optional<Descriptor> descriptor(Id id) {
    switch (id) {
    case Id::legacy_lpn_prefix:
        return Descriptor{id, 1};
    case Id::circuit_prf:
        return Descriptor{id, 6};
    case Id::lpn_v2:
        return Descriptor{id, 2};
    case Id::native:
        return Descriptor{id, 1};
    case Id::invalid:
        return std::nullopt;
    }
    return std::nullopt;
}

inline const char* domain(Id id) {
    switch (id) {
    case Id::legacy_lpn_prefix:
        return "pvac.profile.legacy.lpn.prefix";
    case Id::circuit_prf:
        return "pvac.profile.circuit.prf";
    case Id::lpn_v2:
        return "pvac.profile.lpn.v2";
    case Id::native:
        return "pvac.profile.native";
    case Id::invalid:
        return "";
    }
    return "";
}

inline bool valid(Descriptor profile) {
    const auto expected = descriptor(profile.id);
    return expected.has_value() && expected->version == profile.version;
}

inline bool lpn(Descriptor profile) {
    return valid(profile) && (profile.id == Id::legacy_lpn_prefix || profile.id == Id::lpn_v2);
}

inline bool legacy(Descriptor profile) {
    return valid(profile) && profile.id == Id::legacy_lpn_prefix;
}

inline bool stage_valid(Stage stage) {
    return stage == Stage::base || stage == Stage::product || stage == Stage::wrapper || stage == Stage::recrypt;
}

inline bool stage_matches(const Cipher& cipher, Stage stage) {
    if (cipher.L.empty()) return false;
    if (stage == Stage::base) {
        for (const auto& layer : cipher.L)
            if (layer.rule != RRule::BASE) return false;
        return true;
    }
    if (stage == Stage::product) {
        for (const auto& layer : cipher.L)
            if (layer.rule == RRule::PROD) return true;
        return false;
    }
    if (stage == Stage::wrapper)
        return cipher.L.size() == 2 && cipher.L[0].rule == RRule::BASE && cipher.L[1].rule == RRule::BASE;
    return true;
}

inline void add_seed(Sha256& hash, const RSeed& seed) {
    sha256_acc_u64(hash, seed.ztag);
    sha256_acc_u64(hash, seed.nonce.lo);
    sha256_acc_u64(hash, seed.nonce.hi);
}

inline void add_layer(Sha256& hash, const Layer& layer) {
    hash.update(&layer.rule, 1);
    add_seed(hash, layer.seed);
    sha256_acc_u64(hash, layer.pa);
    sha256_acc_u64(hash, layer.pb);
    hash.update(layer.R_com.data(), layer.R_com.size());
    sha256_acc_u64(hash, layer.R_PC.size());
    for (const auto& point : layer.R_PC)
        hash.update(point.data(), point.size());
    sha256_acc_u64(hash, layer.PC.size());
    for (const auto& point : layer.PC)
        hash.update(point.data(), point.size());
}

inline void add_cipher(Sha256& hash, const Cipher& cipher) {
    sha256_acc_u64(hash, cipher.slots);
    sha256_acc_u64(hash, cipher.L.size());
    for (const auto& layer : cipher.L)
        add_layer(hash, layer);
    sha256_acc_u64(hash, cipher.c0.size());
    for (const auto& value : cipher.c0) {
        sha256_acc_u64(hash, value.lo);
        sha256_acc_u64(hash, value.hi & MASK63);
    }
    sha256_acc_u64(hash, cipher.E.size());
    for (const auto& edge : cipher.E) {
        sha256_acc_u64(hash, edge.layer_id);
        sha256_acc_u64(hash, edge.idx);
        hash.update(&edge.ch, 1);
        sha256_acc_u64(hash, edge.w.size());
        for (const auto& value : edge.w) {
            sha256_acc_u64(hash, value.lo);
            sha256_acc_u64(hash, value.hi & MASK63);
        }
        sha256_acc_u64(hash, edge.s.nbits);
        for (const auto word : edge.s.w)
            sha256_acc_u64(hash, word);
    }
}

inline std::optional<Binding> bind(const PubKey& pk, const Cipher& cipher, Descriptor profile, Stage stage) {
    if (!is_cipher_compatible_with_pubkey(pk, cipher) || !valid(profile) || !stage_valid(stage) ||
        !stage_matches(cipher, stage))
        return std::nullopt;

    Sha256 hash;
    hash.init();
    const char* tag = "pvac.mask.profile.binding";
    hash.update(tag, std::strlen(tag));
    hash.update(domain(profile.id), std::strlen(domain(profile.id)));
    hash.update(&profile.id, 1);
    hash.update(&profile.version, 1);
    hash.update(&stage, 1);
    hash.update(pk.H_digest.data(), pk.H_digest.size());
    sha256_acc_u64(hash, pk.canon_tag);
    add_cipher(hash, cipher);

    Binding out{profile, stage, {}};
    hash.finish(out.digest.data());
    return out;
}

inline bool same(const Binding& left, const Binding& right) {
    return left.profile.id == right.profile.id &&
           left.profile.version == right.profile.version &&
           left.stage == right.stage &&
           left.digest == right.digest;
}

}