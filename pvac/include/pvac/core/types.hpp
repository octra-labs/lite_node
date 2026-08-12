// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <array>

#include "field.hpp"
#include "bitvec.hpp"
#include "random.hpp"

namespace pvac {

namespace Dom {
    inline constexpr const char* H_GEN = "pvac.dom.h_gen";
    inline constexpr const char* X_SEED = "pvac.dom.x_seed";
    inline constexpr const char* NOISE = "pvac.dom.noise";

    inline constexpr const char* PRF_LPN = "pvac.dom.prf_lpn";
    inline constexpr const char* TOEP = "pvac.dom.toeplitz";

    inline constexpr const char* ZTAG = "pvac.dom.ztag";
    inline constexpr const char* COMMIT = "pvac.dom.commit";

    inline constexpr const char* PRF_R1 = "pvac.prf.r.1";
    inline constexpr const char* PRF_R2 = "pvac.prf.r.2";
    inline constexpr const char* PRF_R3 = "pvac.prf.r.3";

    inline constexpr const char* PRF_NOISE1 = "pvac.prf.noise.1";
    inline constexpr const char* PRF_NOISE2 = "pvac.prf.noise.2";
    inline constexpr const char* PRF_NOISE3 = "pvac.prf.noise.3";

    inline constexpr const char* R_COM = "pvac.dom.r_com";
    inline constexpr const char* PRF_RHO = "pvac.prf.rho";
    inline constexpr const char* PRF_RHO_PROD = "pvac.prf.rho.prod";
    inline constexpr const char* CIRCUIT_PRF_KEY = "pvac.v6.circuit_prf.key";
    inline constexpr const char* CIRCUIT_PRF_BLIND = "pvac.v6.circuit_prf.blind";
    inline constexpr const char* CIRCUIT_PRF_CHALLENGE = "pvac.v6.circuit_prf.challenge";
    inline constexpr const char* CIRCUIT_PRF_ROUND = "pvac.v6.circuit_prf.mimc.round";
    inline constexpr const char* PRF_R_ALPHA = "pvac.v6.prf.r.alpha";
    inline constexpr const char* CIRCUIT_PRF_KEY_V7 = "pvac.v7.circuit_prf.key";
    inline constexpr const char* CIRCUIT_PRF_BLIND_V7 = "pvac.v7.circuit_prf.blind";
    inline constexpr const char* CIRCUIT_PRF_CHALLENGE_V7 = "pvac.v7.circuit_prf.challenge";
    inline constexpr const char* CIRCUIT_PRF_ROUND_V7 = "pvac.v7.circuit_prf.mimc.round";
}

struct Params {

    int B = 337;

    int m_bits = 8192;
    int n_bits = 16384;
    int h_col_wt = 192;
    int x_col_wt = 128;
    int err_wt = 128;

    double noise_entropy_bits = 120.0;
    double tuple2_fraction = 0.55;
    double depth_slope_bits = 16.0;
    size_t edge_budget = 1200000;

    int lpn_n = 4096;
    int lpn_t = 16384;
    int lpn_tau_num = 1;
    int lpn_tau_den = 8;

    double recrypt_lo = 0.48;
    double recrypt_hi = 0.52;
    int recrypt_rounds = 8;
};

struct Nonce128 {
    uint64_t lo;
    uint64_t hi;
};

inline Fp fp_from_hash32_nonzero(const uint8_t in[32]) {
    uint64_t lo = 0;
    uint64_t hi = 0;
    for (int i = 0; i < 8; ++i) lo |= static_cast<uint64_t>(in[i]) << (8 * i);
    for (int i = 0; i < 8; ++i) hi |= static_cast<uint64_t>(in[8 + i]) << (8 * i);
    Fp out = fp_from_words(lo, hi & MASK63);
    return (out.lo || out.hi) ? out : fp_from_u64(1);
}

inline Nonce128 make_nonce128() {
    return Nonce128 { csprng_u64(), csprng_u64() };
}

struct Ubk {
    std::vector<int> perm;
    std::vector<int> inv;
};

struct RSeed {
    uint64_t ztag;
    Nonce128 nonce;
};

enum class RRule : uint8_t {
    BASE = 0,
    PROD = 1
};

enum class CircuitPrfProfile : uint8_t {
    MIMC_X3_V6 = 1,
    MIMC_X5_V7 = 2
};

inline bool valid_circuit_prf_profile(CircuitPrfProfile profile) {
    return profile == CircuitPrfProfile::MIMC_X3_V6 ||
        profile == CircuitPrfProfile::MIMC_X5_V7;
}

struct Layer {
    RRule rule;
    RSeed seed;
    uint32_t pa;
    uint32_t pb;
    std::array<uint8_t, 32> R_com = {};
    std::vector<std::array<uint8_t, 32>> R_PC;
    std::vector<std::array<uint8_t, 32>> PC;
};

enum EdgeSign : uint8_t {
    SGN_P = 0,
    SGN_M = 1
};

struct Edge {
    uint32_t layer_id;
    uint16_t idx;
    uint8_t ch;
    std::vector<Fp> w;
    BitVec  s;
};

struct Cipher {
    std::vector<Layer> L;
    std::vector<Edge> E;
    std::vector<Fp> c0;
    size_t slots = 1;
};

struct PubKey {
    Params prm;
    uint64_t canon_tag;
    std::vector<BitVec> H;
    Ubk ubk;
    std::array<uint8_t, 32> H_digest;
    Fp omega_B;
    std::vector<Fp> powg_B;
    std::array<uint8_t, 32> circuit_prf_key_commit = {};
    CircuitPrfProfile circuit_prf_profile = CircuitPrfProfile::MIMC_X3_V6;
};

inline bool is_valid_cipher_shape(const Cipher& cipher) {
    if (cipher.slots == 0)
        return false;
    if (!cipher.c0.empty() && cipher.c0.size() != cipher.slots)
        return false;
    for (size_t layer_id = 0; layer_id < cipher.L.size(); ++layer_id) {
        const auto& layer = cipher.L[layer_id];
        if (layer.rule != RRule::BASE && layer.rule != RRule::PROD)
            return false;
        if (layer.rule == RRule::PROD && (layer.pa >= layer_id || layer.pb >= layer_id))
            return false;
        if (layer.rule == RRule::PROD && !layer.PC.empty())
            return false;
        if (layer.rule == RRule::PROD && !layer.R_PC.empty())
            return false;
        if (!layer.PC.empty() && layer.PC.size() != cipher.slots)
            return false;
        if (!layer.R_PC.empty() && layer.R_PC.size() != cipher.slots)
            return false;
    }
    for (const auto& edge : cipher.E) {
        if (edge.layer_id >= cipher.L.size())
            return false;
        if (edge.ch != SGN_P && edge.ch != SGN_M)
            return false;
        if (edge.w.size() != cipher.slots)
            return false;
    }
    return true;
}

inline bool is_valid_pubkey_shape(const PubKey& pk) {
    if (pk.prm.B <= 0 || pk.prm.m_bits <= 0 || pk.prm.n_bits <= 0)
        return false;
    if (pk.H.size() != static_cast<size_t>(pk.prm.n_bits))
        return false;
    if (pk.ubk.perm.size() != static_cast<size_t>(pk.prm.m_bits) ||
        pk.ubk.inv.size() != static_cast<size_t>(pk.prm.m_bits))
        return false;
    std::vector<uint8_t> seen_perm(pk.ubk.perm.size(), 0);
    std::vector<uint8_t> seen_inv(pk.ubk.inv.size(), 0);
    for (size_t i = 0; i < pk.ubk.perm.size(); ++i) {
        int p = pk.ubk.perm[i];
        int q = pk.ubk.inv[i];
        if (p < 0 || q < 0 || p >= pk.prm.m_bits || q >= pk.prm.m_bits)
            return false;
        if (seen_perm[static_cast<size_t>(p)] || seen_inv[static_cast<size_t>(q)])
            return false;
        seen_perm[static_cast<size_t>(p)] = 1;
        seen_inv[static_cast<size_t>(q)] = 1;
    }
    for (size_t i = 0; i < pk.ubk.perm.size(); ++i) {
        if (pk.ubk.inv[static_cast<size_t>(pk.ubk.perm[i])] != static_cast<int>(i))
            return false;
    }
    if (pk.powg_B.size() != static_cast<size_t>(pk.prm.B))
        return false;
    for (const auto& column : pk.H) {
        if (column.nbits != static_cast<uint64_t>(pk.prm.m_bits))
            return false;
    }
    if (!valid_circuit_prf_profile(pk.circuit_prf_profile))
        return false;
    return true;
}

inline bool is_cipher_compatible_with_pubkey(const PubKey& pk, const Cipher& cipher) {
    if (!is_valid_pubkey_shape(pk) || !is_valid_cipher_shape(cipher))
        return false;
    for (const auto& edge : cipher.E) {
        if (edge.idx >= pk.powg_B.size())
            return false;
        if (edge.s.nbits != static_cast<uint64_t>(pk.prm.m_bits))
            return false;
    }
    return true;
}

struct SecKey {
    std::array<uint64_t, 4> prf_k;
    Fp circuit_prf_key = {0, 0};
    std::array<uint8_t, 32> circuit_prf_key_blind = {};
    Fp circuit_prf_key_v6 = {0, 0};
    std::array<uint8_t, 32> circuit_prf_key_blind_v6 = {};
    std::vector<uint64_t> lpn_s_bits;
};

struct EvalKey {
    std::vector<Cipher> zero_pool;
    Cipher enc_one;
};

inline int sgn_val(uint8_t ch) {
    return (ch == SGN_P) ? +1 : -1;
}

inline Fp rand_fp_nonzero() {
    for (;;) {
        uint64_t lo = csprng_u64();
        uint64_t hi = csprng_u64() & MASK63;
        Fp x = fp_from_words(lo, hi);

        if (x.lo || x.hi) {
            return x;
        }
    }
}
}