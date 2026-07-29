// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <cstdint>
#include <vector>

#include "matrix.hpp"
#include "ristretto255.hpp"
#include "circuit_prf_profile.hpp"

namespace pvac {

inline bool legacy_params_equal(const Params& a, const Params& b) {
    return a.B == b.B &&
        a.m_bits == b.m_bits &&
        a.n_bits == b.n_bits &&
        a.h_col_wt == b.h_col_wt &&
        a.x_col_wt == b.x_col_wt &&
        a.err_wt == b.err_wt &&
        a.noise_entropy_bits == b.noise_entropy_bits &&
        a.tuple2_fraction == b.tuple2_fraction &&
        a.depth_slope_bits == b.depth_slope_bits &&
        a.edge_budget == b.edge_budget &&
        a.lpn_n == b.lpn_n &&
        a.lpn_t == b.lpn_t &&
        a.lpn_tau_num == b.lpn_tau_num &&
        a.lpn_tau_den == b.lpn_tau_den &&
        a.recrypt_lo == b.recrypt_lo &&
        a.recrypt_hi == b.recrypt_hi &&
        a.recrypt_rounds == b.recrypt_rounds;
}

inline void legacy_v1_matrix(PubKey& pk) {
    const int m = pk.prm.m_bits;
    const int n = pk.prm.n_bits;
    const int wt = pk.prm.h_col_wt;
    pk.H.resize(n, BitVec::make(m));
    for (int c = 0; c < n; ++c) {
        BitVec col = BitVec::make(m);
        const std::vector<uint64_t> words {
            static_cast<uint64_t>(m),
            static_cast<uint64_t>(n),
            static_cast<uint64_t>(wt),
            static_cast<uint64_t>(c),
            pk.canon_tag
        };
        for (int row : prg_choose_k(wt, m, Dom::H_GEN, words))
            col.w[static_cast<size_t>(row) >> 6] |= 1ull << (row & 63);
        pk.H[c] = std::move(col);
    }
    Sha256 hash;
    hash.init();
    hash.update("H|v2", 4);
    sha256_acc_u64(hash, pk.prm.m_bits);
    sha256_acc_u64(hash, pk.prm.n_bits);
    sha256_acc_u64(hash, pk.prm.h_col_wt);
    for (const auto& col : pk.H) {
        const size_t bytes = (col.nbits + 7) / 8;
        const size_t full = bytes / 8;
        const size_t rem = bytes % 8;
        for (size_t i = 0; i < full; ++i) {
            uint8_t raw[8];
            store_le64(raw, col.w[i]);
            hash.update(raw, sizeof(raw));
        }
        if (rem) {
            std::array<uint8_t, 8> raw{};
            for (size_t i = 0; i < rem; ++i)
                raw[i] = static_cast<uint8_t>(col.w[full] >> (8 * i));
            hash.update(raw.data(), rem);
        }
    }
    hash.finish(pk.H_digest.data());
}

inline bool legacy_v1_pubkey_matches(const PubKey& legacy, const PubKey& current) {
    if (!legacy_params_equal(legacy.prm, current.prm)) return false;
    if (legacy.canon_tag != current.canon_tag) return false;
    if (legacy.powg_B.size() != current.powg_B.size()) return false;
    for (size_t i = 0; i < legacy.powg_B.size(); ++i) {
        if (legacy.powg_B[i].lo != current.powg_B[i].lo ||
            legacy.powg_B[i].hi != current.powg_B[i].hi)
            return false;
    }
    if (legacy.ubk.perm != current.ubk.perm || legacy.ubk.inv != current.ubk.inv)
        return false;
    if (legacy.circuit_prf_key_commit != std::array<uint8_t, 32>{})
        return false;
    PubKey expected;
    expected.prm = current.prm;
    expected.canon_tag = current.canon_tag;
    legacy_v1_matrix(expected);
    if (legacy.H_digest != expected.H_digest || legacy.H.size() != expected.H.size())
        return false;
    for (size_t i = 0; i < legacy.H.size(); ++i) {
        if (legacy.H[i].nbits != expected.H[i].nbits || legacy.H[i].w != expected.H[i].w)
            return false;
    }
    return legacy.omega_B.hi <= MASK63 && (legacy.omega_B.lo != 0 || legacy.omega_B.hi != 0);
}

inline bool public_profile_matches(const PubKey& a, const PubKey& b) {
    if (!legacy_params_equal(a.prm, b.prm)) return false;
    if (a.canon_tag != b.canon_tag) return false;
    if (a.H_digest != b.H_digest) return false;
    if (a.powg_B.size() != b.powg_B.size()) return false;
    for (size_t i = 0; i < a.powg_B.size(); ++i) {
        if (a.powg_B[i].lo != b.powg_B[i].lo ||
            a.powg_B[i].hi != b.powg_B[i].hi)
            return false;
    }
    if (a.H.size() != b.H.size()) return false;
    for (size_t i = 0; i < a.H.size(); ++i) {
        if (a.H[i].nbits != b.H[i].nbits || a.H[i].w != b.H[i].w)
            return false;
    }
    return a.ubk.perm == b.ubk.perm && a.ubk.inv == b.ubk.inv;
}

inline bool proof_profile_matches(const PubKey& a, const PubKey& b) {
    return public_profile_matches(a, b) &&
        a.circuit_prf_key_commit == b.circuit_prf_key_commit &&
        a.circuit_prf_profile == b.circuit_prf_profile;
}

inline bool secret_profile_matches(
    const PubKey& candidate,
    const PubKey& current,
    const SecKey& sk
) {
    if (!public_profile_matches(candidate, current))
        return false;
    if (!valid_circuit_prf_profile(candidate.circuit_prf_profile))
        return false;
    const Fp& key = circuit_prf_key_for_profile(sk, candidate.circuit_prf_profile);
    const auto& blind = circuit_prf_blind_for_profile(sk, candidate.circuit_prf_profile);
    return candidate.circuit_prf_key_commit ==
        pedersen_commit(sc_from_fp_signed(key), sc_reduce256(blind.data()));
}

}