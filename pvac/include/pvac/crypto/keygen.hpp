// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <stdexcept>
#include <cstring>

#include "../core/types.hpp"
#include "../core/ct_safe.hpp"
#include "../core/seedable_rng.hpp"
#include "../core/hash.hpp"

#include "ristretto255.hpp"
#include "matrix.hpp"
#include "circuit_prf_profile.hpp"

namespace pvac {

inline void bytes_from_u64s(uint8_t out[32], const std::array<uint64_t, 4>& words) {
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 8; ++j)
            out[i * 8 + j] = static_cast<uint8_t>((words[i] >> (8 * j)) & 0xFF);
    }
}

inline Scalar scalar_from_key_blind(const std::array<uint8_t, 32>& blind) {
    return sc_reduce256(blind.data());
}

inline void set_circuit_prf_commit(PubKey& pk, const SecKey& sk) {
    const Fp& key = circuit_prf_key_for_profile(sk, pk.circuit_prf_profile);
    const auto& blind = circuit_prf_blind_for_profile(sk, pk.circuit_prf_profile);
    pk.circuit_prf_key_commit =
        pedersen_commit(sc_from_fp_signed(key), scalar_from_key_blind(blind));
}

inline void set_circuit_prf_profile(
    PubKey& pk,
    const SecKey& sk,
    CircuitPrfProfile profile
) {
    if (!valid_circuit_prf_profile(profile))
        throw std::runtime_error("pvac: circuit prf profile rejected");
    pk.circuit_prf_profile = profile;
    set_circuit_prf_commit(pk, sk);
}

inline void derive_circuit_prf_material_random(PubKey& pk, SecKey& sk) {
    sk.circuit_prf_key_v6 = rand_fp_nonzero();
    for (int i = 0; i < 4; ++i) {
        uint64_t w = csprng_u64();
        for (int j = 0; j < 8; ++j)
            sk.circuit_prf_key_blind_v6[i * 8 + j] =
                static_cast<uint8_t>((w >> (8 * j)) & 0xFF);
    }
    sk.circuit_prf_key = rand_fp_nonzero();
    for (int i = 0; i < 4; ++i) {
        uint64_t w = csprng_u64();
        for (int j = 0; j < 8; ++j)
            sk.circuit_prf_key_blind[i * 8 + j] =
                static_cast<uint8_t>((w >> (8 * j)) & 0xFF);
    }
    pk.circuit_prf_profile = CircuitPrfProfile::MIMC_X5_V7;
    set_circuit_prf_commit(pk, sk);
}

inline void derive_circuit_prf_material_seeded(PubKey& pk, SecKey& sk, const uint8_t master[32]) {
    uint8_t key_v6_bytes[32];
    {
        Sha256 h;
        h.init();
        h.update(Dom::CIRCUIT_PRF_KEY, std::strlen(Dom::CIRCUIT_PRF_KEY));
        h.update(master, 32);
        h.finish(key_v6_bytes);
    }
    sk.circuit_prf_key_v6 = fp_from_hash32_nonzero(key_v6_bytes);
    {
        Sha256 h;
        h.init();
        h.update(Dom::CIRCUIT_PRF_BLIND, std::strlen(Dom::CIRCUIT_PRF_BLIND));
        h.update(master, 32);
        h.finish(sk.circuit_prf_key_blind_v6.data());
    }
    uint8_t key_v7_bytes[32];
    {
        Sha256 h;
        h.init();
        h.update(Dom::CIRCUIT_PRF_KEY_V7, std::strlen(Dom::CIRCUIT_PRF_KEY_V7));
        h.update(master, 32);
        h.finish(key_v7_bytes);
    }
    sk.circuit_prf_key = fp_from_hash32_nonzero(key_v7_bytes);
    {
        Sha256 h;
        h.init();
        h.update(Dom::CIRCUIT_PRF_BLIND_V7, std::strlen(Dom::CIRCUIT_PRF_BLIND_V7));
        h.update(master, 32);
        h.finish(sk.circuit_prf_key_blind.data());
    }
    pk.circuit_prf_profile = CircuitPrfProfile::MIMC_X5_V7;
    set_circuit_prf_commit(pk, sk);
}

inline void require_params(const Params& prm) {
    if (prm.B < 3)
        throw std::runtime_error("pvac: params basis rejected");
    if (prm.m_bits <= 0 || prm.n_bits <= 0)
        throw std::runtime_error("pvac: params matrix shape rejected");
    if (prm.h_col_wt < 0 || prm.h_col_wt > prm.m_bits)
        throw std::runtime_error("pvac: params H weight rejected");
    if (prm.x_col_wt < 0 || prm.x_col_wt > prm.n_bits)
        throw std::runtime_error("pvac: params X weight rejected");
    if (prm.err_wt < 0 || prm.err_wt > prm.m_bits)
        throw std::runtime_error("pvac: params noise weight rejected");
    if (prm.lpn_n <= 0 || prm.lpn_t <= 0 || prm.lpn_tau_den <= 0)
        throw std::runtime_error("pvac: params lpn shape rejected");
}

inline std::vector<int> factor_small(int n) {
    std::vector<int> p;
    int x = n;

    for (int d = 2; d * (long long)d <= x; ++d) {
        if (x % d == 0) {
            p.push_back(d);

            while (x % d == 0) {
                x /= d;
            }
        }
    }

    if (x > 1) {
        p.push_back(x);
    }

    return p;
}

inline Fp fp_pow_u128(Fp a, u128 e) {
    Fp acc = fp_from_u64(1);

    while (e) {
        if (e & 1) {
            acc = fp_mul(acc, a);
        }

        a = fp_mul(a, a);
        e >>= 1;
    }

    return acc;
}

inline void keygen(const Params & prm, PubKey & pk, SecKey & sk) {
    require_params(prm);
    pk.prm = prm;

    if (pk.prm.B == 0)
        throw std::runtime_error("pvac: keygen: prm.B is zero");

    u128 pm1 = (((u128)1) << 127) - 2;

    if ((pm1 % (u128)pk.prm.B) != 0) {
        throw std::runtime_error("pvac: keygen: prm.B does not divide pm1");
    }

    pk.canon_tag = csprng_u64();

    gen_H(pk);

    pk.ubk = gen_ubk_public(pk.canon_tag, pk.prm.m_bits);

    for (int i = 0; i < 4; i++) {
        sk.prf_k[i] = csprng_u64();
    }
    derive_circuit_prf_material_random(pk, sk);

    u128 E = pm1 / (u128)pk.prm.B;

    auto rand_fp = [&]() {
        for (;;) {
            uint64_t lo = csprng_u64();
            uint64_t hi = csprng_u64() & MASK63;
            Fp x = fp_from_words(lo, hi);

            if (ct::fp_is_nonzero(x)) {
                return x;
            }
        }
    };

    Fp g;

    for (;;) {
        Fp h = rand_fp();
        Fp acc = fp_pow_u128(h, E);

        if (!ct::fp_is_one(acc)) {
            g = acc;
            break;
        }
    }

    pk.powg_B.assign(pk.prm.B, fp_from_u64(0));
    pk.powg_B[0] = fp_from_u64(1);

    for (int i = 1; i < pk.prm.B; i++) {
        pk.powg_B[i] = fp_mul(pk.powg_B[i - 1], g);
    }

    auto primes = factor_small(pk.prm.B);

    for (;;) {
        Fp h = rand_fp();
        Fp w = fp_pow_u128(h, E);

        if (ct::fp_is_one(w)) {
            continue;
        }

        bool ok = true;

        for (int p : primes) {
            Fp t = fp_pow_u64(w, (uint64_t)(pk.prm.B / p));

            if (ct::fp_is_one(t)) {
                ok = false;
                break;
            }
        }

        if (ok) {
            pk.omega_B = w;
            break;
        }
    }

    size_t s_words = (pk.prm.lpn_n + 63) / 64;
    sk.lpn_s_bits.resize(s_words);

    for (size_t i = 0; i < s_words; i++) {
        sk.lpn_s_bits[i] = csprng_u64();
    }

    if (pk.prm.lpn_n & 63) {
        uint64_t m = (pk.prm.lpn_n & 63);
        uint64_t mask = (m == 64) ? ~0ull : ((1ull << m) - 1ull);
        sk.lpn_s_bits.back() &= mask;
    }
}

inline void keygen_from_seed(const Params& prm, PubKey& pk, SecKey& sk, const uint8_t wallet_privkey[32]) {
    require_params(prm);
    pk.prm = prm;

    if (pk.prm.B == 0)
        throw std::runtime_error("pvac: keygen_from_seed: prm.B is zero");

    u128 pm1 = (((u128)1) << 127) - 2;

    if ((pm1 % (u128)pk.prm.B) != 0) {
        throw std::runtime_error("pvac: keygen_from_seed: prm.B does not divide pm1");
    }

    uint8_t master[32];
    {
        Sha256 s; s.init();
        s.update("OCTRA_PVAC_MASTER_V1", 20);
        s.update(wallet_privkey, 32);
        s.finish(master);
    }

    uint8_t tag_seed[32];
    {
        Sha256 s; s.init();
        s.update("OCTRA_PVAC_TAG", 14);
        s.update(master, 32);
        s.finish(tag_seed);
    }
    pk.canon_tag = ((uint64_t)tag_seed[0])
                 | ((uint64_t)tag_seed[1] << 8)
                 | ((uint64_t)tag_seed[2] << 16)
                 | ((uint64_t)tag_seed[3] << 24)
                 | ((uint64_t)tag_seed[4] << 32)
                 | ((uint64_t)tag_seed[5] << 40)
                 | ((uint64_t)tag_seed[6] << 48)
                 | ((uint64_t)tag_seed[7] << 56);

    gen_H(pk);
    pk.ubk = gen_ubk_public(pk.canon_tag, pk.prm.m_bits);

    uint8_t sk_seed[32];
    {
        Sha256 s; s.init();
        s.update("OCTRA_PVAC_SK", 13);
        s.update(master, 32);
        s.finish(sk_seed);
    }

    SeedableRng rng = make_seeded_rng(sk_seed);
    for (int i = 0; i < 4; i++) sk.prf_k[i] = rng.u64();
    derive_circuit_prf_material_seeded(pk, sk, master);

    size_t s_words = (pk.prm.lpn_n + 63) / 64;
    sk.lpn_s_bits.resize(s_words);
    for (size_t i = 0; i < s_words; i++) sk.lpn_s_bits[i] = rng.u64();
    if (pk.prm.lpn_n & 63) {
        uint64_t m = (pk.prm.lpn_n & 63);
        uint64_t mask = (m == 64) ? ~0ull : ((1ull << m) - 1ull);
        sk.lpn_s_bits.back() &= mask;
    }

    u128 E = pm1 / (u128)pk.prm.B;

    uint8_t gen_seed[32];
    {
        Sha256 s; s.init();
        s.update("OCTRA_PVAC_GEN", 14);
        s.update(master, 32);
        s.finish(gen_seed);
    }
    SeedableRng gen_rng = make_seeded_rng(gen_seed);

    auto rand_fp_det = [&]() {
        for (;;) {
            uint64_t lo = gen_rng.u64();
            uint64_t hi = gen_rng.u64() & MASK63;
            Fp x = fp_from_words(lo, hi);
            if (ct::fp_is_nonzero(x)) return x;
        }
    };

    Fp g;
    for (;;) {
        Fp h = rand_fp_det();
        Fp acc = fp_pow_u128(h, E);
        if (!ct::fp_is_one(acc)) { g = acc; break; }
    }

    pk.powg_B.assign(pk.prm.B, fp_from_u64(0));
    pk.powg_B[0] = fp_from_u64(1);
    for (int i = 1; i < pk.prm.B; i++) pk.powg_B[i] = fp_mul(pk.powg_B[i - 1], g);

    auto primes = factor_small(pk.prm.B);
    for (;;) {
        Fp h = rand_fp_det();
        Fp w = fp_pow_u128(h, E);
        if (ct::fp_is_one(w)) continue;
        bool ok = true;
        for (int p : primes) {
            Fp t = fp_pow_u64(w, (uint64_t)(pk.prm.B / p));
            if (ct::fp_is_one(t)) { ok = false; break; }
        }
        if (ok) { pk.omega_B = w; break; }
    }
}

}