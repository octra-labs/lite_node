// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>

#include "lpn.hpp"
#include "toeplitz_full.hpp"

namespace pvac {

namespace lpn_v2 {

inline constexpr const char* R1 = "pvac.v2.prf.r.1";
inline constexpr const char* R2 = "pvac.v2.prf.r.2";
inline constexpr const char* R3 = "pvac.v2.prf.r.3";
inline constexpr const char* N1 = "pvac.v2.prf.noise.1";
inline constexpr const char* N2 = "pvac.v2.prf.noise.2";
inline constexpr const char* N3 = "pvac.v2.prf.noise.3";

inline Fp core(
    const PubKey& pk,
    const SecKey& sk,
    const RSeed& seed,
    const char* dom
) {
    std::vector<uint64_t> ybits;
    lpn_make_ybits(pk, sk, seed, dom, ybits);

    uint8_t toep_key[32];
    uint64_t toep_nonce;
    derive_aes_key(pk, sk, seed, Dom::TOEP, toep_key, toep_nonce);
    toep_nonce ^= fnv1a_domain(dom);

    AesCtr256 prg;
    prg.init(toep_key, toep_nonce);

    const size_t top_words = ((size_t)pk.prm.lpn_t + 126u + 63u) / 64u;
    std::vector<uint64_t> top(top_words);
    prg.fill_u64(top.data(), top_words);

    uint64_t lo = 0;
    uint64_t hi = 0;
    toep_127_full(top, ybits, (size_t)pk.prm.lpn_t, lo, hi);
    return hash_to_fp_nonzero(lo, hi);
}

inline Fp r(const PubKey& pk, const SecKey& sk, const RSeed& seed) {
    const Fp a = core(pk, sk, seed, R1);
    const Fp b = core(pk, sk, seed, R2);
    const Fp c = core(pk, sk, seed, R3);
    return fp_mul(fp_mul(a, b), c);
}

inline Fp noise(const PubKey& pk, const SecKey& sk, const RSeed& seed) {
    const Fp a = core(pk, sk, seed, N1);
    const Fp b = core(pk, sk, seed, N2);
    const Fp c = core(pk, sk, seed, N3);
    return fp_mul(fp_mul(a, b), c);
}

inline std::vector<Fp> slots(
    const PubKey& pk,
    const SecKey& sk,
    const RSeed& seed,
    size_t count
) {
    std::vector<Fp> out(count);
    if (count == 1) {
        out[0] = r(pk, sk, seed);
        return out;
    }

    for (size_t j = 0; j < count; ++j) {
        RSeed s = seed;
        const uint64_t t = static_cast<uint64_t>(j) * 0x9E3779B97F4A7C15ULL;
        s.nonce.hi ^= t;
        s.nonce.lo ^= (t << 32) ^ (t >> 32);
        s.ztag = prg_layer_ztag(pk.canon_tag, s.nonce);
        out[j] = r(pk, sk, s);
    }
    return out;
}

}

}