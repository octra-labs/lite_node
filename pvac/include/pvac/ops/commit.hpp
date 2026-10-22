// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <cstring>
#include <array>
#include <limits>
#include <string_view>
#include <type_traits>

#include "../core/types.hpp"
#include "../core/hash.hpp"

namespace pvac {

struct KeyHash {
    Sha256 state;

    KeyHash() { state.init(); }

    void word(uint64_t value) { sha256_acc_u64(state, value); }

    void tag(std::string_view value) {
        word(value.size());
        state.update(value.data(), value.size());
    }

    template<typename... T>
    void group(std::string_view name, const T&... fields) {
        tag("record");
        tag(name);
        word(sizeof...(T));
        (add(fields), ...);
    }

    template<typename T>
    std::enable_if_t<std::is_integral_v<T>> add(T value) {
        static_assert(sizeof(T) <= sizeof(uint64_t));
        tag(std::is_signed_v<T> ? "signed" : "unsigned");
        word(static_cast<uint64_t>(value));
    }

    template<typename T>
    std::enable_if_t<!std::is_integral_v<T>> add(const T&) = delete;

    void add(double value) {
        static_assert(sizeof(double) == sizeof(uint64_t));
        static_assert(std::numeric_limits<double>::is_iec559);
        uint64_t bits;
        std::memcpy(&bits, &value, sizeof(bits));
        tag("real");
        word(bits);
    }

    template<typename T>
    void add(const std::vector<T>& values) {
        tag("list");
        word(values.size());
        for (const auto& value : values) add(value);
    }

    template<size_t N>
    void add(const std::array<uint8_t, N>& value) {
        tag("bytes");
        word(N);
        state.update(value.data(), N);
    }

    void add(const Fp& value) {
        const auto& [lo, hi] = value;
        group("field", lo, hi);
    }

    void add(const BitVec& value) {
        const auto& [bits, words] = value;
        group("bits", bits, words);
    }

    void add(const Ubk& value) {
        const auto& [perm, inv] = value;
        group("index", perm, inv);
    }

    void add(const Params& value) {
        const auto& [branches, rows, columns, h_weight, x_weight, error,
            entropy, fraction, depth, budget, lpn_rows, lpn_columns,
            tau_num, tau_den, low, high, rounds] = value;
        group("params", branches, rows, columns, h_weight, x_weight, error,
            entropy, fraction, depth, budget, lpn_rows, lpn_columns,
            tau_num, tau_den, low, high, rounds);
    }

    void add(CircuitPrfProfile value) {
        tag("profile");
        word(static_cast<uint8_t>(value));
    }

    void add(const PubKey& value) {
        const auto& [params, tag, matrix, index, digest, omega, powers,
            commitment, profile] = value;
        group("public_key", params, tag, matrix, index, digest, omega, powers,
            commitment, profile);
    }

    std::array<uint8_t, 32> finish() {
        std::array<uint8_t, 32> digest{};
        state.finish(digest.data());
        return digest;
    }
};

inline std::array<uint8_t, 32> proof_key_digest(const PubKey& pk) {
    KeyHash hash;
    hash.tag("pvac.proof.public_key.fields");
    hash.add(pk);
    return hash.finish();
}

inline std::array<uint8_t, 32> commit_ct(const PubKey & pk, const Cipher & C)
{
    Sha256 s;
    s.init();
    s.update(Dom::COMMIT, std::strlen(Dom::COMMIT));

    s.update(pk.H_digest.data(), 32);

    sha256_acc_u64(s, pk.canon_tag);

    sha256_acc_u64(s, (uint64_t)C.L.size());
    for (const auto & L : C.L) {
        uint8_t r[1] = { (uint8_t)L.rule };

        s.update(r, 1);

        if (L.rule == RRule::BASE) {

            sha256_acc_u64(s, L.seed.ztag);
            sha256_acc_u64(s, L.seed.nonce.lo);
            sha256_acc_u64(s, L.seed.nonce.hi);
        } else {
            sha256_acc_u64(s, L.pa);
            sha256_acc_u64(s, L.pb);
        }

        s.update(L.R_com.data(), 32);
        sha256_acc_u64(s, (uint64_t)L.R_PC.size());
        for (const auto& rpc : L.R_PC) {
            s.update(rpc.data(), 32);
        }
        sha256_acc_u64(s, (uint64_t)L.PC.size());
        for (const auto& pc : L.PC) {
            s.update(pc.data(), 32);
        }
    }

    sha256_acc_u64(s, (uint64_t)C.slots);
    sha256_acc_u64(s, (uint64_t)C.c0.size());
    for (const auto& x : C.c0) {
        sha256_acc_u64(s, x.lo);
        sha256_acc_u64(s, x.hi & MASK63);
    }

    sha256_acc_u64(s, (uint64_t)C.E.size());
    for (const auto & e : C.E) {
        sha256_acc_u64(s, e.layer_id);
        sha256_acc_u64(s, e.idx);

        uint8_t ch[1] = { e.ch };
        s.update(ch, 1);

        sha256_acc_u64(s, (uint64_t)e.w.size());
        for (size_t j = 0; j < e.w.size(); ++j) {
            uint8_t w16[16];

            for (int i = 0; i < 8; i++)
            {
                w16[i] = (uint8_t)((e.w[j].lo >> (8 * i)) & 0xFF);
            }

            for (int i = 0; i < 8; i++) {
                w16[8 + i] = (uint8_t)(((e.w[j].hi & MASK63) >> (8 * i)) & 0xFF);
            }

            s.update(w16, 16);
        }

        size_t bytes = (e.s.nbits + 7) / 8;
        size_t full = bytes / 8;
        size_t rem = bytes % 8;

        for (size_t i = 0; i < full; i++) {
            uint8_t b[8];
            store_le64(b, e.s.w[i]);
            s.update(b, 8);
        }

        if (rem) {
            uint8_t b[8];

            uint64_t x = e.s.w[full];

            for (size_t j = 0; j < rem; j++) {
                b[j] = (uint8_t)((x >> (8 * j)) & 0xFF);
            }

            s.update(b, rem);
        }
    }

    std::array<uint8_t, 32> out {};
    s.finish(out.data());

    return out;
}
}