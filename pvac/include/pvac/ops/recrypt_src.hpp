// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <stdexcept>

#include "recrypt_ru.hpp"

namespace pvac {

struct RuSrcEval {
    Cipher a;
    Cipher y;
    RSeed seed;
    std::array<uint8_t, 32> digest = {};
    bool key = false;
    bool source = false;
    bool eval = false;
    bool inv = false;
    bool proof = false;
    bool admitted = false;
};

using NatSrcEval = RuSrcEval;

inline Cipher ru_acc(const PubKey& pk, const Rku& rk, const RSeed& seed) {
    (void)pk;
    (void)rk;
    (void)seed;
    throw std::runtime_error("pvac: source eval rejected");
}

inline Cipher nat_acc(const PubKey& pk, const NatKey& key, const RSeed& seed) {
    return ru_acc(pk, key, seed);
}

inline RuSrcEval eval_ru_src(const PubKey& pk, const Rku& rk, const RSeed& seed) {
    (void)pk;
    (void)rk;
    (void)seed;
    throw std::runtime_error("pvac: source eval rejected");
}

inline NatSrcEval eval_nat_src(const PubKey& pk, const NatKey& key, const RSeed& seed) {
    return eval_ru_src(pk, key, seed);
}

inline bool verify_ru_src(const PubKey& pk, const Rku& rk, const RSeed& seed, const RuSrcEval& eval) {
    try {
        auto expected = eval_ru_src(pk, rk, seed);
        return
            eval.key &&
            eval.source &&
            eval.eval &&
            eval.inv &&
            eval.proof &&
            eval.admitted &&
            native_chosen_digest_eq(eval.digest, expected.digest) &&
            native_chosen_digest_eq(native_runtime_cipher_digest(pk, eval.y), expected.digest);
    } catch (...) {
        return false;
    }
}

inline bool verify_nat_src(const PubKey& pk, const NatKey& key, const RSeed& seed, const NatSrcEval& eval) {
    return verify_ru_src(pk, key, seed, eval);
}

}