// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <cstdint>
#include <stdexcept>
#include <vector>

#include "../core/types.hpp"
#include "layer_coeffs.hpp"

namespace pvac {

inline bool tape_fp_vec_eq(const std::vector<Fp>& a, const std::vector<Fp>& b) {
    if (a.size() != b.size())
        return false;
    for (size_t i = 0; i < a.size(); ++i) {
        uint64_t diff = (a[i].lo ^ b[i].lo) | ((a[i].hi ^ b[i].hi) & MASK63);
        if (diff != 0)
            return false;
    }
    return true;
}

inline std::vector<Fp> tape_fp_zeros(size_t n) {
    return std::vector<Fp>(n, fp_from_u64(0));
}

inline std::vector<Fp> tape_fp_vec_mul(const std::vector<Fp>& a, const std::vector<Fp>& b) {
    if (a.size() != b.size())
        throw std::runtime_error("pvac: tape vector shape mismatch");
    std::vector<Fp> out(a.size(), fp_from_u64(0));
    for (size_t i = 0; i < a.size(); ++i) {
        out[i] = fp_mul(a[i], b[i]);
    }
    return out;
}

inline std::vector<Fp> tape_c0(const Cipher& cipher) {
    return cipher.c0.empty() ? tape_fp_zeros(cipher.slots) : cipher.c0;
}

inline bool tape_seed_eq(const RSeed& a, const RSeed& b) {
    return a.ztag == b.ztag && a.nonce.lo == b.nonce.lo && a.nonce.hi == b.nonce.hi;
}

inline bool tape_layer_eq(const Layer& a, const Layer& b) {
    return
        a.rule == b.rule &&
        tape_seed_eq(a.seed, b.seed) &&
        a.pa == b.pa &&
        a.pb == b.pb &&
        a.R_com == b.R_com &&
        a.PC == b.PC;
}

inline Layer tape_shift_layer(Layer layer, uint32_t off) {
    if (layer.rule == RRule::PROD) {
        layer.pa += off;
        layer.pb += off;
    }
    return layer;
}

inline bool tape_product_layer_eq(const Layer& layer, uint32_t pa, uint32_t pb, const Layer& la, const Layer& lb) {
    auto expected = pa < pb ? compute_R_com_prod(la.R_com, lb.R_com) : compute_R_com_prod(lb.R_com, la.R_com);
    return
        layer.rule == RRule::PROD &&
        layer.pa == (pa < pb ? pa : pb) &&
        layer.pb == (pa < pb ? pb : pa) &&
        layer.R_com == expected &&
        layer.PC.empty();
}

inline bool tape_header_product_eq(const Cipher& a, const Cipher& b, const Cipher& c) {
    const auto la = static_cast<uint32_t>(a.L.size());
    const auto lb = static_cast<uint32_t>(b.L.size());
    const auto product_count = static_cast<size_t>(la) * static_cast<size_t>(lb);
    if (c.L.size() != a.L.size() + b.L.size() + product_count)
        return false;
    for (size_t i = 0; i < a.L.size(); ++i) {
        if (!tape_layer_eq(c.L[i], a.L[i]))
            return false;
    }
    for (size_t i = 0; i < b.L.size(); ++i) {
        if (!tape_layer_eq(c.L[a.L.size() + i], tape_shift_layer(b.L[i], la)))
            return false;
    }
    size_t lid = a.L.size() + b.L.size();
    for (uint32_t i = 0; i < la; ++i) {
        for (uint32_t j = 0; j < lb; ++j) {
            if (!tape_product_layer_eq(c.L[lid], i, la + j, c.L[i], c.L[la + j]))
                return false;
            ++lid;
        }
    }
    return true;
}

inline std::vector<std::vector<Fp>> tape_expected_product_coeffs(const PubKey& pk, const Cipher& a, const Cipher& b) {
    auto ca = compute_layer_coeffs(pk, a);
    auto cb = compute_layer_coeffs(pk, b);
    const auto la = static_cast<uint32_t>(a.L.size());
    const auto lb = static_cast<uint32_t>(b.L.size());
    std::vector<std::vector<Fp>> out(a.L.size() + b.L.size() + static_cast<size_t>(la) * static_cast<size_t>(lb), tape_fp_zeros(a.slots));
    const auto a0 = tape_c0(a);
    const auto b0 = tape_c0(b);
    for (uint32_t i = 0; i < la; ++i) {
        out[i] = tape_fp_vec_mul(ca[i], b0);
    }
    for (uint32_t j = 0; j < lb; ++j) {
        out[la + j] = tape_fp_vec_mul(cb[j], a0);
    }
    size_t lid = a.L.size() + b.L.size();
    for (uint32_t i = 0; i < la; ++i) {
        for (uint32_t j = 0; j < lb; ++j) {
            out[lid] = tape_fp_vec_mul(ca[i], cb[j]);
            ++lid;
        }
    }
    return out;
}

inline bool tape_product_coeffs_eq(const PubKey& pk, const Cipher& a, const Cipher& b, const Cipher& c) {
    auto expected = tape_expected_product_coeffs(pk, a, b);
    auto actual = compute_layer_coeffs(pk, c);
    if (expected.size() != actual.size())
        return false;
    for (size_t i = 0; i < expected.size(); ++i) {
        if (!tape_fp_vec_eq(expected[i], actual[i]))
            return false;
    }
    return true;
}

inline bool tape_verify_product(const PubKey& pk, const Cipher& a, const Cipher& b, const Cipher& c) {
    if (!is_cipher_compatible_with_pubkey(pk, a) ||
        !is_cipher_compatible_with_pubkey(pk, b) ||
        !is_cipher_compatible_with_pubkey(pk, c))
        return false;
    if (a.slots != b.slots || a.slots != c.slots)
        return false;
    if (!tape_fp_vec_eq(tape_c0(c), tape_fp_vec_mul(tape_c0(a), tape_c0(b))))
        return false;
    if (!tape_header_product_eq(a, b, c))
        return false;
    return tape_product_coeffs_eq(pk, a, b, c);
}

}