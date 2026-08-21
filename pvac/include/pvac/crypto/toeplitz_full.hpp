// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <vector>

#include "toeplitz.hpp"

namespace pvac {

inline void toep_127_full(
    const std::vector<uint64_t>& top,
    const std::vector<uint64_t>& ybits,
    size_t bits,
    uint64_t& out_lo,
    uint64_t& out_hi
) {
    if (!bits) throw std::runtime_error("pvac: toeplitz width rejected");

    const size_t y_words = (bits + 63) / 64;
    const size_t top_bits = bits + 126;
    const size_t top_words = (top_bits + 63) / 64;

    if (ybits.size() != y_words || top.size() != top_words)
        throw std::runtime_error("pvac: toeplitz shape rejected");

    std::vector<uint64_t> y = ybits;
    if (bits & 63) y.back() &= (1ull << (bits & 63)) - 1;

    std::vector<uint64_t> conv;
#if defined(__PCLMUL__)
    gf2_conv_clmul(y, top, conv);
#elif defined(__aarch64__) && defined(__ARM_FEATURE_CRYPTO)
    gf2_conv_pmull(y, top, conv);
#else
    gf2_conv_scalar(y, top, conv);
#endif

    const size_t offset = bits - 1;
    out_lo = 0;
    out_hi = 0;

    for (size_t r = 0; r < 127; ++r) {
        const size_t pos = offset + r;
        const uint64_t bit = (conv[pos >> 6] >> (pos & 63)) & 1ull;
        if (r < 64) out_lo |= bit << r;
        else out_hi |= bit << (r - 64);
    }
}

}