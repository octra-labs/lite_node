// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <array>
#include <thread>
#include <algorithm>
#include <stdexcept>

#include "../core/types.hpp"
#include "verify_zero.hpp"
#include "verify_zero_circuit.hpp"
#include "arithmetic.hpp"
#include "encrypt.hpp"
#include "commit.hpp"

namespace pvac {

static constexpr size_t RANGE_BITS = 64;
static constexpr size_t RANGE_MAX_VALUE_LAYERS = 512;
static constexpr size_t RANGE_MAX_VALUE_EDGES = 65536;
static constexpr size_t RANGE_MAX_BIT_LAYERS = 4;
static constexpr size_t RANGE_MAX_BIT_EDGES = 8192;
static constexpr size_t RANGE_MAX_SLOTS = 1;

struct RangeProof {

    std::vector<Cipher> ct_bit;

    std::vector<ZeroProof> bit_proofs;

    ZeroProof lc_proof;
};

inline bool range_value_cipher_ok(const PubKey& pk, const Cipher& ct) {
    return
        is_cipher_compatible_with_pubkey(pk, ct) &&
        ct.slots > 0 &&
        ct.slots <= RANGE_MAX_SLOTS &&
        ct.L.size() <= RANGE_MAX_VALUE_LAYERS &&
        ct.E.size() <= RANGE_MAX_VALUE_EDGES;
}

inline bool range_bit_cipher_ok(const PubKey& pk, const Cipher& ct, size_t slots) {
    return
        is_cipher_compatible_with_pubkey(pk, ct) &&
        ct.slots == slots &&
        ct.L.size() <= RANGE_MAX_BIT_LAYERS &&
        ct.E.size() <= RANGE_MAX_BIT_EDGES;
}

inline RangeProof make_range_proof(
    const PubKey& pk,
    const SecKey& sk,
    const Cipher& ct_value,
    uint64_t value
) {
    if (!range_value_cipher_ok(pk, ct_value))
        throw std::runtime_error("pvac: range proof value rejected");
    RangeProof rp;
    rp.ct_bit.resize(RANGE_BITS);
    rp.bit_proofs.resize(RANGE_BITS);

    unsigned hw = std::thread::hardware_concurrency();
    unsigned n_threads = (hw > 1) ? std::min(hw, (unsigned)RANGE_BITS) : 1;

    if (n_threads <= 1) {
        for (size_t i = 0; i < RANGE_BITS; ++i) {
            uint64_t b_i = (value >> i) & 1;
            rp.ct_bit[i] = enc_value(pk, sk, b_i);
            auto ct_b_m1 = ct_sub_const(pk, rp.ct_bit[i], (uint64_t)1);
            uint8_t mul_seed[32];
            for (int k = 0; k < 32; ++k)
                mul_seed[k] = (uint8_t)((i * 37 + k * 13 + 0xA0) & 0xFF);
            auto ct_check = ct_mul_seeded(pk, rp.ct_bit[i], ct_b_m1, mul_seed);
            rp.bit_proofs[i] = make_zero_proof(pk, sk, ct_check);
        }
    } else {
        auto worker = [&](size_t from, size_t to) {
            for (size_t i = from; i < to; ++i) {
                uint64_t b_i = (value >> i) & 1;
                rp.ct_bit[i] = enc_value(pk, sk, b_i);
                auto ct_b_m1 = ct_sub_const(pk, rp.ct_bit[i], (uint64_t)1);
                uint8_t mul_seed[32];
                for (int k = 0; k < 32; ++k)
                    mul_seed[k] = (uint8_t)((i * 37 + k * 13 + 0xA0) & 0xFF);
                auto ct_check = ct_mul_seeded(pk, rp.ct_bit[i], ct_b_m1, mul_seed);
                rp.bit_proofs[i] = make_zero_proof(pk, sk, ct_check);
            }
        };

        std::vector<std::thread> threads;
        size_t chunk = (RANGE_BITS + n_threads - 1) / n_threads;
        for (unsigned t = 0; t < n_threads; ++t) {
            size_t from = t * chunk;
            size_t to = std::min(from + chunk, RANGE_BITS);
            if (from < to)
                threads.emplace_back(worker, from, to);
        }
        for (auto& th : threads)
            th.join();
    }

    Cipher ct_sum = rp.ct_bit[0];

    for (size_t i = 1; i < RANGE_BITS; ++i) {

        Fp power_of_two;
        if (i < 64) {
            power_of_two = fp_from_u64(1ULL << i);
        } else {

            power_of_two = fp_from_words(0, 1ULL << (i - 64));
        }

        auto scaled = ct_scale(pk, rp.ct_bit[i], power_of_two);
        ct_sum = ct_add(pk, ct_sum, scaled);
    }

    auto ct_lc_diff = ct_sub(pk, ct_sum, ct_value);

    rp.lc_proof = make_zero_proof(pk, sk, ct_lc_diff);

    return rp;
}

inline bool verify_range(
    const PubKey& pk,
    const Cipher& ct_value,
    const RangeProof& rp
) {
    if (!range_value_cipher_ok(pk, ct_value)) return false;

    if (rp.ct_bit.size() != RANGE_BITS) return false;
    if (rp.bit_proofs.size() != RANGE_BITS) return false;
    for (const auto& ct_bit : rp.ct_bit)
        if (!range_bit_cipher_ok(pk, ct_bit, ct_value.slots))
            return false;

    unsigned hw = std::thread::hardware_concurrency();
    unsigned n_threads = (hw > 1) ? std::min(hw, (unsigned)RANGE_BITS) : 1;
    std::vector<bool> results(RANGE_BITS, false);

    if (n_threads <= 1) {
        for (size_t i = 0; i < RANGE_BITS; ++i) {
            auto ct_b_m1 = ct_sub_const(pk, rp.ct_bit[i], (uint64_t)1);
            uint8_t mul_seed[32];
            for (int k = 0; k < 32; ++k)
                mul_seed[k] = (uint8_t)((i * 37 + k * 13 + 0xA0) & 0xFF);
            auto ct_check = ct_mul_seeded(pk, rp.ct_bit[i], ct_b_m1, mul_seed);
            if (!verify_zero(pk, ct_check, rp.bit_proofs[i]))
                return false;
        }
    } else {
        auto worker = [&](size_t from, size_t to) {
            for (size_t i = from; i < to; ++i) {
                auto ct_b_m1 = ct_sub_const(pk, rp.ct_bit[i], (uint64_t)1);
                uint8_t mul_seed[32];
                for (int k = 0; k < 32; ++k)
                    mul_seed[k] = (uint8_t)((i * 37 + k * 13 + 0xA0) & 0xFF);
                auto ct_check = ct_mul_seeded(pk, rp.ct_bit[i], ct_b_m1, mul_seed);
                results[i] = verify_zero(pk, ct_check, rp.bit_proofs[i]);
            }
        };

        std::vector<std::thread> threads;
        size_t chunk = (RANGE_BITS + n_threads - 1) / n_threads;
        for (unsigned t = 0; t < n_threads; ++t) {
            size_t from = t * chunk;
            size_t to = std::min(from + chunk, RANGE_BITS);
            if (from < to)
                threads.emplace_back(worker, from, to);
        }
        for (auto& th : threads)
            th.join();

        for (size_t i = 0; i < RANGE_BITS; ++i) {
            if (!results[i]) return false;
        }
    }

    Cipher ct_sum = rp.ct_bit[0];

    for (size_t i = 1; i < RANGE_BITS; ++i) {
        Fp power_of_two;
        if (i < 64) {
            power_of_two = fp_from_u64(1ULL << i);
        } else {
            power_of_two = fp_from_words(0, 1ULL << (i - 64));
        }

        auto scaled = ct_scale(pk, rp.ct_bit[i], power_of_two);
        ct_sum = ct_add(pk, ct_sum, scaled);
    }

    auto ct_lc_diff = ct_sub(pk, ct_sum, ct_value);

    if (!verify_zero(pk, ct_lc_diff, rp.lc_proof)) {
        return false;
    }

    return true;
}

struct AggregatedRangeProof {
    std::vector<Cipher> ct_bit;
    bp::R1CSProof proof;
};

namespace detail {

struct BitPrepData {
    Cipher ct_check;
    std::vector<std::vector<Fp>> rinv;
    std::vector<std::vector<Fp>> A;
    std::vector<size_t> bases;
};

inline void prepare_bit(
    const PubKey& pk, const SecKey& sk,
    const Cipher& ct_bit_i, size_t i,
    BitPrepData& out
) {
    auto ct_b_m1 = ct_sub_const(pk, ct_bit_i, (uint64_t)1);
    uint8_t mul_seed[32];
    for (int k = 0; k < 32; ++k)
        mul_seed[k] = (uint8_t)((i * 37 + k * 13 + 0xA0) & 0xFF);
    out.ct_check = ct_mul_seeded(pk, ct_bit_i, ct_b_m1, mul_seed);

    size_t nL = out.ct_check.L.size();
    size_t S  = out.ct_check.slots;

    std::vector<std::vector<Fp>> cache(nL);
    std::vector<uint8_t> st(nL, 0);
    for (size_t lid = 0; lid < nL; lid++)
        layer_R_cached(pk, sk, out.ct_check, (uint32_t)lid, st, cache);

    out.rinv.resize(nL);
    for (size_t lid = 0; lid < nL; lid++) {
        out.rinv[lid].resize(S);
        for (size_t j = 0; j < S; j++)
            out.rinv[lid][j] = fp_inv(cache[lid][j]);
    }
    out.A = compute_layer_coeffs(pk, out.ct_check);
    out.bases = base_layer_indices(out.ct_check);
}

inline void prepare_lc(
    const PubKey& pk, const SecKey& sk,
    const Cipher& ct_lc_diff,
    BitPrepData& out
) {
    out.ct_check = ct_lc_diff;

    size_t nL = ct_lc_diff.L.size();
    size_t S  = ct_lc_diff.slots;

    std::vector<std::vector<Fp>> cache(nL);
    std::vector<uint8_t> st(nL, 0);
    for (size_t lid = 0; lid < nL; lid++)
        layer_R_cached(pk, sk, ct_lc_diff, (uint32_t)lid, st, cache);

    out.rinv.resize(nL);
    for (size_t lid = 0; lid < nL; lid++) {
        out.rinv[lid].resize(S);
        for (size_t j = 0; j < S; j++)
            out.rinv[lid][j] = fp_inv(cache[lid][j]);
    }
    out.A = compute_layer_coeffs(pk, ct_lc_diff);
    out.bases = base_layer_indices(ct_lc_diff);
}

inline Cipher compute_lc_diff(
    const PubKey& pk,
    const std::vector<Cipher>& ct_bit,
    const Cipher& ct_value
) {
    Cipher ct_sum = ct_bit[0];
    for (size_t i = 1; i < RANGE_BITS; ++i) {
        Fp power_of_two;
        if (i < 64) power_of_two = fp_from_u64(1ULL << i);
        else        power_of_two = fp_from_words(0, 1ULL << (i - 64));
        ct_sum = ct_add(pk, ct_sum, ct_scale(pk, ct_bit[i], power_of_two));
    }
    return ct_sub(pk, ct_sum, ct_value);
}

inline void append_transcript_params(
    bp::Transcript& transcript,
    const std::vector<BitPrepData>& bit_data,
    const BitPrepData& lc_data
) {
    transcript.append_u64("RANGE_BITS", RANGE_BITS);
    for (size_t i = 0; i < RANGE_BITS; ++i) {
        transcript.append_u64("nL", bit_data[i].ct_check.L.size());
        transcript.append_u64("S", bit_data[i].ct_check.slots);
        transcript.append_u64("nB", bit_data[i].bases.size());
    }
    transcript.append_u64("nL", lc_data.ct_check.L.size());
    transcript.append_u64("S", lc_data.ct_check.slots);
    transcript.append_u64("nB", lc_data.bases.size());
}

}

inline AggregatedRangeProof make_aggregated_range_proof(
    const PubKey& pk,
    const SecKey& sk,
    const Cipher& ct_value,
    uint64_t value
) {
    if (!range_value_cipher_ok(pk, ct_value))
        throw std::runtime_error("pvac: aggregated range value rejected");
    AggregatedRangeProof arp;
    arp.ct_bit.resize(RANGE_BITS);

    std::vector<detail::BitPrepData> bit_data(RANGE_BITS);

    unsigned hw = std::thread::hardware_concurrency();
    unsigned n_threads = (hw > 1) ? std::min(hw, (unsigned)RANGE_BITS) : 1;

    auto worker = [&](size_t from, size_t to) {
        for (size_t i = from; i < to; ++i) {
            uint64_t b_i = (value >> i) & 1;
            arp.ct_bit[i] = enc_value(pk, sk, b_i);
            detail::prepare_bit(pk, sk, arp.ct_bit[i], i, bit_data[i]);
        }
    };

    if (n_threads <= 1) {
        worker(0, RANGE_BITS);
    } else {
        std::vector<std::thread> threads;
        size_t chunk = (RANGE_BITS + n_threads - 1) / n_threads;
        for (unsigned t = 0; t < n_threads; ++t) {
            size_t from = t * chunk;
            size_t to = std::min(from + chunk, RANGE_BITS);
            if (from < to)
                threads.emplace_back(worker, from, to);
        }
        for (auto& th : threads)
            th.join();
    }

    auto ct_lc_diff = detail::compute_lc_diff(pk, arp.ct_bit, ct_value);
    detail::BitPrepData lc_data;
    detail::prepare_lc(pk, sk, ct_lc_diff, lc_data);

    bp::R1CSProver prover;
    for (size_t i = 0; i < RANGE_BITS; ++i) {
        detail::build_circuit(prover, bit_data[i].ct_check,
                              bit_data[i].A, bit_data[i].bases,
                              &bit_data[i].rinv, &sk);
    }
    detail::build_circuit(prover, lc_data.ct_check,
                          lc_data.A, lc_data.bases,
                          &lc_data.rinv, &sk);

    bp::Transcript transcript("pvac.range_proof.aggregated");
    detail::append_transcript_params(transcript, bit_data, lc_data);

    arp.proof = prover.prove(transcript);
    return arp;
}

inline bool verify_aggregated_range(
    const PubKey& pk,
    const Cipher& ct_value,
    const AggregatedRangeProof& arp
) {
    (void)pk;
    (void)ct_value;
    (void)arp;
    return false;
}

}