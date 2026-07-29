// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include "recrypt_native.hpp"
#include "recrypt_eval.hpp"
#include "recrypt_mat.hpp"

namespace pvac {

struct Rk {
    std::array<uint8_t, 32> seed = {};
    std::array<uint8_t, 32> input = {};
    std::array<uint8_t, 32> view = {};
    std::vector<Layer> layers;
    std::vector<Fp> shift;
    std::vector<std::vector<Fp>> beta;
    NativeRecryptBundle cert;
    NativeRecryptParams params;
    size_t edges = 8;
    size_t slots = 0;
    bool stmt = false;
    bool certed = false;
    bool safe = false;
};

inline std::array<uint8_t, 32> rk_view(const PubKey& pk, const Rk& rk) {
    Sha256 h;
    h.init();
    h.update("pvac.native.rk.view", std::strlen("pvac.native.rk.view"));
    h.update(rk.seed.data(), rk.seed.size());
    h.update(rk.input.data(), rk.input.size());
    sha256_acc_u64(h, rk.params.out_layers);
    sha256_acc_u64(h, rk.params.edges_per_layer);
    sha256_acc_u64(h, rk.params.max_bytes);
    sha256_acc_u64(h, rk.edges);
    sha256_acc_u64(h, rk.slots);
    sha256_acc_u64(h, rk.stmt ? 1 : 0);
    sha256_acc_u64(h, rk.certed ? 1 : 0);
    sha256_acc_u64(h, rk.safe ? 1 : 0);
    sha256_acc_u64(h, rk.layers.size());
    for (const auto& layer : rk.layers) {
        native_runtime_acc_layer(h, layer);
    }
    sha256_acc_u64(h, rk.shift.size());
    for (const auto& x : rk.shift) {
        native_runtime_acc_fp(h, x);
    }
    sha256_acc_u64(h, rk.beta.size());
    for (const auto& row : rk.beta) {
        sha256_acc_u64(h, row.size());
        for (const auto& x : row) {
            native_runtime_acc_fp(h, x);
        }
    }
    if (rk.certed) {
        auto out_digest = native_runtime_cipher_digest(pk, rk.cert.output);
        auto stmt_digest = native_reset_stmt_digest(rk.cert.stmt);
        auto reset_digest = native_reset_output_digest(rk.cert.reset_out);
        auto trace_digest = native_reset_proof_trace_digest(rk.cert.proof);
        h.update(out_digest.data(), out_digest.size());
        h.update(stmt_digest.data(), stmt_digest.size());
        h.update(reset_digest.data(), reset_digest.size());
        h.update(rk.cert.cert.relation_digest.data(), rk.cert.cert.relation_digest.size());
        h.update(trace_digest.data(), trace_digest.size());
        h.update(rk.cert.material.material_digest.data(), rk.cert.material.material_digest.size());
        h.update(rk.cert.handoff.handoff_digest.data(), rk.cert.handoff.handoff_digest.size());
        h.update(rk.cert.eq.proof_digest.data(), rk.cert.eq.proof_digest.size());
        h.update(rk.cert.reusable.output_digest.data(), rk.cert.reusable.output_digest.size());
        sha256_acc_u64(h, rk.cert.claim.production ? 1 : 0);
        sha256_acc_u64(h, rk.cert.claim.admitted ? 1 : 0);
    }
    std::array<uint8_t, 32> out{};
    h.finish(out.data());
    return out;
}

inline Cipher rk_apply(const PubKey& pk, const Rk& rk, const Cipher& ct) {
    SeedableRng rng = make_seeded_rng(rk.seed.data());
    Cipher out;
    out.slots = ct.slots;
    out.L = rk.layers;
    out.c0 = ct.c0.empty() ? field::Op::zeros(ct.slots) : ct.c0;
    out.c0 = field::Op::add(out.c0, rk.shift);
    for (size_t new_id = 0; new_id < rk.layers.size(); ++new_id) {
        if (!field::Op::nz(rk.beta[new_id]))
            continue;
        auto repacked = detail::emit_repack_edges(pk, static_cast<uint32_t>(new_id), out.L[new_id], rk.beta[new_id], rk.edges ? rk.edges : 1, rng);
        std::move(repacked.begin(), repacked.end(), std::back_inserter(out.E));
    }
    guard_budget(pk, out, "recrypt");
    compact_edges(pk, out);
    compact_layers(out);
    return out;
}

inline bool rk_safe(const PubKey& pk, const Cipher& ct, const Rk& rk) {
    if (!is_cipher_compatible_with_pubkey(pk, ct))
        return false;
    if (!native_chosen_digest_eq(rk.input, native_runtime_cipher_digest(pk, ct)))
        return false;
    if (rk.slots == 0 || rk.slots != ct.slots)
        return false;
    if (rk.layers.empty() || rk.shift.size() != rk.slots)
        return false;
    if (rk.beta.size() != rk.layers.size())
        return false;
    for (const auto& row : rk.beta) {
        if (row.size() != rk.slots)
            return false;
    }
    if (!rk.stmt || !rk.safe || !rk.certed)
        return false;
    if (!native_chosen_digest_eq(rk.view, rk_view(pk, rk)))
        return false;
    if (!verify_native_recrypt(pk, ct, rk.cert, rk.params))
        return false;
    try {
        auto out = rk_apply(pk, rk, ct);
        return native_chosen_digest_eq(native_runtime_cipher_digest(pk, out), native_runtime_cipher_digest(pk, rk.cert.output));
    } catch (...) {
        return false;
    }
}

inline Rk make_rk(const PubKey& pk, const SecKey& sk, const Cipher& ct, const uint8_t seed[32], size_t out_layers, size_t edges = 8) {
    if (!is_cipher_compatible_with_pubkey(pk, ct))
        throw std::runtime_error("pvac: public recrypt input rejected");
    Rk rk;
    auto key = make_recrypt_key_seeded(pk, sk, ct, out_layers, seed);
    NativeRecryptParams params;
    params.out_layers = out_layers;
    params.edges_per_layer = edges;
    auto alpha = compute_layer_coeffs(pk, ct);
    if (alpha.size() != key.bias.size())
        throw std::runtime_error("pvac: public recrypt statement rejected");
    for (size_t i = 0; i < rk.seed.size(); ++i) {
        rk.seed[i] = seed[i];
    }
    rk.input = native_runtime_cipher_digest(pk, ct);
    rk.layers = key.layers;
    rk.shift = field::Op::zeros(ct.slots);
    rk.beta.assign(key.layers.size(), field::Op::zeros(ct.slots));
    rk.params = params;
    rk.edges = edges;
    rk.slots = ct.slots;
    for (size_t old_id = 0; old_id < alpha.size(); ++old_id) {
        for (size_t j = 0; j < ct.slots; ++j) {
            rk.shift[j] = fp_add(rk.shift[j], fp_mul(alpha[old_id][j], key.bias[old_id][j]));
            for (size_t new_id = 0; new_id < key.layers.size(); ++new_id) {
                rk.beta[new_id][j] = fp_add(rk.beta[new_id][j], fp_mul(alpha[old_id][j], key.mix[old_id][new_id][j]));
            }
        }
    }
    rk.stmt = true;
    rk.safe = true;
    rk.cert = native_recrypt_seeded(pk, sk, ct, seed, params);
    rk.certed = true;
    rk.view = rk_view(pk, rk);
    return rk;
}

inline Cipher recrypt(const PubKey& pk, const Rk& rk, const Cipher& ct) {
    if (!rk_safe(pk, ct, rk))
        throw std::runtime_error("pvac: public recrypt key unsafe");
    return rk_apply(pk, rk, ct);
}

inline bool recrypt_verify(const PubKey& pk, const Rk& rk, const Cipher& ct, const Cipher& clean) {
    try {
        auto expected = recrypt(pk, rk, ct);
        if (!native_chosen_digest_eq(native_runtime_cipher_digest(pk, expected), native_runtime_cipher_digest(pk, clean)))
            return false;
        if (!rk.certed)
            return false;
        return native_chosen_digest_eq(native_runtime_cipher_digest(pk, rk.cert.output), native_runtime_cipher_digest(pk, clean));
    } catch (...) {
        return false;
    }
}

NativeRecryptBundle recrypt(const PubKey& pk, const SecKey& sk, const Cipher& ct, const uint8_t seed[32], const NativeRecryptParams& params = NativeRecryptParams{}) = delete;

}