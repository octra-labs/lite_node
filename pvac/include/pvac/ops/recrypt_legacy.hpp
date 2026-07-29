// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <iterator>
#include <stdexcept>
#include <utility>
#include <vector>

#include "../core/types.hpp"
#include "../crypto/matrix.hpp"
#include "encrypt.hpp"
#include "arithmetic.hpp"
#include "decrypt.hpp"

namespace pvac {

inline EvalKey make_evalkey(const PubKey& pk, const SecKey& sk, size_t pool_size, int depth_hint) {
    EvalKey ek;
    ek.zero_pool.reserve(pool_size);
    for (size_t i = 0; i < pool_size; ++i)
        ek.zero_pool.push_back(enc_zero_depth(pk, sk, depth_hint));
    ek.enc_one = enc_value(pk, sk, 1);
    return ek;
}

inline bool sigma_needs_balance(const PubKey& pk, const Cipher& C) {
    double d = sigma_density(pk, C);
    return d < 0.495 || d > 0.505;
}

inline Cipher ct_recrypt(const PubKey& pk, const EvalKey& ek, const Cipher& in) {
    if (ek.zero_pool.empty() || in.E.empty()) return in;

    Cipher result = in;

    for (int it = 0; it < 8 && sigma_needs_balance(pk, result); ++it) {
        size_t idx = csprng_u64() % ek.zero_pool.size();
        result = ct_add(pk, result, ek.zero_pool[idx]);
        ubk_apply(pk, result);
        guard_budget(pk, result, "recrypt");
    }

    compact_edges(pk, result);
    compact_layers(result);
    return result;
}

inline Cipher ct_recrypt_seeded(const PubKey& pk, const EvalKey& ek, const Cipher& in, const uint8_t seed[32]) {
    if (ek.zero_pool.empty() || in.E.empty()) return in;

    SeedableRng rng = make_seeded_rng(seed);
    Cipher result = in;

    for (int it = 0; it < 8 && sigma_needs_balance(pk, result); ++it) {
        size_t idx = rng.bounded(ek.zero_pool.size());
        result = ct_add(pk, result, ek.zero_pool[idx]);
        ubk_apply(pk, result);
        guard_budget(pk, result, "recrypt");
    }

    compact_edges(pk, result);
    compact_layers(result);
    return result;
}

struct RecryptKey {
    size_t slots = 0;
    std::vector<Layer> layers;
    std::vector<std::vector<Fp>> bias;
    std::vector<std::vector<std::vector<Fp>>> mix;
};

struct RecryptPublicAudit {
    size_t slots = 0;
    size_t old_layers = 0;
    size_t target_layers = 0;
    std::vector<size_t> rank;
    std::vector<size_t> quotient;
    bool reusable_public_safe = false;
};

inline bool recrypt_nonzero(const Fp& x) {
    return x.lo || x.hi;
}

inline Layer make_recrypt_layer(const PubKey& pk, const SecKey& sk, size_t slots, SeedableRng& rng) {
    Layer layer{};
    layer.rule = RRule::BASE;
    std::vector<Fp> R;
    for (size_t attempt = 0; attempt < 16 && R.empty(); ++attempt) {
        layer.seed.nonce = rng.nonce128();
        layer.seed.ztag = prg_layer_ztag(pk.canon_tag, layer.seed.nonce);
        auto candidate = circuit_prf_R_slots(pk, sk, layer.seed, slots);
        if (circuit_prf_slots_nonzero(candidate))
            R = std::move(candidate);
    }
    if (R.empty())
        throw std::runtime_error("pvac: recrypt circuit prf nonzero mask generation failed");
    layer.R_com = compute_R_com_base(pk.canon_tag, layer.seed.ztag, layer.seed.nonce.lo, layer.seed.nonce.hi, R);
    compute_layer_PC(layer, sk, R, slots);
    return layer;
}

inline std::vector<std::vector<Fp>> recrypt_layer_inv(const PubKey& pk, const SecKey& sk, const Cipher& in) {
    size_t layers = in.L.size();
    std::vector<std::vector<Fp>> cache(layers);
    std::vector<std::vector<Fp>> inv(layers);
    std::vector<uint8_t> state(layers, 0);
    for (size_t i = 0; i < layers; ++i) {
        auto R = layer_R_cached(pk, sk, in, static_cast<uint32_t>(i), state, cache);
        inv[i].resize(in.slots);
        for (size_t j = 0; j < in.slots; ++j) {
            inv[i][j] = fp_inv(R[j]);
        }
    }
    return inv;
}

inline std::vector<std::vector<Fp>> recrypt_new_inv(const PubKey& pk, const SecKey& sk, const std::vector<Layer>& layers, size_t slots) {
    std::vector<std::vector<Fp>> inv(layers.size(), field::Op::zeros(slots));
    for (size_t i = 0; i < layers.size(); ++i) {
        auto R = layers[i].R_PC.empty() ?
            prf_R_slots(pk, sk, layers[i].seed, slots) :
            circuit_prf_R_slots(pk, sk, layers[i].seed, slots);
        for (size_t j = 0; j < slots; ++j) {
            inv[i][j] = fp_inv(R[j]);
        }
    }
    return inv;
}

inline RecryptKey make_recrypt_key_seeded(const PubKey& pk, const SecKey& sk, const Cipher& in, size_t out_layers, const uint8_t seed[32]) {
    if (!is_cipher_compatible_with_pubkey(pk, in))
        throw std::runtime_error("pvac: make_recrypt_key_seeded: cipher/pubkey mismatch");
    if (out_layers == 0)
        throw std::runtime_error("pvac: make_recrypt_key_seeded: out_layers is zero");

    SeedableRng rng = make_seeded_rng(seed);
    RecryptKey key;
    key.slots = in.slots;
    key.layers.reserve(out_layers);
    for (size_t i = 0; i < out_layers; ++i) {
        key.layers.push_back(make_recrypt_layer(pk, sk, in.slots, rng));
    }

    auto old_inv = recrypt_layer_inv(pk, sk, in);
    auto new_inv = recrypt_new_inv(pk, sk, key.layers, in.slots);

    key.bias.assign(in.L.size(), field::Op::zeros(in.slots));
    key.mix.assign(in.L.size(), std::vector<std::vector<Fp>>(out_layers, field::Op::zeros(in.slots)));

    for (size_t old_id = 0; old_id < in.L.size(); ++old_id) {
        auto sum = field::Op::zeros(in.slots);
        for (size_t new_id = 0; new_id < out_layers; ++new_id) {
            for (size_t j = 0; j < in.slots; ++j) {
                key.mix[old_id][new_id][j] = rng.fp_nonzero();
                sum[j] = fp_add(sum[j], fp_mul(key.mix[old_id][new_id][j], new_inv[new_id][j]));
            }
        }
        key.bias[old_id] = field::Op::sub(old_inv[old_id], sum);
    }
    return key;
}

inline bool is_recrypt_key_compatible(const Cipher& in, const RecryptKey& key) {
    if (key.slots != in.slots)
        return false;
    if (key.layers.empty())
        return false;
    if (key.bias.size() != in.L.size())
        return false;
    if (key.mix.size() != in.L.size())
        return false;
    for (size_t i = 0; i < in.L.size(); ++i) {
        if (key.bias[i].size() != in.slots)
            return false;
        if (key.mix[i].size() != key.layers.size())
            return false;
        for (const auto& row : key.mix[i]) {
            if (row.size() != in.slots)
                return false;
        }
    }
    return true;
}

inline size_t recrypt_mix_rank_slot(const RecryptKey& key, size_t slot) {
    size_t rows = key.mix.size();
    size_t cols = key.layers.size();
    std::vector<std::vector<Fp>> a(rows, std::vector<Fp>(cols, fp_from_u64(0)));
    for (size_t r = 0; r < rows; ++r) {
        for (size_t c = 0; c < cols; ++c) {
            a[r][c] = key.mix[r][c][slot];
        }
    }

    size_t rank = 0;
    for (size_t col = 0; col < cols && rank < rows; ++col) {
        size_t pivot = rows;
        for (size_t r = rank; r < rows; ++r) {
            if (recrypt_nonzero(a[r][col])) {
                pivot = r;
                break;
            }
        }
        if (pivot == rows) {
            continue;
        }
        if (pivot != rank) {
            std::swap(a[pivot], a[rank]);
        }
        Fp inv = fp_inv(a[rank][col]);
        for (size_t c = col; c < cols; ++c) {
            a[rank][c] = fp_mul(a[rank][c], inv);
        }
        for (size_t r = 0; r < rows; ++r) {
            if (r == rank || !recrypt_nonzero(a[r][col])) {
                continue;
            }
            Fp f = a[r][col];
            for (size_t c = col; c < cols; ++c) {
                a[r][c] = fp_sub(a[r][c], fp_mul(f, a[rank][c]));
            }
        }
        ++rank;
    }
    return rank;
}

inline RecryptPublicAudit audit_recrypt_public_key(const Cipher& in, const RecryptKey& key) {
    if (!is_recrypt_key_compatible(in, key))
        throw std::runtime_error("pvac: audit_recrypt_public_key: recrypt key mismatch");

    RecryptPublicAudit audit;
    audit.slots = key.slots;
    audit.old_layers = in.L.size();
    audit.target_layers = key.layers.size();
    audit.rank.reserve(key.slots);
    audit.quotient.reserve(key.slots);

    bool safe = true;
    for (size_t slot = 0; slot < key.slots; ++slot) {
        size_t rank = recrypt_mix_rank_slot(key, slot);
        audit.rank.push_back(rank);
        audit.quotient.push_back(audit.old_layers - rank);
        safe = safe && rank == audit.old_layers;
    }
    audit.reusable_public_safe = safe;
    return audit;
}

inline void require_recrypt_reusable_public_safe(const Cipher& in, const RecryptKey& key) {
    auto audit = audit_recrypt_public_key(in, key);
    if (!audit.reusable_public_safe)
        throw std::runtime_error("pvac: public recrypt key leaks quotient relations");
}

inline Cipher ct_recrypt_apply_seeded(const PubKey& pk, const RecryptKey& key, const Cipher& in, const uint8_t seed[32], size_t edges_per_layer = 8) {
    if (!is_cipher_compatible_with_pubkey(pk, in))
        throw std::runtime_error("pvac: ct_recrypt_apply_seeded: cipher/pubkey mismatch");
    if (!is_recrypt_key_compatible(in, key))
        throw std::runtime_error("pvac: ct_recrypt_apply_seeded: recrypt key mismatch");

    SeedableRng rng = make_seeded_rng(seed);
    Cipher out;
    out.slots = in.slots;
    out.L = key.layers;
    out.c0 = in.c0.empty() ? field::Op::zeros(in.slots) : in.c0;

    std::vector<std::vector<Fp>> targets(key.layers.size(), field::Op::zeros(in.slots));

    for (const auto& edge : in.E) {
        Fp gp = pk.powg_B[edge.idx];
        for (size_t j = 0; j < in.slots; ++j) {
            Fp signed_coeff = fp_mul(edge.w[j], gp);
            if (edge.ch == SGN_M) {
                signed_coeff = fp_neg(signed_coeff);
            }
            out.c0[j] = fp_add(out.c0[j], fp_mul(signed_coeff, key.bias[edge.layer_id][j]));
            for (size_t new_id = 0; new_id < key.layers.size(); ++new_id) {
                targets[new_id][j] = fp_add(targets[new_id][j], fp_mul(signed_coeff, key.mix[edge.layer_id][new_id][j]));
            }
        }
    }

    for (size_t new_id = 0; new_id < key.layers.size(); ++new_id) {
        if (!field::Op::nz(targets[new_id])) {
            continue;
        }
        auto edges = detail::emit_repack_edges(pk, static_cast<uint32_t>(new_id), out.L[new_id], targets[new_id], edges_per_layer ? edges_per_layer : 1, rng);
        std::move(edges.begin(), edges.end(), std::back_inserter(out.E));
    }

    guard_budget(pk, out, "recrypt reset");
    compact_edges(pk, out);
    compact_layers(out);
    return out;
}

inline Cipher ct_recrypt_reset_seeded(const PubKey& pk, const SecKey& sk, const Cipher& in, const uint8_t seed[32], size_t out_layers = 2, size_t edges_per_layer = 8) {
    auto key = make_recrypt_key_seeded(pk, sk, in, out_layers, seed);
    return ct_recrypt_apply_seeded(pk, key, in, seed, edges_per_layer);
}

}