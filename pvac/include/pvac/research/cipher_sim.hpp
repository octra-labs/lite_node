// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <array>
#include <cstdint>
#include <stdexcept>
#include <vector>

#include "../core/hash.hpp"
#include "../core/seedable_rng.hpp"
#include "../core/types.hpp"
#include "../crypto/matrix.hpp"
#include "../crypto/ristretto255.hpp"

namespace pvac::research::cipher_sim {

struct Config {
    size_t slots = 1;
    size_t layers = 1;
    size_t edges = 8;
    size_t delta = 2;
    std::vector<size_t> layer_edges;
    bool wrapper = false;
    bool rcomless = true;
    bool lpn_v2 = true;
    bool delta_v2 = true;
    bool pc_blind = true;
};

struct Gate {
    bool admitted;
    bool exact;
    const char* reason;
};

struct Model {
    Cipher ct;
    std::vector<std::vector<Fp>> delta;
    std::vector<std::vector<Fp>> agg;
    std::vector<std::vector<Fp>> mask;
    std::array<uint8_t, 32> digest;
    bool exact;
};

inline Gate reject(const char* reason) {
    return {false, false, reason};
}

inline Gate admit(const Config& cfg, bool full) {
    if (!cfg.slots || !cfg.layers || !cfg.edges || !cfg.delta)
        return reject("shape");
    const size_t layer_count = cfg.wrapper ? 2 : cfg.layers;
    if (!cfg.layer_edges.empty()) {
        if (cfg.layer_edges.size() != layer_count) return reject("edge_shape");
        for (const auto count : cfg.layer_edges)
            if (!count) return reject("edge_shape");
    }
    if (full)
        return reject("full_path");
    if (!cfg.rcomless)
        return reject("rcom_binding");
    if (!cfg.lpn_v2)
        return reject("mask_profile");
    if (cfg.slots > 1 && !cfg.delta_v2)
        return reject("slot_delta_profile");
    if (!cfg.pc_blind)
        return reject("pc_blinding");
    return {true, false, "admitted"};
}

inline Scalar blind(SeedableRng& rng) {
    uint64_t w[8];
    for (auto& x : w) x = rng.u64();
    Scalar out = sc_reduce512(w);
    if (!out.v[0] && !out.v[1] && !out.v[2] && !out.v[3])
        out.v[0] = 1;
    return out;
}

inline RSeed seed(const PubKey& pk, SeedableRng& rng) {
    RSeed out{};
    out.nonce = rng.nonce128();
    out.ztag = prg_layer_ztag(pk.canon_tag, out.nonce);
    return out;
}

inline std::vector<Fp> values(SeedableRng& rng, size_t slots) {
    std::vector<Fp> out(slots);
    for (auto& x : out) x = rng.fp_nonzero();
    return out;
}

inline std::array<uint8_t, 32> digest(const PubKey& pk, const Cipher& ct) {
    Sha256 h;
    h.init();
    h.update("pvac.research.cipher.sim", 24);
    h.update(pk.H_digest.data(), pk.H_digest.size());
    sha256_acc_u64(h, pk.canon_tag);
    sha256_acc_u64(h, ct.L.size());
    for (const auto& layer : ct.L) {
        h.update(&layer.rule, 1);
        if (layer.rule == RRule::BASE) {
            sha256_acc_u64(h, layer.seed.ztag);
            sha256_acc_u64(h, layer.seed.nonce.lo);
            sha256_acc_u64(h, layer.seed.nonce.hi);
        } else {
            sha256_acc_u64(h, layer.pa);
            sha256_acc_u64(h, layer.pb);
        }
        sha256_acc_u64(h, layer.R_PC.size());
        for (const auto& point : layer.R_PC)
            h.update(point.data(), point.size());
        sha256_acc_u64(h, layer.PC.size());
        for (const auto& point : layer.PC)
            h.update(point.data(), point.size());
    }
    sha256_acc_u64(h, ct.slots);
    sha256_acc_u64(h, ct.c0.size());
    for (const auto& x : ct.c0) {
        sha256_acc_u64(h, x.lo);
        sha256_acc_u64(h, x.hi & MASK63);
    }
    sha256_acc_u64(h, ct.E.size());
    for (const auto& edge : ct.E) {
        sha256_acc_u64(h, edge.layer_id);
        sha256_acc_u64(h, edge.idx);
        h.update(&edge.ch, 1);
        sha256_acc_u64(h, edge.w.size());
        for (const auto& x : edge.w) {
            sha256_acc_u64(h, x.lo);
            sha256_acc_u64(h, x.hi & MASK63);
        }
        sha256_acc_u64(h, edge.s.nbits);
        for (const auto x : edge.s.w)
            sha256_acc_u64(h, x);
    }
    std::array<uint8_t, 32> out{};
    h.finish(out.data());
    return out;
}

inline Model build(const PubKey& pk, const Config& cfg, const uint8_t seed_bytes[32]) {
    const Gate gate = admit(cfg, false);
    if (!gate.admitted)
        throw std::invalid_argument(gate.reason);

    SeedableRng rng = make_seeded_rng(seed_bytes);
    Model out{};
    out.ct.slots = cfg.slots;
    out.ct.c0.assign(cfg.slots, fp_from_u64(0));
    const size_t layer_count = cfg.wrapper ? 2 : cfg.layers;
    out.ct.L.reserve(layer_count);
    out.delta.reserve(layer_count);
    out.mask.reserve(layer_count);

    for (size_t layer_id = 0; layer_id < layer_count; ++layer_id) {
        Layer layer{};
        layer.rule = RRule::BASE;
        layer.seed = seed(pk, rng);
        layer.R_PC.resize(cfg.slots);
        layer.PC.resize(cfg.slots);
        for (size_t slot = 0; slot < cfg.slots; ++slot) {
            const Scalar a = blind(rng);
            const Scalar b = blind(rng);
            layer.R_PC[slot] = pedersen_commit(sc_zero(), a);
            layer.PC[slot] = pedersen_commit(sc_zero(), b);
        }
        out.ct.L.push_back(layer);

        std::vector<Fp> sum(cfg.slots, fp_from_u64(0));
        std::vector<Fp> masks = values(rng, cfg.slots);
        for (size_t i = 0; i < cfg.delta; ++i) {
            auto d = values(rng, cfg.slots);
            for (size_t slot = 0; slot < cfg.slots; ++slot)
                sum[slot] = fp_add(sum[slot], d[slot]);
            out.delta.push_back(std::move(d));
        }
        out.agg.push_back(std::move(sum));
        out.mask.push_back(std::move(masks));

        const size_t edge_count = cfg.layer_edges.empty() ? cfg.edges : cfg.layer_edges[layer_id];
        for (size_t i = 0; i < edge_count; ++i) {
            Edge edge{};
            edge.layer_id = static_cast<uint32_t>(layer_id);
            edge.idx = static_cast<uint16_t>(rng.bounded(static_cast<uint64_t>(pk.prm.B)));
            edge.ch = static_cast<uint8_t>(rng.u64() & 1);
            edge.w = values(rng, cfg.slots);
            edge.s = sigma_from_H(pk, layer.seed.ztag, layer.seed.nonce, edge.idx, edge.ch, rng.u64());
            out.ct.E.push_back(std::move(edge));
        }
    }

    out.digest = digest(pk, out.ct);
    out.exact = false;
    return out;
}

inline bool valid(const PubKey& pk, const Model& model) {
    if (!is_valid_cipher_shape(model.ct)) return false;
    if (model.ct.L.empty() || model.ct.slots == 0) return false;
    for (const auto& layer : model.ct.L) {
        for (const auto byte : layer.R_com)
            if (byte) return false;
        if (layer.R_PC.size() != model.ct.slots || layer.PC.size() != model.ct.slots)
            return false;
        for (const auto& point : layer.R_PC) {
            ExtPoint p;
            if (!rist_decode(p, point)) return false;
        }
        for (const auto& point : layer.PC) {
            ExtPoint p;
            if (!rist_decode(p, point)) return false;
        }
    }
    for (const auto& edge : model.ct.E) {
        if (edge.s.nbits != static_cast<size_t>(pk.prm.m_bits)) return false;
    }
    return digest(pk, model.ct) == model.digest;
}

inline bool shape_match(const PubKey& pk, const Cipher& real, const Model& model) {
    if (!is_valid_cipher_shape(real) || !valid(pk, model)) return false;
    if (real.slots != model.ct.slots || real.L.size() != model.ct.L.size() || real.E.size() != model.ct.E.size())
        return false;
    for (size_t i = 0; i < real.L.size(); ++i) {
        if (real.L[i].rule != model.ct.L[i].rule) return false;
        if (real.L[i].R_PC.size() != model.ct.L[i].R_PC.size()) return false;
        if (real.L[i].PC.size() != model.ct.L[i].PC.size()) return false;
    }
    for (size_t i = 0; i < real.E.size(); ++i) {
        if (real.E[i].w.size() != model.ct.E[i].w.size()) return false;
        if (real.E[i].s.nbits != model.ct.E[i].s.nbits) return false;
        if (real.E[i].idx >= pk.powg_B.size()) return false;
    }
    return true;
}

}