// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <cstring>
#include <string>
#include <vector>
#include <stdexcept>
#include "pvac/pvac.hpp"
#include "pvac/core/pvac_compress.hpp"

namespace pvac_ser {

static constexpr uint8_t MAGIC[4] = {'P', 'V', 'A', 'C'};
static constexpr uint8_t VERSION_V1 = 0x01;
static constexpr uint8_t VERSION_V2 = 0x02;
static constexpr uint8_t VERSION_V3 = 0x03;
static constexpr uint8_t VERSION_V4 = 0x04;
static constexpr uint8_t VERSION_V5 = 0x05;
static constexpr uint8_t VERSION = VERSION_V3;
static constexpr uint8_t TAG_CIPHER = 0;
static constexpr uint8_t TAG_PUBKEY = 1;
static constexpr uint8_t TAG_SECKEY = 2;
static constexpr uint8_t TAG_RANGE_PROOF = 4;
static constexpr uint8_t TAG_AGG_RANGE_PROOF = 5;
static constexpr uint8_t TAG_ZERO_PROOF = 6;
static constexpr uint8_t TAG_BOUND_RANGE_PROOF = 7;
static constexpr uint64_t MAX_BITVEC_BITS = 1ULL << 20;

struct Writer {
    std::vector<uint8_t> buf;

    void u8(uint8_t v) { buf.push_back(v); }

    void u16(uint16_t v) {
        buf.push_back(v & 0xFF);
        buf.push_back((v >> 8) & 0xFF);
    }

    void u32(uint32_t v) {
        uint8_t b[4];
        for (int i = 0; i < 4; ++i) b[i] = (v >> (8 * i)) & 0xFF;
        buf.insert(buf.end(), b, b + 4);
    }

    void u64(uint64_t v) {
        uint8_t b[8];
        for (int i = 0; i < 8; ++i) b[i] = (v >> (8 * i)) & 0xFF;
        buf.insert(buf.end(), b, b + 8);
    }

    void i32(int32_t v) { u32(static_cast<uint32_t>(v)); }

    void f64(double v) {
        uint64_t bits;
        std::memcpy(&bits, &v, 8);
        u64(bits);
    }

    void raw(const uint8_t* p, size_t n) {
        buf.insert(buf.end(), p, p + n);
    }

    void fp(const pvac::Fp& x) {
        if ((x.hi & ~pvac::MASK63) != 0
            || (x.lo == UINT64_MAX && x.hi == pvac::MASK63))
            throw std::runtime_error("pvac_ser: noncanonical field element");
        u64(x.lo);
        u64(x.hi);
    }

    void scalar(const pvac::Scalar& s) {
        uint8_t b[32];
        pvac::sc_tobytes(b, s);
        raw(b, 32);
    }

    void rist_point(const pvac::RistrettoPoint& pt) {
        raw(pt.data(), 32);
    }

    void bitvec(const pvac::BitVec& bv) {
        u64(bv.nbits);
        u64(bv.w.size());
        for (auto w : bv.w) u64(w);
    }

    void header_version(uint8_t tag, uint8_t version) {
        raw(MAGIC, 4);
        u8(version);
        u8(tag);
    }

    void header(uint8_t tag) { header_version(tag, VERSION); }
};

struct Reader {
    const uint8_t* p;
    const uint8_t* end;
    bool failed;
    char error[128];

    Reader(const uint8_t* data, size_t len)
        : p(data), end(data + len), failed(false) { error[0] = 0; }

    void fail(const char* msg) {
        if (!failed) {
            failed = true;
            snprintf(error, sizeof(error), "%s", msg);
        }
    }

    void need(size_t n) {
        if (failed) return;
        if (p + n > end) fail("pvac_ser: truncated");
    }

    uint8_t u8() { need(1); if (failed) return 0; return *p++; }

    uint16_t u16() {
        need(2); if (failed) return 0;
        uint16_t v = p[0] | ((uint16_t)p[1] << 8);
        p += 2;
        return v;
    }

    uint32_t u32() {
        need(4); if (failed) return 0;
        uint32_t v = 0;
        for (int i = 0; i < 4; ++i) v |= ((uint32_t)p[i]) << (8 * i);
        p += 4;
        return v;
    }

    uint64_t u64() {
        need(8); if (failed) return 0;
        uint64_t v = 0;
        for (int i = 0; i < 8; ++i) v |= ((uint64_t)p[i]) << (8 * i);
        p += 8;
        return v;
    }

    int32_t i32() { return static_cast<int32_t>(u32()); }

    double f64() {
        uint64_t bits = u64();
        double v;
        std::memcpy(&v, &bits, 8);
        return v;
    }

    void raw(uint8_t* out, size_t n) {
        need(n); if (failed) { std::memset(out, 0, n); return; }
        std::memcpy(out, p, n);
        p += n;
    }

    pvac::Fp fp() {
        uint64_t lo = u64();
        uint64_t hi = u64();
        if (failed) return {};
        if ((hi & ~pvac::MASK63) != 0
            || (lo == UINT64_MAX && hi == pvac::MASK63)) {
            fail("pvac_ser: noncanonical field element");
            return {};
        }
        return pvac::Fp{lo, hi};
    }

    pvac::Scalar scalar() {
        uint8_t b[32];
        raw(b, 32);
        pvac::Scalar scalar = pvac::sc_from_bytes(b);
        if (!failed && !pvac::sc_is_canonical(scalar))
            fail("pvac_ser: non-canonical scalar encoding");
        return failed ? pvac::sc_zero() : scalar;
    }

    pvac::RistrettoPoint rist_point() {
        pvac::RistrettoPoint pt;
        raw(pt.data(), 32);
        if (!failed) {
            pvac::ExtPoint decoded;
            if (!pvac::rist_decode(decoded, pt))
                fail("pvac_ser: invalid Ristretto point encoding");
        }
        return pt;
    }

    pvac::BitVec bitvec() {
        pvac::BitVec bv;
        bv.nbits = u64();
        size_t nw = u64();
        if (!failed && bv.nbits > MAX_BITVEC_BITS)
            fail("pvac_ser: bitvec too large");
        check_count(nw, 8);
        size_t expected_nw = static_cast<size_t>((bv.nbits + 63) / 64);
        if (!failed && nw != expected_nw)
            fail("pvac_ser: bitvec word count mismatch");
        if (failed) return bv;
        bv.w.resize(nw);
        for (size_t i = 0; i < nw; ++i) bv.w[i] = u64();
        return bv;
    }

    size_t remaining() const { return (size_t)(end - p); }

    void check_count(size_t count, size_t elem_bytes) {
        if (failed) return;
        if (elem_bytes > 0 && count > remaining() / elem_bytes)
            fail("pvac_ser: count exceeds remaining data");
        if (count > (1ULL << 24))
            fail("pvac_ser: count exceeds maximum");
    }

    uint8_t header(uint8_t expected_tag) {
        uint8_t m[4];
        raw(m, 4);
        if (failed) return 0;
        if (std::memcmp(m, MAGIC, 4) != 0) { fail("pvac_ser: bad magic"); return 0; }
        uint8_t ver = u8();
        if (failed) return 0;
        if (ver != VERSION_V1 && ver != VERSION_V2 && ver != VERSION_V3 &&
            ver != VERSION_V4 && ver != VERSION_V5) {
            fail("pvac_ser: bad version");
            return 0;
        }
        uint8_t tag = u8();
        if (failed) return 0;
        if (tag != expected_tag) { fail("pvac_ser: wrong type tag"); return 0; }
        return ver;
    }
};

inline const char* cipher_structure_error(const pvac::Cipher& cipher) {
    if (cipher.slots == 0)
        return "pvac_ser: cipher slots must be positive";
    if (!cipher.c0.empty() && cipher.c0.size() != cipher.slots)
        return "pvac_ser: c0/slots size mismatch";
    for (size_t layer_id = 0; layer_id < cipher.L.size(); ++layer_id) {
        const auto& layer = cipher.L[layer_id];
        if (layer.rule != pvac::RRule::BASE && layer.rule != pvac::RRule::PROD)
            return "pvac_ser: invalid layer rule";
        if (layer.rule == pvac::RRule::PROD &&
            (layer.pa >= layer_id || layer.pb >= layer_id))
            return "pvac_ser: invalid product parent";
        if (layer.rule == pvac::RRule::PROD && !layer.PC.empty())
            return "pvac_ser: product layer must not contain PC";
        if (layer.rule == pvac::RRule::PROD && !layer.R_PC.empty())
            return "pvac_ser: product layer must not contain R_PC";
        if (!layer.PC.empty() && layer.PC.size() != cipher.slots)
            return "pvac_ser: layer PC/slots size mismatch";
        if (!layer.R_PC.empty() && layer.R_PC.size() != cipher.slots)
            return "pvac_ser: layer R_PC/slots size mismatch";
    }
    for (const auto& edge : cipher.E) {
        if (edge.layer_id >= cipher.L.size())
            return "pvac_ser: edge layer out of range";
        if (edge.ch != pvac::SGN_P && edge.ch != pvac::SGN_M)
            return "pvac_ser: invalid edge sign";
        if (edge.w.size() != cipher.slots)
            return "pvac_ser: edge weight/slots size mismatch";
    }
    return nullptr;
}

inline void validate_cipher_structure(const pvac::Cipher& cipher) {
    if (const char* error = cipher_structure_error(cipher))
        throw std::runtime_error(error);
}

inline const char* pubkey_structure_error(const pvac::PubKey& pk) {
    if (pk.prm.B <= 0 || pk.prm.m_bits <= 0 || pk.prm.n_bits <= 0)
        return "pvac_ser: invalid public key dimensions";
    if (pk.H.size() != static_cast<size_t>(pk.prm.n_bits))
        return "pvac_ser: H column count mismatch";
    if (pk.ubk.perm.size() != static_cast<size_t>(pk.prm.m_bits) ||
        pk.ubk.inv.size() != static_cast<size_t>(pk.prm.m_bits))
        return "pvac_ser: UBK size mismatch";
    if (!pvac::is_valid_pubkey_shape(pk))
        return "pvac_ser: invalid UBK permutation";
    if (pk.powg_B.size() != static_cast<size_t>(pk.prm.B))
        return "pvac_ser: powg_B size mismatch";
    for (const auto& column : pk.H) {
        if (column.nbits != static_cast<uint64_t>(pk.prm.m_bits))
            return "pvac_ser: H bitvec length mismatch";
    }
    return nullptr;
}

inline void validate_pubkey_structure(const pvac::PubKey& pk) {
    if (const char* error = pubkey_structure_error(pk))
        throw std::runtime_error(error);
}

inline void write_params(Writer& w, const pvac::Params& prm) {
    w.i32(prm.B);
    w.i32(prm.m_bits);
    w.i32(prm.n_bits);
    w.i32(prm.h_col_wt);
    w.i32(prm.x_col_wt);
    w.i32(prm.err_wt);
    w.f64(prm.noise_entropy_bits);
    w.f64(prm.tuple2_fraction);
    w.f64(prm.depth_slope_bits);
    w.u64(prm.edge_budget);
    w.i32(prm.lpn_n);
    w.i32(prm.lpn_t);
    w.i32(prm.lpn_tau_num);
    w.i32(prm.lpn_tau_den);
    w.f64(prm.recrypt_lo);
    w.f64(prm.recrypt_hi);
    w.i32(prm.recrypt_rounds);
}

inline pvac::Params read_params(Reader& r) {
    pvac::Params prm;
    prm.B = r.i32();
    prm.m_bits = r.i32();
    prm.n_bits = r.i32();
    prm.h_col_wt = r.i32();
    prm.x_col_wt = r.i32();
    prm.err_wt = r.i32();
    prm.noise_entropy_bits = r.f64();
    prm.tuple2_fraction = r.f64();
    prm.depth_slope_bits = r.f64();
    prm.edge_budget = r.u64();
    prm.lpn_n = r.i32();
    prm.lpn_t = r.i32();
    prm.lpn_tau_num = r.i32();
    prm.lpn_tau_den = r.i32();
    prm.recrypt_lo = r.f64();
    prm.recrypt_hi = r.f64();
    prm.recrypt_rounds = r.i32();
    return prm;
}

inline void write_layer(Writer& w, const pvac::Layer& L) {
    w.u8(static_cast<uint8_t>(L.rule));
    if (L.rule == pvac::RRule::BASE) {
        w.u64(L.seed.ztag);
        w.u64(L.seed.nonce.lo);
        w.u64(L.seed.nonce.hi);
    } else {
        w.u32(L.pa);
        w.u32(L.pb);
    }

    w.raw(L.R_com.data(), 32);

    w.u64(L.R_PC.size());
    for (const auto& rpc : L.R_PC)
        w.raw(rpc.data(), 32);

    w.u64(L.PC.size());
    for (const auto& pc : L.PC)
        w.raw(pc.data(), 32);
}

inline void write_layer_public(Writer& w, const pvac::Layer& L) {
    w.u8(static_cast<uint8_t>(L.rule));
    if (L.rule == pvac::RRule::BASE) {
        w.u64(L.seed.ztag);
        w.u64(L.seed.nonce.lo);
        w.u64(L.seed.nonce.hi);
    } else {
        w.u32(L.pa);
        w.u32(L.pb);
    }
}

inline void mark_public_base_layer(pvac::Layer& L, size_t slots) {
    if (L.rule == pvac::RRule::BASE) {
        L.R_com = {};
        L.R_PC.assign(slots, {});
        L.PC.clear();
    }
}

inline pvac::Layer read_layer(Reader& r, uint8_t ver = VERSION_V2, size_t slots = 0) {
    pvac::Layer L{};
    L.rule = static_cast<pvac::RRule>(r.u8());
    if (L.rule == pvac::RRule::BASE) {
        L.seed.ztag = r.u64();
        L.seed.nonce.lo = r.u64();
        L.seed.nonce.hi = r.u64();
    } else {
        L.pa = r.u32();
        L.pb = r.u32();
    }

    if (ver < VERSION_V4)
        r.raw(L.R_com.data(), 32);

    if (ver >= VERSION_V3 && ver < VERSION_V4) {
        size_t nRPC = r.u64();
        r.check_count(nRPC, 32);
        if (r.failed) return L;
        L.R_PC.resize(nRPC);
        for (size_t i = 0; i < nRPC; i++)
            L.R_PC[i] = r.rist_point();
    }

    if (ver >= VERSION_V2 && ver < VERSION_V4) {
        size_t nPC = r.u64();
        r.check_count(nPC, 32);
        if (r.failed) return L;
        L.PC.resize(nPC);
        for (size_t i = 0; i < nPC; i++)
            L.PC[i] = r.rist_point();
    }

    if (ver >= VERSION_V4)
        mark_public_base_layer(L, slots);

    return L;
}

inline void write_edge(Writer& w, const pvac::Edge& e) {
    w.u32(e.layer_id);
    w.u16(e.idx);
    w.u8(e.ch);
    w.u64(e.w.size());
    for (const auto& x : e.w) w.fp(x);
    w.bitvec(e.s);
}

inline pvac::Edge read_edge(Reader& r) {
    pvac::Edge e;
    e.layer_id = r.u32();
    e.idx = r.u16();
    e.ch = r.u8();
    size_t nw = r.u64();
    r.check_count(nw, 16);
    if (r.failed) return e;
    e.w.resize(nw);
    for (size_t i = 0; i < nw; ++i) e.w[i] = r.fp();
    e.s = r.bitvec();
    return e;
}

inline std::vector<uint8_t> serialize_cipher(const pvac::Cipher& C) {
    validate_cipher_structure(C);
    Writer w;
    w.header(TAG_CIPHER);
    w.u64(C.slots);
    w.u64(C.L.size());
    for (const auto& L : C.L) write_layer(w, L);
    w.u64(C.c0.size());
    for (const auto& x : C.c0) w.fp(x);
    w.u64(C.E.size());
    for (const auto& e : C.E) write_edge(w, e);
    return std::move(w.buf);
}

inline std::vector<uint8_t> serialize_cipher_public(const pvac::Cipher& C) {
    return serialize_cipher(C);
}

inline bool deserialize_cipher_checked(
    const uint8_t* data,
    size_t len,
    pvac::Cipher& cipher,
    std::string& error) {
    Reader r(data, len);
    uint8_t ver = r.header(TAG_CIPHER);
    cipher = pvac::Cipher();
    if (!r.failed) {
        cipher.slots = r.u64();
        size_t nL = r.u64();
        r.check_count(nL, 8);
        if (!r.failed) {
            cipher.L.resize(nL);
            for (size_t i = 0; i < nL; ++i)
                cipher.L[i] = read_layer(r, ver, cipher.slots);
        }
        size_t nc = r.u64();
        r.check_count(nc, 16);
        if (!r.failed) {
            cipher.c0.resize(nc);
            for (size_t i = 0; i < nc; ++i) cipher.c0[i] = r.fp();
        }
        size_t nE = r.u64();
        r.check_count(nE, 8);
        if (!r.failed) {
            cipher.E.resize(nE);
            for (size_t i = 0; i < nE; ++i) cipher.E[i] = read_edge(r);
        }
    }
    if (r.failed) {
        error = r.error;
        return false;
    }
    if (r.remaining() != 0) {
        error = "pvac_ser: trailing cipher bytes";
        return false;
    }
    if (const char* structure_error = cipher_structure_error(cipher)) {
        error = structure_error;
        return false;
    }
    error.clear();
    return true;
}

inline pvac::Cipher deserialize_cipher(const uint8_t* data, size_t len) {
    pvac::Cipher cipher;
    std::string error;
    if (!deserialize_cipher_checked(data, len, cipher, error))
        throw std::runtime_error(error);
    return cipher;
}

inline std::vector<uint8_t> serialize_pubkey_raw(const pvac::PubKey& pk) {
    Writer w;
    w.header_version(TAG_PUBKEY, VERSION_V5);
    write_params(w, pk.prm);
    w.u64(pk.canon_tag);

    w.u64(pk.H.size());
    for (const auto& bv : pk.H) w.bitvec(bv);

    w.u64(pk.ubk.perm.size());
    for (auto x : pk.ubk.perm) w.i32(x);
    w.u64(pk.ubk.inv.size());
    for (auto x : pk.ubk.inv) w.i32(x);

    w.raw(pk.H_digest.data(), 32);
    w.fp(pk.omega_B);

    w.u64(pk.powg_B.size());
    for (const auto& x : pk.powg_B) w.fp(x);

    w.rist_point(pk.circuit_prf_key_commit);
    w.u8(static_cast<uint8_t>(pk.circuit_prf_profile));

    return std::move(w.buf);
}

inline std::vector<uint8_t> serialize_pubkey(const pvac::PubKey& pk, bool compressed = true) {
    auto raw = serialize_pubkey_raw(pk);
    if (!compressed) return raw;
    return pvac::compress::pack(raw);
}

inline bool deserialize_pubkey_raw_checked(
    const uint8_t* data,
    size_t len,
    pvac::PubKey& pk,
    std::string& error) {
    Reader r(data, len);
    uint8_t ver = r.header(TAG_PUBKEY);
    pk = pvac::PubKey();
    if (!r.failed) {
        pk.prm = read_params(r);
        pk.canon_tag = r.u64();

        size_t nH = r.u64();
        r.check_count(nH, 8);
        if (!r.failed) {
            pk.H.resize(nH);
            for (size_t i = 0; i < nH; ++i) pk.H[i] = r.bitvec();
        }

        size_t np = r.u64();
        r.check_count(np, 4);
        if (!r.failed) {
            pk.ubk.perm.resize(np);
            for (size_t i = 0; i < np; ++i) pk.ubk.perm[i] = r.i32();
        }
        size_t ni = r.u64();
        r.check_count(ni, 4);
        if (!r.failed) {
            pk.ubk.inv.resize(ni);
            for (size_t i = 0; i < ni; ++i) pk.ubk.inv[i] = r.i32();
        }

        r.raw(pk.H_digest.data(), 32);
        pk.omega_B = r.fp();

        size_t ng = r.u64();
        r.check_count(ng, 16);
        if (!r.failed) {
            pk.powg_B.resize(ng);
            for (size_t i = 0; i < ng; ++i) pk.powg_B[i] = r.fp();
        }

        if (ver >= VERSION_V3)
            pk.circuit_prf_key_commit = r.rist_point();
        if (ver >= VERSION_V5)
            pk.circuit_prf_profile =
                static_cast<pvac::CircuitPrfProfile>(r.u8());
        else
            pk.circuit_prf_profile = pvac::CircuitPrfProfile::MIMC_X3_V6;
    }
    if (r.failed) {
        error = r.error;
        return false;
    }
    if (r.remaining() != 0) {
        error = "pvac_ser: trailing pubkey bytes";
        return false;
    }
    if (const char* structure_error = pubkey_structure_error(pk)) {
        error = structure_error;
        return false;
    }
    error.clear();
    return true;
}

inline pvac::PubKey deserialize_pubkey_raw(const uint8_t* data, size_t len) {
    pvac::PubKey pk;
    std::string error;
    if (!deserialize_pubkey_raw_checked(data, len, pk, error))
        throw std::runtime_error(error);
    return pk;
}

inline bool deserialize_pubkey_checked(
    const uint8_t* data,
    size_t len,
    pvac::PubKey& pk,
    std::string& error) {
    if (pvac::compress::is_packed(data, len)) {
        std::vector<uint8_t> raw;
        if (!pvac::compress::unpack_checked(data, len, raw, error))
            return false;
        return deserialize_pubkey_raw_checked(raw.data(), raw.size(), pk, error);
    }
    return deserialize_pubkey_raw_checked(data, len, pk, error);
}

inline pvac::PubKey deserialize_pubkey(const uint8_t* data, size_t len) {
    pvac::PubKey pk;
    std::string error;
    if (!deserialize_pubkey_checked(data, len, pk, error))
        throw std::runtime_error(error);
    return pk;
}

inline std::vector<uint8_t> serialize_seckey(const pvac::SecKey& sk) {
    Writer w;
    w.header_version(TAG_SECKEY, VERSION_V5);
    for (int i = 0; i < 4; ++i) w.u64(sk.prf_k[i]);
    w.fp(sk.circuit_prf_key);
    w.raw(sk.circuit_prf_key_blind.data(), 32);
    w.fp(sk.circuit_prf_key_v6);
    w.raw(sk.circuit_prf_key_blind_v6.data(), 32);
    w.u64(sk.lpn_s_bits.size());
    for (auto x : sk.lpn_s_bits) w.u64(x);
    return std::move(w.buf);
}

inline pvac::SecKey deserialize_seckey(const uint8_t* data, size_t len) {
    Reader r(data, len);
    uint8_t ver = r.header(TAG_SECKEY);
    pvac::SecKey sk;
    if (r.failed) throw std::runtime_error(r.error);
    for (int i = 0; i < 4; ++i) sk.prf_k[i] = r.u64();
    if (ver >= VERSION_V3) {
        sk.circuit_prf_key = r.fp();
        r.raw(sk.circuit_prf_key_blind.data(), 32);
    }
    if (ver >= VERSION_V5) {
        sk.circuit_prf_key_v6 = r.fp();
        r.raw(sk.circuit_prf_key_blind_v6.data(), 32);
    } else {
        sk.circuit_prf_key_v6 = sk.circuit_prf_key;
        sk.circuit_prf_key_blind_v6 = sk.circuit_prf_key_blind;
    }
    size_t ns = r.u64();
    r.check_count(ns, 8);
    if (!r.failed) {
        sk.lpn_s_bits.resize(ns);
        for (size_t i = 0; i < ns; ++i) sk.lpn_s_bits[i] = r.u64();
    }
    if (r.failed) throw std::runtime_error(r.error);
    if (r.remaining() != 0) throw std::runtime_error("pvac_ser: trailing seckey bytes");
    return sk;
}

inline void write_r1cs_proof_raw(Writer& w, const pvac::bp::R1CSProof& proof) {

    w.rist_point(proof.A_I1);
    w.rist_point(proof.A_O1);
    w.rist_point(proof.S1);

    w.rist_point(proof.T_1);
    w.rist_point(proof.T_3);
    w.rist_point(proof.T_4);
    w.rist_point(proof.T_5);
    w.rist_point(proof.T_6);

    w.scalar(proof.t_x);
    w.scalar(proof.t_x_blinding);
    w.scalar(proof.e_blinding);

    w.u64(proof.ipp.L.size());
    for (const auto& pt : proof.ipp.L) w.rist_point(pt);
    for (const auto& pt : proof.ipp.R) w.rist_point(pt);
    w.scalar(proof.ipp.a);
    w.scalar(proof.ipp.b);

    w.u64(proof.V.size());
    for (const auto& pt : proof.V) w.rist_point(pt);
}

inline pvac::bp::R1CSProof read_r1cs_proof_raw(Reader& r) {
    pvac::bp::R1CSProof proof;

    proof.A_I1 = r.rist_point();
    proof.A_O1 = r.rist_point();
    proof.S1 = r.rist_point();

    proof.T_1 = r.rist_point();
    proof.T_3 = r.rist_point();
    proof.T_4 = r.rist_point();
    proof.T_5 = r.rist_point();
    proof.T_6 = r.rist_point();

    proof.t_x = r.scalar();
    proof.t_x_blinding = r.scalar();
    proof.e_blinding = r.scalar();

    size_t nLR = r.u64();
    r.check_count(nLR, 32);
    if (!r.failed) {
        proof.ipp.L.resize(nLR);
        for (size_t i = 0; i < nLR; ++i) proof.ipp.L[i] = r.rist_point();

        proof.ipp.R.resize(nLR);
        for (size_t i = 0; i < nLR; ++i) proof.ipp.R[i] = r.rist_point();
    }
    proof.ipp.a = r.scalar();
    proof.ipp.b = r.scalar();

    size_t nV = r.u64();
    r.check_count(nV, 32);
    if (!r.failed) {
        proof.V.resize(nV);
        for (size_t i = 0; i < nV; ++i) proof.V[i] = r.rist_point();
    }

    return proof;
}

inline void write_zero_proof_raw(Writer& w, const pvac::ZeroProof& zp) {

    write_r1cs_proof_raw(w, zp.proof);

    w.u8(zp.is_bound ? 1 : 0);
}

inline pvac::ZeroProof read_zero_proof_raw(Reader& r) {
    pvac::ZeroProof zp;
    zp.proof = read_r1cs_proof_raw(r);

    zp.is_bound = (r.u8() != 0);
    return zp;
}

inline std::vector<uint8_t> serialize_zero_proof(const pvac::ZeroProof& zp) {
    Writer w;
    w.header(TAG_ZERO_PROOF);
    write_zero_proof_raw(w, zp);
    return std::move(w.buf);
}

inline pvac::ZeroProof deserialize_zero_proof(const uint8_t* data, size_t len) {
    Reader r(data, len);
    r.header(TAG_ZERO_PROOF);
    if (r.failed) throw std::runtime_error(r.error);
    auto zp = read_zero_proof_raw(r);
    if (r.failed) throw std::runtime_error(r.error);
    if (r.remaining() != 0) throw std::runtime_error("pvac_ser: trailing zero proof bytes");
    return zp;
}

inline void write_cipher_raw(Writer& w, const pvac::Cipher& C) {
    w.u64(C.slots);
    w.u64(C.L.size());
    for (const auto& L : C.L) write_layer(w, L);
    w.u64(C.c0.size());
    for (const auto& x : C.c0) w.fp(x);
    w.u64(C.E.size());
    for (const auto& e : C.E) write_edge(w, e);
}

inline pvac::Cipher read_cipher_raw(Reader& r, uint8_t ver = VERSION_V2) {
    pvac::Cipher C;
    C.slots = r.u64();
    size_t nL = r.u64();
    r.check_count(nL, 8);
    if (!r.failed) {
        C.L.resize(nL);
        for (size_t i = 0; i < nL; ++i) C.L[i] = read_layer(r, ver, C.slots);
    }
    size_t nc = r.u64();
    r.check_count(nc, 16);
    if (!r.failed) {
        C.c0.resize(nc);
        for (size_t i = 0; i < nc; ++i) C.c0[i] = r.fp();
    }
    size_t nE = r.u64();
    r.check_count(nE, 8);
    if (!r.failed) {
        C.E.resize(nE);
        for (size_t i = 0; i < nE; ++i) C.E[i] = read_edge(r);
    }
    if (r.failed)
        throw std::runtime_error(r.error);
    validate_cipher_structure(C);
    return C;
}

inline std::vector<uint8_t> serialize_range_proof(const pvac::RangeProof& rp) {
    Writer w;
    w.header(TAG_RANGE_PROOF);

    w.u64(rp.ct_bit.size());

    for (const auto& ct : rp.ct_bit) write_cipher_raw(w, ct);

    for (const auto& zp : rp.bit_proofs) write_zero_proof_raw(w, zp);

    write_zero_proof_raw(w, rp.lc_proof);

    return std::move(w.buf);
}

inline pvac::RangeProof deserialize_range_proof(const uint8_t* data, size_t len) {
    Reader r(data, len);
    uint8_t ver = r.header(TAG_RANGE_PROOF);
    if (r.failed) throw std::runtime_error(r.error);

    pvac::RangeProof rp;
    size_t nbits = r.u64();
    if (nbits != pvac::RANGE_BITS)
        r.fail("pvac_ser: unexpected range proof bit length");
    if (!r.failed)
        r.check_count(nbits, 8);

    if (!r.failed) {
        rp.ct_bit.resize(nbits);
        for (size_t i = 0; i < nbits && !r.failed; ++i)
            rp.ct_bit[i] = read_cipher_raw(r, ver);
    }

    if (!r.failed) {
        rp.bit_proofs.resize(nbits);
        for (size_t i = 0; i < nbits && !r.failed; ++i)
            rp.bit_proofs[i] = read_zero_proof_raw(r);
    }

    if (!r.failed)
        rp.lc_proof = read_zero_proof_raw(r);

    if (r.failed) throw std::runtime_error(r.error);
    if (r.remaining() != 0) throw std::runtime_error("pvac_ser: trailing range proof bytes");
    return rp;
}

inline std::vector<uint8_t> serialize_bound_range_proof(const pvac::ZeroProof& proof) {
    Writer w;
    w.header(TAG_BOUND_RANGE_PROOF);
    write_zero_proof_raw(w, proof);
    return std::move(w.buf);
}

inline pvac::ZeroProof deserialize_bound_range_proof(const uint8_t* data, size_t len) {
    Reader r(data, len);
    r.header(TAG_BOUND_RANGE_PROOF);
    if (r.failed) throw std::runtime_error(r.error);
    auto proof = read_zero_proof_raw(r);
    if (r.failed) throw std::runtime_error(r.error);
    if (r.remaining() != 0) throw std::runtime_error("pvac_ser: trailing bound range proof bytes");
    return proof;
}

inline std::vector<uint8_t> serialize_agg_range_proof(const pvac::AggregatedRangeProof& arp) {
    Writer w;
    w.header(TAG_AGG_RANGE_PROOF);
    w.u64(arp.ct_bit.size());
    for (const auto& ct : arp.ct_bit) write_cipher_raw(w, ct);
    write_r1cs_proof_raw(w, arp.proof);
    return std::move(w.buf);
}

inline pvac::AggregatedRangeProof deserialize_agg_range_proof(const uint8_t* data, size_t len) {
    Reader r(data, len);
    uint8_t ver = r.header(TAG_AGG_RANGE_PROOF);
    if (r.failed) throw std::runtime_error(r.error);

    pvac::AggregatedRangeProof arp;
    size_t nbits = r.u64();
    if (nbits != pvac::RANGE_BITS)
        r.fail("pvac_ser: unexpected range proof bit length");
    if (!r.failed)
        r.check_count(nbits, 8);
    if (!r.failed) {
        arp.ct_bit.resize(nbits);
        for (size_t i = 0; i < nbits && !r.failed; ++i)
            arp.ct_bit[i] = read_cipher_raw(r, ver);
    }
    if (!r.failed)
        arp.proof = read_r1cs_proof_raw(r);
    if (r.failed) throw std::runtime_error(r.error);
    if (r.remaining() != 0) throw std::runtime_error("pvac_ser: trailing aggregated range proof bytes");
    return arp;
}

enum RangeProofFormat { RP_OLD = 0, RP_AGGREGATED = 1, RP_BOUND = 2 };

struct RangeProofAny {
    RangeProofFormat format;
    pvac::RangeProof old_proof;
    pvac::AggregatedRangeProof agg_proof;
    pvac::ZeroProof bound_proof;
};

inline RangeProofAny deserialize_range_proof_any(const uint8_t* data, size_t len) {
    if (len < 6) throw std::runtime_error("pvac_ser: range proof too short");
    uint8_t tag = data[5];
    RangeProofAny result;
    if (tag == TAG_RANGE_PROOF) {
        result.format = RP_OLD;
        result.old_proof = deserialize_range_proof(data, len);
    } else if (tag == TAG_AGG_RANGE_PROOF) {
        result.format = RP_AGGREGATED;
        result.agg_proof = deserialize_agg_range_proof(data, len);
    } else if (tag == TAG_BOUND_RANGE_PROOF) {
        result.format = RP_BOUND;
        result.bound_proof = deserialize_bound_range_proof(data, len);
    } else {
        throw std::runtime_error("pvac_ser: unknown range proof tag");
    }
    return result;
}

}