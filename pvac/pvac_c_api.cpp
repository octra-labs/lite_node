// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include "pvac_c_api.h"

#include "include/pvac/pvac.hpp"
#include "include/pvac/crypto/legacy_profile.hpp"
#include "../lib/pvac_ffi/pvac_serialize.hpp"

#include <cstring>
#include <cstdlib>
#include <memory>
#include <new>

#define PK(h) (reinterpret_cast<pvac::PubKey*>(h))
#define SK(h) (reinterpret_cast<pvac::SecKey*>(h))
#define CT(h) (reinterpret_cast<pvac::Cipher*>(h))
#define PRM(h) (reinterpret_cast<pvac::Params*>(h))
#define ZP(h) (reinterpret_cast<pvac::ZeroProof*>(h))
#define RP(h) (reinterpret_cast<pvac::RangeProof*>(h))
#define ARP(h) (reinterpret_cast<pvac::AggregatedRangeProof*>(h))

static uint8_t* copy_bytes(const std::vector<uint8_t>& buf, size_t* len) noexcept {
    if (len)
        *len = 0;
    if (!len || buf.empty())
        return nullptr;
    auto* out = static_cast<uint8_t*>(std::malloc(buf.size()));
    if (!out)
        return nullptr;
    std::memcpy(out, buf.data(), buf.size());
    *len = buf.size();
    return out;
}

extern "C" {

pvac_params pvac_default_params(void) {
    auto* p = new (std::nothrow) pvac::Params();
    return p;
}

void pvac_keygen_from_seed(pvac_params prm, const uint8_t seed[32],
                           pvac_pubkey* pk_out, pvac_seckey* sk_out) {
    auto* pk = new pvac::PubKey();
    auto* sk = new pvac::SecKey();
    pvac::set_debug_level(0);
    pvac::keygen_from_seed(*PRM(prm), *pk, *sk, seed);
    *pk_out = pk;
    *sk_out = sk;
}

int pvac_pubkey_is_legacy_v1_profile(pvac_pubkey legacy, pvac_pubkey current) {
    try {
        if (!legacy || !current)
            return 0;
        return pvac::legacy_v1_pubkey_matches(*PK(legacy), *PK(current)) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

int pvac_pubkey_is_proof_profile(pvac_pubkey a, pvac_pubkey b) {
    try {
        if (!a || !b)
            return 0;
        return pvac::proof_profile_matches(*PK(a), *PK(b)) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

int pvac_pubkey_matches_secret_profile(
    pvac_pubkey candidate,
    pvac_pubkey current,
    pvac_seckey sk
) {
    try {
        if (!candidate || !current || !sk)
            return 0;
        return pvac::secret_profile_matches(
            *PK(candidate),
            *PK(current),
            *SK(sk)) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

pvac_cipher pvac_enc_value_seeded(pvac_pubkey pk, pvac_seckey sk,
                                  uint64_t val, const uint8_t seed[32]) {
    auto* ct = new pvac::Cipher();
    *ct = pvac::enc_value_seeded(*PK(pk), *SK(sk), val, seed);
    return ct;
}

pvac_cipher pvac_enc_zero_seeded(pvac_pubkey pk, pvac_seckey sk,
                                 const uint8_t seed[32]) {
    auto* ct = new pvac::Cipher();
    *ct = pvac::enc_zero_seeded(*PK(pk), *SK(sk), seed);
    return ct;
}

uint64_t pvac_dec_value(pvac_pubkey pk, pvac_seckey sk, pvac_cipher ct) {
    try {
        pvac::Fp r = pvac::dec_value(*PK(pk), *SK(sk), *CT(ct));
        return r.lo;
    } catch (...) {
        return 0;
    }
}

int pvac_dec_value_i64(pvac_pubkey pk, pvac_seckey sk, pvac_cipher ct,
                       int64_t* value_out) {
    if (!value_out)
        return 0;
    try {
        pvac::Fp value = pvac::dec_value(*PK(pk), *SK(sk), *CT(ct));
        return pvac::fp_to_i64(value, *value_out) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

int pvac_dec_value_legacy_i64(pvac_pubkey pk, pvac_seckey sk, pvac_cipher ct,
                              int64_t* value_out) {
    if (!value_out)
        return 0;
    try {
        pvac::Fp value = pvac::dec_value_legacy_mask(*PK(pk), *SK(sk), *CT(ct));
        return pvac::fp_to_i64(value, *value_out) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

void pvac_dec_value_fp(pvac_pubkey pk, pvac_seckey sk, pvac_cipher ct,
                       uint64_t* lo_out, uint64_t* hi_out) {
    if (!lo_out || !hi_out)
        return;
    try {
        pvac::Fp r = pvac::dec_value(*PK(pk), *SK(sk), *CT(ct));
        *lo_out = r.lo;
        *hi_out = r.hi;
    } catch (...) {
        *lo_out = 0;
        *hi_out = 0;
    }
}

pvac_cipher pvac_enc_value_fp_seeded(pvac_pubkey pk, pvac_seckey sk,
                                     uint64_t lo, uint64_t hi,
                                     const uint8_t seed[32]) {
    pvac::SeedableRng rng = pvac::make_seeded_rng(seed);
    pvac::Fp v;
    v.lo = lo;
    v.hi = hi;
    std::vector<pvac::Fp> vals = {v};
    std::vector<pvac::Fp> m = {rng.fp_nonzero()};
    auto* ct = new pvac::Cipher();
    *ct = pvac::combine_ciphers(*PK(pk),
        pvac::enc_fp_depth_seeded(*PK(pk), *SK(sk), pvac::field::Op::add(vals, m), 0, rng),
        pvac::enc_fp_depth_seeded(*PK(pk), *SK(sk), pvac::field::Op::neg(m), 0, rng));
    return ct;
}

pvac_cipher pvac_ct_add(pvac_pubkey pk, pvac_cipher a, pvac_cipher b) {
    auto* ct = new pvac::Cipher();
    *ct = pvac::ct_add(*PK(pk), *CT(a), *CT(b));
    return ct;
}

pvac_cipher pvac_ct_sub(pvac_pubkey pk, pvac_cipher a, pvac_cipher b) {
    auto* ct = new pvac::Cipher();
    *ct = pvac::ct_sub(*PK(pk), *CT(a), *CT(b));
    return ct;
}

pvac_cipher pvac_ct_mul_seeded(pvac_pubkey pk, pvac_cipher a, pvac_cipher b, const uint8_t seed[32]) {
    auto* ct = new pvac::Cipher();
    *ct = pvac::ct_mul_seeded(*PK(pk), *CT(a), *CT(b), seed);
    return ct;
}

pvac_cipher pvac_ct_scale(pvac_pubkey pk, pvac_cipher ct, int64_t scalar) {
    auto* out = new pvac::Cipher();
    *out = pvac::ct_scale(*PK(pk), *CT(ct), pvac::fp_from_u64(static_cast<uint64_t>(scalar)));
    return out;
}

pvac_cipher pvac_ct_add_const(pvac_pubkey pk, pvac_cipher ct, uint64_t k_lo, uint64_t k_hi) {
    auto* out = new pvac::Cipher();
    pvac::Fp k;
    k.lo = k_lo;
    k.hi = k_hi;
    *out = *CT(ct);
    for (size_t j = 0; j < out->c0.size(); ++j)
        out->c0[j] = pvac::fp_add(out->c0[j], k);
    return out;
}

pvac_cipher pvac_ct_sub_const(pvac_pubkey pk, pvac_cipher ct, uint64_t k) {
    auto* out = new pvac::Cipher();
    *out = *CT(ct);
    pvac::Fp neg_k = pvac::fp_neg(pvac::fp_from_u64(k));
    for (size_t j = 0; j < out->c0.size(); ++j)
        out->c0[j] = pvac::fp_add(out->c0[j], neg_k);
    return out;
}

pvac_cipher pvac_ct_div_const(pvac_pubkey pk, pvac_cipher ct, uint64_t k_lo, uint64_t k_hi) {
    auto* out = new pvac::Cipher();
    pvac::Fp k;
    k.lo = k_lo;
    k.hi = k_hi;
    *out = pvac::ct_scale(*PK(pk), *CT(ct), pvac::fp_inv(k));
    return out;
}

pvac_cipher pvac_ct_square_seeded(pvac_pubkey pk, pvac_cipher ct, const uint8_t seed[32]) {
    auto* out = new pvac::Cipher();
    *out = pvac::ct_square_seeded(*PK(pk), *CT(ct), seed);
    return out;
}

void pvac_commit_ct(pvac_pubkey pk, pvac_cipher ct, uint8_t out[32]) {
    if (!out)
        return;
    auto h = pvac::commit_ct(*PK(pk), *CT(ct));
    std::memcpy(out, h.data(), 32);
}

int pvac_commit_ct_v2(pvac_pubkey pk, pvac_cipher ct, uint8_t *out, size_t out_cap, size_t *out_len) {
    if (out_len) *out_len = 32;
    if (!out || out_cap < 32) return -1;
    try {
        auto h = pvac::commit_ct(*PK(pk), *CT(ct));
        std::memcpy(out, h.data(), 32);
        return 0;
    } catch (...) {
        return -2;
    }
}

pvac_zero_proof pvac_make_zero_proof(pvac_pubkey pk, pvac_seckey sk, pvac_cipher ct) {
    auto* zp = new pvac::ZeroProof();
    *zp = pvac::make_zero_proof(*PK(pk), *SK(sk), *CT(ct));
    return zp;
}

int pvac_verify_zero(pvac_pubkey pk, pvac_cipher ct, pvac_zero_proof proof) {
    try {
        return pvac::verify_zero(*PK(pk), *CT(ct), *ZP(proof)) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

pvac_zero_proof pvac_make_zero_proof_bound(pvac_pubkey pk, pvac_seckey sk, pvac_cipher ct,
                                            uint64_t amount, const uint8_t blinding[32]) {
    pvac::Scalar blind = pvac::sc_reduce256(blinding);
    auto* zp = new pvac::ZeroProof();
    *zp = pvac::make_zero_proof_bound(*PK(pk), *SK(sk), *CT(ct), amount, blind);
    return zp;
}

int pvac_verify_zero_bound(pvac_pubkey pk, pvac_cipher ct, pvac_zero_proof proof,
                            const uint8_t amount_commitment[32]) {
    pvac::RistrettoPoint commit;
    std::memcpy(commit.data(), amount_commitment, 32);
    try {
        pvac::ExtPoint decoded_commit;
        if (!pvac::rist_decode(decoded_commit, commit))
            return 0;
        return pvac::verify_zero_bound(*PK(pk), *CT(ct), *ZP(proof), commit) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

pvac_zero_proof pvac_make_zero_proof_bound_key_switch(
    pvac_pubkey pk,
    pvac_seckey sk,
    pvac_cipher ct,
    uint64_t amount,
    const uint8_t blinding[32]
) {
    if (!pk || !sk || !ct || !blinding)
        return nullptr;
    try {
        pvac::Scalar blind = pvac::sc_reduce256(blinding);
        auto proof = std::make_unique<pvac::ZeroProof>(
            pvac::make_zero_proof_bound_key_switch(
                *PK(pk),
                *SK(sk),
                *CT(ct),
                amount,
                blind));
        return proof.release();
    } catch (...) {
        return nullptr;
    }
}

int pvac_verify_zero_bound_key_switch(
    pvac_pubkey pk,
    pvac_cipher ct,
    pvac_zero_proof proof,
    const uint8_t amount_commitment[32]
) {
    if (!pk || !ct || !proof || !amount_commitment)
        return 0;
    pvac::RistrettoPoint commit;
    std::memcpy(commit.data(), amount_commitment, 32);
    try {
        pvac::ExtPoint decoded_commit;
        if (!pvac::rist_decode(decoded_commit, commit))
            return 0;
        return pvac::verify_zero_bound_key_switch(
            *PK(pk),
            *CT(ct),
            *ZP(proof),
            commit) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

void pvac_pedersen_commit(uint64_t amount, const uint8_t blinding[32], uint8_t out[32]) {
    if (!out)
        return;
    pvac::Scalar val = pvac::bp::sc_from_u64(amount);
    pvac::Scalar blind = pvac::sc_reduce256(blinding);
    pvac::RistrettoPoint pt = pvac::pedersen_commit(val, blind);
    std::memcpy(out, pt.data(), 32);
}

int pvac_pedersen_commit_v2(uint64_t amount, const uint8_t blinding[32],
                            uint8_t *out, size_t out_cap, size_t *out_len) {
    if (out_len) *out_len = 32;
    if (!out || out_cap < 32) return -1;
    try {
        pvac::Scalar val = pvac::bp::sc_from_u64(amount);
        pvac::Scalar blind = pvac::sc_reduce256(blinding);
        pvac::RistrettoPoint pt = pvac::pedersen_commit(val, blind);
        std::memcpy(out, pt.data(), 32);
        return 0;
    } catch (...) {
        return -2;
    }
}

pvac_range_proof pvac_make_range_proof(pvac_pubkey pk, pvac_seckey sk,
                                       pvac_cipher ct, uint64_t value) {
    auto* rp = new pvac::RangeProof();
    *rp = pvac::make_range_proof(*PK(pk), *SK(sk), *CT(ct), value);
    return rp;
}

int pvac_verify_range(pvac_pubkey pk, pvac_cipher ct, pvac_range_proof proof) {
    try {
        return pvac::verify_range(*PK(pk), *CT(ct), *RP(proof)) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

uint8_t* pvac_serialize_cipher(pvac_cipher ct, size_t* len) {
    try {
        return copy_bytes(pvac_ser::serialize_cipher(*CT(ct)), len);
    } catch (...) {
        if (len)
            *len = 0;
        return nullptr;
    }
}

pvac_cipher pvac_deserialize_cipher(const uint8_t* data, size_t len) {
    try {
        auto* ct = new pvac::Cipher();
        *ct = pvac_ser::deserialize_cipher(data, len);
        return ct;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] deserialize_cipher failed: %s\n", e.what());
        return nullptr;
    } catch (...) {
        fprintf(stderr, "[pvac_c_api] deserialize_cipher failed: unknown\n");
        return nullptr;
    }
}

uint8_t* pvac_serialize_pubkey(pvac_pubkey pk, size_t* len) {
    try {
        return copy_bytes(pvac_ser::serialize_pubkey(*PK(pk)), len);
    } catch (...) {
        if (len)
            *len = 0;
        return nullptr;
    }
}

pvac_pubkey pvac_deserialize_pubkey(const uint8_t* data, size_t len) {
    try {
        auto* pk = new pvac::PubKey();
        *pk = pvac_ser::deserialize_pubkey(data, len);
        return pk;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] deserialize_pubkey failed: %s\n", e.what());
        return nullptr;
    } catch (...) {
        fprintf(stderr, "[pvac_c_api] deserialize_pubkey failed: unknown\n");
        return nullptr;
    }
}

uint8_t* pvac_serialize_seckey(pvac_seckey sk, size_t* len) {
    try {
        return copy_bytes(pvac_ser::serialize_seckey(*SK(sk)), len);
    } catch (...) {
        if (len)
            *len = 0;
        return nullptr;
    }
}

pvac_seckey pvac_deserialize_seckey(const uint8_t* data, size_t len) {
    try {
        auto* sk = new pvac::SecKey();
        *sk = pvac_ser::deserialize_seckey(data, len);
        return sk;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] deserialize_seckey failed: %s\n", e.what());
        return nullptr;
    } catch (...) {
        fprintf(stderr, "[pvac_c_api] deserialize_seckey failed: unknown\n");
        return nullptr;
    }
}

uint8_t* pvac_serialize_zero_proof(pvac_zero_proof zp, size_t* len) {
    try {
        pvac_ser::Writer w;
        pvac_ser::write_zero_proof_raw(w, *ZP(zp));
        return copy_bytes(w.buf, len);
    } catch (...) {
        if (len)
            *len = 0;
        return nullptr;
    }
}

pvac_zero_proof pvac_deserialize_zero_proof(const uint8_t* data, size_t len) {
    try {
        pvac_ser::Reader r(data, len);
        auto* zp = new pvac::ZeroProof();
        *zp = pvac_ser::read_zero_proof_raw(r);
        if (r.failed) {
            delete zp;
            fprintf(stderr, "[pvac_c_api] deserialize_zero_proof failed: %s\n", r.error);
            return nullptr;
        }
        return zp;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] deserialize_zero_proof failed: %s\n", e.what());
        return nullptr;
    } catch (...) {
        fprintf(stderr, "[pvac_c_api] deserialize_zero_proof failed: unknown\n");
        return nullptr;
    }
}

uint8_t* pvac_serialize_range_proof(pvac_range_proof rp, size_t* len) {
    try {
        return copy_bytes(pvac_ser::serialize_range_proof(*RP(rp)), len);
    } catch (...) {
        if (len)
            *len = 0;
        return nullptr;
    }
}

pvac_range_proof pvac_deserialize_range_proof(const uint8_t* data, size_t len) {
    try {
        auto* rp = new pvac::RangeProof();
        *rp = pvac_ser::deserialize_range_proof(data, len);
        return rp;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] deserialize_range_proof failed: %s\n", e.what());
        return nullptr;
    } catch (...) {
        fprintf(stderr, "[pvac_c_api] deserialize_range_proof failed: unknown\n");
        return nullptr;
    }
}

pvac_agg_range_proof pvac_make_aggregated_range_proof(pvac_pubkey pk, pvac_seckey sk,
                                                       pvac_cipher ct, uint64_t value) {
    auto* arp = new pvac::AggregatedRangeProof();
    *arp = pvac::make_aggregated_range_proof(*PK(pk), *SK(sk), *CT(ct), value);
    return arp;
}

int pvac_verify_aggregated_range(pvac_pubkey pk, pvac_cipher ct, pvac_agg_range_proof proof) {
    try {
        return pvac::verify_aggregated_range(*PK(pk), *CT(ct), *ARP(proof)) ? 1 : 0;
    } catch (...) {
        return 0;
    }
}

uint8_t* pvac_serialize_agg_range_proof(pvac_agg_range_proof arp, size_t* len) {
    try {
        return copy_bytes(pvac_ser::serialize_agg_range_proof(*ARP(arp)), len);
    } catch (...) {
        if (len)
            *len = 0;
        return nullptr;
    }
}

pvac_agg_range_proof pvac_deserialize_agg_range_proof(const uint8_t* data, size_t len) {
    try {
        auto* arp = new pvac::AggregatedRangeProof();
        *arp = pvac_ser::deserialize_agg_range_proof(data, len);
        return arp;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] deserialize_agg_range_proof failed: %s\n", e.what());
        return nullptr;
    }
}

int pvac_verify_range_any(pvac_pubkey pk, pvac_cipher ct,
                           const uint8_t* proof_data, size_t proof_len) {
    try {
        if (proof_len < 6 || proof_data[5] != pvac_ser::TAG_BOUND_RANGE_PROOF)
            return 0;
        auto rpa = pvac_ser::deserialize_range_proof_any(proof_data, proof_len);
        if (rpa.format != pvac_ser::RP_BOUND)
            return 0;
        return pvac::verify_zero_bound_range(*PK(pk), *CT(ct), rpa.bound_proof) ? 1 : 0;
    } catch (const std::exception& e) {
        fprintf(stderr, "[pvac_c_api] verify_range_any failed: %s\n", e.what());
        return 0;
    }
}

void pvac_free_agg_range_proof(pvac_agg_range_proof p) { delete ARP(p); }

void pvac_aes_kat(uint8_t out[16]) {
    pvac::Sha256 h;
    h.init();
    const char* label = "pvac.aes.kat.key";
    h.update(label, strlen(label));
    uint8_t key[32];
    h.finish(key);

    pvac::AesCtr256 prg;
    prg.init(key, 0);
    alignas(16) uint64_t buf[2];
    buf[0] = prg.next_u64();
    buf[1] = prg.next_u64();
    memcpy(out, buf, 16);
}

void pvac_free_params(pvac_params p) { delete PRM(p); }
void pvac_free_pubkey(pvac_pubkey p) { delete PK(p); }
void pvac_free_seckey(pvac_seckey p) { delete SK(p); }
void pvac_free_cipher(pvac_cipher p) { delete CT(p); }
void pvac_free_zero_proof(pvac_zero_proof p) { delete ZP(p); }
void pvac_free_range_proof(pvac_range_proof p) { delete RP(p); }
void pvac_free_bytes(uint8_t* buf) { std::free(buf); }

}