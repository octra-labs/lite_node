// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <mcl/bn.h>
#include <stdint.h>
#include <string.h>

namespace {

constexpr size_t G1_SIZE = 64;
constexpr size_t G2_SIZE = 128;
constexpr size_t FE_SIZE = 32;
constexpr size_t MAGIC_SIZE = 6;
constexpr size_t VK_HEADER_SIZE = MAGIC_SIZE + 4;
constexpr size_t VK_FIXED_SIZE = VK_HEADER_SIZE + G1_SIZE + 3 * G2_SIZE;
constexpr size_t PROOF_SIZE = MAGIC_SIZE + G1_SIZE + G2_SIZE + G1_SIZE;
constexpr uint32_t MAX_N_PUBLIC = 32;
constexpr size_t SCRATCH_SIZE = 128;

constexpr int IO_BE = MCLBN_IO_SERIALIZE | MCLBN_IO_BIG_ENDIAN;

bool ensure_init() {
    static bool initialized = false;
    static bool init_ok = false;
    if (!initialized) {
        int ret = mclBn_init(MCL_BN_SNARK1, MCLBN_COMPILED_TIME_VAR);
        if (ret == 0) {
            mclBn_verifyOrderG1(1);
            mclBn_verifyOrderG2(1);
            init_ok = true;
        }
        initialized = true;
    }
    return init_ok;
}

bool is_all_zero(const uint8_t* buf, size_t n) {
    for (size_t i = 0; i < n; i++) {
        if (buf[i]) return false;
    }
    return true;
}

bool round_trip_canonical(const char* back, size_t n, const uint8_t* buf) {
    if (n < FE_SIZE) return false;
    size_t excess = n - FE_SIZE;
    for (size_t i = 0; i < excess; i++) {
        if (back[i] != 0) return false;
    }
    return memcmp(back + excess, buf, FE_SIZE) == 0;
}

bool fp_deserialize_strict(mclBnFp* x, const uint8_t* buf) {
    if (mclBnFp_setStr(x, (const char*)buf, FE_SIZE, IO_BE) != 0) return false;
    char back[SCRATCH_SIZE];
    size_t n = mclBnFp_getStr(back, sizeof(back), x, IO_BE);
    if (n == 0) return false;
    return round_trip_canonical(back, n, buf);
}

bool fp2_deserialize(mclBnFp2* x, const uint8_t* buf) {
    return fp_deserialize_strict(&x->d[0], buf)
        && fp_deserialize_strict(&x->d[1], buf + FE_SIZE);
}

bool g1_deserialize(mclBnG1* P, const uint8_t* buf) {
    if (is_all_zero(buf, G1_SIZE)) return false;
    if (!fp_deserialize_strict(&P->x, buf)) return false;
    if (!fp_deserialize_strict(&P->y, buf + FE_SIZE)) return false;
    mclBnFp_setInt32(&P->z, 1);
    return mclBnG1_isValid(P) != 0;
}

bool g2_deserialize(mclBnG2* P, const uint8_t* buf) {
    if (is_all_zero(buf, G2_SIZE)) return false;
    if (!fp2_deserialize(&P->x, buf)) return false;
    if (!fp2_deserialize(&P->y, buf + 2 * FE_SIZE)) return false;
    mclBnFp_setInt32(&P->z.d[0], 1);
    mclBnFp_clear(&P->z.d[1]);
    return mclBnG2_isValid(P) != 0;
}

bool fr_deserialize_strict(mclBnFr* x, const uint8_t* buf) {
    if (mclBnFr_setBigEndianMod(x, buf, FE_SIZE) != 0) return false;
    char back[SCRATCH_SIZE];
    size_t n = mclBnFr_getStr(back, sizeof(back), x, IO_BE);
    if (n == 0) return false;
    return round_trip_canonical(back, n, buf);
}

struct ParsedVK {
    uint32_t n_public;
    const uint8_t* alpha;
    const uint8_t* beta;
    const uint8_t* gamma;
    const uint8_t* delta;
    const uint8_t* ic;
    size_t ic_count;
};

struct ParsedProof {
    const uint8_t* a;
    const uint8_t* b;
    const uint8_t* c;
};

bool parse_vk(const uint8_t* buf, size_t len, ParsedVK& out) {
    if (len < VK_FIXED_SIZE) return false;
    if (memcmp(buf, "OG16V1", MAGIC_SIZE) != 0) return false;
    uint32_t n =
        ((uint32_t)buf[MAGIC_SIZE] << 24) |
        ((uint32_t)buf[MAGIC_SIZE + 1] << 16) |
        ((uint32_t)buf[MAGIC_SIZE + 2] << 8) |
        ((uint32_t)buf[MAGIC_SIZE + 3]);
    if (n > MAX_N_PUBLIC) return false;
    size_t expected = VK_FIXED_SIZE + (size_t)(n + 1) * G1_SIZE;
    if (len != expected) return false;
    out.n_public = n;
    out.alpha = buf + VK_HEADER_SIZE;
    out.beta = out.alpha + G1_SIZE;
    out.gamma = out.beta + G2_SIZE;
    out.delta = out.gamma + G2_SIZE;
    out.ic = out.delta + G2_SIZE;
    out.ic_count = (size_t)n + 1;
    return true;
}

bool parse_proof(const uint8_t* buf, size_t len, ParsedProof& out) {
    if (len != PROOF_SIZE) return false;
    if (memcmp(buf, "OG16P1", MAGIC_SIZE) != 0) return false;
    out.a = buf + MAGIC_SIZE;
    out.b = out.a + G1_SIZE;
    out.c = out.b + G2_SIZE;
    return true;
}

bool parse_inputs_shape(size_t len, uint32_t expected_n) {
    if (len % FE_SIZE != 0) return false;
    return (uint32_t)(len / FE_SIZE) == expected_n;
}

bool groth16_verify_bn254_impl(
    const uint8_t* vk_bytes, size_t vk_len,
    const uint8_t* proof_bytes, size_t proof_len,
    const uint8_t* inputs_bytes, size_t inputs_len) {
    if (!ensure_init()) return false;

    ParsedVK vk;
    if (!parse_vk(vk_bytes, vk_len, vk)) return false;
    ParsedProof pf;
    if (!parse_proof(proof_bytes, proof_len, pf)) return false;
    if (!parse_inputs_shape(inputs_len, vk.n_public)) return false;

    mclBnG1 alpha;
    mclBnG2 beta, gamma, delta;
    if (!g1_deserialize(&alpha, vk.alpha)) return false;
    if (!g2_deserialize(&beta, vk.beta)) return false;
    if (!g2_deserialize(&gamma, vk.gamma)) return false;
    if (!g2_deserialize(&delta, vk.delta)) return false;

    mclBnG1 a, c;
    mclBnG2 b;
    if (!g1_deserialize(&a, pf.a)) return false;
    if (!g2_deserialize(&b, pf.b)) return false;
    if (!g1_deserialize(&c, pf.c)) return false;

    mclBnG1 L;
    if (!g1_deserialize(&L, vk.ic)) return false;
    for (uint32_t i = 0; i < vk.n_public; i++) {
        mclBnFr scalar;
        if (!fr_deserialize_strict(&scalar, inputs_bytes + (size_t)i * FE_SIZE)) return false;
        mclBnG1 ic_i;
        if (!g1_deserialize(&ic_i, vk.ic + (size_t)(i + 1) * G1_SIZE)) return false;
        mclBnG1 term;
        mclBnG1_mul(&term, &ic_i, &scalar);
        mclBnG1_add(&L, &L, &term);
    }

    mclBnG1 neg_a;
    mclBnG1_neg(&neg_a, &a);

    mclBnG1 g1s[4];
    mclBnG2 g2s[4];
    memcpy(&g1s[0], &neg_a, sizeof(mclBnG1));
    memcpy(&g1s[1], &alpha, sizeof(mclBnG1));
    memcpy(&g1s[2], &L, sizeof(mclBnG1));
    memcpy(&g1s[3], &c, sizeof(mclBnG1));
    memcpy(&g2s[0], &b, sizeof(mclBnG2));
    memcpy(&g2s[1], &beta, sizeof(mclBnG2));
    memcpy(&g2s[2], &gamma, sizeof(mclBnG2));
    memcpy(&g2s[3], &delta, sizeof(mclBnG2));

    mclBnGT result;
    mclBn_millerLoopVec(&result, g1s, g2s, 4);
    mclBn_finalExp(&result, &result);

    return mclBnGT_isOne(&result) != 0;
}

}

extern "C" {

CAMLprim value caml_zk_groth16_verify_bn254(value vk_v, value proof_v, value inputs_v) {
    CAMLparam3(vk_v, proof_v, inputs_v);
    const uint8_t* vk_p = (const uint8_t*)Bytes_val(vk_v);
    size_t vk_len = caml_string_length(vk_v);
    const uint8_t* pf_p = (const uint8_t*)Bytes_val(proof_v);
    size_t pf_len = caml_string_length(proof_v);
    const uint8_t* in_p = (const uint8_t*)Bytes_val(inputs_v);
    size_t in_len = caml_string_length(inputs_v);
    bool ok = groth16_verify_bn254_impl(vk_p, vk_len, pf_p, pf_len, in_p, in_len);
    CAMLreturn(Val_bool(ok ? 1 : 0));
}

}