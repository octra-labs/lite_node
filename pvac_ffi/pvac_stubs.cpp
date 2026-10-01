// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>
}

#include "pvac/pvac.hpp"
#include "pvac/ops/recrypt_legacy.hpp"
#include "pvac_serialize.hpp"

#include <cstring>
#include <new>
#include <stdexcept>
#include <cstdio>
#ifdef __APPLE__
#include <mach/mach.h>
static size_t get_rss_mb() {
    struct mach_task_basic_info info;
    mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
    if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO, (task_info_t)&info, &count) == KERN_SUCCESS)
        return info.resident_size / (1024 * 1024);
    return 0;
}
#else
#include <cstdio>
#ifdef __linux__
#include <signal.h>
#include <sys/prctl.h>
#include <unistd.h>
#endif
static size_t get_rss_mb() {
    FILE* f = fopen("/proc/self/statm", "r");
    if (!f) return 0;
    long dummy = 0, pages = 0;
    if (fscanf(f, "%ld %ld", &dummy, &pages) != 2) pages = 0;
    fclose(f);
    return (size_t)(pages * 4096 / (1024 * 1024));
}
#endif

extern "C" CAMLprim value caml_pvac_worker_isolate(value v_unit) {
    CAMLparam1(v_unit);
#ifdef __linux__
    if (prctl(PR_SET_PDEATHSIG, SIGKILL) != 0) {
        caml_failwith("pvac worker parent binding failed");
    }
    if (getppid() == 1) {
        kill(getpid(), SIGKILL);
    }
#endif
    CAMLreturn(Val_unit);
}

#ifdef PVAC_DEBUG
#define DBG_ENTER(name) fprintf(stderr, "[pvac_ffi] >> %s rss = %zu MB\n", name, get_rss_mb())
#define DBG_EXIT(name) fprintf(stderr, "[pvac_ffi] << %s rss = %zu MB\n", name, get_rss_mb())
#define DBG_SIZE(name, val) fprintf(stderr, "event = pvac_size name = %s count = %zu\n", name, (size_t)(val))
#else
#define DBG_ENTER(name) ((void)0)
#define DBG_EXIT(name) ((void)0)
#define DBG_SIZE(name, val) ((void)0)
#endif

#define Handle_val(T, v) (*((T**) Data_custom_val(v)))

template<typename T>
static void handle_finalize(value v) {
    T* ptr = Handle_val(T, v);
    if (ptr) delete ptr;
}

static struct custom_operations pubkey_ops = {
    (char*)"pvac.pubkey",
    [](value v) { handle_finalize<pvac::PubKey>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations seckey_ops = {
    (char*)"pvac.seckey",
    [](value v) { handle_finalize<pvac::SecKey>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations evalkey_ops = {
    (char*)"pvac.evalkey",
    [](value v) { handle_finalize<pvac::EvalKey>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations cipher_ops = {
    (char*)"pvac.cipher",
    [](value v) { handle_finalize<pvac::Cipher>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations params_ops = {
    (char*)"pvac.params",
    [](value v) { handle_finalize<pvac::Params>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations zero_proof_ops = {
    (char*)"pvac.zero_proof",
    [](value v) { handle_finalize<pvac::ZeroProof>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations range_proof_ops = {
    (char*)"pvac.range_proof",
    [](value v) { handle_finalize<pvac::RangeProof>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

static struct custom_operations agg_range_proof_ops = {
    (char*)"pvac.agg_range_proof",
    [](value v) { handle_finalize<pvac::AggregatedRangeProof>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

template<typename T>
static value wrap(struct custom_operations* ops, T* ptr, size_t mem = 1024) {
    value v = caml_alloc_custom_mem(ops, sizeof(T*), mem > 0 ? mem : 1024);
    Handle_val(T, v) = ptr;
    return v;
}

template<typename T>
static size_t vec_mem(const std::vector<T>& v) {
    return v.capacity() * sizeof(T);
}

static size_t bitvec_mem(const pvac::BitVec& b) {
    return sizeof(b) + vec_mem(b.w);
}

static size_t r1cs_proof_mem(const pvac::bp::R1CSProof& p) {
    return sizeof(p) + vec_mem(p.ipp.L) + vec_mem(p.ipp.R) + vec_mem(p.V);
}

static size_t zero_proof_mem(const pvac::ZeroProof& zp) {
    return sizeof(zp) + r1cs_proof_mem(zp.proof);
}

static size_t layer_mem(const pvac::Layer& l) {
    return sizeof(l) + vec_mem(l.R_PC) + vec_mem(l.PC);
}

static size_t edge_mem(const pvac::Edge& e) {
    return sizeof(e) + vec_mem(e.w) + bitvec_mem(e.s);
}

static size_t cipher_mem(const pvac::Cipher& c) {
    size_t n = sizeof(c) + vec_mem(c.L) + vec_mem(c.E) + vec_mem(c.c0);
    for (const auto& l : c.L) n += layer_mem(l);
    for (const auto& e : c.E) n += edge_mem(e);
    return n;
}

static size_t pubkey_mem(const pvac::PubKey& pk) {
    size_t n = sizeof(pk) + vec_mem(pk.H) + vec_mem(pk.ubk.perm) + vec_mem(pk.ubk.inv) + vec_mem(pk.powg_B);
    for (const auto& h : pk.H) n += bitvec_mem(h);
    return n;
}

static size_t seckey_mem(const pvac::SecKey& sk) {
    return sizeof(sk) + vec_mem(sk.lpn_s_bits);
}

static size_t evalkey_mem(const pvac::EvalKey& ek) {
    size_t n = sizeof(ek) + vec_mem(ek.zero_pool) + cipher_mem(ek.enc_one);
    for (const auto& c : ek.zero_pool) n += cipher_mem(c);
    return n;
}

static size_t range_proof_mem(const pvac::RangeProof& rp) {
    size_t n = sizeof(rp) + vec_mem(rp.ct_bit) + vec_mem(rp.bit_proofs) + zero_proof_mem(rp.lc_proof);
    for (const auto& c : rp.ct_bit) n += cipher_mem(c);
    for (const auto& zp : rp.bit_proofs) n += zero_proof_mem(zp);
    return n;
}

static size_t agg_range_proof_mem(const pvac::AggregatedRangeProof& arp) {
    size_t n = sizeof(arp) + vec_mem(arp.ct_bit) + r1cs_proof_mem(arp.proof);
    for (const auto& c : arp.ct_bit) n += cipher_mem(c);
    return n;
}

static const char* cipher_structure_error(const pvac::Cipher& cipher);

static bool read_layer_safe(pvac_ser::Reader& r, uint8_t ver, pvac::Layer& layer, size_t slots) {
    layer = pvac::Layer{};
    layer.rule = static_cast<pvac::RRule>(r.u8());
    if (layer.rule == pvac::RRule::BASE) {
        layer.seed.ztag = r.u64();
        layer.seed.nonce.lo = r.u64();
        layer.seed.nonce.hi = r.u64();
    } else {
        layer.pa = r.u32();
        layer.pb = r.u32();
    }
    if (ver < pvac_ser::VERSION_V4)
        r.raw(layer.R_com.data(), 32);
    if (ver >= pvac_ser::VERSION_V3 && ver < pvac_ser::VERSION_V4) {
        size_t n_rpc = r.u64();
        r.check_count(n_rpc, 32);
        if (r.failed)
            return false;
        layer.R_PC.resize(n_rpc);
        for (size_t i = 0; i < n_rpc; ++i) {
            layer.R_PC[i] = r.rist_point();
            if (r.failed)
                return false;
        }
    }
    if (ver >= pvac_ser::VERSION_V2 && ver < pvac_ser::VERSION_V4) {
        size_t n_pc = r.u64();
        r.check_count(n_pc, 32);
        if (r.failed)
            return false;
        layer.PC.resize(n_pc);
        for (size_t i = 0; i < n_pc; ++i) {
            layer.PC[i] = r.rist_point();
            if (r.failed)
                return false;
        }
    }
    if (r.failed)
        return false;
    if (ver >= pvac_ser::VERSION_V4)
        pvac_ser::mark_public_base_layer(layer, slots);
    return !r.failed;
}

static bool read_cipher_safe(
    pvac_ser::Reader& r,
    uint8_t ver,
    pvac::Cipher& cipher,
    bool strict = true,
    bool cap = true
) {
    cipher = pvac::Cipher{};
    if (!pvac_ser::read_cipher_slots(r, ver, cipher.slots, strict, cap))
        return false;
    size_t n_l = r.u64();
    r.check_count(n_l, 8);
    pvac_ser::check_public_cells(r, ver, cipher.slots, n_l, strict, cap);
    if (r.failed)
        return false;
    cipher.L.resize(n_l);
    for (size_t i = 0; i < n_l; ++i)
        if (!read_layer_safe(r, ver, cipher.L[i], cipher.slots))
            return false;
    size_t n_c = r.u64();
    r.check_count(n_c, 16);
    if (r.failed)
        return false;
    cipher.c0.resize(n_c);
    for (size_t i = 0; i < n_c; ++i) {
        cipher.c0[i] = r.fp();
        if (r.failed)
            return false;
    }
    size_t n_e = r.u64();
    r.check_count(n_e, 8);
    if (r.failed)
        return false;
    cipher.E.resize(n_e);
    for (size_t i = 0; i < n_e; ++i) {
        cipher.E[i] = pvac_ser::read_edge(r);
        if (r.failed)
            return false;
    }
    return cipher_structure_error(cipher) == nullptr;
}

static bool read_range_old_safe(const uint8_t* data, size_t len, pvac::RangeProof& proof) {
    pvac_ser::Reader r(data, len);
    uint8_t ver = r.header(pvac_ser::TAG_RANGE_PROOF);
    if (r.failed)
        return false;
    size_t nbits = r.u64();
    if (nbits != pvac::RANGE_BITS)
        return false;
    r.check_count(nbits, 8);
    if (r.failed)
        return false;
    proof = pvac::RangeProof{};
    proof.ct_bit.resize(nbits);
    for (size_t i = 0; i < nbits; ++i)
        if (!read_cipher_safe(r, ver, proof.ct_bit[i]))
            return false;
    proof.bit_proofs.resize(nbits);
    for (size_t i = 0; i < nbits; ++i) {
        proof.bit_proofs[i] = pvac_ser::read_zero_proof_raw(r);
        if (r.failed)
            return false;
    }
    proof.lc_proof = pvac_ser::read_zero_proof_raw(r);
    return !r.failed;
}

static bool read_range_agg_safe(const uint8_t* data, size_t len, pvac::AggregatedRangeProof& proof) {
    pvac_ser::Reader r(data, len);
    uint8_t ver = r.header(pvac_ser::TAG_AGG_RANGE_PROOF);
    if (r.failed)
        return false;
    size_t nbits = r.u64();
    if (nbits != pvac::RANGE_BITS)
        return false;
    r.check_count(nbits, 8);
    if (r.failed)
        return false;
    proof = pvac::AggregatedRangeProof{};
    proof.ct_bit.resize(nbits);
    for (size_t i = 0; i < nbits; ++i)
        if (!read_cipher_safe(r, ver, proof.ct_bit[i]))
            return false;
    proof.proof = pvac_ser::read_r1cs_proof_raw(r);
    return !r.failed;
}

static bool parse_range_any_safe(const uint8_t* data, size_t len, pvac_ser::RangeProofAny& out) {
    try {
        if (len < 6)
            return false;
        uint8_t tag = data[5];
        out = pvac_ser::RangeProofAny{};
        if (tag != pvac_ser::TAG_BOUND_RANGE_PROOF)
            return false;
        out.format = pvac_ser::RP_BOUND;
        out.bound_proof = pvac_ser::deserialize_bound_range_proof(data, len);
        return true;
    } catch (const std::bad_alloc&) {
        throw;
    } catch (const std::exception& e) {
        return false;
    } catch (...) {
        return false;
    }
}

static bool verify_range_any_safe(
    pvac::PubKey& pk,
    pvac::Cipher& ct,
    const pvac_ser::RangeProofAny& proof,
    bool strict, pvac::ScalarRule rule) {
    try {
        if (proof.format != pvac_ser::RP_BOUND)
            return false;
        return strict
            ? pvac::verify_zero_bound_range(pk, ct, proof.bound_proof, rule)
            : pvac::verify_range_amount_prior(pk, ct, proof.bound_proof, rule);
    } catch (const std::bad_alloc&) {
        throw;
    } catch (const std::exception& e) {
        return false;
    } catch (...) {
        return false;
    }
}

static const char* cipher_structure_error(const pvac::Cipher& cipher) {
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

static const uint8_t* bytes_data(value v) {
    return (const uint8_t*) Bytes_val(v);
}

static size_t bytes_len(value v) {
    return caml_string_length(v);
}

static uint64_t nonnegative_u64(value v, const char* context) {
    int64_t raw = Int64_val(v);
    if (raw < 0) caml_failwith(context);
    return static_cast<uint64_t>(raw);
}

#include "native_call.hpp"

extern "C" {

CAMLprim value caml_pvac_default_params(value unit) {
    CAMLparam1(unit);
    DBG_ENTER("default_params");
    CAMLreturn(native_handle<pvac::Params>(&params_ops,
        [] { return pvac::Params(); }, [](const pvac::Params&) { return sizeof(pvac::Params); }));
}

CAMLprim value caml_pvac_keygen(value v_prm) {
    CAMLparam1(v_prm);
    CAMLlocal3(v_pk, v_sk, v_pair);
    DBG_ENTER("keygen");

    pvac::Params& prm = *Handle_val(pvac::Params, v_prm);
    CAMLreturn(native_keys([&] {
        Native_keys keys;
        pvac::set_debug_level(0);
        pvac::keygen(prm, keys.pk, keys.sk);
        return keys;
    }));
}

CAMLprim value caml_pvac_keygen_from_seed(value v_prm, value v_wallet_priv) {
    CAMLparam2(v_prm, v_wallet_priv);
    CAMLlocal3(v_pk, v_sk, v_pair);
    DBG_ENTER("keygen_from_seed");

    pvac::Params& prm = *Handle_val(pvac::Params, v_prm);

    if (bytes_len(v_wallet_priv) < 32) caml_failwith("wallet privkey must be >= 32 bytes");

    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_wallet_priv), sizeof(seed));
    CAMLreturn(native_keys([&] {
        Native_keys keys;
        pvac::set_debug_level(0);
        pvac::keygen_from_seed(prm, keys.pk, keys.sk, seed);
        return keys;
    }));
}

CAMLprim value caml_pvac_make_evalkey(value v_pk, value v_sk, value v_pool, value v_depth) {
    CAMLparam4(v_pk, v_sk, v_pool, v_depth);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    size_t pool_size = Long_val(v_pool);
    int depth = Int_val(v_depth);

    CAMLreturn(native_handle<pvac::EvalKey>(&evalkey_ops,
        [&] { return pvac::make_evalkey(pk, sk, pool_size, depth); }, evalkey_mem));
}

CAMLprim value caml_pvac_enc_value_seeded(value v_pk, value v_sk, value v_val, value v_seed) {
    CAMLparam4(v_pk, v_sk, v_val, v_seed);
    DBG_ENTER("enc_value_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    uint64_t val = nonnegative_u64(v_val, "enc_value_seeded: negative value");
    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");
    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_seed), sizeof(seed));
    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops,
        [&] { return pvac::enc_value_seeded(pk, sk, val, seed); }, cipher_mem));
}

CAMLprim value caml_pvac_enc_values_seeded(value v_pk, value v_sk, value v_vals, value v_seed) {
    CAMLparam4(v_pk, v_sk, v_vals, v_seed);
    DBG_ENTER("enc_values_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");
    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_seed), sizeof(seed));
    size_t n = Wosize_val(v_vals);
    for (size_t i = 0; i < n; ++i)
        nonnegative_u64(Field(v_vals, i), "enc_values_seeded: negative value");
    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        std::vector<uint64_t> vals(n);
        for (size_t i = 0; i < n; ++i) vals[i] = Int64_val(Field(v_vals, i));
        return pvac::enc_values_seeded(pk, sk, vals, seed);
    }, cipher_mem));
}

CAMLprim value caml_pvac_enc_zero_seeded(value v_pk, value v_sk, value v_seed) {
    CAMLparam3(v_pk, v_sk, v_seed);
    DBG_ENTER("enc_zero_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");
    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_seed), sizeof(seed));
    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops,
        [&] { return pvac::enc_zero_seeded(pk, sk, seed); }, cipher_mem));
}

CAMLprim value caml_pvac_dec_value(value v_pk, value v_sk, value v_ct) {
    CAMLparam3(v_pk, v_sk, v_ct);
    DBG_ENTER("dec_value");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    pvac::Fp result;
    char err_buf[256] = {0};
    bool exhausted = false;
    try {
        result = pvac::dec_value(pk, sk, ct);
    } catch (const std::bad_alloc&) {
        exhausted = true;
    } catch (const std::exception& e) {
        snprintf(err_buf, sizeof(err_buf), "%s", e.what());
    } catch (...) {
        snprintf(err_buf, sizeof(err_buf), "pvac: dec_value: unknown C++ exception");
    }
    if (exhausted) caml_raise_out_of_memory();
    if (err_buf[0]) caml_failwith(err_buf);

    DBG_EXIT("dec_value");
    int64_t decoded;
    if (!pvac::fp_to_i64(result, decoded))
        caml_failwith("pvac: decrypted field value is outside int64 range");
    CAMLreturn(caml_copy_int64(decoded));
}

CAMLprim value caml_pvac_dec_values(value v_pk, value v_sk, value v_ct) {
    CAMLparam3(v_pk, v_sk, v_ct);
    CAMLlocal3(v_arr, owner, item);
    DBG_ENTER("dec_values");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    owner = native_handle<Native_values>(&native_values_ops,
        [&] { return pvac::dec_values(pk, sk, ct); },
        [](const Native_values& values) { return sizeof(values) + vec_mem(values); });
    Native_values* results = Handle_val(Native_values, owner);
    v_arr = caml_alloc(results->size(), 0);
    for (size_t i = 0; i < results->size(); ++i) Store_field(v_arr, i, Val_unit);
    for (size_t i = 0; i < results->size(); ++i) {
        int64_t v;
        if (!pvac::fp_to_i64((*results)[i], v))
            caml_failwith("pvac: decrypted field value is outside int64 range");
        item = caml_copy_int64(v);
        Store_field(v_arr, i, item);
    }
    delete results;
    Handle_val(Native_values, owner) = nullptr;
    CAMLreturn(v_arr);
}

CAMLprim value caml_pvac_ct_add(value v_pk, value v_a, value v_b) {
    CAMLparam3(v_pk, v_a, v_b);
    DBG_ENTER("ct_add");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& a = *Handle_val(pvac::Cipher, v_a);
    pvac::Cipher& b = *Handle_val(pvac::Cipher, v_b);
    DBG_SIZE("a.layers", a.L.size());
    DBG_SIZE("a.edges", a.E.size());
    DBG_SIZE("b.layers", b.L.size());
    DBG_SIZE("b.edges", b.E.size());

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        Runtime_scope scope;
        return pvac::ct_add(pk, a, b);
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_sub(value v_pk, value v_a, value v_b) {
    CAMLparam3(v_pk, v_a, v_b);
    DBG_ENTER("ct_sub");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& a = *Handle_val(pvac::Cipher, v_a);
    pvac::Cipher& b = *Handle_val(pvac::Cipher, v_b);
    DBG_SIZE("a.layers", a.L.size());
    DBG_SIZE("a.edges", a.E.size());
    DBG_SIZE("b.layers", b.L.size());
    DBG_SIZE("b.edges", b.E.size());

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        Runtime_scope scope;
        return pvac::ct_sub(pk, a, b);
    }, cipher_mem));
}

static value multiply_cipher(bool math, size_t draws, value v_pk, value v_a, value v_b, value v_seed) {
    CAMLparam4(v_pk, v_a, v_b, v_seed);
    DBG_ENTER("ct_mul_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& a = *Handle_val(pvac::Cipher, v_a);
    pvac::Cipher& b = *Handle_val(pvac::Cipher, v_b);
    DBG_SIZE("a.layers", a.L.size());
    DBG_SIZE("a.edges", a.E.size());
    DBG_SIZE("b.layers", b.L.size());
    DBG_SIZE("b.edges", b.E.size());

    if (a.slots != b.slots)
        caml_failwith("pvac: ct_mul: slot count mismatch between operands");

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");
    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_seed), sizeof(seed));

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        Runtime_scope scope;
        return pvac::ct_mul_seeded(pk, a, b, seed, 8, math, draws);
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_mul_seeded_math(value v_math, value v_pk, value v_a, value v_b, value v_seed) {
    CAMLparam5(v_math, v_pk, v_a, v_b, v_seed);
    CAMLreturn(multiply_cipher(Bool_val(v_math), 0, v_pk, v_a, v_b, v_seed));
}

CAMLprim value caml_pvac_ct_mul_work(value v_policy, value v_pk, value v_a, value v_b, value v_seed) {
    CAMLparam5(v_policy, v_pk, v_a, v_b, v_seed);
    const intnat draws = Long_val(Field(v_policy, 1));
    if (draws <= 0 || draws > 1024)
        caml_invalid_argument("pvac: sampling effort rejected");
    CAMLreturn(multiply_cipher(Bool_val(Field(v_policy, 0)), static_cast<size_t>(draws),
        v_pk, v_a, v_b, v_seed));
}

CAMLprim value caml_pvac_ct_scale_math(value v_math, value v_pk, value v_ct, value v_scalar) {
    CAMLparam4(v_math, v_pk, v_ct, v_scalar);
    const bool math = Bool_val(v_math);
    DBG_ENTER("ct_scale");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    int64_t scalar = Int64_val(v_scalar);
    DBG_SIZE("ct.edges", ct.E.size());

    pvac::Fp s = math ? pvac::detail::fp_from_i64(scalar) : pvac::fp_from_u64(static_cast<uint64_t>(scalar));
    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        return pvac::ct_scale(pk, ct, s);
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_add_const_math(value v_math, value v_pk, value v_ct, value v_lo, value v_hi) {
    CAMLparam5(v_math, v_pk, v_ct, v_lo, v_hi);
    const bool math = Bool_val(v_math);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t lo = Int64_val(v_lo);
    uint64_t hi = Int64_val(v_hi);

    pvac::Fp k;
    k.lo = lo;
    k.hi = hi;

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        pvac::Cipher result = ct;
        if (math && result.c0.empty()) result.c0 = pvac::field::Op::zeros(result.slots);
        for (size_t j = 0; j < result.c0.size(); ++j)
            result.c0[j] = pvac::fp_add(result.c0[j], k);
        return result;
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_sub_const_math(value v_math, value v_pk, value v_ct, value v_k) {
    CAMLparam4(v_math, v_pk, v_ct, v_k);
    const bool math = Bool_val(v_math);

    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t k = Int64_val(v_k);

    pvac::Fp neg_k = pvac::fp_neg(pvac::fp_from_u64(k));

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        pvac::Cipher result = ct;
        if (math && result.c0.empty()) result.c0 = pvac::field::Op::zeros(result.slots);
        for (size_t j = 0; j < result.c0.size(); ++j)
            result.c0[j] = pvac::fp_add(result.c0[j], neg_k);
        return result;
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_div_const(value v_pk, value v_ct, value v_lo, value v_hi) {
    CAMLparam4(v_pk, v_ct, v_lo, v_hi);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t lo = Int64_val(v_lo);
    uint64_t hi = Int64_val(v_hi);

    pvac::Fp k;
    k.lo = lo;
    k.hi = hi;

    if ((k.lo | k.hi) == 0) caml_failwith("pvac: zero divisor");

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        return pvac::ct_div_const(pk, ct, k);
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_square_seeded_math(value v_math, value v_pk, value v_ct, value v_seed) {
    CAMLparam4(v_math, v_pk, v_ct, v_seed);
    const bool math = Bool_val(v_math);
    DBG_ENTER("ct_square_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");
    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_seed), sizeof(seed));

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        return pvac::ct_square_seeded(pk, ct, seed, 8, math);
    }, cipher_mem));
}

CAMLprim value caml_pvac_ct_recrypt_seeded(value v_pk, value v_ek, value v_ct, value v_seed) {
    CAMLparam4(v_pk, v_ek, v_ct, v_seed);
    DBG_ENTER("ct_recrypt_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::EvalKey& ek = *Handle_val(pvac::EvalKey, v_ek);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    DBG_SIZE("ek.zero_pool", ek.zero_pool.size());

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");
    uint8_t seed[32];
    std::memcpy(seed, bytes_data(v_seed), sizeof(seed));

    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        return pvac::ct_recrypt_seeded(pk, ek, ct, seed);
    }, cipher_mem));
}

CAMLprim value caml_pvac_commit_ct(value v_pk, value v_ct) {
    CAMLparam2(v_pk, v_ct);
    CAMLlocal1(v_bytes);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);

    CAMLreturn(native_bytes([&] {
        auto hash = pvac::commit_ct(pk, ct);
        return Native_bytes(hash.begin(), hash.end());
    }));
}

CAMLprim value caml_pvac_cipher_has_key_bound_material(value v_ct) {
    CAMLparam1(v_ct);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    if (cipher_structure_error(ct) != nullptr)
        CAMLreturn(Val_bool(false));
    if (ct.slots == 0 || ct.L.empty())
        CAMLreturn(Val_bool(false));
    bool has_base = false;
    for (const auto& layer : ct.L) {
        if (layer.rule == pvac::RRule::BASE) {
            has_base = true;
            if (layer.R_PC.size() != ct.slots || layer.PC.size() != ct.slots)
                CAMLreturn(Val_bool(false));
        }
    }
    CAMLreturn(Val_bool(has_base));
}

CAMLprim value caml_pvac_cipher_base_layers(value v_ct) {
    CAMLparam1(v_ct);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    if (cipher_structure_error(ct) != nullptr)
        caml_failwith("cipher structure rejected");
    size_t count = 0;
    for (const auto& layer : ct.L)
        if (layer.rule == pvac::RRule::BASE)
            ++count;
    if (count > static_cast<size_t>(Max_long))
        caml_failwith("cipher base layer count overflow");
    CAMLreturn(Val_long(count));
}

CAMLprim value caml_pvac_cipher_shape(value v_ct) {
    CAMLparam1(v_ct);
    CAMLlocal1(v_shape);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    if (cipher_structure_error(ct) != nullptr)
        caml_failwith("cipher structure rejected");
    size_t base_layers = 0;
    for (const auto& layer : ct.L)
        if (layer.rule == pvac::RRule::BASE)
            ++base_layers;
    const size_t max_value = static_cast<size_t>(Max_long);
    if (ct.slots > max_value || ct.L.size() > max_value ||
        ct.E.size() > max_value || ct.c0.size() > max_value ||
        base_layers > max_value)
        caml_failwith("cipher shape overflow");
    v_shape = caml_alloc_tuple(5);
    Store_field(v_shape, 0, Val_long(ct.slots));
    Store_field(v_shape, 1, Val_long(ct.L.size()));
    Store_field(v_shape, 2, Val_long(ct.E.size()));
    Store_field(v_shape, 3, Val_long(ct.c0.size()));
    Store_field(v_shape, 4, Val_long(base_layers));
    CAMLreturn(v_shape);
}

CAMLprim value caml_pvac_cipher_bit_words(value v_ct) {
    CAMLparam1(v_ct);
    const pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    size_t words = 0;
    for (const auto& edge : ct.E) {
        if (edge.s.w.size() > static_cast<size_t>(Max_long) - words)
            caml_failwith("cipher bit words overflow");
        words += edge.s.w.size();
    }
    CAMLreturn(Val_long(words));
}

CAMLprim value caml_pvac_pubkey_bit_words(value v_pk) {
    CAMLparam1(v_pk);
    const pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    if (pk.prm.m_bits <= 0)
        caml_failwith("pubkey bit size rejected");
    CAMLreturn(Val_long((static_cast<size_t>(pk.prm.m_bits) + 63) / 64));
}

CAMLprim value caml_pvac_pubkey_sampling(value v_pk) {
    CAMLparam1(v_pk);
    CAMLlocal1(v_shape);
    const pvac::Params& prm = Handle_val(pvac::PubKey, v_pk)->prm;
    v_shape = caml_alloc_tuple(5);
    Store_field(v_shape, 0, Val_long(prm.m_bits));
    Store_field(v_shape, 1, Val_long(prm.n_bits));
    Store_field(v_shape, 2, Val_long(prm.x_col_wt));
    Store_field(v_shape, 3, Val_long(prm.err_wt));
    Store_field(v_shape, 4, Val_long(prm.B));
    CAMLreturn(v_shape);
}

CAMLprim value caml_pvac_pubkey_image_size(value v_pk) {
    CAMLparam1(v_pk);
    const pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    size_t size = 219;
    const auto add = [&](size_t count, size_t width) {
        if (count > (static_cast<size_t>(Max_long) - size) / width)
            caml_failwith("pubkey image size overflow");
        size += count * width;
    };
    add(pk.H.size(), 16);
    for (const auto& row : pk.H) add(row.w.size(), 8);
    add(pk.ubk.perm.size(), 4);
    add(pk.ubk.inv.size(), 4);
    add(pk.powg_B.size(), 16);
    CAMLreturn(Val_long(size));
}

CAMLprim value caml_pvac_cipher_mul_depth(value v_ct) {
    CAMLparam1(v_ct);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    size_t depth = 0;
    if (!pvac::cipher_mul_depth(ct, depth))
        caml_failwith("cipher structure rejected");
    if (depth > static_cast<size_t>(Max_long))
        caml_failwith("cipher multiplication depth overflow");
    CAMLreturn(Val_long(depth));
}

CAMLprim value caml_pvac_cipher_is_wrapped_scalar(value v_ct) {
    CAMLparam1(v_ct);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    if (cipher_structure_error(ct) != nullptr)
        CAMLreturn(Val_bool(false));
    if (ct.slots != 1 || ct.L.size() != 2 || ct.c0.size() != 1)
        CAMLreturn(Val_bool(false));
    if (ct.c0[0].lo != 0 || ct.c0[0].hi != 0)
        CAMLreturn(Val_bool(false));
    for (const auto& layer : ct.L)
        if (layer.rule != pvac::RRule::BASE ||
            layer.R_PC.size() != 1 || layer.PC.size() != 1)
            CAMLreturn(Val_bool(false));
    CAMLreturn(Val_bool(true));
}

static bool fp_eq(const pvac::Fp& a, const pvac::Fp& b) {
    return a.lo == b.lo && a.hi == b.hi;
}

static bool fp_vec_eq(const std::vector<pvac::Fp>& a, const std::vector<pvac::Fp>& b) {
    if (a.size() != b.size()) return false;
    for (size_t i = 0; i < a.size(); ++i)
        if (!fp_eq(a[i], b[i])) return false;
    return true;
}

static bool bitvec_eq(const pvac::BitVec& a, const pvac::BitVec& b) {
    return a.nbits == b.nbits && a.w == b.w;
}

static bool point_vec_eq(
    const std::vector<std::array<uint8_t, 32>>& a,
    const std::vector<std::array<uint8_t, 32>>& b
) {
    return a == b;
}

static bool rseed_eq(const pvac::RSeed& a, const pvac::RSeed& b) {
    return a.ztag == b.ztag && a.nonce.lo == b.nonce.lo && a.nonce.hi == b.nonce.hi;
}

static bool edge_eq(const pvac::Edge& a, const pvac::Edge& b) {
    return a.layer_id == b.layer_id &&
        a.idx == b.idx &&
        a.ch == b.ch &&
        fp_vec_eq(a.w, b.w) &&
        bitvec_eq(a.s, b.s);
}

static bool bytes32_zero(const std::array<uint8_t, 32>& x) {
    return x == std::array<uint8_t, 32>{};
}

static bool params_eq(const pvac::Params& a, const pvac::Params& b) {
    return a.B == b.B &&
        a.m_bits == b.m_bits &&
        a.n_bits == b.n_bits &&
        a.h_col_wt == b.h_col_wt &&
        a.x_col_wt == b.x_col_wt &&
        a.err_wt == b.err_wt &&
        a.noise_entropy_bits == b.noise_entropy_bits &&
        a.tuple2_fraction == b.tuple2_fraction &&
        a.depth_slope_bits == b.depth_slope_bits &&
        a.edge_budget == b.edge_budget &&
        a.lpn_n == b.lpn_n &&
        a.lpn_t == b.lpn_t &&
        a.lpn_tau_num == b.lpn_tau_num &&
        a.lpn_tau_den == b.lpn_tau_den &&
        a.recrypt_lo == b.recrypt_lo &&
        a.recrypt_hi == b.recrypt_hi &&
        a.recrypt_rounds == b.recrypt_rounds;
}

static bool int_vec_eq(const std::vector<int>& a, const std::vector<int>& b) {
    return a == b;
}

static bool pubkey_is_key_bound_extension_impl(const pvac::PubKey& legacy, const pvac::PubKey& bound) {
    if (!params_eq(legacy.prm, bound.prm)) return false;
    if (legacy.canon_tag != bound.canon_tag) return false;
    if (legacy.H_digest != bound.H_digest) return false;
    if (!fp_eq(legacy.omega_B, bound.omega_B)) return false;
    if (!fp_vec_eq(legacy.powg_B, bound.powg_B)) return false;
    if (legacy.H.size() != bound.H.size()) return false;
    for (size_t i = 0; i < legacy.H.size(); ++i)
        if (!bitvec_eq(legacy.H[i], bound.H[i]))
            return false;
    if (!int_vec_eq(legacy.ubk.perm, bound.ubk.perm)) return false;
    if (!int_vec_eq(legacy.ubk.inv, bound.ubk.inv)) return false;
    if (bytes32_zero(bound.circuit_prf_key_commit)) return false;
    if (!bytes32_zero(legacy.circuit_prf_key_commit) &&
        legacy.circuit_prf_key_commit != bound.circuit_prf_key_commit)
        return false;
    return true;
}

CAMLprim value caml_pvac_pubkey_is_key_bound_extension(value v_legacy, value v_bound) {
    CAMLparam2(v_legacy, v_bound);
    pvac::PubKey& legacy = *Handle_val(pvac::PubKey, v_legacy);
    pvac::PubKey& bound = *Handle_val(pvac::PubKey, v_bound);
    CAMLreturn(Val_bool(pubkey_is_key_bound_extension_impl(legacy, bound)));
}

CAMLprim value caml_pvac_pubkey_supports_alias_rejection(value v_pk) {
    CAMLparam1(v_pk);
    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    CAMLreturn(Val_bool(
        pk.circuit_prf_profile == pvac::CircuitPrfProfile::MIMC_X5_V7));
}

static bool legacy_layer_matches_bound_extension(
    const pvac::Layer& legacy,
    const pvac::Layer& bound,
    size_t slots
) {
    if (legacy.rule != bound.rule) return false;
    if (!rseed_eq(legacy.seed, bound.seed)) return false;
    if (legacy.pa != bound.pa || legacy.pb != bound.pb) return false;
    if (legacy.R_com != bound.R_com) return false;
    if (legacy.rule == pvac::RRule::PROD)
        return legacy.R_PC.empty() && bound.R_PC.empty() && point_vec_eq(legacy.PC, bound.PC);
    if (legacy.R_PC.empty()) {
        if (bound.R_PC.size() != slots || bound.PC.size() != slots)
            return false;
    } else if (!point_vec_eq(legacy.R_PC, bound.R_PC)) {
        return false;
    } else if (!point_vec_eq(legacy.PC, bound.PC)) {
        return false;
    }
    if (bound.PC.empty())
        return legacy.PC.empty();
    if (legacy.R_PC.empty())
        return bound.PC.size() == slots;
    return point_vec_eq(legacy.PC, bound.PC);
}

static bool cipher_is_key_bound_extension_impl(const pvac::Cipher& legacy, const pvac::Cipher& bound) {
    if (cipher_structure_error(legacy) != nullptr) return false;
    if (cipher_structure_error(bound) != nullptr) return false;
    if (legacy.slots != bound.slots) return false;
    if (!fp_vec_eq(legacy.c0, bound.c0)) return false;
    if (legacy.L.size() != bound.L.size() || legacy.E.size() != bound.E.size()) return false;
    for (size_t i = 0; i < legacy.L.size(); ++i)
        if (!legacy_layer_matches_bound_extension(legacy.L[i], bound.L[i], legacy.slots))
            return false;
    for (size_t i = 0; i < legacy.E.size(); ++i)
        if (!edge_eq(legacy.E[i], bound.E[i]))
            return false;
    return true;
}

CAMLprim value caml_pvac_cipher_is_key_bound_extension(value v_legacy, value v_bound) {
    CAMLparam2(v_legacy, v_bound);
    pvac::Cipher& legacy = *Handle_val(pvac::Cipher, v_legacy);
    pvac::Cipher& bound = *Handle_val(pvac::Cipher, v_bound);
    CAMLreturn(Val_bool(cipher_is_key_bound_extension_impl(legacy, bound)));
}

CAMLprim value caml_pvac_serialize_cipher(value v_ct) {
    CAMLparam1(v_ct);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_cipher");

    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    CAMLreturn(native_bytes([&] {
        Runtime_scope scope;
        return pvac_ser::serialize_cipher(ct);
    }));
}

CAMLprim value caml_pvac_serialize_cipher_public(value v_ct) {
    CAMLparam1(v_ct);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_cipher_public");

    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);

    CAMLreturn(native_bytes([&] { return pvac_ser::serialize_cipher_public(ct); }));
}

static value parse_cipher(value v_bytes, bool strict, bool cap, bool result) {
    CAMLparam1(v_bytes);
    CAMLreturn(native_handle<pvac::Cipher>(&cipher_ops, [&] {
        Native_bytes input(bytes_data(v_bytes), bytes_data(v_bytes) + bytes_len(v_bytes));
        Runtime_scope scope;
        pvac::Cipher cipher;
        std::string reason;
        if (!pvac_ser::deserialize_cipher_checked(input.data(), input.size(), cipher,
                reason, strict, cap))
            throw std::runtime_error(reason);
        return cipher;
    }, cipher_mem, result));
}

CAMLprim value caml_pvac_deserialize_cipher(value v_bytes) {
    return parse_cipher(v_bytes, true, true, false);
}

CAMLprim value caml_pvac_deserialize_cipher_result(value v_bytes) {
    return parse_cipher(v_bytes, true, true, true);
}

CAMLprim value caml_pvac_deserialize_cipher_prior_result(value v_bytes) {
    return parse_cipher(v_bytes, false, false, true);
}

CAMLprim value caml_pvac_deserialize_cipher_cap_result(value v_bytes) {
    return parse_cipher(v_bytes, false, true, true);
}

CAMLprim value caml_pvac_serialize_pubkey(value v_pk) {
    CAMLparam1(v_pk);
    CAMLlocal1(v_bytes);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);

    CAMLreturn(native_bytes([&] {
        Runtime_scope scope;
        return pvac_ser::serialize_pubkey(pk);
    }));
}

CAMLprim value caml_pvac_serialize_pubkey_legacy_v2(value v_pk) {
    CAMLparam1(v_pk);
    CAMLlocal1(v_bytes);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);

    CAMLreturn(native_bytes([&] {
        Runtime_scope scope;
        auto raw = pvac_ser::serialize_pubkey_raw(pk);
        if (raw.size() < 39)
            throw std::runtime_error("serialize_pubkey_legacy_v2: invalid pubkey size");
        raw[4] = pvac_ser::VERSION_V2;
        raw.resize(raw.size() - 33);
        return pvac::compress::pack(raw);
    }));
}

CAMLprim value caml_pvac_test_set_historical_profile(value v_pk, value v_sk) {
    CAMLparam2(v_pk, v_sk);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::set_circuit_prf_profile(
        pk,
        sk,
        pvac::CircuitPrfProfile::MIMC_X3_V6);

    CAMLreturn(Val_unit);
}

static value parse_pubkey(value v_bytes, bool result) {
    CAMLparam1(v_bytes);
    CAMLreturn(native_handle<pvac::PubKey>(&pubkey_ops, [&] {
        Native_bytes input(bytes_data(v_bytes), bytes_data(v_bytes) + bytes_len(v_bytes));
        Runtime_scope scope;
        pvac::PubKey key;
        std::string reason;
        if (!pvac_ser::deserialize_pubkey_checked(input.data(), input.size(), key, reason))
            throw std::runtime_error(reason);
        return key;
    }, pubkey_mem, result));
}

CAMLprim value caml_pvac_deserialize_pubkey(value v_bytes) {
    return parse_pubkey(v_bytes, false);
}

CAMLprim value caml_pvac_deserialize_pubkey_result(value v_bytes) {
    return parse_pubkey(v_bytes, true);
}

CAMLprim value caml_pvac_serialize_seckey(value v_sk) {
    CAMLparam1(v_sk);
    CAMLlocal1(v_bytes);

    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);

    CAMLreturn(native_bytes([&] { return pvac_ser::serialize_seckey(sk); }));
}

CAMLprim value caml_pvac_deserialize_seckey(value v_bytes) {
    CAMLparam1(v_bytes);

    CAMLreturn(native_handle<pvac::SecKey>(&seckey_ops,
        [&] { return pvac_ser::deserialize_seckey(bytes_data(v_bytes), bytes_len(v_bytes)); }, seckey_mem));
}

CAMLprim value caml_pvac_make_zero_proof_math(value v_math, value v_pk, value v_sk, value v_ct) {
    CAMLparam4(v_math, v_pk, v_sk, v_ct);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("make_zero_proof");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    CAMLreturn(native_handle<pvac::ZeroProof>(&zero_proof_ops,
        [&] { return pvac::make_zero_proof(pk, sk, ct, rule); }, zero_proof_mem));
}

CAMLprim value caml_pvac_verify_zero_math(value v_math, value v_pk, value v_ct, value v_proof) {
    CAMLparam4(v_math, v_pk, v_ct, v_proof);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("verify_zero");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::ZeroProof& proof = *Handle_val(pvac::ZeroProof, v_proof);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    DBG_SIZE("proof.V", proof.proof.V.size());

    CAMLreturn(native_check([&] {
        Runtime_scope scope;
        return pvac::verify_zero(pk, ct, proof, rule);
    }));
}

CAMLprim value caml_pvac_make_zero_proof_bound_math(value v_math, value v_pk, value v_sk, value v_ct, value v_amount, value v_blinding) {
    CAMLparam5(v_math, v_pk, v_sk, v_ct, v_amount);
    CAMLxparam1(v_blinding);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("make_zero_proof_bound");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t amount = nonnegative_u64(v_amount, "make_zero_proof_bound: negative amount");
    const uint8_t* blind_data = bytes_data(v_blinding);
    pvac::Scalar blind = pvac::sc_reduce256(blind_data);

    CAMLreturn(native_handle<pvac::ZeroProof>(&zero_proof_ops,
        [&] { return pvac::make_zero_proof_bound(pk, sk, ct, amount, blind, rule); }, zero_proof_mem));
}

CAMLprim value caml_pvac_make_zero_proof_bound_bytecode(value* argv, int) {
    return caml_pvac_make_zero_proof_bound_math(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value caml_pvac_verify_zero_bound_math(value v_math, value v_pk, value v_ct, value v_proof, value v_commitment) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof, v_commitment);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("verify_zero_bound");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::ZeroProof& proof = *Handle_val(pvac::ZeroProof, v_proof);
    if (bytes_len(v_commitment) != 32)
        CAMLreturn(Val_bool(false));
    const uint8_t* commit_data = bytes_data(v_commitment);

    pvac::RistrettoPoint commitment;
    std::memcpy(commitment.data(), commit_data, 32);
    pvac::ExtPoint decoded_commitment;
    if (!pvac::rist_decode(decoded_commitment, commitment))
        CAMLreturn(Val_bool(false));

    CAMLreturn(native_check([&] {
        Runtime_scope scope;
        return pvac::verify_zero_bound(pk, ct, proof, commitment, rule);
    }));
}

static value caml_pvac_verify_zero_amount_prior_impl(
    value v_math,
    value v_pk,
    value v_ct,
    value v_proof,
    value v_commitment,
    bool (*verify)(
        const pvac::PubKey&,
        const pvac::Cipher&,
        const pvac::ZeroProof&,
        const pvac::RistrettoPoint&,
        pvac::ScalarRule)
) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof, v_commitment);
    const auto rule = Bool_val(v_math) ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::ZeroProof& proof = *Handle_val(pvac::ZeroProof, v_proof);
    if (bytes_len(v_commitment) != 32)
        CAMLreturn(Val_bool(false));
    pvac::RistrettoPoint commitment;
    std::memcpy(commitment.data(), bytes_data(v_commitment), 32);
    pvac::ExtPoint decoded;
    if (!pvac::rist_decode(decoded, commitment))
        CAMLreturn(Val_bool(false));
    CAMLreturn(native_check([&] {
        Runtime_scope scope;
        return verify(pk, ct, proof, commitment, rule);
    }));
}

CAMLprim value caml_pvac_verify_zero_amount_prior_math(value v_math, value v_pk, value v_ct, value v_proof, value v_commitment) {
    return caml_pvac_verify_zero_amount_prior_impl(
        v_math,
        v_pk,
        v_ct,
        v_proof,
        v_commitment,
        pvac::verify_zero_amount_prior);
}

CAMLprim value caml_pvac_verify_zero_bound_key_switch_math(value v_math, value v_pk, value v_ct, value v_proof, value v_commitment) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof, v_commitment);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("verify_zero_bound_key_switch");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::ZeroProof& proof = *Handle_val(pvac::ZeroProof, v_proof);
    if (bytes_len(v_commitment) != 32)
        CAMLreturn(Val_bool(false));
    const uint8_t* commit_data = bytes_data(v_commitment);

    pvac::RistrettoPoint commitment;
    std::memcpy(commitment.data(), commit_data, 32);
    pvac::ExtPoint decoded_commitment;
    if (!pvac::rist_decode(decoded_commitment, commitment))
        CAMLreturn(Val_bool(false));

    CAMLreturn(native_check([&] {
        Runtime_scope scope;
        return pvac::verify_zero_bound_key_switch(pk, ct, proof, commitment, rule);
    }));
}

CAMLprim value caml_pvac_verify_zero_amount_key_switch_prior_math(value v_math, value v_pk, value v_ct, value v_proof, value v_commitment) {
    return caml_pvac_verify_zero_amount_prior_impl(
        v_math,
        v_pk,
        v_ct,
        v_proof,
        v_commitment,
        pvac::verify_zero_amount_key_switch_prior);
}

CAMLprim value caml_pvac_make_zero_proof_bound_historical_migration_math(value v_math, value v_pk, value v_sk, value v_ct, value v_amount, value v_blinding) {
    CAMLparam5(v_math, v_pk, v_sk, v_ct, v_amount);
    CAMLxparam1(v_blinding);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t amount = nonnegative_u64(
        v_amount,
        "make_zero_proof_bound_historical_migration: negative amount");
    if (bytes_len(v_blinding) != 32)
        caml_failwith(
            "make_zero_proof_bound_historical_migration: blinding must be 32 bytes");
    pvac::Scalar blind = pvac::sc_reduce256(bytes_data(v_blinding));
    CAMLreturn(native_handle<pvac::ZeroProof>(&zero_proof_ops,
        [&] { return pvac::make_zero_proof_bound_historical_migration(
            pk, sk, ct, amount, blind, rule); }, zero_proof_mem));
}

CAMLprim value caml_pvac_make_zero_proof_bound_historical_migration_bytecode(value* argv, int) {
    return caml_pvac_make_zero_proof_bound_historical_migration_math(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value caml_pvac_verify_zero_bound_historical_migration_math(value v_math, value v_pk, value v_ct, value v_proof, value v_commitment) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof, v_commitment);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::ZeroProof& proof = *Handle_val(pvac::ZeroProof, v_proof);
    if (bytes_len(v_commitment) != 32)
        CAMLreturn(Val_bool(false));
    pvac::RistrettoPoint commitment;
    std::memcpy(commitment.data(), bytes_data(v_commitment), 32);
    pvac::ExtPoint decoded_commitment;
    if (!pvac::rist_decode(decoded_commitment, commitment))
        CAMLreturn(Val_bool(false));
    CAMLreturn(native_check([&] {
        Runtime_scope scope;
        return pvac::verify_zero_bound_historical_migration(
            pk,
            ct,
            proof,
            commitment, rule);
    }));
}

CAMLprim value caml_pvac_verify_zero_amount_historical_prior_math(value v_math, value v_pk, value v_ct, value v_proof, value v_commitment) {
    return caml_pvac_verify_zero_amount_prior_impl(
        v_math,
        v_pk,
        v_ct,
        v_proof,
        v_commitment,
        pvac::verify_zero_amount_historical_prior);
}

CAMLprim value caml_pvac_make_zero_proof_bound_range_math(value v_math, value v_pk, value v_sk, value v_ct, value v_amount, value v_blinding) {
    CAMLparam5(v_math, v_pk, v_sk, v_ct, v_amount);
    CAMLxparam1(v_blinding);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("make_zero_proof_bound_range");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t amount = nonnegative_u64(v_amount, "make_zero_proof_bound_range: negative amount");
    if (bytes_len(v_blinding) != 32)
        caml_failwith("make_zero_proof_bound_range: blinding must be 32 bytes");
    const uint8_t* blind_data = bytes_data(v_blinding);
    pvac::Scalar blind = pvac::sc_reduce256(blind_data);

    CAMLreturn(native_handle<pvac::ZeroProof>(&zero_proof_ops,
        [&] { return pvac::make_zero_proof_bound_range(pk, sk, ct, amount, blind, rule); }, zero_proof_mem));
}

CAMLprim value caml_pvac_make_zero_proof_bound_range_bytecode(value* argv, int) {
    return caml_pvac_make_zero_proof_bound_range_math(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value caml_pvac_pedersen_commit_amount(value v_amount, value v_blinding) {
    CAMLparam2(v_amount, v_blinding);
    CAMLlocal1(v_out);

    uint64_t amount = nonnegative_u64(v_amount, "pedersen_commit_amount: negative amount");
    if (bytes_len(v_blinding) != 32)
        caml_failwith("pedersen_commit_amount: blinding must be 32 bytes");
    const uint8_t* blind_data = bytes_data(v_blinding);
    pvac::Scalar val = pvac::bp::sc_from_u64(amount);
    pvac::Scalar blind = pvac::sc_reduce256(blind_data);
    pvac::RistrettoPoint pt = pvac::pedersen_commit(val, blind);

    v_out = caml_alloc_string(32);
    std::memcpy(Bytes_val(v_out), pt.data(), 32);

    CAMLreturn(v_out);
}

CAMLprim value caml_pvac_pedersen_identity(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(v_out);

    pvac::RistrettoPoint pt = pvac::rist_identity();
    v_out = caml_alloc_string(32);
    std::memcpy(Bytes_val(v_out), pt.data(), 32);

    CAMLreturn(v_out);
}

static value caml_pvac_pedersen_binop(value v_a, value v_b, bool add) {
    CAMLparam2(v_a, v_b);
    CAMLlocal1(v_out);

    if (bytes_len(v_a) != 32 || bytes_len(v_b) != 32)
        caml_failwith("pedersen point op: commitments must be 32 bytes");

    pvac::RistrettoPoint a;
    pvac::RistrettoPoint b;
    std::memcpy(a.data(), bytes_data(v_a), 32);
    std::memcpy(b.data(), bytes_data(v_b), 32);

    pvac::ExtPoint point_a;
    pvac::ExtPoint point_b;
    if (!pvac::rist_decode(point_a, a) || !pvac::rist_decode(point_b, b))
        caml_failwith("pedersen point op: invalid commitment");
    pvac::RistrettoPoint pt =
        add
        ? pvac::rist_encode(pvac::ext_add(point_a, point_b))
        : pvac::rist_encode(pvac::ext_sub(point_a, point_b));
    v_out = caml_alloc_string(32);
    std::memcpy(Bytes_val(v_out), pt.data(), 32);

    CAMLreturn(v_out);
}

CAMLprim value caml_pvac_pedersen_add(value v_a, value v_b) {
    return caml_pvac_pedersen_binop(v_a, v_b, true);
}

CAMLprim value caml_pvac_pedersen_sub(value v_a, value v_b) {
    return caml_pvac_pedersen_binop(v_a, v_b, false);
}

CAMLprim value caml_pvac_make_range_proof_math(value v_math, value v_pk, value v_sk, value v_ct, value v_value) {
    CAMLparam5(v_math, v_pk, v_sk, v_ct, v_value);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("make_range_proof");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t val = nonnegative_u64(v_value, "make_range_proof: negative value");
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    CAMLreturn(native_handle<pvac::RangeProof>(&range_proof_ops,
        [&] { return pvac::make_range_proof(pk, sk, ct, val, rule); }, range_proof_mem));
}

CAMLprim value caml_pvac_verify_range_math(value v_math, value v_pk, value v_ct, value v_proof) {
    CAMLparam4(v_math, v_pk, v_ct, v_proof);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("verify_range");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::RangeProof& proof = *Handle_val(pvac::RangeProof, v_proof);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    DBG_SIZE("proof.ct_bit", proof.ct_bit.size());
    DBG_SIZE("proof.bit_proofs", proof.bit_proofs.size());

    CAMLreturn(native_check([&] {
        Runtime_scope scope;
        return pvac::verify_range(pk, ct, proof, rule);
    }));
}

CAMLprim value caml_pvac_serialize_zero_proof(value v_zp) {
    CAMLparam1(v_zp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_zero_proof");

    pvac::ZeroProof& zp = *Handle_val(pvac::ZeroProof, v_zp);
    DBG_SIZE("zp.V", zp.proof.V.size());

    CAMLreturn(native_bytes([&] {
        pvac_ser::Writer w;
        pvac_ser::write_zero_proof_raw(w, zp);
        return std::move(w.buf);
    }));
}

CAMLprim value caml_pvac_serialize_bound_range_proof(value v_zp) {
    CAMLparam1(v_zp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_bound_range_proof");

    pvac::ZeroProof& zp = *Handle_val(pvac::ZeroProof, v_zp);
    CAMLreturn(native_bytes([&] { return pvac_ser::serialize_bound_range_proof(zp); }));
}

CAMLprim value caml_pvac_deserialize_zero_proof(value v_bytes) {
    CAMLparam1(v_bytes);
    DBG_ENTER("deserialize_zero_proof");
    DBG_SIZE("input_bytes", bytes_len(v_bytes));

    CAMLreturn(native_handle<pvac::ZeroProof>(&zero_proof_ops, [&] {
        Native_bytes input(bytes_data(v_bytes), bytes_data(v_bytes) + bytes_len(v_bytes));
        Runtime_scope scope;
        pvac_ser::Reader r(input.data(), input.size());
        auto proof = pvac_ser::read_zero_proof_raw(r);
        if (r.failed) throw std::runtime_error(r.error);
        return proof;
    }, zero_proof_mem));
}

CAMLprim value caml_pvac_serialize_range_proof(value v_rp) {
    CAMLparam1(v_rp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_range_proof");

    pvac::RangeProof& rp = *Handle_val(pvac::RangeProof, v_rp);
    DBG_SIZE("rp.ct_bit", rp.ct_bit.size());
    DBG_SIZE("rp.bit_proofs", rp.bit_proofs.size());

    CAMLreturn(native_bytes([&] { return pvac_ser::serialize_range_proof(rp); }));
}

CAMLprim value caml_pvac_deserialize_range_proof(value v_bytes) {
    CAMLparam1(v_bytes);
    DBG_ENTER("deserialize_range_proof");
    DBG_SIZE("input_bytes", bytes_len(v_bytes));

    CAMLreturn(native_handle<pvac::RangeProof>(&range_proof_ops, [&] {
        pvac::RangeProof proof;
        std::string reason;
        if (!pvac_ser::deserialize_range_proof_checked(
                bytes_data(v_bytes),
                bytes_len(v_bytes),
                proof,
                reason))
            throw std::runtime_error(reason);
        return proof;
    }, range_proof_mem));
}

CAMLprim value caml_pvac_make_aggregated_range_proof_math(value v_math, value v_pk, value v_sk, value v_ct, value v_value) {
    CAMLparam5(v_math, v_pk, v_sk, v_ct, v_value);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("make_aggregated_range_proof");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t val = nonnegative_u64(v_value, "make_aggregated_range_proof: negative value");

    CAMLreturn(native_handle<pvac::AggregatedRangeProof>(&agg_range_proof_ops,
        [&] { return pvac::make_aggregated_range_proof(pk, sk, ct, val, rule); }, agg_range_proof_mem));
}

CAMLprim value caml_pvac_serialize_agg_range_proof(value v_arp) {
    CAMLparam1(v_arp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_agg_range_proof");

    pvac::AggregatedRangeProof& arp = *Handle_val(pvac::AggregatedRangeProof, v_arp);

    CAMLreturn(native_bytes([&] { return pvac_ser::serialize_agg_range_proof(arp); }));
}

CAMLprim value caml_pvac_verify_range_any_math(value v_math, value v_pk, value v_ct, value v_proof_bytes, value v_strict) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof_bytes, v_strict);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    DBG_ENTER("verify_range_any");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    bool strict = Bool_val(v_strict);
    CAMLreturn(native_check([&] {
        Native_bytes input(bytes_data(v_proof_bytes),
            bytes_data(v_proof_bytes) + bytes_len(v_proof_bytes));
        Runtime_scope scope;
        pvac_ser::RangeProofAny proof;
        return parse_range_any_safe(input.data(), input.size(), proof) &&
            verify_range_any_safe(pk, ct, proof, strict, rule);
    }));
}

CAMLprim value caml_pvac_verify_range_bound_math(value v_math, value v_pk, value v_ct, value v_proof_bytes, value v_commitment) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof_bytes, v_commitment);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    if (bytes_len(v_commitment) != 32)
        CAMLreturn(Val_bool(false));

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::RistrettoPoint commitment;
    std::memcpy(commitment.data(), bytes_data(v_commitment), 32);
    pvac::ExtPoint decoded;
    if (!pvac::rist_decode(decoded, commitment))
        CAMLreturn(Val_bool(false));

    CAMLreturn(native_check([&] {
        Native_bytes input(bytes_data(v_proof_bytes),
            bytes_data(v_proof_bytes) + bytes_len(v_proof_bytes));
        Runtime_scope scope;
        pvac_ser::RangeProofAny proof;
        return parse_range_any_safe(input.data(), input.size(), proof) &&
            proof.format == pvac_ser::RP_BOUND &&
            pvac::verify_zero_bound_range(pk, ct, proof.bound_proof, commitment, rule);
    }));
}

CAMLprim value caml_pvac_verify_range_amount_prior_math(value v_math, value v_pk, value v_ct, value v_proof_bytes, value v_commitment) {
    CAMLparam5(v_math, v_pk, v_ct, v_proof_bytes, v_commitment);
    const bool math = Bool_val(v_math);
    const auto rule = math ? pvac::ScalarRule::Wide : pvac::ScalarRule::Prior;
    if (bytes_len(v_commitment) != 32)
        CAMLreturn(Val_bool(false));

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::RistrettoPoint commitment;
    std::memcpy(commitment.data(), bytes_data(v_commitment), 32);
    pvac::ExtPoint decoded;
    if (!pvac::rist_decode(decoded, commitment))
        CAMLreturn(Val_bool(false));

    CAMLreturn(native_check([&] {
        Native_bytes input(bytes_data(v_proof_bytes),
            bytes_data(v_proof_bytes) + bytes_len(v_proof_bytes));
        Runtime_scope scope;
        pvac_ser::RangeProofAny proof;
        return parse_range_any_safe(input.data(), input.size(), proof) &&
            proof.format == pvac_ser::RP_BOUND &&
            pvac::verify_range_amount_prior(pk, ct, proof.bound_proof, commitment, rule);
    }));
}

CAMLprim value caml_pvac_aes_kat(value v_unit) {
    CAMLparam1(v_unit);
    CAMLlocal1(v_out);

    pvac::Sha256 h;
    h.init();
    const char* label = "pvac.aes.kat.key";
    h.update(label, std::strlen(label));
    uint8_t key[32];
    h.finish(key);

    pvac::AesCtr256 prg;
    prg.init(key, 0);
    alignas(16) uint64_t buf[2];
    buf[0] = prg.next_u64();
    buf[1] = prg.next_u64();

    v_out = caml_alloc_string(16);
    std::memcpy(Bytes_val(v_out), buf, 16);

    CAMLreturn(v_out);
}

}