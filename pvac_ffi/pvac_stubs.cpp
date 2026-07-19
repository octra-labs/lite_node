/*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*/


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
#include <pvac_serialize.hpp>

#include <cstring>
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
static size_t get_rss_mb() {
    FILE* f = fopen("/proc/self/statm", "r");
    if (!f) return 0;
    long dummy = 0, pages = 0;
    if (fscanf(f, "%ld %ld", &dummy, &pages) != 2) pages = 0;
    fclose(f);
    return (size_t)(pages * 4096 / (1024 * 1024));
}
#endif

#ifdef PVAC_DEBUG
#define DBG_ENTER(name) fprintf(stderr, "[pvac_ffi] >> %s (RSS=%zuMB)\n", name, get_rss_mb())
#define DBG_EXIT(name)  fprintf(stderr, "[pvac_ffi] << %s (RSS=%zuMB)\n", name, get_rss_mb())
#define DBG_SIZE(name, val) fprintf(stderr, "[pvac_ffi]    %s = %zu\n", name, (size_t)(val))
#else
#define DBG_ENTER(name) ((void)0)
#define DBG_EXIT(name)  ((void)0)
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
    if (ver >= pvac_ser::VERSION_V4)
        pvac_ser::mark_public_base_layer(layer, slots);
    return !r.failed;
}

static bool read_cipher_safe(pvac_ser::Reader& r, uint8_t ver, pvac::Cipher& cipher) {
    cipher = pvac::Cipher{};
    cipher.slots = r.u64();
    size_t n_l = r.u64();
    r.check_count(n_l, 8);
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
        if (tag == pvac_ser::TAG_RANGE_PROOF) {
            out.format = pvac_ser::RP_OLD;
            return read_range_old_safe(data, len, out.old_proof);
        }
        if (tag == pvac_ser::TAG_AGG_RANGE_PROOF) {
            out.format = pvac_ser::RP_AGGREGATED;
            return read_range_agg_safe(data, len, out.agg_proof);
        }
        if (tag == pvac_ser::TAG_BOUND_RANGE_PROOF) {
            out.format = pvac_ser::RP_BOUND;
            out.bound_proof = pvac_ser::deserialize_bound_range_proof(data, len);
            return true;
        }
        return false;
    } catch (const std::exception& e) {
        return false;
    } catch (...) {
        return false;
    }
}

static bool verify_range_any_safe(pvac::PubKey& pk, pvac::Cipher& ct, const pvac_ser::RangeProofAny& proof) {
    try {
        if (proof.format == pvac_ser::RP_OLD)
            return pvac::verify_range(pk, ct, proof.old_proof);
        if (proof.format == pvac_ser::RP_AGGREGATED)
            return pvac::verify_aggregated_range(pk, ct, proof.agg_proof);
        return pvac::verify_zero_bound_range(pk, ct, proof.bound_proof);
    } catch (const std::exception& e) {
        return false;
    } catch (...) {
        return false;
    }
}

static value wrap_pubkey(pvac::PubKey* pk) { return wrap(&pubkey_ops, pk, pubkey_mem(*pk)); }
static value wrap_seckey(pvac::SecKey* sk) { return wrap(&seckey_ops, sk, seckey_mem(*sk)); }
static value wrap_evalkey(pvac::EvalKey* ek) { return wrap(&evalkey_ops, ek, evalkey_mem(*ek)); }
static value wrap_cipher(pvac::Cipher* ct) { return wrap(&cipher_ops, ct, cipher_mem(*ct)); }
static value wrap_zero_proof(pvac::ZeroProof* zp) { return wrap(&zero_proof_ops, zp, zero_proof_mem(*zp)); }
static value wrap_range_proof(pvac::RangeProof* rp) { return wrap(&range_proof_ops, rp, range_proof_mem(*rp)); }
static value wrap_agg_range_proof(pvac::AggregatedRangeProof* arp) { return wrap(&agg_range_proof_ops, arp, agg_range_proof_mem(*arp)); }

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

extern "C" {

CAMLprim value caml_pvac_default_params(value unit) {
    CAMLparam1(unit);
    DBG_ENTER("default_params");
    pvac::Params* prm = new pvac::Params();
    DBG_EXIT("default_params");
    CAMLreturn(wrap(&params_ops, prm));
}

CAMLprim value caml_pvac_keygen(value v_prm) {
    CAMLparam1(v_prm);
    CAMLlocal3(v_pk, v_sk, v_pair);
    DBG_ENTER("keygen");

    pvac::Params& prm = *Handle_val(pvac::Params, v_prm);
    pvac::PubKey* pk = new pvac::PubKey();
    pvac::SecKey* sk = new pvac::SecKey();

    try {
        pvac::set_debug_level(0);
        pvac::keygen(prm, *pk, *sk);
    } catch (const std::exception& e) {
        delete pk; delete sk;
        caml_failwith(e.what());
    }

    v_pk = wrap_pubkey(pk);
    v_sk = wrap_seckey(sk);
    v_pair = caml_alloc_tuple(2);
    Store_field(v_pair, 0, v_pk);
    Store_field(v_pair, 1, v_sk);
    DBG_EXIT("keygen");
    CAMLreturn(v_pair);
}

CAMLprim value caml_pvac_keygen_from_seed(value v_prm, value v_wallet_priv) {
    CAMLparam2(v_prm, v_wallet_priv);
    CAMLlocal3(v_pk, v_sk, v_pair);
    DBG_ENTER("keygen_from_seed");

    pvac::Params& prm = *Handle_val(pvac::Params, v_prm);

    if (bytes_len(v_wallet_priv) < 32) caml_failwith("wallet privkey must be >= 32 bytes");

    pvac::PubKey* pk = new pvac::PubKey();
    pvac::SecKey* sk = new pvac::SecKey();

    try {
        pvac::set_debug_level(0);
        pvac::keygen_from_seed(prm, *pk, *sk, bytes_data(v_wallet_priv));
    } catch (const std::exception& e) {
        delete pk; delete sk;
        caml_failwith(e.what());
    }

    v_pk = wrap_pubkey(pk);
    v_sk = wrap_seckey(sk);
    v_pair = caml_alloc_tuple(2);
    Store_field(v_pair, 0, v_pk);
    Store_field(v_pair, 1, v_sk);
    DBG_EXIT("keygen_from_seed");
    CAMLreturn(v_pair);
}

CAMLprim value caml_pvac_make_evalkey(value v_pk, value v_sk, value v_pool, value v_depth) {
    CAMLparam4(v_pk, v_sk, v_pool, v_depth);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    size_t pool_size = Long_val(v_pool);
    int depth = Int_val(v_depth);

    pvac::EvalKey* ek = new pvac::EvalKey();
    try {
        *ek = pvac::make_evalkey(pk, sk, pool_size, depth);
    } catch (const std::exception& e) {
        delete ek;
        caml_failwith(e.what());
    }

    CAMLreturn(wrap_evalkey(ek));
}

CAMLprim value caml_pvac_enc_value_seeded(value v_pk, value v_sk, value v_val, value v_seed) {
    CAMLparam4(v_pk, v_sk, v_val, v_seed);
    DBG_ENTER("enc_value_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    uint64_t val = nonnegative_u64(v_val, "enc_value_seeded: negative value");
    const uint8_t* seed = bytes_data(v_seed);

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");

    pvac::Cipher* ct = new pvac::Cipher();
    try {
        *ct = pvac::enc_value_seeded(pk, sk, val, seed);
    } catch (const std::exception& e) {
        delete ct;
        caml_failwith(e.what());
    }

    DBG_SIZE("layers", ct->L.size());
    DBG_SIZE("edges", ct->E.size());
    DBG_EXIT("enc_value_seeded");
    CAMLreturn(wrap_cipher(ct));
}

CAMLprim value caml_pvac_enc_values_seeded(value v_pk, value v_sk, value v_vals, value v_seed) {
    CAMLparam4(v_pk, v_sk, v_vals, v_seed);
    DBG_ENTER("enc_values_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    const uint8_t* seed = bytes_data(v_seed);

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");

    size_t n = Wosize_val(v_vals);
    DBG_SIZE("n_vals", n);
    std::vector<uint64_t> vals(n);
    for (size_t i = 0; i < n; ++i)
        vals[i] = nonnegative_u64(Field(v_vals, i), "enc_values_seeded: negative value");

    pvac::Cipher* ct = new pvac::Cipher();
    try {
        *ct = pvac::enc_values_seeded(pk, sk, vals, seed);
    } catch (const std::exception& e) {
        delete ct;
        caml_failwith(e.what());
    }

    DBG_SIZE("layers", ct->L.size());
    DBG_SIZE("edges", ct->E.size());
    DBG_EXIT("enc_values_seeded");
    CAMLreturn(wrap_cipher(ct));
}

CAMLprim value caml_pvac_enc_zero_seeded(value v_pk, value v_sk, value v_seed) {
    CAMLparam3(v_pk, v_sk, v_seed);
    DBG_ENTER("enc_zero_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    const uint8_t* seed = bytes_data(v_seed);

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");

    pvac::Cipher* ct = new pvac::Cipher();
    try {
        *ct = pvac::enc_zero_seeded(pk, sk, seed);
    } catch (const std::exception& e) {
        delete ct;
        caml_failwith(e.what());
    }

    DBG_SIZE("layers", ct->L.size());
    DBG_SIZE("edges", ct->E.size());
    DBG_EXIT("enc_zero_seeded");
    CAMLreturn(wrap_cipher(ct));
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
    std::string err;
    bool failed = false;
    try {
        result = pvac::dec_value(pk, sk, ct);
    } catch (const std::exception& e) {
        err = e.what();
        failed = true;
    } catch (...) {
        err = "pvac: dec_value: unknown C++ exception";
        failed = true;
    }
    if (failed) caml_failwith(err.c_str());

    DBG_EXIT("dec_value");

    if (result.hi == 0) {
        CAMLreturn(caml_copy_int64((int64_t)result.lo));
    } else {
        __uint128_t val = ((__uint128_t)result.hi << 64) | result.lo;
        __uint128_t p   = ((__uint128_t)1 << 127) - 1;
        if (val > p / 2) {
            __uint128_t neg = p - val;
            CAMLreturn(caml_copy_int64(-(int64_t)neg));
        } else {
            CAMLreturn(caml_copy_int64((int64_t)result.lo));
        }
    }
}

CAMLprim value caml_pvac_dec_values(value v_pk, value v_sk, value v_ct) {
    CAMLparam3(v_pk, v_sk, v_ct);
    CAMLlocal1(v_arr);
    DBG_ENTER("dec_values");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    std::vector<pvac::Fp> results;
    try {
        results = pvac::dec_values(pk, sk, ct);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    v_arr = caml_alloc(results.size(), 0);
    for (size_t i = 0; i < results.size(); ++i) {
        int64_t v;
        if (results[i].hi == 0) {
            v = (int64_t)results[i].lo;
        } else {
            __uint128_t val = ((__uint128_t)results[i].hi << 64) | results[i].lo;
            __uint128_t p   = ((__uint128_t)1 << 127) - 1;
            if (val > p / 2) {
                __uint128_t neg = p - val;
                v = -(int64_t)neg;
            } else {
                v = (int64_t)results[i].lo;
            }
        }
        Store_field(v_arr, i, caml_copy_int64(v));
    }

    DBG_EXIT("dec_values");
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

    pvac::Cipher* ct = new pvac::Cipher();
    try {
        *ct = pvac::ct_add(pk, a, b);
    } catch (const std::exception& e) {
        delete ct;
        caml_failwith(e.what());
    }

    DBG_SIZE("out.layers", ct->L.size());
    DBG_SIZE("out.edges", ct->E.size());
    DBG_EXIT("ct_add");
    CAMLreturn(wrap_cipher(ct));
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

    pvac::Cipher* ct = new pvac::Cipher();
    try {
        *ct = pvac::ct_sub(pk, a, b);
    } catch (const std::exception& e) {
        delete ct;
        caml_failwith(e.what());
    }

    DBG_SIZE("out.layers", ct->L.size());
    DBG_SIZE("out.edges", ct->E.size());
    DBG_EXIT("ct_sub");
    CAMLreturn(wrap_cipher(ct));
}

CAMLprim value caml_pvac_ct_mul_seeded(value v_pk, value v_a, value v_b, value v_seed) {
    CAMLparam4(v_pk, v_a, v_b, v_seed);
    DBG_ENTER("ct_mul_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& a = *Handle_val(pvac::Cipher, v_a);
    pvac::Cipher& b = *Handle_val(pvac::Cipher, v_b);
    const uint8_t* seed = bytes_data(v_seed);
    DBG_SIZE("a.layers", a.L.size());
    DBG_SIZE("a.edges", a.E.size());
    DBG_SIZE("b.layers", b.L.size());
    DBG_SIZE("b.edges", b.E.size());

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");

    pvac::Cipher* ct = new pvac::Cipher();
    try {
        *ct = pvac::ct_mul_seeded(pk, a, b, seed);
    } catch (const std::exception& e) {
        delete ct;
        caml_failwith(e.what());
    }

    DBG_SIZE("out.layers", ct->L.size());
    DBG_SIZE("out.edges", ct->E.size());
    DBG_EXIT("ct_mul_seeded");
    CAMLreturn(wrap_cipher(ct));
}

CAMLprim value caml_pvac_ct_scale(value v_pk, value v_ct, value v_scalar) {
    CAMLparam3(v_pk, v_ct, v_scalar);
    DBG_ENTER("ct_scale");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    int64_t scalar = Int64_val(v_scalar);
    DBG_SIZE("ct.edges", ct.E.size());

    pvac::Fp s = pvac::fp_from_u64(static_cast<uint64_t>(scalar));
    pvac::Cipher* result = new pvac::Cipher();
    try {
        *result = pvac::ct_scale(pk, ct, s);
    } catch (const std::exception& e) {
        delete result;
        caml_failwith(e.what());
    }

    DBG_EXIT("ct_scale");
    CAMLreturn(wrap_cipher(result));
}

CAMLprim value caml_pvac_ct_add_const(value v_pk, value v_ct, value v_lo, value v_hi) {
    CAMLparam4(v_pk, v_ct, v_lo, v_hi);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t lo = Int64_val(v_lo);
    uint64_t hi = Int64_val(v_hi);

    pvac::Fp k;
    k.lo = lo;
    k.hi = hi;

    pvac::Cipher* result = new pvac::Cipher(ct);
    for (size_t j = 0; j < result->c0.size(); ++j)
        result->c0[j] = pvac::fp_add(result->c0[j], k);

    CAMLreturn(wrap_cipher(result));
}

CAMLprim value caml_pvac_ct_sub_const(value v_pk, value v_ct, value v_k) {
    CAMLparam3(v_pk, v_ct, v_k);

    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t k = Int64_val(v_k);

    pvac::Fp neg_k = pvac::fp_neg(pvac::fp_from_u64(k));

    pvac::Cipher* result = new pvac::Cipher(ct);
    for (size_t j = 0; j < result->c0.size(); ++j)
        result->c0[j] = pvac::fp_add(result->c0[j], neg_k);

    CAMLreturn(wrap_cipher(result));
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

    pvac::Cipher* result = new pvac::Cipher();
    try {
        *result = pvac::ct_div_const(pk, ct, k);
    } catch (const std::exception& e) {
        delete result;
        caml_failwith(e.what());
    }

    CAMLreturn(wrap_cipher(result));
}

CAMLprim value caml_pvac_ct_square_seeded(value v_pk, value v_ct, value v_seed) {
    CAMLparam3(v_pk, v_ct, v_seed);
    DBG_ENTER("ct_square_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    const uint8_t* seed = bytes_data(v_seed);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");

    pvac::Cipher* result = new pvac::Cipher();
    try {
        *result = pvac::ct_square_seeded(pk, ct, seed);
    } catch (const std::exception& e) {
        delete result;
        caml_failwith(e.what());
    }

    DBG_SIZE("out.layers", result->L.size());
    DBG_SIZE("out.edges", result->E.size());
    DBG_EXIT("ct_square_seeded");
    CAMLreturn(wrap_cipher(result));
}

CAMLprim value caml_pvac_ct_recrypt_seeded(value v_pk, value v_ek, value v_ct, value v_seed) {
    CAMLparam4(v_pk, v_ek, v_ct, v_seed);
    DBG_ENTER("ct_recrypt_seeded");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::EvalKey& ek = *Handle_val(pvac::EvalKey, v_ek);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    const uint8_t* seed = bytes_data(v_seed);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    DBG_SIZE("ek.zero_pool", ek.zero_pool.size());

    if (bytes_len(v_seed) < 32) caml_failwith("seed must be 32 bytes");

    pvac::Cipher* result = new pvac::Cipher();
    try {
        *result = pvac::ct_recrypt_seeded(pk, ek, ct, seed);
    } catch (const std::exception& e) {
        delete result;
        caml_failwith(e.what());
    }

    DBG_SIZE("out.layers", result->L.size());
    DBG_SIZE("out.edges", result->E.size());
    DBG_EXIT("ct_recrypt_seeded");
    CAMLreturn(wrap_cipher(result));
}

CAMLprim value caml_pvac_commit_ct(value v_pk, value v_ct) {
    CAMLparam2(v_pk, v_ct);
    CAMLlocal1(v_bytes);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);

    auto hash = pvac::commit_ct(pk, ct);

    v_bytes = caml_alloc_string(32);
    std::memcpy(Bytes_val(v_bytes), hash.data(), 32);

    CAMLreturn(v_bytes);
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

    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_cipher(ct);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    DBG_SIZE("buf_bytes", buf.size());
    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    DBG_EXIT("serialize_cipher");
    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_serialize_cipher_public(value v_ct) {
    CAMLparam1(v_ct);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_cipher_public");

    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);

    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_cipher_public(ct);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    DBG_SIZE("buf_bytes", buf.size());
    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    DBG_EXIT("serialize_cipher_public");
    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_deserialize_cipher(value v_bytes) {
    CAMLparam1(v_bytes);
    DBG_ENTER("deserialize_cipher");
    DBG_SIZE("input_bytes", bytes_len(v_bytes));

    pvac::Cipher* ct = nullptr;
    char err_buf[256] = {0};
    ct = new pvac::Cipher();
    pvac_ser::Reader r(bytes_data(v_bytes), bytes_len(v_bytes));
    uint8_t ver = r.header(pvac_ser::TAG_CIPHER);
    if (!r.failed) {
        ct->slots = r.u64();
        size_t nL = r.u64();
        r.check_count(nL, 8);
        if (!r.failed) {
            ct->L.resize(nL);
            for (size_t i = 0; i < nL && !r.failed; ++i)
                ct->L[i] = pvac_ser::read_layer(r, ver, ct->slots);
        }
        size_t nc = r.u64();
        r.check_count(nc, 16);
        if (!r.failed) {
            ct->c0.resize(nc);
            for (size_t i = 0; i < nc && !r.failed; ++i)
                ct->c0[i] = r.fp();
        }
        size_t nE = r.u64();
        r.check_count(nE, 8);
        if (!r.failed) {
            ct->E.resize(nE);
            for (size_t i = 0; i < nE && !r.failed; ++i)
                ct->E[i] = pvac_ser::read_edge(r);
        }
    }
    if (r.failed) {
        snprintf(err_buf, sizeof(err_buf), "%s", r.error);
    } else if (const char* msg = cipher_structure_error(*ct)) {
        snprintf(err_buf, sizeof(err_buf), "%s", msg);
    }
    if (err_buf[0]) {
        delete ct;
        caml_failwith(err_buf);
    }

    DBG_SIZE("ct.layers", ct->L.size());
    DBG_SIZE("ct.edges", ct->E.size());
    DBG_EXIT("deserialize_cipher");
    CAMLreturn(wrap_cipher(ct));
}

CAMLprim value caml_pvac_serialize_pubkey(value v_pk) {
    CAMLparam1(v_pk);
    CAMLlocal1(v_bytes);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);

    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_pubkey(pk);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_serialize_pubkey_legacy_v2(value v_pk) {
    CAMLparam1(v_pk);
    CAMLlocal1(v_bytes);

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);

    std::vector<uint8_t> buf;
    try {
        auto raw = pvac_ser::serialize_pubkey_raw(pk);
        if (raw.size() < 38)
            caml_failwith("serialize_pubkey_legacy_v2: invalid pubkey size");
        raw[4] = pvac_ser::VERSION_V2;
        raw.resize(raw.size() - 32);
        buf = pvac::compress::pack(raw);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_deserialize_pubkey(value v_bytes) {
    CAMLparam1(v_bytes);

    pvac::PubKey* pk = nullptr;
    char err_buf[256] = {0};
    try {
        pk = new pvac::PubKey();
        *pk = pvac_ser::deserialize_pubkey(bytes_data(v_bytes), bytes_len(v_bytes));
    } catch (const std::exception& e) {
        if (pk) { delete pk; pk = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "%s", e.what());
    } catch (...) {
        if (pk) { delete pk; pk = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "deserialize_pubkey failed: unknown error");
    }
    if (err_buf[0]) caml_failwith(err_buf);

    CAMLreturn(wrap_pubkey(pk));
}

CAMLprim value caml_pvac_serialize_seckey(value v_sk) {
    CAMLparam1(v_sk);
    CAMLlocal1(v_bytes);

    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);

    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_seckey(sk);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_deserialize_seckey(value v_bytes) {
    CAMLparam1(v_bytes);

    pvac::SecKey* sk = nullptr;
    char err_buf[256] = {0};
    try {
        sk = new pvac::SecKey();
        *sk = pvac_ser::deserialize_seckey(bytes_data(v_bytes), bytes_len(v_bytes));
    } catch (const std::exception& e) {
        if (sk) { delete sk; sk = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "%s", e.what());
    } catch (...) {
        if (sk) { delete sk; sk = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "deserialize_seckey failed: unknown error");
    }
    if (err_buf[0]) caml_failwith(err_buf);

    CAMLreturn(wrap_seckey(sk));
}

CAMLprim value caml_pvac_make_zero_proof(value v_pk, value v_sk, value v_ct) {
    CAMLparam3(v_pk, v_sk, v_ct);
    DBG_ENTER("make_zero_proof");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());

    pvac::ZeroProof* zp = new pvac::ZeroProof();
    try {
        *zp = pvac::make_zero_proof(pk, sk, ct);
    } catch (const std::exception& e) {
        delete zp;
        caml_failwith(e.what());
    }

    DBG_SIZE("proof.V", zp->proof.V.size());
    DBG_EXIT("make_zero_proof");
    CAMLreturn(wrap_zero_proof(zp));
}

CAMLprim value caml_pvac_verify_zero(value v_pk, value v_ct, value v_proof) {
    CAMLparam3(v_pk, v_ct, v_proof);
    DBG_ENTER("verify_zero");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::ZeroProof& proof = *Handle_val(pvac::ZeroProof, v_proof);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    DBG_SIZE("proof.V", proof.proof.V.size());

    bool ok = false;
    caml_release_runtime_system();
    try {
        ok = pvac::verify_zero(pk, ct, proof);
    } catch (const std::exception& e) {
        ok = false;
    } catch (...) {
        ok = false;
    }
    caml_acquire_runtime_system();

    DBG_EXIT("verify_zero");
    CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_pvac_make_zero_proof_bound(value v_pk, value v_sk, value v_ct,
                                                value v_amount, value v_blinding) {
    CAMLparam5(v_pk, v_sk, v_ct, v_amount, v_blinding);
    DBG_ENTER("make_zero_proof_bound");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t amount = nonnegative_u64(v_amount, "make_zero_proof_bound: negative amount");
    const uint8_t* blind_data = bytes_data(v_blinding);
    pvac::Scalar blind = pvac::sc_reduce256(blind_data);

    pvac::ZeroProof* zp = new pvac::ZeroProof();
    try {
        *zp = pvac::make_zero_proof_bound(pk, sk, ct, amount, blind);
    } catch (const std::exception& e) {
        delete zp;
        caml_failwith(e.what());
    }

    DBG_SIZE("proof.V", zp->proof.V.size());
    DBG_SIZE("proof.is_bound", zp->is_bound);
    DBG_EXIT("make_zero_proof_bound");
    CAMLreturn(wrap_zero_proof(zp));
}

CAMLprim value caml_pvac_verify_zero_bound(value v_pk, value v_ct, value v_proof,
                                            value v_commitment) {
    CAMLparam4(v_pk, v_ct, v_proof, v_commitment);
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

    bool ok = false;
    caml_release_runtime_system();
    try {
        ok = pvac::verify_zero_bound(pk, ct, proof, commitment);
    } catch (const std::exception& e) {
        ok = false;
    } catch (...) {
        ok = false;
    }
    caml_acquire_runtime_system();

    DBG_EXIT("verify_zero_bound");
    CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_pvac_make_zero_proof_bound_range(value v_pk, value v_sk, value v_ct,
                                                     value v_amount, value v_blinding) {
    CAMLparam5(v_pk, v_sk, v_ct, v_amount, v_blinding);
    DBG_ENTER("make_zero_proof_bound_range");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t amount = nonnegative_u64(v_amount, "make_zero_proof_bound_range: negative amount");
    if (bytes_len(v_blinding) != 32)
        caml_failwith("make_zero_proof_bound_range: blinding must be 32 bytes");
    const uint8_t* blind_data = bytes_data(v_blinding);
    pvac::Scalar blind = pvac::sc_reduce256(blind_data);

    pvac::ZeroProof* zp = new pvac::ZeroProof();
    try {
        *zp = pvac::make_zero_proof_bound_range(pk, sk, ct, amount, blind);
    } catch (const std::exception& e) {
        delete zp;
        caml_failwith(e.what());
    }

    DBG_SIZE("proof.V", zp->proof.V.size());
    DBG_SIZE("proof.is_bound", zp->is_bound);
    DBG_EXIT("make_zero_proof_bound_range");
    CAMLreturn(wrap_zero_proof(zp));
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

    try {
        pvac::RistrettoPoint pt = add ? pvac::rist_add(a, b) : pvac::rist_sub(a, b);
        v_out = caml_alloc_string(32);
        std::memcpy(Bytes_val(v_out), pt.data(), 32);
    } catch (...) {
        caml_failwith("pedersen point op: invalid commitment");
    }

    CAMLreturn(v_out);
}

CAMLprim value caml_pvac_pedersen_add(value v_a, value v_b) {
    return caml_pvac_pedersen_binop(v_a, v_b, true);
}

CAMLprim value caml_pvac_pedersen_sub(value v_a, value v_b) {
    return caml_pvac_pedersen_binop(v_a, v_b, false);
}

CAMLprim value caml_pvac_make_range_proof(value v_pk, value v_sk, value v_ct, value v_value) {
    CAMLparam4(v_pk, v_sk, v_ct, v_value);
    DBG_ENTER("make_range_proof");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t val = nonnegative_u64(v_value, "make_range_proof: negative value");
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    fprintf(stderr, "[pvac_ffi]    value = %llu\n", (unsigned long long)val);

    pvac::RangeProof* rp = new pvac::RangeProof();
    try {
        *rp = pvac::make_range_proof(pk, sk, ct, val);
    } catch (const std::exception& e) {
        delete rp;
        caml_failwith(e.what());
    }

    DBG_SIZE("rp.ct_bit", rp->ct_bit.size());
    DBG_SIZE("rp.bit_proofs", rp->bit_proofs.size());
    DBG_EXIT("make_range_proof");
    CAMLreturn(wrap_range_proof(rp));
}

CAMLprim value caml_pvac_verify_range(value v_pk, value v_ct, value v_proof) {
    CAMLparam3(v_pk, v_ct, v_proof);
    DBG_ENTER("verify_range");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    pvac::RangeProof& proof = *Handle_val(pvac::RangeProof, v_proof);
    DBG_SIZE("ct.layers", ct.L.size());
    DBG_SIZE("ct.edges", ct.E.size());
    DBG_SIZE("proof.ct_bit", proof.ct_bit.size());
    DBG_SIZE("proof.bit_proofs", proof.bit_proofs.size());

    bool ok = false;
    try {
        ok = pvac::verify_range(pk, ct, proof);
    } catch (const std::exception& e) {
        ok = false;
    } catch (...) {
        ok = false;
    }

    DBG_EXIT("verify_range");
    CAMLreturn(Val_bool(ok));
}

CAMLprim value caml_pvac_serialize_zero_proof(value v_zp) {
    CAMLparam1(v_zp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_zero_proof");

    pvac::ZeroProof& zp = *Handle_val(pvac::ZeroProof, v_zp);
    DBG_SIZE("zp.V", zp.proof.V.size());

    pvac_ser::Writer w;
    try {
        pvac_ser::write_zero_proof_raw(w, zp);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    DBG_SIZE("buf_bytes", w.buf.size());
    v_bytes = caml_alloc_string(w.buf.size());
    std::memcpy(Bytes_val(v_bytes), w.buf.data(), w.buf.size());

    DBG_EXIT("serialize_zero_proof");
    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_serialize_bound_range_proof(value v_zp) {
    CAMLparam1(v_zp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_bound_range_proof");

    pvac::ZeroProof& zp = *Handle_val(pvac::ZeroProof, v_zp);
    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_bound_range_proof(zp);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    DBG_EXIT("serialize_bound_range_proof");
    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_deserialize_zero_proof(value v_bytes) {
    CAMLparam1(v_bytes);
    DBG_ENTER("deserialize_zero_proof");
    DBG_SIZE("input_bytes", bytes_len(v_bytes));

    pvac::ZeroProof* zp = nullptr;
    char err_buf[256] = {0};
    try {
        pvac_ser::Reader r(bytes_data(v_bytes), bytes_len(v_bytes));
        zp = new pvac::ZeroProof();
        *zp = pvac_ser::read_zero_proof_raw(r);
        if (r.failed) {
            snprintf(err_buf, sizeof(err_buf), "%s", r.error);
            delete zp; zp = nullptr;
        }
    } catch (const std::exception& e) {
        if (zp) { delete zp; zp = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "%s", e.what());
    } catch (...) {
        if (zp) { delete zp; zp = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "deserialize_zero_proof failed: unknown error");
    }
    if (err_buf[0]) caml_failwith(err_buf);

    DBG_SIZE("zp.V", zp->proof.V.size());
    DBG_EXIT("deserialize_zero_proof");
    CAMLreturn(wrap_zero_proof(zp));
}

CAMLprim value caml_pvac_serialize_range_proof(value v_rp) {
    CAMLparam1(v_rp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_range_proof");

    pvac::RangeProof& rp = *Handle_val(pvac::RangeProof, v_rp);
    DBG_SIZE("rp.ct_bit", rp.ct_bit.size());
    DBG_SIZE("rp.bit_proofs", rp.bit_proofs.size());

    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_range_proof(rp);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    DBG_SIZE("buf_bytes", buf.size());
    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    DBG_EXIT("serialize_range_proof");
    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_deserialize_range_proof(value v_bytes) {
    CAMLparam1(v_bytes);
    DBG_ENTER("deserialize_range_proof");
    DBG_SIZE("input_bytes", bytes_len(v_bytes));

    pvac::RangeProof* rp = nullptr;
    char err_buf[256] = {0};
    try {
        rp = new pvac::RangeProof();
        *rp = pvac_ser::deserialize_range_proof(bytes_data(v_bytes), bytes_len(v_bytes));
    } catch (const std::exception& e) {
        if (rp) { delete rp; rp = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "%s", e.what());
    } catch (...) {
        if (rp) { delete rp; rp = nullptr; }
        snprintf(err_buf, sizeof(err_buf), "deserialize_range_proof failed: unknown error");
    }
    if (err_buf[0]) caml_failwith(err_buf);

    DBG_SIZE("rp.ct_bit", rp->ct_bit.size());
    DBG_SIZE("rp.bit_proofs", rp->bit_proofs.size());
    DBG_EXIT("deserialize_range_proof");
    CAMLreturn(wrap_range_proof(rp));
}



CAMLprim value caml_pvac_make_aggregated_range_proof(value v_pk, value v_sk, value v_ct, value v_value) {
    CAMLparam4(v_pk, v_sk, v_ct, v_value);
    DBG_ENTER("make_aggregated_range_proof");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::SecKey& sk = *Handle_val(pvac::SecKey, v_sk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    uint64_t val = nonnegative_u64(v_value, "make_aggregated_range_proof: negative value");

    pvac::AggregatedRangeProof* arp = new pvac::AggregatedRangeProof();
    try {
        *arp = pvac::make_aggregated_range_proof(pk, sk, ct, val);
    } catch (const std::exception& e) {
        delete arp;
        caml_failwith(e.what());
    }

    DBG_SIZE("arp.ct_bit", arp->ct_bit.size());
    DBG_SIZE("arp.proof.V", arp->proof.V.size());
    DBG_EXIT("make_aggregated_range_proof");
    CAMLreturn(wrap_agg_range_proof(arp));
}

CAMLprim value caml_pvac_serialize_agg_range_proof(value v_arp) {
    CAMLparam1(v_arp);
    CAMLlocal1(v_bytes);
    DBG_ENTER("serialize_agg_range_proof");

    pvac::AggregatedRangeProof& arp = *Handle_val(pvac::AggregatedRangeProof, v_arp);

    std::vector<uint8_t> buf;
    try {
        buf = pvac_ser::serialize_agg_range_proof(arp);
    } catch (const std::exception& e) {
        caml_failwith(e.what());
    }

    v_bytes = caml_alloc_string(buf.size());
    std::memcpy(Bytes_val(v_bytes), buf.data(), buf.size());

    DBG_EXIT("serialize_agg_range_proof");
    CAMLreturn(v_bytes);
}

CAMLprim value caml_pvac_verify_range_any(value v_pk, value v_ct, value v_proof_bytes) {
    CAMLparam3(v_pk, v_ct, v_proof_bytes);
    DBG_ENTER("verify_range_any");

    pvac::PubKey& pk = *Handle_val(pvac::PubKey, v_pk);
    pvac::Cipher& ct = *Handle_val(pvac::Cipher, v_ct);
    const uint8_t* data = bytes_data(v_proof_bytes);
    size_t len = bytes_len(v_proof_bytes);

    pvac_ser::RangeProofAny proof;
    bool ok = false;
    if (parse_range_any_safe(data, len, proof)) {
        caml_release_runtime_system();
        ok = verify_range_any_safe(pk, ct, proof);
        caml_acquire_runtime_system();
    }

    DBG_EXIT("verify_range_any");
    CAMLreturn(Val_bool(ok));
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