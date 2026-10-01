// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

class Runtime_scope {
public:
    Runtime_scope() { caml_release_runtime_system(); }
    ~Runtime_scope() { caml_acquire_runtime_system(); }
    Runtime_scope(const Runtime_scope&) = delete;
    Runtime_scope& operator=(const Runtime_scope&) = delete;
};

template<typename Check>
static value native_check(Check check) {
    CAMLparam0();
    bool accepted = false;
    bool exhausted = false;
    try {
        accepted = check();
    } catch (const std::bad_alloc&) {
        exhausted = true;
    } catch (...) {
        accepted = false;
    }
    if (exhausted) caml_raise_out_of_memory();
    CAMLreturn(Val_bool(accepted));
}

template<typename T, typename Make, typename Size>
static value native_handle(custom_operations* ops, Make make, Size size, bool result = false) {
    CAMLparam0();
    CAMLlocal3(owner, output, payload);
    owner = caml_alloc_custom_mem(ops, sizeof(T*), sizeof(T));
    Handle_val(T, owner) = nullptr;
    bool exhausted = false;
    bool failed = false;
    char error[256] = {0};
    try {
        Handle_val(T, owner) = new T(make());
    } catch (const std::bad_alloc&) {
        exhausted = true;
    } catch (const std::exception& ex) {
        failed = true;
        snprintf(error, sizeof(error), "%s", ex.what());
    } catch (...) {
        failed = true;
        snprintf(error, sizeof(error), "pvac: unknown native error");
    }
    if (exhausted) caml_raise_out_of_memory();
    if (failed) {
        if (!result) caml_failwith(error);
        payload = caml_copy_string(error);
        output = caml_alloc(1, 1);
        Store_field(output, 0, payload);
    } else {
        payload = wrap(ops, Handle_val(T, owner), size(*Handle_val(T, owner)));
        Handle_val(T, owner) = nullptr;
        if (!result) CAMLreturn(payload);
        output = caml_alloc(1, 0);
        Store_field(output, 0, payload);
    }
    CAMLreturn(output);
}

using Native_bytes = std::vector<uint8_t>;

using Native_values = std::vector<pvac::Fp>;

static custom_operations native_values_ops = {
    (char*)"pvac.values",
    [](value v) { handle_finalize<Native_values>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

struct Native_keys {
    pvac::PubKey pk;
    pvac::SecKey sk;
};

static custom_operations native_keys_ops = {
    (char*)"pvac.keys",
    [](value v) { handle_finalize<Native_keys>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

template<typename Make>
static value native_keys(Make make) {
    CAMLparam0();
    CAMLlocal4(owner, pk, sk, output);
    owner = native_handle<Native_keys>(&native_keys_ops, make,
        [](const Native_keys& keys) { return pubkey_mem(keys.pk) + seckey_mem(keys.sk); });
    pk = native_handle<pvac::PubKey>(&pubkey_ops,
        [&] { return std::move(Handle_val(Native_keys, owner)->pk); }, pubkey_mem);
    sk = native_handle<pvac::SecKey>(&seckey_ops,
        [&] { return std::move(Handle_val(Native_keys, owner)->sk); }, seckey_mem);
    delete Handle_val(Native_keys, owner);
    Handle_val(Native_keys, owner) = nullptr;
    output = caml_alloc_tuple(2);
    Store_field(output, 0, pk);
    Store_field(output, 1, sk);
    CAMLreturn(output);
}

static custom_operations native_bytes_ops = {
    (char*)"pvac.bytes",
    [](value v) { handle_finalize<Native_bytes>(v); },
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

template<typename Make>
static value native_bytes(Make make) {
    CAMLparam0();
    CAMLlocal2(owner, output);
    owner = native_handle<Native_bytes>(&native_bytes_ops, make,
        [](const Native_bytes& bytes) { return sizeof(bytes) + bytes.capacity(); });
    Native_bytes* bytes = Handle_val(Native_bytes, owner);
    output = caml_alloc_string(bytes->size());
    std::memcpy(Bytes_val(output), bytes->data(), bytes->size());
    delete bytes;
    Handle_val(Native_bytes, owner) = nullptr;
    CAMLreturn(output);
}