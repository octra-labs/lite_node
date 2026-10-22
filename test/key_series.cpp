// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include "pvac/pvac.hpp"
#include "pvac_serialize.hpp"
#include <fstream>
#include <iostream>
#include <stdexcept>

void check(bool value, const char* reason) {
    if (!value) throw std::runtime_error(reason);
}

void series_cases() {
    using namespace pvac;
    PubKey key;
    key.prm.B = 3;
    const u128 order = (u128{1} << 127) - 2;
    Fp ratio = fp_from_u64(1);
    for (uint64_t base = 2; ratio.lo == 1 && ratio.hi == 0; ++base)
        ratio = fp_pow_u128(fp_from_u64(base), order / 3);
    key.powg_B = {fp_from_u64(1), ratio, fp_mul(ratio, ratio)};
    check(pubkey_powers_ok(key), "cyclic series refused");
    for (size_t i = 0; i < key.powg_B.size(); ++i) {
        auto changed = key;
        changed.powg_B[i] = fp_add(changed.powg_B[i], fp_from_u64(1));
        check(!pubkey_powers_ok(changed), "changed series accepted");
    }
    auto changed = key;
    changed.powg_B[0] = Fp{1, 1};
    check(!pubkey_powers_ok(changed), "high first word accepted");
    changed = key;
    changed.powg_B[2].hi = 1;
    check(!pubkey_powers_ok(changed), "high inner word accepted");
    changed = key;
    changed.powg_B.pop_back();
    check(!pubkey_powers_ok(changed), "short series accepted");
    changed = key;
    changed.powg_B = {fp_from_u64(1), fp_from_u64(0), fp_from_u64(0)};
    check(!pubkey_powers_ok(changed), "zero ratio accepted");
    changed = key;
    changed.powg_B = {fp_from_u64(1), Fp{UINT64_MAX, MASK63}, fp_from_u64(0)};
    check(!pubkey_powers_ok(changed), "field modulus accepted");
    changed = key;
    const u128 alias = ((u128{ratio.hi} << 64) | ratio.lo) + ((u128{1} << 127) - 1);
    changed.powg_B[1] = Fp{static_cast<uint64_t>(alias), static_cast<uint64_t>(alias >> 64)};
    check(!pubkey_powers_ok(changed), "field alias accepted");
    changed.prm.B = 0;
    changed.powg_B.clear();
    check(!pubkey_powers_ok(changed), "empty series accepted");
    changed.prm.B = 1;
    changed.powg_B = {fp_from_u64(1)};
    check(!pubkey_powers_ok(changed), "unit series accepted");
    changed.prm.B = 2;
    changed.powg_B = {fp_from_u64(1), fp_neg(fp_from_u64(1))};
    check(!pubkey_powers_ok(changed), "short cycle accepted");
    changed.prm.B = 4;
    changed.powg_B.assign(4, fp_from_u64(1));
    check(!pubkey_powers_ok(changed), "unit ratio accepted");
    changed.powg_B = {fp_from_u64(1), fp_from_u64(3), fp_from_u64(9), fp_from_u64(27)};
    check(!pubkey_powers_ok(changed), "open series accepted");
    changed.powg_B = {fp_from_u64(1), fp_neg(fp_from_u64(1)),
        fp_from_u64(1), fp_neg(fp_from_u64(1))};
    check(pubkey_powers_ok(changed), "shorter divisor cycle refused");
    Params params;
    params.m_bits = 256;
    params.n_bits = 512;
    params.h_col_wt = 32;
    params.x_col_wt = 32;
    params.err_wt = 32;
    for (int branches : {3, 7, 9, 337}) {
        params.B = branches;
        for (uint8_t value : {uint8_t{51}, uint8_t{52}}) {
            const uint8_t seed[32] = {value};
            PubKey generated;
            SecKey secret;
            keygen_from_seed(params, generated, secret, seed);
            check(pubkey_powers_ok(generated), "generated cyclic key refused");
        }
    }
}

void sample(const char* path) {
    using namespace pvac;
    Params params;
    params.m_bits = 256;
    params.n_bits = 512;
    params.h_col_wt = 32;
    params.x_col_wt = 32;
    params.err_wt = 32;
    std::ofstream output(path, std::ios::binary);
    const auto write = [&](const auto& bytes) {
        check(bytes.size() <= UINT32_MAX, "sample too large");
        const auto size = static_cast<uint32_t>(bytes.size());
        for (int shift : {24, 16, 8, 0}) output.put(static_cast<char>(size >> shift));
        output.write(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    };
    PubKey key;
    SecKey secret;
    for (uint8_t value : {uint8_t{81}, uint8_t{82}}) {
        const uint8_t seed[32] = {value};
        keygen_from_seed(params, key, secret, seed);
        check(pubkey_powers_ok(key), "generated key refused");
        write(pvac_ser::serialize_pubkey(key));
        write(pvac_ser::serialize_seckey(secret));
    }
    auto changed = key;
    check(changed.powg_B.size() > 304, "key series sample is too short");
    for (size_t index = 301; index <= 304; ++index)
        changed.powg_B[index] = fp_add(changed.powg_B[index], fp_from_u64(1));
    check(!pubkey_powers_ok(changed), "changed key accepted");
    write(pvac_ser::serialize_pubkey(changed));
    write(pvac_ser::serialize_pubkey_raw(changed));
    const auto ratio = fp_mul(key.powg_B[1], key.powg_B[1]);
    for (size_t i = 1; i < changed.powg_B.size(); ++i)
        changed.powg_B[i] = fp_mul(changed.powg_B[i - 1], ratio);
    check(pubkey_powers_ok(changed), "other series refused");
    write(pvac_ser::serialize_pubkey(changed));
    changed = key;
    changed.powg_B[0] = Fp{1, 1};
    write(pvac_ser::serialize_pubkey(changed));
    changed = key;
    changed.powg_B[2].hi ^= 1;
    write(pvac_ser::serialize_pubkey(changed));
    write(pvac_ser::serialize_pubkey_raw(changed));
    changed = key;
    std::fill(changed.powg_B.begin() + 1, changed.powg_B.end(), fp_from_u64(0));
    write(pvac_ser::serialize_pubkey(changed));
    changed = key;
    std::fill(changed.powg_B.begin(), changed.powg_B.end(), fp_from_u64(1));
    write(pvac_ser::serialize_pubkey(changed));
    changed.prm.B = 2;
    changed.powg_B = {fp_from_u64(1), fp_neg(fp_from_u64(1))};
    write(pvac_ser::serialize_pubkey(changed));
    changed = key;
    for (size_t i = 1; i < changed.powg_B.size(); ++i)
        changed.powg_B[i] = fp_mul(changed.powg_B[i - 1], fp_from_u64(3));
    write(pvac_ser::serialize_pubkey(changed));
    output.close();
    check(output.good(), "sample write failed");
}

int main(int argc, char** argv) {
    try {
        series_cases();
        if (argc == 2) sample(argv[1]);
        else check(argc == 1, "invalid arguments");
        std::cout << "status = pass test = key_series\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "status = fail reason = " << error.what() << '\n';
        return 1;
    }
}