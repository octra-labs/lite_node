// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include "pvac_serialize.hpp"
#include <atomic>
#include <iostream>
#include <thread>
#include <type_traits>
#ifdef PVAC_C_API
#include "pvac_c_api.h"
#endif

void check(bool value, const char* name) {
    if (!value) throw std::runtime_error(name);
}

std::string hex(const uint8_t* data, size_t size) {
    const char* digits = "0123456789abcdef";
    std::string out;
    for (size_t i = 0; i < size; ++i) {
        out += digits[data[i] >> 4];
        out += digits[data[i] & 15];
    }
    return out;
}

void shake_case(const std::vector<uint8_t>& input, const std::string& expected) {
    pvac::Shake256 whole;
    whole.init();
    whole.absorb(input.data(), input.size());
    std::vector<uint8_t> output(expected.size() / 2);
    whole.squeeze(output.data(), output.size());
    check(hex(output.data(), output.size()) == expected, "shake vector");
    pvac::Shake256 split;
    split.init();
    for (const auto& byte : input) split.absorb(&byte, 1);
    for (auto& byte : output) split.squeeze(&byte, 1);
    check(hex(output.data(), output.size()) == expected, "shake chunks");
}

void shake_cases() {
    for (int i = 0; i < 64; ++i) {
        const auto x = uint64_t{1} << i;
        check(pvac::Shake256::rotl(x, 0) == x, "rotation zero");
        check(pvac::Shake256::rotl(x, 1) == (i == 63 ? 1 : x << 1), "rotation one");
    }
    shake_case({}, "46b9dd2b0ba88d13233b3feb743eeb243fcd52ea62b81b82b50c27646ed5762fd75dc4ddd8c0f200cb05019d67b592f6fc821c49479ab48640292eacb3b7c4be");
    shake_case({'a', 'b', 'c'}, "483366601360a8771c6863080cc4114d8db44530f8f1e1ee4f94ea37e78b5739d5a15bef186a5386c75744c0527e1faa9f8726e462a12a4feb06bd8801e751e4");
    std::vector<uint8_t> input(256);
    std::iota(input.begin(), input.end(), 0);
    shake_case(input, "336c8aa7f2b08bda6bd7402cd2ea89760b7728a8b31802b80524756361165366ff8159f2f4568a2bfa286db6387895629938c2868a6421c37f988455763a75e4b9259e0a939aaa68295119ccea72c9f0ca7d048aa70eeeb4534c6bd08ecc6163217c790f33b84a89623f8e5538b734967e9490a48b7d0658afb4565364e8b234dfe6a2bceb12ce2130eec00bf2113615a276819d7815f5891d07600275f4d8fb");
}

void sample_cases() {
    const uint8_t seed[32] = {7};
    for (const auto& item : std::vector<std::pair<size_t, int>>{
             {1, 0}, {1, -1}, {8, 7}, {65537, 65537}}) {
        for (bool seeded : {false, true}) {
            auto rng = pvac::make_seeded_rng(seed);
            auto prior = rng;
            uint16_t output = 91;
            bool refused = false;
            try {
                if (seeded) pvac::detail::sample_unique_indices(&output, item.first, item.second, rng);
                else pvac::detail::sample_unique_indices(&output, item.first, item.second);
            } catch (const std::runtime_error&) {
                refused = true;
            }
            check(refused && output == 91 && rng.u64() == prior.u64(), "sample refusal");
        }
    }
    auto rng = pvac::make_seeded_rng(seed);
    pvac::detail::sample_unique_indices(nullptr, 0, 0, rng);
    pvac::detail::sample_unique_indices(nullptr, 0, -1);
    for (int size : {1, 8, 64}) {
        std::vector<uint16_t> output(size);
        pvac::detail::sample_unique_indices(output.data(), output.size(), size, rng);
        std::sort(output.begin(), output.end());
        for (int i = 0; i < size; ++i) check(output[i] == i, "sample domain");
    }
}

void reader_cases() {
    const uint8_t bytes[8] = {};
    for (size_t length = 0; length <= sizeof(bytes); ++length) {
        pvac_ser::Reader reader(bytes, length);
        reader.u64();
        check(reader.failed == (length < 8), "reader width");
        if (reader.failed) check(std::string(reader.error) == "pvac_ser: truncated", "reader reason");
    }
    pvac_ser::Reader reader(bytes, sizeof(bytes));
    reader.need(std::numeric_limits<size_t>::max());
    check(reader.failed && reader.p == bytes, "reader length");
    pvac_ser::Reader empty(nullptr, 0);
    check(empty.remaining() == 0, "reader empty");
    empty.u8();
    check(empty.failed, "reader empty read");
}

void product_case(bool wire) {
    pvac::PubKey key{};
    key.prm.B = 1;
    key.prm.m_bits = 1;
    key.prm.n_bits = 1;
    key.H = {pvac::BitVec::make(1)};
    key.ubk.perm = {0};
    key.ubk.inv = {0};
    key.omega_B = pvac::fp_from_u64(1);
    key.powg_B = {key.omega_B};
    pvac::Cipher cipher;
    cipher.L = {pvac::Layer{}};
    cipher.c0 = {pvac::fp_from_u64(0)};
    const auto pk_raw = pvac_ser::serialize_pubkey(key, false);
    const auto ct_raw = pvac_ser::serialize_cipher(cipher);
    const auto pk = pvac_ser::deserialize_pubkey(pk_raw.data(), pk_raw.size());
    const auto ct = pvac_ser::deserialize_cipher(ct_raw.data(), ct_raw.size(), true, true);
    check(pvac::is_cipher_compatible_with_pubkey(pk, ct), "product input");
    const uint8_t seed[32] = {};
    bool refused = false;
    try { pvac::ct_mul_seeded(pk, ct, ct, seed); }
    catch (const std::runtime_error& error) {
        refused = std::string(error.what()) == "pvac: index count exceeds domain";
    }
    check(refused, "product refusal");
    pvac::RangeProof range;
    range.ct_bit.assign(pvac::RANGE_BITS, ct);
    range.bit_proofs.resize(pvac::RANGE_BITS);
    check(!pvac::verify_range(pk, ct, range), "range product refusal");
#ifdef PVAC_C_API
    check(pvac_ct_mul_seeded(&key, &cipher, &cipher, seed) == nullptr, "C product refusal");
    pvac::SecKey secret{};
    check(pvac_make_range_proof(&key, &secret, &cipher, 0) == nullptr, "C range refusal");
    check(pvac_make_aggregated_range_proof(&key, &secret, &cipher, 0) == nullptr, "C aggregate refusal");
#endif
    if (wire) {
        std::cout << hex(pk_raw.data(), pk_raw.size()) << "\n";
        std::cout << hex(ct_raw.data(), ct_raw.size()) << "\n";
    }
}

void table_cases() {
    std::atomic<unsigned> finished{0};
    bool refused = false;
    try {
        pvac::detail::range_run(4, [&](size_t from, size_t) {
            ++finished;
            if (from == 0) throw std::runtime_error("range task");
        });
    } catch (const std::runtime_error& error) {
        refused = std::string(error.what()) == "range task";
    }
    check(refused && finished == 4, "range joins");
    pvac::bp::GeneratorTable table;
    check(!std::is_reference<decltype(table.G(0))>::value, "generator ownership");
    std::atomic<bool> go{false};
    std::atomic<bool> ok{true};
    std::vector<std::thread> threads;
    for (size_t lane = 0; lane < 4; ++lane) {
        threads.emplace_back([&, lane] {
            while (!go.load()) std::this_thread::yield();
            try {
                for (size_t i = 0; i < 32; ++i) {
                    const auto point = table.G(i + lane * 32);
                    table.precompute(1 + i + lane * 32);
                    if (point != pvac::bp::hash_to_ristretto_point("pvac.bp.gen.G", i + lane * 32)) ok = false;
                    const std::vector<uint8_t> raw(4096, static_cast<uint8_t>(lane));
                    if (pvac::compress::unpack(pvac::compress::pack(raw)) != raw) ok = false;
                }
            } catch (...) { ok = false; }
        });
    }
    go = true;
    for (auto& thread : threads) thread.join();
    check(ok, "table concurrency");
    for (size_t i = 0; i < 256; ++i)
        check(pvac::compress::detail::AdaptiveState::rate_table[i] == 32768 / (i + i + 3), "rate value");
}

std::string vector_digest() {
    const uint8_t seed[32] = {17};
    auto rng = pvac::make_seeded_rng(seed);
    std::vector<uint8_t> raw;
    for (int size : {8, 64, 256}) {
        std::array<uint16_t, 8> output;
        pvac::detail::sample_unique_indices(output.data(), output.size(), size, rng);
        for (auto value : output) {
            raw.push_back(value & 255);
            raw.push_back(value >> 8);
        }
    }
    for (int i = 0; i < 2048; ++i) raw.push_back(rng.u64() & 255);
    const auto packed = pvac::compress::pack(raw);
    pvac::Sha256 digest;
    digest.init();
    digest.update(packed.data(), packed.size());
    uint8_t output[32];
    digest.finish(output);
    return hex(output, sizeof(output));
}

pvac::Scalar reduce_ref(const uint64_t words[8]) {
    pvac::Scalar out = pvac::sc_zero();
    for (int bit = 511; bit >= 0; --bit) {
        uint64_t carry = (words[bit / 64] >> (bit % 64)) & 1;
        for (int i = 0; i < 4; ++i) {
            const uint64_t next = out.v[i] >> 63;
            out.v[i] = (out.v[i] << 1) | carry;
            carry = next;
        }
        if (!pvac::sc_is_canonical(out)) {
            uint64_t borrow = 0;
            for (int i = 0; i < 4; ++i) {
                const auto word = pvac::u128(out.v[i]) - pvac::SC_L[i] - borrow;
                out.v[i] = uint64_t(word);
                borrow = uint64_t(word >> 127);
            }
        }
    }
    return out;
}

bool scalar_eq(const pvac::Scalar& a, const pvac::Scalar& b) {
    return std::equal(a.v, a.v + 4, b.v);
}

void scalar_cases() {
    using namespace pvac;
    const ScalarOps wide(ScalarRule::Wide);
    for (uint64_t k = 1; k <= 8; ++k) {
        const Scalar near{{SC_L[0] - k, SC_L[1], SC_L[2], SC_L[3]}};
        for (int limb : {1, 2, 3}) {
            uint64_t words[8] = {};
            std::copy(near.v, near.v + 4, words + limb);
            Scalar power = sc_zero();
            power.v[limb] = 1;
            const auto expected = reduce_ref(words);
            check(scalar_eq(wide.reduce(words), expected), "scalar reduction");
            check(scalar_eq(wide.mul(near, power), expected), "scalar product");
        }
        check(scalar_eq(wide.mul(near, wide.inv(near)), bp::sc_from_u64(1)), "scalar inverse");
    }
    uint64_t words[8] = {SC_L[0], SC_L[1], SC_L[2], SC_L[3]};
    check(scalar_eq(sc_reduce512(words), wide.reduce(words)), "scalar narrow");
    for (size_t bit = 0; bit < 512; ++bit) {
        std::fill(words, words + 8, 0);
        words[bit / 64] = uint64_t{1} << (bit % 64);
        check(scalar_eq(wide.reduce(words), reduce_ref(words)), "scalar bit");
        if (bit <= 256)
            check(scalar_eq(sc_reduce512(words), wide.reduce(words)), "scalar prior bit");
    }
    std::fill(words, words + 8, std::numeric_limits<uint64_t>::max());
    check(scalar_eq(wide.reduce(words), reduce_ref(words)), "scalar full width");
    const Scalar near{{SC_L[0] - 1, SC_L[1], SC_L[2], SC_L[3]}};
    const Scalar power{{0, 1, 0, 0}};
    check(!scalar_eq(sc_mul(near, power), wide.mul(near, power)), "scalar prior retained");
}

void proof_math_cases() {
    using namespace pvac;
    for (const auto rule : {ScalarRule::Prior, ScalarRule::Wide}) {
        bp::R1CSProver prover(rule);
        const auto [left, right, out] = prover.allocate(bp::sc_from_u64(3), bp::sc_from_u64(4));
        prover.constrain(bp::LinearCombination(out) - bp::LinearCombination(bp::Variable::one(), bp::sc_from_u64(12)));
        bp::Transcript transcript("math", rule);
        const auto proof = prover.prove(transcript);
        bp::ConstraintSystem system;
        system.num_gates = prover.num_gates();
        system.num_committed = prover.num_committed();
        system.constraints = prover.get_constraints();
        bp::Transcript verify("math", rule);
        check(bp::r1cs_verify(verify, system, proof), "proof scalar rule");
        bp::Transcript other("math", rule == ScalarRule::Prior ? ScalarRule::Wide : ScalarRule::Prior);
        bool refused = false;
        try { prover.prove(other); }
        catch (const std::runtime_error& error) { refused = std::string(error.what()) == "pvac: scalar rule mismatch"; }
        check(refused, "proof rule mismatch");
    }
}

bool field_eq(const pvac::Fp& a, const pvac::Fp& b) {
    return a.lo == b.lo && a.hi == b.hi;
}

void cipher_math_cases() {
    using namespace pvac;
    Params params;
    PubKey key;
    SecKey secret;
    const uint8_t seed[32] = {31};
    keygen_from_seed(params, key, secret, seed);
    Cipher zero;
    zero.c0 = {fp_from_u64(0)};
    auto empty = zero;
    empty.c0.clear();
    const auto one = enc_value_seeded(key, secret, 1, seed);
    check(is_cipher_compatible_with_pubkey(key, empty), "empty input shape");
    for (const auto value : {uint64_t{0}, uint64_t{1}, std::numeric_limits<uint64_t>::max()}) {
        const auto expected = fp_neg(fp_from_u64(value));
        check(field_eq(dec_value(key, secret, ct_sub_const(key, zero, value)), expected), "unsigned subtraction");
        const auto next = ct_sub_const(key, empty, value, true);
        check(field_eq(dec_value(key, secret, next), expected), "empty subtraction");
        check(pvac_ser::serialize_cipher(next) == pvac_ser::serialize_cipher(ct_sub_const(key, zero, value, true)), "subtraction representation");
    }
    for (const auto value : {int64_t{-1}, std::numeric_limits<int64_t>::min(), std::numeric_limits<int64_t>::max()}) {
        const auto expected = fp_neg(detail::fp_from_i64(value));
        check(field_eq(dec_value(key, secret, ct_sub_const(key, zero, value)), expected), "signed subtraction");
    }
    check(ct_add_const(key, empty, uint64_t{7}, false).c0.empty(), "prior empty constant");
    const auto next = ct_add_const(key, empty, uint64_t{7}, true);
    check(field_eq(dec_value(key, secret, next), fp_from_u64(7)), "empty addition");
    check(pvac_ser::serialize_cipher(next) == pvac_ser::serialize_cipher(ct_add_const(key, zero, uint64_t{7}, true)), "addition representation");
    for (const auto& pair : std::vector<std::pair<Cipher, Cipher>>{{empty, one}, {one, empty}, {empty, empty}}) {
        const auto product = ct_mul_seeded(key, pair.first, pair.second, seed, 8, true);
        check(field_eq(dec_value(key, secret, product), fp_from_u64(0)), "empty product");
    }
    check(empty.c0.empty() && zero.c0.size() == 1, "input unchanged");
#ifdef PVAC_C_API
    auto input = one;
    check(pvac_ct_div_const(&key, &input, 0, 0) == nullptr, "C zero divisor");
    check(pvac_ct_div_const(&key, &input, UINT64_MAX, UINT64_MAX >> 1) == nullptr, "C modulus divisor");
    check(pvac_ct_div_const(&key, &input, UINT64_MAX - 1, UINT64_MAX) == nullptr, "C double modulus divisor");
    for (const auto value : {int64_t{-1}, std::numeric_limits<int64_t>::min(), int64_t{1}}) {
        const auto output = pvac_ct_scale(&key, &input, value);
        check(output != nullptr, "C scale success");
        const auto actual = dec_value(key, secret, *static_cast<Cipher*>(output));
        pvac_free_cipher(output);
        check(field_eq(actual, detail::fp_from_i64(value)), "C scale sign");
    }
    const auto divided = pvac_ct_div_const(&key, &input, 1, 0);
    check(divided != nullptr, "C division success");
    const auto actual = dec_value(key, secret, *static_cast<Cipher*>(divided));
    pvac_free_cipher(divided);
    check(field_eq(actual, fp_from_u64(1)), "C division value");
#endif
}

#ifdef PVAC_C_API
void api_proof_cases() {
    using namespace pvac;
    Params params;
    PubKey key;
    SecKey secret;
    const uint8_t seed[32] = {43};
    const uint8_t blind[32] = {11};
    keygen_from_seed(params, key, secret, seed);
    auto zero = enc_value_seeded(key, secret, 0, seed);
    auto one = enc_value_seeded(key, secret, 1, seed);
    const auto proof = pvac_make_zero_proof(&key, &secret, &zero);
    check(proof != nullptr, "C zero proof");
    check(pvac_verify_zero(&key, &zero, proof) == 1, "C zero verification");
    check(verify_zero(key, zero, *static_cast<ZeroProof*>(proof), ScalarRule::Wide), "C zero current");
    check(verify_zero(key, zero, *static_cast<ZeroProof*>(proof), ScalarRule::Prior), "C zero prior control");
    check(pvac_verify_zero(&key, &one, proof) == 0, "C zero statement");
    pvac_free_zero_proof(proof);
    uint8_t commitment[32];
    pvac_pedersen_commit(1, blind, commitment);
    const auto amount = pvac_make_zero_proof_bound(&key, &secret, &one, 1, blind);
    check(amount != nullptr, "C amount proof");
    check(pvac_verify_zero_bound(&key, &one, amount, commitment) == 1, "C amount verification");
    RistrettoPoint point;
    std::copy(commitment, commitment + 32, point.begin());
    check(verify_zero_bound(key, one, *static_cast<ZeroProof*>(amount), point, ScalarRule::Wide), "C amount current");
    check(verify_zero_bound(key, one, *static_cast<ZeroProof*>(amount), point, ScalarRule::Prior), "C amount prior control");
    pvac_pedersen_commit(2, blind, commitment);
    check(pvac_verify_zero_bound(&key, &one, amount, commitment) == 0, "C amount statement");
    pvac_free_zero_proof(amount);
    check(pvac_make_zero_proof(nullptr, &secret, &zero) == nullptr, "C zero missing key");
    check(pvac_make_zero_proof_bound(&key, &secret, &one, 1, nullptr) == nullptr, "C amount missing blinding");
    check(pvac_verify_zero(&key, &zero, nullptr) == 0, "C zero missing proof");
    check(pvac_verify_zero_bound(&key, &one, nullptr, commitment) == 0, "C amount missing proof");
}
#endif

int main(int argc, char** argv) {
    try {
        const std::string mode = argc > 1 ? argv[1] : "all";
        if (mode == "vector") { std::cout << "vector = " << vector_digest() << "\n"; return 0; }
        if (mode == "wire") { product_case(true); return 0; }
        if (mode == "all") check(vector_digest() == "ef53594fe19cae770e67aea7ac2f7ead7b881e968843c40fd881f9441f440839", "prior vector");
        if (mode == "all" || mode == "shake") shake_cases();
        if (mode == "all" || mode == "sample") sample_cases();
        if (mode == "all" || mode == "reader") reader_cases();
        if (mode == "all" || mode == "product") product_case(false);
        if (mode == "all" || mode == "table") table_cases();
        if (mode == "all" || mode == "scalar") scalar_cases();
        if (mode == "all" || mode == "proof") proof_math_cases();
        if (mode == "all" || mode == "cipher") cipher_math_cases();
#ifdef PVAC_C_API
        if (mode == "all" || mode == "api") api_proof_cases();
#endif
        std::cout << "status = pass test = native_math\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "status = fail reason = " << error.what() << "\n";
        return 1;
    }
}