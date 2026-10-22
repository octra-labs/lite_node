// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <algorithm>
#include <atomic>
#include <exception>
#include <fstream>
#include <iostream>
#include <mutex>
#include <stdexcept>
#include <thread>
#include <vector>
#include "pvac/crypto/bulletproofs/r1cs.hpp"

thread_local uint64_t random_state = 1;

extern "C" void arc4random_buf(void* data, size_t size) {
    auto* bytes = static_cast<uint8_t*>(data);
    for (size_t i = 0; i < size; ++i) {
        random_state ^= random_state << 13;
        random_state ^= random_state >> 7;
        random_state ^= random_state << 17;
        bytes[i] = static_cast<uint8_t>(random_state);
    }
}

#if defined(__linux__)
extern "C" ssize_t getrandom(void* data, size_t size, unsigned int) {
    arc4random_buf(data, size);
    return static_cast<ssize_t>(size);
}
#endif

using namespace pvac;
using namespace pvac::bp;

void check(bool accepted, const char* reason) {
    if (!accepted) throw std::runtime_error(reason);
}

std::string proof_bytes(const R1CSProof& proof) {
    std::vector<uint8_t> bytes;
    const auto count = [&](size_t value) {
        for (size_t i = 0; i < 8; ++i) bytes.push_back(static_cast<uint8_t>(value >> (8 * i)));
    };
    const auto point = [&](const auto& value) {
        bytes.insert(bytes.end(), value.begin(), value.end());
    };
    const auto scalar = [&](const Scalar& value) {
        uint8_t words[32];
        sc_tobytes(words, value);
        bytes.insert(bytes.end(), words, words + sizeof words);
    };
    count(proof.V.size());
    for (const auto& value : proof.V) point(value);
    point(proof.A_I1); point(proof.A_O1); point(proof.S1);
    point(proof.T_1); point(proof.T_3); point(proof.T_4);
    point(proof.T_5); point(proof.T_6);
    scalar(proof.t_x); scalar(proof.t_x_blinding); scalar(proof.e_blinding);
    count(proof.ipp.L.size());
    for (const auto& value : proof.ipp.L) point(value);
    count(proof.ipp.R.size());
    for (const auto& value : proof.ipp.R) point(value);
    scalar(proof.ipp.a); scalar(proof.ipp.b);
    std::string text;
    for (auto byte : bytes) {
        text += "0123456789abcdef"[byte >> 4];
        text += "0123456789abcdef"[byte & 15];
    }
    return text;
}

std::string prove(size_t index) {
    const auto rule = index < 6 ? ScalarRule::Prior : ScalarRule::Wide;
    const size_t sizes[] = {1, 2, 7, 32, 128, 1025};
    const size_t count = sizes[index % 6];
    random_state = count + 73;
    R1CSProver prover(rule);
    for (size_t i = 0; i < count; ++i) {
        const auto [left, right, out] = prover.allocate(Scalar{{3, 0, 0, 0}}, Scalar{{4, 0, 0, 0}});
        prover.constrain(LinearCombination(out) -
            LinearCombination(Variable::one(), Scalar{{12, 0, 0, 0}}));
    }
    Transcript transcript("prover.sum", rule);
    const auto proof = prover.prove(transcript);
    ConstraintSystem system;
    system.num_gates = prover.num_gates();
    system.num_committed = prover.num_committed();
    system.constraints = prover.get_constraints();
    Transcript verify("prover.sum", rule);
    check(r1cs_verify(verify, system, proof), "honest proof refused");
    auto changed = proof;
    changed.t_x = sc_add(changed.t_x, Scalar{{1, 0, 0, 0}});
    Transcript refuse("prover.sum", rule);
    check(!r1cs_verify(refuse, system, changed), "changed proof accepted");
    return proof_bytes(proof);
}

std::vector<std::string> proofs(size_t workers) {
    std::vector<std::string> records(12);
    std::atomic<size_t> next{0};
    std::exception_ptr error;
    std::mutex lock;
    const auto work = [&] {
        try {
            for (;;) {
                const auto index = next.fetch_add(1);
                if (index >= records.size()) break;
                records[index] = prove(index);
            }
        } catch (...) {
            std::lock_guard<std::mutex> guard(lock);
            if (!error) error = std::current_exception();
        }
    };
    std::vector<std::thread> threads;
    try {
        for (size_t i = 1; i < workers; ++i) threads.emplace_back(work);
        work();
    } catch (...) {
        for (auto& thread : threads) thread.join();
        throw;
    }
    for (auto& thread : threads) thread.join();
    if (error) std::rethrow_exception(error);
    return records;
}

void sum_cases() {
    auto sum = ext_identity();
    for (size_t i = 0; i < 129; ++i) {
        const auto bytes = i % 7 == 0 ? rist_identity() : hash_to_ristretto_point("prover.add", i);
        const auto added = prover_add(sum, prover_point(bytes));
        sum = ext_add(sum, rist_decode_or_throw(bytes));
        check(rist_encode(added) == rist_encode(sum), "saved point sum differs");
        sum = ext_double(sum);
    }
    const auto base = hash_to_ristretto_point("prover.bits", 0);
    const auto other = hash_to_ristretto_point("prover.bits", 1);
    const auto opposite = rist_encode(ext_neg(rist_decode_or_throw(base)));
    const Scalar one{{1, 0, 0, 0}};
    check(prover_msm({one, one}, {base, opposite}) == rist_identity(),
        "paired cancellation differs");
    check(prover_msm({one, one, one}, {base, opposite, rist_identity()}) == rist_identity(),
        "bucket cancellation differs");
    for (size_t bit = 0; bit < 256; ++bit) {
        Scalar left = sc_zero();
        Scalar right = sc_zero();
        left.v[bit / 64] = uint64_t{1} << (bit % 64);
        right.v[(255 - bit) / 64] = uint64_t{1} << ((255 - bit) % 64);
        check(prover_msm({left, right}, {base, other}) ==
            multi_scalar_mul({left, right}, {base, other}), "paired bit differs");
    }
    for (const size_t count : {0, 1, 2, 3, 7, 16, 31, 257, 1025, 8193}) {
        std::vector<Scalar> scalars(count);
        std::vector<RistrettoPoint> points(count);
        random_state = count + 59;
        for (size_t i = 0; i < count; ++i) {
            uint64_t words[8];
            arc4random_buf(words, sizeof words);
            scalars[i] = i % 4 == 0 ? sc_zero() : ScalarOps(ScalarRule::Wide).reduce(words);
            points[i] = i % 7 == 0 ? rist_identity() : hash_to_ristretto_point("prover.sum", i % 31);
        }
        check(prover_msm(scalars, points) == multi_scalar_mul(scalars, points), "sum differs");
        check(((size_t{1} << prover_window(count)) - 1) * sizeof(ExtPoint) <= 256 * 1024,
            "bucket memory exceeds cap");
    }
    bool refused = false;
    try { prover_msm({sc_zero()}, {}); }
    catch (const std::runtime_error&) { refused = true; }
    check(refused, "unequal counts accepted");
    auto invalid = rist_identity();
    invalid.fill(255);
    refused = false;
    try { prover_msm({sc_zero()}, {invalid}); }
    catch (const std::exception&) { refused = true; }
    check(refused, "invalid point accepted");
    refused = false;
    try { prover_msm({sc_zero(), sc_zero(), sc_zero()}, {base, invalid, other}); }
    catch (const std::exception&) { refused = true; }
    check(refused, "zero coefficient hid invalid point");
}

int main(int argc, char** argv) {
    try {
        sum_cases();
        check(argc == 2, "reference path missing");
        const auto expected = proofs(1);
        std::ifstream input(argv[1]);
        check(input.good(), "reference missing");
        for (const auto& bytes : expected) {
            std::string value;
            check(bool(input >> value) && value == bytes, "proof bytes differ");
        }
        std::string extra;
        check(!(input >> extra), "reference has extra records");
        for (size_t workers : {2, 8})
            check(proofs(workers) == expected, "parallel proof bytes differ");
        std::cout << "status = pass test = prover_sum threads = 1,2,8 cases = 12\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "status = fail reason = " << error.what() << '\n';
        return 1;
    }
}