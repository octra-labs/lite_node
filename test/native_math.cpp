// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include "pvac_serialize.hpp"
#include <atomic>
#include <chrono>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <new>
#include <thread>
#include <type_traits>
#ifdef PVAC_C_API
#include "pvac_c_api.h"
#endif

thread_local bool measure_alloc = false;
thread_local size_t allocated_bytes = 0;
struct LiveAllocation {
    void* pointer;
    size_t size;
};
thread_local std::array<LiveAllocation, 4096> live_allocations{};
thread_local bool measure_peak = false;
thread_local bool peak_overflow = false;
thread_local size_t live_bytes = 0;
thread_local size_t peak_bytes = 0;

void* operator new(size_t size) {
    if (measure_alloc) allocated_bytes += size;
    if (void* ptr = std::malloc(size == 0 ? 1 : size)) {
        if (measure_peak) {
            auto slot = std::find_if(live_allocations.begin(), live_allocations.end(),
                [](const auto& item) { return item.pointer == nullptr; });
            if (slot == live_allocations.end()) peak_overflow = true;
            else {
                *slot = {ptr, size};
                live_bytes += size;
                peak_bytes = std::max(peak_bytes, live_bytes);
            }
        }
        return ptr;
    }
    throw std::bad_alloc();
}

void* operator new[](size_t size) { return ::operator new(size); }
void operator delete(void* ptr) noexcept {
    if (ptr && measure_peak) {
        auto slot = std::find_if(live_allocations.begin(), live_allocations.end(),
            [ptr](const auto& item) { return item.pointer == ptr; });
        if (slot != live_allocations.end()) {
            live_bytes -= slot->size;
            *slot = {};
        }
    }
    std::free(ptr);
}
void operator delete[](void* ptr) noexcept { ::operator delete(ptr); }
void operator delete(void* ptr, size_t) noexcept { ::operator delete(ptr); }
void operator delete[](void* ptr, size_t) noexcept { ::operator delete(ptr); }

template<typename Run>
size_t allocation_size(Run run) {
    allocated_bytes = 0;
    measure_alloc = true;
    try {
        run();
    } catch (...) {
        measure_alloc = false;
        throw;
    }
    measure_alloc = false;
    return allocated_bytes;
}

void check(bool value, const char* name) {
    if (!value) throw std::runtime_error(name);
}

template<typename Run>
size_t allocation_peak(Run run) {
    check(!measure_peak, "nested allocation measurement");
    live_allocations.fill({});
    live_bytes = 0;
    peak_bytes = 0;
    peak_overflow = false;
    measure_peak = true;
    try {
        run();
    } catch (...) {
        measure_peak = false;
        throw;
    }
    measure_peak = false;
    check(!peak_overflow && live_bytes == 0, "allocation measurement incomplete");
    return peak_bytes;
}

void peak_meter_cases() {
    void* earlier = ::operator new(1000);
    const auto peak = allocation_peak([&] {
        ::operator delete(earlier);
        void* first = ::operator new(128);
        void* second = ::operator new[](256);
        ::operator delete(first);
        void* third = ::operator new(64);
        ::operator delete[](second);
        ::operator delete(third);
    });
    check(peak == 384, "allocation peak differs");
    bool raised = false;
    try {
        allocation_peak([] { throw 7; });
    } catch (int value) {
        raised = value == 7;
    }
    check(raised && !measure_peak, "allocation exception state");
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

std::vector<uint8_t> edge_bytes(const std::vector<pvac::Edge>& edges) {
    pvac_ser::Writer writer;
    for (const auto& edge : edges) pvac_ser::write_edge(writer, edge);
    return writer.buf;
}

std::vector<pvac::Edge> dense_merge(const std::vector<pvac::Edge>& edges, int basis, int layers) {
    struct Slot { bool active = false; pvac::Edge edge; };
    std::vector<Slot> plus(basis * layers), minus(basis * layers);
    for (const auto& edge : edges) {
        auto& slot = (edge.ch == pvac::SGN_P ? plus : minus)[edge.layer_id * basis + edge.idx];
        if (!slot.active) {
            slot.active = true;
            slot.edge = edge;
        } else {
            for (size_t j = 0; j < edge.w.size(); ++j)
                slot.edge.w[j] = pvac::fp_add(slot.edge.w[j], edge.w[j]);
            slot.edge.s.xor_with(edge.s);
        }
    }
    std::vector<pvac::Edge> output;
    for (int layer = 0; layer < layers; ++layer) {
        for (int index = 0; index < basis; ++index) {
            for (auto* slots : {&plus, &minus}) {
                auto& slot = (*slots)[layer * basis + index];
                if (slot.active && (std::any_of(slot.edge.w.begin(), slot.edge.w.end(),
                        [](const pvac::Fp& value) { return pvac::ct::fp_is_nonzero(value); })
                        || std::any_of(slot.edge.s.w.begin(), slot.edge.s.w.end(),
                            [](uint64_t value) { return value != 0; })))
                    output.push_back(std::move(slot.edge));
            }
        }
    }
    return output;
}

void merge_cases() {
    pvac::PubKey key{};
    key.prm.m_bits = 65;
    for (int basis : {1, 7, 337}) {
        key.prm.B = basis;
        for (int layers : {1, 3, 17}) {
            for (int width : {1, 8}) {
                std::vector<pvac::Edge> edges;
                for (uint32_t n = 0; n < 300; ++n) {
                    pvac::Edge edge{n % static_cast<uint32_t>(layers),
                        static_cast<uint16_t>((n * 11) % basis),
                        n % 2 ? pvac::SGN_P : pvac::SGN_M,
                        std::vector<pvac::Fp>(width, pvac::fp_from_u64(n % 19)), pvac::BitVec::make(65)};
                    edge.s.w[0] = n;
                    edge.s.w[1] = n % 2;
                    edges.push_back(edge);
                    for (auto& value : edge.w) value = pvac::fp_neg(value);
                    edges.push_back(edge);
                    if (n % 3) edges.push_back(edge);
                }
                for (int order = 0; order < 3; ++order) {
                    const auto expected = edge_bytes(dense_merge(edges, basis, layers));
                    const auto result = pvac::reduction::merge(pvac::alg::Carrier<pvac::Edge>{edges}, key);
                    check(edge_bytes(result.data) == expected, "merge prior bytes");
                    std::rotate(edges.begin(), edges.begin() + edges.size() / 3, edges.end());
                    std::reverse(edges.begin(), edges.end());
                }
            }
        }
    }
    key.prm.B = std::numeric_limits<int>::max();
    const pvac::Edge edge{0x7fffffff, 65535, pvac::SGN_M,
        {pvac::fp_from_u64(9)}, pvac::BitVec::make(65)};
    const auto result = pvac::reduction::merge(pvac::alg::Carrier<pvac::Edge>{{edge}}, key);
    check(edge_bytes(result.data) == edge_bytes({edge}), "merge sparse domain");
    for (int bad = 0; bad < 4; ++bad) {
        auto altered = edge;
        if (bad == 0) altered.ch = 9;
        if (bad == 1) altered.w.clear();
        if (bad == 2) altered.s = pvac::BitVec::make(64);
        key.prm.B = bad == 3 ? 1 : std::numeric_limits<int>::max();
        bool refused = false;
        try { pvac::reduction::merge(pvac::alg::Carrier<pvac::Edge>{{altered}}, key); }
        catch (const std::runtime_error&) { refused = true; }
        check(refused, "merge malformed shape");
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

size_t r1cs_peak_limit(size_t gates) {
    using namespace pvac;
    using namespace pvac::bp;
    const auto count = next_power_of_2(gates);
    const auto depth = log2_size(count);
    const auto msm = [](size_t size) {
        size_t width = 0;
        for (size_t value = size; value; value >>= 1) ++width;
        const auto buckets = (size_t{1} << std::min(width, size_t{16})) - 1;
        return size * (sizeof(Scalar) + sizeof(RistrettoPoint) + sizeof(ExtPoint) + 32)
            + buckets * sizeof(ExtPoint);
    };
    const auto outer = (4 * count + 1) * sizeof(Scalar) + msm(2 * count + 5);
    const auto inner = (3 * count + 3 * depth) * sizeof(Scalar) + msm(2 * count + 2 * depth + 2);
    return std::max(outer, inner) + 4096;
}

void proof_peak_cases(const std::string& mode, const std::string& path) {
    using namespace pvac;
    using namespace pvac::bp;
    peak_meter_cases();
    const bool writing = mode == "peak-write";
    const bool reading = mode == "peak-read";
    std::vector<uint8_t> input;
    if (reading) {
        std::ifstream file(path, std::ios::binary | std::ios::ate);
        check(file.good() && file.tellg() > 0 && file.tellg() < 1'000'000, "proof file size");
        input.resize(static_cast<size_t>(file.tellg()));
        file.seekg(0);
        file.read(reinterpret_cast<char*>(input.data()), input.size());
        check(file.good(), "proof file read");
    }
    pvac_ser::Reader reader(input.data(), input.size());
    pvac_ser::Writer output;
    for (const auto rule : {ScalarRule::Prior, ScalarRule::Wide}) {
        for (size_t gates : {1, 3, 17, 256, 1024}) {
            R1CSProver prover(rule);
            const auto committed = prover.commit(sc_from_u64(3), sc_from_u64(17));
            for (size_t i = 0; i < gates; ++i) {
                const auto [left, right, product] = prover.multiply(LinearCombination(committed),
                    LinearCombination(Variable::one(), sc_from_u64(4)));
                prover.constrain(LinearCombination(product) -
                    LinearCombination(Variable::one(), sc_from_u64(12)));
            }
            ConstraintSystem system{prover.num_gates(), prover.num_committed(), prover.get_constraints()};
            R1CSProof proof;
            if (reading) {
                check(reader.u8() == static_cast<uint8_t>(rule) && reader.u64() == gates,
                    "proof file order");
                proof = pvac_ser::read_r1cs_proof_raw(reader);
                check(!reader.failed, "proof file decode");
            } else {
                Transcript proving("peak", rule);
                proof = prover.prove(proving);
                if (writing) {
                    output.u8(static_cast<uint8_t>(rule));
                    output.u64(gates);
                    pvac_ser::write_r1cs_proof_raw(output, proof);
                }
            }
            generators().precompute(system.padded_gates());
            for (int variant = 0; variant < (writing ? 7 : 8); ++variant) {
                auto altered = proof;
                auto relation = system;
                if (variant == 1) altered.t_x = sc_add(altered.t_x, sc_from_u64(1));
                if (variant == 2) altered.ipp.a = sc_add(altered.ipp.a, sc_from_u64(1));
                if (variant == 3) relation.constraints.front().lc +=
                    LinearCombination(Variable::one(), sc_from_u64(1));
                if (variant == 4) altered.V.clear();
                if (variant == 5) altered.A_I1.fill(255);
                if (variant == 6) altered.ipp.R.push_back(rist_identity());
                if (variant == 7) altered.e_blinding = sc_add(altered.e_blinding, sc_from_u64(1));
                bool accepted = false;
                Transcript checked("peak", rule);
                const auto peak = allocation_peak([&] {
                    accepted = r1cs_verify(checked, relation, altered);
                });
                check(accepted == (variant == 0), "proof outcome differs");
                const auto challenge = checked.challenge_scalar("after_verify");
                if (writing) {
                    output.u8(accepted);
                    output.scalar(challenge);
                }
                if (reading && variant < 7) {
                    check(reader.u8() == accepted && scalar_eq(reader.scalar(), challenge),
                        "proof transcript changed");
                    check(!reader.failed, "proof result decode");
                }
                std::cout << "event = r1cs_peak gates = " << gates
                    << " math = " << (rule == ScalarRule::Wide) << " variant = " << variant
                    << " peak = " << peak << " limit = " << r1cs_peak_limit(gates) << "\n";
                if (!writing) check(peak <= r1cs_peak_limit(gates), "r1cs peak retention");
            }
        }
    }
    if (reading) check(reader.remaining() == 0, "proof file extra bytes");
    if (writing) {
        std::ofstream file(path, std::ios::binary | std::ios::trunc);
        file.write(reinterpret_cast<const char*>(output.buf.data()), output.buf.size());
        file.close();
        check(file.good(), "proof file write");
    }
}

bool same_constraints(const std::vector<pvac::bp::Constraint>& left,
    const std::vector<pvac::bp::Constraint>& right) {
    if (left.size() != right.size()) return false;
    for (size_t i = 0; i < left.size(); ++i) {
        const auto& a = left[i].lc.terms;
        const auto& b = right[i].lc.terms;
        if (a.size() != b.size()) return false;
        for (size_t j = 0; j < a.size(); ++j) {
            if (a[j].first.type != b[j].first.type || a[j].first.index != b[j].first.index ||
                !scalar_eq(a[j].second, b[j].second)) return false;
        }
    }
    return true;
}

void build_relation(pvac::bp::R1CSProver& builder, bool witness) {
    using namespace pvac;
    using namespace pvac::bp;
    const auto x = builder.commit(sc_from_u64(witness ? 3 : 0), sc_from_u64(17));
    auto [a, b, product] = builder.multiply(LinearCombination(x),
        LinearCombination(Variable::one(), sc_from_u64(4)));
    builder.constrain(LinearCombination(product) -
        LinearCombination(Variable::one(), sc_from_u64(12)));
    const auto [c, d, sum] = builder.allocate(sc_from_u64(witness ? 7 : 0), sc_from_u64(1));
    builder.constrain(LinearCombination(sum) -
        LinearCombination(Variable::one(), sc_from_u64(7)));
    builder.multiply(LinearCombination(product) + LinearCombination(sum), LinearCombination(x));
}

void build_cases() {
    using namespace pvac;
    using namespace pvac::bp;
    for (const auto rule : {ScalarRule::Prior, ScalarRule::Wide}) {
        R1CSProver prover(rule);
        R1CSProver prior(rule);
        auto checked = R1CSProver::verification(rule);
        build_relation(prover, true);
        build_relation(prior, false);
        build_relation(checked, false);
        const auto expected = prior.get_constraints();
        check(same_constraints(prover.get_constraints(), expected), "witness changes constraints");
        check(same_constraints(checked.get_constraints(), expected), "verification constraints differ");
        check(checked.num_gates() == prior.num_gates(), "verification gate count");
        check(checked.num_committed() == prior.num_committed(), "verification commitment count");
        check(checked.constraint_terms() == prior.constraint_terms(), "verification term count");
        Transcript signing("builder", rule);
        const auto proof = prover.prove(signing);
        ConstraintSystem system;
        system.num_gates = checked.num_gates();
        system.num_committed = checked.num_committed();
        const auto copied = allocation_size([&] { system.constraints = prior.get_constraints(); });
        check(copied > 0, "allocation probe inactive");
        const auto moved = allocation_size([&] { system.constraints = std::move(checked).get_constraints(); });
        check(moved == 0, "verification constraint copy");
        check(same_constraints(system.constraints, expected), "transferred constraints differ");
        Transcript verifying("builder", rule);
        check(r1cs_verify(verifying, system, proof), "verification builder rejects proof");
        auto altered = system;
        altered.constraints.back().lc += LinearCombination(Variable::one(), sc_from_u64(1));
        Transcript invalid("builder", rule);
        check(!r1cs_verify(invalid, altered, proof), "verification ignores constraint");
        auto measured = R1CSProver::verification(rule);
        R1CSProver control(rule);
        const auto populate = [&](R1CSProver& builder) {
            for (size_t i = 0; i < 4096; ++i) {
                const auto value = sc_from_u64(i);
                const auto committed = builder.commit(value, value);
                const auto [left, right, out] = builder.allocate(value, value);
                check(committed.index == i && left.index == i && right.index == i && out.index == i,
                    "verification variable index");
            }
        };
        const auto retained = allocation_size([&] { populate(control); });
        const auto skipped = allocation_size([&] { populate(measured); });
        check(retained > 0, "witness allocation probe inactive");
        check(skipped == 0, "verification witness allocation");
        check(measured.num_gates() == 4096 && measured.num_committed() == 4096,
            "verification totals differ");
        bool refused = false;
        try {
            Transcript missing("builder", rule);
            checked.prove(missing);
        } catch (const std::runtime_error& error) {
            refused = std::string(error.what()) == "pvac: proof witness is missing";
        }
        check(refused, "verification creates proof");
        std::cout << "event = r1cs_storage copied = " << copied << " moved = " << moved
            << " witness = " << retained << " verification = " << skipped << "\n";
    }
}

std::string system_digest(const std::vector<pvac::bp::Constraint>& constraints) {
    pvac::Sha256 hash;
    hash.init();
    pvac_ser::Writer writer;
    writer.u64(constraints.size());
    hash.update(writer.buf.data(), writer.buf.size());
    for (const auto& constraint : constraints) {
        writer.buf.clear();
        writer.u64(constraint.lc.terms.size());
        for (const auto& term : constraint.lc.terms) {
            writer.u8(static_cast<uint8_t>(term.first.type));
            writer.u64(term.first.index);
            for (uint64_t word : term.second.v) writer.u64(word);
        }
        hash.update(writer.buf.data(), writer.buf.size());
    }
    uint8_t digest[32];
    hash.finish(digest);
    return hex(digest, sizeof(digest));
}

void circuit_cases() {
    using namespace pvac;
    Params params;
    PubKey key;
    SecKey secret;
    uint8_t seed[32] = {37};
    keygen_from_seed(params, key, secret, seed);
    const auto first = enc_value_seeded(key, secret, 0, seed);
    seed[0] = 38;
    const auto second = enc_value_seeded(key, secret, 0, seed);
    const auto product = ct_mul_seeded(key, first, second, seed);
    for (const auto& cipher : {first, product}) {
        const auto bases = base_layer_indices(cipher);
        const auto coefficients = compute_layer_coeffs(key, cipher);
        for (const auto rule : {ScalarRule::Prior, ScalarRule::Wide}) {
            for (int kind : {0, 1, 2}) {
                std::string expected;
                size_t gates = 0;
                size_t committed = 0;
                size_t terms = 0;
                size_t prior_bytes = 0;
                double prior_ms = 0;
                for (bool fast : {false, true}) {
                    auto builder = fast ? bp::R1CSProver::verification(rule) : bp::R1CSProver(rule);
                    detail::AmountBinding amount;
                    amount.range_bits = kind == 2 ? 64 : 0;
                    const auto start = std::chrono::steady_clock::now();
                    const auto bytes = allocation_size([&] {
                        detail::build_key_bound_circuit(builder, key, nullptr, cipher,
                            coefficients, bases, nullptr, nullptr, kind == 0 ? nullptr : &amount);
                    });
                    const auto ms = std::chrono::duration<double, std::milli>(
                        std::chrono::steady_clock::now() - start).count();
                    if (!fast) {
                        gates = builder.num_gates();
                        committed = builder.num_committed();
                        terms = builder.constraint_terms();
                        prior_bytes = bytes;
                        prior_ms = ms;
                        expected = system_digest(std::move(builder).get_constraints());
                    } else {
                        check(builder.num_gates() == gates, "circuit gate count");
                        check(builder.num_committed() == committed, "circuit commitment count");
                        check(builder.constraint_terms() == terms, "circuit term count");
                        check(system_digest(std::move(builder).get_constraints()) == expected,
                            "circuit constraints differ");
                        check(bytes < prior_bytes, "circuit witness retained");
                        std::cout << "event = r1cs_build layers = " << cipher.L.size()
                            << " math = " << (rule == ScalarRule::Wide)
                            << " kind = " << kind << " gates = " << gates << " terms = " << terms
                            << " prior_bytes = " << prior_bytes << " next_bytes = " << bytes
                            << " prior_ms = " << prior_ms << " next_ms = " << ms << "\n";
                    }
                }
            }
        }
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
        if (mode == "peak-write" || mode == "peak-read") {
            check(argc == 3, "proof file missing");
            proof_peak_cases(mode, argv[2]);
            return 0;
        }
        if (mode == "all") check(vector_digest() == "ef53594fe19cae770e67aea7ac2f7ead7b881e968843c40fd881f9441f440839", "prior vector");
        if (mode == "all" || mode == "shake") shake_cases();
        if (mode == "all" || mode == "sample") sample_cases();
        if (mode == "all" || mode == "reader") reader_cases();
        if (mode == "all" || mode == "merge") merge_cases();
        if (mode == "all" || mode == "product") product_case(false);
        if (mode == "all" || mode == "table") table_cases();
        if (mode == "all" || mode == "scalar") scalar_cases();
        if (mode == "all" || mode == "proof") proof_math_cases();
        if (mode == "all" || mode == "peak") proof_peak_cases(mode, "");
        if (mode == "all" || mode == "build") build_cases();
        if (mode == "all" || mode == "circuit") circuit_cases();
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