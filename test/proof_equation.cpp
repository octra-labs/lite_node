// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include <fstream>
#include <iostream>
#include <stdexcept>
#include <atomic>
#include <iomanip>
#include <sstream>
#include "pvac/crypto/bulletproofs/r1cs.hpp"

using namespace pvac;
using namespace pvac::bp;

void check(bool value, const char* reason) {
    if (!value) throw std::runtime_error(reason);
}

void generator_cases() {
    for (const size_t count : {0, 1, 4095, 4096, 8193, 16389}) {
        std::vector<std::atomic<unsigned>> seen(count);
        for (auto& value : seen) value = 0;
        generator_run(17, 17 + count, [&](size_t first, size_t last) {
            check(first >= 17 && last <= 17 + count && first < last,
                "generator partition exceeds interval");
            for (size_t i = first; i < last; ++i) ++seen.at(i - 17);
        });
        for (const auto& value : seen)
            check(value == 1, "generator partition differs");
    }
    for (const unsigned failure : {0, 1}) {
        std::vector<std::atomic<unsigned>> seen(16389);
        for (auto& value : seen) value = 0;
        unsigned attempts = 0;
        bool escaped = false;
        try {
            generator_run(17, 16406, [&](size_t first, size_t last) {
                for (size_t i = first; i < last; ++i) ++seen.at(i - 17);
            }, [&](auto task) {
                if (attempts++ == failure)
                    throw std::system_error(std::make_error_code(std::errc::resource_unavailable_try_again));
                return std::thread(std::move(task));
            });
        } catch (const std::system_error&) { escaped = true; }
        check(!escaped, "generator launch failure escaped");
        for (const auto& value : seen)
            check(value == 1, "generator launch recovery differs");
        check(attempts == (std::thread::hardware_concurrency() > 1 ? failure + 1 : 0),
            "generator launch control not reached");
    }
    std::atomic<size_t> finished{0};
    bool refused = false;
    try {
        generator_run(17, 16406, [&](size_t first, size_t last) {
            finished += last - first;
            if (first == 17) throw std::runtime_error("generator task");
        });
    } catch (const std::runtime_error& error) {
        refused = std::string(error.what()) == "generator task";
    }
    check(refused && finished == 16389, "generator task not joined");
    GeneratorTable table;
    for (const size_t count : {0, 1, 4097, 16389}) {
        table.precompute(count);
        for (size_t i = 0; i < count; ++i) {
            check(table.G(i) == hash_to_ristretto_point("pvac.bp.gen.G", i),
                "generator first domain differs");
            check(table.H(i) == hash_to_ristretto_point("pvac.bp.gen.H", i),
                "generator second domain differs");
        }
    }
    refused = false;
    try { table.precompute(BP_MAX_VECTOR_SIZE + 1); }
    catch (const std::runtime_error&) { refused = true; }
    check(refused && table.G(0) == hash_to_ristretto_point("pvac.bp.gen.G", 0),
        "generator rejected size changed prefix");
    table.precompute(262144);
    Sha256 digest;
    digest.init();
    for (size_t i = 0; i < 262144; ++i)
        for (const auto& point : {table.G(i), table.H(i)})
            digest.update(point.data(), point.size());
    uint8_t bytes[32];
    digest.finish(bytes);
    std::ostringstream text;
    for (const auto byte : bytes)
        text << std::hex << std::setfill('0') << std::setw(2) << unsigned(byte);
    check(text.str() == "b677fa457399228b03064817e1f8d25c0136b0755f83395f2b0f0c5dad2c1898",
        "generator saved digest differs");
    std::cout << "event = generators status = passed points = 524288\n";
}

void msm_cases() {
    size_t checked = 0;
    const auto compare = [&](const std::array<uint8_t, 32>& bytes) {
        for (size_t width = 1; width <= 16; ++width) {
            for (size_t bit = 0; bit < 256; ++bit) {
                size_t expected = 0;
                for (size_t index = 0; index < width && bit + index < 256; ++index)
                    expected |= size_t{(bytes[(bit + index) / 8] >> ((bit + index) % 8)) & 1U} << index;
                check(msm_digit(bytes, bit, width) == expected, "scalar window differs");
                ++checked;
            }
        }
    };
    std::array<uint8_t, 32> bytes{};
    compare(bytes);
    bytes.fill(255);
    compare(bytes);
    for (size_t bit = 0; bit < 256; ++bit) {
        bytes.fill(0);
        bytes[bit / 8] = uint8_t{1} << (bit % 8);
        compare(bytes);
        for (auto& byte : bytes) byte ^= 255;
        compare(bytes);
    }
    for (const auto [bit, width] : {std::pair<size_t, size_t>{256, 1}, {0, 0}, {0, 17}}) {
        bool refused = false;
        try { (void)msm_digit(bytes, bit, width); }
        catch (const std::runtime_error&) { refused = true; }
        check(refused, "invalid scalar window accepted");
    }
    for (size_t count : {0, 1, 2, 3, 7, 8, 31, 32, 127, 128, 1025}) {
        std::vector<Scalar> scalars;
        std::vector<RistrettoPoint> points;
        auto expected = ext_identity();
        for (size_t index = 0; index < count; ++index) {
            const auto point = hash_to_ristretto_point("msm.check", index);
            const Scalar scalar{{index + 1, UINT64_MAX - index,
                uint64_t{1} << (index % 64), UINT64_MAX}};
            scalars.push_back(scalar);
            points.push_back(point);
            expected = ext_add(expected, ext_scalarmul(rist_decode_or_throw(point), scalar));
        }
        check(multi_scalar_mul(scalars, points) == rist_encode(expected), "point sum differs");
        ++checked;
        if (count == 0) continue;
        auto invalid = points;
        invalid.back().fill(255);
        scalars.back() = sc_zero();
        bool refused = false;
        try { (void)multi_scalar_mul(scalars, invalid); }
        catch (const std::runtime_error& error) {
            refused = std::string(error.what()) == "pvac: invalid Ristretto point encoding";
        }
        check(refused, "invalid point with zero scalar accepted");
    }
    bool refused = false;
    try { (void)multi_scalar_mul({sc_zero()}, {}); }
    catch (const std::runtime_error&) { refused = true; }
    check(refused, "point count mismatch accepted");
    const auto point = hash_to_ristretto_point("msm.check", 0);
    for (size_t count : {8192, 16384, 32768, 65536, 70000}) {
        std::vector<Scalar> scalars;
        auto sum = sc_zero();
        for (size_t index = 0; index < count; ++index) {
            const Scalar small{{index + 1, 0, 0, 0}};
            const auto scalar = index % 3 == 0 ? sc_neg(small) : small;
            scalars.push_back(scalar);
            sum = sc_add(sum, scalar);
        }
        const auto expected = rist_encode(ext_scalarmul(rist_decode_or_throw(point), sum));
        check(multi_scalar_mul(scalars, std::vector<RistrettoPoint>(count, point)) == expected,
            "large point sum differs");
        ++checked;
    }
    const std::vector<Scalar> ones(8, Scalar{{1, 0, 0, 0}});
    const std::vector<RistrettoPoint> identities(8, rist_identity());
    check(multi_scalar_mul(ones, identities) == rist_identity(), "identity points differ");
    check(multi_scalar_mul(std::vector<Scalar>(8, sc_zero()),
        std::vector<RistrettoPoint>(8, point)) == rist_identity(), "zero scalars differ");
    auto single = identities;
    single.back() = point;
    check(multi_scalar_mul(ones, single) == point, "mixed identity points differ");
    std::cout << "event = point_identity status = passed checks = 3\n";
    std::cout << "event = point_sum status = passed checks = " << checked << '\n';
}

struct ProofReader {
    const std::string& text;
    size_t offset = 0;

    uint8_t byte() {
        check(offset + 2 <= text.size(), "record ended");
        const auto value = std::stoul(text.substr(offset, 2), nullptr, 16);
        offset += 2;
        return static_cast<uint8_t>(value);
    }

    size_t count() {
        uint64_t value = 0;
        for (size_t index = 0; index < 8; ++index)
            value |= uint64_t{byte()} << (8 * index);
        check(value <= BP_MAX_VECTOR_SIZE, "record count exceeds limit");
        return static_cast<size_t>(value);
    }

    RistrettoPoint point() {
        RistrettoPoint result;
        for (auto& value : result) value = byte();
        return result;
    }

    Scalar scalar() {
        Scalar value{};
        for (size_t index = 0; index < 32; ++index)
            value.v[index / 8] |= uint64_t{byte()} << (8 * (index % 8));
        return value;
    }

    std::vector<RistrettoPoint> points() {
        std::vector<RistrettoPoint> result(count());
        for (auto& value : result) value = point();
        return result;
    }

    R1CSProof proof() {
        R1CSProof value;
        value.V = points();
        value.A_I1 = point(); value.A_O1 = point(); value.S1 = point();
        value.T_1 = point(); value.T_3 = point(); value.T_4 = point();
        value.T_5 = point(); value.T_6 = point();
        value.t_x = scalar(); value.t_x_blinding = scalar(); value.e_blinding = scalar();
        value.ipp.L = points(); value.ipp.R = points();
        value.ipp.a = scalar(); value.ipp.b = scalar();
        check(offset == text.size(), "record has extra bytes");
        return value;
    }
};

void cases(const R1CSProof& proof, size_t index) {
    const auto rule = index < 6 ? ScalarRule::Prior : ScalarRule::Wide;
    const size_t sizes[] = {1, 2, 7, 32, 128, 1025};
    const size_t count = sizes[index % 6];
    R1CSProver builder = R1CSProver::verification(rule);
    for (size_t gate = 0; gate < count; ++gate) {
        const auto [left, right, out] = builder.allocate(Scalar{{3, 0, 0, 0}}, Scalar{{4, 0, 0, 0}});
        builder.constrain(LinearCombination(out) - LinearCombination(Variable::one(), Scalar{{12, 0, 0, 0}}));
    }
    ConstraintSystem system;
    system.num_gates = builder.num_gates();
    system.num_committed = builder.num_committed();
    system.constraints = builder.get_constraints();
    const auto verify = [&](const R1CSProof& value, const ConstraintSystem& shape) {
        Transcript transcript("prover.sum", rule);
        return r1cs_verify(transcript, shape, value);
    };
    size_t padded = 0;
    check(system.padded_gates_checked(padded), "saved circuit size refused");
    const bool ready = bool(ready_table().get(padded));
    const auto rejected = ready_table().rejected();
    check(verify(proof, system), "saved proof refused");
    check(ready_table().rejected() == rejected, "valid proof recorded as refused");
    size_t refusals = 0;
    const auto refuse = [&](const R1CSProof& value) {
        check(!verify(value, system), "changed proof accepted");
        ++refusals;
    };
    RistrettoPoint invalid;
    invalid.fill(255);
    for (size_t field = 0; field < 8 + proof.ipp.L.size() * 2; ++field) {
        for (bool malformed : {false, true}) {
            auto value = proof;
            std::vector<RistrettoPoint*> points{&value.A_I1, &value.A_O1, &value.S1,
                &value.T_1, &value.T_3, &value.T_4, &value.T_5, &value.T_6};
            for (auto& point : value.ipp.L) points.push_back(&point);
            for (auto& point : value.ipp.R) points.push_back(&point);
            *points[field] = malformed ? invalid : hash_to_ristretto_point("proof.changed", field);
            const auto rejected = ready_table().rejected();
            refuse(value);
            check(!malformed || ready_table().rejected() == rejected,
                "malformed point recorded as completed");
        }
    }
    for (size_t field = 0; field < 5; ++field) {
        auto value = proof;
        const std::array<Scalar*, 5> scalars{&value.t_x, &value.t_x_blinding,
            &value.e_blinding, &value.ipp.a, &value.ipp.b};
        *scalars[field] = sc_add(*scalars[field], Scalar{{1, 0, 0, 0}});
        const auto rejected = ready_table().rejected();
        const auto sums = ready_table().sums();
        refuse(value);
        check(ready_table().rejected() == rejected + (ready && field >= 2 ? 1 : 0),
            "scalar refusal stage differs");
        check(ready_table().sums() == sums + (ready && field >= 2 ? 1 : 0),
            "scalar equation count differs");
    }
    auto value = proof;
    value.ipp.L.push_back(rist_identity());
    refuse(value);
    value = proof;
    value.V.push_back(rist_identity());
    refuse(value);
    auto changed = system;
    changed.constraints[0].lc.terms.push_back({Variable::one(), Scalar{{1, 0, 0, 0}}});
    check(!verify(proof, changed), "changed equation accepted");
    std::cout << "event = equation index = " << index << " accepted = 1 refusals = " << refusals + 1 << '\n';
}

#ifdef EQUATION_TERMS
void terms_cases() {
    size_t checked = 0;
    const Scalar one{{1, 0, 0, 0}};
    for (auto rule : {ScalarRule::Prior, ScalarRule::Wide}) {
        for (size_t count : {1, 2, 8, 32}) {
            InnerProductProof proof;
            proof.a = Scalar{{19, 0, 0, 0}};
            proof.b = Scalar{{23, 0, 0, 0}};
            const auto size = log2_size(count);
            std::vector<Scalar> challenges, powers(count, one), g(count), h(count);
            Transcript reference("equation.terms", rule);
            for (size_t index = 0; index < size; ++index) {
                proof.L.push_back(hash_to_ristretto_point("equation.left", index));
                proof.R.push_back(hash_to_ristretto_point("equation.right", index));
                reference.append_point("L", proof.L.back());
                reference.append_point("R", proof.R.back());
                challenges.push_back(reference.challenge_scalar("u"));
            }
            const auto weights = ipp_verification_scalars(challenges, reference.ops);
            const auto inverse = batch_sc_inv(weights, reference.ops);
            std::vector<Scalar> terms, shift;
            std::vector<RistrettoPoint> points, basis;
            for (size_t index = 0; index < count; ++index) {
                g[index] = index % 2 == 0 ? sc_neg(Scalar{{index + 7, 0, 0, 0}}) : sc_zero();
                h[index] = Scalar{{index + 11, 0, 0, 0}};
                powers[index] = Scalar{{index + 3, 0, 0, 0}};
                terms.push_back(reference.ops.mul(proof.a, weights[index]));
                terms.push_back(reference.ops.mul(reference.ops.mul(proof.b, inverse[index]), powers[index]));
                points.push_back(generators().G(index)); points.push_back(generators().H(index));
                shift.push_back(g[index]); shift.push_back(h[index]);
                basis.push_back(points[2 * index]); basis.push_back(points[2 * index + 1]);
            }
            const auto q = pedersen_B();
            terms.push_back(reference.ops.mul(proof.a, proof.b)); points.push_back(q);
            std::vector<Scalar> squared;
            for (const auto& challenge : challenges) squared.push_back(reference.ops.mul(challenge, challenge));
            const auto inverted = batch_sc_inv(squared, reference.ops);
            for (size_t index = 0; index < size; ++index) {
                terms.push_back(sc_neg(squared[index])); points.push_back(proof.L[index]);
                terms.push_back(sc_neg(inverted[index])); points.push_back(proof.R[index]);
            }
            const auto expected = multi_scalar_mul(terms, points);
            const auto small = rist_sub(expected, multi_scalar_mul(shift, basis));
            const auto verify = [&](const RistrettoPoint& p, const auto& gs, const auto& hs) {
                Transcript transcript("equation.terms", rule);
                return ipp_verify_with_y(transcript, p, q, proof, count, powers, gs, hs);
            };
            check(verify(expected, std::vector<Scalar>{}, std::vector<Scalar>{}), "original point equation differs");
            check(verify(small, g, h), "shifted point equation differs");
            auto gs = g;
            gs[0] = sc_add(gs[0], one);
            check(!verify(small, gs, h), "changed generator coefficient accepted");
            auto hs = h;
            hs.back() = sc_add(hs.back(), one);
            check(!verify(small, g, hs), "changed second coefficient accepted");
            gs = g;
            gs.push_back(one);
            check(!verify(small, gs, h), "generator count accepted");
            hs = h;
            hs.push_back(one);
            check(!verify(small, g, hs), "second count accepted");
            auto invalid = expected;
            invalid.fill(255);
            check(!verify(invalid, g, h), "invalid equation point accepted");
            checked += 7;
        }
    }
    std::cout << "event = terms status = passed checks = " << checked << '\n';
}
#endif

void records(const char* path) {
    std::ifstream input(path);
    check(input.good(), "saved records missing");
    for (size_t index = 0; index < 12; ++index) {
        std::string text;
        check(bool(input >> text), "saved record missing");
        const auto proof = ProofReader{text}.proof();
        cases(proof, index);
    }
    std::string extra;
    check(!(input >> extra), "extra record present");
}

void ready_size_cases() {
    ReadyTable table;
    check(!table.get(1) && table.size() == 0, "ready table exists before build");
    for (const size_t n : {size_t{0}, BP_MAX_VECTOR_SIZE + 1}) {
        bool refused = false;
        try { table.build(n); }
        catch (const std::runtime_error& error) {
            refused = std::string(error.what()) == "pvac: ready size rejected";
        }
        check(refused && table.size() == 0, "invalid ready size published");
    }
    std::atomic<size_t> built{0}, refused{0}, broken{0};
    std::atomic<bool> start{false};
    std::vector<std::thread> builders;
    for (size_t i = 0; i < 4; ++i) builders.emplace_back([&] {
        while (!start.load()) std::this_thread::yield();
        try { table.build(16); ++built; }
        catch (const std::runtime_error& error) {
            if (std::string(error.what()) == "pvac: ready table already exists") ++refused;
            else ++broken;
        }
    });
    start = true;
    while (built.load() + refused.load() + broken.load() < 4) {
        const auto points = table.get(16);
        if (points && points->size() != 16) ++broken;
        std::this_thread::yield();
    }
    for (auto& thread : builders) thread.join();
    check(built == 1 && refused == 3 && broken == 0, "ready build ownership differs");
    const auto points = table.get(16);
    check(points && !table.get(17) && !table.get(0), "ready size exceeded");
    check(table.get(1) == points, "ready prefix replaced snapshot");
    check(table.sums() == 0, "ready build counted as sum");
    std::cout << "event = ready_size status = passed builders = 4\n";
}

void ready_sum_cases(const ReadyPoints& ready) {
    for (const size_t count : {1, 2, 3, 7, 1025, 32767, 32768, 65535, 65536, 70000, 524325}) {
        size_t expected = 1;
        while (expected < 16 && (size_t{1} << expected) <= count) ++expected;
        check(ready_width(count) == expected, "ready width differs");
    }
    const auto start = ready_table().sums();
    const std::array<size_t, 5> counts{3, 7, 1025, 70000, 2 * ready.size() + 1};
    for (const size_t count : counts) {
        const size_t n = std::min(ready.size(), (count - 1) / 2);
        std::vector<Scalar> scalars;
        std::vector<RistrettoPoint> points, rest;
        for (size_t i = 0; i < count; ++i) {
            scalars.push_back(i % 7 == 0 ? sc_zero()
                : Scalar{{i + 1, UINT64_MAX - i, uint64_t{1} << (i % 64), UINT64_MAX}});
            points.push_back(i < n ? generators().G(i)
                : i < 2 * n ? generators().H(i - n) : pedersen_B());
            if (i >= 2 * n) rest.push_back(points.back());
        }
        check(ready_sum(ready, scalars, n, rest) == multi_scalar_mul(scalars, points),
            "ready point sum differs");
    }
    check(ready_table().sums() == start + counts.size(), "ready sum unused");
    const auto reject = [&](const std::vector<Scalar>& scalars, size_t n,
        const std::vector<RistrettoPoint>& rest, const char* reason) {
        bool refused = false;
        const auto before = ready_table().sums();
        try { (void)ready_sum(ready, scalars, n, rest); }
        catch (const std::runtime_error& error) { refused = std::string(error.what()) == reason; }
        check(refused, "invalid ready sum accepted");
        check(ready_table().sums() == before, "failed ready sum counted");
    };
    const char* count_error = "pvac: ready point count rejected";
    reject({sc_zero()}, 0, {pedersen_B()}, count_error);
    reject({sc_zero(), sc_zero()}, 1, {}, count_error);
    reject({sc_zero(), sc_zero(), sc_zero()}, ready.size() + 1, {pedersen_B()}, count_error);
    reject({sc_zero(), sc_zero(), sc_zero()}, 1, {}, count_error);
    reject({}, 1, {pedersen_B()}, count_error);
    RistrettoPoint invalid;
    invalid.fill(255);
    reject({sc_zero(), sc_zero(), sc_zero()}, 1, {invalid},
        "pvac: invalid Ristretto point encoding");
    std::cout << "event = ready_sum status = passed sums = " << counts.size()
        << " size = " << ready.size() << " refusals = 6\n";
}

void ready_digest_cases(const ReadyPoints& ready) {
    Sha256 digest;
    digest.init();
    constexpr char domain[] = "pvac.ready.points";
    digest.update(reinterpret_cast<const uint8_t*>(domain), sizeof(domain) - 1);
    uint8_t size[8];
    for (size_t i = 0; i < 8; ++i) size[i] = uint8_t(uint64_t{ready.size()} >> (8 * i));
    digest.update(size, sizeof(size));
    for (bool second : {false, true}) {
        for (size_t i = 0; i < ready.size(); ++i) {
            const auto decoded = rist_decode_or_throw(
                hash_to_ristretto_point(second ? "pvac.bp.gen.H" : "pvac.bp.gen.G", i));
            const auto& actual = second ? ready.H(i) : ready.G(i);
            const std::array<Fe25519, 3> expected{fe_sub(decoded.Y, decoded.X),
                fe_add(decoded.Y, decoded.X), fe_mul(decoded.T, ed_2d())};
            const std::array<Fe25519, 3> fields{actual.minus, actual.plus, actual.product};
            for (size_t k = 0; k < fields.size(); ++k) {
                uint8_t bytes[32], other[32];
                fe_tobytes(bytes, expected[k]);
                fe_tobytes(other, fields[k]);
                check(std::equal(bytes, bytes + 32, other), "ready point differs");
                digest.update(bytes, sizeof(bytes));
            }
        }
    }
    std::array<uint8_t, 32> expected;
    digest.finish(expected.data());
    check(ready_digest(ready) == expected, "ready digest differs");
    std::ostringstream text;
    for (const auto byte : expected)
        text << std::hex << std::setfill('0') << std::setw(2) << unsigned(byte);
    std::cout << "event = ready_digest points = " << 2 * ready.size()
        << " sha256 = " << text.str() << '\n';
}

int main(int argc, char** argv) {
    try {
        check(argc == 2 || argc == 3, "saved records required");
        generator_cases();
        msm_cases();
        records(argv[1]);
#ifdef EQUATION_TERMS
        terms_cases();
#endif
        if (argc == 3) {
            const std::string mode(argv[2]);
            check(mode == "ready" || mode == "ready-part" || mode == "ready-full"
                || mode == "ready-large", "ready mode refused");
            ready_size_cases();
            const size_t size = mode == "ready-part" ? 256 : mode == "ready-full" ? 262144
                : mode == "ready-large" ? ready_capacity : 2048;
            ready_table().build(size);
            const auto ready = ready_table().get(size);
            check(ready && !ready_table().get(size + 1), "ready size exceeded");
            bool refused = false;
            try { ready_table().build(size); }
            catch (const std::runtime_error& error) {
                refused = std::string(error.what()) == "pvac: ready table already exists";
            }
            check(refused && ready_table().get(size) == ready, "ready snapshot replaced");
            ready_digest_cases(*ready);
            check(ready_check() == (size == ready_capacity), "ready image differs");
            ready_sum_cases(*ready);
            const auto before = ready_table().sums();
            records(argv[1]);
            check(ready_table().sums() >= before + (size >= 2048 ? 12 : 10), "ready equations unused");
            if (size < 2048) {
                std::ifstream input(argv[1]);
                std::string text;
                for (size_t i = 0; i < 6; ++i) check(bool(input >> text), "saved record missing");
                const auto uses = ready_table().sums();
                cases(ProofReader{text}.proof(), 5);
                check(ready_table().sums() == uses, "ready short snapshot used");
            }
#ifdef EQUATION_TERMS
            terms_cases();
#endif
            std::cout << "event = ready_equations status = passed generated = 0\n";
        }
        std::cout << "event = proof_equation status = passed generated = 0\n";
        return 0;
    } catch (const std::exception& error) {
        std::cerr << "event = failure reason = " << error.what() << '\n';
        return 1;
    }
}