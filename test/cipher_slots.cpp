// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include "pvac_serialize.hpp"
#include "pvac/crypto/bulletproofs/ready_points.hpp"
#include <array>
#include <iostream>
#include <string>
#include <vector>
#include <numeric>
#include <stdexcept>
#include <cstdio>
#include <fcntl.h>
#include <unistd.h>

static int circuit_sizes() {
    using Profile = pvac::CircuitPrfProfile;
    using Limit = pvac::bp::R1CSLimitProfile;
    struct Case {
        const char* name;
        Profile profile;
        size_t bases;
        size_t layers;
        size_t range_bits;
        bool aliases;
        Limit limit;
        size_t gates;
        size_t constraints;
        size_t padded;
        bool allowed;
    };
    std::vector<Case> cases;
    for (size_t layers = 2; layers <= 8; ++layers)
        cases.push_back({"circle", Profile::MIMC_X5_V7, 2, layers, 64, false, Limit::Default,
            254529 + 1513 * (layers - 2), 507225 + 3017 * (layers - 2),
            layers < 8 ? size_t{262144} : size_t{524288}, true});
    for (const auto& row : std::vector<Case>{
        {"x3_range", Profile::MIMC_X3_V6, 8, 16, 64, false, Limit::Default, 689310, 1373403, 1048576, true},
        {"x3_history", Profile::MIMC_X3_V6, 8, 16, 0, true, Limit::KeySwitchRefresh, 693735, 1382252, 1048576, true},
        {"x5_range", Profile::MIMC_X5_V7, 5, 16, 64, false, Limit::Default, 652559, 1300438, 1048576, true},
        {"x5_switch", Profile::MIMC_X5_V7, 6, 15, 0, false, Limit::KeySwitchRefresh, 776598, 1547617, 1048576, true},
        {"x5_switch_limit", Profile::MIMC_X5_V7, 6, 16, 0, false, Limit::KeySwitchRefresh, 778111, 1550634, 1048576, false},
        {"x5_prepared", Profile::MIMC_X5_V7, 4, 14, 64, false, Limit::Default, 523917, 1044079, 524288, true},
        {"x5_unprepared", Profile::MIMC_X5_V7, 4, 15, 64, false, Limit::Default, 525430, 1047096, 1048576, true}})
        cases.push_back(row);
    pvac::PubKey key;
    pvac::SecKey secret;
    std::array<uint8_t, 32> seed{};
    seed.fill(63);
    pvac::keygen_from_seed(pvac::Params{}, key, secret, seed.data());
    for (const auto& row : cases) {
        pvac::set_circuit_prf_profile(key, secret, row.profile);
        seed.fill(63);
        auto cipher = pvac::enc_value_seeded(key, secret, 3, seed.data());
        while (cipher.L.size() < row.bases) {
            seed[0] = static_cast<uint8_t>(cipher.L.size());
            const auto extra = pvac::enc_value_seeded(key, secret, 0, seed.data());
            cipher.L.push_back(extra.L.front());
        }
        while (cipher.L.size() < row.layers) {
            const uint32_t parent = static_cast<uint32_t>(cipher.L.size() - 1);
            cipher.L.push_back(pvac::detail::make_prod_layer(key, 0, parent,
                cipher.L.front().R_com, cipher.L.back().R_com));
        }
        const auto bytes = pvac_ser::serialize_cipher(cipher);
        pvac::Cipher restored;
        std::string error;
        if (!pvac_ser::deserialize_cipher_checked(bytes.data(), bytes.size(), restored, error, true, true))
            throw std::runtime_error(error);
        const auto amount = pvac::dec_value(key, secret, restored);
        if (restored.slots != 1 || restored.c0.size() != 1 || restored.L.size() != row.layers
                || pvac::base_layer_indices(restored).size() != row.bases || restored.E.size() > 512
                || amount.hi != 0 || amount.lo != 3)
            throw std::runtime_error("cipher shape differs");
        const bool shape = row.limit == Limit::KeySwitchRefresh
            ? pvac::key_bound_key_switch_shape_ok(key, restored)
            : pvac::key_bound_verify_shape_ok(key, restored);
        if (!shape) throw std::runtime_error("native cipher shape rejected");
        auto builder = pvac::bp::R1CSProver::verification(pvac::ScalarRule::Wide);
        pvac::detail::AmountBinding value;
        value.range_bits = row.range_bits;
        pvac::detail::build_key_bound_circuit(builder, key, nullptr, restored,
            pvac::compute_layer_coeffs(key, restored), pvac::base_layer_indices(restored),
            nullptr, nullptr, &value, row.aliases);
        pvac::bp::ConstraintSystem system;
        system.num_gates = builder.num_gates();
        system.num_committed = builder.num_committed();
        system.constraints = std::move(builder).get_constraints();
        const auto count = system.padded_gates();
        const auto terms = std::accumulate(system.constraints.begin(), system.constraints.end(),
            size_t{0}, [](size_t sum, const auto& row) { return sum + row.lc.terms.size(); });
        if (system.num_gates != row.gates || system.constraints.size() != row.constraints
                || count != row.padded || system.num_committed != 2 + 2 * row.bases
                || terms > pvac::bp::R1CS_MAX_TERMS)
            throw std::runtime_error("cipher circuit cost differs");
        const bool allowed = pvac::bp::r1cs_system_ok(system, count, row.limit);
        if (allowed != row.allowed)
            throw std::runtime_error("cipher circuit limit differs");
        if (allowed && count > pvac::bp::ready_capacity)
            throw std::runtime_error("allowed cipher exceeds prepared capacity");
        std::cout << "event = cipher_circuit case = " << row.name << " bases = " << row.bases
            << " layers = " << row.layers << " gates = " << system.num_gates
            << " padded = " << count << " constraints = " << system.constraints.size()
            << " terms = " << terms << " prepared_capacity = " << pvac::bp::ready_capacity
            << " prepared = " << (count <= pvac::bp::ready_capacity) << " allowed = " << row.allowed << "\n";
    }
    return 0;
}

static int emit_large(const char* path) {
    const int file = open(path, O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC, 0600);
    if (file < 0) throw std::runtime_error("proof output unavailable");
    std::unique_ptr<FILE, decltype(&std::fclose)> output(fdopen(file, "wb"), std::fclose);
    if (!output) {
        close(file);
        throw std::runtime_error("proof stream unavailable");
    }
    pvac::PubKey key;
    pvac::SecKey secret;
    std::array<uint8_t, 32> seed{};
    seed.fill(63);
    pvac::keygen_from_seed(pvac::Params{}, key, secret, seed.data());
    auto cipher = pvac::enc_value_seeded(key, secret, 3, seed.data());
    while (cipher.L.size() < 8) {
        cipher.L.push_back(pvac::detail::make_prod_layer(key, 0,
            static_cast<uint32_t>(cipher.L.size() - 1), cipher.L.front().R_com, cipher.L.back().R_com));
    }
    const auto blind = pvac::bp::sc_from_u64(19);
    const auto point = pvac::pedersen_commit(pvac::bp::sc_from_u64(3), blind);
    const auto proof = pvac::make_zero_proof_bound_range(key, secret, cipher, 3, blind,
        pvac::ScalarRule::Wide, true);
    if (!pvac::verify_zero_bound_range_checked(key, cipher, proof, point, true,
        pvac::ScalarRule::Wide, true)) throw std::runtime_error("large proof refused");
    const auto write = [&](const auto& bytes) {
        if (bytes.size() > UINT32_MAX) throw std::runtime_error("proof field too large");
        const uint32_t size = static_cast<uint32_t>(bytes.size());
        const uint8_t length[]{uint8_t(size >> 24), uint8_t(size >> 16), uint8_t(size >> 8), uint8_t(size)};
        if (std::fwrite(length, 1, 4, output.get()) != 4
                || std::fwrite(bytes.data(), 1, bytes.size(), output.get()) != bytes.size())
            throw std::runtime_error("proof output incomplete");
    };
    write(pvac_ser::serialize_pubkey(key));
    write(pvac_ser::serialize_cipher(cipher));
    write(pvac_ser::serialize_bound_range_proof(proof));
    write(point);
    write(pvac::commit_ct(key, cipher));
    if (std::fflush(output.get()) != 0 || fsync(file) != 0)
        throw std::runtime_error("proof output not stored");
    std::cout << "event = large_proof layers = 8 base_layers = 2 amount = 3 proofs_generated = 1\n";
    return 0;
}

static int history_images() {
    pvac::PubKey key;
    pvac::SecKey secret;
    std::array<uint8_t, 32> seed{};
    seed.fill(62);
    pvac::keygen_from_seed(pvac::Params{}, key, secret, seed.data());
    const auto cipher = pvac::enc_value_seeded(key, secret, 3, seed.data());
    for (const auto tag : {pvac_ser::VERSION_V1, pvac_ser::VERSION_V2}) {
        pvac_ser::Writer out;
        out.header_version(pvac_ser::TAG_CIPHER, tag);
        out.u64(cipher.slots);
        out.u64(cipher.L.size());
        for (const auto& layer : cipher.L) {
            pvac_ser::write_layer_public(out, layer);
            out.raw(layer.R_com.data(), layer.R_com.size());
            if (tag == pvac_ser::VERSION_V2) {
                out.u64(layer.PC.size());
                for (const auto& point : layer.PC) out.raw(point.data(), point.size());
            }
        }
        out.u64(cipher.c0.size());
        for (const auto& value : cipher.c0) out.fp(value);
        out.u64(cipher.E.size());
        for (const auto& edge : cipher.E) pvac_ser::write_edge(out, edge);
        static const char hex[] = "0123456789abcdef";
        for (const auto byte : out.buf) std::cout << hex[byte >> 4] << hex[byte & 15];
        std::cout << '\n';
    }
    return 0;
}

int main(int argc, char** argv) {
    if (argc == 3 && std::string(argv[1]) == "--emit-large") return emit_large(argv[2]);
    if (argc == 2 && std::string(argv[1]) == "--cost") return circuit_sizes();
    if (argc == 2 && std::string(argv[1]) == "--history") return history_images();
    if (argc != 1) return 1;
    constexpr std::array<uint64_t, 6> values = {0, 1, 8, 9, 65536, 65537};
    size_t cases = 0;
    for (uint8_t version = pvac_ser::VERSION_V1; version <= pvac_ser::VERSION_V5; ++version) {
        for (bool strict : {false, true}) {
            for (bool cap : {false, true}) {
                for (uint64_t value : values) {
                    pvac_ser::Writer writer;
                    writer.u64(value);
                    pvac_ser::Reader reader(writer.buf.data(), writer.buf.size());
                    size_t slots = 0;
                    const bool ok = pvac_ser::read_cipher_slots(
                        reader,
                        version,
                        slots,
                        strict,
                        cap);
                    const bool expected = (value > 0 || (!strict && !cap)) &&
                        (!cap || value <= 65536) &&
                        (!strict || version < pvac_ser::VERSION_V4 || value <= 8);
                    if (ok != expected || reader.failed == ok ||
                        (ok && (slots != value || reader.remaining() != 0))) {
                        std::cerr << "event = cipher_slots status = fail\n";
                        return 1;
                    }
                    ++cases;
                }
            }
        }
    }
    for (uint8_t version = pvac_ser::VERSION_V1; version <= pvac_ser::VERSION_V5; ++version) {
        for (bool strict : {false, true}) {
            for (bool cap : {false, true}) {
                pvac_ser::Writer writer;
                pvac_ser::Reader reader(writer.buf.data(), writer.buf.size());
                const bool ok = pvac_ser::check_public_cells(
                    reader,
                    version,
                    8,
                    513,
                    strict,
                    cap);
                const bool expected = version < pvac_ser::VERSION_V4 ||
                    (!strict && !cap);
                if (ok != expected || reader.failed == ok) {
                    std::cerr << "event = cipher_cells status = fail\n";
                    return 1;
                }
                ++cases;
            }
        }
    }
    struct ImageCase {
        uint8_t version;
        std::vector<uint64_t> words;
        bool strict;
        bool cap;
        const char* error;
    };
    const std::vector<ImageCase> images = {
        {pvac_ser::VERSION_V3, {0, 1}, false, false, "pvac_ser: count exceeds remaining data"},
        {pvac_ser::VERSION_V3, {0}, false, false, "pvac_ser: truncated"},
        {pvac_ser::VERSION_V3, {0, 1}, true, true, "pvac_ser: cipher slots must be positive"},
        {pvac_ser::VERSION_V3, {0, 1}, false, true, "pvac_ser: cipher slots must be positive"},
        {pvac_ser::VERSION_V3, {0, 0, 0, 0}, false, false, "pvac_ser: cipher slots must be positive"},
    };
    for (const auto& image : images) {
        pvac_ser::Writer writer;
        writer.header_version(pvac_ser::TAG_CIPHER, image.version);
        for (uint64_t word : image.words) writer.u64(word);
        pvac::Cipher cipher;
        std::string error;
        const bool ok = pvac_ser::deserialize_cipher_checked(
            writer.buf.data(), writer.buf.size(), cipher, error, image.strict, image.cap);
        if (ok || error != image.error) {
            std::cerr << "event = cipher_image status = fail\n";
            return 1;
        }
        ++cases;
    }
    {
        pvac_ser::Writer writer;
        writer.header_version(pvac_ser::TAG_CIPHER, pvac_ser::VERSION_V4);
        writer.u64(0);
        writer.u64(1);
        writer.u8(0);
        writer.u64(0);
        writer.u64(0);
        writer.u64(0);
        writer.u64(0);
        writer.u64(0);
        pvac::Cipher cipher;
        std::string error;
        const bool ok = pvac_ser::deserialize_cipher_checked(
            writer.buf.data(), writer.buf.size(), cipher, error, false, false);
        if (ok || error != "pvac_ser: cipher slots must be positive") {
            std::cerr << "event = cipher_image_public status = fail\n";
            return 1;
        }
        ++cases;
    }
    std::array<uint8_t, 32> tag{};
    std::array<uint8_t, 32> blind{};
    const auto opening = pvac::make_com_open(pvac::fp_from_u64(7), blind);
    const auto commitment = pvac::make_com_cell(tag, opening, true);
    auto altered = opening;
    altered.value.hi ^= uint64_t{1} << 63;
    if (pvac::decide_com_open(commitment, altered).admitted) {
        std::cerr << "event = commitment_open status = fail\n";
        return 1;
    }
    ++cases;
    bool rejected = false;
    try {
        static_cast<void>(pvac::make_com_open(altered.value, blind));
    } catch (const std::invalid_argument&) {
        rejected = true;
    }
    if (!rejected) {
        std::cerr << "event = commitment_value status = fail\n";
        return 1;
    }
    ++cases;
    std::cout << "event = cipher_slots status = pass cases = " << cases << '\n';
}