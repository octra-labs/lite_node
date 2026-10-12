// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#include "pvac_serialize.hpp"
#include <array>
#include <iostream>
#include <string>
#include <vector>

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