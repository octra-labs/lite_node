// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <cstring>
#include <vector>
#include "../../core/hash.hpp"
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

class Transcript {

    uint8_t state_[64];

    void mix(const char* tag, const uint8_t* data, size_t len, uint8_t op_byte) {
        Sha256 h;
        h.init();

        h.update(state_, 64);

        h.update(&op_byte, 1);

        uint32_t tag_len = (uint32_t)strlen(tag);
        h.update(reinterpret_cast<const uint8_t*>(&tag_len), 4);
        h.update(reinterpret_cast<const uint8_t*>(tag), tag_len);

        if (data && len > 0) {
            uint32_t dlen = (uint32_t)len;
            h.update(reinterpret_cast<const uint8_t*>(&dlen), 4);
            h.update(data, len);
        }

        h.finish(state_);

        Sha256 h2;
        h2.init();
        h2.update(state_, 32);
        uint8_t tweak = 0xFF;
        h2.update(&tweak, 1);
        h2.finish(state_ + 32);
    }

public:
    explicit Transcript(const char* domain_separator) {

        Sha256 h;
        h.init();
        h.update(reinterpret_cast<const uint8_t*>("pvac.bp.transcript"), 18);
        uint32_t ds_len = (uint32_t)strlen(domain_separator);
        h.update(reinterpret_cast<const uint8_t*>(&ds_len), 4);
        h.update(reinterpret_cast<const uint8_t*>(domain_separator), ds_len);
        h.finish(state_);

        Sha256 h2;
        h2.init();
        h2.update(state_, 32);
        uint8_t tweak = 0x01;
        h2.update(&tweak, 1);
        h2.finish(state_ + 32);
    }

    void append_message(const char* label, const uint8_t* data, size_t len) {
        mix(label, data, len, 'A');
    }

    void append_u64(const char* label, uint64_t v) {
        uint8_t buf[8];
        for (int i = 0; i < 8; i++) buf[i] = (uint8_t)(v >> (i * 8));
        append_message(label, buf, 8);
    }

    void append_point(const char* label, const RistrettoPoint& pt) {
        append_message(label, pt.data(), 32);
    }

    void append_scalar(const char* label, const Scalar& s) {
        uint8_t buf[32];
        sc_tobytes(buf, s);
        append_message(label, buf, 32);
    }

    Scalar challenge_scalar(const char* label) {
        mix(label, nullptr, 0, 'C');

        uint64_t w[8];
        for (int i = 0; i < 8; i++) {
            w[i] = 0;
            for (int j = 0; j < 8; j++)
                w[i] |= (uint64_t)state_[i * 8 + j] << (j * 8);
        }
        Scalar result = sc_reduce512(w);

        mix("post_challenge", nullptr, 0, 'A');
        return result;
    }
};

}
}