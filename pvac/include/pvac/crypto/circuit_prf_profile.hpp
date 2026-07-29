// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstddef>
#include <stdexcept>

#include "../core/types.hpp"

namespace pvac {

inline const Fp& circuit_prf_key_for_profile(
    const SecKey& sk,
    CircuitPrfProfile profile
) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return sk.circuit_prf_key_v6;
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return sk.circuit_prf_key;
    throw std::runtime_error("pvac: circuit prf profile rejected");
}

inline const std::array<uint8_t, 32>& circuit_prf_blind_for_profile(
    const SecKey& sk,
    CircuitPrfProfile profile
) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return sk.circuit_prf_key_blind_v6;
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return sk.circuit_prf_key_blind;
    throw std::runtime_error("pvac: circuit prf profile rejected");
}

inline const char* circuit_prf_challenge_domain(CircuitPrfProfile profile) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return Dom::CIRCUIT_PRF_CHALLENGE;
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return Dom::CIRCUIT_PRF_CHALLENGE_V7;
    throw std::runtime_error("pvac: circuit prf profile rejected");
}

inline const char* circuit_prf_round_domain(CircuitPrfProfile profile) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return Dom::CIRCUIT_PRF_ROUND;
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return Dom::CIRCUIT_PRF_ROUND_V7;
    throw std::runtime_error("pvac: circuit prf profile rejected");
}

inline const char* circuit_prf_proof_version(CircuitPrfProfile profile) {
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return "pvac-v6-key-bound";
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return "pvac-v7-key-bound-x5";
    throw std::runtime_error("pvac: circuit prf profile rejected");
}

inline size_t circuit_prf_rounds(CircuitPrfProfile profile) {
    if (!valid_circuit_prf_profile(profile))
        throw std::runtime_error("pvac: circuit prf profile rejected");
    return 91;
}

inline Fp circuit_prf_power(CircuitPrfProfile profile, const Fp& value) {
    const Fp square = fp_mul(value, value);
    if (profile == CircuitPrfProfile::MIMC_X3_V6)
        return fp_mul(square, value);
    if (profile == CircuitPrfProfile::MIMC_X5_V7)
        return fp_mul(fp_mul(square, square), value);
    throw std::runtime_error("pvac: circuit prf profile rejected");
}

}