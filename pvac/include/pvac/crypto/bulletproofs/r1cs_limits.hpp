// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstddef>

namespace pvac {
namespace bp {

inline constexpr size_t R1CS_MAX_GATES = static_cast<size_t>(1) << 20;
inline constexpr size_t R1CS_MAX_COMMITTED = static_cast<size_t>(1) << 16;
inline constexpr size_t R1CS_MAX_CONSTRAINTS = 1'500'000;
inline constexpr size_t R1CS_KEY_SWITCH_MAX_CONSTRAINTS = 1'550'000;
inline constexpr size_t R1CS_MAX_TERMS = static_cast<size_t>(1) << 22;

enum class R1CSLimitProfile {
    Default,
    KeySwitchRefresh
};

inline constexpr size_t r1cs_constraint_limit(R1CSLimitProfile profile) {
    switch (profile) {
        case R1CSLimitProfile::Default:
            return R1CS_MAX_CONSTRAINTS;
        case R1CSLimitProfile::KeySwitchRefresh:
            return R1CS_KEY_SWITCH_MAX_CONSTRAINTS;
    }
    return 0;
}

}
}