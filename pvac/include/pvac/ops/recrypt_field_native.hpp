// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <vector>

#include "../core/types.hpp"
#include "recrypt_native_cert.hpp"

namespace pvac {

struct NativeResetProofBundle {
    NativeResetCert cert;
    NativeResetProofPayload payload;
    NativeResetPublicEvalMaterial material;
};

inline NativeResetPublicSpec native_reset_field_spec() {
    NativeResetPublicSpec spec;
    spec.bind = NativeResetBindKind::FIELD_NATIVE;
    spec.stmt_bound = true;
    spec.output_bound = true;
    spec.material_bound = true;
    spec.relation_bound = true;
    spec.no_basis_oracle = true;
    spec.native_backend = true;
    return spec;
}

inline NativeResetSecrets native_reset_secrets_from_rows(const NativeResetRows& rows, const std::vector<std::vector<Fp>>& new_inv) {
    NativeResetSecrets sec;
    sec.new_y = new_inv;
    sec.old_x.assign(rows.bias.size(), native_fp_zeros(new_inv.empty() ? 0 : new_inv[0].size()));
    for (size_t i = 0; i < rows.bias.size(); ++i) {
        for (size_t j = 0; j < sec.old_x[i].size(); ++j) {
            Fp v = rows.bias[i][j];
            for (size_t t = 0; t < sec.new_y.size(); ++t) {
                v = fp_add(v, fp_mul(rows.mix[i][t][j], sec.new_y[t][j]));
            }
            sec.old_x[i][j] = v;
        }
    }
    return sec;
}

}