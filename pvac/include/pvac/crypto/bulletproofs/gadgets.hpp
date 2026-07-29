// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <cassert>
#include "r1cs_prover.hpp"
#include "../../core/field.hpp"

namespace pvac {
namespace bp {

inline Scalar sc_from_u64(uint64_t v) {
    return Scalar{{v, 0, 0, 0}};
}

inline Scalar sc_pow2_64() {
    return Scalar{{0, 1, 0, 0}};
}

inline Scalar sc_mersenne_p() {
    return Scalar{{UINT64_MAX, 0x7FFFFFFFFFFFFFFFULL, 0, 0}};
}

inline Scalar sc_mersenne_p_inv() {
    return sc_inv(sc_mersenne_p());
}

inline void range_check(
    R1CSProver& prover,
    const Variable& v_var,
    const Scalar& v_val,
    size_t n_bits
) {
    assert(n_bits > 0 && n_bits <= 252);

    auto get_bit = [&](size_t bit) -> uint64_t {
        size_t word = bit / 64;
        size_t pos = bit % 64;
        return (v_val.v[word] >> pos) & 1;
    };

    LinearCombination reconstruction;
    Scalar power_of_2 = sc_from_u64(1);
    Scalar two = sc_from_u64(2);
    Scalar one_sc = sc_from_u64(1);

    for (size_t i = 0; i < n_bits; i++) {
        Scalar bit_val = sc_from_u64(get_bit(i));

        auto [left, right, out] = prover.allocate(
            bit_val, sc_sub(one_sc, bit_val));

        prover.constrain(LinearCombination(out));

        LinearCombination sum_lc(left);
        sum_lc += LinearCombination(right);
        sum_lc -= LinearCombination(Variable::one());
        prover.constrain(sum_lc);

        reconstruction += LinearCombination(left, power_of_2);
        power_of_2 = sc_mul(power_of_2, two);
    }

    reconstruction -= LinearCombination(v_var);
    prover.constrain(reconstruction);
}

inline void reject_mersenne_modulus_alias(
    R1CSProver& prover,
    const Variable& v_var,
    const Scalar& v_val
) {
    const Scalar one = sc_from_u64(1);
    const Scalar delta = sc_sub(sc_mersenne_p(), v_val);
    const Scalar inverse = sc_inv(delta);
    auto [delta_var, inverse_var, product_var] = prover.allocate(delta, inverse);

    LinearCombination delta_lc(delta_var);
    delta_lc += LinearCombination(v_var);
    delta_lc -= LinearCombination(Variable::one(), sc_mersenne_p());
    prover.constrain(delta_lc);

    LinearCombination product_lc(product_var);
    product_lc -= LinearCombination(Variable::one(), one);
    prover.constrain(product_lc);
}

struct FpMulResult {
    Variable var;
    Scalar val;
};

inline FpMulResult fp_mul_gadget(
    R1CSProver& prover,
    const Variable& a_var, const Scalar& a_val,
    const Variable& b_var, const Scalar& b_val,
    bool canonical = false
) {
    Scalar one_sc = sc_from_u64(1);

    Scalar a0_val = sc_from_u64(a_val.v[0]);
    Scalar a1_val = sc_from_u64(a_val.v[1]);

    auto [a0_var, a0_r, a0_o] = prover.allocate(a0_val, one_sc);
    auto [a1_var, a1_r, a1_o] = prover.allocate(a1_val, one_sc);

    {
        LinearCombination lc(a0_var);
        lc += LinearCombination(a1_var, sc_pow2_64());
        lc -= LinearCombination(a_var);
        prover.constrain(lc);
    }

    range_check(prover, a0_var, a0_val, 64);
    range_check(prover, a1_var, a1_val, 63);
    if (canonical)
        reject_mersenne_modulus_alias(prover, a_var, a_val);

    Scalar b0_val = sc_from_u64(b_val.v[0]);
    Scalar b1_val = sc_from_u64(b_val.v[1]);

    auto [b0_var, b0_r, b0_o] = prover.allocate(b0_val, one_sc);
    auto [b1_var, b1_r, b1_o] = prover.allocate(b1_val, one_sc);

    {
        LinearCombination lc(b0_var);
        lc += LinearCombination(b1_var, sc_pow2_64());
        lc -= LinearCombination(b_var);
        prover.constrain(lc);
    }

    range_check(prover, b0_var, b0_val, 64);
    range_check(prover, b1_var, b1_val, 63);
    if (canonical)
        reject_mersenne_modulus_alias(prover, b_var, b_val);

    auto [p00_l, p00_r, p00_out] = prover.multiply(
        LinearCombination(a0_var), LinearCombination(b0_var));
    Scalar p00_val = sc_mul(a0_val, b0_val);

    auto [p01_l, p01_r, p01_out] = prover.multiply(
        LinearCombination(a0_var), LinearCombination(b1_var));
    Scalar p01_val = sc_mul(a0_val, b1_val);

    auto [p10_l, p10_r, p10_out] = prover.multiply(
        LinearCombination(a1_var), LinearCombination(b0_var));
    Scalar p10_val = sc_mul(a1_val, b0_val);

    auto [p11_l, p11_r, p11_out] = prover.multiply(
        LinearCombination(a1_var), LinearCombination(b1_var));
    Scalar p11_val = sc_mul(a1_val, b1_val);

    Scalar pow2_64 = sc_pow2_64();
    Scalar two = sc_from_u64(2);
    Scalar T_red_val = sc_add(
        sc_add(p00_val, sc_mul(pow2_64, sc_add(p01_val, p10_val))),
        sc_mul(two, p11_val)
    );

    Fp a_fp = {a_val.v[0], a_val.v[1]};
    Fp b_fp = {b_val.v[0], b_val.v[1]};
    Fp c_fp = fp_mul(a_fp, b_fp);
    Scalar c_val = Scalar{{c_fp.lo, c_fp.hi, 0, 0}};

    Scalar p_sc = sc_mersenne_p();
    Scalar T_minus_c = sc_sub(T_red_val, c_val);
    Scalar q_val = sc_mul(T_minus_c, sc_inv(p_sc));

    auto [c_var, c_r, c_o] = prover.allocate(c_val, one_sc);
    auto [q_var, q_r, q_o] = prover.allocate(q_val, one_sc);

    auto [q_left, p_right, qp_out] = prover.multiply(
        LinearCombination(q_var),
        LinearCombination(Variable::one(), p_sc)
    );

    {
        LinearCombination lc(qp_out);
        lc -= LinearCombination(p00_out);
        lc -= LinearCombination(p01_out, pow2_64);
        lc -= LinearCombination(p10_out, pow2_64);
        lc -= LinearCombination(p11_out, two);
        lc += LinearCombination(c_var);
        prover.constrain(lc);
    }

    range_check(prover, c_var, c_val, 127);
    if (canonical)
        reject_mersenne_modulus_alias(prover, c_var, c_val);
    range_check(prover, q_var, q_val, 66);

    return {c_var, c_val};
}

inline FpMulResult fp_cube_gadget(
    R1CSProver& prover,
    const Variable& x_var,
    const Scalar& x_val,
    bool canonical = false
) {
    auto x2 = fp_mul_gadget(prover, x_var, x_val, x_var, x_val, canonical);
    return fp_mul_gadget(prover, x2.var, x2.val, x_var, x_val, canonical);
}

inline FpMulResult fp_fifth_gadget(
    R1CSProver& prover,
    const Variable& x_var,
    const Scalar& x_val,
    bool canonical
) {
    auto x2 = fp_mul_gadget(prover, x_var, x_val, x_var, x_val, canonical);
    auto x4 = fp_mul_gadget(prover, x2.var, x2.val, x2.var, x2.val, canonical);
    return fp_mul_gadget(prover, x4.var, x4.val, x_var, x_val, canonical);
}

struct FpLimbs {
    Variable x0_var, x1_var;
    Scalar x0_val, x1_val;
};

inline FpLimbs fp127_decompose(
    R1CSProver& prover,
    const Variable& x_var,
    const Scalar& x_val,
    bool canonical = false
) {
    Scalar x0_val = sc_from_u64(x_val.v[0]);
    Scalar x1_val = sc_from_u64(x_val.v[1]);
    Scalar one_sc = sc_from_u64(1);

    auto [x0_var, x0_r, x0_o] = prover.allocate(x0_val, one_sc);
    auto [x1_var, x1_r, x1_o] = prover.allocate(x1_val, one_sc);

    {
        LinearCombination lc(x0_var);
        lc += LinearCombination(x1_var, sc_pow2_64());
        lc -= LinearCombination(x_var);
        prover.constrain(lc);
    }

    range_check(prover, x0_var, x0_val, 64);
    range_check(prover, x1_var, x1_val, 63);
    if (canonical)
        reject_mersenne_modulus_alias(prover, x_var, x_val);

    return {x0_var, x1_var, x0_val, x1_val};
}

struct BoundPcResult {
    Variable x_var;
    Scalar x_val;
    FpLimbs limbs;
};

inline BoundPcResult bind_pc_value(
    R1CSProver& prover,
    const Variable& v_signed,
    const Fp& x_fp,
    bool canonical = false
) {
    Scalar x_val = Scalar{{x_fp.lo, x_fp.hi, 0, 0}};
    bool is_high = (x_fp.hi & (1ULL << 62)) != 0;
    Scalar b_val = sc_from_u64(is_high ? 1 : 0);
    Scalar one_sc = sc_from_u64(1);

    auto [b_left, b_right, b_out] = prover.allocate(b_val, sc_sub(one_sc, b_val));
    prover.constrain(LinearCombination(b_out));
    {
        LinearCombination sum(b_left);
        sum += LinearCombination(b_right);
        sum -= LinearCombination(Variable::one());
        prover.constrain(sum);
    }

    auto [x_var, x_r, x_o] = prover.allocate(x_val, one_sc);

    {
        LinearCombination lc(x_var);
        lc -= LinearCombination(v_signed);
        lc -= LinearCombination(b_left, sc_mersenne_p());
        prover.constrain(lc);
    }

    auto limbs = fp127_decompose(prover, x_var, x_val, canonical);

    return {x_var, x_val, limbs};
}

struct FpFoldedTerm {
    LinearCombination lc;
    Scalar val;
};

inline FpFoldedTerm fp_mul_const_var_folded(
    const Fp& A_const,
    const FpLimbs& x_limbs
) {

    Scalar A0 = sc_from_u64(A_const.lo);
    Scalar A1 = sc_from_u64(A_const.hi);
    Scalar pow2_64 = sc_pow2_64();
    Scalar two = sc_from_u64(2);

    LinearCombination lc;

    lc += LinearCombination(x_limbs.x0_var, A0);

    lc += LinearCombination(x_limbs.x1_var, sc_mul(pow2_64, A0));

    lc += LinearCombination(x_limbs.x0_var, sc_mul(pow2_64, A1));

    lc += LinearCombination(x_limbs.x1_var, sc_mul(two, A1));

    Scalar x0 = x_limbs.x0_val;
    Scalar x1 = x_limbs.x1_val;
    Scalar T_val = sc_add(
        sc_add(sc_mul(A0, x0), sc_mul(sc_mul(pow2_64, A0), x1)),
        sc_add(sc_mul(sc_mul(pow2_64, A1), x0), sc_mul(sc_mul(two, A1), x1))
    );

    return {lc, T_val};
}

}
}