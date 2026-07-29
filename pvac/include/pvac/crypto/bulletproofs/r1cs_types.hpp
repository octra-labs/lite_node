// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

#pragma once

#include <cstdint>
#include <vector>
#include <utility>
#include "../ristretto255.hpp"

namespace pvac {
namespace bp {

enum class VarType : uint8_t {
    ONE,
    COMMITTED,
    MULT_LEFT,
    MULT_RIGHT,
    MULT_OUT
};

struct Variable {
    VarType type;
    size_t index;

    static Variable one() { return {VarType::ONE, 0}; }
    static Variable committed(size_t j) { return {VarType::COMMITTED, j}; }
    static Variable mult_left(size_t i) { return {VarType::MULT_LEFT, i}; }
    static Variable mult_right(size_t i) { return {VarType::MULT_RIGHT, i}; }
    static Variable mult_out(size_t i) { return {VarType::MULT_OUT, i}; }
};

struct LinearCombination {
    std::vector<std::pair<Variable, Scalar>> terms;

    LinearCombination() = default;

    explicit LinearCombination(Variable v) {
        terms.push_back({v, Scalar{{1, 0, 0, 0}}});
    }

    LinearCombination(Variable v, Scalar coeff) {
        terms.push_back({v, coeff});
    }

    LinearCombination& operator+=(std::pair<Variable, Scalar> term) {
        terms.push_back(term);
        return *this;
    }

    LinearCombination& operator+=(const LinearCombination& other) {
        terms.insert(terms.end(), other.terms.begin(), other.terms.end());
        return *this;
    }

    LinearCombination& operator-=(const LinearCombination& other) {
        for (const auto& [var, coeff] : other.terms)
            terms.push_back({var, sc_neg(coeff)});
        return *this;
    }

    LinearCombination operator*(const Scalar& s) const {
        LinearCombination result;
        result.terms.reserve(terms.size());
        for (const auto& [var, coeff] : terms)
            result.terms.push_back({var, sc_mul(coeff, s)});
        return result;
    }

    LinearCombination operator+(const Scalar& c) const {
        LinearCombination result = *this;
        result.terms.push_back({Variable::one(), c});
        return result;
    }

    LinearCombination operator-(const Scalar& c) const {
        return *this + sc_neg(c);
    }
};

inline LinearCombination operator+(LinearCombination a, const LinearCombination& b) {
    a += b;
    return a;
}

inline LinearCombination operator-(LinearCombination a, const LinearCombination& b) {
    a -= b;
    return a;
}

struct R1CSProof {

    RistrettoPoint A_I1;
    RistrettoPoint A_O1;
    RistrettoPoint S1;

    RistrettoPoint T_1, T_3, T_4, T_5, T_6;

    Scalar t_x;
    Scalar t_x_blinding;
    Scalar e_blinding;

    InnerProductProof ipp;

    std::vector<RistrettoPoint> V;
};

struct Constraint {
    LinearCombination lc;
};

}
}