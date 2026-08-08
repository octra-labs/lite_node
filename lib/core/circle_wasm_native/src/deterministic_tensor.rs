// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

pub const FRAC_BITS: u32 = 24;
pub const ONE: i64 = 1_i64 << FRAC_BITS;
pub const HALF: i64 = ONE / 2;

const LN_2: i64 = 11_629_080;
const EXP_BOUND: i64 = 16 * ONE;
const RMS_EPSILON_Q48: u128 = ((1_u128 << 48) + 50_000) / 100_000;
const PI: i64 = 52_707_179;
const TWO_PI: i64 = 105_414_357;
const HALF_PI: i64 = 26_353_589;

fn checked_i64(value: i128) -> Result<i64, &'static str> {
    i64::try_from(value).map_err(|_| "fixed point overflow")
}

fn rounded_div(numerator: i128, denominator: i128) -> Result<i128, &'static str> {
    if denominator <= 0 {
        return Err("fixed point denominator");
    }
    let magnitude = numerator.unsigned_abs();
    let denominator = denominator as u128;
    let quotient = (magnitude + denominator / 2) / denominator;
    let quotient = i128::try_from(quotient).map_err(|_| "fixed point overflow")?;
    Ok(if numerator < 0 { -quotient } else { quotient })
}

fn rounded_shift(value: i128, bits: u32) -> Result<i64, &'static str> {
    rounded_div(value, 1_i128 << bits).and_then(checked_i64)
}

pub fn mul(left: i64, right: i64) -> Result<i64, &'static str> {
    rounded_shift((left as i128) * (right as i128), FRAC_BITS)
}

pub fn div(numerator: i64, denominator: i64) -> Result<i64, &'static str> {
    rounded_div((numerator as i128) << FRAC_BITS, denominator as i128).and_then(checked_i64)
}

fn shift_unsigned(value: u128, power: i32) -> Result<u128, &'static str> {
    if power >= 0 {
        value
            .checked_shl(power as u32)
            .ok_or("fixed point scale overflow")
    } else {
        let shift = power.unsigned_abs();
        if shift >= 128 {
            Ok(0)
        } else if shift == 0 {
            Ok(value)
        } else {
            Ok((value + (1_u128 << (shift - 1))) >> shift)
        }
    }
}

pub fn scale_bits_to_q24(bits: i64) -> Result<i64, &'static str> {
    let raw = bits as u64;
    if raw >> 63 != 0 {
        return Err("negative tensor scale");
    }
    let exponent = ((raw >> 52) & 0x7ff) as i32;
    let fraction = raw & ((1_u64 << 52) - 1);
    if exponent == 0x7ff || (exponent == 0 && fraction == 0) {
        return Err("invalid tensor scale");
    }
    let significand = if exponent == 0 {
        fraction as u128
    } else {
        ((1_u64 << 52) | fraction) as u128
    };
    let unbiased = if exponent == 0 {
        -1022
    } else {
        exponent - 1023
    };
    let scaled = shift_unsigned(significand, unbiased - 52 + FRAC_BITS as i32)?;
    let value = i64::try_from(scaled).map_err(|_| "tensor scale overflow")?;
    if value <= 0 {
        Err("tensor scale below precision")
    } else {
        Ok(value)
    }
}

pub fn load_i8(values: &[u8], scale: i64) -> Result<Vec<i64>, &'static str> {
    if scale <= 0 {
        return Err("invalid tensor scale");
    }
    values
        .iter()
        .map(|value| {
            (i128::from(*value as i8) * i128::from(scale))
                .try_into()
                .map_err(|_| "fixed point overflow")
        })
        .collect()
}

pub fn linear_i8(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
) -> Result<Vec<i64>, &'static str> {
    if input.is_empty()
        || out_dim == 0
        || weights.len()
            != input
                .len()
                .checked_mul(out_dim)
                .ok_or("tensor size overflow")?
        || scale <= 0
    {
        return Err("invalid linear tensor shape");
    }
    let mut accumulators = vec![0_i128; out_dim];
    for (input_index, value) in input.iter().enumerate() {
        let offset = input_index * out_dim;
        for output_index in 0..out_dim {
            let product = i128::from(weights[offset + output_index] as i8)
                .checked_mul(i128::from(scale))
                .and_then(|scaled| scaled.checked_mul(i128::from(*value)))
                .ok_or("linear accumulator overflow")?;
            accumulators[output_index] = accumulators[output_index]
                .checked_add(product)
                .ok_or("linear accumulator overflow")?;
        }
    }
    accumulators
        .into_iter()
        .map(|sum| rounded_shift(sum, FRAC_BITS))
        .collect()
}

pub fn top1_i8(
    input: &[i64],
    weights: &[u8],
    rows: usize,
    scale: i64,
) -> Result<(usize, i64), &'static str> {
    if input.is_empty()
        || rows == 0
        || weights.len()
            != input
                .len()
                .checked_mul(rows)
                .ok_or("tensor size overflow")?
        || scale <= 0
    {
        return Err("invalid top1 tensor shape");
    }
    let scores = weights
        .chunks_exact(input.len())
        .map(|row| {
            row.iter()
                .zip(input)
                .try_fold(0_i128, |sum, (weight, value)| {
                    sum.checked_add(
                        i128::from(*weight as i8)
                            .checked_mul(i128::from(scale))
                            .and_then(|scaled| scaled.checked_mul(i128::from(*value)))
                            .ok_or("top1 accumulator overflow")?,
                    )
                    .ok_or("top1 accumulator overflow")
                })
                .and_then(|sum| rounded_shift(sum, FRAC_BITS))
        })
        .collect::<Result<Vec<_>, _>>()?;
    scores
        .into_iter()
        .enumerate()
        .reduce(|best, candidate| {
            if candidate.1 > best.1 {
                candidate
            } else {
                best
            }
        })
        .ok_or("empty top1 tensor")
}

fn integer_sqrt(value: u128) -> u128 {
    if value < 2 {
        return value;
    }
    let mut current = 1_u128 << ((128 - value.leading_zeros() as usize + 1) / 2);
    loop {
        let next = (current + value / current) / 2;
        if next >= current {
            return current;
        }
        current = next;
    }
}

pub fn rmsnorm(values: &[i64], gamma: &[i64]) -> Result<Vec<i64>, &'static str> {
    if values.is_empty() || values.len() != gamma.len() {
        return Err("invalid rmsnorm tensor shape");
    }
    let squares = values.iter().try_fold(0_u128, |sum, value| {
        let magnitude = i128::from(*value).unsigned_abs();
        sum.checked_add(magnitude.checked_mul(magnitude).ok_or("rmsnorm overflow")?)
            .ok_or("rmsnorm overflow")
    })?;
    let mean = squares / values.len() as u128;
    let rms = integer_sqrt(
        mean.checked_add(RMS_EPSILON_Q48)
            .ok_or("rmsnorm overflow")?,
    );
    if rms == 0 || rms > i128::MAX as u128 {
        return Err("invalid rmsnorm divisor");
    }
    values
        .iter()
        .zip(gamma)
        .map(|(value, gain)| {
            rounded_div(i128::from(*value) * i128::from(*gain), rms as i128).and_then(checked_i64)
        })
        .collect()
}

fn exp_polynomial(value: i64) -> Result<i64, &'static str> {
    let mut term = ONE;
    let mut total = ONE;
    for divisor in 1_i64..=8 {
        term = mul(term, value)?;
        term = checked_i64(rounded_div(i128::from(term), i128::from(divisor))?)?;
        total = total.checked_add(term).ok_or("exponential overflow")?;
    }
    Ok(total.max(0))
}

pub fn exp(value: i64) -> Result<i64, &'static str> {
    if value <= -EXP_BOUND {
        return Ok(0);
    }
    if value >= EXP_BOUND {
        return Err("exponential input exceeds bound");
    }
    let power = checked_i64(rounded_div(i128::from(value), i128::from(LN_2))?)?;
    let remainder = value
        .checked_sub(power.checked_mul(LN_2).ok_or("exponential overflow")?)
        .ok_or("exponential overflow")?;
    let base = exp_polynomial(remainder)?;
    if power >= 0 {
        base.checked_shl(power as u32).ok_or("exponential overflow")
    } else {
        let shift = power.unsigned_abs();
        if shift >= 63 {
            Ok(0)
        } else if shift == 0 {
            Ok(base)
        } else {
            Ok((base + (1_i64 << (shift - 1))) >> shift)
        }
    }
}

pub fn sigmoid(value: i64) -> Result<i64, &'static str> {
    if value >= 0 {
        div(
            ONE,
            ONE.checked_add(exp(-value)?).ok_or("sigmoid overflow")?,
        )
    } else {
        let magnitude = exp(value)?;
        div(
            magnitude,
            ONE.checked_add(magnitude).ok_or("sigmoid overflow")?,
        )
    }
}

pub fn silu(values: &[i64]) -> Result<Vec<i64>, &'static str> {
    values
        .iter()
        .map(|value| mul(*value, sigmoid(*value)?))
        .collect()
}

pub fn elementwise_mul(left: &[i64], right: &[i64]) -> Result<Vec<i64>, &'static str> {
    if left.len() != right.len() {
        return Err("invalid elementwise tensor shape");
    }
    left.iter().zip(right).map(|(a, b)| mul(*a, *b)).collect()
}

pub fn residual_add(left: &[i64], right: &[i64]) -> Result<Vec<i64>, &'static str> {
    if left.len() != right.len() {
        return Err("invalid residual tensor shape");
    }
    left.iter()
        .zip(right)
        .map(|(a, b)| a.checked_add(*b).ok_or("residual overflow"))
        .collect()
}

fn divide_integer(value: i64, divisor: i64) -> Result<i64, &'static str> {
    checked_i64(rounded_div(i128::from(value), i128::from(divisor))?)
}

fn sin_cos_polynomial(value: i64) -> Result<(i64, i64), &'static str> {
    let square = mul(value, value)?;
    let mut sin_term = value;
    let mut sin_total = value;
    for index in 1_i64..=7 {
        sin_term = divide_integer(mul(sin_term, -square)?, (2 * index) * (2 * index + 1))?;
        sin_total = sin_total.checked_add(sin_term).ok_or("sine overflow")?;
    }
    let mut cos_term = ONE;
    let mut cos_total = ONE;
    for index in 1_i64..=7 {
        cos_term = divide_integer(mul(cos_term, -square)?, (2 * index - 1) * (2 * index))?;
        cos_total = cos_total.checked_add(cos_term).ok_or("cosine overflow")?;
    }
    Ok((sin_total, cos_total))
}

pub fn sin_cos(value: i64) -> Result<(i64, i64), &'static str> {
    let mut reduced = value % TWO_PI;
    if reduced > PI {
        reduced -= TWO_PI;
    } else if reduced < -PI {
        reduced += TWO_PI;
    }
    if reduced > HALF_PI {
        let (sine, cosine) = sin_cos_polynomial(PI - reduced)?;
        Ok((sine, -cosine))
    } else if reduced < -HALF_PI {
        let (sine, cosine) = sin_cos_polynomial(-PI - reduced)?;
        Ok((sine, -cosine))
    } else {
        sin_cos_polynomial(reduced)
    }
}

pub fn rope(values: &[i64], frequencies: &[i64], position: i64) -> Result<Vec<i64>, &'static str> {
    if values.len() % 2 != 0 || frequencies.len() * 2 != values.len() || position < 0 {
        return Err("invalid rope tensor shape");
    }
    let half = frequencies.len();
    let mut output = values.to_vec();
    for index in 0..half {
        let angle = frequencies[index]
            .checked_mul(position)
            .ok_or("rope angle overflow")?;
        let (sine, cosine) = sin_cos(angle)?;
        let left = values[index];
        let right = values[index + half];
        output[index] = mul(left, cosine)?
            .checked_sub(mul(right, sine)?)
            .ok_or("rope overflow")?;
        output[index + half] = mul(left, sine)?
            .checked_add(mul(right, cosine)?)
            .ok_or("rope overflow")?;
    }
    Ok(output)
}

pub fn attention_kv(
    query: &[i64],
    keys: &[i64],
    values: &[i64],
    sequence_len: usize,
    query_heads: usize,
    kv_heads: usize,
    head_dim: usize,
    scale: i64,
) -> Result<Vec<i64>, &'static str> {
    if sequence_len == 0
        || query_heads == 0
        || kv_heads == 0
        || head_dim == 0
        || query_heads % kv_heads != 0
        || query.len()
            != query_heads
                .checked_mul(head_dim)
                .ok_or("tensor size overflow")?
        || keys.len()
            != sequence_len
                .checked_mul(kv_heads)
                .and_then(|size| size.checked_mul(head_dim))
                .ok_or("tensor size overflow")?
        || values.len() != keys.len()
        || scale <= 0
    {
        return Err("invalid attention tensor shape");
    }
    let kv_dim = kv_heads * head_dim;
    let group = query_heads / kv_heads;
    let mut output = vec![0_i64; query.len()];
    for head in 0..query_heads {
        let kv_head = head / group;
        let query_start = head * head_dim;
        let mut scores = Vec::with_capacity(sequence_len);
        for position in 0..sequence_len {
            let key_start = position * kv_dim + kv_head * head_dim;
            let dot = (0..head_dim).try_fold(0_i128, |sum, index| {
                sum.checked_add(
                    i128::from(query[query_start + index])
                        .checked_mul(i128::from(keys[key_start + index]))
                        .ok_or("attention accumulator overflow")?,
                )
                .ok_or("attention accumulator overflow")
            })?;
            scores.push(mul(rounded_shift(dot, FRAC_BITS)?, scale)?);
        }
        let maximum = *scores.iter().max().ok_or("empty attention score")?;
        let exponentials = scores
            .iter()
            .map(|score| {
                exp(score
                    .checked_sub(maximum)
                    .ok_or("attention score overflow")?)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let denominator = exponentials.iter().try_fold(0_i128, |sum, value| {
            sum.checked_add(i128::from(*value))
                .ok_or("attention sum overflow")
        })?;
        if denominator <= 0 {
            return Err("invalid attention denominator");
        }
        for dimension in 0..head_dim {
            let numerator = (0..sequence_len).try_fold(0_i128, |sum, position| {
                let value_index = position * kv_dim + kv_head * head_dim + dimension;
                sum.checked_add(
                    i128::from(exponentials[position])
                        .checked_mul(i128::from(values[value_index]))
                        .ok_or("attention output overflow")?,
                )
                .ok_or("attention output overflow")
            })?;
            output[query_start + dimension] = checked_i64(rounded_div(numerator, denominator)?)?;
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scale_decoding_is_exact() {
        assert_eq!(scale_bits_to_q24(0.5_f64.to_bits() as i64), Ok(HALF));
        assert_eq!(scale_bits_to_q24(1.0_f64.to_bits() as i64), Ok(ONE));
    }

    #[test]
    fn linear_and_top1_are_stable() {
        let input = [ONE, 2 * ONE];
        let weights = [1_u8, 2_u8, 3_u8, 4_u8];
        assert_eq!(
            load_i8(&weights, HALF),
            Ok(vec![HALF, ONE, 3 * HALF, 2 * ONE])
        );
        assert_eq!(
            linear_i8(&input, &weights, 2, HALF),
            Ok(vec![3 * ONE + HALF, 5 * ONE])
        );
        assert_eq!(top1_i8(&input, &weights, 2, HALF), Ok((1, 5 * ONE + HALF)));
        assert_eq!(
            elementwise_mul(&input, &[2 * ONE, HALF]),
            Ok(vec![2 * ONE, ONE])
        );
        assert_eq!(residual_add(&input, &[ONE, -ONE]), Ok(vec![2 * ONE, ONE]));
    }

    #[test]
    fn nonlinear_functions_preserve_symmetry() {
        assert_eq!(sigmoid(0), Ok(HALF));
        let positive = sigmoid(ONE).unwrap();
        let negative = sigmoid(-ONE).unwrap();
        assert!((positive + negative - ONE).abs() <= 2);
        assert_eq!(silu(&[0]), Ok(vec![0]));
        let normalized = rmsnorm(&[ONE, -ONE], &[ONE, ONE]).unwrap();
        assert_eq!(normalized[0], -normalized[1]);
        assert!((normalized[0] - ONE).abs() < 256);
    }

    #[test]
    fn rope_and_attention_have_fixed_outputs() {
        assert_eq!(sin_cos(0), Ok((0, ONE)));
        let quarter = sin_cos(HALF_PI).unwrap();
        assert!((quarter.0 - ONE).abs() < 16);
        assert!(quarter.1.abs() < 16);
        assert_eq!(
            rope(&[ONE, 2 * ONE, 3 * ONE, 4 * ONE], &[ONE, HALF], 0),
            Ok(vec![ONE, 2 * ONE, 3 * ONE, 4 * ONE])
        );
        let attended = attention_kv(
            &[ONE, 0],
            &[ONE, 0, 0, ONE],
            &[ONE, 2 * ONE, 3 * ONE, 4 * ONE],
            2,
            1,
            1,
            2,
            ONE,
        )
        .unwrap();
        assert_eq!(attended, vec![25_801_394, 42_578_610]);
    }
}