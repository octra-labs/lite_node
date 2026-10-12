// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use std::sync::OnceLock;

pub const FRAC_BITS: u32 = 24;
pub const ONE: i64 = 1_i64 << FRAC_BITS;
pub const HALF: i64 = ONE / 2;

const LN_2: i64 = 11_629_080;
const EXP_BOUND: i64 = 16 * ONE;
const RMS_EPSILON_Q48: u128 = ((1_u128 << 48) + 50_000) / 100_000;
const PI: i64 = 52_707_179;
const TWO_PI: i64 = 105_414_357;
const HALF_PI: i64 = 26_353_589;
const MAX_COMPUTE_THREADS: usize = 6;
const WORK_PER_THREAD: usize = 500_000;

fn available_compute_threads() -> usize {
    static THREADS: OnceLock<usize> = OnceLock::new();
    *THREADS.get_or_init(|| {
        std::thread::available_parallelism()
            .map(|value| value.get())
            .unwrap_or(1)
            .saturating_sub(2)
            .clamp(1, MAX_COMPUTE_THREADS)
    })
}

fn compute_threads(work: usize, partitions: usize, thread_limit: usize) -> usize {
    let available = thread_limit.clamp(1, MAX_COMPUTE_THREADS);
    work.checked_add(WORK_PER_THREAD - 1)
        .map(|value| value / WORK_PER_THREAD)
        .unwrap_or(available)
        .clamp(1, available.min(partitions.max(1)))
}

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

fn i64_accumulator_safe(input: &[i64], terms: usize) -> bool {
    let maximum = input
        .iter()
        .map(|value| u128::from(value.unsigned_abs()))
        .max()
        .unwrap_or(0);
    maximum
        .checked_mul(128)
        .and_then(|value| value.checked_mul(terms as u128))
        .map(|value| value <= i64::MAX as u128)
        .unwrap_or(false)
}

fn i128_accumulator_safe(input: &[i64], terms: usize) -> bool {
    let maximum = input
        .iter()
        .map(|value| u128::from(value.unsigned_abs()))
        .max()
        .unwrap_or(0);
    maximum
        .checked_mul(128)
        .and_then(|value| value.checked_mul(terms as u128))
        .map(|value| value <= i128::MAX as u128)
        .unwrap_or(false)
}

fn scaled_sum(sum: i128, scale: i64, message: &'static str) -> Result<i64, &'static str> {
    sum.checked_mul(i128::from(scale))
        .ok_or(message)
        .and_then(|value| rounded_shift(value, FRAC_BITS))
}

fn cells<B: AsMut<[i64]>, E: From<&'static str>>(
    output: &mut B,
    count: usize,
) -> Result<&mut [i64], E> {
    let values = output.as_mut();
    if values.len() != count {
        return Err("tensor output shape differs".into());
    }
    Ok(values)
}

pub fn linear_i8(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
) -> Result<Vec<i64>, &'static str> {
    linear_i8_with_thread_limit(input, weights, out_dim, scale, available_compute_threads())
}

pub(crate) fn linear_i8_with<B: AsMut<[i64]>, E: From<&'static str>>(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    linear_alloc(input, weights, out_dim, scale, available_compute_threads(), alloc)
}

fn linear_shape(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
) -> Result<bool, &'static str> {
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
    let narrow = i64_accumulator_safe(input, input.len());
    if !narrow && !i128_accumulator_safe(input, input.len()) {
        return Err("linear accumulator overflow");
    }
    Ok(narrow)
}

const LINEAR_TILE: usize = 256;

fn linear_chunk(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    start: usize,
    scale: i64,
    narrow: bool,
    output: &mut [i64],
) -> Result<(), &'static str> {
    for (index, tile) in output.chunks_mut(LINEAR_TILE).enumerate() {
        let column = start + index * LINEAR_TILE;
        let length = tile.len();
        if narrow {
            tile.fill(0);
            for (row, value) in input.iter().copied().enumerate() {
                let offset = row * out_dim + column;
                for (sum, weight) in tile.iter_mut().zip(&weights[offset..offset + length]) {
                    *sum += i64::from(*weight as i8) * value;
                }
            }
            for value in tile {
                *value = scaled_sum(i128::from(*value), scale, "linear accumulator overflow")?;
            }
        } else {
            let mut sums = [0_i128; LINEAR_TILE];
            for (row, value) in input.iter().copied().enumerate() {
                let offset = row * out_dim + column;
                for (sum, weight) in sums[..length].iter_mut().zip(&weights[offset..offset + length]) {
                    *sum += i128::from(*weight as i8) * i128::from(value);
                }
            }
            for (value, sum) in tile.iter_mut().zip(sums) {
                *value = scaled_sum(sum, scale, "linear accumulator overflow")?;
            }
        }
    }
    Ok(())
}

pub fn visit_linear_i8(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
    mut emit: impl FnMut(&[i64]),
) -> Result<(), &'static str> {
    let narrow = linear_shape(input, weights, out_dim, scale)?;
    let mut tile = [0_i64; LINEAR_TILE];
    for start in (0..out_dim).step_by(LINEAR_TILE) {
        let length = (out_dim - start).min(LINEAR_TILE);
        linear_chunk(input, weights, out_dim, start, scale, narrow, &mut tile[..length])?;
        emit(&tile[..length]);
    }
    Ok(())
}

fn linear_i8_with_thread_limit(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
    thread_limit: usize,
) -> Result<Vec<i64>, &'static str> {
    let narrow = linear_shape(input, weights, out_dim, scale)?;
    let mut output = vec![0_i64; out_dim];
    let threads = compute_threads(input.len() * out_dim, out_dim, thread_limit);
    let chunk_size = out_dim.div_ceil(threads);
    std::thread::scope(|scope| {
        output.chunks_mut(chunk_size).enumerate()
            .map(|(index, chunk)| {
                scope.spawn(move || linear_chunk(
                    input, weights, out_dim, index * chunk_size, scale, narrow, chunk))
            })
            .collect::<Vec<_>>()
            .into_iter()
            .try_for_each(|worker| worker.join()
                .unwrap_or_else(|error| std::panic::resume_unwind(error)))
    })?;
    Ok(output)
}

fn linear_alloc<B: AsMut<[i64]>, E: From<&'static str>>(
    input: &[i64],
    weights: &[u8],
    out_dim: usize,
    scale: i64,
    thread_limit: usize,
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    let narrow = linear_shape(input, weights, out_dim, scale)?;
    let mut output = alloc(out_dim)?;
    let values = cells::<_, E>(&mut output, out_dim)?;
    let threads = compute_threads(input.len() * out_dim, out_dim, thread_limit);
    let chunk_size = out_dim.div_ceil(threads);
    std::thread::scope(|scope| {
        values.chunks_mut(chunk_size).enumerate()
            .map(|(index, chunk)| {
                scope.spawn(move || linear_chunk(
                    input, weights, out_dim, index * chunk_size, scale, narrow, chunk))
            })
            .collect::<Vec<_>>()
            .into_iter()
            .try_for_each(|worker| worker.join()
                .unwrap_or_else(|error| std::panic::resume_unwind(error)))
    })?;
    Ok(output)
}

fn top1_i64(
    input: &[i64],
    weights: &[u8],
    row_start: usize,
    scale: i64,
) -> Result<(usize, i64), &'static str> {
    weights
        .chunks_exact(input.len())
        .enumerate()
        .try_fold(None, |best, (local_index, row)| {
            let sum = row.iter().zip(input).fold(0_i64, |sum, (weight, value)| {
                sum + i64::from(*weight as i8) * *value
            });
            let score = scaled_sum(i128::from(sum), scale, "top1 accumulator overflow")?;
            let candidate = (row_start + local_index, score);
            Ok::<_, &'static str>(match best {
                Some((_, best_score)) if best_score >= score => best,
                _ => Some(candidate),
            })
        })?
        .ok_or("empty top1 tensor")
}

fn top1_i128(
    input: &[i64],
    weights: &[u8],
    row_start: usize,
    scale: i64,
) -> Result<(usize, i64), &'static str> {
    weights
        .chunks_exact(input.len())
        .enumerate()
        .try_fold(None, |best, (local_index, row)| {
            let sum = row.iter().zip(input).fold(0_i128, |sum, (weight, value)| {
                sum + i128::from(*weight as i8) * i128::from(*value)
            });
            let score = scaled_sum(sum, scale, "top1 accumulator overflow")?;
            let candidate = (row_start + local_index, score);
            Ok::<_, &'static str>(match best {
                Some((_, best_score)) if best_score >= score => best,
                _ => Some(candidate),
            })
        })?
        .ok_or("empty top1 tensor")
}

fn parallel_top1(
    input: &[i64],
    weights: &[u8],
    rows: usize,
    scale: i64,
    thread_limit: usize,
    worker: fn(&[i64], &[u8], usize, i64) -> Result<(usize, i64), &'static str>,
) -> Result<(usize, i64), &'static str> {
    let threads = compute_threads(input.len() * rows, rows, thread_limit);
    let rows_per_thread = rows.div_ceil(threads);
    let bytes_per_thread = rows_per_thread * input.len();
    let results = std::thread::scope(|scope| {
        weights
            .chunks(bytes_per_thread)
            .enumerate()
            .map(|(chunk_index, chunk)| {
                scope.spawn(move || worker(input, chunk, chunk_index * rows_per_thread, scale))
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|worker| worker.join().map_err(|_| "top1 worker failed")?)
            .collect::<Result<Vec<_>, &'static str>>()
    })?;
    results
        .into_iter()
        .reduce(|best, candidate| {
            if candidate.1 > best.1 {
                candidate
            } else {
                best
            }
        })
        .ok_or("empty top1 tensor")
}

pub fn top1_i8(
    input: &[i64],
    weights: &[u8],
    rows: usize,
    scale: i64,
) -> Result<(usize, i64), &'static str> {
    top1_i8_with_thread_limit(input, weights, rows, scale, available_compute_threads())
}

fn top1_i8_with_thread_limit(
    input: &[i64],
    weights: &[u8],
    rows: usize,
    scale: i64,
    thread_limit: usize,
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
    if i64_accumulator_safe(input, input.len()) {
        return parallel_top1(input, weights, rows, scale, thread_limit, top1_i64);
    }
    if !i128_accumulator_safe(input, input.len()) {
        return Err("top1 accumulator overflow");
    }
    parallel_top1(input, weights, rows, scale, thread_limit, top1_i128)
}

pub(crate) fn verify_thread_invariance() -> Result<(), &'static str> {
    let input_len = 768;
    let rows = 4096;
    let input = (0..input_len)
        .map(|index| ((index as i64 * 104_729) % (8 * ONE)) - 4 * ONE)
        .collect::<Vec<_>>();
    let linear_weights = (0..input_len * rows)
        .map(|index| ((index * 73 + index / 17 + 19) & 255) as u8)
        .collect::<Vec<_>>();
    let row = (0..input_len)
        .map(|index| ((index * 29 + index / 11 + 7) & 255) as u8)
        .collect::<Vec<_>>();
    let top1_weights = row.repeat(rows);
    let scale = HALF + 1_337;
    let expected_linear = linear_i8_with_thread_limit(&input, &linear_weights, rows, scale, 1)?;
    let expected_top1 = top1_i8_with_thread_limit(&input, &top1_weights, rows, scale, 1)?;
    if expected_top1.0 != 0 {
        return Err("top1 tie rule differs");
    }
    for thread_limit in [2, MAX_COMPUTE_THREADS] {
        if linear_i8_with_thread_limit(&input, &linear_weights, rows, scale, thread_limit)?
            != expected_linear
            || top1_i8_with_thread_limit(&input, &top1_weights, rows, scale, thread_limit)?
                != expected_top1
        {
            return Err("tensor thread partition differs");
        }
    }
    Ok(())
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

pub(crate) fn rmsnorm_with<B: AsMut<[i64]>, E: From<&'static str>>(
    values: &[i64],
    gamma: &[i64],
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    if values.is_empty() || values.len() != gamma.len() {
        return Err("invalid rmsnorm tensor shape".into());
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
        return Err("invalid rmsnorm divisor".into());
    }
    let mut output = alloc(values.len())?;
    let out = cells::<_, E>(&mut output, values.len())?;
    for ((value, gain), cell) in values.iter().zip(gamma).zip(out) {
        *cell = rounded_div(i128::from(*value) * i128::from(*gain), rms as i128)
            .and_then(checked_i64)?;
    }
    Ok(output)
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

pub(crate) fn silu_with<B: AsMut<[i64]>, E: From<&'static str>>(
    values: &[i64],
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    let mut output = alloc(values.len())?;
    let out = cells::<_, E>(&mut output, values.len())?;
    for (value, cell) in values.iter().zip(out) {
        *cell = mul(*value, sigmoid(*value)?)?;
    }
    Ok(output)
}

pub fn elementwise_mul(left: &[i64], right: &[i64]) -> Result<Vec<i64>, &'static str> {
    if left.len() != right.len() {
        return Err("invalid elementwise tensor shape");
    }
    left.iter().zip(right).map(|(a, b)| mul(*a, *b)).collect()
}

pub(crate) fn elementwise_mul_with<B: AsMut<[i64]>, E: From<&'static str>>(
    left: &[i64],
    right: &[i64],
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    if left.len() != right.len() {
        return Err("invalid elementwise tensor shape".into());
    }
    let mut output = alloc(left.len())?;
    let out = cells::<_, E>(&mut output, left.len())?;
    for ((a, b), cell) in left.iter().zip(right).zip(out) {
        *cell = mul(*a, *b)?;
    }
    Ok(output)
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

pub(crate) fn residual_add_with<B: AsMut<[i64]>, E: From<&'static str>>(
    left: &[i64],
    right: &[i64],
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    if left.len() != right.len() {
        return Err("invalid residual tensor shape".into());
    }
    let mut output = alloc(left.len())?;
    let out = cells::<_, E>(&mut output, left.len())?;
    for ((a, b), cell) in left.iter().zip(right).zip(out) {
        *cell = a.checked_add(*b).ok_or("residual overflow")?;
    }
    Ok(output)
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

pub(crate) fn rope_with<B: AsMut<[i64]>, E: From<&'static str>>(
    values: &[i64],
    frequencies: &[i64],
    position: i64,
    alloc: impl FnOnce(usize) -> Result<B, E>,
) -> Result<B, E> {
    if values.len() % 2 != 0 || frequencies.len() * 2 != values.len() || position < 0 {
        return Err("invalid rope tensor shape".into());
    }
    let half = frequencies.len();
    let mut output = alloc(values.len())?;
    let out = cells::<_, E>(&mut output, values.len())?;
    out.copy_from_slice(values);
    for index in 0..half {
        let angle = frequencies[index]
            .checked_mul(position)
            .ok_or("rope angle overflow")?;
        let (sine, cosine) = sin_cos(angle)?;
        let left = values[index];
        let right = values[index + half];
        out[index] = mul(left, cosine)?
            .checked_sub(mul(right, sine)?)
            .ok_or("rope overflow")?;
        out[index + half] = mul(left, sine)?
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
    let mut scores = Vec::with_capacity(sequence_len);
    for head in 0..query_heads {
        let kv_head = head / group;
        let query_start = head * head_dim;
        scores.clear();
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
        for score in &mut scores {
            *score = exp(score.checked_sub(maximum).ok_or("attention score overflow")?)?;
        }
        let denominator = scores.iter().try_fold(0_i128, |sum, value| {
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
                    i128::from(scores[position])
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

pub(crate) fn attention_kv_with<B: AsMut<[i64]>, E: From<&'static str>>(
    query: &[i64],
    keys: &[i64],
    values: &[i64],
    sequence_len: usize,
    query_heads: usize,
    kv_heads: usize,
    head_dim: usize,
    scale: i64,
    mut alloc: impl FnMut(usize) -> Result<B, E>,
) -> Result<B, E> {
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
        return Err("invalid attention tensor shape".into());
    }
    let kv_dim = kv_heads * head_dim;
    let group = query_heads / kv_heads;
    let mut output = alloc(query.len())?;
    let out = cells::<_, E>(&mut output, query.len())?;
    let mut scratch = alloc(sequence_len)?;
    let scores = cells::<_, E>(&mut scratch, sequence_len)?;
    for head in 0..query_heads {
        let kv_head = head / group;
        let query_start = head * head_dim;
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
            scores[position] = mul(rounded_shift(dot, FRAC_BITS)?, scale)?;
        }
        let maximum = *scores.iter().max().ok_or("empty attention score")?;
        for score in scores.iter_mut() {
            *score = exp(score.checked_sub(maximum).ok_or("attention score overflow")?)?;
        }
        let denominator = scores.iter().try_fold(0_i128, |sum, value| {
            sum.checked_add(i128::from(*value))
                .ok_or("attention sum overflow")
        })?;
        if denominator <= 0 {
            return Err("invalid attention denominator".into());
        }
        for dimension in 0..head_dim {
            let numerator = (0..sequence_len).try_fold(0_i128, |sum, position| {
                let value_index = position * kv_dim + kv_head * head_dim + dimension;
                sum.checked_add(
                    i128::from(scores[position])
                        .checked_mul(i128::from(values[value_index]))
                        .ok_or("attention output overflow")?,
                )
                .ok_or("attention output overflow")
            })?;
            out[query_start + dimension] = checked_i64(rounded_div(numerator, denominator)?)?;
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn output(count: usize) -> Result<Vec<i64>, &'static str> {
        Ok(vec![0; count])
    }

    #[test]
    fn owned_results_match() {
        for count in [0, 1, 2, 4, 17, 513] {
            let values = (0..count).map(|index| (index as i64 % 9 - 4) * ONE / 3)
                .collect::<Vec<_>>();
            let gamma = vec![ONE; count];
            let weights = (0..count * 3).map(|index| (index % 7) as u8).collect::<Vec<_>>();
            let frequencies = vec![ONE / 13; count / 2];
            assert_eq!(rmsnorm_with(&values, &gamma, output), rmsnorm(&values, &gamma));
            assert_eq!(silu_with(&values, output), silu(&values));
            assert_eq!(elementwise_mul_with(&values, &gamma, output),
                elementwise_mul(&values, &gamma));
            assert_eq!(residual_add_with(&values, &gamma, output), residual_add(&values, &gamma));
            assert_eq!(rope_with(&values, &frequencies, 19, output),
                rope(&values, &frequencies, 19));
            assert_eq!(linear_i8_with(&values, &weights, 3, ONE, output),
                linear_i8(&values, &weights, 3, ONE));
        }
        for values in [vec![i64::MAX; 5], vec![i64::MIN, i64::MAX], vec![0, ONE]] {
            let gamma = vec![i64::MAX; values.len()];
            assert_eq!(rmsnorm_with(&values, &gamma, output), rmsnorm(&values, &gamma));
            assert_eq!(silu_with(&values, output), silu(&values));
            assert_eq!(elementwise_mul_with(&values, &gamma, output),
                elementwise_mul(&values, &gamma));
            assert_eq!(residual_add_with(&values, &gamma, output), residual_add(&values, &gamma));
            assert_eq!(rope_with(&values, &[i64::MAX], 2, output),
                rope(&values, &[i64::MAX], 2));
        }
        for (query, keys, values, count) in [
            (vec![ONE; 4], vec![ONE; 6], vec![ONE; 6], 3),
            (vec![i64::MIN, i64::MAX], vec![i64::MAX; 2], vec![ONE; 2], 1),
            (vec![ONE; 4], vec![ONE; 6], vec![ONE; 5], 3),
        ] {
            assert_eq!(attention_kv_with(&query, &keys, &values, count, 2, 1, 2, ONE, output),
                attention_kv(&query, &keys, &values, count, 2, 1, 2, ONE));
        }
    }

    #[test]
    fn output_shape_checked() {
        let short = |_: usize| Ok::<_, &'static str>(Vec::<i64>::new());
        let long = |count: usize| Ok::<_, &'static str>(vec![0_i64; count + 1]);
        for alloc in [short as fn(usize) -> _, long as fn(usize) -> _] {
            let expected = Err("tensor output shape differs");
            assert_eq!(rmsnorm_with(&[ONE], &[ONE], alloc), expected);
            assert_eq!(silu_with(&[ONE], alloc), expected);
            assert_eq!(elementwise_mul_with(&[ONE], &[ONE], alloc), expected);
            assert_eq!(residual_add_with(&[ONE], &[ONE], alloc), expected);
            assert_eq!(rope_with(&[ONE; 2], &[ONE], 0, alloc), expected);
            assert_eq!(linear_i8_with(&[ONE], &[1], 1, ONE, alloc), expected);
            assert_eq!(attention_kv_with(&[ONE], &[ONE], &[ONE], 1, 1, 1, 1, ONE, alloc),
                expected);
        }
    }

    fn reference_linear(
        input: &[i64],
        weights: &[u8],
        out_dim: usize,
        scale: i64,
    ) -> Result<Vec<i64>, &'static str> {
        let mut sums = vec![0_i128; out_dim];
        for (input_index, value) in input.iter().enumerate() {
            let offset = input_index * out_dim;
            for output_index in 0..out_dim {
                sums[output_index] = sums[output_index]
                    .checked_add(
                        i128::from(weights[offset + output_index] as i8) * i128::from(*value),
                    )
                    .ok_or("linear accumulator overflow")?;
            }
        }
        sums.into_iter()
            .map(|sum| scaled_sum(sum, scale, "linear accumulator overflow"))
            .collect()
    }

    fn reference_top1(
        input: &[i64],
        weights: &[u8],
        rows: usize,
        scale: i64,
    ) -> Result<(usize, i64), &'static str> {
        if weights.len() != rows * input.len() {
            return Err("invalid top1 tensor shape");
        }
        weights
            .chunks_exact(input.len())
            .enumerate()
            .map(|(index, row)| {
                let sum = row
                    .iter()
                    .zip(input)
                    .try_fold(0_i128, |sum, (weight, value)| {
                        sum.checked_add(i128::from(*weight as i8) * i128::from(*value))
                            .ok_or("top1 accumulator overflow")
                    })?;
                Ok((index, scaled_sum(sum, scale, "top1 accumulator overflow")?))
            })
            .collect::<Result<Vec<_>, &'static str>>()?
            .into_iter()
            .reduce(|best, candidate| {
                if candidate.1 > best.1 {
                    candidate
                } else {
                    best
                }
            })
            .ok_or("empty top1 tensor")
    }

    fn reference_attention(query: &[i64], keys: &[i64], values: &[i64],
        count: usize, heads: usize, groups: usize, dim: usize, scale: i64)
        -> Result<Vec<i64>, &'static str> {
        let mut output = vec![0; query.len()];
        for head in 0..heads {
            let group = head / (heads / groups);
            let mut scores = Vec::with_capacity(count);
            for position in 0..count {
                let start = (position * groups + group) * dim;
                let dot = (0..dim).try_fold(0_i128, |sum, index| {
                    sum.checked_add(i128::from(query[head * dim + index])
                        * i128::from(keys[start + index]))
                        .ok_or("attention accumulator overflow")
                })?;
                scores.push(mul(rounded_shift(dot, FRAC_BITS)?, scale)?);
            }
            let maximum = *scores.iter().max().unwrap();
            let weights = scores.iter().map(|score| {
                exp(score.checked_sub(maximum).ok_or("attention score overflow")?)
            }).collect::<Result<Vec<_>, _>>()?;
            let denominator = weights.iter().try_fold(0_i128, |sum, value| {
                sum.checked_add(i128::from(*value)).ok_or("attention sum overflow")
            })?;
            for dimension in 0..dim {
                let numerator = (0..count).try_fold(0_i128, |sum, position| {
                    let index = (position * groups + group) * dim + dimension;
                    sum.checked_add(i128::from(weights[position]) * i128::from(values[index]))
                        .ok_or("attention output overflow")
                })?;
                output[head * dim + dimension] = checked_i64(rounded_div(numerator, denominator)?)?;
            }
        }
        Ok(output)
    }

    #[test]
    fn attention_values() {
        for count in [1, 2, 7, 17] {
            for (heads, groups, dim) in [(1, 1, 1), (4, 1, 3), (6, 3, 4)] {
                let query: Vec<_> = (0..heads * dim)
                    .map(|index| (index as i64 % 7 - 3) * HALF + 1).collect();
                let keys: Vec<_> = (0..count * groups * dim)
                    .map(|index| (index as i64 % 11 - 5) * ONE - 1).collect();
                let values: Vec<_> = (0..keys.len())
                    .map(|index| (index as i64 % 13 - 6) * HALF + 3).collect();
                for scale in [1, HALF, ONE, 16 * ONE] {
                    assert_eq!(attention_kv(&query, &keys, &values,
                        count, heads, groups, dim, scale),
                        reference_attention(&query, &keys, &values,
                            count, heads, groups, dim, scale));
                }
            }
        }
        for values in [[0, 1], [0, -1], [i64::MIN, i64::MAX]] {
            assert_eq!(attention_kv(&[0], &[0, 0], &values, 2, 1, 1, 1, ONE),
                reference_attention(&[0], &[0, 0], &values, 2, 1, 1, 1, ONE));
        }
        assert_eq!(attention_kv(&[0], &[0, 0], &[0, 1], 2, 1, 1, 1, ONE), Ok(vec![1]));
        assert_eq!(attention_kv(&[0], &[0, 0], &[0, -1], 2, 1, 1, 1, ONE), Ok(vec![-1]));
        for value in [-16 * ONE - 1, -16 * ONE, -16 * ONE + 1] {
            assert_eq!(attention_kv(&[ONE], &[0, value], &[1, 3], 2, 1, 1, 1, ONE),
                reference_attention(&[ONE], &[0, value], &[1, 3], 2, 1, 1, 1, ONE));
        }
    }

    #[test]
    fn attention_errors() {
        let shape = Err("invalid attention tensor shape");
        assert_eq!(attention_kv(&[], &[], &[], 0, usize::MAX, 1, 2, 0), shape);
        assert_eq!(attention_kv(&[], &[], &[], 1, 0, 1, 1, ONE), shape);
        assert_eq!(attention_kv(&[], &[], &[], 1, 1, 0, 1, ONE), shape);
        assert_eq!(attention_kv(&[], &[], &[], 1, 1, 1, 0, ONE), shape);
        assert_eq!(attention_kv(&[], &[], &[], 1, 3, 2, 1, ONE), shape);
        assert_eq!(attention_kv(&[], &[], &[], usize::MAX, 1, 1, 2, ONE), shape);
        assert_eq!(attention_kv(&[], &[], &[], 1, usize::MAX, 1, 2, ONE),
            Err("tensor size overflow"));
        assert_eq!(attention_kv(&[0, 0], &[], &[], usize::MAX, 1, 1, 2, ONE),
            Err("tensor size overflow"));
        assert_eq!(attention_kv(&[0, 0], &[], &[], usize::MAX, 2, 2, 1, ONE),
            Err("tensor size overflow"));
        assert_eq!(attention_kv(&[0], &[], &[], 1, 1, 1, 1, ONE), shape);
        assert_eq!(attention_kv(&[0], &[0], &[], 1, 1, 1, 1, ONE), shape);
        for scale in [0, -1] {
            assert_eq!(attention_kv(&[0], &[0], &[0], 1, 1, 1, 1, scale), shape);
        }
        for (query, keys, count, heads, dim, scale, error) in [
            (vec![ONE], vec![i64::MIN, i64::MAX], 2, 1, 1, ONE, "attention score overflow"),
            (vec![i64::MAX], vec![i64::MAX], 1, 1, 1, ONE, "fixed point overflow"),
            (vec![ONE], vec![i64::MAX], 1, 1, 1, 2 * ONE, "fixed point overflow"),
            (vec![i64::MAX; 3], vec![i64::MAX; 3], 1, 1, 3, ONE,
                "attention accumulator overflow"),
            (vec![ONE, ONE], vec![i64::MIN, 0, i64::MAX, 0, i64::MAX, i64::MAX],
                3, 1, 2, ONE, "fixed point overflow"),
            (vec![0, ONE], vec![i64::MIN, i64::MAX], 2, 2, 1, ONE,
                "attention score overflow"),
        ] {
            let values = vec![ONE; keys.len()];
            assert_eq!(reference_attention(&query, &keys, &values,
                count, heads, 1, dim, scale), Err(error));
            assert_eq!(attention_kv(&query, &keys, &values,
                count, heads, 1, dim, scale), Err(error));
        }
    }

    #[test]
    fn linear_tiles() {
        for wide in [false, true] {
            let unit = if wide { i64::MAX } else { ONE };
            let input: Vec<_> = (0..2049).map(|index| {
                if index == 2048 { 1 } else if index % 2 == 0 { unit } else { -unit }
            }).collect();
            let columns = 1537;
            let weights: Vec<_> = (0..input.len() * columns).map(|index| {
                let row = index / columns;
                let column = index % columns;
                ((row / 2 + column) as i64 % 3 - 1) as i8 as u8
            }).collect();
            let expected = reference_linear(&input, &weights, columns, HALF).unwrap();
            for threads in 1..=6 {
                assert_eq!(linear_i8_with_thread_limit(&input, &weights, columns, HALF, threads),
                    Ok(expected.clone()));
            }
            let mut streamed = Vec::new();
            visit_linear_i8(&input, &weights, columns, HALF, |tile| {
                assert!(tile.len() <= LINEAR_TILE);
                streamed.extend_from_slice(tile);
            }).unwrap();
            assert_eq!(streamed, expected);
        }
        assert_eq!(linear_i8(&[1, -1, 3, -3], &[1, 0, 0, 0, 0, 1, 0, 0,
            0, 0, 1, 0, 0, 0, 0, 1], 4, HALF), Ok(vec![1, -1, 2, -2]));
    }

    #[test]
    fn linear_error_order() {
        for columns in [2, 257, 1025] {
            let mut weights = vec![0; columns];
            weights[columns - 2] = 1;
            weights[columns - 1] = 127;
            for threads in 1..=6 {
                assert_eq!(linear_i8_with_thread_limit(&[i64::MAX], &weights,
                    columns, 1_i64 << 62, threads), Err("fixed point overflow"));
            }
            assert_eq!(visit_linear_i8(&[i64::MAX], &weights, columns, 1_i64 << 62, |_| {}),
                Err("fixed point overflow"));
            weights[columns - 2] = 127;
            assert_eq!(linear_i8(&[i64::MAX], &weights, columns, 1_i64 << 62),
                Err("linear accumulator overflow"));
        }
        let mut input = vec![0; 1025];
        input[0] = i64::MAX;
        let mut weights = vec![0; input.len() * 1025];
        weights[0] = 1;
        weights[1024] = 127;
        for threads in 1..=6 {
            assert_eq!(linear_i8_with_thread_limit(&input, &weights, 1025, 1_i64 << 62, threads),
                Err("fixed point overflow"));
        }
        for (input, weights, columns, scale) in [
            (&[][..], &[0][..], 1, ONE),
            (&[1][..], &[][..], 0, ONE),
            (&[1][..], &[1][..], 2, ONE),
            (&[1][..], &[1][..], 1, 0),
        ] {
            assert_eq!(linear_i8(input, weights, columns, scale),
                Err("invalid linear tensor shape"));
            assert_eq!(visit_linear_i8(input, weights, columns, scale, |_| {
                panic!("invalid tensor emitted output");
            }), Err("invalid linear tensor shape"));
        }
    }

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
    fn parallel_linear_top1() {
        let input_len = 1024;
        let rows = 1024;
        let input = (0..input_len)
            .map(|index| ((index as i64 * 104_729) % (8 * ONE)) - 4 * ONE)
            .collect::<Vec<_>>();
        let weights = (0..input_len * rows)
            .map(|index| ((index * 73 + index / 17 + 19) & 255) as u8)
            .collect::<Vec<_>>();
        let scale = HALF + 1_337;
        assert_eq!(
            linear_i8(&input, &weights, rows, scale),
            reference_linear(&input, &weights, rows, scale)
        );
        assert_eq!(
            top1_i8(&input, &weights, rows, scale),
            reference_top1(&input, &weights, rows, scale)
        );
    }

    #[test]
    fn thread_partitions_are_identical() {
        assert_eq!(verify_thread_invariance(), Ok(()));
    }

    #[test]
    fn nonlinear_symmetry() {
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
    fn rope_attention_outputs() {
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