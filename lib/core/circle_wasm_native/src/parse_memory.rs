// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use crate::native_bytes::{Lease, Pool};
use std::sync::Arc;

pub(crate) fn size(input: &[u8], unit: usize) -> Result<usize, &'static str> {
    let mut quoted = false;
    let mut escaped = false;
    let mut slots = 0_usize;
    for byte in input {
        if quoted {
            if escaped {
                escaped = false;
            } else if *byte == b'\\' {
                escaped = true;
            } else if *byte == b'"' {
                quoted = false;
            }
        } else if *byte == b'"' {
            quoted = true;
        } else if matches!(*byte, b'[' | b',') {
            slots = slots.checked_add(1).ok_or("parse memory size overflow")?;
        }
    }
    let arrays = slots.checked_mul(8).and_then(|size| size.checked_mul(unit))
        .ok_or("parse memory size overflow")?;
    let diagnostic = input.len().checked_mul(6).and_then(|size| size.checked_add(unit))
        .and_then(|size| size.checked_mul(3)).ok_or("parse memory size overflow")?;
    input.len().checked_mul(4).and_then(|size| size.checked_add(arrays))
        .and_then(|size| size.checked_add(diagnostic))
        .and_then(|size| size.checked_add(unit)).ok_or("parse memory size overflow")
}

pub(crate) fn take(input: &[u8], pool: &Arc<Pool>, unit: usize)
    -> Result<Lease, &'static str>
{
    pool.take(size(input, unit)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn quoted_slots() {
        for input in [br#"{"x":"[,]"}"#.as_slice(), br#"{"x":"\"[,]"}"#] {
            assert_eq!(size(input, 32).unwrap(), input.len() * 22 + 4 * 32);
        }
        let input = br#"{"x":["a,b",[],"\\",0]}"#;
        assert_eq!(size(input, 32).unwrap(), input.len() * 22 + 5 * 8 * 32 + 4 * 32);
    }

    #[test]
    fn checked_sizes() {
        assert!(size(b"[]", usize::MAX).is_err());
        assert_eq!(size(b"", 32).unwrap(), 128);
    }
}