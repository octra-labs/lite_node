// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use wasmi::errors::{ErrorKind, InstantiationError};

pub(super) fn text(error: &wasmi::Error, stable: bool) -> String {
    if stable {
        match error.kind() {
            ErrorKind::Instantiation(InstantiationError::ElementSegmentDoesNotFit {
                offset, amount, ..
            }) => return format!("element segment does not fit: offset = {offset} amount = {amount}"),
            ErrorKind::Instantiation(InstantiationError::ImportsExternalsMismatch { .. }) =>
                return "import external kind differs".to_owned(),
            _ => {}
        }
    }
    error.to_string()
}