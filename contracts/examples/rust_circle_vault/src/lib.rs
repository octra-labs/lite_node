// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use rust_circle_sdk::{decode_request, Host, Request, Value};

#[cfg(feature = "test_calls")]
mod calls;

fn amount(value: &Value) -> Result<u128, i32> {
    match value {
        Value::Int(text) => text.parse().map_err(|_| 60),
        _ => Err(60),
    }
}

fn credit(who: &str) -> Result<u128, i32> {
    Host::kv_get_string(&format!("credit/{who}"))?
        .unwrap_or_else(|| "0".to_owned()).parse().map_err(|_| 61)
}

fn update(request: Request) -> Result<Value, i32> {
    #[cfg(feature = "test_calls")]
    if let Some(result) = calls::execute(&request) {
        return result;
    }
    match request.method.as_str() {
        "deposit" => {
            let caller = Host::caller()?;
            let value = amount(&Host::call_value()?)?;
            if value == 0 {
                return Err(62);
            }
            let total = credit(&caller)?.checked_add(value).ok_or(63)?;
            Host::kv_put_string(&format!("credit/{caller}"), &total.to_string())?;
            Ok(Value::Int(total.to_string()))
        }
        "withdraw" => {
            let caller = Host::caller()?;
            let value = amount(request.param(0).ok_or(64)?)?;
            if value == 0 || Host::kv_get_string("lock")?.as_deref() == Some("1") {
                return Err(65);
            }
            let remaining = credit(&caller)?.checked_sub(value).ok_or(66)?;
            Host::kv_put_string("lock", "1")?;
            Host::kv_put_string(&format!("credit/{caller}"), &remaining.to_string())?;
            if !Host::transfer(&caller, &value.to_string())? {
                return Err(67);
            }
            Host::kv_put_string("lock", "0")?;
            Ok(Value::Int(remaining.to_string()))
        }
        "read" => Host::call(request.string_param(0)?, "credit_of", &[
            Value::String(request.string_param(1)?.to_owned()),
        ]),
        "credit_of" => Ok(Value::Int(credit(request.string_param(0)?)?.to_string())),
        _ => Err(68),
    }
}

#[no_mangle]
pub extern "C" fn octra_manifest(_pointer: i32, _length: i32) -> i32 {
    #[cfg(feature = "test_calls")]
    return calls::manifest();
    #[cfg(not(feature = "test_calls"))]
    Host::respond_manifest_json(r#"{
        "methods": [
            {"name": "deposit", "view": false},
            {"name": "withdraw", "view": false},
            {"name": "read", "view": false},
            {"name": "credit_of", "view": true}
        ]
    }"#)
}

#[no_mangle]
pub extern "C" fn octra_update(pointer: i32, length: i32) -> i32 {
    match decode_request(pointer, length).and_then(update) {
        Ok(value) => Host::respond_value(value),
        Err(code) => code,
    }
}

#[no_mangle]
pub extern "C" fn octra_query(pointer: i32, length: i32) -> i32 {
    match decode_request(pointer, length).and_then(|request| {
        if request.method != "credit_of" {
            return Err(68);
        }
        Ok(Value::Int(credit(request.string_param(0)?)?.to_string()))
    }) {
        Ok(value) => Host::respond_value(value),
        Err(code) => code,
    }
}