// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use rust_circle_sdk::{Host, Request, Value};

fn visit() -> Result<Value, i32> {
    let value = Host::kv_get_string("visits")?.unwrap_or_else(|| "0".to_owned())
        .parse::<u64>().map_err(|_| 80)?;
    let value = value.checked_add(1).ok_or(81)?;
    Host::kv_put_string("visits", &value.to_string())?;
    Ok(Value::Int(value.to_string()))
}

fn probe(request: &Request) -> Result<Value, i32> {
    visit()?;
    Host::call(request.string_param(0)?, "visit", &[Value::String(Host::self_addr()?)])?;
    if request.bool_param(1)? {
        return Err(82);
    }
    visit()
}

fn policy() -> Result<Value, i32> {
    let key = "hfhe_policy:pedersen_mode";
    let prior = Host::kv_get_string(key)?;
    let next = if prior.as_deref() == Some("deny") {"any_registered"} else {"deny"};
    Host::kv_put_string(key, next)?;
    Host::call(&Host::self_addr()?, "visit", &[])?;
    match prior {
        Some(value) => Host::kv_put_string(key, &value)?,
        None => Host::kv_del(key)?,
    }
    Ok(Value::Bool(true))
}

fn number(request: &Request) -> Result<Value, i32> {
    visit()?;
    if !Host::transfer(request.string_param(1)?, "1")? {
        return Err(83);
    }
    Host::call(&Host::self_addr()?, "hex", &request.params)
}

fn hex(request: &Request) -> Result<Value, i32> {
    let length = usize::try_from(request.int_param(0)?).map_err(|_| 84)?;
    visit()?;
    if !Host::transfer(request.string_param(1)?, "1")? {
        return Err(83);
    }
    Ok(Value::Int(format!("0x{}", "f".repeat(length))))
}

fn error(request: &Request, nested: bool) -> Result<Value, i32> {
    visit()?;
    if !Host::transfer(request.string_param(1)?, "1")? {
        return Err(83);
    }
    if nested {
        Host::call(&Host::self_addr()?, "deny", &request.params)
    } else {
        Host::kv_put_string(request.string_param(0)?, "1")?;
        Host::call(&Host::self_addr()?, "visit", &[])
    }
}

fn work(request: &Request, nested: bool) -> Result<Value, i32> {
    visit()?;
    if !Host::transfer(request.string_param(1)?, "1")? {
        return Err(83);
    }
    let steps = request.int_param(0)?;
    if nested && steps > 0 {
        Host::call(&Host::self_addr()?, "depth", &[
            Value::Int((steps - 1).to_string()), request.params[1].clone(),
        ])
    } else {
        for _ in 0..steps {
            Host::kv_get_string("visits")?;
        }
        Ok(Value::Bool(true))
    }
}

fn fhe_pair(request: &Request) -> Result<Value, i32> {
    visit()?;
    if request.bool_param(4)? && !Host::transfer(request.string_param(6)?, "1")? {
        return Err(83);
    }
    if request.bool_param(5)? {
        let mut params = request.params.clone();
        params[5] = Value::Bool(false);
        return Host::call(&Host::self_addr()?, "fhe_pair", &params);
    }
    let pk = if request.params.len() > 7 {
        request.string_param(7)?.to_owned()
    } else {
        Host::fhe_load_pk(request.string_param(0)?)?
    };
    let lhs = request.string_param(1)?;
    let rhs = request.string_param(2)?;
    let cipher = if request.bool_param(3)? {
        Host::fhe_sub(&pk, lhs, rhs)?
    } else {
        Host::fhe_add(&pk, lhs, rhs)?
    };
    Host::kv_put_string("cipher", &cipher.len().to_string())?;
    Ok(Value::Bool(true))
}

pub fn execute(request: &Request) -> Option<Result<Value, i32>> {
    match request.method.as_str() {
        "visit" => Some(visit()),
        "probe" => Some(probe(request)),
        "policy" => Some(policy()),
        "number" => Some(number(request)),
        "hex" => Some(hex(request)),
        "error" => Some(error(request, true)),
        "deny" => Some(error(request, false)),
        "depth" => Some(work(request, true)),
        "fuel" => Some(work(request, false)),
        "fhe_pair" => Some(fhe_pair(request)),
        _ => None,
    }
}

pub fn manifest() -> i32 {
    Host::respond_manifest_json(r#"{
        "methods": [
            {"name": "deposit", "view": false},
            {"name": "withdraw", "view": false},
            {"name": "read", "view": false},
            {"name": "credit_of", "view": true},
            {"name": "visit", "view": false},
            {"name": "probe", "view": false},
            {"name": "policy", "view": false},
            {"name": "number", "view": false},
            {"name": "hex", "view": false},
            {"name": "error", "view": false},
            {"name": "deny", "view": false},
            {"name": "depth", "view": false},
            {"name": "fuel", "view": false},
            {"name": "fhe_pair", "view": false}
        ]
    }"#)
}