// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use rust_circle_sdk::{decode_request, Host, Value};

fn manifest() -> i32 {
    Host::respond_manifest_json(
        "{\"methods\":[{\"name\":\"spawn_media\",\"view\":false},{\"name\":\"spawn_media_with_asset\",\"view\":false},{\"name\":\"spawn_media_with_encrypted_asset\",\"view\":false},{\"name\":\"spawn_media_with_encrypted_locator_asset\",\"view\":false},{\"name\":\"spawn_many\",\"view\":false},{\"name\":\"spawn_view_probe\",\"view\":true}]}"
    )
}

fn spawn_from_request(request: &rust_circle_sdk::Request) -> i32 {
    let payload_json = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let owner_mode = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    match Host::circle_spawn(payload_json, owner_mode) {
        Ok(circle_id) => Host::respond_value(Value::String(circle_id)),
        Err(code) => code,
    }
}

fn spawn_with_asset(request: &rust_circle_sdk::Request) -> i32 {
    let payload_json = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let owner_mode = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let body_b64 = match request.string_param(2) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let circle_id = match Host::circle_spawn(payload_json, owner_mode) {
        Ok(value) => value,
        Err(code) => return code,
    };
    match Host::circle_asset_put(
        circle_id.as_str(),
        "/nft/image.svg",
        "image/svg+xml",
        body_b64,
        Some("identity"),
    ) {
        Ok(true) => Host::respond_value(Value::String(circle_id)),
        Ok(false) => 63,
        Err(code) => code,
    }
}

fn spawn_with_encrypted_asset(request: &rust_circle_sdk::Request) -> i32 {
    let payload_json = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let owner_mode = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let ciphertext_b64 = match request.string_param(2) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let plaintext_hash = match request.string_param(3) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let circle_id = match Host::circle_spawn(payload_json, owner_mode) {
        Ok(value) => value,
        Err(code) => return code,
    };
    match Host::circle_asset_put_encrypted(
        circle_id.as_str(),
        "/nft/private-image.bin",
        "application/octet-stream",
        ciphertext_b64,
        "media-key-v1",
        plaintext_hash,
        Some("identity"),
        Some("media_v1"),
        None,
        None,
        Some("opaque"),
    ) {
        Ok(true) => Host::respond_value(Value::String(circle_id)),
        Ok(false) => 63,
        Err(code) => code,
    }
}

fn spawn_with_encrypted_locator_asset(request: &rust_circle_sdk::Request) -> i32 {
    let payload_json = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let owner_mode = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let ciphertext_b64 = match request.string_param(2) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let plaintext_hash = match request.string_param(3) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let locator_kind = match request.string_param(4) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let locator_value = match request.string_param(5) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let circle_id = match Host::circle_spawn(payload_json, owner_mode) {
        Ok(value) => value,
        Err(code) => return code,
    };
    match Host::circle_asset_put_encrypted_at(
        circle_id.as_str(),
        locator_kind,
        locator_value,
        "application/octet-stream",
        ciphertext_b64,
        "media-key-v1",
        plaintext_hash,
        Some("identity"),
        Some("media_v1"),
        None,
        None,
        Some("opaque"),
    ) {
        Ok(true) => Host::respond_value(Value::String(circle_id)),
        Ok(false) => 63,
        Err(code) => code,
    }
}

fn spawn_many(request: &rust_circle_sdk::Request) -> i32 {
    let payload_json = match request.string_param(0) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let owner_mode = match request.string_param(1) {
        Ok(value) => value,
        Err(code) => return code,
    };
    let mut last = String::new();
    for _ in 0..5 {
        match Host::circle_spawn(payload_json, owner_mode) {
            Ok(circle_id) => last = circle_id,
            Err(code) => return code,
        }
    }
    Host::respond_value(Value::String(last))
}

fn run_query(ptr: i32, len: i32) -> i32 {
    let request = match decode_request(ptr, len) {
        Ok(request) => request,
        Err(code) => return code,
    };
    match request.method.as_str() {
        "spawn_view_probe" => spawn_from_request(&request),
        _ => 61,
    }
}

fn run_update(ptr: i32, len: i32) -> i32 {
    let request = match decode_request(ptr, len) {
        Ok(request) => request,
        Err(code) => return code,
    };
    match request.method.as_str() {
        "spawn_media" => spawn_from_request(&request),
        "spawn_media_with_asset" => spawn_with_asset(&request),
        "spawn_media_with_encrypted_asset" => spawn_with_encrypted_asset(&request),
        "spawn_media_with_encrypted_locator_asset" => spawn_with_encrypted_locator_asset(&request),
        "spawn_many" => spawn_many(&request),
        _ => 62,
    }
}

#[no_mangle]
pub extern "C" fn octra_manifest(_ptr: i32, _len: i32) -> i32 {
    manifest()
}

#[no_mangle]
pub extern "C" fn octra_query(ptr: i32, len: i32) -> i32 {
    run_query(ptr, len)
}

#[no_mangle]
pub extern "C" fn octra_update(ptr: i32, len: i32) -> i32 {
    run_update(ptr, len)
}