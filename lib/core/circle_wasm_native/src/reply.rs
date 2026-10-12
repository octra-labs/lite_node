// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::{AssetPutJson, CallResult, EncryptedAssetPutJson, EventJson,
    HfheReceiptEntryJson, HostFailure, Runtime, SpawnJson, compute_memory, native_bytes};
use base64::{display::Base64Display, engine::general_purpose::STANDARD};
use serde::{Serialize, Serializer, ser::{SerializeMap, SerializeSeq}};
use std::{io, sync::Arc};

struct Size(usize);

impl io::Write for Size {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0 = self.0.checked_add(bytes.len())
            .ok_or_else(|| io::Error::from(io::ErrorKind::OutOfMemory))?;
        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> { Ok(()) }
}

pub(super) fn encode<T: Serialize>(value: &T, pool: Option<&Arc<native_bytes::Pool>>)
    -> Result<native_bytes::Value, HostFailure>
{
    encode_in(value, pool, None)
}

fn encode_in<T: Serialize>(value: &T, pool: Option<&Arc<native_bytes::Pool>>,
    quota: Option<&Arc<native_bytes::Budget>>) -> Result<native_bytes::Value, HostFailure>
{
    let Some(pool) = pool else {
        return serde_json::to_vec(value).map(Into::into)
            .map_err(|error| HostFailure::Rejected(format!(
                "octra_circle_wasm_host: json encode failed: {error}")));
    };
    let mut size = Size(0);
    serde_json::to_writer(&mut size, value)
        .map_err(|_| HostFailure::Unavailable("reply size failed".to_owned()))?;
    let lease = pool.take_in(size.0, quota)
        .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(size.0)
        .map_err(|_| HostFailure::Unavailable("reply allocation failed".to_owned()))?;
    if bytes.capacity() > lease.size() {
        return Err(HostFailure::Unavailable("reply capacity differs".to_owned()));
    }
    bytes.resize(size.0, 0);
    let mut output = native_bytes::Value::charged(bytes, lease)
        .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
    let mut destination = output.as_mut();
    serde_json::to_writer(&mut destination, value)
        .map_err(|_| HostFailure::Unavailable("reply encoding failed".to_owned()))?;
    if !destination.is_empty() {
        return Err(HostFailure::Unavailable("reply size differs".to_owned()));
    }
    Ok(output)
}

struct Blob<'a>(&'a [u8]);

impl Serialize for Blob<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_str(&Base64Display::new(self.0, &STANDARD))
    }
}

struct Entry<'a, T>(&'a T);
struct Rows<'a, T>(&'a [T]);

impl<T> Serialize for Rows<'_, T> where for<'a> Entry<'a, T>: Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut rows = serializer.serialize_seq(Some(self.0.len()))?;
        for value in self.0 { rows.serialize_element(&Entry(value))?; }
        rows.end()
    }
}

macro_rules! entry {
    ($ty:ty, $($field:ident),+) => {
        impl Serialize for Entry<'_, $ty> {
            fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
                let mut map = serializer.serialize_map(None)?;
                $(map.serialize_entry(stringify!($field), &self.0.$field)?;)+
                map.end()
            }
        }
    };
}

entry!(EventJson, data_b64, topic_b64);
entry!(SpawnJson, circle_id, owner_mode, payload_json, spawn_nonce);
entry!(AssetPutJson, body_b64, circle_id, content_type, encoding, path);
entry!(EncryptedAssetPutJson, activate_after_epoch, ciphertext_b64, circle_id,
    content_type, encoding, expire_after_epoch, key_id, metadata_mode, padding_class,
    path, plaintext_hash, slot_ref, state_ref);
entry!(HfheReceiptEntryJson, method, request_hash, response_hash, result);

struct View<'a> {
    runtime: &'a Runtime,
    call: &'a CallResult,
}

impl Serialize for View<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let state = self.runtime.store.data();
        let call = self.call;
        let trace = state.compute_trace.as_ref();
        let root = trace.map(|trace| trace.finish(call.code, &call.bytes, call.effort));
        let response = (!call.bytes.is_empty()).then(|| Blob(&call.bytes));
        let mut map = serializer.serialize_map(Some(if trace.is_some() { 17 } else { 15 }))?;
        map.serialize_entry("assets", &Rows(&state.assets))?;
        if trace.is_some() {
            map.serialize_entry("compute_exit", &call.code)?;
            map.serialize_entry("compute_fault", &state.compute_fault)?;
        }
        map.serialize_entry("compute_operations", &trace.map(|trace| trace.operations))?;
        map.serialize_entry("compute_steps", &trace.map(|_| call.effort))?;
        map.serialize_entry("compute_trace_root", &root)?;
        map.serialize_entry("effort_used", &call.effort)?;
        map.serialize_entry("encrypted_assets", &Rows(&state.encrypted_assets))?;
        map.serialize_entry("error", &call.error)?;
        map.serialize_entry("events", &Rows(&state.events))?;
        map.serialize_entry("hfhe_receipt_entries", &Rows(&state.hfhe_receipt_entries))?;
        map.serialize_entry("response_b64", &response)?;
        map.serialize_entry("spawns", &Rows(&state.spawns))?;
        map.serialize_entry("status_code", &state.response_status)?;
        map.serialize_entry("storage_pairs", &None::<()>)?;
        map.serialize_entry("success", &(call.code == 0))?;
        map.serialize_entry("unavailable", &state.unavailable)?;
        map.end()
    }
}

pub(super) fn execute(runtime: &Runtime, call: &CallResult)
    -> Result<native_bytes::Value, HostFailure>
{
    if !runtime.store.data().is_view {
        return Err(HostFailure::Unavailable("reply view required".to_owned()));
    }
    encode_in(&View { runtime, call }, Some(compute_memory()), runtime.store.data().budget.as_ref())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    #[test]
    fn job_reply() {
        let value = "x".repeat(4097);
        let pool = native_bytes::Pool::new(16384);
        let quota = native_bytes::Budget::new(4098);
        let (result, copies) = native_bytes::tests::large_allocs(4097,
            || encode_in(&value, Some(&pool), Some(&quota)));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(copies, 0);
        assert_eq!((pool.used(), quota.used()), (0, 0));
        let quota = native_bytes::Budget::new(16384);
        let bytes = encode_in(&value, Some(&pool), Some(&quota)).unwrap();
        assert_eq!((pool.used(), quota.used()), (4099, 4099));
        let (owner, _, _) = bytes.into_foreign().unwrap();
        assert!(quota.used() > 4099);
        unsafe { super::super::octra_compute_input_drop(owner); }
        assert_eq!((pool.used(), quota.used()), (0, 0));
    }

    #[test]
    fn quota() {
        let value = "x".repeat(4097);
        let size = value.len() + 2;
        let pool = native_bytes::Pool::new(size);
        let held = pool.take(1).unwrap();
        let (result, count) = native_bytes::tests::large_allocs(size,
            || encode(&value, Some(&pool)));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(count, 0);
        assert_eq!(pool.used(), 1);
        drop(held);
        let bytes = encode(&value, Some(&pool)).unwrap();
        assert_eq!(pool.used(), size);
        assert_eq!(bytes.as_slice(), serde_json::to_vec(&value).unwrap());
        drop(bytes);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn allocation() {
        let value = "x".repeat(4097);
        let size = value.len() + 2;
        let pool = native_bytes::Pool::new(20_000);
        let result = native_bytes::tests::refused_alloc(size,
            || encode(&value, Some(&pool)));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(pool.used(), 0);
        let bytes = encode(&value, Some(&pool)).unwrap();
        let result = native_bytes::tests::refused_alloc(std::mem::size_of::<native_bytes::Value>(),
            || bytes.into_foreign());
        assert_eq!(result.unwrap_err(), "reply owner allocation failed");
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn copy_quota() {
        let value = "x".repeat(4097);
        let size = value.len() + 2;
        let need = 2 * size + 2 * std::mem::size_of::<usize>()
            + std::mem::size_of::<native_bytes::Value>();
        for limit in [need - 1, need] {
            let pool = native_bytes::Pool::new(limit);
            let bytes = encode(&value, Some(&pool)).unwrap();
            let output = bytes.into_foreign();
            if limit < need {
                assert_eq!(output.unwrap_err(), "compute memory busy");
            } else {
                assert_eq!(pool.used(), need);
                let (owner, ptr, len) = output.unwrap();
                let copy = unsafe { std::slice::from_raw_parts(ptr, len) }.to_vec();
                assert_eq!(copy, serde_json::to_vec(&value).unwrap());
                assert_eq!(pool.used(), need);
                unsafe { super::super::octra_compute_input_drop(owner); }
            }
            assert_eq!(pool.used(), 0);
        }
    }

    #[test]
    fn lifetime() {
        let input = "x".repeat(4097);
        let size = input.len() + 2;
        let pool = native_bytes::Pool::new(20_000);
        let worker = pool.clone();
        let (_, freed) = native_bytes::tests::paused_alloc(size, 1, move || {
            let bytes = encode(&input, Some(&worker)).unwrap();
            let (owner, _, _) = bytes.into_foreign().unwrap();
            unsafe { super::super::octra_compute_input_drop(owner); }
        }, |phase| {
            let expected = match phase {
                native_bytes::tests::AllocPhase::Allocated => size,
                native_bytes::tests::AllocPhase::Releasing => 2 * size
                    + 2 * std::mem::size_of::<usize>()
                    + std::mem::size_of::<native_bytes::Value>(),
            };
            assert_eq!(pool.used(), expected);
        });
        assert!(freed);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn large_encoding() {
        let input = vec![255; super::super::MAX_RESPONSE_BYTES];
        let pool = native_bytes::Pool::new(8 * 1024 * 1024);
        let (bytes, count) = native_bytes::tests::large_allocs(4096,
            || encode(&Blob(&input), Some(&pool)).unwrap());
        assert!(bytes.len() > input.len());
        assert_eq!(count, 1);
        assert_eq!(bytes.as_slice(),
            serde_json::to_vec(&super::super::encode_b64(&input)).unwrap());
        drop(bytes);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn encoding() {
        let pool = native_bytes::Pool::new(100_000);
        for size in [0, 1, 2, 3, 31, 4097] {
            let input = vec![255; size];
            let bytes = encode(&Blob(&input), Some(&pool)).unwrap();
            let expected = serde_json::to_vec(&super::super::encode_b64(&input)).unwrap();
            assert_eq!(bytes.as_slice(), expected);
            drop(bytes);
            assert_eq!(pool.used(), 0);
        }
        let input = serde_json::json!({"z": [null, true, 7], "a": "\"\\\n\u{0010}\u{0430}"});
        let bytes = encode(&input, Some(&pool)).unwrap();
        assert_eq!(bytes.as_slice(), serde_json::to_vec(&input).unwrap());
    }

    fn check_rows<T: Serialize>(value: T) where for<'a> Entry<'a, T>: Serialize {
        let values = [value];
        let expected = serde_json::to_vec(&serde_json::to_value(&values).unwrap()).unwrap();
        let bytes = encode(&Rows(&values), Some(&native_bytes::Pool::new(10_000))).unwrap();
        assert_eq!(bytes.as_slice(), expected);
    }

    #[test]
    fn rows() {
        check_rows(EventJson { topic_b64: "a==".into(), data_b64: "b==".into() });
        check_rows(SpawnJson { circle_id: "circle".into(), owner_mode: "owner".into(),
            spawn_nonce: 3, payload_json: "{\"x\":1}".into() });
        check_rows(AssetPutJson { circle_id: "circle".into(), path: "a/b".into(),
            content_type: "text/plain".into(), encoding: None, body_b64: "a==".into() });
        check_rows(EncryptedAssetPutJson { circle_id: "circle".into(), path: None,
            slot_ref: Some("slot".into()), state_ref: None, content_type: "text/plain".into(),
            encoding: None, key_id: "key".into(), plaintext_hash: "hash".into(),
            padding_class: None, activate_after_epoch: Some("20".into()),
            expire_after_epoch: Some("30".into()), metadata_mode: Some("sealed".into()),
            ciphertext_b64: "a==".into() });
        check_rows(HfheReceiptEntryJson { method: "method".into(), request_hash: "request".into(),
            response_hash: "response".into(), result: Some(true) });
    }

    struct Changing(Cell<usize>);

    impl Serialize for Changing {
        fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
            let size = self.0.get();
            self.0.set(size + 1);
            serializer.serialize_str(if size == 0 { "" } else { "x" })
        }
    }

    #[test]
    fn changed_size() {
        let pool = native_bytes::Pool::new(100);
        let result = encode(&Changing(Cell::new(0)), Some(&pool));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(pool.used(), 0);
    }
}