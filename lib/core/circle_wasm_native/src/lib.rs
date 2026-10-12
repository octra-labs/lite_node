// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use base64::{engine::general_purpose::STANDARD as BASE64, Engine as _};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use sha2::{Digest, Sha256};
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::slice;
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use wasmi::{
    AsContext, AsContextMut, Caller, Config, Engine, Error as WasmiError, Extern, ExternType, Func,
    Instance, Linker, Module, ResourceLimiter, Store, StoreLimits, StoreLimitsBuilder,
};
use wasmi::errors::{MemoryError, TableError};

mod deterministic_tensor;
mod float_ops;
#[cfg(test)]
mod float_cases;
mod native_bytes;
mod parse_memory;
mod reply;
mod call_step;
mod compute_replay;
use native_bytes::write_owned_bytes;

type StorageData = BTreeMap<String, Arc<native_bytes::Value>>;

#[derive(Debug, Default)]
struct Storage {
    data: StorageData,
    keys: Option<native_bytes::Lease>,
}

impl Storage {
    fn uncounted(&self, quota: &Arc<native_bytes::Budget>) -> Result<usize, &'static str> {
        let keys = match self.keys.as_ref() {
            Some(lease) if lease.counted(quota) => 0,
            Some(lease) => lease.size(),
            None => self.data.keys().try_fold(0usize, |size, key|
                size.checked_add(key.capacity()).ok_or("storage size overflow"))?,
        };
        self.data.values().try_fold(keys, |size, value|
            size.checked_add(value.uncounted(quota)?).ok_or("storage size overflow"))
    }
}

impl From<StorageData> for Storage {
    fn from(data: StorageData) -> Self {
        Self { data, keys: None }
    }
}

impl std::ops::Deref for Storage {
    type Target = StorageData;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

#[cfg(test)]
impl<const N: usize> From<[(String, Arc<native_bytes::Value>); N]> for Storage {
    fn from(entries: [(String, Arc<native_bytes::Value>); N]) -> Self {
        BTreeMap::from(entries).into()
    }
}

#[cfg(test)]
impl PartialEq for Storage {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

const MAX_RESPONSE_BYTES: usize = 2 * 1024 * 1024;
const MAX_SPAWN_PAYLOAD_JSON_BYTES: usize = 2 * 1024 * 1024;
const MAX_ASSET_EFFECT_BODY_B64_BYTES: usize = 1_398_104;
const MAX_GUEST_READ_BYTES: usize = 16 * 1024 * 1024;
const LOCAL_INPUT_LIMIT: usize = 128 * 1024 * 1024;
const MAX_GUEST_WRITE_BYTES: usize = 16 * 1024 * 1024;
const MAX_EVENT_COUNT: usize = 64;
const MAX_EVENT_TOPIC_BYTES: usize = 256;
const MAX_EVENT_DATA_BYTES: usize = 64 * 1024;
const MAX_EVENT_BYTES: usize = 256 * 1024;
const MAX_PUBLIC_READS: usize = 16;
const MAX_PUBLIC_READ_BYTES: usize = 256 * 1024;
const MAX_HFHE_RECEIPT_ENTRIES: usize = 16;
const MAX_HFHE_VERIFIER_ENTRIES: usize = 2;
const MAX_TENSOR_SESSION_BYTES: usize = 8 * 1024 * 1024;
const DEFAULT_FUEL_LIMIT: u64 = 20_000_000;
const MAX_FUEL_LIMIT: u64 = 20_000_000;
const COMPUTE_MAX_GUEST_BYTES: usize = 64 * 1024 * 1024;
const COMPUTE_MAX_TENSOR_SESSION_BYTES: usize = 128 * 1024 * 1024;
const COMPUTE_DEFAULT_FUEL_LIMIT: u64 = 1_000_000_000;
const COMPUTE_MAX_FUEL_LIMIT: u64 = 2_000_000_000;
const CIRCLE_INVOKE_FUEL: u64 = 100_000;
const HFHE_INVOKE_FUEL: u64 = 2_000_000;
const PAGE_BYTES: usize = 65536;
const MAX_INITIAL_PAGES: usize = 512;
const COMPUTE_MAX_INITIAL_PAGES: usize = 1024;
const SESSION_CACHE_LIMIT: usize = 128;
const LOCAL_SESSION_BYTES: usize = 128 * 1024 * 1024;
const SESSION_CACHE_TTL_SECS: f64 = 300.0;
const TENSOR_SESSION_CACHE_LIMIT: usize = 8;
const FIXED_TENSOR_SESSION_CACHE_LIMIT: usize = 2;
const TENSOR_SESSION_CACHE_TTL_SECS: f64 = 300.0;
const Q24_FUEL_SCALE: usize = 64;
const Q24_CALL_LIMIT: usize = 250_000;
const Q24_WALL_LIMIT_SECS: u64 = 30;
const MODULE_CACHE_LIMIT: usize = 32;
const MODULE_CACHE_TTL_SECS: f64 = 300.0;
const STORAGE_CACHE_LIMIT: usize = 16;
const STORAGE_CACHE_TTL_SECS: f64 = 300.0;
const STORAGE_CACHE_BYTE_LIMIT: usize = 48 * 1024 * 1024;
const COMPUTE_STORAGE_CACHE_LIMIT: usize = 2;
const COMPUTE_STORAGE_LEASE_LIMIT: usize = 128;
const COMPUTE_STORAGE_BUILDER_LIMIT: usize = 2;
const COMPUTE_STORAGE_ENTRY_LIMIT: usize = 1024;
const COMPUTE_STORAGE_KEY_LIMIT: usize = 256;
const COMPUTE_STORAGE_VALUE_LIMIT: usize = 8 * 1024 * 1024;
const COMPUTE_STORAGE_BYTE_LIMIT: usize = 512 * 1024 * 1024;
const COMPUTE_STORAGE_RESIDENT_LIMIT: usize =
    (COMPUTE_STORAGE_CACHE_LIMIT + COMPUTE_STORAGE_BUILDER_LIMIT)
        * (COMPUTE_STORAGE_BYTE_LIMIT + (COMPUTE_STORAGE_KEY_LIMIT + 2) * COMPUTE_STORAGE_ENTRY_LIMIT)
        + COMPUTE_STORAGE_BUILDER_LIMIT
            * (COMPUTE_STORAGE_VALUE_LIMIT + 2 + MAX_GUEST_READ_BYTES);
const COMPUTE_TRACE_DOMAIN: &[u8] = b"octra:resource-compute-trace:v1\0";
const REQUEST_MAGIC: &[u8; 5] = b"OCWR1";
const RESPONSE_MAGIC: &[u8; 5] = b"OCWS1";
const BASE58_ALPHABET: &[u8; 58] = b"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

const ALLOWED_IMPORTS: &[&str] = &[
    "host_response_reset",
    "host_response_write",
    "host_response_finish",
    "host_circle_invoke_len",
    "host_circle_invoke",
    "host_hfhe_invoke_len",
    "host_hfhe_invoke",
    "host_kv_get_len",
    "host_kv_get",
    "host_kv_put",
    "host_kv_del",
    "host_session_get_len",
    "host_session_get",
    "host_session_put",
    "host_tensor_session_has",
    "host_tensor_session_reset",
    "host_tensor_session_append_kv_fp",
    "host_tensor_session_attention_kv_fp",
    "host_epoch",
    "host_caller_len",
    "host_caller_read",
    "host_self_len",
    "host_self_read",
    "host_state_path_key_len",
    "host_state_path_key",
    "host_emit_event",
    "host_tensor_load_i8_kv",
    "host_tensor_matmul_fp",
    "host_tensor_rmsnorm_fp",
    "host_tensor_silu_fp",
    "host_tensor_elemwise_mul_fp",
    "host_tensor_residual_add_fp",
    "host_tensor_rope_apply_fp",
    "host_tensor_attention_kv_fp",
    "host_tensor_append_vec_fp",
    "host_tensor_argmax_fp",
    "host_tensor_linear_i8_kv",
    "host_tensor_argmax_i8_kv",
    "host_tensor_top1_i8_kv",
    "host_tensor_session_has_q24",
    "host_tensor_session_reset_q24",
    "host_tensor_session_append_kv_q24",
    "host_tensor_session_attention_kv_q24",
    "host_tensor_load_i8_kv_q24",
    "host_tensor_rmsnorm_q24",
    "host_tensor_silu_q24",
    "host_tensor_elemwise_mul_q24",
    "host_tensor_residual_add_q24",
    "host_tensor_rope_apply_q24",
    "host_tensor_linear_i8_kv_q24",
    "host_tensor_top1_i8_kv_q24",
];

const UPDATE_DISABLED_IMPORTS: &[&str] = &[
    "host_session_get_len",
    "host_session_get",
    "host_session_put",
    "host_tensor_session_has",
    "host_tensor_session_reset",
    "host_tensor_load_i8_kv",
    "host_tensor_matmul_fp",
    "host_tensor_rmsnorm_fp",
    "host_tensor_silu_fp",
    "host_tensor_elemwise_mul_fp",
    "host_tensor_residual_add_fp",
    "host_tensor_rope_apply_fp",
    "host_tensor_attention_kv_fp",
    "host_tensor_append_vec_fp",
    "host_tensor_argmax_fp",
    "host_tensor_linear_i8_kv",
    "host_tensor_argmax_i8_kv",
    "host_tensor_top1_i8_kv",
    "host_tensor_session_append_kv_fp",
    "host_tensor_session_attention_kv_fp",
];

const COMPUTE_IMPORTS: &[&str] = &[
    "host_tensor_session_has_q24",
    "host_tensor_session_reset_q24",
    "host_tensor_session_append_kv_q24",
    "host_tensor_session_attention_kv_q24",
    "host_tensor_load_i8_kv_q24",
    "host_tensor_rmsnorm_q24",
    "host_tensor_silu_q24",
    "host_tensor_elemwise_mul_q24",
    "host_tensor_residual_add_q24",
    "host_tensor_rope_apply_q24",
    "host_tensor_linear_i8_kv_q24",
    "host_tensor_top1_i8_kv_q24",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionProfile {
    Standard,
    Manifest,
    Compute,
}

impl ExecutionProfile {
    fn of_payload(payload: &Payload) -> Result<Self, String> {
        match payload.execution_profile.as_deref().unwrap_or("standard") {
            "standard" => Ok(Self::Standard),
            "manifest" => Ok(Self::Manifest),
            "compute" => Ok(Self::Compute),
            _ => Err("invalid wasm execution profile".to_owned()),
        }
    }

    fn max_guest_bytes(self) -> usize {
        match self {
            Self::Standard => MAX_GUEST_READ_BYTES.min(MAX_GUEST_WRITE_BYTES),
            Self::Manifest => MAX_GUEST_READ_BYTES.min(MAX_GUEST_WRITE_BYTES),
            Self::Compute => COMPUTE_MAX_GUEST_BYTES,
        }
    }

    fn max_tensor_session_bytes(self) -> usize {
        match self {
            Self::Standard => MAX_TENSOR_SESSION_BYTES,
            Self::Manifest => MAX_TENSOR_SESSION_BYTES,
            Self::Compute => COMPUTE_MAX_TENSOR_SESSION_BYTES,
        }
    }

    fn max_initial_pages(self) -> usize {
        match self {
            Self::Standard => MAX_INITIAL_PAGES,
            Self::Manifest => COMPUTE_MAX_INITIAL_PAGES,
            Self::Compute => COMPUTE_MAX_INITIAL_PAGES,
        }
    }

    fn fuel(self, requested: Option<u64>) -> u64 {
        match self {
            Self::Standard => requested.unwrap_or(DEFAULT_FUEL_LIMIT).min(MAX_FUEL_LIMIT),
            Self::Manifest => requested.unwrap_or(DEFAULT_FUEL_LIMIT).min(MAX_FUEL_LIMIT),
            Self::Compute => requested
                .unwrap_or(COMPUTE_DEFAULT_FUEL_LIMIT)
                .min(COMPUTE_MAX_FUEL_LIMIT),
        }
    }
}

#[derive(Debug, Deserialize)]
struct Payload<'a> {
    action: Option<String>,
    #[serde(borrow)]
    code_b64: Option<TextRef<'a>>,
    code_cache_key: Option<String>,
    export_name: Option<String>,
    #[serde(borrow)]
    request_b64: Option<TextRef<'a>>,
    storage_cache_key: Option<String>,
    storage_pairs: Option<Vec<StoragePairJson>>,
    caller: Option<String>,
    address: Option<String>,
    tx_hash: Option<String>,
    current_epoch: Option<i64>,
    hfhe_caps: Option<Vec<String>>,
    hfhe_pubkeys: Option<Vec<HfhePubkeyJson>>,
    hfhe_active_key: Option<HfheActiveKey>,
    hfhe_strict: Option<bool>,
    hfhe_pairs: Option<bool>,
    math: Option<bool>,
    float_mode: Option<bool>,
    hfhe_receipt_mode: Option<String>,
    hfhe_receipt_entries: Option<Vec<HfheReceiptEntryJson>>,
    public_reads: Option<Vec<PublicReadJson>>,
    is_view: Option<bool>,
    update_policy: Option<bool>,
    inspect_policy: Option<bool>,
    fuel_limit: Option<u64>,
    guest_bytes: Option<usize>,
    execution_profile: Option<String>,
    compute_storage_cache_key: Option<String>,
    compute_storage_key: Option<String>,
    #[serde(borrow)]
    compute_storage_value_b64: Option<TextRef<'a>>,
    compute_storage_root: Option<String>,
    compute_storage_entries: Option<usize>,
    compute_storage_bytes: Option<usize>,
    compute_session_scope: Option<String>,
    #[serde(default)]
    memory_bytes: Option<usize>,
}

#[derive(Debug, Deserialize)]
#[serde(transparent)]
struct TextRef<'a>(#[serde(borrow)] Cow<'a, str>);

impl std::ops::Deref for TextRef<'_> {
    type Target = str;

    fn deref(&self) -> &str {
        self.0.as_ref()
    }
}

#[derive(Debug, Clone, Deserialize)]
struct StoragePairJson {
    key_b64: String,
    value_b64: String,
}

#[derive(Debug, Clone, Deserialize)]
struct HfhePubkeyJson {
    addr: String,
    pubkey_b64: String,
}

#[derive(Debug, Clone, Deserialize)]
struct HfheActiveKey {
    key_id: String,
    pubkey_b64: String,
    seckey_b64: String,
}

#[derive(Debug, Clone, Deserialize, Serialize, PartialEq, Eq)]
struct HfheReceiptEntryJson {
    method: String,
    request_hash: String,
    response_hash: String,
    result: Option<bool>,
}

#[derive(Debug, Clone, Deserialize)]
struct PublicReadJson {
    snapshot_root: String,
    circle_id: String,
    canonical_path: String,
    offset: usize,
    max_bytes: usize,
    metadata_json: String,
    body_b64: String,
}

#[derive(Debug)]
struct PublicRead {
    metadata_json: String,
    body_b64: String,
    body_bytes: usize,
}

#[derive(Debug, Clone, Serialize)]
struct ExportInfoJson {
    name: String,
    kind: String,
}

#[derive(Debug, Serialize)]
struct EventJson {
    topic_b64: String,
    data_b64: String,
}

#[derive(Debug, Clone, Serialize)]
struct SpawnJson {
    circle_id: String,
    owner_mode: String,
    spawn_nonce: i32,
    payload_json: String,
}

#[derive(Debug, Clone, Serialize)]
struct AssetPutJson {
    circle_id: String,
    path: String,
    content_type: String,
    encoding: Option<String>,
    body_b64: String,
}

#[derive(Debug, Clone, Serialize)]
struct EncryptedAssetPutJson {
    circle_id: String,
    path: Option<String>,
    slot_ref: Option<String>,
    state_ref: Option<String>,
    content_type: String,
    encoding: Option<String>,
    key_id: String,
    plaintext_hash: String,
    padding_class: Option<String>,
    activate_after_epoch: Option<String>,
    expire_after_epoch: Option<String>,
    metadata_mode: Option<String>,
    ciphertext_b64: String,
}

#[derive(Debug)]
struct HostState {
    limits: StoreLimits,
    guest: Option<native_bytes::Lease>,
    guest_prior: usize,
    execution_profile: ExecutionProfile,
    max_guest_bytes: usize,
    max_tensor_session_bytes: usize,
    storage: Arc<Storage>,
    _storage_hold: Option<native_bytes::Lease>,
    budget: Option<Arc<native_bytes::Budget>>,
    hfhe_caps: BTreeSet<String>,
    hfhe_pubkeys: HashMap<String, String>,
    hfhe_active_key: Option<HfheActiveKey>,
    hfhe_strict: bool,
    hfhe_pairs: bool,
    math: bool,
    hfhe_receipt_mode: String,
    hfhe_receipt_expected: Vec<HfheReceiptEntryJson>,
    hfhe_receipt_entries: Vec<HfheReceiptEntryJson>,
    hfhe_receipt_index: usize,
    public_reads: BTreeMap<String, PublicRead>,
    public_read_bytes: usize,
    caller_bytes: Vec<u8>,
    self_bytes: Vec<u8>,
    tx_hash_bytes: Vec<u8>,
    current_epoch: i64,
    is_view: bool,
    response_bytes: native_bytes::Value,
    response_status: i32,
    events: Vec<EventJson>,
    event_bytes: usize,
    spawns: Vec<SpawnJson>,
    assets: Vec<AssetPutJson>,
    encrypted_assets: Vec<EncryptedAssetPutJson>,
    circle_invoke_cache: Option<(Vec<u8>, Arc<Vec<u8>>)>,
    calls: bool,
    call_request: Option<call_step::Request>,
    hfhe_invoke_cache: Option<(Vec<u8>, Arc<Vec<u8>>)>,
    unavailable: Option<String>,
    inspect_error: Option<String>,
    query_output: bool,
    local_storage: bool,
    local_sessions: bool,
    session_owner: Option<Arc<SessionOwner>>,
    compute_trace: Option<ComputeTrace>,
    compute_fault: Option<&'static str>,
    compute_session_scope: Option<String>,
    compute_started_at: Instant,
    q24_calls: usize,
}

impl ResourceLimiter for HostState {
    fn memory_growing(&mut self, current: usize, desired: usize, maximum: Option<usize>)
        -> Result<bool, MemoryError>
    {
        if !self.limits.memory_growing(current, desired, maximum)? {
            return Ok(false);
        }
        if let Some(guest) = self.guest.as_mut() {
            let result = if guest.size() != current {
                Err("guest page reservation differs")
            } else {
                guest.resize(desired)
            };
            if let Err(reason) = result {
                self.unavailable = Some(reason.to_owned());
                return Err(MemoryError::OutOfBoundsGrowth);
            }
            self.guest_prior = current;
        }
        Ok(true)
    }

    fn memory_grow_failed(&mut self, error: &MemoryError) {
        self.limits.memory_grow_failed(error);
        if let Some(guest) = self.guest.as_mut() {
            if let Err(reason) = guest.resize(self.guest_prior) {
                self.unavailable = Some(reason.to_owned());
            }
        }
    }

    fn table_growing(&mut self, current: u32, desired: u32, maximum: Option<u32>)
        -> Result<bool, TableError>
    {
        self.limits.table_growing(current, desired, maximum)
    }

    fn table_grow_failed(&mut self, error: &TableError) {
        self.limits.table_grow_failed(error);
    }

    fn instances(&self) -> usize {
        self.limits.instances()
    }

    fn tables(&self) -> usize {
        self.limits.tables()
    }

    fn memories(&self) -> usize {
        self.limits.memories()
    }
}

#[derive(Debug)]
struct ComputeTrace {
    hasher: Sha256,
    operations: u64,
}

impl ComputeTrace {
    fn new(program_root: &str, storage_root: Option<&str>, session_scope: Option<&str>) -> Self {
        let mut hasher = Sha256::new();
        hasher.update(COMPUTE_TRACE_DOMAIN);
        trace_put(&mut hasher, program_root.as_bytes());
        trace_put(&mut hasher, storage_root.unwrap_or("").as_bytes());
        trace_put(&mut hasher, session_scope.unwrap_or("").as_bytes());
        Self {
            hasher,
            operations: 0,
        }
    }

    fn begin(&mut self, export_name: &str, request_bytes: &[u8]) {
        trace_put(&mut self.hasher, b"request");
        trace_put(&mut self.hasher, export_name.as_bytes());
        trace_put(&mut self.hasher, &Sha256::digest(request_bytes));
    }

    fn record(&mut self, operation: &str, fields: &[&[u8]]) {
        self.operations = self.operations.saturating_add(1);
        trace_put(&mut self.hasher, operation.as_bytes());
        for field in fields {
            trace_put(&mut self.hasher, field);
        }
    }

    fn finish(&self, code: i32, response_bytes: &[u8], effort_used: u64) -> String {
        let mut hasher = self.hasher.clone();
        trace_put(&mut hasher, b"result");
        trace_put(&mut hasher, &code.to_le_bytes());
        trace_put(&mut hasher, &Sha256::digest(response_bytes));
        trace_put(&mut hasher, &effort_used.to_le_bytes());
        trace_put(&mut hasher, &self.operations.to_le_bytes());
        format!("{:x}", hasher.finalize())
    }
}

fn trace_put(hasher: &mut Sha256, value: &[u8]) {
    hasher.update((value.len() as u64).to_le_bytes());
    hasher.update(value);
}

fn trace_i64_digest(values: &[i64]) -> [u8; 32] {
    let mut hasher = Sha256::new();
    for value in values {
        hasher.update(value.to_le_bytes());
    }
    hasher.finalize().into()
}

fn record_compute_trace(caller: &mut Caller<'_, HostState>, operation: &str, fields: &[&[u8]]) {
    if let Some(trace) = caller.data_mut().compute_trace.as_mut() {
        trace.record(operation, fields);
    }
}

#[derive(Debug, Clone)]
struct SessionCacheEntry {
    value: Arc<native_bytes::Value>,
    updated_at: f64,
}

#[derive(Debug)]
struct TensorSessionEntry {
    n_layers: usize,
    max_t: usize,
    kv_dim: usize,
    k_cache: Vec<f64>,
    v_cache: Vec<f64>,
    _lease: Option<native_bytes::Lease>,
    updated_at: f64,
}

#[derive(Debug)]
struct FixedTensorSessionEntry {
    n_layers: usize,
    max_t: usize,
    kv_dim: usize,
    k_cache: Vec<i64>,
    v_cache: Vec<i64>,
    _lease: Option<native_bytes::Lease>,
    updated_at: f64,
}

#[derive(Debug, Clone)]
struct ModuleCacheEntry {
    module: Arc<Module>,
    exports: Vec<ExportInfoJson>,
    has_update_disabled_imports: bool,
    has_compute_imports: bool,
    updated_at: f64,
}

#[derive(Debug, Clone)]
struct StorageCacheEntry {
    storage: Arc<Storage>,
    weight: usize,
    updated_at: f64,
}

#[derive(Debug)]
struct ComputeStorageBuilder {
    storage: Storage,
    weight: usize,
}

impl ComputeStorageBuilder {
    fn next_weight(&self, key: &str, size: usize) -> Result<usize, String> {
        if let Some(current) = self.storage.get(key) {
            return if current.len() == size {
                Ok(self.weight)
            } else {
                Err("compute storage key differs".to_owned())
            };
        }
        if self.storage.len() >= COMPUTE_STORAGE_ENTRY_LIMIT {
            return Err("compute storage entry count exceeds limit".to_owned());
        }
        let next = self.weight.checked_add(size)
            .ok_or_else(|| "compute storage bytes overflow".to_owned())?;
        if next > COMPUTE_STORAGE_BYTE_LIMIT {
            return Err("compute storage bytes exceed limit".to_owned());
        }
        Ok(next)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BuildToken {
    key: String,
    id: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BuildPhase {
    Writing,
    Sealing,
    Closing,
}

#[derive(Debug, Clone, Default)]
struct BuildOwner {
    slots: BTreeMap<String, (u64, BuildPhase)>,
    next: u64,
    failed: bool,
}

enum BuildMessage {
    Begin(String),
    Check(BuildToken, BuildPhase),
    Seal(BuildToken),
    Close(BuildToken),
    Finish(BuildToken),
    Stop,
}

enum BuildReply {
    Begun(BuildToken),
    Checked,
    Take,
    Retire(bool),
    Finished,
}

impl BuildOwner {
    fn delta(&self, message: BuildMessage) -> Result<(Self, BuildReply), String> {
        if self.failed && !matches!(&message, BuildMessage::Close(_) | BuildMessage::Finish(_) | BuildMessage::Stop) {
            return Err("compute storage builder unavailable".to_owned());
        }
        let mut state = self.clone();
        let reply = match message {
            BuildMessage::Begin(key) => {
                if self.slots.contains_key(&key) {
                    return Err("compute storage builder busy".to_owned());
                }
                if self.slots.len() >= COMPUTE_STORAGE_BUILDER_LIMIT {
                    return Err("compute storage builders busy".to_owned());
                }
                if self.next >= i64::MAX as u64 {
                    return Err("compute storage generation exhausted".to_owned());
                }
                state.next += 1;
                state.slots.insert(key.clone(), (state.next, BuildPhase::Writing));
                BuildReply::Begun(BuildToken { key, id: state.next })
            }
            BuildMessage::Check(token, phase) => {
                if self.slots.get(&token.key) != Some(&(token.id, phase)) {
                    return Err("compute storage builder changed".to_owned());
                }
                BuildReply::Checked
            }
            BuildMessage::Seal(token) => {
                if self.slots.get(&token.key) != Some(&(token.id, BuildPhase::Writing)) {
                    return Err("compute storage builder changed".to_owned());
                }
                state.slots.insert(token.key, (token.id, BuildPhase::Sealing));
                BuildReply::Take
            }
            BuildMessage::Close(token) => {
                let phase = self.slots.get(&token.key).copied();
                let retire = matches!(phase, Some((id, BuildPhase::Writing)) if id == token.id);
                if matches!(phase, Some((id, _)) if id == token.id) {
                    state.slots.insert(token.key, (token.id, BuildPhase::Closing));
                }
                BuildReply::Retire(retire)
            }
            BuildMessage::Finish(token) => {
                if matches!(self.slots.get(&token.key), Some((id, phase))
                    if *id == token.id && *phase != BuildPhase::Writing)
                {
                    state.slots.remove(&token.key);
                }
                BuildReply::Finished
            }
            BuildMessage::Stop => {
                state.failed = true;
                BuildReply::Finished
            }
        };
        Ok((state, reply))
    }
}

#[derive(Default)]
struct BuildTable {
    owner: BuildOwner,
    data: BTreeMap<String, ComputeStorageBuilder>,
}

impl BuildTable {
    fn call(&mut self, message: BuildMessage) -> Result<BuildReply, String> {
        let (state, reply) = self.owner.delta(message)?;
        self.owner = state;
        Ok(reply)
    }
}

pub struct ComputeBuild {
    port: Mutex<BuildPort>,
}

struct BuildPort {
    token: Option<BuildToken>,
    closed: bool,
}

fn build_finish(token: &BuildToken) {
    let mut table = match compute_storage_builders().lock() {
        Ok(table) => table,
        Err(poisoned) => poisoned.into_inner(),
    };
    let _ = table.call(BuildMessage::Finish(token.clone()));
}

fn retire_loop(receiver: Receiver<BuildToken>) {
    for token in receiver {
        let data = {
            let mut table = match compute_storage_builders().lock() {
                Ok(table) => table,
                Err(poisoned) => poisoned.into_inner(),
            };
            if table.owner.slots.get(&token.key) == Some(&(token.id, BuildPhase::Closing)) {
                table.data.remove(&token.key)
            } else {
                None
            }
        };
        drop(data);
        build_finish(&token);
    }
}

fn retire_sender() -> &'static Mutex<Option<SyncSender<BuildToken>>> {
    static SENDER: OnceLock<Mutex<Option<SyncSender<BuildToken>>>> = OnceLock::new();
    SENDER.get_or_init(|| Mutex::new(None))
}

fn retire_ready() -> Result<(), String> {
    let mut sender = retire_sender().lock()
        .map_err(|_| "compute storage retirement poisoned".to_owned())?;
    if sender.is_none() {
        let (next, receiver) = sync_channel(COMPUTE_STORAGE_BUILDER_LIMIT);
        std::thread::Builder::new().name("compute-retire".to_owned())
            .spawn(move || retire_loop(receiver))
            .map_err(|_| "compute storage retirement unavailable".to_owned())?;
        *sender = Some(next);
    }
    Ok(())
}

impl ComputeBuild {
    fn close(&self) {
        let token = match self.port.lock() {
            Ok(mut port) => {
                port.closed = true;
                port.token.take()
            }
            Err(poisoned) => {
                let mut port = poisoned.into_inner();
                port.closed = true;
                port.token.take()
            }
        };
        if let Some(token) = token {
            let retire = {
                let mut table = match compute_storage_builders().lock() {
                    Ok(table) => table,
                    Err(poisoned) => poisoned.into_inner(),
                };
                matches!(table.call(BuildMessage::Close(token.clone())), Ok(BuildReply::Retire(true)))
            };
            if retire {
                let delivered = {
                    let sender = match retire_sender().lock() {
                        Ok(sender) => sender,
                        Err(poisoned) => poisoned.into_inner(),
                    };
                    sender.as_ref().map(|sender| sender.try_send(token).is_ok()).unwrap_or(false)
                };
                if !delivered {
                    let mut table = match compute_storage_builders().lock() {
                        Ok(table) => table,
                        Err(poisoned) => poisoned.into_inner(),
                    };
                    let _ = table.call(BuildMessage::Stop);
                }
            }
        }
    }

    fn token(&self) -> Result<BuildToken, String> {
        self.port.lock().map_err(|_| "compute storage port poisoned".to_owned())?
            .token.clone().ok_or_else(|| "compute storage port closed".to_owned())
    }
}

impl Drop for ComputeBuild {
    fn drop(&mut self) {
        self.close();
    }
}

struct BuildWork {
    token: BuildToken,
    data: Option<ComputeStorageBuilder>,
}

impl Drop for BuildWork {
    fn drop(&mut self) {
        drop(self.data.take());
        build_finish(&self.token);
    }
}

#[no_mangle]
pub extern "C" fn octra_compute_build_new() -> *mut Arc<ComputeBuild> {
    Box::into_raw(Box::new(Arc::new(ComputeBuild {
        port: Mutex::new(BuildPort { token: None, closed: false }),
    })))
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_build_retain(port: *const Arc<ComputeBuild>) -> *mut Arc<ComputeBuild> {
    if port.is_null() { std::ptr::null_mut() }
    else { Box::into_raw(Box::new((*port).clone())) }
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_build_release(port: *mut Arc<ComputeBuild>) {
    if !port.is_null() { drop(Box::from_raw(port)); }
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_build_close(port: *mut Arc<ComputeBuild>) {
    if !port.is_null() { (*port).close(); }
}

fn build_start(
    port: &ComputeBuild, key: &str, ready: impl FnOnce() -> Result<(), String>,
) -> Result<(), String> {
    normalize_hex64(key, "compute storage cache key")?;
    let mut port = port.port.lock().map_err(|_| "compute storage port poisoned".to_owned())?;
    if port.closed || port.token.is_some() { return Err("compute storage port used".to_owned()); }
    ready()?;
    let mut table = compute_storage_builders().lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    let reply = table.call(BuildMessage::Begin(key.to_owned()))?;
    match reply {
        BuildReply::Begun(selected) => {
            table.data.insert(key.to_owned(), ComputeStorageBuilder {
                storage: Storage::default(), weight: 0,
            });
            port.token = Some(selected);
            Ok(())
        }
        _ => Err("compute storage begin reply differs".to_owned()),
    }
}

fn build_begin(port: &ComputeBuild, key: &str) -> Result<(), String> {
    build_start(port, key, retire_ready)
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_build_begin(
    port: *const Arc<ComputeBuild>, input: *const u8, length: usize,
) -> i32 {
    if port.is_null() || input.is_null() || length > 64 { return 5; }
    let result = std::panic::catch_unwind(|| {
        let key = std::str::from_utf8(slice::from_raw_parts(input, length))
            .map_err(|_| "compute storage key input refused".to_owned())?;
        build_begin(&*(*port), key)
    });
    match result {
        Ok(Ok(())) => 0,
        Ok(Err(reason)) => match reason.as_str() {
            "compute storage builder busy" => 1,
            "compute storage builders busy" => 2,
            "compute storage generation exhausted" => 3,
            "compute storage port used" => 4,
            "compute storage builder poisoned" | "compute storage port poisoned" => 6,
            "compute storage retirement unavailable" | "compute storage retirement poisoned" => 6,
            "compute storage builder unavailable" => 6,
            _ => 5,
        },
        Err(_) => 6,
    }
}

#[no_mangle]
pub unsafe extern "C" fn octra_circle_input_take(
    size: usize, owner: *mut *mut std::ffi::c_void,
) -> i32 {
    if owner.is_null() { return 3; }
    *owner = std::ptr::null_mut();
    let result = std::panic::catch_unwind(|| {
        static POOL: OnceLock<Arc<native_bytes::Pool>> = OnceLock::new();
        POOL.get_or_init(|| native_bytes::Pool::new(LOCAL_INPUT_LIMIT)).take(size.max(1))
    });
    match result {
        Ok(Ok(lease)) => {
            let layout = std::alloc::Layout::new::<native_bytes::Lease>();
            let saved = std::alloc::alloc(layout).cast::<native_bytes::Lease>();
            if saved.is_null() { return 4; }
            saved.write(lease);
            *owner = saved.cast();
            0
        }
        Ok(Err("compute memory busy")) => 2,
        _ => 3,
    }
}

#[no_mangle]
pub unsafe extern "C" fn octra_circle_input_drop(owner: *mut std::ffi::c_void) {
    if !owner.is_null() {
        drop(Box::from_raw(owner.cast::<native_bytes::Lease>()));
    }
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_input_new(
    input: *const u8, length: usize, owner: *mut *mut std::ffi::c_void,
    bytes: *mut *const u8,
) -> i32 {
    if !owner.is_null() { *owner = std::ptr::null_mut(); }
    if !bytes.is_null() { *bytes = std::ptr::null(); }
    if owner.is_null() || bytes.is_null() || input.is_null()
        || length == 0 || length > MAX_GUEST_READ_BYTES
    {
        return 1;
    }
    let result = std::panic::catch_unwind(|| {
        copy_input(compute_memory(), slice::from_raw_parts(input, length))
    });
    match result {
        Ok(Ok(value)) => {
            let layout = std::alloc::Layout::new::<native_bytes::Value>();
            let saved = std::alloc::alloc(layout).cast::<native_bytes::Value>();
            if saved.is_null() {
                drop(value);
                return 4;
            }
            *bytes = value.as_ptr();
            saved.write(value);
            *owner = saved.cast();
            0
        }
        Ok(Err(reason)) if reason == "compute memory busy" => 2,
        Ok(Err(reason)) if reason == "compute input allocation failed" => 4,
        _ => 3,
    }
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_input_drop(owner: *mut std::ffi::c_void) {
    if !owner.is_null() {
        drop(Box::from_raw(owner.cast::<native_bytes::Value>()));
    }
}

fn copy_input(pool: &Arc<native_bytes::Pool>, input: &[u8]) -> Result<native_bytes::Value, &'static str> {
    let lease = pool.take(input.len())?;
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(input.len())
        .map_err(|_| "compute input allocation failed")?;
    bytes.extend_from_slice(input);
    native_bytes::Value::charged(bytes, lease)
}

fn payload_memory(input: &[u8]) -> Result<native_bytes::Lease, &'static str> {
    let unit = [
        std::mem::size_of::<Payload>(),
        std::mem::size_of::<StoragePairJson>(),
        std::mem::size_of::<HfhePubkeyJson>(),
        std::mem::size_of::<HfheReceiptEntryJson>(),
        std::mem::size_of::<PublicReadJson>(),
        std::mem::size_of::<String>(),
    ].into_iter().max().unwrap();
    parse_memory::take(input, compute_memory(), unit)
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_build_step(
    port: *const Arc<ComputeBuild>, input: *const u8, length: usize,
    output: *mut *mut u8, size: *mut usize,
) -> i32 {
    let result = std::panic::catch_unwind(|| -> Result<String, String> {
        if port.is_null() || input.is_null() || length > MAX_GUEST_READ_BYTES {
            return Err("compute storage input refused".to_owned());
        }
        let token = (*port).token()?;
        let input = slice::from_raw_parts(input, length);
        let _memory = payload_memory(input)?;
        let payload: Payload = serde_json::from_slice(input)
            .map_err(|_| "compute storage input refused".to_owned())?;
        let reply = match payload.action.as_deref() {
            Some("compute_storage_put") => run_compute_storage_put(&token, &payload)?,
            Some("compute_storage_seal") => run_compute_storage_seal(&token, &payload)?,
            _ => return Err("compute storage action refused".to_owned()),
        };
        serde_json::to_string(&reply).map_err(|_| "compute storage reply refused".to_owned())
    });
    let (code, body) = match result {
        Ok(Ok(body)) => (0, body),
        Ok(Err(reason)) => (1, reason),
        Err(_) => (2, "compute storage execution failed".to_owned()),
    };
    write_owned_bytes(body.into_bytes(), output, size);
    code
}

#[derive(Debug, Clone)]
struct ComputeStorageEntry {
    storage: Arc<Storage>,
    root: String,
    weight: usize,
    updated_at: f64,
}

#[derive(Debug, Clone, Default)]
struct ComputeCache {
    entries: HashMap<String, ComputeStorageEntry>,
    leases: BTreeMap<u64, String>,
    next: u64,
}

enum CacheMessage {
    Hold { key: String, root: String, entries: usize, bytes: usize },
    Release(u64),
    Drop(String),
    Seal(String, ComputeStorageEntry),
    Read(String, f64),
}

enum CacheReply {
    Held(u64),
    Released,
    Missing,
    Busy,
    Ready(ComputeStorageEntry),
}

pub struct ComputeLease {
    id: Option<u64>,
}

impl Drop for ComputeLease {
    fn drop(&mut self) {
        if let Some(id) = self.id.take() {
            let mut owner = match compute_storage_cache().lock() {
                Ok(owner) => owner,
                Err(poisoned) => poisoned.into_inner(),
            };
            owner.leases.remove(&id);
        }
    }
}

#[no_mangle]
pub extern "C" fn octra_compute_lease_new() -> *mut ComputeLease {
    Box::into_raw(Box::new(ComputeLease { id: None }))
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_lease_drop(port: *mut ComputeLease) {
    if !port.is_null() {
        drop(Box::from_raw(port));
    }
}

#[no_mangle]
pub unsafe extern "C" fn octra_compute_lease_hold(
    port: *mut ComputeLease, input: *const u8, length: usize,
) -> i32 {
    if port.is_null() || input.is_null() || length > 4096 {
        return 5;
    }
    if (*port).id.is_some() {
        return 5;
    }
    let result = std::panic::catch_unwind(|| {
        let input = slice::from_raw_parts(input, length);
        let _memory = payload_memory(input)?;
        let payload: Payload = serde_json::from_slice(input)
            .map_err(|_| "compute storage hold input refused".to_owned())?;
        let key = compute_storage_key(&payload)?.to_owned();
        let root = payload.compute_storage_root
            .ok_or_else(|| "missing compute storage root".to_owned())?;
        normalize_hex64(&root, "compute storage root")?;
        let entries = payload.compute_storage_entries
            .ok_or_else(|| "missing compute storage entries".to_owned())?;
        let bytes = payload.compute_storage_bytes
            .ok_or_else(|| "missing compute storage bytes".to_owned())?;
        match compute_cache_call(CacheMessage::Hold { key, root, entries, bytes })? {
            CacheReply::Held(id) => {
                (*port).id = Some(id);
                Ok(())
            }
            _ => Err("compute storage hold reply differs".to_owned()),
        }
    });
    match result {
        Ok(Ok(())) => 0,
        Ok(Err(reason)) => match reason.as_str() {
            "compute storage cache missing" => 1,
            "compute storage cache identity differs" => 2,
            "compute storage leases busy" => 3,
            "compute storage generation exhausted" => 4,
            "compute storage cache poisoned" | "compute memory busy"
            | "compute memory owner failed" => 6,
            _ => 5,
        },
        Err(_) => 6,
    }
}

impl ComputeCache {
    fn held(&self, key: &str) -> bool {
        self.leases.values().any(|value| value == key)
    }

    fn delta(&self, message: CacheMessage) -> Result<(Self, CacheReply), String> {
        let mut state = self.clone();
        let reply = match message {
            CacheMessage::Hold { key, root, entries, bytes } => {
                let entry = self.entries.get(&key)
                    .ok_or_else(|| "compute storage cache missing".to_owned())?;
                if entry.root != root || entry.storage.len() != entries || entry.weight != bytes {
                    return Err("compute storage cache identity differs".to_owned());
                }
                if self.leases.len() >= COMPUTE_STORAGE_LEASE_LIMIT {
                    return Err("compute storage leases busy".to_owned());
                }
                if self.next >= i64::MAX as u64 {
                    return Err("compute storage generation exhausted".to_owned());
                }
                state.next = self.next + 1;
                state.leases.insert(state.next, key);
                CacheReply::Held(state.next)
            }
            CacheMessage::Release(id) => {
                state.leases.remove(&id);
                CacheReply::Released
            }
            CacheMessage::Drop(key) => {
                if self.held(&key) {
                    CacheReply::Busy
                } else {
                    state.entries.remove(&key);
                    CacheReply::Missing
                }
            }
            CacheMessage::Seal(key, entry) => {
                if self.held(&key) {
                    let current = self.entries.get(&key)
                        .ok_or_else(|| "compute storage held entry missing".to_owned())?;
                    if current.root != entry.root || current.storage.len() != entry.storage.len()
                        || current.weight != entry.weight
                    {
                        return Err("compute storage cache busy".to_owned());
                    }
                    CacheReply::Ready(current.clone())
                } else {
                    if !self.entries.contains_key(&key)
                        && self.entries.len() >= COMPUTE_STORAGE_CACHE_LIMIT
                    {
                        let victim = self.entries.iter()
                            .filter(|(key, _)| !self.held(key))
                            .min_by(|left, right| {
                                left.1.updated_at.total_cmp(&right.1.updated_at)
                                    .then_with(|| left.0.cmp(right.0))
                            })
                            .map(|(key, _)| key.clone())
                            .ok_or_else(|| "compute storage cache busy".to_owned())?;
                        state.entries.remove(&victim);
                    }
                    state.entries.insert(key, entry.clone());
                    CacheReply::Ready(entry)
                }
            }
            CacheMessage::Read(key, now) => match state.entries.get_mut(&key) {
                Some(entry) => {
                    entry.updated_at = now;
                    CacheReply::Ready(entry.clone())
                }
                None => CacheReply::Missing,
            },
        };
        Ok((state, reply))
    }
}

fn storage_cache(local: bool) -> &'static Mutex<HashMap<String, StorageCacheEntry>> {
    static STORAGE_CACHE: OnceLock<Mutex<HashMap<String, StorageCacheEntry>>> = OnceLock::new();
    static LOCAL_STORAGE_CACHE: OnceLock<Mutex<HashMap<String, StorageCacheEntry>>> = OnceLock::new();
    if local {
        LOCAL_STORAGE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
    } else {
        STORAGE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
    }
}

fn cache_guard<T: Default>(cache: &Mutex<T>) -> MutexGuard<'_, T> {
    match cache.lock() {
        Ok(guard) => guard,
        Err(poisoned) => {
            let mut guard = poisoned.into_inner();
            *guard = T::default();
            cache.clear_poison();
            guard
        }
    }
}

fn compute_storage_builders() -> &'static Mutex<BuildTable> {
    static COMPUTE_STORAGE_BUILDERS: OnceLock<Mutex<BuildTable>> =
        OnceLock::new();
    COMPUTE_STORAGE_BUILDERS.get_or_init(|| Mutex::new(BuildTable::default()))
}

fn compute_storage_cache() -> &'static Mutex<ComputeCache> {
    static COMPUTE_STORAGE_CACHE: OnceLock<Mutex<ComputeCache>> =
        OnceLock::new();
    COMPUTE_STORAGE_CACHE.get_or_init(|| Mutex::new(ComputeCache::default()))
}

fn compute_cache_call(message: CacheMessage) -> Result<CacheReply, String> {
    let mut owner = compute_storage_cache().lock()
        .map_err(|_| "compute storage cache poisoned".to_owned())?;
    let (state, reply) = owner.delta(message)?;
    let retired = std::mem::replace(&mut *owner, state);
    drop(owner);
    drop(retired);
    Ok(reply)
}

fn session_cache() -> &'static Mutex<HashMap<String, SessionCacheEntry>> {
    static SESSION_CACHE: OnceLock<Mutex<HashMap<String, SessionCacheEntry>>> = OnceLock::new();
    SESSION_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn local_session_cache() -> &'static Mutex<HashMap<String, SessionCacheEntry>> {
    static CACHE: OnceLock<Mutex<HashMap<String, SessionCacheEntry>>> = OnceLock::new();
    CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn local_session_pool() -> &'static Arc<native_bytes::Pool> {
    static POOL: OnceLock<Arc<native_bytes::Pool>> = OnceLock::new();
    POOL.get_or_init(|| compute_memory().child(LOCAL_SESSION_BYTES))
}

fn session_store(local: bool) -> &'static Mutex<HashMap<String, SessionCacheEntry>> {
    if local { local_session_cache() } else { session_cache() }
}

#[derive(Debug)]
struct SessionOwner {
    modules: Mutex<HashMap<String, ModuleCacheEntry>>,
    values: Mutex<HashMap<String, SessionCacheEntry>>,
    tensors: Mutex<HashMap<String, TensorSessionEntry>>,
    fixed: Mutex<HashMap<String, FixedTensorSessionEntry>>,
    pool: Arc<native_bytes::Pool>,
}

impl Default for SessionOwner {
    fn default() -> Self {
        Self { modules: Mutex::new(HashMap::new()), values: Mutex::new(HashMap::new()), tensors: Mutex::new(HashMap::new()),
            fixed: Mutex::new(HashMap::new()), pool: compute_memory().child(LOCAL_SESSION_BYTES) }
    }
}

fn value_table(owner: &Option<Arc<SessionOwner>>, local: bool)
    -> &Mutex<HashMap<String, SessionCacheEntry>>
{
    owner.as_ref().map_or_else(|| session_store(local), |value| &value.values)
}

fn tensor_table(owner: &Option<Arc<SessionOwner>>, local: bool)
    -> &Mutex<HashMap<String, TensorSessionEntry>>
{
    owner.as_ref().map_or_else(|| tensor_store(local), |value| &value.tensors)
}

fn fixed_table(owner: &Option<Arc<SessionOwner>>, local: bool)
    -> &Mutex<HashMap<String, FixedTensorSessionEntry>>
{
    owner.as_ref().map_or_else(|| fixed_tensor_store(local), |value| &value.fixed)
}

#[cfg(test)]
fn session_value(key: &str, local: bool) -> Option<Arc<native_bytes::Value>> {
    let mut guard = cache_guard(session_store(local));
    prune_session_cache_locked(&mut guard);
    guard.get(key).map(|entry| entry.value.clone())
}

fn tensor_session_cache() -> &'static Mutex<HashMap<String, TensorSessionEntry>> {
    static TENSOR_SESSION_CACHE: OnceLock<Mutex<HashMap<String, TensorSessionEntry>>> =
        OnceLock::new();
    TENSOR_SESSION_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn fixed_tensor_session_cache() -> &'static Mutex<HashMap<String, FixedTensorSessionEntry>> {
    static FIXED_TENSOR_SESSION_CACHE: OnceLock<Mutex<HashMap<String, FixedTensorSessionEntry>>> =
        OnceLock::new();
    FIXED_TENSOR_SESSION_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn tensor_store(local: bool) -> &'static Mutex<HashMap<String, TensorSessionEntry>> {
    static STORE: OnceLock<Mutex<HashMap<String, TensorSessionEntry>>> = OnceLock::new();
    if local { STORE.get_or_init(|| Mutex::new(HashMap::new())) } else { tensor_session_cache() }
}

fn fixed_tensor_store(local: bool) -> &'static Mutex<HashMap<String, FixedTensorSessionEntry>> {
    static STORE: OnceLock<Mutex<HashMap<String, FixedTensorSessionEntry>>> = OnceLock::new();
    if local { STORE.get_or_init(|| Mutex::new(HashMap::new())) } else { fixed_tensor_session_cache() }
}

fn tensor_memory<T: Clone>(total: usize, key_bytes: usize, zero: T, quota: Option<&Arc<native_bytes::Budget>>)
    -> Option<(Vec<T>, Vec<T>, native_bytes::Lease)> {
    let size = total.checked_mul(std::mem::size_of::<T>())?;
    let lease = compute_memory().take_in(size.checked_mul(2)?.checked_add(key_bytes)?, quota).ok()?;
    let mut keys = Vec::new();
    keys.try_reserve_exact(total).ok()?;
    let mut values = Vec::new();
    values.try_reserve_exact(total).ok()?;
    if keys.capacity() != total || values.capacity() != total {
        return None;
    }
    keys.resize(total, zero.clone());
    values.resize(total, zero);
    Some((keys, values, lease))
}

fn tensor_hold(quota: Option<&Arc<native_bytes::Budget>>, lease: Option<&native_bytes::Lease>,
    keys: usize, values: usize, key_bytes: usize) -> Result<Option<native_bytes::Lease>, HostFailure>
{
    let size = keys.checked_add(values).and_then(|n| n.checked_mul(8))
        .and_then(|n| n.checked_add(key_bytes))
        .ok_or_else(|| HostFailure::Unavailable("tensor capacity overflow".to_owned()))?;
    native_bytes::Lease::borrow(quota, lease, size)
        .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))
}

fn shared_engine() -> &'static Engine {
    static SHARED_ENGINE: OnceLock<Engine> = OnceLock::new();
    SHARED_ENGINE.get_or_init(|| {
        let mut config = Config::default();
        config.consume_fuel(true);
        Engine::new(&config)
    })
}

fn module_cache() -> &'static Mutex<HashMap<String, ModuleCacheEntry>> {
    static MODULE_CACHE: OnceLock<Mutex<HashMap<String, ModuleCacheEntry>>> = OnceLock::new();
    MODULE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn now_secs() -> f64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|value| value.as_secs_f64())
        .unwrap_or(0.0)
}

#[derive(Debug)]
struct Runtime {
    store: Store<HostState>,
    instance: Instance,
    exports: Vec<ExportInfoJson>,
    fuel_limit: u64,
    has_update_disabled_imports: bool,
    update_policy: bool,
    has_compute_imports: bool,
}

#[derive(Debug)]
struct RequestFrame {
    method: String,
    params: Vec<FrameValue>,
}

#[derive(Debug)]
enum HostFailure {
    Rejected(String),
    Unavailable(String),
}

impl From<String> for HostFailure {
    fn from(error: String) -> Self {
        Self::Rejected(error)
    }
}

impl From<&'static str> for HostFailure {
    fn from(error: &'static str) -> Self {
        Self::Rejected(error.to_owned())
    }
}

impl std::fmt::Display for HostFailure {
    fn fmt(&self, output: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Rejected(reason) | Self::Unavailable(reason) => output.write_str(reason),
        }
    }
}

fn rejected<T>(result: Result<T, String>) -> Result<T, HostFailure> {
    result.map_err(HostFailure::Rejected)
}

fn host_failure(caller: &mut Caller<'_, HostState>, failure: HostFailure) -> WasmiError {
    match failure {
        HostFailure::Rejected(reason) => WasmiError::new(reason),
        HostFailure::Unavailable(reason) => {
            caller.data_mut().unavailable = Some(reason);
            WasmiError::new("host unavailable")
        }
    }
}

#[derive(Debug, Clone)]
enum FrameValue {
    Null,
    Bool(bool),
    Int(String),
    String(String),
}

#[cfg(not(test))]
extern "C" {
    fn octra_circle_wasm_host_hfhe_call_json(
        input_ptr: *const u8,
        input_len: usize,
        out_ptr: *mut *mut u8,
        out_len: *mut usize,
        err_ptr: *mut *mut u8,
        err_len: *mut usize,
    ) -> i32;
}

#[cfg(test)]
unsafe fn octra_circle_wasm_host_hfhe_call_json(
    _input_ptr: *const u8,
    _input_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
    err_ptr: *mut *mut u8,
    err_len: *mut usize,
) -> i32 {
    if !out_ptr.is_null() {
        *out_ptr = std::ptr::null_mut();
    }
    if !out_len.is_null() {
        *out_len = 0;
    }
    if !err_ptr.is_null() {
        *err_ptr = std::ptr::null_mut();
    }
    if !err_len.is_null() {
        *err_len = 0;
    }
    1
}

#[no_mangle]
pub extern "C" fn octra_circle_wasm_host_run_json(
    input_ptr: *const u8,
    input_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
    err_ptr: *mut *mut u8,
    err_len: *mut usize,
    owner: *mut *mut std::ffi::c_void,
) -> i32 {
    if !owner.is_null() { unsafe { *owner = std::ptr::null_mut(); } }
    let result = std::panic::catch_unwind(|| {
        let output = run_reply(input_ptr, input_len)?;
        if output.is_charged() {
            if owner.is_null() || out_ptr.is_null() || out_len.is_null() {
                return Err(HostFailure::Unavailable("reply owner missing".to_owned()));
            }
            let (saved, bytes, size) = output.into_foreign()
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
            unsafe { *owner = saved; *out_ptr = bytes; *out_len = size; }
        } else {
            unsafe { write_owned_bytes(output.into_plain(), out_ptr, out_len); }
        }
        Ok::<_, HostFailure>(())
    });
    match result {
        Ok(Ok(())) => unsafe {
            if !err_ptr.is_null() {
                *err_ptr = std::ptr::null_mut();
            }
            if !err_len.is_null() {
                *err_len = 0;
            }
            0
        },
        Ok(Err(error)) => unsafe {
            let status = match &error {
                HostFailure::Rejected(_) => 1,
                HostFailure::Unavailable(_) => 2,
            };
            write_owned_bytes(error.to_string().into_bytes(), err_ptr, err_len);
            if !out_ptr.is_null() {
                *out_ptr = std::ptr::null_mut();
            }
            if !out_len.is_null() {
                *out_len = 0;
            }
            status
        },
        Err(_) => unsafe {
            write_owned_bytes(b"octra_circle_wasm_host: panic".to_vec(), err_ptr, err_len);
            if !out_ptr.is_null() {
                *out_ptr = std::ptr::null_mut();
            }
            if !out_len.is_null() {
                *out_len = 0;
            }
            2
        },
    }
}

#[no_mangle]
pub extern "C" fn octra_circle_wasm_host_free_bytes(ptr: *mut u8, len: usize) {
    if ptr.is_null() || len == 0 {
        return;
    }
    unsafe {
        native_bytes::free_bytes(ptr, len);
    }
}

fn run_reply(input_ptr: *const u8, input_len: usize) -> Result<native_bytes::Value, HostFailure> {
    if input_ptr.is_null() && input_len != 0 {
        return Err(HostFailure::Rejected("octra_circle_wasm_host: null input".to_owned()));
    }
    let input = if input_len == 0 {
        &[]
    } else {
        unsafe { slice::from_raw_parts(input_ptr, input_len) }
    };
    let input_text = std::str::from_utf8(input)
        .map_err(|e| format!("octra_circle_wasm_host: invalid utf8 input: {e}"))?;
    let payload: Payload = serde_json::from_str(input_text)
        .map_err(|e| format!("octra_circle_wasm_host: invalid json input: {e}"))?;
    let action = payload.action.as_deref().unwrap_or("");
    let local = payload.is_view == Some(true)
        && (payload.inspect_policy.unwrap_or(false)
            || payload.execution_profile.as_deref() == Some("compute"));
    if local && action == "execute" {
        let (mut runtime, request, export) = prepare_execution(&payload)?;
        let call = finish_call(&mut runtime, export, &request).map_err(HostFailure::Rejected)?;
        return reply::execute(&runtime, &call);
    }
    let output = match action {
        "validate" => run_validate(&payload)?,
        "describe" => run_describe(&payload)?,
        "execute" => run_execute(&payload)?,
        "compute_replay" if local => compute_replay::run(input_text)?,
        "compute_storage_status" => run_compute_storage_status(&payload)?,
        "compute_storage_drop" => run_compute_storage_drop(&payload)?,
        "compute_self_test" => run_compute_self_test()?,
        _ => {
            return Err(HostFailure::Rejected(format!(
                "octra_circle_wasm_host: unsupported action: {}",
                if action.is_empty() { "<none>" } else { action }
            )));
        }
    };
    reply::encode(&output, local.then(compute_memory))
}

#[cfg(test)]
fn run_json(input: *const u8, size: usize) -> Result<String, HostFailure> {
    run_reply(input, size).map(|value| String::from_utf8(value.as_slice().to_vec()).unwrap())
}

fn self_test_i64(hasher: &mut Sha256, values: &[i64]) {
    hasher.update((values.len() as u64).to_be_bytes());
    for value in values {
        hasher.update(value.to_be_bytes());
    }
}

fn run_compute_self_test() -> Result<JsonValue, String> {
    deterministic_tensor::verify_thread_invariance().map_err(str::to_owned)?;
    let one = deterministic_tensor::ONE;
    let half = deterministic_tensor::HALF;
    let loaded = deterministic_tensor::load_i8(&[1, 2, 3, 4], half).map_err(str::to_owned)?;
    let linear = deterministic_tensor::linear_i8(&[one, 2 * one], &[1, 2, 3, 4], 2, half)
        .map_err(str::to_owned)?;
    let top1 = deterministic_tensor::top1_i8(&[one, 2 * one], &[1, 2, 3, 4], 2, half)
        .map_err(str::to_owned)?;
    let normalized =
        deterministic_tensor::rmsnorm(&[one, -one], &[one, one]).map_err(str::to_owned)?;
    let activated = deterministic_tensor::silu(&[-one, 0, one]).map_err(str::to_owned)?;
    let multiplied = deterministic_tensor::elementwise_mul(&[one, 2 * one], &[2 * one, half])
        .map_err(str::to_owned)?;
    let residual =
        deterministic_tensor::residual_add(&[one, 2 * one], &[one, -one]).map_err(str::to_owned)?;
    let rotated = deterministic_tensor::rope(&[one, 2 * one, 3 * one, 4 * one], &[one, half], 3)
        .map_err(str::to_owned)?;
    let attended = deterministic_tensor::attention_kv(
        &[one, 0],
        &[one, 0, 0, one],
        &[one, 2 * one, 3 * one, 4 * one],
        2,
        1,
        1,
        2,
        one,
    )
    .map_err(str::to_owned)?;
    let mut evidence = Sha256::new();
    evidence.update(b"octra:resource-compute-evidence:q24-v1\0");
    self_test_i64(&mut evidence, &loaded);
    self_test_i64(&mut evidence, &linear);
    self_test_i64(&mut evidence, &[top1.0 as i64, top1.1]);
    self_test_i64(&mut evidence, &normalized);
    self_test_i64(&mut evidence, &activated);
    self_test_i64(&mut evidence, &multiplied);
    self_test_i64(&mut evidence, &residual);
    self_test_i64(&mut evidence, &rotated);
    self_test_i64(&mut evidence, &attended);
    let evidence_root: [u8; 32] = evidence.finalize().into();
    let mut executor = Sha256::new();
    executor.update(b"octra:resource-compute-executor\0");
    executor.update(evidence_root);
    executor.update((Q24_FUEL_SCALE as u64).to_be_bytes());
    executor.update((Q24_CALL_LIMIT as u64).to_be_bytes());
    executor.update(Q24_WALL_LIMIT_SECS.to_be_bytes());
    executor.update(COMPUTE_MAX_FUEL_LIMIT.to_be_bytes());
    executor.update((COMPUTE_MAX_TENSOR_SESSION_BYTES as u64).to_be_bytes());
    executor.update(b"top1-score-status-v1\0");
    executor.update(b"tensor-cache-memory-v1\0");
    executor.update((COMPUTE_STORAGE_RESIDENT_LIMIT as u64).to_be_bytes());
    executor.update((TENSOR_SESSION_CACHE_LIMIT as u64).to_be_bytes());
    executor.update((FIXED_TENSOR_SESSION_CACHE_LIMIT as u64).to_be_bytes());
    executor.update(b"guest-pages-v1\0");
    executor.update(b"tensor-input-memory-v1\0");
    executor.update(b"tensor-result-memory-v1\0");
    executor.update(b"inline-storage-memory-v1\0");
    executor.update(b"storage-write-memory-v1\0");
    executor.update(b"compute-key-memory-v1\0");
    executor.update(b"caller-guest-pages\0");
    executor.update(b"response-buffer-memory\0");
    executor.update(b"reply-owner-memory\0");
    executor.update(b"builder-parse-memory\0");
    executor.update(b"session-shared-memory\0");
    executor.update(b"job-memory-budget\0");
    executor.update(b"storage-job-owner\0");
    executor.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
    executor.update(b"linear-quiet-nan\0");
    executor.update(0x7ff8000000000000_u64.to_le_bytes());
    executor.update(b"tensor-fixed-math\0libm-0.2.16\0");
    let executor_root = executor.finalize();
    Ok(json!({
        "evidence_root": hex::encode(evidence_root),
        "executor_root": hex::encode(executor_root),
        "profile": "q24-v1"
    }))
}

fn compute_storage_key<'a>(payload: &'a Payload<'_>) -> Result<&'a str, String> {
    let key = payload
        .compute_storage_cache_key
        .as_deref()
        .ok_or_else(|| "missing compute storage cache key".to_owned())?;
    normalize_hex64(key, "compute storage cache key")?;
    Ok(key)
}

fn compute_storage_root(storage: &StorageData) -> String {
    let mut hasher = Sha256::new();
    hasher.update(b"octra:compute_storage_cache:v1\0");
    for (key, value) in storage {
        hasher.update((key.len() as u32).to_be_bytes());
        hasher.update(key.as_bytes());
        hasher.update((value.len() as u64).to_be_bytes());
        hasher.update(Sha256::digest(value.as_slice()));
    }
    hex::encode(hasher.finalize())
}

fn b64_size(value: &str) -> usize {
    let len = value.len();
    let padding = usize::from(value.ends_with('=')) + usize::from(value.ends_with("=="));
    (len / 4 * 3 + (len % 4).saturating_sub(1)).saturating_sub(padding)
}

fn compute_memory() -> &'static Arc<native_bytes::Pool> {
    static POOL: OnceLock<Arc<native_bytes::Pool>> = OnceLock::new();
    POOL.get_or_init(|| native_bytes::Pool::new(COMPUTE_STORAGE_RESIDENT_LIMIT))
}

fn run_compute_storage_put(token: &BuildToken, payload: &Payload) -> Result<JsonValue, String> {
    put_storage(token, payload, compute_memory())
}

fn put_storage(
    token: &BuildToken, payload: &Payload, pool: &Arc<native_bytes::Pool>,
) -> Result<JsonValue, String> {
    let key = payload
        .compute_storage_key
        .as_deref()
        .ok_or_else(|| "missing compute storage key".to_owned())?;
    if key.is_empty() || key.len() > COMPUTE_STORAGE_KEY_LIMIT || key.as_bytes().contains(&0) {
        return Err("invalid compute storage key".to_owned());
    }
    let encoded = payload
        .compute_storage_value_b64
        .as_ref()
        .map(|value| value.0.as_ref())
        .ok_or_else(|| "missing compute storage value".to_owned())?;
    let size = b64_size(encoded);
    if size > COMPUTE_STORAGE_VALUE_LIMIT {
        return Err("compute storage value exceeds limit".to_owned());
    }
    {
        let table = compute_storage_builders().lock()
            .map_err(|_| "compute storage builder poisoned".to_owned())?;
        table.owner.delta(BuildMessage::Check(token.clone(), BuildPhase::Writing))?;
        table.data.get(&token.key)
            .ok_or_else(|| "compute storage builder data missing".to_owned())?
            .next_weight(key, size)?;
    }
    let value = decode_storage(pool, encoded)?;
    if value.len() > COMPUTE_STORAGE_VALUE_LIMIT {
        return Err("compute storage value exceeds limit".to_owned());
    }
    commit_storage(token, key, value, pool)
}

fn decode_storage(pool: &Arc<native_bytes::Pool>, encoded: &str) -> Result<native_bytes::Value, String> {
    let lease = pool.take(base64::decoded_len_estimate(encoded.len()))?;
    native_bytes::Value::charged(decode_b64("compute storage value", encoded)?, lease)
        .map_err(str::to_owned)
}

fn commit_storage(
    token: &BuildToken, key: &str, value: native_bytes::Value, pool: &Arc<native_bytes::Pool>,
) -> Result<JsonValue, String> {
    let mut table = compute_storage_builders()
        .lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    table.call(BuildMessage::Check(token.clone(), BuildPhase::Writing))?;
    let builder = table.data.get_mut(&token.key)
        .ok_or_else(|| "compute storage builder data missing".to_owned())?;
    let next_weight = builder.next_weight(key, value.len())?;
    match builder.storage.get(key) {
        Some(current) if current.as_slice() == value.as_slice() => {}
        Some(_) => return Err("compute storage key differs".to_owned()),
        None => {
            let mut lease = pool.take(key.len())?;
            let mut owned = String::new();
            owned.try_reserve_exact(key.len())
                .map_err(|_| "compute storage allocation failed".to_owned())?;
            if owned.capacity() > lease.size() {
                return Err("compute storage capacity differs".to_owned());
            }
            owned.push_str(key);
            if let Some(keys) = builder.storage.keys.as_mut() {
                keys.join(&mut lease)?;
            } else {
                builder.storage.keys = Some(lease);
            }
            builder.storage.data.insert(owned, Arc::new(value));
            builder.weight = next_weight;
        }
    }
    Ok(json!({
        "bytes": builder.weight,
        "cache_key": token.key,
        "entries": builder.storage.len(),
        "status": "building",
    }))
}

fn build_take(token: &BuildToken) -> Result<BuildWork, String> {
    let mut table = compute_storage_builders().lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    table.call(BuildMessage::Seal(token.clone()))?;
    let data = table.data.remove(&token.key)
        .ok_or_else(|| "compute storage builder data missing".to_owned())?;
    Ok(BuildWork { token: token.clone(), data: Some(data) })
}

fn build_publish(token: &BuildToken, entry: ComputeStorageEntry) -> Result<(), String> {
    let mut table = compute_storage_builders().lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    table.call(BuildMessage::Check(token.clone(), BuildPhase::Sealing))?;
    let mut cache = compute_storage_cache().lock()
        .map_err(|_| "compute storage cache poisoned".to_owned())?;
    let (state, _) = cache.delta(CacheMessage::Seal(token.key.clone(), entry.clone()))?;
    let retired = std::mem::replace(&mut *cache, state);
    drop(cache);
    drop(table);
    drop(retired);
    Ok(())
}

fn run_compute_storage_seal(token: &BuildToken, payload: &Payload) -> Result<JsonValue, String> {
    let expected_root = payload
        .compute_storage_root
        .as_deref()
        .ok_or_else(|| "missing compute storage root".to_owned())?;
    normalize_hex64(expected_root, "compute storage root")?;
    let expected_entries = payload
        .compute_storage_entries
        .ok_or_else(|| "missing compute storage entries".to_owned())?;
    let expected_bytes = payload
        .compute_storage_bytes
        .ok_or_else(|| "missing compute storage bytes".to_owned())?;
    let mut work = build_take(token)?;
    let data = work.data.as_ref().unwrap();
    let actual_root = compute_storage_root(&data.storage);
    if data.storage.len() != expected_entries
        || data.weight != expected_bytes
        || actual_root != expected_root
    {
        return Err("compute storage seal differs".to_owned());
    }
    let data = work.data.take().unwrap();
    build_publish(token, ComputeStorageEntry {
        storage: Arc::new(data.storage), root: actual_root.clone(),
        weight: data.weight, updated_at: now_secs(),
    })?;
    Ok(json!({
        "bytes": expected_bytes,
        "cache_key": token.key,
        "entries": expected_entries,
        "root": actual_root,
        "status": "ready",
    }))
}

fn run_compute_storage_status(payload: &Payload) -> Result<JsonValue, String> {
    let cache_key = compute_storage_key(payload)?.to_owned();
    match compute_cache_call(CacheMessage::Read(cache_key.clone(), now_secs()))? {
        CacheReply::Ready(entry) => {
            Ok(json!({
                "bytes": entry.weight,
                "cache_key": cache_key,
                "entries": entry.storage.len(),
                "root": entry.root,
                "status": "ready",
            }))
        }
        CacheReply::Missing => Ok(json!({"cache_key": cache_key, "status": "missing"})),
        _ => Err("compute storage status reply differs".to_owned()),
    }
}

fn run_compute_storage_drop(payload: &Payload) -> Result<JsonValue, String> {
    let cache_key = compute_storage_key(payload)?.to_owned();
    let table = compute_storage_builders().lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    if table.owner.slots.contains_key(&cache_key) {
        return Ok(json!({"cache_key": cache_key, "status": "busy"}));
    }
    let mut cache = compute_storage_cache().lock()
        .map_err(|_| "compute storage cache poisoned".to_owned())?;
    let (state, reply) = cache.delta(CacheMessage::Drop(cache_key.clone()))?;
    let retired = std::mem::replace(&mut *cache, state);
    drop(cache);
    drop(table);
    drop(retired);
    match reply {
        CacheReply::Missing => {
            Ok(json!({"cache_key": cache_key, "status": "missing"}))
        }
        CacheReply::Busy => Ok(json!({"cache_key": cache_key, "status": "busy"})),
        _ => Err("compute storage drop reply differs".to_owned()),
    }
}

fn run_validate(payload: &Payload) -> Result<JsonValue, HostFailure> {
    let runtime = Runtime::prepare(payload, true, false, None)?;
    Ok(json!({
        "exports": runtime.exports,
        "manifest": manifest_from_exports(&runtime.exports),
    }))
}

fn run_describe(payload: &Payload) -> Result<JsonValue, HostFailure> {
    let mut runtime = Runtime::prepare(payload, true, false, None)?;
    let manifest = if runtime
        .instance
        .get_func(&runtime.store, "octra_manifest")
        .is_some()
    {
        match runtime.call_export("octra_manifest", &[]) {
            Ok((_code, response_bytes, _effort_used)) => {
                serde_json::from_slice::<JsonValue>(&response_bytes).ok()
            }
            Err(_) => None,
        }
    } else {
        None
    };
    if let Some(reason) = runtime.store.data().inspect_error.as_ref() {
        return Err(reason.clone().into());
    }
    if runtime.store.data().local_storage {
        if let Some(reason) = runtime.store.data().unavailable.as_ref() {
            return Err(HostFailure::Unavailable(reason.clone()));
        }
    }
    Ok(json!({
        "exports": runtime.exports,
        "manifest": manifest,
    }))
}

fn prepare_execution<'a>(payload: &'a Payload)
    -> Result<(Runtime, native_bytes::Value, &'a str), HostFailure>
{
    prepare_owned_execution(payload, None)
}

fn prepare_owned_execution<'a>(payload: &'a Payload, owner: Option<Arc<SessionOwner>>)
    -> Result<(Runtime, native_bytes::Value, &'a str), HostFailure>
{
    let export_name = payload
        .export_name
        .as_deref()
        .ok_or_else(|| "missing export_name".to_owned())?;
    let request_b64 = payload
        .request_b64
        .as_deref()
        .ok_or_else(|| "missing request_b64".to_owned())?;
    let local = payload.is_view == Some(true)
        && (payload.inspect_policy.unwrap_or(false)
            || payload.execution_profile.as_deref() == Some("compute"));
    let budget = if local && payload.execution_profile.as_deref() == Some("compute") {
        memory_quota(payload, true, ExecutionProfile::Compute)?
    } else { None };
    let request_bytes = if local {
        let size = base64::decoded_len_estimate(request_b64.len());
        let lease = compute_memory().take_in(size, budget.as_ref())
            .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
        let mut bytes = Vec::new();
        bytes.try_reserve_exact(size)
            .map_err(|_| HostFailure::Unavailable("request allocation failed".to_owned()))?;
        if bytes.capacity() > lease.size() {
            return Err(HostFailure::Unavailable("request capacity differs".to_owned()));
        }
        bytes.resize(size, 0);
        let used = BASE64.decode_slice(request_b64, &mut bytes)
            .map_err(|error| match error {
                base64::DecodeSliceError::DecodeError(error) =>
                    HostFailure::Rejected(format!("invalid request_b64 base64: {error}")),
                base64::DecodeSliceError::OutputSliceTooSmall =>
                    HostFailure::Unavailable("request capacity differs".to_owned()),
            })?;
        bytes.truncate(used);
        native_bytes::Value::charged(bytes, lease)
            .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?
    } else {
        decode_b64("request_b64", request_b64)?.into()
    };
    let runtime = Runtime::prepare_owned(payload, false, false, budget, owner)?;
    Ok((runtime, request_bytes, export_name))
}

fn run_execute(payload: &Payload) -> Result<JsonValue, HostFailure> {
    let (mut runtime, request, export) = prepare_execution(payload)?;
    finish_execute(&mut runtime, export, &request).map_err(HostFailure::Rejected)
}

fn execute_output(
    runtime: &Runtime,
    code: i32,
    success: bool,
    response_bytes: &[u8],
    effort_used: u64,
    error: Option<String>,
) -> JsonValue {
    let state = runtime.store.data();
    let compute_trace_root = state
        .compute_trace
        .as_ref()
        .map(|trace| trace.finish(code, &response_bytes, effort_used));
    let compute_operations = state.compute_trace.as_ref().map(|trace| trace.operations);
    let compute_steps = state.compute_trace.as_ref().map(|_| effort_used);
    let storage_pairs = if state.is_view {
        JsonValue::Null
    } else {
        JsonValue::Array(
            state
                .storage
                .iter()
                .map(|(key, value)| {
                    json!({
                        "key_b64": encode_b64(key.as_bytes()),
                        "value_b64": encode_b64(value),
                    })
                })
                .collect(),
        )
    };
    let mut output = json!({
        "success": success,
        "status_code": state.response_status,
        "response_b64": if response_bytes.is_empty() { JsonValue::Null } else { JsonValue::String(encode_b64(response_bytes)) },
        "storage_pairs": storage_pairs,
        "events": state.events,
        "spawns": state.spawns,
        "assets": state.assets,
        "encrypted_assets": state.encrypted_assets,
        "hfhe_receipt_entries": state.hfhe_receipt_entries,
        "effort_used": effort_used,
        "compute_trace_root": compute_trace_root,
        "compute_steps": compute_steps,
        "compute_operations": compute_operations,
        "error": error,
        "unavailable": state.unavailable,
    });
    if state.compute_trace.is_some() {
        output["compute_exit"] = json!(code);
        output["compute_fault"] = json!(state.compute_fault);
    }
    output
}

struct CallResult {
    code: i32,
    bytes: native_bytes::Value,
    effort: u64,
    error: Option<String>,
}

fn finish_call(
    runtime: &mut Runtime,
    export_name: &str,
    request_bytes: &[u8],
) -> Result<CallResult, String> {
    if runtime.update_policy && !runtime.store.data().is_view && runtime.has_update_disabled_imports
    {
        return Err("wasm_v1 nonpersistent host imports are disabled for updates".to_owned());
    }
    if !runtime.store.data().is_view && runtime.has_compute_imports {
        return Err("wasm_v1 compute host imports are disabled for updates".to_owned());
    }
    if runtime.has_compute_imports
        && runtime.store.data().execution_profile != ExecutionProfile::Compute
    {
        return Err("wasm_v1 compute host imports require compute execution profile".to_owned());
    }
    maintain_compute_session_caches();
    if let Some(trace) = runtime.store.data_mut().compute_trace.as_mut() {
        trace.begin(export_name, request_bytes);
    }
    let (code, success, response_bytes, effort_used, error) =
        match runtime.call_export(export_name, request_bytes) {
            Ok((code, response_bytes, effort_used)) => (
                code,
                code == 0,
                response_bytes,
                effort_used,
                if code == 0 {
                    None
                } else {
                    Some(format!("wasm export returned {code}"))
                },
            ),
            Err(error) => {
                let remaining = runtime
                    .store
                    .get_fuel()
                    .map_err(|e| format!("wasm fuel read failed: {e}"))?;
                (
                    -1,
                    false,
                    native_bytes::Value::default(),
                    runtime.fuel_limit.saturating_sub(remaining),
                    Some(error),
                )
            }
        };
    let receipt_complete = runtime.store.data().hfhe_receipt_mode != "consume"
        || runtime.store.data().hfhe_receipt_index
            == runtime.store.data().hfhe_receipt_expected.len();
    if !success || !receipt_complete {
        clear_compute_session_scope(runtime.store.data())?;
    }
    if !receipt_complete {
        return Err("hfhe receipt was not fully consumed".to_owned());
    }
    Ok(CallResult { code, bytes: response_bytes, effort: effort_used, error })
}

fn finish_execute(runtime: &mut Runtime, export: &str, request: &[u8])
    -> Result<JsonValue, String>
{
    let call = finish_call(runtime, export, request)?;
    Ok(execute_output(runtime, call.code, call.code == 0, &call.bytes,
        call.effort, call.error))
}

impl Runtime {
    #[cfg(test)]
    fn instantiate(payload: &Payload, description_only: bool) -> Result<Self, String> {
        Self::prepare(payload, description_only, false, None).map_err(|error| error.to_string())
    }

    fn prepare(payload: &Payload, description_only: bool, calls: bool,
        budget: Option<Arc<native_bytes::Budget>>) -> Result<Self, HostFailure> {
        Self::prepare_owned(payload, description_only, calls, budget, None)
    }

    fn prepare_owned(payload: &Payload, description_only: bool, calls: bool,
        budget: Option<Arc<native_bytes::Budget>>, owner: Option<Arc<SessionOwner>>)
        -> Result<Self, HostFailure>
    {
        let profile = ExecutionProfile::of_payload(payload)?;
        if payload.compute_session_scope.is_some() && profile != ExecutionProfile::Compute {
            return Err(HostFailure::Rejected("compute session scope requires compute execution profile".to_owned()));
        }
        let code_b64_opt = payload.code_b64.as_deref();
        if owner.is_some() && code_b64_opt.is_none_or(str::is_empty) {
            return Err(HostFailure::Rejected("compute replay code missing".to_owned()));
        }
        let module_key = match payload.code_cache_key.as_deref() {
            Some(key) if !key.is_empty() => key.to_owned(),
            _ => {
                let code_b64 = code_b64_opt.ok_or_else(|| "missing code_b64".to_owned())?;
                let mut hasher = Sha256::new();
                hasher.update(code_b64.as_bytes());
                format!("{:x}", hasher.finalize())
            }
        };
        let float_mode = profile == ExecutionProfile::Compute || payload.float_mode.unwrap_or(false);
        let cache_key = if float_mode {
            format!("float_bits:{module_key}")
        } else {
            format!("float_prior:{module_key}")
        };
        let (module, exports, has_update_disabled_imports, has_compute_imports) = {
            let modules = owner.as_ref().map_or_else(|| module_cache(), |owner| &owner.modules);
            let mut guard = cache_guard(modules);
            if owner.is_none() { prune_module_cache_locked(&mut guard); }
            if let Some(entry) = guard.get_mut(&cache_key) {
                entry.updated_at = now_secs();
                (
                    entry.module.clone(),
                    entry.exports.clone(),
                    entry.has_update_disabled_imports,
                    entry.has_compute_imports,
                )
            } else {
                let code_b64 = code_b64_opt
                    .ok_or_else(|| format!("missing wasm module cache: {module_key}"))?;
                let wasm_bytes = decode_b64("code_b64", code_b64)?;
                let wasm_bytes = if float_mode {
                    float_ops::prepare(shared_engine(), &wasm_bytes)?
                } else {
                    wasm_bytes
                };
                let module = Arc::new(
                    Module::new(shared_engine(), &mut &wasm_bytes[..])
                        .map_err(|e| format!("wasm compile failed: {e}"))?,
                );
                validate_imports(&module)?;
                let exports = collect_exports(&module);
                validate_exports(&exports)?;
                let has_update_disabled_imports = module
                    .imports()
                    .any(|entry| UPDATE_DISABLED_IMPORTS.contains(&entry.name()));
                let has_compute_imports = module
                    .imports()
                    .any(|entry| COMPUTE_IMPORTS.contains(&entry.name()));
                guard.insert(
                    cache_key,
                    ModuleCacheEntry {
                        module: module.clone(),
                        exports: exports.clone(),
                        has_update_disabled_imports,
                        has_compute_imports,
                        updated_at: now_secs(),
                    },
                );
                (
                    module,
                    exports,
                    has_update_disabled_imports,
                    has_compute_imports,
                )
            }
        };

        if calls && (has_update_disabled_imports || has_compute_imports) {
            return Err(HostFailure::Rejected("circle update imports invalid".to_owned()));
        }
        if payload.inspect_policy.unwrap_or(false)
            && !description_only
            && payload.is_view == Some(false)
        {
            if has_update_disabled_imports {
                return Err(HostFailure::Rejected(
                    "wasm_v1 nonpersistent host imports are disabled for updates".to_owned(),
                ));
            }
            if has_compute_imports {
                return Err(HostFailure::Rejected(
                    "wasm_v1 compute host imports are disabled for updates".to_owned(),
                ));
            }
        }
        if profile == ExecutionProfile::Standard && has_compute_imports {
            let name = module
                .imports()
                .find(|entry| COMPUTE_IMPORTS.contains(&entry.name()))
                .map(|entry| entry.name().to_owned())
                .unwrap_or_else(|| "<unknown>".to_owned());
            return Err(HostFailure::Rejected(format!("unsupported wasm import: {name}")));
        }

        let mut state = HostState::from_quota(payload, description_only, budget)?;
        state.session_owner = owner;
        if state.local_sessions {
            state.guest = Some(compute_memory().take_in(0, state.budget.as_ref())
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?);
        }
        let mut store = Store::new(shared_engine(), state);
        if profile == ExecutionProfile::Compute {
            if let Some(scope) = payload.compute_session_scope.as_deref() {
                normalize_hex64(scope, "compute session scope")?;
            }
            store.data_mut().compute_trace = Some(ComputeTrace::new(
                &module_key,
                payload.compute_storage_cache_key.as_deref(),
                payload.compute_session_scope.as_deref(),
            ));
        }
        store.limiter(|state| state);
        let fuel_limit = profile.fuel(payload.fuel_limit);
        store
            .set_fuel(fuel_limit)
            .map_err(|e| format!("wasm fuel setup failed: {e}"))?;
        if description_only {
            let data = store.data_mut();
            data.storage = Arc::new(Storage::default());
            data.caller_bytes.clear();
            data.self_bytes.clear();
            data.current_epoch = 0;
            data.is_view = true;
        }

        let prepared: Result<Instance, HostFailure> = (|| {
            let mut linker = Linker::new(shared_engine());
            define_imports(&mut store, &mut linker)?;
            if description_only && payload.inspect_policy.unwrap_or(false) {
                seal_imports(&module, &mut store, &mut linker)?;
            }
            let started = match linker.instantiate(&mut store, &module) {
                Ok(instance) => instance.start(&mut store)
                    .map_err(|e| format!("wasm start failed: {e}")),
                Err(error) => Err(format!("wasm instantiate failed: {error}")),
            };
            if let Some(reason) = store.data().inspect_error.as_ref() {
                return Err(HostFailure::Rejected(reason.clone()));
            }
            if calls || store.data().local_storage {
                if let Some(reason) = store.data().unavailable.as_ref() {
                    return Err(HostFailure::Unavailable(reason.clone()));
                }
            }
            let instance = started?;
            let memory = instance
                .get_memory(&store, "memory")
                .ok_or_else(|| "wasm_v1 memory export missing".to_owned())?;
            let initial_pages = memory.data_size(&store) / PAGE_BYTES;
            if initial_pages > profile.max_initial_pages() {
                return Err(HostFailure::Rejected(format!(
                    "wasm_v1 memory exceeds {} pages",
                    profile.max_initial_pages()
                )));
            }
            Ok(instance)
        })();
        let instance = match prepared {
            Ok(instance) => instance,
            Err(error) => {
                if store.data().session_owner.is_some() {
                    clear_compute_session_scope(store.data()).map_err(HostFailure::Unavailable)?;
                } else if store.data().local_sessions {
                    clear_compute_session_prefix("", Some(&local_session_prefix(store.data())))
                        .map_err(HostFailure::Unavailable)?;
                }
                return Err(error);
            }
        };
        Ok(Self {
            store,
            instance,
            exports,
            fuel_limit,
            has_update_disabled_imports,
            update_policy: payload.update_policy.unwrap_or(false),
            has_compute_imports,
        })
    }

    fn call_export(
        &mut self,
        export_name: &str,
        request_bytes: &[u8],
    ) -> Result<(i32, native_bytes::Value, u64), String> {
        self.store.data_mut().compute_fault = None;
        let alloc = self
            .instance
            .get_typed_func::<i32, i32>(&self.store, "octra_alloc")
            .map_err(|_| "wasm export not found: octra_alloc".to_owned())?;
        let func = self
            .instance
            .get_typed_func::<(i32, i32), i32>(&self.store, export_name)
            .map_err(|_| format!("wasm export not found: {export_name}"))?;
        let ptr = alloc
            .call(&mut self.store, request_bytes.len() as i32)
            .map_err(|e| {
                if e.as_trap_code() == Some(wasmi::core::TrapCode::OutOfFuel) {
                    self.store.data_mut().compute_fault = Some("fuel");
                }
                format!("octra_alloc failed: {e}")
            })?;
        if ptr < 0 {
            return Err("invalid octra_alloc pointer".to_owned());
        }
        let memory = self
            .instance
            .get_memory(&self.store, "memory")
            .ok_or_else(|| "wasm_v1 memory export missing".to_owned())?;
        if (ptr as usize) + request_bytes.len() > memory.data_size(&self.store) {
            return Err("octra_alloc returned out-of-bounds pointer".to_owned());
        }
        memory
            .write(&mut self.store, ptr as usize, request_bytes)
            .map_err(|_| "octra_alloc returned out-of-bounds pointer".to_owned())?;
        let code = func
            .call(&mut self.store, (ptr, request_bytes.len() as i32))
            .map_err(|e| {
                if e.as_trap_code() == Some(wasmi::core::TrapCode::OutOfFuel) {
                    self.store.data_mut().compute_fault = Some("fuel");
                }
                format!("wasm export trapped: {e}")
            })?;
        let response_bytes = if self.store.data().local_storage {
            std::mem::take(&mut self.store.data_mut().response_bytes)
        } else {
            self.store.data().response_bytes.as_slice().to_vec().into()
        };
        let remaining = self
            .store
            .get_fuel()
            .map_err(|e| format!("wasm fuel read failed: {e}"))?;
        Ok((code, response_bytes, self.fuel_limit - remaining))
    }
}

fn is_lower_hex64(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn is_hfhe_verify_method(method: &str) -> bool {
    matches!(
        method,
        "fhe_verify_zero" | "fhe_verify_range" | "fhe_verify_bound"
    )
}

fn validate_hfhe_receipt_input(mode: &str, entries: &[HfheReceiptEntryJson]) -> Result<(), String> {
    if !matches!(mode, "direct" | "capture" | "consume") {
        return Err("invalid hfhe receipt mode".to_owned());
    }
    if entries.len() > MAX_HFHE_RECEIPT_ENTRIES {
        return Err("hfhe receipt entry limit exceeded".to_owned());
    }
    if mode != "consume" && !entries.is_empty() {
        return Err("hfhe receipt entries require consume mode".to_owned());
    }
    if entries
        .iter()
        .filter(|entry| is_hfhe_verify_method(&entry.method))
        .count()
        > MAX_HFHE_VERIFIER_ENTRIES
    {
        return Err("hfhe verifier entry limit exceeded".to_owned());
    }
    for entry in entries {
        if entry.method.is_empty()
            || entry.method.len() > 64
            || !is_lower_hex64(&entry.request_hash)
            || !is_lower_hex64(&entry.response_hash)
            || is_hfhe_verify_method(&entry.method) != entry.result.is_some()
        {
            return Err("invalid hfhe receipt entry".to_owned());
        }
    }
    Ok(())
}

fn memory_quota(payload: &Payload, local: bool, profile: ExecutionProfile)
    -> Result<Option<Arc<native_bytes::Budget>>, HostFailure>
{
    if payload.memory_bytes.is_some() && (!local || profile != ExecutionProfile::Compute) {
        return Err("memory budget requires local compute".into());
    }
    if !local || profile != ExecutionProfile::Compute { return Ok(None); }
    let bytes = payload.memory_bytes.unwrap_or(COMPUTE_STORAGE_RESIDENT_LIMIT);
    if bytes == 0 || bytes > COMPUTE_STORAGE_RESIDENT_LIMIT {
        return Err("compute memory bytes refused".into());
    }
    Ok(Some(native_bytes::Budget::new(bytes)))
}

impl HostState {
    #[cfg(test)]
    fn from_payload(payload: &Payload, description_only: bool) -> Result<Self, HostFailure> {
        Self::from_quota(payload, description_only, None)
    }

    fn from_quota(payload: &Payload, description_only: bool, budget: Option<Arc<native_bytes::Budget>>)
        -> Result<Self, HostFailure>
    {
        let (hfhe_strict, is_view) = if description_only {
            (false, true)
        } else {
            (
                payload
                    .hfhe_strict
                    .ok_or_else(|| "missing hfhe_strict".to_owned())?,
                payload.is_view.ok_or_else(|| "missing is_view".to_owned())?,
            )
        };
        let execution_profile = ExecutionProfile::of_payload(payload)?;
        let guest_bytes = match (execution_profile, payload.guest_bytes) {
            (ExecutionProfile::Compute, Some(bytes)) => {
                if bytes < PAGE_BYTES || bytes > COMPUTE_MAX_INITIAL_PAGES * PAGE_BYTES
                    || bytes % PAGE_BYTES != 0
                {
                    return Err(HostFailure::Rejected("compute guest bytes refused".to_owned()));
                }
                bytes
            }
            _ => execution_profile.max_initial_pages() * PAGE_BYTES,
        };
        let local_storage = payload.is_view == Some(true)
            && (payload.inspect_policy.unwrap_or(false)
                || execution_profile == ExecutionProfile::Compute);
        let local_sessions = !description_only && local_storage;
        let budget = match budget {
            Some(budget) => Some(budget),
            None => memory_quota(payload, local_storage, execution_profile)?,
        };
        let storage = build_storage_from_payload(payload, execution_profile, local_storage, budget.as_ref())?;
        let storage_hold = match budget.as_ref() {
            Some(budget) => Some(budget.take(storage.uncounted(budget)?)
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?),
            None => None,
        };
        let hfhe_caps = payload
            .hfhe_caps
            .clone()
            .unwrap_or_default()
            .into_iter()
            .collect();
        let hfhe_pubkeys = payload
            .hfhe_pubkeys
            .clone()
            .unwrap_or_default()
            .into_iter()
            .map(|entry| (entry.addr, entry.pubkey_b64))
            .collect();
        let hfhe_receipt_mode = payload
            .hfhe_receipt_mode
            .clone()
            .unwrap_or_else(|| "direct".to_owned());
        let hfhe_receipt_expected = payload.hfhe_receipt_entries.clone().unwrap_or_default();
        validate_hfhe_receipt_input(&hfhe_receipt_mode, &hfhe_receipt_expected)?;
        Ok(Self {
            limits: StoreLimitsBuilder::new()
                .memory_size(guest_bytes)
                .memories(1)
                .tables(1)
                .instances(1)
                .trap_on_grow_failure(true)
                .build(),
            guest: None,
            guest_prior: 0,
            execution_profile,
            max_guest_bytes: execution_profile.max_guest_bytes(),
            max_tensor_session_bytes: execution_profile.max_tensor_session_bytes(),
            storage,
            _storage_hold: storage_hold,
            budget,
            hfhe_caps,
            hfhe_pubkeys,
            hfhe_active_key: payload.hfhe_active_key.clone(),
            hfhe_strict,
            hfhe_pairs: payload.hfhe_pairs.unwrap_or(false),
            math: payload.math.unwrap_or(false),
            hfhe_receipt_mode,
            hfhe_receipt_expected,
            hfhe_receipt_entries: Vec::new(),
            hfhe_receipt_index: 0,
            public_reads: build_public_reads(payload)?,
            public_read_bytes: 0,
            caller_bytes: payload.caller.clone().unwrap_or_default().into_bytes(),
            self_bytes: payload.address.clone().unwrap_or_default().into_bytes(),
            tx_hash_bytes: payload.tx_hash.clone().unwrap_or_default().into_bytes(),
            current_epoch: payload.current_epoch.unwrap_or(0),
            is_view,
            response_bytes: native_bytes::Value::default(),
            response_status: 0,
            events: Vec::new(),
            event_bytes: 0,
            spawns: Vec::new(),
            assets: Vec::new(),
            encrypted_assets: Vec::new(),
            circle_invoke_cache: None,
            calls: false,
            call_request: None,
            hfhe_invoke_cache: None,
            unavailable: None,
            inspect_error: None,
            query_output: !description_only && is_view,
            local_storage,
            local_sessions,
            session_owner: None,
            compute_trace: None,
            compute_fault: None,
            compute_session_scope: payload.compute_session_scope.clone(),
            compute_started_at: Instant::now(),
            q24_calls: 0,
        })
    }
}

fn public_read_key(
    circle_id: &str,
    canonical_path: &str,
    offset: usize,
    max_bytes: usize,
) -> String {
    format!("{circle_id}\0{canonical_path}\0{offset}\0{max_bytes}")
}

fn build_public_reads(payload: &Payload) -> Result<BTreeMap<String, PublicRead>, String> {
    let reads = payload.public_reads.as_deref().unwrap_or(&[]);
    if reads.len() > MAX_PUBLIC_READS {
        return Err("wasm public read count exceeds limit".to_owned());
    }
    let mut total_bytes = 0usize;
    let mut result = BTreeMap::new();
    for read in reads {
        if read.circle_id.is_empty()
            || read.canonical_path.is_empty()
            || read.canonical_path.as_bytes().contains(&0)
            || read.circle_id.as_bytes().contains(&0)
            || read.max_bytes == 0
            || read.max_bytes > 65_536
        {
            return Err("invalid wasm public read".to_owned());
        }
        normalize_hex64(&read.snapshot_root, "snapshot_root")?;
        let metadata = serde_json::from_str::<JsonValue>(&read.metadata_json)
            .map_err(|_| "invalid wasm public read metadata".to_owned())?;
        if metadata.get("snapshot_root").and_then(JsonValue::as_str)
            != Some(read.snapshot_root.as_str())
        {
            return Err("wasm public read snapshot root mismatch".to_owned());
        }
        let body = BASE64
            .decode(read.body_b64.as_bytes())
            .map_err(|_| "invalid wasm public read body".to_owned())?;
        if body.len() > read.max_bytes {
            return Err("wasm public read body exceeds declaration".to_owned());
        }
        total_bytes = total_bytes
            .checked_add(body.len())
            .ok_or_else(|| "wasm public read bytes overflow".to_owned())?;
        if total_bytes > MAX_PUBLIC_READ_BYTES {
            return Err("wasm public read bytes exceed limit".to_owned());
        }
        let key = public_read_key(
            &read.circle_id,
            &read.canonical_path,
            read.offset,
            read.max_bytes,
        );
        if result
            .insert(
                key,
                PublicRead {
                    metadata_json: read.metadata_json.clone(),
                    body_b64: read.body_b64.clone(),
                    body_bytes: body.len(),
                },
            )
            .is_some()
        {
            return Err("duplicate wasm public read".to_owned());
        }
    }
    Ok(result)
}

fn build_storage_from_payload(
    payload: &Payload,
    execution_profile: ExecutionProfile,
    local: bool,
    quota: Option<&Arc<native_bytes::Budget>>,
) -> Result<Arc<Storage>, HostFailure> {
    if local {
        prune_storage_cache_locked(&mut cache_guard(storage_cache(true)));
    }
    if let Some(key) = payload.compute_storage_cache_key.as_deref() {
        if execution_profile != ExecutionProfile::Compute {
            return Err("compute storage requires compute execution profile".into());
        }
        if payload.storage_cache_key.is_some() || payload.storage_pairs.is_some() {
            return Err("compute storage cannot mix storage sources".into());
        }
        normalize_hex64(key, "compute storage cache key")?;
        match compute_cache_call(CacheMessage::Read(key.to_owned(), now_secs()))? {
            CacheReply::Ready(entry) => {
                return Ok(entry.storage.clone());
            }
            CacheReply::Missing => return Err(format!("missing compute storage cache: {key}").into()),
            _ => return Err("compute storage read reply differs".into()),
        }
    }
    let cache_key = payload.storage_cache_key.clone();
    let pairs_opt = payload.storage_pairs.as_deref();

    if pairs_opt.is_none() {
        if let Some(key) = cache_key {
            let mut guard = cache_guard(storage_cache(local));
            prune_storage_cache_locked(&mut guard);
            if let Some(entry) = guard.get_mut(&key) {
                entry.updated_at = now_secs();
                return Ok(entry.storage.clone());
            }
            return Err(format!("missing storage cache: {key}").into());
        }
        return Ok(Arc::new(Storage::default()));
    }

    let pairs = pairs_opt.unwrap_or(&[]);
    let keys = if local {
        let size = pairs.iter().try_fold(0usize, |total, pair| {
            total.checked_add(base64::decoded_len_estimate(pair.key_b64.len()))
                .ok_or("storage key bytes overflow")
        })?;
        Some(compute_memory().take_in(size, quota)
            .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?)
    } else {
        None
    };
    let mut storage = BTreeMap::new();
    for pair in pairs {
        let bytes = if local {
            read_storage_bytes("storage key", &pair.key_b64)?
        } else {
            decode_b64("storage key", &pair.key_b64)?
        };
        let key = String::from_utf8(bytes)
            .map_err(|_| "invalid storage key encoding".to_owned())?;
        let value = if local {
            let size = base64::decoded_len_estimate(pair.value_b64.len());
            let lease = compute_memory().take_in(size, quota)
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
            native_bytes::Value::charged(read_storage_bytes("storage value", &pair.value_b64)?, lease)
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?
        } else {
            decode_b64("storage value", &pair.value_b64)?.into()
        };
        storage.insert(key, Arc::new(value));
    }
    let shared = Arc::new(Storage { data: storage, keys });

    if let Some(key) = cache_key {
        let mut guard = cache_guard(storage_cache(local));
        let weight = storage_weight(&shared);
        prepare_storage_cache_insert(&mut guard, &key, weight);
        if weight <= STORAGE_CACHE_BYTE_LIMIT {
            guard.insert(
                key,
                StorageCacheEntry {
                    storage: shared.clone(),
                    weight,
                    updated_at: now_secs(),
                },
            );
        }

        return Ok(shared);
    }

    Ok(shared)
}

fn read_storage_bytes(name: &str, encoded: &str) -> Result<Vec<u8>, HostFailure> {
    let size = base64::decoded_len_estimate(encoded.len());
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(size)
        .map_err(|_| HostFailure::Unavailable("storage allocation failed".to_owned()))?;
    if bytes.capacity() > size {
        return Err(HostFailure::Unavailable("storage capacity differs".to_owned()));
    }
    bytes.resize(size, 0);
    let used = BASE64.decode_slice(encoded, &mut bytes)
        .map_err(|error| match error {
            base64::DecodeSliceError::DecodeError(error) =>
                HostFailure::Rejected(format!("invalid {name} base64: {error}")),
            base64::DecodeSliceError::OutputSliceTooSmall =>
                HostFailure::Unavailable("storage capacity differs".to_owned()),
        })?;
    bytes.truncate(used);
    Ok(bytes)
}

fn validate_imports(module: &Module) -> Result<(), String> {
    for import in module.imports() {
        if import.module() != "octra" {
            return Err(format!(
                "unsupported wasm import module: {}",
                import.module()
            ));
        }
        match import.ty() {
            ExternType::Func(_) => {}
            ExternType::Table(_) => return Err("unsupported wasm import kind: table".to_owned()),
            ExternType::Memory(_) => return Err("unsupported wasm import kind: memory".to_owned()),
            ExternType::Global(_) => return Err("unsupported wasm import kind: global".to_owned()),
        }
        if !ALLOWED_IMPORTS.iter().any(|name| *name == import.name()) {
            return Err(format!("unsupported wasm import: {}", import.name()));
        }
    }
    Ok(())
}

fn collect_exports(module: &Module) -> Vec<ExportInfoJson> {
    module
        .exports()
        .map(|entry| ExportInfoJson {
            name: entry.name().to_owned(),
            kind: export_kind(entry.ty()),
        })
        .collect()
}

fn export_kind(ty: &ExternType) -> String {
    match ty {
        ExternType::Func(_) => "function",
        ExternType::Table(_) => "table",
        ExternType::Memory(_) => "memory",
        ExternType::Global(_) => "global",
    }
    .to_owned()
}

fn validate_exports(exports: &[ExportInfoJson]) -> Result<(), String> {
    let names: BTreeSet<&str> = exports.iter().map(|entry| entry.name.as_str()).collect();
    if !names.contains("memory") {
        return Err("wasm_v1 module must export memory".to_owned());
    }
    if !names.contains("octra_alloc") {
        return Err("wasm_v1 module must export octra_alloc".to_owned());
    }
    if !names.contains("octra_query")
        && !names.contains("octra_update")
        && !names.contains("octra_http_request")
    {
        return Err(
            "wasm_v1 module must export octra_query, octra_update, or octra_http_request"
                .to_owned(),
        );
    }
    Ok(())
}

fn manifest_from_exports(exports: &[ExportInfoJson]) -> JsonValue {
    let names: BTreeSet<&str> = exports
        .iter()
        .filter(|entry| entry.kind == "function")
        .map(|entry| entry.name.as_str())
        .collect();
    let mut methods = Vec::new();
    if names.contains("octra_query") {
        methods.push(json!({"name":"octra_query","view":true}));
    }
    if names.contains("octra_update") {
        methods.push(json!({"name":"octra_update","view":false}));
    }
    if names.contains("octra_http_request") {
        methods.push(json!({"name":"octra_http_request","view":true}));
    }
    json!({ "methods": methods })
}

fn inspect_import(name: &str) -> bool {
    UPDATE_DISABLED_IMPORTS.contains(&name) || COMPUTE_IMPORTS.contains(&name)
}

fn seal_imports(
    module: &Module,
    store: &mut Store<HostState>,
    linker: &mut Linker<HostState>,
) -> Result<(), String> {
    linker.allow_shadowing(true);
    for entry in module.imports().filter(|entry| inspect_import(entry.name())) {
        let func = linker
            .get(&*store, entry.module(), entry.name())
            .and_then(Extern::into_func)
            .ok_or_else(|| format!("missing wasm host import: {}", entry.name()))?;
        let ty = func.ty(&*store);
        let reason = format!(
            "nonpersistent wasm import during inspection: {}",
            entry.name(),
        );
        let denied = Func::new(&mut *store, ty, move |mut caller, _, _| {
            caller.data_mut().inspect_error = Some(reason.clone());
            Err(WasmiError::new(reason.clone()))
        });
        linker
            .define(entry.module(), entry.name(), denied)
            .map_err(|e| format!("wasm inspection link failed: {e}"))?;
    }
    linker.allow_shadowing(false);
    Ok(())
}

fn define_imports(
    store: &mut Store<HostState>,
    linker: &mut Linker<HostState>,
) -> Result<(), String> {
    linker
        .define(
            "octra",
            "host_response_reset",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>| -> Result<i32, WasmiError> {
                    let data = caller.data_mut();
                    data.response_bytes.clear();
                    data.response_status = 0;
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_response_reset failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_response_write",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 ptr: i32,
                 len: i32|
                 -> Result<i32, WasmiError> {
                    if len < 0 {
                        return Ok(-1);
                    }
                    if caller.data().local_storage {
                        if ptr < 0 {
                            return Err(WasmiError::new("negative pointer"));
                        }
                        let size = len as usize;
                        if size > caller.data().max_guest_bytes {
                            return Err(WasmiError::new("guest read exceeds cap"));
                        }
                        charge_host_work(&mut caller, size.saturating_add(64))?;
                        let memory = caller.get_export("memory").and_then(Extern::into_memory)
                            .ok_or_else(|| WasmiError::new("wasm_v1 memory export missing"))?;
                        let (bytes, data) = memory.data_and_store_mut(&mut caller);
                        let chunk = bytes.get(ptr as usize..).and_then(|bytes| bytes.get(..size))
                            .ok_or_else(|| WasmiError::new("guest memory read out of bounds"))?;
                        if data.response_bytes.len().saturating_add(size) > MAX_RESPONSE_BYTES {
                            return Err(WasmiError::new("wasm response exceeds limit"));
                        }
                        data.response_bytes.append_in(Some(compute_memory()), data.budget.as_ref(), chunk, MAX_RESPONSE_BYTES)
                            .map_err(|reason| {
                                data.unavailable = Some(reason.to_owned());
                                WasmiError::new(reason)
                            })?;
                        return Ok(len);
                    }
                    let chunk = read_guest_bytes(&mut caller, ptr, len)?;
                    let data = caller.data_mut();
                    let total = data.response_bytes.len() + chunk.len();
                    if total > MAX_RESPONSE_BYTES {
                        return Err(WasmiError::new("wasm response exceeds limit"));
                    }
                    data.response_bytes.append(None, &chunk, MAX_RESPONSE_BYTES)
                        .map_err(WasmiError::new)?;
                    Ok(len)
                },
            ),
        )
        .map_err(|e| format!("link host_response_write failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_response_finish",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>, status_code: i32| -> Result<i32, WasmiError> {
                    caller.data_mut().response_status = status_code;
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_response_finish failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_circle_invoke_len",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 req_ptr: i32,
                 req_len: i32|
                 -> Result<i32, WasmiError> {
                    let request_bytes = read_guest_bytes(&mut caller, req_ptr, req_len)?;
                    if caller.data().calls && call_step::is_request(&request_bytes) {
                        if let Some((request, response)) = &caller.data().circle_invoke_cache {
                            if request == &request_bytes {
                                return Ok(response.len() as i32);
                            }
                        }
                        charge_host_fuel(&mut caller, CIRCLE_INVOKE_FUEL)?;
                        return call_step::suspend(&mut caller, request_bytes, None);
                    }
                    charge_host_fuel(&mut caller, CIRCLE_INVOKE_FUEL)?;
                    let response_bytes = execute_circle_invoke(&mut caller, &request_bytes)
                        .map_err(|error| host_failure(&mut caller, error))?;
                    let len = response_bytes.len() as i32;
                    caller.data_mut().circle_invoke_cache = Some((request_bytes, Arc::new(response_bytes)));
                    Ok(len)
                },
            ),
        )
        .map_err(|e| format!("link host_circle_invoke_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_circle_invoke",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 req_ptr: i32,
                 req_len: i32,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let request_bytes = read_guest_bytes(&mut caller, req_ptr, req_len)?;
                    let cached = caller.data().circle_invoke_cache.as_ref()
                        .filter(|(request, _)| request == &request_bytes)
                        .map(|(_, response)| response.clone());
                    let response;
                    let response_bytes = match &cached {
                        Some(value) => value.as_slice(),
                        None => {
                            charge_host_fuel(&mut caller, CIRCLE_INVOKE_FUEL)?;
                            if caller.data().calls && call_step::is_request(&request_bytes) {
                                return call_step::suspend(
                                    &mut caller, request_bytes, Some((out_ptr, out_cap)),
                                );
                            }
                            response = execute_circle_invoke(&mut caller, &request_bytes)
                                .map_err(|error| host_failure(&mut caller, error))?;
                            response.as_slice()
                        }
                    };
                    if out_cap < 0 || (out_cap as usize) < response_bytes.len() {
                        return Ok(-2);
                    }
                    let written = write_guest_bytes(&mut caller, out_ptr, response_bytes)?;
                    caller.data_mut().circle_invoke_cache = None;
                    Ok(written)
                },
            ),
        )
        .map_err(|e| format!("link host_circle_invoke failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_hfhe_invoke_len",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 req_ptr: i32,
                 req_len: i32|
                 -> Result<i32, WasmiError> {
                    let request_bytes = read_guest_bytes(&mut caller, req_ptr, req_len)?;
                    charge_host_fuel(&mut caller, HFHE_INVOKE_FUEL)?;
                    let response_bytes = match execute_hfhe_invoke(&mut caller, &request_bytes) {
                        Ok(response) => response,
                        Err(failure) => return Err(host_failure(&mut caller, failure)),
                    };
                    let len = response_bytes.len() as i32;
                    caller.data_mut().hfhe_invoke_cache = Some((request_bytes, Arc::new(response_bytes)));
                    Ok(len)
                },
            ),
        )
        .map_err(|e| format!("link host_hfhe_invoke_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_hfhe_invoke",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 req_ptr: i32,
                 req_len: i32,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let request_bytes = read_guest_bytes(&mut caller, req_ptr, req_len)?;
                    let cached = caller.data().hfhe_invoke_cache.as_ref()
                        .filter(|(request, _)| request == &request_bytes)
                        .map(|(_, response)| response.clone());
                    let response;
                    let response_bytes = match &cached {
                        Some(value) => value.as_slice(),
                        None => {
                            charge_host_fuel(&mut caller, HFHE_INVOKE_FUEL)?;
                            response = match execute_hfhe_invoke(&mut caller, &request_bytes) {
                                Ok(response) => response,
                                Err(failure) => return Err(host_failure(&mut caller, failure)),
                            };
                            response.as_slice()
                        }
                    };
                    if out_cap < 0 || (out_cap as usize) < response_bytes.len() {
                        return Ok(-2);
                    }
                    let written = write_guest_bytes(&mut caller, out_ptr, response_bytes)?;
                    caller.data_mut().hfhe_invoke_cache = None;
                    Ok(written)
                },
            ),
        )
        .map_err(|e| format!("link host_hfhe_invoke failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_kv_get_len",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32|
                 -> Result<i32, WasmiError> {
                    let key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    Ok(caller
                        .data()
                        .storage
                        .get(&key)
                        .map(|value| value.len() as i32)
                        .unwrap_or(-1))
                },
            ),
        )
        .map_err(|e| format!("link host_kv_get_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_kv_get",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let storage = caller.data().storage.clone();
                    let value = match storage.get(&key) {
                        Some(value) => value,
                        None => return Ok(-1),
                    };
                    if out_cap < 0 || (out_cap as usize) < value.len() {
                        return Ok(-2);
                    }
                    write_guest_bytes(&mut caller, out_ptr, value)
                },
            ),
        )
        .map_err(|e| format!("link host_kv_get failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_kv_put",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 value_ptr: i32,
                 value_len: i32|
                 -> Result<i32, WasmiError> {
                    if caller.data().is_view {
                        return Ok(-1);
                    }
                    let key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let value = read_guest_bytes(&mut caller, value_ptr, value_len)?;
                    let result = StorageEdit(&mut caller.data_mut().storage, false, None).insert(key, value);
                    result.map_err(|error| host_failure(&mut caller, error))?;
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_kv_put failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_kv_del",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32|
                 -> Result<i32, WasmiError> {
                    if caller.data().is_view {
                        return Ok(-1);
                    }
                    let key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let result = StorageEdit(&mut caller.data_mut().storage, false, None).remove(&key);
                    result.map_err(|error| host_failure(&mut caller, error))?;
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_kv_del failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_session_get_len",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32|
                 -> Result<i32, WasmiError> {
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = session_namespaced_key(caller.data(), &raw_key);
                    let owner = caller.data().session_owner.clone();
                    let mut guard = cache_guard(value_table(&owner, caller.data().local_sessions));
                    if owner.is_none() { prune_session_cache_locked(&mut guard); }
                    Ok(guard
                        .get(&cache_key)
                        .map(|entry| entry.value.len() as i32)
                        .unwrap_or(-1))
                },
            ),
        )
        .map_err(|e| format!("link host_session_get_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_session_get",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = session_namespaced_key(caller.data(), &raw_key);
                    let owner = caller.data().session_owner.clone();
                    let value = match {
                        let mut guard = cache_guard(value_table(&owner, caller.data().local_sessions));
                        if owner.is_none() { prune_session_cache_locked(&mut guard); }
                        guard.get(&cache_key).map(|entry| entry.value.clone())
                    } {
                        Some(value) => value,
                        None => return Ok(-1),
                    };
                    if out_cap < 0 || (out_cap as usize) < value.len() {
                        return Ok(-2);
                    }
                    let _held = value.borrow(caller.data().budget.as_ref())
                        .map_err(|reason| host_failure(&mut caller, HostFailure::Unavailable(reason.to_owned())))?;
                    write_guest_bytes(&mut caller, out_ptr, &value)
                },
            ),
        )
        .map_err(|e| format!("link host_session_get failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_session_put",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 value_ptr: i32,
                 value_len: i32|
                 -> Result<i32, WasmiError> {
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = session_namespaced_key(caller.data(), &raw_key);
                    if caller.data().local_sessions {
                        return put_local_session(&mut caller, cache_key, value_ptr, value_len);
                    }
                    let value = Arc::new(read_guest_bytes(&mut caller, value_ptr, value_len)?.into());
                    let mut guard = cache_guard(session_cache());
                    prune_session_cache_locked(&mut guard);
                    guard.insert(
                        cache_key,
                        SessionCacheEntry {
                            value,
                            updated_at: now_secs(),
                        },
                    );
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_session_put failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_has",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 n_layers: i32,
                 max_t: i32,
                 kv_dim: i32|
                 -> Result<i32, WasmiError> {
                    if n_layers <= 0 || max_t <= 0 || kv_dim <= 0 {
                        return Ok(-1);
                    }
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = tensor_session_namespaced_key(caller.data(), &raw_key);
                    let owner = caller.data().session_owner.clone();
                    let mut guard = cache_guard(tensor_table(&owner, caller.data().local_sessions));
                    if owner.is_none() { prune_tensor_session_cache_locked(&mut guard); }
                    match guard.get(&cache_key) {
                        Some(entry)
                            if entry.n_layers == n_layers as usize
                                && entry.max_t == max_t as usize
                                && entry.kv_dim == kv_dim as usize =>
                        {
                            Ok(1)
                        }
                        Some(_) => {
                            guard.remove(&cache_key);
                            Ok(0)
                        }
                        None => Ok(0),
                    }
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_has failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_reset",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 n_layers: i32,
                 max_t: i32,
                 kv_dim: i32|
                 -> Result<i32, WasmiError> {
                    if n_layers <= 0 || max_t <= 0 || kv_dim <= 0 {
                        return Ok(-1);
                    }
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = tensor_session_namespaced_key(caller.data(), &raw_key);
                    let (n_layers, max_t, kv_dim) =
                        (n_layers as usize, max_t as usize, kv_dim as usize);
                    let total = n_layers
                        .checked_mul(max_t)
                        .and_then(|value| value.checked_mul(kv_dim))
                        .ok_or_else(|| WasmiError::new("tensor session size overflow"))?;
                    let total_bytes = total
                        .checked_mul(16)
                        .filter(|value| *value <= MAX_TENSOR_SESSION_BYTES)
                        .ok_or_else(|| WasmiError::new("tensor session exceeds cap"))?;
                    charge_host_work(&mut caller, total_bytes)?;
                    let local = caller.data().local_sessions;
                    let owner = caller.data().session_owner.clone();
                    let mut guard = cache_guard(tensor_table(&owner, local));
                    if owner.is_none() { prune_tensor_session_cache_locked(&mut guard); }
                    let (keys, values, lease) = if local {
                        if guard.len() >= TENSOR_SESSION_CACHE_LIMIT && !guard.contains_key(&cache_key) {
                            return Ok(-1);
                        }
                        let key_bytes = guard.get_key_value(&cache_key)
                            .map_or(cache_key.capacity(), |(key, _)| key.capacity().max(cache_key.capacity()));
                        match tensor_memory(total, key_bytes, 0.0_f64, caller.data().budget.as_ref()) {
                            Some((keys, values, lease)) => (keys, values, Some(lease)),
                            None if owner.is_some() => return Err(host_failure(&mut caller,
                                HostFailure::Unavailable("replay tensor allocation failed".to_owned()))),
                            None => return Ok(-1),
                        }
                    } else {
                        (vec![0.0_f64; total], vec![0.0_f64; total], None)
                    };
                    guard.insert(
                        cache_key,
                        TensorSessionEntry {
                            n_layers,
                            max_t,
                            kv_dim,
                            k_cache: keys,
                            v_cache: values,
                            _lease: lease,
                            updated_at: now_secs(),
                        },
                    );
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_reset failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_append_kv_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 layer_idx: i32,
                 position: i32,
                 k_ptr: i32,
                 v_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_session_append_kv_fp(
                        &mut caller,
                        key_ptr,
                        key_len,
                        layer_idx,
                        position,
                        k_ptr,
                        v_ptr,
                        n,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_append_kv_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_attention_kv_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 q_ptr: i32,
                 ctx_ptr: i32,
                 t_total: i32,
                 layer_idx: i32,
                 n_q_heads: i32,
                 n_kv_heads: i32,
                 head_dim: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_session_attention_kv_fp(
                        &mut caller,
                        key_ptr,
                        key_len,
                        q_ptr,
                        ctx_ptr,
                        t_total,
                        layer_idx,
                        n_q_heads,
                        n_kv_heads,
                        head_dim,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_attention_kv_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_epoch",
            Func::wrap(
                &mut *store,
                |caller: Caller<'_, HostState>| -> Result<i64, WasmiError> {
                    Ok(caller.data().current_epoch)
                },
            ),
        )
        .map_err(|e| format!("link host_epoch failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_caller_len",
            Func::wrap(
                &mut *store,
                |caller: Caller<'_, HostState>| -> Result<i32, WasmiError> {
                    Ok(caller.data().caller_bytes.len() as i32)
                },
            ),
        )
        .map_err(|e| format!("link host_caller_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_caller_read",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let bytes = caller.data().caller_bytes.clone();
                    if out_cap < 0 || (out_cap as usize) < bytes.len() {
                        return Ok(-2);
                    }
                    write_guest_bytes(&mut caller, out_ptr, &bytes)
                },
            ),
        )
        .map_err(|e| format!("link host_caller_read failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_self_len",
            Func::wrap(
                &mut *store,
                |caller: Caller<'_, HostState>| -> Result<i32, WasmiError> {
                    Ok(caller.data().self_bytes.len() as i32)
                },
            ),
        )
        .map_err(|e| format!("link host_self_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_self_read",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let bytes = caller.data().self_bytes.clone();
                    if out_cap < 0 || (out_cap as usize) < bytes.len() {
                        return Ok(-2);
                    }
                    write_guest_bytes(&mut caller, out_ptr, &bytes)
                },
            ),
        )
        .map_err(|e| format!("link host_self_read failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_state_path_key_len",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 ptr: i32,
                 len: i32|
                 -> Result<i32, WasmiError> {
                    let state_ref = read_guest_utf8(&mut caller, ptr, len)?;
                    let encoded = state_path_key(&state_ref).into_bytes();
                    Ok(encoded.len() as i32)
                },
            ),
        )
        .map_err(|e| format!("link host_state_path_key_len failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_state_path_key",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 ptr: i32,
                 len: i32,
                 out_ptr: i32,
                 out_cap: i32|
                 -> Result<i32, WasmiError> {
                    let state_ref = read_guest_utf8(&mut caller, ptr, len)?;
                    let encoded = state_path_key(&state_ref).into_bytes();
                    if out_cap < 0 || (out_cap as usize) < encoded.len() {
                        return Ok(-2);
                    }
                    write_guest_bytes(&mut caller, out_ptr, &encoded)
                },
            ),
        )
        .map_err(|e| format!("link host_state_path_key failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_emit_event",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 topic_ptr: i32,
                 topic_len: i32,
                 data_ptr: i32,
                 data_len: i32|
                 -> Result<i32, WasmiError> {
                    if caller.data().is_view {
                        return Ok(-1);
                    }
                    if topic_len < 0
                        || data_len < 0
                        || topic_len as usize > MAX_EVENT_TOPIC_BYTES
                        || data_len as usize > MAX_EVENT_DATA_BYTES
                    {
                        return Err(WasmiError::new("wasm event exceeds field cap"));
                    }
                    let event_bytes = (topic_len as usize)
                        .checked_add(data_len as usize)
                        .ok_or_else(|| WasmiError::new("wasm event size overflow"))?;
                    if caller.data().events.len() >= MAX_EVENT_COUNT
                        || caller.data().event_bytes.saturating_add(event_bytes) > MAX_EVENT_BYTES
                    {
                        return Err(WasmiError::new("wasm event cap exceeded"));
                    }
                    let topic = read_guest_bytes(&mut caller, topic_ptr, topic_len)?;
                    let data = read_guest_bytes(&mut caller, data_ptr, data_len)?;
                    let state = caller.data_mut();
                    state.event_bytes += event_bytes;
                    state.events.push(EventJson {
                        topic_b64: encode_b64(&topic),
                        data_b64: encode_b64(&data),
                    });
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_emit_event failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_load_i8_kv",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 dst_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 off: i32,
                 n: i32,
                 scale_bits: i64|
                 -> Result<i32, WasmiError> {
                    host_tensor_load_i8_kv(
                        &mut caller,
                        dst_ptr,
                        key_ptr,
                        key_len,
                        off,
                        n,
                        scale_bits,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_load_i8_kv failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_matmul_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 dst_ptr: i32,
                 lhs_ptr: i32,
                 rhs_ptr: i32,
                 m: i32,
                 k: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_matmul_fp(&mut caller, dst_ptr, lhs_ptr, rhs_ptr, m, k, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_matmul_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_rmsnorm_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 addr_ptr: i32,
                 n: i32,
                 gamma_ptr: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_rmsnorm_fp(&mut caller, addr_ptr, n, gamma_ptr)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_rmsnorm_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_silu_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 addr_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_silu_fp(&mut caller, addr_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_silu_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_elemwise_mul_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 dst_ptr: i32,
                 src_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_elemwise_mul_fp(&mut caller, dst_ptr, src_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_elemwise_mul_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_residual_add_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 dst_ptr: i32,
                 src_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_residual_add_fp(&mut caller, dst_ptr, src_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_residual_add_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_rope_apply_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 addr_ptr: i32,
                 n_dim: i32,
                 pos: i32,
                 base_bits: i64|
                 -> Result<i32, WasmiError> {
                    host_tensor_rope_apply_fp(&mut caller, addr_ptr, n_dim, pos, base_bits)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_rope_apply_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_attention_kv_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 q_ptr: i32,
                 k_ptr: i32,
                 v_ptr: i32,
                 ctx_ptr: i32,
                 t_total: i32,
                 n_q_heads: i32,
                 n_kv_heads: i32,
                 head_dim: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_attention_kv_fp(
                        &mut caller,
                        q_ptr,
                        k_ptr,
                        v_ptr,
                        ctx_ptr,
                        t_total,
                        n_q_heads,
                        n_kv_heads,
                        head_dim,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_attention_kv_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_append_vec_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 dst_ptr: i32,
                 pos: i32,
                 src_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_append_vec_fp(&mut caller, dst_ptr, pos, src_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_append_vec_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_argmax_fp",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 addr_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_argmax_fp(&mut caller, addr_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_argmax_fp failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_linear_i8_kv",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 out_ptr: i32,
                 in_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 in_dim: i32,
                 out_dim: i32,
                 scale_bits: i64|
                 -> Result<i32, WasmiError> {
                    host_tensor_linear_i8_kv(
                        &mut caller,
                        out_ptr,
                        in_ptr,
                        key_ptr,
                        key_len,
                        in_dim,
                        out_dim,
                        scale_bits,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_linear_i8_kv failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_argmax_i8_kv",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 in_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 rows: i32,
                 cols: i32,
                 scale_bits: i64|
                 -> Result<i32, WasmiError> {
                    host_tensor_argmax_i8_kv(
                        &mut caller,
                        in_ptr,
                        key_ptr,
                        key_len,
                        rows,
                        cols,
                        scale_bits,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_argmax_i8_kv failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_top1_i8_kv",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 in_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 rows: i32,
                 cols: i32,
                 scale_bits: i64,
                 score_out_ptr: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_top1_i8_kv(
                        &mut caller,
                        in_ptr,
                        key_ptr,
                        key_len,
                        rows,
                        cols,
                        scale_bits,
                        score_out_ptr,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_top1_i8_kv failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_has_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 n_layers: i32,
                 max_t: i32,
                 kv_dim: i32|
                 -> Result<i32, WasmiError> {
                    require_compute_profile(&caller)?;
                    if n_layers <= 0 || max_t <= 0 || kv_dim <= 0 {
                        return Ok(-1);
                    }
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = fixed_tensor_session_namespaced_key(caller.data(), &raw_key);
                    let owner = caller.data().session_owner.clone();
                    let mut guard = fixed_table(&owner, caller.data().local_sessions)
                        .lock()
                        .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
                    if owner.is_none() { prune_fixed_tensor_session_cache_locked(&mut guard); }
                    let result: i32 = match guard.get(&cache_key) {
                        Some(entry)
                            if entry.n_layers == n_layers as usize
                                && entry.max_t == max_t as usize
                                && entry.kv_dim == kv_dim as usize =>
                        {
                            1
                        }
                        Some(_) => {
                            guard.remove(&cache_key);
                            0
                        }
                        None => 0,
                    };
                    drop(guard);
                    let key_digest: [u8; 32] = Sha256::digest(raw_key.as_bytes()).into();
                    let layers_bytes = n_layers.to_le_bytes();
                    let tokens_bytes = max_t.to_le_bytes();
                    let dimension_bytes = kv_dim.to_le_bytes();
                    let result_bytes = result.to_le_bytes();
                    record_compute_trace(
                        &mut caller,
                        "tensor_session_has_q24",
                        &[
                            &key_digest,
                            &layers_bytes,
                            &tokens_bytes,
                            &dimension_bytes,
                            &result_bytes,
                        ],
                    );
                    Ok(result)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_has_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_reset_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 n_layers: i32,
                 max_t: i32,
                 kv_dim: i32|
                 -> Result<i32, WasmiError> {
                    require_compute_profile(&caller)?;
                    if n_layers <= 0 || max_t <= 0 || kv_dim <= 0 {
                        return Ok(-1);
                    }
                    let raw_key = read_guest_utf8(&mut caller, key_ptr, key_len)?;
                    let cache_key = fixed_tensor_session_namespaced_key(caller.data(), &raw_key);
                    let (n_layers, max_t, kv_dim) =
                        (n_layers as usize, max_t as usize, kv_dim as usize);
                    let total = n_layers
                        .checked_mul(max_t)
                        .and_then(|value| value.checked_mul(kv_dim))
                        .ok_or_else(|| WasmiError::new("fixed tensor session size overflow"))?;
                    let total_bytes = total
                        .checked_mul(16)
                        .filter(|value| *value <= caller.data().max_tensor_session_bytes)
                        .ok_or_else(|| WasmiError::new("fixed tensor session exceeds cap"))?;
                    charge_q24_work(&mut caller, total_bytes)?;
                    let local = caller.data().local_sessions;
                    let owner = caller.data().session_owner.clone();
                    let mut guard = fixed_table(&owner, local)
                        .lock()
                        .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
                    if owner.is_none() { prune_fixed_tensor_session_cache_locked(&mut guard); }
                    let (keys, values, lease) = if local {
                        if guard.len() >= FIXED_TENSOR_SESSION_CACHE_LIMIT && !guard.contains_key(&cache_key) {
                            return Ok(-1);
                        }
                        let key_bytes = guard.get_key_value(&cache_key)
                            .map_or(cache_key.capacity(), |(key, _)| key.capacity().max(cache_key.capacity()));
                        match tensor_memory(total, key_bytes, 0_i64, caller.data().budget.as_ref()) {
                            Some((keys, values, lease)) => (keys, values, Some(lease)),
                            None if owner.is_some() => return Err(host_failure(&mut caller,
                                HostFailure::Unavailable("replay tensor allocation failed".to_owned()))),
                            None => return Ok(-1),
                        }
                    } else {
                        (vec![0_i64; total], vec![0_i64; total], None)
                    };
                    guard.insert(
                        cache_key,
                        FixedTensorSessionEntry {
                            n_layers,
                            max_t,
                            kv_dim,
                            k_cache: keys,
                            v_cache: values,
                            _lease: lease,
                            updated_at: now_secs(),
                        },
                    );
                    if owner.is_none() { prune_fixed_tensor_session_cache_locked(&mut guard); }
                    drop(guard);
                    let key_digest: [u8; 32] = Sha256::digest(raw_key.as_bytes()).into();
                    let layers_bytes = (n_layers as u64).to_le_bytes();
                    let tokens_bytes = (max_t as u64).to_le_bytes();
                    let dimension_bytes = (kv_dim as u64).to_le_bytes();
                    record_compute_trace(
                        &mut caller,
                        "tensor_session_reset_q24",
                        &[&key_digest, &layers_bytes, &tokens_bytes, &dimension_bytes],
                    );
                    Ok(0)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_reset_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_append_kv_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 layer_idx: i32,
                 position: i32,
                 key_values_ptr: i32,
                 values_ptr: i32,
                 n: i32|
                 -> Result<i32, WasmiError> {
                    host_tensor_session_append_kv_q24(
                        &mut caller,
                        key_ptr,
                        key_len,
                        layer_idx,
                        position,
                        key_values_ptr,
                        values_ptr,
                        n,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_append_kv_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_session_attention_kv_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 key_ptr: i32,
                 key_len: i32,
                 query_ptr: i32,
                 output_ptr: i32,
                 sequence_len: i32,
                 layer_idx: i32,
                 query_heads: i32,
                 kv_heads: i32,
                 head_dim: i32,
                 scale: i64|
                 -> Result<i32, WasmiError> {
                    host_tensor_session_attention_kv_q24(
                        &mut caller,
                        key_ptr,
                        key_len,
                        query_ptr,
                        output_ptr,
                        sequence_len,
                        layer_idx,
                        query_heads,
                        kv_heads,
                        head_dim,
                        scale,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_session_attention_kv_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_load_i8_kv_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 dst_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 off: i32,
                 n: i32,
                 scale_bits: i64|
                 -> Result<i32, WasmiError> {
                    host_tensor_load_i8_kv_q24(
                        &mut caller,
                        dst_ptr,
                        key_ptr,
                        key_len,
                        off,
                        n,
                        scale_bits,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_load_i8_kv_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_rmsnorm_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>, addr_ptr: i32, n: i32, gamma_ptr: i32| {
                    host_tensor_rmsnorm_q24(&mut caller, addr_ptr, n, gamma_ptr)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_rmsnorm_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_silu_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>, addr_ptr: i32, n: i32| {
                    host_tensor_silu_q24(&mut caller, addr_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_silu_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_elemwise_mul_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>, dst_ptr: i32, src_ptr: i32, n: i32| {
                    host_tensor_elemwise_mul_q24(&mut caller, dst_ptr, src_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_elemwise_mul_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_residual_add_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>, dst_ptr: i32, src_ptr: i32, n: i32| {
                    host_tensor_residual_add_q24(&mut caller, dst_ptr, src_ptr, n)
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_residual_add_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_rope_apply_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 addr_ptr: i32,
                 n_dim: i32,
                 position: i32,
                 frequencies_ptr: i32| {
                    host_tensor_rope_apply_q24(
                        &mut caller,
                        addr_ptr,
                        n_dim,
                        position,
                        frequencies_ptr,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_rope_apply_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_linear_i8_kv_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 out_ptr: i32,
                 in_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 in_dim: i32,
                 out_dim: i32,
                 scale_bits: i64| {
                    host_tensor_linear_i8_kv_q24(
                        &mut caller,
                        out_ptr,
                        in_ptr,
                        key_ptr,
                        key_len,
                        in_dim,
                        out_dim,
                        scale_bits,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_linear_i8_kv_q24 failed: {e}"))?;
    linker
        .define(
            "octra",
            "host_tensor_top1_i8_kv_q24",
            Func::wrap(
                &mut *store,
                |mut caller: Caller<'_, HostState>,
                 in_ptr: i32,
                 key_ptr: i32,
                 key_len: i32,
                 rows: i32,
                 cols: i32,
                 scale_bits: i64,
                 score_out_ptr: i32| {
                    host_tensor_top1_i8_kv_q24(
                        &mut caller,
                        in_ptr,
                        key_ptr,
                        key_len,
                        rows,
                        cols,
                        scale_bits,
                        score_out_ptr,
                    )
                },
            ),
        )
        .map_err(|e| format!("link host_tensor_top1_i8_kv_q24 failed: {e}"))?;
    Ok(())
}

fn charge_host_fuel(caller: &mut Caller<'_, HostState>, cost: u64) -> Result<(), WasmiError> {
    let remaining = caller
        .get_fuel()
        .map_err(|e| WasmiError::new(format!("wasm fuel read failed: {e}")))?;
    if cost > remaining {
        caller
            .set_fuel(0)
            .map_err(|e| WasmiError::new(format!("wasm fuel update failed: {e}")))?;
        return Err(WasmiError::new("wasm fuel exhausted"));
    }
    caller
        .set_fuel(remaining - cost)
        .map_err(|e| WasmiError::new(format!("wasm fuel update failed: {e}")))
}

fn charge_host_work(caller: &mut Caller<'_, HostState>, units: usize) -> Result<(), WasmiError> {
    charge_host_fuel(caller, u64::try_from(units).unwrap_or(u64::MAX))
}

fn charge_q24_work(caller: &mut Caller<'_, HostState>, units: usize) -> Result<(), WasmiError> {
    {
        let data = caller.data_mut();
        if data.q24_calls >= Q24_CALL_LIMIT
            || (data.session_owner.is_none()
                && data.compute_started_at.elapsed() > Duration::from_secs(Q24_WALL_LIMIT_SECS))
        {
            return Err(WasmiError::new("q24 execution limit reached"));
        }
        data.q24_calls += 1;
    }
    charge_host_work(caller, units.div_ceil(Q24_FUEL_SCALE).max(1))
}

fn host_work_product(factors: &[usize]) -> Result<usize, WasmiError> {
    factors
        .iter()
        .try_fold(1_usize, |value, factor| value.checked_mul(*factor))
        .ok_or_else(|| WasmiError::new("wasm host work overflow"))
}

fn read_guest_bytes(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: i32,
) -> Result<Vec<u8>, WasmiError> {
    if ptr < 0 || len < 0 {
        return Err(WasmiError::new("negative pointer"));
    }
    let len = len as usize;
    if len > caller.data().max_guest_bytes {
        return Err(WasmiError::new("guest read exceeds cap"));
    }
    charge_host_work(caller, len.saturating_add(64))?;
    let memory = caller
        .get_export("memory")
        .and_then(Extern::into_memory)
        .ok_or_else(|| WasmiError::new("wasm_v1 memory export missing"))?;
    let bytes = memory.data(&*caller).get(ptr as usize..)
        .and_then(|bytes| bytes.get(..len))
        .ok_or_else(|| WasmiError::new("guest memory read out of bounds"))?;
    Ok(bytes.to_vec())
}

fn read_guest_utf8(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: i32,
) -> Result<String, WasmiError> {
    let bytes = read_guest_bytes(caller, ptr, len)?;
    String::from_utf8(bytes).map_err(|_| WasmiError::new("invalid guest utf8"))
}

fn write_guest_bytes(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    bytes: &[u8],
) -> Result<i32, WasmiError> {
    if ptr < 0 {
        return Ok(-2);
    }
    if bytes.len() > caller.data().max_guest_bytes {
        return Err(WasmiError::new("guest write exceeds cap"));
    }
    let memory = caller
        .get_export("memory")
        .and_then(Extern::into_memory)
        .ok_or_else(|| WasmiError::new("wasm_v1 memory export missing"))?;
    if (ptr as usize) + bytes.len() > memory.data_size(caller.as_context()) {
        return Ok(-2);
    }
    memory
        .write(caller.as_context_mut(), ptr as usize, bytes)
        .map_err(|_| WasmiError::new("guest memory write out of bounds"))?;
    Ok(bytes.len() as i32)
}

fn session_namespaced_key(data: &HostState, raw_key: &str) -> String {
    if data.local_sessions {
        return format!("{}{raw_key}", local_session_prefix(data));
    }
    legacy_session_key(data, raw_key)
}

fn legacy_session_key(data: &HostState, raw_key: &str) -> String {
    match data.compute_session_scope.as_deref() {
        Some(scope) => format!(
            "{}|{}|{}|{}",
            String::from_utf8_lossy(&data.self_bytes),
            String::from_utf8_lossy(&data.caller_bytes),
            scope,
            raw_key
        ),
        None => format!(
            "{}|{}|{}",
            String::from_utf8_lossy(&data.self_bytes),
            String::from_utf8_lossy(&data.caller_bytes),
            raw_key
        ),
    }
}

fn local_session_prefix(data: &HostState) -> String {
    let address = String::from_utf8_lossy(&data.self_bytes);
    let caller = String::from_utf8_lossy(&data.caller_bytes);
    let scope = data.compute_session_scope.as_deref().unwrap_or("");
    format!("{}:{}|{}:{}|{}|{}:{}|", address.len(), address, caller.len(), caller,
        data.compute_session_scope.is_some(), scope.len(), scope)
}

fn put_local_session(
    caller: &mut Caller<'_, HostState>,
    key: String,
    ptr: i32,
    len: i32,
) -> Result<i32, WasmiError> {
    if ptr < 0 || len < 0 {
        return Err(WasmiError::new("negative pointer"));
    }
    let size = len as usize;
    if size > caller.data().max_guest_bytes {
        return Err(WasmiError::new("guest read exceeds cap"));
    }
    let memory = caller.get_export("memory").and_then(Extern::into_memory)
        .ok_or_else(|| WasmiError::new("wasm_v1 memory export missing"))?;
    if (ptr as usize).checked_add(size)
        .filter(|end| *end <= memory.data_size(caller.as_context())).is_none()
    {
        return Err(WasmiError::new("guest memory read out of bounds"));
    }
    let owner = caller.data().session_owner.clone();
    let mut cache = cache_guard(value_table(&owner, true));
    if owner.is_none() { prune_session_cache_locked(&mut cache); }
    if cache.len() >= SESSION_CACHE_LIMIT && !cache.contains_key(&key) {
        return Ok(-1);
    }
    let bytes = key.capacity().checked_add(size)
        .ok_or_else(|| WasmiError::new("session size overflow"))?;
    let pool = owner.as_ref().map_or_else(|| local_session_pool(), |owner| &owner.pool);
    let lease = match pool.take_in(bytes.max(1), caller.data().budget.as_ref()) {
        Ok(lease) => lease,
        Err(reason) if owner.is_some() => return Err(host_failure(caller,
            HostFailure::Unavailable(reason.to_owned()))),
        Err(_) => return Ok(-1),
    };
    charge_host_work(caller, size.saturating_add(64))?;
    let mut bytes = Vec::new();
    bytes.try_reserve_exact(size).map_err(|_| host_failure(caller,
        HostFailure::Unavailable("session allocation failed".to_owned())))?;
    bytes.extend_from_slice(&memory.data(&*caller)[ptr as usize..ptr as usize + size]);
    let value = native_bytes::Value::charged(bytes, lease).map_err(WasmiError::new)?;
    cache.insert(key, SessionCacheEntry {
        value: Arc::new(value), updated_at: now_secs(),
    });
    Ok(0)
}

fn maintain_compute_session_caches() {
    prune_storage_cache_locked(&mut cache_guard(storage_cache(true)));
    prune_session_cache_locked(&mut cache_guard(session_cache()));
    prune_session_cache_locked(&mut cache_guard(local_session_cache()));
    for local in [false, true] {
        prune_tensor_session_cache_locked(&mut cache_guard(tensor_store(local)));
        prune_fixed_tensor_session_cache_locked(&mut cache_guard(fixed_tensor_store(local)));
    }
}

fn compute_session_prefix(data: &HostState) -> Option<String> {
    data.compute_session_scope.as_deref().map(|scope| {
        format!(
            "{}|{}|{}|",
            String::from_utf8_lossy(&data.self_bytes),
            String::from_utf8_lossy(&data.caller_bytes),
            scope
        )
    })
}

fn clear_compute_session_prefix(prefix: &str, local: Option<&str>) -> Result<(), String> {
    session_store(local.is_some())
        .lock()
        .map_err(|_| "session cache poisoned".to_owned())?
        .retain(|key, _| !key.starts_with(local.unwrap_or(prefix)));
    let tensor_prefix = format!("tensor|{}", local.unwrap_or(prefix));
    tensor_store(local.is_some())
        .lock()
        .map_err(|_| "tensor session cache poisoned".to_owned())?
        .retain(|key, _| !key.starts_with(&tensor_prefix));
    let fixed_prefix = format!("tensor_q24|{}", local.unwrap_or(prefix));
    fixed_tensor_store(local.is_some())
        .lock()
        .map_err(|_| "fixed tensor session cache poisoned".to_owned())?
        .retain(|key, _| !key.starts_with(&fixed_prefix));
    Ok(())
}

fn clear_compute_session_scope(data: &HostState) -> Result<(), String> {
    if let Some(owner) = data.session_owner.as_ref() {
        cache_guard(&owner.values).clear();
        cache_guard(&owner.tensors).clear();
        cache_guard(&owner.fixed).clear();
        return Ok(());
    }
    match compute_session_prefix(data) {
        Some(prefix) => {
            let local = data.local_sessions.then(|| local_session_prefix(data));
            clear_compute_session_prefix(&prefix, local.as_deref())
        }
        None => Ok(()),
    }
}

fn tensor_session_namespaced_key(data: &HostState, raw_key: &str) -> String {
    format!("tensor|{}", session_namespaced_key(data, raw_key))
}

fn fixed_tensor_session_namespaced_key(data: &HostState, raw_key: &str) -> String {
    format!("tensor_q24|{}", session_namespaced_key(data, raw_key))
}

fn prune_session_cache_locked(cache: &mut HashMap<String, SessionCacheEntry>) {
    let now = now_secs();
    cache.retain(|_, entry| now - entry.updated_at <= SESSION_CACHE_TTL_SECS);
    if cache.len() > SESSION_CACHE_LIMIT {
        cache.clear();
    }
}

fn prune_tensor_session_cache_locked(cache: &mut HashMap<String, TensorSessionEntry>) {
    let now = now_secs();
    cache.retain(|_, entry| now - entry.updated_at <= TENSOR_SESSION_CACHE_TTL_SECS);
    if cache.len() > TENSOR_SESSION_CACHE_LIMIT {
        cache.clear();
    }
}

fn prune_fixed_tensor_session_cache_locked(cache: &mut HashMap<String, FixedTensorSessionEntry>) {
    let now = now_secs();
    cache.retain(|_, entry| now - entry.updated_at <= TENSOR_SESSION_CACHE_TTL_SECS);
    while cache.len() > FIXED_TENSOR_SESSION_CACHE_LIMIT {
        if let Some(key) = cache
            .iter()
            .min_by(|(_, left), (_, right)| left.updated_at.total_cmp(&right.updated_at))
            .map(|(key, _)| key.clone())
        {
            cache.remove(&key);
        } else {
            break;
        }
    }
}

fn prune_module_cache_locked(cache: &mut HashMap<String, ModuleCacheEntry>) {
    let now = now_secs();
    cache.retain(|_, entry| now - entry.updated_at <= MODULE_CACHE_TTL_SECS);
    if cache.len() > MODULE_CACHE_LIMIT {
        cache.clear();
    }
}

fn storage_weight(storage: &Storage) -> usize {
    storage
        .iter()
        .map(|(key, value)| key.len() + value.len() + 64)
        .sum()
}

fn storage_cache_weight(cache: &HashMap<String, StorageCacheEntry>) -> usize {
    cache.values().map(|entry| entry.weight).sum()
}

fn remove_oldest_storage_cache_entry(cache: &mut HashMap<String, StorageCacheEntry>) {
    if let Some(key) = cache
        .iter()
        .min_by(|(_, left), (_, right)| left.updated_at.total_cmp(&right.updated_at))
        .map(|(key, _)| key.clone())
    {
        cache.remove(&key);
    }
}

fn prune_storage_cache_locked(cache: &mut HashMap<String, StorageCacheEntry>) {
    let now = now_secs();
    cache.retain(|_, entry| now - entry.updated_at <= STORAGE_CACHE_TTL_SECS);
    while cache.len() > STORAGE_CACHE_LIMIT
        || storage_cache_weight(cache) > STORAGE_CACHE_BYTE_LIMIT
    {
        remove_oldest_storage_cache_entry(cache);
    }
}

fn prepare_storage_cache_insert(
    cache: &mut HashMap<String, StorageCacheEntry>,
    key: &str,
    incoming_weight: usize,
) {
    prune_storage_cache_locked(cache);
    if let Some((circle_id, _)) = key.split_once(':') {
        let prefix = format!("{circle_id}:");
        cache.retain(|cached_key, _| cached_key == key || !cached_key.starts_with(&prefix));
    }
    cache.remove(key);
    while !cache.is_empty()
        && (cache.len() >= STORAGE_CACHE_LIMIT
            || storage_cache_weight(cache) + incoming_weight > STORAGE_CACHE_BYTE_LIMIT)
    {
        remove_oldest_storage_cache_entry(cache);
    }
}

fn read_tensor_bytes<'a>(
    caller: &'a mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
) -> Result<&'a [u8], WasmiError> {
    let byte_len = len
        .checked_mul(8)
        .filter(|value| *value <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("tensor input exceeds cap"))?;
    if ptr < 0 {
        return Err(WasmiError::new("negative pointer"));
    }
    charge_host_work(caller, byte_len.saturating_add(64))?;
    let memory = caller.get_export("memory").and_then(Extern::into_memory)
        .ok_or_else(|| WasmiError::new("wasm_v1 memory export missing"))?;
    memory.data(&*caller).get(ptr as usize..)
        .and_then(|bytes| bytes.get(..byte_len))
        .ok_or_else(|| WasmiError::new("guest memory read out of bounds"))
}

fn tensor_output<'a>(
    caller: &'a mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
) -> Result<Option<&'a mut [u8]>, WasmiError> {
    let byte_len = len.checked_mul(8)
        .filter(|value| *value <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("tensor output exceeds cap"))?;
    if ptr < 0 {
        return Ok(None);
    }
    let memory = caller.get_export("memory").and_then(Extern::into_memory)
        .ok_or_else(|| WasmiError::new("wasm_v1 memory export missing"))?;
    Ok(memory.data_mut(&mut *caller).get_mut(ptr as usize..)
        .and_then(|bytes| bytes.get_mut(..byte_len)))
}

fn read_guest_vec<T>(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
    decode: fn([u8; 8]) -> T,
) -> Result<native_bytes::Value<T>, WasmiError> {
    let local = caller.data().local_sessions;
    let quota = caller.data().budget.clone();
    let raw = read_tensor_bytes(caller, ptr, len)?;
    let result = (|| {
        let size = len.checked_mul(std::mem::size_of::<T>())
            .ok_or("tensor input size overflow")?;
        let lease = if local { Some(compute_memory().take_in(size, quota.as_ref())?) } else { None };
        let mut out = Vec::new();
        if local {
            out.try_reserve_exact(len).map_err(|_| "tensor input allocation failed")?;
        } else {
            out.reserve_exact(len);
        }
        for chunk in raw.chunks_exact(8) {
            out.push(decode([
                chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5], chunk[6], chunk[7],
            ]));
        }
        match lease {
            Some(lease) => native_bytes::Value::charged(out, lease),
            None => Ok(out.into()),
        }
    })();
    result.map_err(|reason| host_failure(caller, HostFailure::Unavailable(reason.to_owned())))
}

fn read_guest_f64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
) -> Result<native_bytes::Value<f64>, WasmiError> {
    read_guest_vec(caller, ptr, len, f64::from_le_bytes)
}

fn tensor_result<T: Copy>(
    local: bool,
    count: usize,
    zero: T,
    quota: Option<&Arc<native_bytes::Budget>>,
) -> Result<native_bytes::Value<T>, HostFailure> {
    if !local {
        return Ok(vec![zero; count].into());
    }
    let size = count.checked_mul(std::mem::size_of::<T>())
        .ok_or_else(|| HostFailure::Unavailable("tensor result size overflow".to_owned()))?;
    let lease = compute_memory().take_in(size, quota)
        .map_err(|error| HostFailure::Unavailable(error.to_owned()))?;
    let mut values = Vec::new();
    values.try_reserve_exact(count)
        .map_err(|_| HostFailure::Unavailable("tensor result allocation failed".to_owned()))?;
    values.resize(count, zero);
    native_bytes::Value::charged(values, lease)
        .map_err(|error| HostFailure::Unavailable(error.to_owned()))
}

fn read_guest_f32_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
) -> Result<native_bytes::Value<f32>, WasmiError> {
    read_guest_vec(caller, ptr, len, |bytes| f64::from_le_bytes(bytes) as f32)
}

fn write_guest_f64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    values: &[f64],
    arithmetic: bool,
) -> Result<i32, WasmiError> {
    let compute = arithmetic && caller.data().execution_profile == ExecutionProfile::Compute;
    let Some(bytes) = tensor_output(caller, ptr, values.len())? else {
        return Ok(-2);
    };
    for (chunk, value) in bytes.chunks_exact_mut(8).zip(values) {
        chunk.copy_from_slice(&float_ops::bytes(*value, compute));
    }
    Ok(bytes.len() as i32)
}

fn require_compute_profile(caller: &Caller<'_, HostState>) -> Result<(), WasmiError> {
    if caller.data().execution_profile == ExecutionProfile::Compute && caller.data().is_view {
        Ok(())
    } else {
        Err(WasmiError::new(
            "compute tensor import requires compute view",
        ))
    }
}

fn read_guest_i64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
) -> Result<native_bytes::Value<i64>, WasmiError> {
    require_compute_profile(caller)?;
    read_guest_vec(caller, ptr, len, i64::from_le_bytes)
}

fn write_guest_i64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    values: &[i64],
) -> Result<i32, WasmiError> {
    require_compute_profile(caller)?;
    let Some(bytes) = tensor_output(caller, ptr, values.len())? else {
        return Ok(-2);
    };
    for (chunk, value) in bytes.chunks_exact_mut(8).zip(values) {
        chunk.copy_from_slice(&value.to_le_bytes());
    }
    Ok(bytes.len() as i32)
}

fn compute_error(error: &'static str) -> WasmiError {
    WasmiError::new(error)
}

fn bits_to_fp64(bits: i64) -> f64 {
    f64::from_bits(bits as u64)
}

fn host_tensor_load_i8_kv(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    off: i32,
    n: i32,
    scale_bits: i64,
) -> Result<i32, WasmiError> {
    if off < 0 || n <= 0 {
        return Ok(-1);
    }
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    charge_host_work(caller, n as usize)?;
    let scale = bits_to_fp64(scale_bits);
    let value = match caller.data().storage.get(&key) {
        Some(value) => Arc::clone(value),
        None => return Ok(-3),
    };
    let start = off as usize;
    let count = n as usize;
    if start.checked_add(count).filter(|end| *end <= value.len()).is_none() {
        return Ok(-1);
    }
    if dst_ptr < 0 {
        return Ok(-2);
    }
    let size = count.checked_mul(8).filter(|size| *size <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("guest write exceeds cap"))?;
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    let Some(out) = tensor_output(caller, dst_ptr, count)? else {
        return Ok(-2);
    };
    for (cell, byte) in out.chunks_exact_mut(8).zip(&value[start..start + count]) {
        let signed = i8::from_ne_bytes([*byte]) as f64;
        cell.copy_from_slice(&float_ops::bytes(signed * scale, compute));
    }
    Ok(size as i32)
}

fn host_tensor_matmul_fp(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    lhs_ptr: i32,
    rhs_ptr: i32,
    m: i32,
    k: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if m <= 0 || k <= 0 || n <= 0 {
        return Ok(-1);
    }
    let (m, k, n) = (m as usize, k as usize, n as usize);
    charge_host_work(caller, host_work_product(&[m, k, n])?)?;
    let lhs = read_guest_f64_vec(caller, lhs_ptr, m * k)?;
    let rhs = read_guest_f64_vec(caller, rhs_ptr, k * n)?;
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    let Some(dst) = tensor_output(caller, dst_ptr, m * n)? else {
        return Ok(-2);
    };
    for (r, row) in dst.chunks_exact_mut(n * 8).enumerate() {
        let r_k = r * k;
        for (c, cell) in row.chunks_exact_mut(8).enumerate() {
            let mut acc = 0.0_f64;
            for i in 0..k {
                acc += lhs[r_k + i] * rhs[i * n + c];
            }
            cell.copy_from_slice(&float_ops::bytes(acc, compute));
        }
    }
    Ok(dst.len() as i32)
}

fn host_tensor_rmsnorm_fp(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n: i32,
    gamma_ptr: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_host_work(caller, n.saturating_mul(3))?;
    let mut values = read_guest_f64_vec(caller, addr_ptr, n)?;
    let gamma = read_guest_f64_vec(caller, gamma_ptr, n)?;
    let sum_sq = values
        .iter()
        .fold(0.0_f64, |acc, value| acc + (value * value));
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    let inv_rms = 1.0_f64 / float_ops::sqrt((sum_sq / n as f64) + 1e-5_f64, compute);
    for idx in 0..n {
        values[idx] = gamma[idx] * values[idx] * inv_rms;
    }
    write_guest_f64_vec(caller, addr_ptr, &values, true)
}

fn host_tensor_silu_fp(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_host_work(caller, n.saturating_mul(4))?;
    let mut values = read_guest_f64_vec(caller, addr_ptr, n)?;
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    for value in values.iter_mut() {
        let sig = 1.0_f64 / (1.0_f64 + float_ops::exp(-*value, compute));
        *value *= sig;
    }
    write_guest_f64_vec(caller, addr_ptr, &values, true)
}

fn host_tensor_elemwise_mul_fp(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    src_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_host_work(caller, n)?;
    let mut dst = read_guest_f64_vec(caller, dst_ptr, n)?;
    let src = read_guest_f64_vec(caller, src_ptr, n)?;
    for idx in 0..n {
        dst[idx] *= src[idx];
    }
    write_guest_f64_vec(caller, dst_ptr, &dst, true)
}

fn host_tensor_residual_add_fp(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    src_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_host_work(caller, n)?;
    let mut dst = read_guest_f64_vec(caller, dst_ptr, n)?;
    let src = read_guest_f64_vec(caller, src_ptr, n)?;
    for idx in 0..n {
        dst[idx] += src[idx];
    }
    write_guest_f64_vec(caller, dst_ptr, &dst, true)
}

fn host_tensor_rope_apply_fp(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n_dim: i32,
    pos: i32,
    base_bits: i64,
) -> Result<i32, WasmiError> {
    if n_dim <= 0 || (n_dim & 1) != 0 || pos < 0 {
        return Ok(-1);
    }
    let n_dim = n_dim as usize;
    charge_host_work(caller, n_dim.saturating_mul(8))?;
    let mut values = read_guest_f64_vec(caller, addr_ptr, n_dim)?;
    let half = n_dim / 2;
    let base = bits_to_fp64(base_bits);
    let pf = pos as f64;
    let nf = n_dim as f64;
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    for idx in 0..half {
        let exp_term = (2.0_f64 * idx as f64) / nf;
        let inv_freq = 1.0_f64 / float_ops::pow(base, exp_term, compute);
        let angle = pf * inv_freq;
        let (s, c) = float_ops::sin_cos(angle, compute);
        let re = values[idx];
        let im = values[idx + half];
        values[idx] = re * c - im * s;
        values[idx + half] = re * s + im * c;
    }
    write_guest_f64_vec(caller, addr_ptr, &values, true)
}

fn attention_ctx_from_slices<B: AsMut<[f64]>, E: From<&'static str>>(
    q: &[f64],
    k: &[f64],
    v: &[f64],
    t_total: usize,
    n_q_heads: usize,
    n_kv_heads: usize,
    head_dim: usize,
    compute: bool,
    mut alloc: impl FnMut(usize) -> Result<B, E>,
) -> Result<B, E> {
    let kv_dim = n_kv_heads * head_dim;
    let count = n_q_heads * head_dim;
    let mut output = alloc(count)?;
    let ctx = output.as_mut();
    let group = n_q_heads / n_kv_heads;
    let inv_sqrt_d = 1.0_f64 / float_ops::sqrt(head_dim as f64, compute);
    let mut scratch = alloc(t_total)?;
    let scores = scratch.as_mut();
    if ctx.len() != count || scores.len() != t_total {
        return Err("tensor output shape differs".into());
    }
    for q_head in 0..n_q_heads {
        let kv_head = q_head / group;
        let q_off = q_head * head_dim;
        for t in 0..t_total {
            let k_off = t * kv_dim + kv_head * head_dim;
            let mut acc = 0.0_f64;
            for d in 0..head_dim {
                acc += q[q_off + d] * k[k_off + d];
            }
            scores[t] = acc * inv_sqrt_d;
        }
        let mut max_score = f64::NEG_INFINITY;
        for score in scores.iter() {
            if *score > max_score {
                max_score = *score;
            }
        }
        let mut sum_exp = 0.0_f64;
        for score in scores.iter_mut() {
            *score = float_ops::exp(*score - max_score, compute);
            sum_exp += *score;
        }
        let inv_sum = 1.0_f64 / sum_exp;
        for score in scores.iter_mut() {
            *score *= inv_sum;
        }
        for d in 0..head_dim {
            let mut acc = 0.0_f64;
            for t in 0..t_total {
                let v_off = t * kv_dim + kv_head * head_dim + d;
                acc += scores[t] * v[v_off];
            }
            ctx[q_off + d] = acc;
        }
    }
    Ok(output)
}

fn host_tensor_attention_kv_fp(
    caller: &mut Caller<'_, HostState>,
    q_ptr: i32,
    k_ptr: i32,
    v_ptr: i32,
    ctx_ptr: i32,
    t_total: i32,
    n_q_heads: i32,
    n_kv_heads: i32,
    head_dim: i32,
) -> Result<i32, WasmiError> {
    if t_total <= 0
        || n_q_heads <= 0
        || n_kv_heads <= 0
        || head_dim <= 0
        || n_q_heads % n_kv_heads != 0
    {
        return Ok(-1);
    }
    let (t_total, n_q_heads, n_kv_heads, head_dim) = (
        t_total as usize,
        n_q_heads as usize,
        n_kv_heads as usize,
        head_dim as usize,
    );
    charge_host_work(
        caller,
        host_work_product(&[t_total, n_q_heads, head_dim, 2])?,
    )?;
    let kv_dim = n_kv_heads * head_dim;
    let q = read_guest_f64_vec(caller, q_ptr, n_q_heads * head_dim)?;
    let k = read_guest_f64_vec(caller, k_ptr, t_total * kv_dim)?;
    let v = read_guest_f64_vec(caller, v_ptr, t_total * kv_dim)?;
    let local = caller.data().local_sessions;
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    let ctx = attention_ctx_from_slices(&q, &k, &v, t_total, n_q_heads, n_kv_heads, head_dim, compute,
        |count| tensor_result(local, count, 0.0, caller.data().budget.as_ref()))
        .map_err(|error| host_failure(caller, error))?;
    write_guest_f64_vec(caller, ctx_ptr, &ctx, true)
}

fn host_tensor_append_vec_fp(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    pos: i32,
    src_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if pos < 0 || n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_host_work(caller, n)?;
    let src = read_guest_f64_vec(caller, src_ptr, n)?;
    let byte_offset = (pos as usize)
        .checked_mul(n)
        .and_then(|value| value.checked_mul(8))
        .and_then(|value| i32::try_from(value).ok())
        .ok_or_else(|| WasmiError::new("tensor append offset overflow"))?;
    let dst_elem_ptr = dst_ptr
        .checked_add(byte_offset)
        .ok_or_else(|| WasmiError::new("tensor append pointer overflow"))?;
    write_guest_f64_vec(caller, dst_elem_ptr, &src, false)
}

fn host_tensor_session_append_kv_fp(
    caller: &mut Caller<'_, HostState>,
    key_ptr: i32,
    key_len: i32,
    layer_idx: i32,
    position: i32,
    k_ptr: i32,
    v_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if layer_idx < 0 || position < 0 || n <= 0 {
        return Ok(-1);
    }
    let raw_key = read_guest_utf8(caller, key_ptr, key_len)?;
    let cache_key = tensor_session_namespaced_key(caller.data(), &raw_key);
    charge_host_work(caller, (n as usize).saturating_mul(2))?;
    let k_src = read_guest_f64_vec(caller, k_ptr, n as usize)?;
    let v_src = read_guest_f64_vec(caller, v_ptr, n as usize)?;
    let owner = caller.data().session_owner.clone();
    let mut guard = cache_guard(tensor_table(&owner, caller.data().local_sessions));
    if owner.is_none() { prune_tensor_session_cache_locked(&mut guard); }
    let entry = match guard.get_mut(&cache_key) {
        Some(entry) => entry,
        None => return Ok(-3),
    };
    let (layer_idx, position, n) = (layer_idx as usize, position as usize, n as usize);
    if layer_idx >= entry.n_layers || position >= entry.max_t || n != entry.kv_dim {
        return Ok(-1);
    }
    let _held = tensor_hold(caller.data().budget.as_ref(), entry._lease.as_ref(),
        entry.k_cache.capacity(), entry.v_cache.capacity(), cache_key.capacity())
        .map_err(|error| host_failure(caller, error))?;
    let layer_span = entry.max_t * entry.kv_dim;
    let off = layer_idx * layer_span + position * entry.kv_dim;
    entry.k_cache[off..off + entry.kv_dim].copy_from_slice(&k_src);
    entry.v_cache[off..off + entry.kv_dim].copy_from_slice(&v_src);
    entry.updated_at = now_secs();
    Ok(0)
}

fn host_tensor_session_attention_kv_fp(
    caller: &mut Caller<'_, HostState>,
    key_ptr: i32,
    key_len: i32,
    q_ptr: i32,
    ctx_ptr: i32,
    t_total: i32,
    layer_idx: i32,
    n_q_heads: i32,
    n_kv_heads: i32,
    head_dim: i32,
) -> Result<i32, WasmiError> {
    if t_total <= 0
        || layer_idx < 0
        || n_q_heads <= 0
        || n_kv_heads <= 0
        || head_dim <= 0
        || n_q_heads % n_kv_heads != 0
    {
        return Ok(-1);
    }
    let raw_key = read_guest_utf8(caller, key_ptr, key_len)?;
    let cache_key = tensor_session_namespaced_key(caller.data(), &raw_key);
    let (t_total, layer_idx, n_q_heads, n_kv_heads, head_dim) = (
        t_total as usize,
        layer_idx as usize,
        n_q_heads as usize,
        n_kv_heads as usize,
        head_dim as usize,
    );
    charge_host_work(
        caller,
        host_work_product(&[t_total, n_q_heads, head_dim, 2])?,
    )?;
    let q = read_guest_f64_vec(caller, q_ptr, n_q_heads * head_dim)?;
    let local = caller.data().local_sessions;
    let kv_dim = n_kv_heads * head_dim;
    let ctx = {
        let owner = caller.data().session_owner.clone();
        let mut guard = cache_guard(tensor_table(&owner, caller.data().local_sessions));
        if owner.is_none() { prune_tensor_session_cache_locked(&mut guard); }
        let entry = match guard.get_mut(&cache_key) {
            Some(entry) => entry,
            None => return Ok(-3),
        };
        if layer_idx >= entry.n_layers || t_total > entry.max_t || entry.kv_dim != kv_dim {
            return Ok(-1);
        }
        let _held = tensor_hold(caller.data().budget.as_ref(), entry._lease.as_ref(),
            entry.k_cache.capacity(), entry.v_cache.capacity(), cache_key.capacity())
            .map_err(|error| host_failure(caller, error))?;
        let layer_span = entry.max_t * entry.kv_dim;
        let layer_off = layer_idx * layer_span;
        let used = t_total * entry.kv_dim;
        entry.updated_at = now_secs();
        attention_ctx_from_slices(
            &q,
            &entry.k_cache[layer_off..layer_off + used],
            &entry.v_cache[layer_off..layer_off + used],
            t_total,
            n_q_heads,
            n_kv_heads,
            head_dim,
            caller.data().execution_profile == ExecutionProfile::Compute,
            |count| tensor_result(local, count, 0.0, caller.data().budget.as_ref()),
        )
    }.map_err(|error| host_failure(caller, error))?;
    write_guest_f64_vec(caller, ctx_ptr, &ctx, true)
}

fn host_tensor_argmax_fp(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    charge_host_work(caller, n as usize)?;
    let values = read_guest_f64_vec(caller, addr_ptr, n as usize)?;
    let mut best_idx = 0usize;
    let mut best_val = f64::NEG_INFINITY;
    for (idx, value) in values.iter().enumerate() {
        if *value > best_val {
            best_val = *value;
            best_idx = idx;
        }
    }
    Ok(best_idx as i32)
}

fn host_tensor_linear_i8_kv(
    caller: &mut Caller<'_, HostState>,
    out_ptr: i32,
    in_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    in_dim: i32,
    out_dim: i32,
    scale_bits: i64,
) -> Result<i32, WasmiError> {
    if in_dim <= 0 || out_dim <= 0 {
        return Ok(-1);
    }
    let (in_dim, out_dim) = (in_dim as usize, out_dim as usize);
    charge_host_work(caller, host_work_product(&[in_dim, out_dim])?)?;
    let input_f32 = read_guest_f32_vec(caller, in_ptr, in_dim)?;
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    let scale = bits_to_fp64(scale_bits) as f32;
    let value = match caller.data().storage.get(&key) {
        Some(value) => Arc::clone(value),
        None => return Ok(-3),
    };
    if value.len() != in_dim * out_dim {
        return Ok(-1);
    }
    let compute = caller.data().execution_profile == ExecutionProfile::Compute;
    let Some(out) = tensor_output(caller, out_ptr, out_dim)? else {
        return Ok(-2);
    };
    out.fill(0);
    for (i, input_v) in input_f32.iter().copied().enumerate() {
        let row_off = i * out_dim;
        for (o, cell) in out.chunks_exact_mut(8).enumerate() {
            let acc = f32::from_le_bytes([cell[0], cell[1], cell[2], cell[3]]);
            let signed = i8::from_ne_bytes([value[row_off + o]]) as f32;
            cell[..4].copy_from_slice(&(acc + input_v * signed * scale).to_le_bytes());
        }
    }
    for cell in out.chunks_exact_mut(8) {
        let acc = f32::from_le_bytes([cell[0], cell[1], cell[2], cell[3]]);
        cell.copy_from_slice(&float_ops::bytes(acc as f64, compute));
    }
    Ok(out.len() as i32)
}

fn host_tensor_argmax_i8_kv(
    caller: &mut Caller<'_, HostState>,
    in_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    rows: i32,
    cols: i32,
    scale_bits: i64,
) -> Result<i32, WasmiError> {
    if rows <= 0 || cols <= 0 {
        return Ok(-1);
    }
    let (rows, cols) = (rows as usize, cols as usize);
    charge_host_work(caller, host_work_product(&[rows, cols])?)?;
    let input_f32 = read_guest_f32_vec(caller, in_ptr, cols)?;
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    let scale = bits_to_fp64(scale_bits) as f32;
    let (best_idx, _) = {
        let data = caller.data();
        let value = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        if value.len() != rows * cols {
            return Ok(-1);
        }
        let mut best_idx = 0usize;
        let mut best_val = f32::NEG_INFINITY;
        for row in 0..rows {
            let row_off = row * cols;
            let mut acc = 0.0_f32;
            for col in 0..cols {
                let signed = i8::from_ne_bytes([value[row_off + col]]) as f32;
                acc += input_f32[col] * signed * scale;
            }
            if acc > best_val {
                best_val = acc;
                best_idx = row;
            }
        }
        (best_idx, best_val)
    };
    Ok(best_idx as i32)
}

fn host_tensor_top1_i8_kv(
    caller: &mut Caller<'_, HostState>,
    in_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    rows: i32,
    cols: i32,
    scale_bits: i64,
    score_out_ptr: i32,
) -> Result<i32, WasmiError> {
    if rows <= 0 || cols <= 0 {
        return Ok(-1);
    }
    let (rows, cols) = (rows as usize, cols as usize);
    charge_host_work(caller, host_work_product(&[rows, cols])?)?;
    let input_f32 = read_guest_f32_vec(caller, in_ptr, cols)?;
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    let scale = bits_to_fp64(scale_bits) as f32;
    let (best_idx, best_val) = {
        let data = caller.data();
        let value = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        if value.len() != rows * cols {
            return Ok(-1);
        }
        let mut best_idx = 0usize;
        let mut best_val = f32::NEG_INFINITY;
        for row in 0..rows {
            let row_off = row * cols;
            let mut acc = 0.0_f32;
            for col in 0..cols {
                let signed = i8::from_ne_bytes([value[row_off + col]]) as f32;
                acc += input_f32[col] * signed * scale;
            }
            if acc > best_val {
                best_val = acc;
                best_idx = row;
            }
        }
        (best_idx, best_val)
    };
    let status = write_guest_f64_vec(caller, score_out_ptr, &[best_val as f64], true)?;
    if caller.data().query_output && status < 0 {
        Ok(status)
    } else {
        Ok(best_idx as i32)
    }
}

fn host_tensor_load_i8_kv_q24(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    off: i32,
    n: i32,
    scale_bits: i64,
) -> Result<i32, WasmiError> {
    require_compute_profile(caller)?;
    if off < 0 || n <= 0 {
        return Ok(-1);
    }
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    charge_q24_work(caller, n as usize)?;
    let scale = deterministic_tensor::scale_bits_to_q24(scale_bits).map_err(compute_error)?;
    let value = match caller.data().storage.get(&key) {
        Some(value) => Arc::clone(value),
        None => return Ok(-3),
    };
    let start = off as usize;
    let count = n as usize;
    if start.checked_add(count).filter(|end| *end <= value.len()).is_none() {
        return Ok(-1);
    }
    let source = &value[start..start + count];
    let mut table = [0_i64; 256];
    let mut checked = [false; 256];
    let mut digest = Sha256::new();
    for byte in source {
        let index = *byte as usize;
        if !checked[index] {
            table[index] = (i128::from(*byte as i8) * i128::from(scale))
                .try_into().map_err(|_| compute_error("fixed point overflow"))?;
            checked[index] = true;
        }
        digest.update(table[index].to_le_bytes());
    }
    let key_digest: [u8; 32] = Sha256::digest(key.as_bytes()).into();
    let output_digest: [u8; 32] = digest.finalize().into();
    let offset_bytes = off.to_le_bytes();
    let count_bytes = n.to_le_bytes();
    let scale_bytes = scale.to_le_bytes();
    let status = match tensor_output(caller, dst_ptr, count)? {
        None => -2,
        Some(out) => {
            for (cell, byte) in out.chunks_exact_mut(8).zip(source) {
                cell.copy_from_slice(&table[*byte as usize].to_le_bytes());
            }
            out.len() as i32
        }
    };
    record_compute_trace(
        caller,
        "tensor_load_i8_q24",
        &[
            &key_digest,
            &offset_bytes,
            &count_bytes,
            &scale_bytes,
            &output_digest,
        ],
    );
    Ok(status)
}

fn host_tensor_rmsnorm_q24(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n: i32,
    gamma_ptr: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_q24_work(caller, n.saturating_mul(4))?;
    let values = read_guest_i64_vec(caller, addr_ptr, n)?;
    let gamma = read_guest_i64_vec(caller, gamma_ptr, n)?;
    let local = caller.data().local_sessions;
    let output = if local {
        deterministic_tensor::rmsnorm_with(&values, &gamma,
            |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
    } else {
        deterministic_tensor::rmsnorm(&values, &gamma)
            .map(native_bytes::Value::from).map_err(HostFailure::from)
    }
        .map_err(|error| host_failure(caller, error))?;
    let values_digest = trace_i64_digest(&values);
    let gamma_digest = trace_i64_digest(&gamma);
    let output_digest = trace_i64_digest(&output);
    let count_bytes = (n as u64).to_le_bytes();
    let status = write_guest_i64_vec(caller, addr_ptr, &output)?;
    record_compute_trace(
        caller,
        "tensor_rmsnorm_q24",
        &[&count_bytes, &values_digest, &gamma_digest, &output_digest],
    );
    Ok(status)
}

fn host_tensor_silu_q24(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_q24_work(caller, n.saturating_mul(12))?;
    let values = read_guest_i64_vec(caller, addr_ptr, n)?;
    let local = caller.data().local_sessions;
    let output = if local {
        deterministic_tensor::silu_with(&values, |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
    } else {
        deterministic_tensor::silu(&values)
            .map(native_bytes::Value::from).map_err(HostFailure::from)
    }
        .map_err(|error| host_failure(caller, error))?;
    let values_digest = trace_i64_digest(&values);
    let output_digest = trace_i64_digest(&output);
    let count_bytes = (n as u64).to_le_bytes();
    let status = write_guest_i64_vec(caller, addr_ptr, &output)?;
    record_compute_trace(
        caller,
        "tensor_silu_q24",
        &[&count_bytes, &values_digest, &output_digest],
    );
    Ok(status)
}

fn host_tensor_elemwise_mul_q24(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    src_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_q24_work(caller, n)?;
    let left = read_guest_i64_vec(caller, dst_ptr, n)?;
    let right = read_guest_i64_vec(caller, src_ptr, n)?;
    let local = caller.data().local_sessions;
    let output = if local {
        deterministic_tensor::elementwise_mul_with(&left, &right,
            |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
    } else {
        deterministic_tensor::elementwise_mul(&left, &right)
            .map(native_bytes::Value::from).map_err(HostFailure::from)
    }
        .map_err(|error| host_failure(caller, error))?;
    let left_digest = trace_i64_digest(&left);
    let right_digest = trace_i64_digest(&right);
    let output_digest = trace_i64_digest(&output);
    let count_bytes = (n as u64).to_le_bytes();
    let status = write_guest_i64_vec(caller, dst_ptr, &output)?;
    record_compute_trace(
        caller,
        "tensor_elementwise_mul_q24",
        &[&count_bytes, &left_digest, &right_digest, &output_digest],
    );
    Ok(status)
}

fn host_tensor_residual_add_q24(
    caller: &mut Caller<'_, HostState>,
    dst_ptr: i32,
    src_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if n <= 0 {
        return Ok(-1);
    }
    let n = n as usize;
    charge_q24_work(caller, n)?;
    let left = read_guest_i64_vec(caller, dst_ptr, n)?;
    let right = read_guest_i64_vec(caller, src_ptr, n)?;
    let local = caller.data().local_sessions;
    let output = if local {
        deterministic_tensor::residual_add_with(&left, &right,
            |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
    } else {
        deterministic_tensor::residual_add(&left, &right)
            .map(native_bytes::Value::from).map_err(HostFailure::from)
    }
        .map_err(|error| host_failure(caller, error))?;
    let left_digest = trace_i64_digest(&left);
    let right_digest = trace_i64_digest(&right);
    let output_digest = trace_i64_digest(&output);
    let count_bytes = (n as u64).to_le_bytes();
    let status = write_guest_i64_vec(caller, dst_ptr, &output)?;
    record_compute_trace(
        caller,
        "tensor_residual_add_q24",
        &[&count_bytes, &left_digest, &right_digest, &output_digest],
    );
    Ok(status)
}

fn host_tensor_rope_apply_q24(
    caller: &mut Caller<'_, HostState>,
    addr_ptr: i32,
    n_dim: i32,
    position: i32,
    frequencies_ptr: i32,
) -> Result<i32, WasmiError> {
    if n_dim <= 0 || n_dim & 1 != 0 || position < 0 {
        return Ok(-1);
    }
    let n_dim = n_dim as usize;
    charge_q24_work(caller, n_dim.saturating_mul(20))?;
    let values = read_guest_i64_vec(caller, addr_ptr, n_dim)?;
    let frequencies = read_guest_i64_vec(caller, frequencies_ptr, n_dim / 2)?;
    let local = caller.data().local_sessions;
    let output = if local {
        deterministic_tensor::rope_with(&values, &frequencies, position as i64,
            |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
    } else {
        deterministic_tensor::rope(&values, &frequencies, position as i64)
            .map(native_bytes::Value::from).map_err(HostFailure::from)
    }
        .map_err(|error| host_failure(caller, error))?;
    let values_digest = trace_i64_digest(&values);
    let frequencies_digest = trace_i64_digest(&frequencies);
    let output_digest = trace_i64_digest(&output);
    let dimension_bytes = (n_dim as u64).to_le_bytes();
    let position_bytes = position.to_le_bytes();
    let status = write_guest_i64_vec(caller, addr_ptr, &output)?;
    record_compute_trace(
        caller,
        "tensor_rope_q24",
        &[
            &dimension_bytes,
            &position_bytes,
            &values_digest,
            &frequencies_digest,
            &output_digest,
        ],
    );
    Ok(status)
}

fn host_tensor_linear_i8_kv_q24(
    caller: &mut Caller<'_, HostState>,
    out_ptr: i32,
    in_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    in_dim: i32,
    out_dim: i32,
    scale_bits: i64,
) -> Result<i32, WasmiError> {
    if in_dim <= 0 || out_dim <= 0 {
        return Ok(-1);
    }
    let (in_dim, out_dim) = (in_dim as usize, out_dim as usize);
    charge_q24_work(caller, host_work_product(&[in_dim, out_dim])?)?;
    let input = read_guest_i64_vec(caller, in_ptr, in_dim)?;
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    let scale = deterministic_tensor::scale_bits_to_q24(scale_bits).map_err(compute_error)?;
    let destination = tensor_output(caller, out_ptr, out_dim).map(|bytes| bytes.is_some());
    let local = caller.data().local_sessions;
    let (output, output_digest) = {
        let data = caller.data();
        let weights = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        if matches!(&destination, Ok(true)) {
            let output = if local {
                deterministic_tensor::linear_i8_with(&input, weights, out_dim, scale,
                    |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
            } else {
                deterministic_tensor::linear_i8(&input, weights, out_dim, scale)
                    .map(native_bytes::Value::from).map_err(HostFailure::from)
            }
                .map_err(|error| host_failure(caller, error))?;
            let digest = trace_i64_digest(&output);
            (Some(output), digest)
        } else {
            let mut hasher = Sha256::new();
            deterministic_tensor::visit_linear_i8(&input, weights, out_dim, scale, |tile| {
                for value in tile {
                    hasher.update(value.to_le_bytes());
                }
            }).map_err(compute_error)?;
            (None, hasher.finalize().into())
        }
    };
    let key_digest: [u8; 32] = Sha256::digest(key.as_bytes()).into();
    let input_digest = trace_i64_digest(&input);
    let input_dimension_bytes = (in_dim as u64).to_le_bytes();
    let output_dimension_bytes = (out_dim as u64).to_le_bytes();
    let scale_bytes = scale.to_le_bytes();
    let status = if let Some(output) = output {
        write_guest_i64_vec(caller, out_ptr, &output)?
    } else {
        destination?;
        -2
    };
    record_compute_trace(
        caller,
        "tensor_linear_i8_q24",
        &[
            &key_digest,
            &input_dimension_bytes,
            &output_dimension_bytes,
            &scale_bytes,
            &input_digest,
            &output_digest,
        ],
    );
    Ok(status)
}

fn host_tensor_top1_i8_kv_q24(
    caller: &mut Caller<'_, HostState>,
    in_ptr: i32,
    key_ptr: i32,
    key_len: i32,
    rows: i32,
    cols: i32,
    scale_bits: i64,
    score_out_ptr: i32,
) -> Result<i32, WasmiError> {
    if rows <= 0 || cols <= 0 {
        return Ok(-1);
    }
    let (rows, cols) = (rows as usize, cols as usize);
    charge_q24_work(caller, host_work_product(&[rows, cols])?)?;
    let input = read_guest_i64_vec(caller, in_ptr, cols)?;
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    let scale = deterministic_tensor::scale_bits_to_q24(scale_bits).map_err(compute_error)?;
    let (index, score) = {
        let data = caller.data();
        let weights = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        deterministic_tensor::top1_i8(&input, weights, rows, scale).map_err(compute_error)?
    };
    let index = i32::try_from(index).map_err(|_| WasmiError::new("top1 index overflow"))?;
    let status = write_guest_i64_vec(caller, score_out_ptr, &[score])?;
    let key_digest: [u8; 32] = Sha256::digest(key.as_bytes()).into();
    let input_digest = trace_i64_digest(&input);
    let rows_bytes = (rows as u64).to_le_bytes();
    let columns_bytes = (cols as u64).to_le_bytes();
    let scale_bytes = scale.to_le_bytes();
    let index_bytes = index.to_le_bytes();
    let score_bytes = score.to_le_bytes();
    record_compute_trace(
        caller,
        "tensor_top1_i8_q24",
        &[
            &key_digest,
            &rows_bytes,
            &columns_bytes,
            &scale_bytes,
            &input_digest,
            &index_bytes,
            &score_bytes,
        ],
    );
    Ok(if caller.data().query_output && status < 0 { status } else { index })
}

fn host_tensor_session_append_kv_q24(
    caller: &mut Caller<'_, HostState>,
    key_ptr: i32,
    key_len: i32,
    layer_idx: i32,
    position: i32,
    key_values_ptr: i32,
    values_ptr: i32,
    n: i32,
) -> Result<i32, WasmiError> {
    if layer_idx < 0 || position < 0 || n <= 0 {
        return Ok(-1);
    }
    let raw_key = read_guest_utf8(caller, key_ptr, key_len)?;
    let cache_key = fixed_tensor_session_namespaced_key(caller.data(), &raw_key);
    charge_q24_work(caller, (n as usize).saturating_mul(2))?;
    let key_values = read_guest_i64_vec(caller, key_values_ptr, n as usize)?;
    let values = read_guest_i64_vec(caller, values_ptr, n as usize)?;
    let owner = caller.data().session_owner.clone();
    let mut cache = fixed_table(&owner, caller.data().local_sessions)
        .lock()
        .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
    if owner.is_none() { prune_fixed_tensor_session_cache_locked(&mut cache); }
    let entry = match cache.get_mut(&cache_key) {
        Some(entry) => entry,
        None => return Ok(-3),
    };
    let (layer_idx, position, n) = (layer_idx as usize, position as usize, n as usize);
    if layer_idx >= entry.n_layers || position >= entry.max_t || n != entry.kv_dim {
        return Ok(-1);
    }
    let _held = tensor_hold(caller.data().budget.as_ref(), entry._lease.as_ref(),
        entry.k_cache.capacity(), entry.v_cache.capacity(), cache_key.capacity())
        .map_err(|error| host_failure(caller, error))?;
    let layer_span = entry.max_t * entry.kv_dim;
    let offset = layer_idx * layer_span + position * entry.kv_dim;
    entry.k_cache[offset..offset + entry.kv_dim].copy_from_slice(&key_values);
    entry.v_cache[offset..offset + entry.kv_dim].copy_from_slice(&values);
    entry.updated_at = now_secs();
    drop(cache);
    let key_digest: [u8; 32] = Sha256::digest(raw_key.as_bytes()).into();
    let key_values_digest = trace_i64_digest(&key_values);
    let values_digest = trace_i64_digest(&values);
    let layer_bytes = (layer_idx as u64).to_le_bytes();
    let position_bytes = (position as u64).to_le_bytes();
    let count_bytes = (n as u64).to_le_bytes();
    record_compute_trace(
        caller,
        "tensor_session_append_q24",
        &[
            &key_digest,
            &layer_bytes,
            &position_bytes,
            &count_bytes,
            &key_values_digest,
            &values_digest,
        ],
    );
    Ok(0)
}

fn host_tensor_session_attention_kv_q24(
    caller: &mut Caller<'_, HostState>,
    key_ptr: i32,
    key_len: i32,
    query_ptr: i32,
    output_ptr: i32,
    sequence_len: i32,
    layer_idx: i32,
    query_heads: i32,
    kv_heads: i32,
    head_dim: i32,
    scale: i64,
) -> Result<i32, WasmiError> {
    if sequence_len <= 0
        || layer_idx < 0
        || query_heads <= 0
        || kv_heads <= 0
        || head_dim <= 0
        || query_heads % kv_heads != 0
    {
        return Ok(-1);
    }
    let raw_key = read_guest_utf8(caller, key_ptr, key_len)?;
    let cache_key = fixed_tensor_session_namespaced_key(caller.data(), &raw_key);
    let (sequence_len, layer_idx, query_heads, kv_heads, head_dim) = (
        sequence_len as usize,
        layer_idx as usize,
        query_heads as usize,
        kv_heads as usize,
        head_dim as usize,
    );
    charge_q24_work(
        caller,
        host_work_product(&[sequence_len, query_heads, head_dim, 3])?,
    )?;
    let query = read_guest_i64_vec(caller, query_ptr, query_heads * head_dim)?;
    let local = caller.data().local_sessions;
    let kv_dim = kv_heads * head_dim;
    let output = {
        let owner = caller.data().session_owner.clone();
        let mut cache = fixed_table(&owner, caller.data().local_sessions)
            .lock()
            .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
        if owner.is_none() { prune_fixed_tensor_session_cache_locked(&mut cache); }
        let entry = match cache.get_mut(&cache_key) {
            Some(entry) => entry,
            None => return Ok(-3),
        };
        if layer_idx >= entry.n_layers || sequence_len > entry.max_t || entry.kv_dim != kv_dim {
            return Ok(-1);
        }
        let _held = tensor_hold(caller.data().budget.as_ref(), entry._lease.as_ref(),
            entry.k_cache.capacity(), entry.v_cache.capacity(), cache_key.capacity())
            .map_err(|error| host_failure(caller, error))?;
        let layer_span = entry.max_t * entry.kv_dim;
        let layer_offset = layer_idx * layer_span;
        let used = sequence_len * entry.kv_dim;
        entry.updated_at = now_secs();
        let keys = &entry.k_cache[layer_offset..layer_offset + used];
        let values = &entry.v_cache[layer_offset..layer_offset + used];
        if local {
            deterministic_tensor::attention_kv_with(&query, keys, values, sequence_len,
                query_heads, kv_heads, head_dim, scale,
                |count| tensor_result(true, count, 0, caller.data().budget.as_ref()))
        } else {
            deterministic_tensor::attention_kv(&query, keys, values, sequence_len,
                query_heads, kv_heads, head_dim, scale)
                .map(native_bytes::Value::from).map_err(HostFailure::from)
        }
    }.map_err(|error| host_failure(caller, error))?;
    let key_digest: [u8; 32] = Sha256::digest(raw_key.as_bytes()).into();
    let query_digest = trace_i64_digest(&query);
    let output_digest = trace_i64_digest(&output);
    let sequence_bytes = (sequence_len as u64).to_le_bytes();
    let layer_bytes = (layer_idx as u64).to_le_bytes();
    let query_heads_bytes = (query_heads as u64).to_le_bytes();
    let kv_heads_bytes = (kv_heads as u64).to_le_bytes();
    let head_dimension_bytes = (head_dim as u64).to_le_bytes();
    let scale_bytes = scale.to_le_bytes();
    let status = write_guest_i64_vec(caller, output_ptr, &output)?;
    record_compute_trace(
        caller,
        "tensor_session_attention_q24",
        &[
            &key_digest,
            &sequence_bytes,
            &layer_bytes,
            &query_heads_bytes,
            &kv_heads_bytes,
            &head_dimension_bytes,
            &scale_bytes,
            &query_digest,
            &output_digest,
        ],
    );
    Ok(status)
}

fn decode_b64(name: &str, value: &str) -> Result<Vec<u8>, String> {
    BASE64
        .decode(value)
        .map_err(|e| format!("invalid {name} base64: {e}"))
}

fn encode_b64(bytes: &[u8]) -> String {
    BASE64.encode(bytes)
}

fn normalize_non_empty(raw: &str, field: &str) -> Result<String, String> {
    let value = raw.trim();
    if value.is_empty() {
        Err(format!("{field} must not be empty"))
    } else {
        Ok(value.to_owned())
    }
}

fn normalize_hex64(raw: &str, field: &str) -> Result<String, String> {
    let value = raw.trim().to_lowercase();
    if value.len() == 64 && value.chars().all(|c| c.is_ascii_hexdigit()) {
        Ok(value)
    } else {
        Err(format!("{field} must be a 64-char hex string"))
    }
}

fn normalize_optional_hex64(value: Option<String>, field: &str) -> Result<Option<String>, String> {
    match value {
        None => Ok(None),
        Some(value) => Ok(Some(normalize_hex64(&value, field)?)),
    }
}

fn normalize_optional_string(value: Option<String>) -> Option<String> {
    match value {
        None => None,
        Some(value) => {
            let trimmed = value.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_owned())
            }
        }
    }
}

fn normalize_bool_literal(value: bool) -> &'static str {
    if value {
        "true"
    } else {
        "false"
    }
}

fn validate_int_string(raw: &str, field: &str) -> Result<String, String> {
    let value = raw.trim();
    if value.is_empty()
        || !value
            .chars()
            .enumerate()
            .all(|(idx, ch)| ch.is_ascii_digit() || (idx == 0 && ch == '-'))
    {
        Err(format!("{field} must be an integer string"))
    } else {
        Ok(value.to_owned())
    }
}

fn parse_string_param(params: &[FrameValue], idx: usize, field: &str) -> Result<String, String> {
    match params.get(idx) {
        Some(FrameValue::String(value)) | Some(FrameValue::Int(value)) => Ok(value.clone()),
        _ => Err(format!("{field} must be a string")),
    }
}

fn parse_optional_string_param(
    params: &[FrameValue],
    idx: usize,
) -> Result<Option<String>, String> {
    match params.get(idx) {
        Some(FrameValue::Null) | None => Ok(None),
        Some(FrameValue::String(value)) | Some(FrameValue::Int(value)) => Ok(Some(value.clone())),
        _ => Err("optional string param must be a string or null".to_owned()),
    }
}

fn parse_bool_param(params: &[FrameValue], idx: usize, field: &str) -> Result<bool, String> {
    match params.get(idx) {
        Some(FrameValue::Bool(value)) => Ok(*value),
        _ => Err(format!("{field} must be a bool")),
    }
}

fn parse_int_param(params: &[FrameValue], idx: usize, field: &str) -> Result<String, String> {
    let value = parse_string_param(params, idx, field)?;
    validate_int_string(&value, field)
}

fn parse_request_frame(raw: &[u8]) -> Result<RequestFrame, String> {
    if raw.len() < REQUEST_MAGIC.len() + 4 {
        return Err("request too short".to_owned());
    }
    if &raw[..REQUEST_MAGIC.len()] != REQUEST_MAGIC {
        return Err("invalid request magic".to_owned());
    }
    let mut offset = REQUEST_MAGIC.len();
    let method_len = get_u16(raw, &mut offset)? as usize;
    let method = read_utf8(raw, &mut offset, method_len)?;
    let param_count = get_u16(raw, &mut offset)? as usize;
    let mut params = Vec::with_capacity(param_count);
    for _ in 0..param_count {
        let tag = get_u8(raw, &mut offset)?;
        let size = get_u32(raw, &mut offset)? as usize;
        let payload = read_bytes(raw, &mut offset, size)?;
        let value = match tag {
            0 => FrameValue::Null,
            1 => FrameValue::Bool(false),
            2 => FrameValue::Bool(true),
            3 => FrameValue::Int(
                String::from_utf8(payload.to_vec())
                    .map_err(|_| "invalid request utf8".to_owned())?,
            ),
            4 => FrameValue::String(
                String::from_utf8(payload.to_vec())
                    .map_err(|_| "invalid request utf8".to_owned())?,
            ),
            _ => return Err(format!("unsupported request param tag: {tag}")),
        };
        params.push(value);
    }
    if offset != raw.len() {
        return Err("request has trailing bytes".to_owned());
    }
    Ok(RequestFrame { method, params })
}

fn encode_response_value(value: FrameValue) -> Vec<u8> {
    let (tag, payload) = match value {
        FrameValue::Null => (0_u8, Vec::new()),
        FrameValue::Bool(false) => (1_u8, Vec::new()),
        FrameValue::Bool(true) => (2_u8, Vec::new()),
        FrameValue::Int(value) | FrameValue::String(value) => {
            let tag = if matches!(value.parse::<i64>(), Ok(_)) {
                3_u8
            } else {
                4_u8
            };
            (tag, value.into_bytes())
        }
    };
    let mut out = Vec::with_capacity(RESPONSE_MAGIC.len() + 5 + payload.len());
    out.extend_from_slice(RESPONSE_MAGIC);
    out.push(tag);
    out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    out.extend_from_slice(&payload);
    out
}

fn frame_null() -> Vec<u8> {
    encode_response_value(FrameValue::Null)
}

fn frame_bool(value: bool) -> Vec<u8> {
    encode_response_value(FrameValue::Bool(value))
}

fn frame_int(value: impl ToString) -> Vec<u8> {
    let mut out = Vec::new();
    let payload = value.to_string();
    out.extend_from_slice(RESPONSE_MAGIC);
    out.push(3);
    out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    out.extend_from_slice(payload.as_bytes());
    out
}

fn frame_string(value: impl ToString) -> Vec<u8> {
    let payload = value.to_string();
    let mut out = Vec::new();
    out.extend_from_slice(RESPONSE_MAGIC);
    out.push(4);
    out.extend_from_slice(&(payload.len() as u32).to_be_bytes());
    out.extend_from_slice(payload.as_bytes());
    out
}

fn frame_from_option_string(value: Option<String>) -> Vec<u8> {
    match value {
        Some(value) => frame_string(value),
        None => frame_null(),
    }
}

fn frame_from_option_bool(value: Option<bool>) -> Vec<u8> {
    match value {
        Some(value) => frame_bool(value),
        None => frame_null(),
    }
}

fn frame_from_option_int_string(value: Option<String>) -> Vec<u8> {
    match value {
        Some(value) => frame_int(value),
        None => frame_null(),
    }
}

fn storage_get_utf8(storage: &Storage, key: &str) -> Option<String> {
    storage
        .get(key)
        .and_then(|value| String::from_utf8(value.as_slice().to_vec()).ok())
}

struct StorageEdit<'a>(&'a mut Arc<Storage>, bool, Option<&'a Arc<native_bytes::Budget>>);

impl std::ops::Deref for StorageEdit<'_> {
    type Target = Storage;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl StorageEdit<'_> {
    fn data_mut(&mut self) -> Result<&mut Storage, HostFailure> {
        if self.1 && Arc::get_mut(self.0).is_none() {
            let size = self.0.keys().try_fold(0usize, |total, key| {
                total.checked_add(key.len()).ok_or("storage key bytes overflow")
            })?;
            let lease = compute_memory().take_in(size, self.2)
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
            let mut data = BTreeMap::new();
            for (key, value) in self.0.iter() {
                let mut copy = String::new();
                copy.try_reserve_exact(key.len())
                    .map_err(|_| HostFailure::Unavailable("storage allocation failed".to_owned()))?;
                if copy.capacity() > key.len() {
                    return Err(HostFailure::Unavailable("storage capacity differs".to_owned()));
                }
                copy.push_str(key);
                data.insert(copy, value.clone());
            }
            *self.0 = Arc::new(Storage { data, keys: Some(lease) });
        }
        if Arc::get_mut(self.0).is_none() {
            *self.0 = Arc::new(self.0.data.clone().into());
        }
        let storage = Arc::get_mut(self.0).unwrap();
        if self.1 && !storage.keys.as_ref().is_some_and(|keys| keys.has_owner(self.2)) {
            let size = storage.keys().try_fold(0usize, |total, key| {
                total.checked_add(key.capacity()).ok_or("storage key bytes overflow")
            })?;
            storage.keys = Some(compute_memory().take_in(size, self.2)
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?);
        }
        Ok(storage)
    }

    fn insert(&mut self, key: String, value: Vec<u8>) -> Result<(), HostFailure> {
        if self.1 {
            return self.put(&key, &value);
        }
        self.data_mut()?.data.insert(key, Arc::new(value.into()));
        Ok(())
    }

    fn put(&mut self, key: &str, value: &[u8]) -> Result<(), HostFailure> {
        if !self.1 {
            return self.insert(key.to_owned(), value.to_vec());
        }
        let mut keys = compute_memory().take_in(key.len(), self.2)
            .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
        let mut owned = String::new();
        owned.try_reserve_exact(key.len())
            .map_err(|_| HostFailure::Unavailable("storage allocation failed".to_owned()))?;
        if owned.capacity() > keys.size() {
            return Err(HostFailure::Unavailable("storage capacity differs".to_owned()));
        }
        owned.push_str(key);
        let lease = compute_memory().take_in(value.len(), self.2)
            .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
        let mut copy = Vec::new();
        copy.try_reserve_exact(value.len())
            .map_err(|_| HostFailure::Unavailable("storage allocation failed".to_owned()))?;
        copy.extend_from_slice(value);
        let value = Arc::new(native_bytes::Value::charged(copy, lease)
            .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?);
        let storage = self.data_mut()?;
        if !storage.contains_key(key) {
            storage.keys.as_mut().unwrap().join(&mut keys)
                .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
            storage.data.insert(owned, value);
        } else {
            storage.data.insert(owned, value);
            drop(keys);
        }
        Ok(())
    }

    fn remove(&mut self, key: &str) -> Result<(), HostFailure> {
        if self.contains_key(key) {
            let local = self.1;
            let storage = self.data_mut()?;
            let (key, value) = storage.data.remove_entry(key).unwrap();
            let size = key.capacity();
            drop(key);
            drop(value);
            if let Some(lease) = storage.keys.as_mut().filter(|_| local) {
                let next = lease.size().checked_sub(size)
                    .ok_or("storage key bytes differ")?;
                lease.resize(next)
                    .map_err(|reason| HostFailure::Unavailable(reason.to_owned()))?;
            }
        }
        Ok(())
    }
}

fn storage_set_utf8(storage: &mut StorageEdit<'_>, key: String, value: impl AsRef<str>)
    -> Result<(), HostFailure>
{
    if storage.1 {
        storage.put(&key, value.as_ref().as_bytes())
    } else {
        storage.insert(key, value.as_ref().as_bytes().to_vec())
    }
}

fn storage_set_optional_utf8(
    storage: &mut StorageEdit<'_>,
    key: String,
    value: Option<String>,
) -> Result<(), HostFailure> {
    match value {
        Some(value) if !value.is_empty() => {
            storage_set_utf8(storage, key, value)
        }
        _ => {
            storage.remove(&key)
        }
    }
}

fn storage_set_bool(storage: &mut StorageEdit<'_>, key: String, value: bool)
    -> Result<(), HostFailure>
{
    storage_set_utf8(storage, key, normalize_bool_literal(value))
}

fn storage_set_int(storage: &mut StorageEdit<'_>, key: String, value: impl AsRef<str>)
    -> Result<(), HostFailure>
{
    storage_set_utf8(storage, key, value)
}

fn storage_get_bool(storage: &Storage, key: &str) -> Option<bool> {
    let raw = storage_get_utf8(storage, key)?;
    match raw.trim().to_lowercase().as_str() {
        "1" | "true" | "yes" => Some(true),
        "0" | "false" | "no" => Some(false),
        _ => None,
    }
}

fn storage_get_int(storage: &Storage, key: &str) -> Option<String> {
    let raw = storage_get_utf8(storage, key)?;
    let trimmed = raw.trim();
    if trimmed.is_empty()
        || !trimmed
            .chars()
            .enumerate()
            .all(|(idx, ch)| ch.is_ascii_digit() || (idx == 0 && ch == '-'))
    {
        None
    } else {
        Some(trimmed.to_owned())
    }
}

fn increment_stored_version(storage: &mut StorageEdit<'_>, key: &str) -> Result<String, HostFailure> {
    let next = storage_get_int(storage, key)
        .and_then(|value| value.parse::<i64>().ok())
        .map(|value| value + 1)
        .unwrap_or(1);
    let value = next.to_string();
    storage_set_int(storage, key.to_owned(), &value)?;
    Ok(value)
}

fn state_descriptor_key(state_ref: &str, suffix: &str) -> String {
    format!("state_descriptor:{}:{suffix}", state_path_key(state_ref))
}

fn state_policy_key(state_ref: &str, suffix: &str) -> String {
    format!("state_policy:{}:{suffix}", state_path_key(state_ref))
}

fn balance_cell_key(state_ref: &str, suffix: &str) -> String {
    format!("balance_cell:{}:{suffix}", state_path_key(state_ref))
}

fn register_cell_key(state_ref: &str, suffix: &str) -> String {
    format!("register_cell:{}:{suffix}", state_path_key(state_ref))
}

fn balance_binding_key(subject_addr: &str, suffix: &str) -> String {
    format!("balance_binding:{subject_addr}:{suffix}")
}

fn register_binding_key(register_ref: &str, suffix: &str) -> String {
    format!("register_binding:{register_ref}:{suffix}")
}

fn balance_workflow_key(workflow_ref: &str, suffix: &str) -> String {
    format!("balance_workflow:{workflow_ref}:{suffix}")
}

fn register_workflow_key(workflow_ref: &str, suffix: &str) -> String {
    format!("register_workflow:{workflow_ref}:{suffix}")
}

fn object_binding_key(object_ref: &str, suffix: &str) -> String {
    format!("object_binding:{object_ref}:{suffix}")
}

fn object_member_key(object_ref: &str, member_ref: &str, suffix: &str) -> String {
    format!("object_member:{object_ref}:{member_ref}:{suffix}")
}

fn object_policy_key(object_ref: &str, suffix: &str) -> String {
    format!("object_policy:{object_ref}:{suffix}")
}

fn object_transition_key(transition_ref: &str, suffix: &str) -> String {
    format!("object_transition:{transition_ref}:{suffix}")
}

fn normalize_member_ref(raw: &str, field: &str) -> Result<String, String> {
    let value = normalize_non_empty(raw, field)?;
    if value.contains(':') {
        Err(format!("{field} must not contain ':'"))
    } else {
        Ok(value)
    }
}

#[derive(Debug)]
enum ObjectMemberDelta {
    Attach {
        member_ref: String,
        state_ref: String,
        member_kind: String,
        state_class: String,
        codec: String,
        status: String,
    },
    Detach {
        member_ref: String,
    },
}

fn parse_object_member_bundle(bundle: &str) -> Result<Vec<ObjectMemberDelta>, String> {
    if bundle.trim().is_empty() {
        return Ok(Vec::new());
    }
    bundle
        .split(';')
        .map(|entry| {
            let parts: Vec<&str> = entry.split('|').collect();
            if parts.first() == Some(&"attach") && parts.len() == 7 {
                Ok(ObjectMemberDelta::Attach {
                    member_ref: normalize_member_ref(parts[1], "member_ref")?,
                    state_ref: normalize_hex64(parts[2], "state_ref")?,
                    member_kind: normalize_non_empty(parts[3], "member_kind")?,
                    state_class: normalize_non_empty(parts[4], "state_class")?,
                    codec: normalize_non_empty(parts[5], "codec")?,
                    status: normalize_non_empty(parts[6], "status")?,
                })
            } else if parts.first() == Some(&"detach") && parts.len() == 2 {
                Ok(ObjectMemberDelta::Detach {
                    member_ref: normalize_member_ref(parts[1], "member_ref")?,
                })
            } else {
                Err("invalid object member bundle".to_owned())
            }
        })
        .collect()
}

fn read_state_descriptor_field(
    storage: &Storage,
    state_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "state_class" | "codec" | "schema_hash" | "subject_addr" | "hfhe_profile" => {
            Ok(frame_from_option_string(storage_get_utf8(
                storage,
                &state_descriptor_key(state_ref, field),
            )))
        }
        "mutable_state" => Ok(frame_from_option_bool(storage_get_bool(
            storage,
            &state_descriptor_key(state_ref, field),
        ))),
        _ => Err(format!("unsupported state descriptor field: {field}")),
    }
}

fn read_state_policy_field(
    storage: &Storage,
    state_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "delivery_key_id" => Ok(frame_from_option_string(storage_get_utf8(
            storage,
            &state_policy_key(state_ref, field),
        ))),
        "activate_after_epoch" | "expire_after_epoch" => Ok(frame_from_option_int_string(
            storage_get_int(storage, &state_policy_key(state_ref, field)),
        )),
        "tombstone" | "revoked" => Ok(frame_from_option_bool(storage_get_bool(
            storage,
            &state_policy_key(state_ref, field),
        ))),
        _ => Err(format!("unsupported state policy field: {field}")),
    }
}

fn read_balance_cell_field(
    storage: &Storage,
    state_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "ciphertext_commitment" | "amount_commitment" | "proof_kind" | "proof_receipt_hash" => {
            Ok(frame_from_option_string(storage_get_utf8(
                storage,
                &balance_cell_key(state_ref, field),
            )))
        }
        _ => Err(format!("unsupported balance cell field: {field}")),
    }
}

fn read_register_cell_field(
    storage: &Storage,
    state_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "ciphertext_commitment" | "proof_kind" | "proof_receipt_hash" => {
            Ok(frame_from_option_string(storage_get_utf8(
                storage,
                &register_cell_key(state_ref, field),
            )))
        }
        _ => Err(format!("unsupported register cell field: {field}")),
    }
}

fn read_balance_binding_field(
    storage: &Storage,
    subject_addr: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "current_state_ref" | "status" | "last_workflow_ref" => Ok(frame_from_option_string(
            storage_get_utf8(storage, &balance_binding_key(subject_addr, field)),
        )),
        "version" => Ok(frame_from_option_int_string(storage_get_int(
            storage,
            &balance_binding_key(subject_addr, field),
        ))),
        _ => Err(format!("unsupported balance binding field: {field}")),
    }
}

fn read_register_binding_field(
    storage: &Storage,
    register_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "current_state_ref" | "status" | "last_workflow_ref" => Ok(frame_from_option_string(
            storage_get_utf8(storage, &register_binding_key(register_ref, field)),
        )),
        "version" => Ok(frame_from_option_int_string(storage_get_int(
            storage,
            &register_binding_key(register_ref, field),
        ))),
        _ => Err(format!("unsupported register binding field: {field}")),
    }
}

fn read_balance_workflow_field(
    storage: &Storage,
    workflow_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "flow_kind"
        | "debit_subject_addr"
        | "credit_subject_addr"
        | "debit_state_ref"
        | "credit_state_ref"
        | "amount_commitment"
        | "proof_kind"
        | "proof_receipt_hash"
        | "status"
        | "intent_id" => Ok(frame_from_option_string(storage_get_utf8(
            storage,
            &balance_workflow_key(workflow_ref, field),
        ))),
        _ => Err(format!("unsupported balance workflow field: {field}")),
    }
}

fn read_register_workflow_field(
    storage: &Storage,
    workflow_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "register_ref" | "previous_state_ref" | "next_state_ref" | "workflow_kind"
        | "proof_kind" | "proof_receipt_hash" | "status" | "intent_id" => {
            Ok(frame_from_option_string(storage_get_utf8(
                storage,
                &register_workflow_key(workflow_ref, field),
            )))
        }
        _ => Err(format!("unsupported register workflow field: {field}")),
    }
}

fn read_object_binding_field(
    storage: &Storage,
    object_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "current_state_ref" | "status" | "last_transition_ref" => Ok(frame_from_option_string(
            storage_get_utf8(storage, &object_binding_key(object_ref, field)),
        )),
        "version" => Ok(frame_from_option_int_string(storage_get_int(
            storage,
            &object_binding_key(object_ref, field),
        ))),
        _ => Err(format!("unsupported object binding field: {field}")),
    }
}

fn read_object_member_field(
    storage: &Storage,
    object_ref: &str,
    member_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "state_ref" | "member_kind" | "state_class" | "codec" | "status" => {
            Ok(frame_from_option_string(storage_get_utf8(
                storage,
                &object_member_key(object_ref, member_ref, field),
            )))
        }
        _ => Err(format!("unsupported object member field: {field}")),
    }
}

fn read_object_policy_field(
    storage: &Storage,
    object_ref: &str,
    field: &str,
) -> Result<Vec<u8>, String> {
    match field {
        "delivery_key_id" | "transition_mode" | "required_proof_kind" => {
            Ok(frame_from_option_string(storage_get_utf8(
                storage,
                &object_policy_key(object_ref, field),
            )))
        }
        "activate_after_epoch" | "expire_after_epoch" | "member_quorum" => {
            Ok(frame_from_option_int_string(storage_get_int(
                storage,
                &object_policy_key(object_ref, field),
            )))
        }
        "tombstone" | "revoked" | "allow_detach" | "allow_root_state_rotation" => {
            Ok(frame_from_option_bool(storage_get_bool(
                storage,
                &object_policy_key(object_ref, field),
            )))
        }
        _ => Err(format!("unsupported object policy field: {field}")),
    }
}

fn encrypted_asset_locator(
    kind: &str,
    value: &str,
) -> Result<(Option<String>, Option<String>, Option<String>), String> {
    let locator_kind = normalize_non_empty(kind, "locator_kind")?;
    let locator_value = normalize_non_empty(value, "locator_value")?;
    if locator_kind == "path" {
        return Ok((Some(locator_value), None, None));
    }
    if locator_kind == "slot" {
        return Ok((
            None,
            Some(normalize_hex64(&locator_value, "slot_ref")?),
            None,
        ));
    }
    if locator_kind == "state" {
        return Ok((
            None,
            None,
            Some(normalize_hex64(&locator_value, "state_ref")?),
        ));
    }
    Err("locator_kind must be path, slot, or state".to_owned())
}

fn read_circle_state(
    storage: &Storage, method: &str, params: &[FrameValue],
) -> Result<Option<Vec<u8>>, String> {
    let response = match method {
        "state_descriptor_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_state_descriptor_field(storage, &state_ref, &field)?)
        }
        "state_policy_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_state_policy_field(storage, &state_ref, &field)?)
        }
        "balance_cell_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_balance_cell_field(storage, &state_ref, &field)?)
        }
        "register_cell_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_register_cell_field(storage, &state_ref, &field)?)
        }
        "balance_binding_get" => {
            let subject_addr = normalize_non_empty(
                &parse_string_param(params, 0, "subject_addr")?,
                "subject_addr",
            )?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_balance_binding_field(storage, &subject_addr, &field)?)
        }
        "register_binding_get" => {
            let register_ref = normalize_hex64(
                &parse_string_param(params, 0, "register_ref")?,
                "register_ref",
            )?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_register_binding_field(storage, &register_ref, &field)?)
        }
        "balance_workflow_get" => {
            let workflow_ref = normalize_hex64(
                &parse_string_param(params, 0, "workflow_ref")?,
                "workflow_ref",
            )?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_balance_workflow_field(storage, &workflow_ref, &field)?)
        }
        "register_workflow_get" => {
            let workflow_ref = normalize_hex64(
                &parse_string_param(params, 0, "workflow_ref")?,
                "workflow_ref",
            )?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_register_workflow_field(storage, &workflow_ref, &field)?)
        }
        "object_binding_get" => {
            let object_ref =
                normalize_hex64(&parse_string_param(params, 0, "object_ref")?, "object_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_object_binding_field(storage, &object_ref, &field)?)
        }
        "object_member_get" => {
            let object_ref =
                normalize_hex64(&parse_string_param(params, 0, "object_ref")?, "object_ref")?;
            let member_ref =
                normalize_non_empty(&parse_string_param(params, 1, "member_ref")?, "member_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 2, "field")?, "field")?;
            Some(read_object_member_field(storage, &object_ref, &member_ref, &field)?)
        }
        "object_policy_get" => {
            let object_ref =
                normalize_hex64(&parse_string_param(params, 0, "object_ref")?, "object_ref")?;
            let field = normalize_non_empty(&parse_string_param(params, 1, "field")?, "field")?;
            Some(read_object_policy_field(storage, &object_ref, &field)?)
        }
        _ => None,
    };
    Ok(response)
}

fn execute_circle_invoke(
    caller: &mut Caller<'_, HostState>,
    req_bytes: &[u8],
) -> Result<Vec<u8>, HostFailure> {
    let request = parse_request_frame(req_bytes)?;
    let params = request.params;
    if request.method == "circle_public_asset_meta" || request.method == "circle_public_asset_read"
    {
        let circle_id = parse_string_param(&params, 0, "circle_id")?;
        let canonical_path = parse_string_param(&params, 1, "canonical_path")?;
        let offset = parse_int_param(&params, 2, "offset")?
            .parse::<usize>()
            .map_err(|_| "offset must be non-negative".to_owned())?;
        let max_bytes = parse_int_param(&params, 3, "max_bytes")?
            .parse::<usize>()
            .map_err(|_| "max_bytes must be positive".to_owned())?;
        let key = public_read_key(&circle_id, &canonical_path, offset, max_bytes);
        let read = caller
            .data()
            .public_reads
            .get(&key)
            .ok_or_else(|| "wasm public read not declared".to_owned())?;
        if request.method == "circle_public_asset_meta" {
            return Ok(frame_string(read.metadata_json.clone()));
        }
        let next_total = caller
            .data()
            .public_read_bytes
            .checked_add(read.body_bytes)
            .ok_or_else(|| "wasm public read bytes overflow".to_owned())?;
        if next_total > MAX_PUBLIC_READ_BYTES {
            return Err("wasm public read bytes exceed limit".to_owned().into());
        }
        let body_b64 = read.body_b64.clone();
        caller.data_mut().public_read_bytes = next_total;
        return Ok(frame_string(body_b64));
    }
    if request.method == "circle_spawn" {
        if caller.data().is_view {
            return Err("circle spawn disabled in view".to_owned().into());
        }
        if caller.data().spawns.len() >= 4 {
            return Err("circle spawn cap exceeded".to_owned().into());
        }
        let payload_json = normalize_non_empty(
            &parse_string_param(&params, 0, "payload_json")?,
            "payload_json",
        )?;
        if payload_json.len() > MAX_SPAWN_PAYLOAD_JSON_BYTES {
            return Err("circle spawn payload exceeds max size".to_owned().into());
        }
        serde_json::from_str::<JsonValue>(&payload_json)
            .map_err(|e| format!("invalid circle spawn payload json: {e}"))?;
        let owner_mode =
            normalize_non_empty(&parse_string_param(&params, 1, "owner_mode")?, "owner_mode")?;
        if owner_mode != "caller" && owner_mode != "parent" {
            return Err("owner_mode must be caller or parent".to_owned().into());
        }
        let spawn_nonce = caller.data().spawns.len() as i32;
        let circle_id = circle_id_of_spawn(caller.data(), &owner_mode, spawn_nonce, &payload_json);
        caller.data_mut().spawns.push(SpawnJson {
            circle_id: circle_id.clone(),
            owner_mode,
            spawn_nonce,
            payload_json,
        });
        return Ok(frame_string(circle_id));
    }
    if request.method == "circle_asset_put" {
        if caller.data().is_view {
            return Err("circle asset effects disabled in view".to_owned().into());
        }
        if caller.data().assets.len() + caller.data().encrypted_assets.len() >= 4 {
            return Err("circle asset effect cap exceeded".to_owned().into());
        }
        let circle_id =
            normalize_non_empty(&parse_string_param(&params, 0, "circle_id")?, "circle_id")?;
        let path = normalize_non_empty(&parse_string_param(&params, 1, "path")?, "path")?;
        let content_type = normalize_non_empty(
            &parse_string_param(&params, 2, "content_type")?,
            "content_type",
        )?;
        let body_b64 =
            normalize_non_empty(&parse_string_param(&params, 3, "body_b64")?, "body_b64")?;
        let encoding = normalize_optional_string(parse_optional_string_param(&params, 4)?);
        if body_b64.len() > MAX_ASSET_EFFECT_BODY_B64_BYTES {
            return Err("circle asset effect body exceeds max encoded size".to_owned().into());
        }
        caller.data_mut().assets.push(AssetPutJson {
            circle_id,
            path,
            content_type,
            encoding,
            body_b64,
        });
        return Ok(frame_bool(true));
    }
    if request.method == "circle_asset_put_encrypted" {
        if caller.data().is_view {
            return Err("circle encrypted asset effects disabled in view".to_owned().into());
        }
        if caller.data().assets.len() + caller.data().encrypted_assets.len() >= 4 {
            return Err("circle asset effect cap exceeded".to_owned().into());
        }
        let circle_id =
            normalize_non_empty(&parse_string_param(&params, 0, "circle_id")?, "circle_id")?;
        let path = normalize_non_empty(&parse_string_param(&params, 1, "path")?, "path")?;
        let content_type = normalize_non_empty(
            &parse_string_param(&params, 2, "content_type")?,
            "content_type",
        )?;
        let ciphertext_b64 = normalize_non_empty(
            &parse_string_param(&params, 3, "ciphertext_b64")?,
            "ciphertext_b64",
        )?;
        let key_id = normalize_non_empty(&parse_string_param(&params, 4, "key_id")?, "key_id")?;
        let plaintext_hash = normalize_hex64(
            &parse_string_param(&params, 5, "plaintext_hash")?,
            "plaintext_hash",
        )?;
        let encoding = normalize_optional_string(parse_optional_string_param(&params, 6)?);
        let padding_class = normalize_optional_string(parse_optional_string_param(&params, 7)?);
        let activate_after_epoch =
            match normalize_optional_string(parse_optional_string_param(&params, 8)?) {
                None => None,
                Some(value) => Some(validate_int_string(&value, "activate_after_epoch")?),
            };
        let expire_after_epoch =
            match normalize_optional_string(parse_optional_string_param(&params, 9)?) {
                None => None,
                Some(value) => Some(validate_int_string(&value, "expire_after_epoch")?),
            };
        let metadata_mode = normalize_optional_string(parse_optional_string_param(&params, 10)?);
        if ciphertext_b64.len() > MAX_ASSET_EFFECT_BODY_B64_BYTES {
            return Err("circle encrypted asset effect body exceeds max encoded size".to_owned().into());
        }
        if metadata_mode
            .as_ref()
            .map(|value| value != "reveal" && value != "opaque")
            .unwrap_or(false)
        {
            return Err("metadata_mode must be reveal or opaque".to_owned().into());
        }
        caller
            .data_mut()
            .encrypted_assets
            .push(EncryptedAssetPutJson {
                circle_id,
                path: Some(path),
                slot_ref: None,
                state_ref: None,
                content_type,
                encoding,
                key_id,
                plaintext_hash,
                padding_class,
                activate_after_epoch,
                expire_after_epoch,
                metadata_mode,
                ciphertext_b64,
            });
        return Ok(frame_bool(true));
    }
    if request.method == "circle_asset_put_encrypted_at" {
        if caller.data().is_view {
            return Err("circle encrypted asset effects disabled in view".to_owned().into());
        }
        if caller.data().assets.len() + caller.data().encrypted_assets.len() >= 4 {
            return Err("circle asset effect cap exceeded".to_owned().into());
        }
        let circle_id =
            normalize_non_empty(&parse_string_param(&params, 0, "circle_id")?, "circle_id")?;
        let (path, slot_ref, state_ref) = encrypted_asset_locator(
            &parse_string_param(&params, 1, "locator_kind")?,
            &parse_string_param(&params, 2, "locator_value")?,
        )?;
        let content_type = normalize_non_empty(
            &parse_string_param(&params, 3, "content_type")?,
            "content_type",
        )?;
        let ciphertext_b64 = normalize_non_empty(
            &parse_string_param(&params, 4, "ciphertext_b64")?,
            "ciphertext_b64",
        )?;
        let key_id = normalize_non_empty(&parse_string_param(&params, 5, "key_id")?, "key_id")?;
        let plaintext_hash = normalize_hex64(
            &parse_string_param(&params, 6, "plaintext_hash")?,
            "plaintext_hash",
        )?;
        let encoding = normalize_optional_string(parse_optional_string_param(&params, 7)?);
        let padding_class = normalize_optional_string(parse_optional_string_param(&params, 8)?);
        let activate_after_epoch =
            match normalize_optional_string(parse_optional_string_param(&params, 9)?) {
                None => None,
                Some(value) => Some(validate_int_string(&value, "activate_after_epoch")?),
            };
        let expire_after_epoch =
            match normalize_optional_string(parse_optional_string_param(&params, 10)?) {
                None => None,
                Some(value) => Some(validate_int_string(&value, "expire_after_epoch")?),
            };
        let metadata_mode = normalize_optional_string(parse_optional_string_param(&params, 11)?);
        if ciphertext_b64.len() > MAX_ASSET_EFFECT_BODY_B64_BYTES {
            return Err("circle encrypted asset effect body exceeds max encoded size".to_owned().into());
        }
        if metadata_mode
            .as_ref()
            .map(|value| value != "reveal" && value != "opaque")
            .unwrap_or(false)
        {
            return Err("metadata_mode must be reveal or opaque".to_owned().into());
        }
        caller
            .data_mut()
            .encrypted_assets
            .push(EncryptedAssetPutJson {
                circle_id,
                path,
                slot_ref,
                state_ref,
                content_type,
                encoding,
                key_id,
                plaintext_hash,
                padding_class,
                activate_after_epoch,
                expire_after_epoch,
                metadata_mode,
                ciphertext_b64,
            });
        return Ok(frame_bool(true));
    }
    if let Some(response) = read_circle_state(&caller.data().storage, &request.method, &params)? {
        return Ok(response);
    }
    let local = caller.data().local_storage;
    let quota = caller.data().budget.clone();
    let mut edit = StorageEdit(&mut caller.data_mut().storage, local, quota.as_ref());
    let storage = &mut edit;
    match request.method.as_str() {
        "state_describe" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let state_class = normalize_non_empty(
                &parse_string_param(&params, 1, "state_class")?,
                "state_class",
            )?;
            let codec = normalize_non_empty(&parse_string_param(&params, 2, "codec")?, "codec")?;
            let schema_hash = normalize_optional_string(parse_optional_string_param(&params, 3)?);
            let subject_addr = normalize_optional_string(parse_optional_string_param(&params, 4)?);
            let hfhe_profile = normalize_non_empty(
                &parse_string_param(&params, 5, "hfhe_profile")?,
                "hfhe_profile",
            )?;
            let mutable_state = parse_bool_param(&params, 6, "mutable_state")?;
            storage_set_utf8(
                storage,
                state_descriptor_key(&state_ref, "state_class"),
                state_class,
            )?;
            storage_set_utf8(storage, state_descriptor_key(&state_ref, "codec"), codec)?;
            storage_set_optional_utf8(
                storage,
                state_descriptor_key(&state_ref, "schema_hash"),
                schema_hash,
            )?;
            storage_set_optional_utf8(
                storage,
                state_descriptor_key(&state_ref, "subject_addr"),
                subject_addr,
            )?;
            storage_set_utf8(
                storage,
                state_descriptor_key(&state_ref, "hfhe_profile"),
                hfhe_profile,
            )?;
            storage_set_bool(
                storage,
                state_descriptor_key(&state_ref, "mutable_state"),
                mutable_state,
            )?;
            Ok(frame_bool(true))
        }
        "state_publish" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let delivery_key_id =
                normalize_optional_string(parse_optional_string_param(&params, 1)?);
            let activate_after = parse_int_param(&params, 2, "activate_after_epoch")?;
            let expire_after = parse_int_param(&params, 3, "expire_after_epoch")?;
            storage_set_optional_utf8(
                storage,
                state_policy_key(&state_ref, "delivery_key_id"),
                delivery_key_id,
            )?;
            storage_set_int(
                storage,
                state_policy_key(&state_ref, "activate_after_epoch"),
                activate_after,
            )?;
            storage_set_int(
                storage,
                state_policy_key(&state_ref, "expire_after_epoch"),
                expire_after,
            )?;
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), false)?;
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), false)?;
            Ok(frame_bool(true))
        }
        "state_release" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage.remove(&state_policy_key(&state_ref, "activate_after_epoch"))?;
            storage.remove(&state_policy_key(&state_ref, "expire_after_epoch"))?;
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), false)?;
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), false)?;
            Ok(frame_bool(true))
        }
        "state_retire" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let expire_after = parse_int_param(&params, 1, "expire_after_epoch")?;
            storage_set_int(
                storage,
                state_policy_key(&state_ref, "expire_after_epoch"),
                expire_after,
            )?;
            Ok(frame_bool(true))
        }
        "state_tombstone_apply" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), true)?;
            Ok(frame_bool(true))
        }
        "state_restore" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), false)?;
            Ok(frame_bool(true))
        }
        "state_revoke_apply" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), true)?;
            Ok(frame_bool(true))
        }
        "state_reinstate" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), false)?;
            Ok(frame_bool(true))
        }
        "balance_cell_materialize" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_utf8(
                storage,
                balance_cell_key(&state_ref, "ciphertext_commitment"),
                normalize_non_empty(
                    &parse_string_param(&params, 1, "ciphertext_commitment")?,
                    "ciphertext_commitment",
                )?,
            )?;
            storage_set_utf8(
                storage,
                balance_cell_key(&state_ref, "amount_commitment"),
                normalize_non_empty(
                    &parse_string_param(&params, 2, "amount_commitment")?,
                    "amount_commitment",
                )?,
            )?;
            storage_set_utf8(
                storage,
                balance_cell_key(&state_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 3, "proof_kind")?, "proof_kind")?,
            )?;
            storage_set_optional_utf8(
                storage,
                balance_cell_key(&state_ref, "proof_receipt_hash"),
                normalize_optional_hex64(
                    parse_optional_string_param(&params, 4)?,
                    "proof_receipt_hash",
                )?,
            )?;
            Ok(frame_bool(true))
        }
        "register_cell_materialize" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_utf8(
                storage,
                register_cell_key(&state_ref, "ciphertext_commitment"),
                normalize_non_empty(
                    &parse_string_param(&params, 1, "ciphertext_commitment")?,
                    "ciphertext_commitment",
                )?,
            )?;
            storage_set_utf8(
                storage,
                register_cell_key(&state_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 2, "proof_kind")?, "proof_kind")?,
            )?;
            storage_set_optional_utf8(
                storage,
                register_cell_key(&state_ref, "proof_receipt_hash"),
                normalize_optional_hex64(
                    parse_optional_string_param(&params, 3)?,
                    "proof_receipt_hash",
                )?,
            )?;
            Ok(frame_bool(true))
        }
        "balance_binding_bind" => {
            let subject_addr = normalize_non_empty(
                &parse_string_param(&params, 0, "subject_addr")?,
                "subject_addr",
            )?;
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 1, "state_ref")?, "state_ref")?;
            let workflow_ref = normalize_hex64(
                &parse_string_param(&params, 2, "workflow_ref")?,
                "workflow_ref",
            )?;
            let status = normalize_non_empty(&parse_string_param(&params, 3, "status")?, "status")?;
            storage_set_utf8(
                storage,
                balance_binding_key(&subject_addr, "current_state_ref"),
                state_ref,
            )?;
            let version =
                increment_stored_version(storage, &balance_binding_key(&subject_addr, "version"))?;
            storage_set_utf8(
                storage,
                balance_binding_key(&subject_addr, "status"),
                status,
            )?;
            storage_set_utf8(
                storage,
                balance_binding_key(&subject_addr, "last_workflow_ref"),
                workflow_ref,
            )?;
            Ok(frame_int(version))
        }
        "register_binding_bind" => {
            let register_ref = normalize_hex64(
                &parse_string_param(&params, 0, "register_ref")?,
                "register_ref",
            )?;
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 1, "state_ref")?, "state_ref")?;
            let workflow_ref = normalize_hex64(
                &parse_string_param(&params, 2, "workflow_ref")?,
                "workflow_ref",
            )?;
            let status = normalize_non_empty(&parse_string_param(&params, 3, "status")?, "status")?;
            storage_set_utf8(
                storage,
                register_binding_key(&register_ref, "current_state_ref"),
                state_ref,
            )?;
            let version =
                increment_stored_version(storage, &register_binding_key(&register_ref, "version"))?;
            storage_set_utf8(
                storage,
                register_binding_key(&register_ref, "status"),
                status,
            )?;
            storage_set_utf8(
                storage,
                register_binding_key(&register_ref, "last_workflow_ref"),
                workflow_ref,
            )?;
            Ok(frame_int(version))
        }
        "balance_workflow_record" => {
            let workflow_ref = normalize_hex64(
                &parse_string_param(&params, 0, "workflow_ref")?,
                "workflow_ref",
            )?;
            let fields = [
                ("flow_kind", parse_string_param(&params, 1, "flow_kind")?),
                (
                    "debit_subject_addr",
                    parse_string_param(&params, 2, "debit_subject_addr")?,
                ),
                (
                    "credit_subject_addr",
                    parse_string_param(&params, 3, "credit_subject_addr")?,
                ),
                (
                    "debit_state_ref",
                    parse_string_param(&params, 4, "debit_state_ref")?,
                ),
                (
                    "credit_state_ref",
                    parse_string_param(&params, 5, "credit_state_ref")?,
                ),
                (
                    "amount_commitment",
                    parse_string_param(&params, 6, "amount_commitment")?,
                ),
                ("proof_kind", parse_string_param(&params, 7, "proof_kind")?),
                (
                    "proof_receipt_hash",
                    parse_string_param(&params, 8, "proof_receipt_hash")?,
                ),
                ("status", parse_string_param(&params, 9, "status")?),
                ("intent_id", parse_string_param(&params, 10, "intent_id")?),
            ];
            for (suffix, value) in fields {
                storage_set_utf8(
                    storage,
                    balance_workflow_key(&workflow_ref, suffix),
                    normalize_non_empty(&value, suffix)?,
                )?;
            }
            Ok(frame_bool(true))
        }
        "register_workflow_record" => {
            let workflow_ref = normalize_hex64(
                &parse_string_param(&params, 0, "workflow_ref")?,
                "workflow_ref",
            )?;
            let fields = [
                (
                    "register_ref",
                    parse_string_param(&params, 1, "register_ref")?,
                ),
                (
                    "previous_state_ref",
                    parse_string_param(&params, 2, "previous_state_ref")?,
                ),
                (
                    "next_state_ref",
                    parse_string_param(&params, 3, "next_state_ref")?,
                ),
                (
                    "workflow_kind",
                    parse_string_param(&params, 4, "workflow_kind")?,
                ),
                ("proof_kind", parse_string_param(&params, 5, "proof_kind")?),
                (
                    "proof_receipt_hash",
                    parse_string_param(&params, 6, "proof_receipt_hash")?,
                ),
                ("status", parse_string_param(&params, 7, "status")?),
                ("intent_id", parse_string_param(&params, 8, "intent_id")?),
            ];
            for (suffix, value) in fields {
                storage_set_utf8(
                    storage,
                    register_workflow_key(&workflow_ref, suffix),
                    normalize_non_empty(&value, suffix)?,
                )?;
            }
            Ok(frame_bool(true))
        }
        "object_bind" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 1, "state_ref")?, "state_ref")?;
            let transition_ref = normalize_hex64(
                &parse_string_param(&params, 2, "transition_ref")?,
                "transition_ref",
            )?;
            let status = normalize_non_empty(&parse_string_param(&params, 3, "status")?, "status")?;
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "current_state_ref"),
                state_ref,
            )?;
            let version =
                increment_stored_version(storage, &object_binding_key(&object_ref, "version"))?;
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "last_transition_ref"),
                transition_ref,
            )?;
            storage_set_utf8(storage, object_binding_key(&object_ref, "status"), status)?;
            Ok(frame_int(version))
        }
        "object_member_attach" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let member_ref =
                normalize_non_empty(&parse_string_param(&params, 1, "member_ref")?, "member_ref")?;
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 2, "state_ref")?, "state_ref")?;
            let member_kind = normalize_non_empty(
                &parse_string_param(&params, 3, "member_kind")?,
                "member_kind",
            )?;
            let state_class = normalize_non_empty(
                &parse_string_param(&params, 4, "state_class")?,
                "state_class",
            )?;
            let codec = normalize_non_empty(&parse_string_param(&params, 5, "codec")?, "codec")?;
            let status = normalize_non_empty(&parse_string_param(&params, 6, "status")?, "status")?;
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "state_ref"),
                state_ref,
            )?;
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "member_kind"),
                member_kind,
            )?;
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "state_class"),
                state_class,
            )?;
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "codec"),
                codec,
            )?;
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "status"),
                status,
            )?;
            Ok(frame_bool(true))
        }
        "object_member_detach" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let member_ref =
                normalize_non_empty(&parse_string_param(&params, 1, "member_ref")?, "member_ref")?;
            storage.remove(&object_member_key(&object_ref, &member_ref, "state_ref"))?;
            storage.remove(&object_member_key(&object_ref, &member_ref, "member_kind"))?;
            storage.remove(&object_member_key(&object_ref, &member_ref, "state_class"))?;
            storage.remove(&object_member_key(&object_ref, &member_ref, "codec"))?;
            storage.remove(&object_member_key(&object_ref, &member_ref, "status"))?;
            Ok(frame_bool(true))
        }
        "object_policy_define" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_optional_utf8(
                storage,
                object_policy_key(&object_ref, "delivery_key_id"),
                normalize_optional_string(parse_optional_string_param(&params, 1)?),
            )?;
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "activate_after_epoch"),
                parse_int_param(&params, 2, "activate_after_epoch")?,
            )?;
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "expire_after_epoch"),
                parse_int_param(&params, 3, "expire_after_epoch")?,
            )?;
            storage_set_utf8(
                storage,
                object_policy_key(&object_ref, "transition_mode"),
                normalize_non_empty(
                    &parse_string_param(&params, 4, "transition_mode")?,
                    "transition_mode",
                )?,
            )?;
            storage_set_utf8(
                storage,
                object_policy_key(&object_ref, "required_proof_kind"),
                normalize_non_empty(
                    &parse_string_param(&params, 5, "required_proof_kind")?,
                    "required_proof_kind",
                )?,
            )?;
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "member_quorum"),
                parse_int_param(&params, 6, "member_quorum")?,
            )?;
            storage_set_bool(
                storage,
                object_policy_key(&object_ref, "allow_detach"),
                parse_bool_param(&params, 7, "allow_detach")?,
            )?;
            storage_set_bool(
                storage,
                object_policy_key(&object_ref, "allow_root_state_rotation"),
                parse_bool_param(&params, 8, "allow_root_state_rotation")?,
            )?;
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), false)?;
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), false)?;
            Ok(frame_bool(true))
        }
        "object_policy_release" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage.remove(&object_policy_key(&object_ref, "delivery_key_id"))?;
            storage.remove(&object_policy_key(&object_ref, "activate_after_epoch"))?;
            storage.remove(&object_policy_key(&object_ref, "expire_after_epoch"))?;
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), false)?;
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), false)?;
            Ok(frame_bool(true))
        }
        "object_policy_retire" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "expire_after_epoch"),
                parse_int_param(&params, 1, "expire_after_epoch")?,
            )?;
            Ok(frame_bool(true))
        }
        "object_policy_tombstone" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), true)?;
            Ok(frame_bool(true))
        }
        "object_policy_restore" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), false)?;
            Ok(frame_bool(true))
        }
        "object_policy_revoke" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), true)?;
            Ok(frame_bool(true))
        }
        "object_policy_reinstate" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), false)?;
            Ok(frame_bool(true))
        }
        "object_transition_record" => {
            let transition_ref = normalize_hex64(
                &parse_string_param(&params, 0, "transition_ref")?,
                "transition_ref",
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "object_ref"),
                normalize_hex64(&parse_string_param(&params, 1, "object_ref")?, "object_ref")?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "previous_state_ref"),
                normalize_hex64(
                    &parse_string_param(&params, 2, "previous_state_ref")?,
                    "previous_state_ref",
                )?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "next_state_ref"),
                normalize_hex64(
                    &parse_string_param(&params, 3, "next_state_ref")?,
                    "next_state_ref",
                )?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "touched_members_hash"),
                normalize_hex64(
                    &parse_string_param(&params, 4, "touched_members_hash")?,
                    "touched_members_hash",
                )?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 5, "proof_kind")?, "proof_kind")?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_receipt_hash"),
                normalize_hex64(
                    &parse_string_param(&params, 6, "proof_receipt_hash")?,
                    "proof_receipt_hash",
                )?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "status"),
                normalize_non_empty(&parse_string_param(&params, 7, "status")?, "status")?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "intent_id"),
                normalize_hex64(&parse_string_param(&params, 8, "intent_id")?, "intent_id")?,
            )?;
            Ok(frame_bool(true))
        }
        "object_transition_apply" => {
            let transition_ref = normalize_hex64(
                &parse_string_param(&params, 0, "transition_ref")?,
                "transition_ref",
            )?;
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 1, "object_ref")?, "object_ref")?;
            let previous_state_ref = normalize_hex64(
                &parse_string_param(&params, 2, "previous_state_ref")?,
                "previous_state_ref",
            )?;
            let next_state_ref = normalize_hex64(
                &parse_string_param(&params, 3, "next_state_ref")?,
                "next_state_ref",
            )?;
            let member_bundle = parse_string_param(&params, 4, "member_bundle")?;
            let deltas = parse_object_member_bundle(&member_bundle)?;
            let touched_members_hash = normalize_hex64(
                &parse_string_param(&params, 5, "touched_members_hash")?,
                "touched_members_hash",
            )?;
            let computed_hash = hex::encode(Sha256::digest(member_bundle.as_bytes()));
            if computed_hash != touched_members_hash {
                return Err("object transition touched_members_hash mismatch".to_owned().into());
            }
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "object_ref"),
                object_ref.clone(),
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "previous_state_ref"),
                previous_state_ref,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "next_state_ref"),
                next_state_ref.clone(),
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "touched_members_hash"),
                touched_members_hash,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 6, "proof_kind")?, "proof_kind")?,
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_receipt_hash"),
                normalize_hex64(
                    &parse_string_param(&params, 7, "proof_receipt_hash")?,
                    "proof_receipt_hash",
                )?,
            )?;
            let status = normalize_non_empty(&parse_string_param(&params, 8, "status")?, "status")?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "status"),
                status.clone(),
            )?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "intent_id"),
                normalize_hex64(&parse_string_param(&params, 9, "intent_id")?, "intent_id")?,
            )?;
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "current_state_ref"),
                next_state_ref,
            )?;
            let version =
                increment_stored_version(storage, &object_binding_key(&object_ref, "version"))?;
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "last_transition_ref"),
                transition_ref,
            )?;
            storage_set_utf8(storage, object_binding_key(&object_ref, "status"), status)?;
            for delta in deltas {
                match delta {
                    ObjectMemberDelta::Attach {
                        member_ref,
                        state_ref,
                        member_kind,
                        state_class,
                        codec,
                        status,
                    } => {
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "state_ref"),
                            state_ref,
                        )?;
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "member_kind"),
                            member_kind,
                        )?;
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "state_class"),
                            state_class,
                        )?;
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "codec"),
                            codec,
                        )?;
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "status"),
                            status,
                        )?;
                    }
                    ObjectMemberDelta::Detach { member_ref } => {
                        storage.remove(&object_member_key(&object_ref, &member_ref, "state_ref"))?;
                        storage.remove(&object_member_key(&object_ref, &member_ref, "member_kind"))?;
                        storage.remove(&object_member_key(&object_ref, &member_ref, "state_class"))?;
                        storage.remove(&object_member_key(&object_ref, &member_ref, "codec"))?;
                        storage.remove(&object_member_key(&object_ref, &member_ref, "status"))?;
                    }
                }
            }
            Ok(frame_int(version))
        }
        _ => Err(format!(
            "unsupported circle host method: {}",
            request.method
        ).into()),
    }
}

fn hfhe_capability_name(method: &str) -> Option<&'static str> {
    match method {
        "fhe_load_pk" => Some("fhe_load_pk"),
        "fhe_encrypt" | "fhe_encrypt_zero" => Some("fhe_encrypt"),
        "fhe_decrypt" => Some("fhe_decrypt"),
        "fhe_add" | "fhe_sub" | "fhe_scale" | "fhe_add_const" | "fhe_sub_const" => {
            Some("fhe_cipher_arithmetic")
        }
        "fhe_commit" | "fhe_bound_commitment" => Some("fhe_commit"),
        "fhe_pedersen" => Some("fhe_pedersen"),
        "fhe_verify_zero" => Some("fhe_verify_zero"),
        "fhe_verify_range" => Some("fhe_verify_range"),
        "fhe_verify_bound" => Some("fhe_verify_bound"),
        _ => None,
    }
}

fn hfhe_receipt_hash(domain: &[u8], value: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(domain);
    hasher.update([0]);
    hasher.update(value);
    hex::encode(hasher.finalize())
}

fn hfhe_request_domain(strict: bool) -> &'static [u8] {
    if strict {
        b"octra:circle_hfhe_request:standard:v1"
    } else {
        b"octra:circle_hfhe_request:v1"
    }
}

fn hfhe_response_domain(strict: bool) -> &'static [u8] {
    if strict {
        b"octra:circle_hfhe_response:standard:v1"
    } else {
        b"octra:circle_hfhe_response:v1"
    }
}

fn hfhe_response_bool(response: &[u8]) -> Option<bool> {
    if response == frame_bool(false) {
        Some(false)
    } else if response == frame_bool(true) {
        Some(true)
    } else {
        None
    }
}

fn hfhe_receipt_entry(
    strict: bool,
    method: &str,
    request: &[u8],
    response: &[u8],
) -> Result<HfheReceiptEntryJson, String> {
    let result = if is_hfhe_verify_method(method) {
        Some(
            hfhe_response_bool(response)
                .ok_or_else(|| "invalid hfhe verifier response".to_owned())?,
        )
    } else {
        None
    };
    Ok(HfheReceiptEntryJson {
        method: method.to_owned(),
        request_hash: hfhe_receipt_hash(hfhe_request_domain(strict), request),
        response_hash: hfhe_receipt_hash(hfhe_response_domain(strict), response),
        result,
    })
}

fn expected_hfhe_receipt_entry(
    caller: &Caller<'_, HostState>,
) -> Result<HfheReceiptEntryJson, String> {
    caller
        .data()
        .hfhe_receipt_expected
        .get(caller.data().hfhe_receipt_index)
        .cloned()
        .ok_or_else(|| "hfhe receipt exhausted".to_owned())
}

fn consume_hfhe_receipt_entry(
    caller: &mut Caller<'_, HostState>,
    entry: HfheReceiptEntryJson,
) -> Result<(), String> {
    let expected = expected_hfhe_receipt_entry(caller)?;
    match_hfhe_receipt_entry(&expected, &entry)?;
    let state = caller.data_mut();
    state.hfhe_receipt_index += 1;
    state.hfhe_receipt_entries.push(entry);
    Ok(())
}

fn match_hfhe_receipt_entry(
    expected: &HfheReceiptEntryJson,
    actual: &HfheReceiptEntryJson,
) -> Result<(), String> {
    if expected == actual {
        Ok(())
    } else {
        Err("hfhe receipt entry mismatch".to_owned())
    }
}

fn match_hfhe_receipt_request(
    expected: &HfheReceiptEntryJson,
    method: &str,
    request_hash: &str,
) -> Result<(), String> {
    if expected.method == method && expected.request_hash == request_hash {
        Ok(())
    } else {
        Err("hfhe receipt request mismatch".to_owned())
    }
}

fn execute_hfhe_invoke(
    caller: &mut Caller<'_, HostState>,
    req_bytes: &[u8],
) -> Result<Vec<u8>, HostFailure> {
    let request = rejected(parse_request_frame(req_bytes))?;
    let capability = rejected(
        hfhe_capability_name(&request.method)
            .ok_or_else(|| format!("unsupported hfhe host method: {}", request.method)),
    )?;
    if !caller.data().hfhe_caps.contains(capability) {
        return Err(HostFailure::Rejected(format!(
            "hfhe capability denied: {}",
            request.method
        )));
    }
    let method = request.method.clone();
    let mode = caller.data().hfhe_receipt_mode.clone();
    let strict = caller.data().hfhe_strict;
    let is_verify = is_hfhe_verify_method(&method);
    let pair = caller.data().hfhe_pairs && matches!(method.as_str(), "fhe_add" | "fhe_sub");
    if (mode == "capture" || (mode == "consume" && pair))
        && caller.data().hfhe_receipt_entries.len() >= MAX_HFHE_RECEIPT_ENTRIES
    {
        return Err(HostFailure::Rejected(
            "hfhe receipt entry limit exceeded".to_owned(),
        ));
    }
    if mode == "capture"
        && is_verify
        && caller
            .data()
            .hfhe_receipt_entries
            .iter()
            .filter(|entry| is_hfhe_verify_method(&entry.method))
            .count()
            >= MAX_HFHE_VERIFIER_ENTRIES
    {
        return Err(HostFailure::Rejected(
            "hfhe verifier entry limit exceeded".to_owned(),
        ));
    }
    if mode == "consume" {
        let expected = rejected(expected_hfhe_receipt_entry(caller))?;
        let request_hash = hfhe_receipt_hash(hfhe_request_domain(strict), req_bytes);
        rejected(match_hfhe_receipt_request(
            &expected,
            &method,
            &request_hash,
        ))?;
        if is_verify {
            let result = rejected(
                expected
                    .result
                    .ok_or_else(|| "hfhe verifier result missing".to_owned()),
            )?;
            let response = frame_bool(result);
            let entry = rejected(hfhe_receipt_entry(strict, &method, req_bytes, &response))?;
            rejected(consume_hfhe_receipt_entry(caller, entry))?;
            return Ok(response);
        }
    } else if !caller.data().is_view && is_verify && mode != "capture" {
        return Err(HostFailure::Rejected(format!(
            "hfhe proof verification requires a preverified receipt: {}",
            method
        )));
    }
    let result = execute_hfhe_direct(caller, req_bytes);
    let refusal;
    let response = match &result {
        Ok(response) => response.as_slice(),
        Err(HostFailure::Rejected(reason)) if pair => {
            refusal = [b"octra:circle_hfhe_refusal:v1\0".as_slice(), reason.as_bytes()].concat();
            refusal.as_slice()
        }
        Err(_) => return result,
    };
    let entry = rejected(hfhe_receipt_entry(strict, &method, req_bytes, response))?;
    match mode.as_str() {
        "capture" => caller.data_mut().hfhe_receipt_entries.push(entry),
        "consume" => rejected(consume_hfhe_receipt_entry(caller, entry))?,
        "direct" => (),
        _ => {
            return Err(HostFailure::Rejected(
                "invalid hfhe receipt mode".to_owned(),
            ))
        }
    }
    result
}

fn execute_hfhe_direct(
    caller: &mut Caller<'_, HostState>,
    req_bytes: &[u8],
) -> Result<Vec<u8>, HostFailure> {
    let request = rejected(parse_request_frame(req_bytes))?;
    let capability = rejected(
        hfhe_capability_name(&request.method)
            .ok_or_else(|| format!("unsupported hfhe host method: {}", request.method)),
    )?;
    if !caller.data().hfhe_caps.contains(capability) {
        return Err(HostFailure::Rejected(format!(
            "hfhe capability denied: {}",
            request.method
        )));
    }
    let strict = caller.data().hfhe_strict;
    let math = caller.data().math;
    let cap = caller.data().is_view || strict;
    let params = request.params;
    match request.method.as_str() {
        "fhe_load_pk" => {
            let requested_addr = rejected(parse_string_param(&params, 0, "requested_addr"))?;
            let pubkey = rejected(
                caller
                    .data()
                    .hfhe_pubkeys
                    .get(&requested_addr)
                    .cloned()
                    .ok_or_else(|| format!("hfhe pubkey unavailable: {requested_addr}")),
            )?;
            Ok(frame_string(pubkey))
        }
        "fhe_encrypt" => {
            let active = rejected(
                caller
                    .data()
                    .hfhe_active_key
                    .clone()
                    .ok_or_else(|| "hfhe active key unavailable".to_owned()),
            )?;
            let amount = rejected(parse_int_param(&params, 0, "amount"))?;
            let seed_b64 = rejected(parse_string_param(&params, 1, "seed_b64"))?;
            let value = call_hfhe_backend(json!({
                "action": "encrypt_value_seeded",
                "pubkey_b64": active.pubkey_b64,
                "seckey_b64": active.seckey_b64,
                "amount": amount,
                "seed_b64": seed_b64,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_encrypt_zero" => {
            let active = rejected(
                caller
                    .data()
                    .hfhe_active_key
                    .clone()
                    .ok_or_else(|| "hfhe active key unavailable".to_owned()),
            )?;
            let seed_b64 = rejected(parse_string_param(&params, 0, "seed_b64"))?;
            let value = call_hfhe_backend(json!({
                "action": "encrypt_zero_seeded",
                "pubkey_b64": active.pubkey_b64,
                "seckey_b64": active.seckey_b64,
                "seed_b64": seed_b64,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_decrypt" => {
            let active = rejected(
                caller
                    .data()
                    .hfhe_active_key
                    .clone()
                    .ok_or_else(|| "hfhe active key unavailable".to_owned()),
            )?;
            let ciphertext = rejected(parse_string_param(&params, 0, "ciphertext"))?;
            let value = call_hfhe_backend(json!({
                "action": "decrypt_value",
                "pubkey_b64": active.pubkey_b64,
                "seckey_b64": active.seckey_b64,
                "ciphertext": ciphertext,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_int(expect_backend_string(value)?))
        }
        "fhe_add" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let lhs_ciphertext = rejected(parse_string_param(&params, 1, "lhs_ciphertext"))?;
            let rhs_ciphertext = rejected(parse_string_param(&params, 2, "rhs_ciphertext"))?;
            let value = call_hfhe_pair(caller.data().hfhe_pairs, json!({
                "action": "cipher_add",
                "pubkey_b64": pubkey_b64,
                "lhs_ciphertext": lhs_ciphertext,
                "rhs_ciphertext": rhs_ciphertext,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_sub" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let lhs_ciphertext = rejected(parse_string_param(&params, 1, "lhs_ciphertext"))?;
            let rhs_ciphertext = rejected(parse_string_param(&params, 2, "rhs_ciphertext"))?;
            let value = call_hfhe_pair(caller.data().hfhe_pairs, json!({
                "action": "cipher_sub",
                "pubkey_b64": pubkey_b64,
                "lhs_ciphertext": lhs_ciphertext,
                "rhs_ciphertext": rhs_ciphertext,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_scale" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let factor = rejected(parse_int_param(&params, 2, "factor"))?;
            let value = call_hfhe_backend(json!({
                "action": "cipher_scale",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "factor": factor,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_add_const" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let amount = rejected(parse_int_param(&params, 2, "amount"))?;
            let value = call_hfhe_backend(json!({
                "action": "cipher_add_const",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "amount": amount,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_sub_const" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let amount = rejected(parse_int_param(&params, 2, "amount"))?;
            let value = call_hfhe_backend(json!({
                "action": "cipher_sub_const",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "amount": amount,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_pedersen" => {
            let amount = rejected(parse_int_param(&params, 0, "amount"))?;
            let blinding_b64 = rejected(parse_string_param(&params, 1, "blinding_b64"))?;
            let value = call_hfhe_backend(json!({
                "action": "pedersen_commit",
                "amount": amount,
                "blinding_b64": blinding_b64,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_commit" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let value = call_hfhe_backend(json!({
                "action": "commit_cipher",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_bound_commitment" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let amount = rejected(parse_int_param(&params, 2, "amount"))?;
            let value = call_hfhe_backend(json!({
                "action": "bound_commitment",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "amount": amount,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_verify_zero" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let proof = rejected(parse_string_param(&params, 2, "proof"))?;
            let value = call_hfhe_backend(json!({
                "action": "verify_zero",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "proof": proof,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_bool(expect_backend_bool(value)?))
        }
        "fhe_verify_range" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let proof = rejected(parse_string_param(&params, 2, "proof"))?;
            let amount_commitment = rejected(parse_string_param(&params, 3, "amount_commitment"))?;
            let value = call_hfhe_backend(json!({
                "action": "verify_range",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "proof": proof,
                "amount_commitment": amount_commitment,
                "strict": strict,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_bool(expect_backend_bool(value)?))
        }
        "fhe_verify_bound" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let ciphertext = rejected(parse_string_param(&params, 1, "ciphertext"))?;
            let proof = rejected(parse_string_param(&params, 2, "proof"))?;
            let amount_commitment = rejected(parse_string_param(&params, 3, "amount_commitment"))?;
            let value = call_hfhe_backend(json!({
                "action": "verify_bound",
                "pubkey_b64": pubkey_b64,
                "ciphertext": ciphertext,
                "proof": proof,
                "amount_commitment": amount_commitment,
                "strict": strict,
                "cap": cap,
                "math": math,
            }))?;
            Ok(frame_bool(expect_backend_bool(value)?))
        }
        _ => Err(HostFailure::Rejected(format!(
            "unsupported hfhe host method: {}",
            request.method
        ))),
    }
}

fn call_hfhe_pair(pairs: bool, mut payload: JsonValue) -> Result<JsonValue, HostFailure> {
    if pairs {
        payload["hfhe_pairs"] = JsonValue::Bool(true);
    }
    call_hfhe_backend(payload)
}

fn call_hfhe_backend(payload: JsonValue) -> Result<JsonValue, HostFailure> {
    let input = serde_json::to_string(&payload)
        .map_err(|e| HostFailure::Unavailable(format!("hfhe payload encode failed: {e}")))?;
    let mut out_ptr: *mut u8 = std::ptr::null_mut();
    let mut out_len: usize = 0;
    let mut err_ptr: *mut u8 = std::ptr::null_mut();
    let mut err_len: usize = 0;
    let rc = unsafe {
        octra_circle_wasm_host_hfhe_call_json(
            input.as_ptr(),
            input.len(),
            &mut out_ptr,
            &mut out_len,
            &mut err_ptr,
            &mut err_len,
        )
    };
    if rc != 0 {
        let message = unsafe { copy_and_free_malloc_string(err_ptr, err_len) };
        return Err(HostFailure::Unavailable(if message.is_empty() {
            "hfhe backend failed".to_owned()
        } else {
            message
        }));
    }
    let output = unsafe { copy_and_free_malloc_string(out_ptr, out_len) };
    let response: JsonValue = serde_json::from_str(&output).map_err(|e| {
        HostFailure::Unavailable(format!("hfhe backend returned invalid json: {e}"))
    })?;
    match response.get("ok").and_then(|value| value.as_bool()) {
        Some(true) => Ok(response.get("value").cloned().unwrap_or(JsonValue::Null)),
        _ => {
            let reason = response
                .get("error")
                .and_then(|value| value.as_str())
                .unwrap_or("hfhe backend failed")
                .to_owned();
            if response.get("class").and_then(|value| value.as_str()) == Some("unavailable") {
                Err(HostFailure::Unavailable(reason))
            } else {
                Err(HostFailure::Rejected(reason))
            }
        }
    }
}

unsafe fn copy_and_free_malloc_string(ptr: *mut u8, len: usize) -> String {
    if ptr.is_null() || len == 0 {
        if !ptr.is_null() {
            libc::free(ptr.cast());
        }
        return String::new();
    }
    let bytes = slice::from_raw_parts(ptr, len).to_vec();
    libc::free(ptr.cast());
    String::from_utf8(bytes).unwrap_or_else(|_| "hfhe backend failed".to_owned())
}

fn expect_backend_string(value: JsonValue) -> Result<String, HostFailure> {
    match value {
        JsonValue::String(value) => Ok(value),
        JsonValue::Number(value) => Ok(value.to_string()),
        _ => Err(HostFailure::Unavailable(
            "hfhe backend returned non-string value".to_owned(),
        )),
    }
}

fn expect_backend_bool(value: JsonValue) -> Result<bool, HostFailure> {
    value
        .as_bool()
        .ok_or_else(|| HostFailure::Unavailable("hfhe backend returned non-bool value".to_owned()))
}

fn state_path_key(raw_state_ref: &str) -> String {
    let state_ref = raw_state_ref.trim().to_lowercase();
    let canonical_path = format!("/_state/{state_ref}");
    h256_hex("octra:asset_state_path:v1", &[canonical_path.as_bytes()])
}

fn h256_raw(tag: &str, parts: &[&[u8]]) -> Vec<u8> {
    let mut hasher = Sha256::new();
    hasher.update(tag.as_bytes());
    hasher.update([0_u8]);
    for part in parts {
        hasher.update((part.len() as u32).to_be_bytes());
        hasher.update(part);
    }
    hasher.finalize().to_vec()
}

fn h256_hex(tag: &str, parts: &[&[u8]]) -> String {
    hex::encode(h256_raw(tag, parts))
}

fn encode_u64(value: i32) -> [u8; 8] {
    (value as u64).to_be_bytes()
}

fn base58_encode(bytes: &[u8]) -> String {
    let mut digits = vec![0_u8];
    for byte in bytes {
        let mut carry = *byte as u32;
        for digit in digits.iter_mut() {
            let value = (*digit as u32) * 256 + carry;
            *digit = (value % 58) as u8;
            carry = value / 58;
        }
        while carry > 0 {
            digits.push((carry % 58) as u8);
            carry /= 58;
        }
    }
    let zeros = bytes.iter().take_while(|byte| **byte == 0).count();
    let mut out = String::with_capacity(zeros + digits.len());
    for _ in 0..zeros {
        out.push('1');
    }
    for digit in digits.iter().rev() {
        out.push(BASE58_ALPHABET[*digit as usize] as char);
    }
    if out.is_empty() {
        "1".to_owned()
    } else {
        out
    }
}

fn base58_part(value: &str) -> String {
    if value.len() >= 44 {
        value[..44].to_owned()
    } else if value.is_empty() {
        "1".repeat(44)
    } else {
        let mut out = value.to_owned();
        let bytes = value.as_bytes();
        let mut i = 0_usize;
        while out.len() < 44 {
            out.push(bytes[i % bytes.len()] as char);
            i += 1;
        }
        out[..44].to_owned()
    }
}

fn circle_id_of_spawn(
    state: &HostState,
    owner_mode: &str,
    spawn_nonce: i32,
    payload_json: &str,
) -> String {
    let nonce = encode_u64(spawn_nonce);
    let seed = h256_raw(
        "octra:circle_spawn_id:v1",
        &[
            &state.self_bytes,
            &state.caller_bytes,
            &state.tx_hash_bytes,
            &nonce,
            owner_mode.as_bytes(),
            payload_json.as_bytes(),
        ],
    );
    format!("oct{}", base58_part(&base58_encode(&seed)))
}

fn get_u8(raw: &[u8], offset: &mut usize) -> Result<u8, String> {
    if *offset >= raw.len() {
        Err("out of bounds".to_owned())
    } else {
        let value = raw[*offset];
        *offset += 1;
        Ok(value)
    }
}

fn get_u16(raw: &[u8], offset: &mut usize) -> Result<u16, String> {
    let bytes = read_bytes(raw, offset, 2)?;
    Ok(u16::from_be_bytes([bytes[0], bytes[1]]))
}

fn get_u32(raw: &[u8], offset: &mut usize) -> Result<u32, String> {
    let bytes = read_bytes(raw, offset, 4)?;
    Ok(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
}

fn read_bytes<'a>(raw: &'a [u8], offset: &mut usize, len: usize) -> Result<&'a [u8], String> {
    if *offset + len > raw.len() {
        Err("out of bounds".to_owned())
    } else {
        let value = &raw[*offset..*offset + len];
        *offset += len;
        Ok(value)
    }
}

fn read_utf8(raw: &[u8], offset: &mut usize, len: usize) -> Result<String, String> {
    let bytes = read_bytes(raw, offset, len)?;
    String::from_utf8(bytes.to_vec()).map_err(|_| "invalid utf8".to_owned())
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{
        BlockType, CodeSection, ConstExpr, DataSection, EntityType, ExportKind,
        ExportSection, Function, FunctionSection, GlobalSection, GlobalType,
        ImportSection, Instruction as I, MemorySection, MemoryType,
        Module as WasmModule, StartSection, TypeSection, ValType,
    };

    static INPUT_TEST: Mutex<()> = Mutex::new(());

    fn response_input(local: bool, compute: bool) -> Payload<'static> {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
        types.ty().function([], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", "host_response_write", EntityType::Function(0));
        imports.import("octra", "host_response_reset", EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [2, 0, 0, 1] { functions.function(ty); }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1, maximum: Some(1), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        for (name, index) in [("octra_alloc", 2), ("octra_query", 3), ("write", 4), ("reset", 5)] {
            exports.export(name, ExportKind::Func, index);
        }
        module.section(&exports);
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        alloc.instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&alloc);
        let mut query = Function::new([]);
        query.instruction(&I::LocalGet(0)).instruction(&I::LocalGet(1))
            .instruction(&I::Call(0)).instruction(&I::Drop)
            .instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&query);
        let mut write = Function::new([]);
        write.instruction(&I::LocalGet(0)).instruction(&I::LocalGet(1))
            .instruction(&I::Call(0)).instruction(&I::End);
        code.function(&write);
        let mut reset = Function::new([]);
        reset.instruction(&I::Call(1)).instruction(&I::End);
        code.function(&reset);
        module.section(&code);
        Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "is_view": true,
            "hfhe_strict": false, "inspect_policy": local,
            "execution_profile": if compute { "compute" } else { "standard" },
            "fuel_limit": 20_000_000,
        })).unwrap()
    }

    #[test]
    fn response_quota() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let mut runtime = Runtime::instantiate(&response_input(true, compute), false).unwrap();
            let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - compute_memory().used()).unwrap();
            let (result, count) = native_bytes::tests::large_allocs(4097,
                || write.call(&mut runtime.store, (0, 4097)));
            assert!(result.is_err(), "response bypassed the shared memory quota");
            assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
            assert_eq!(count, 0, "response allocated before its reservation");
            assert!(runtime.store.data().response_bytes.is_empty());
            drop(held);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn response_growth() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let mut runtime = Runtime::instantiate(&response_input(true, compute), false).unwrap();
            let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
            write.call(&mut runtime.store, (0, 4097)).unwrap();
            let used = compute_memory().used();
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used - 4097).unwrap();
            let (result, count) = native_bytes::tests::large_allocs(4097,
                || write.call(&mut runtime.store, (0, 4097)));
            assert!(result.is_err(), "growth released the live response too soon");
            assert_eq!(count, 0);
            assert_eq!(runtime.store.data().response_bytes.len(), 4097);
            assert!(runtime.store.data().response_bytes.iter().all(|byte| *byte == 0));
            drop(held);
            assert_eq!(compute_memory().used(), used);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn response_owner() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let mut runtime = Runtime::instantiate(&response_input(true, compute), false).unwrap();
            let input = [7_u8; 4097];
            let ((code, output, _), count) = native_bytes::tests::large_allocs(input.len(),
                || runtime.call_export("octra_query", &input).unwrap());
            assert_eq!(code, 0);
            assert_eq!(output.as_slice(), input);
            assert_eq!(count, 1, "response was copied on return");
            assert!(runtime.store.data().response_bytes.is_empty());
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline + input.len());
            drop(output);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn response_alloc() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let mut runtime = Runtime::instantiate(&response_input(true, compute), false).unwrap();
            let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
            let used = compute_memory().used();
            let result = native_bytes::tests::refused_alloc(4097,
                || write.call(&mut runtime.store, (0, 4097)));
            assert!(result.is_err());
            assert_eq!(runtime.store.data().unavailable.as_deref(), Some("response allocation failed"));
            assert!(runtime.store.data().response_bytes.is_empty());
            assert_eq!(compute_memory().used(), used);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn response_modes() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let mut outputs = Vec::new();
        for (local, compute) in [(false, false), (true, false), (true, true)] {
            let mut runtime = Runtime::instantiate(&response_input(local, compute), false).unwrap();
            let input = [7_u8; 4097];
            let (code, output, fuel) = runtime.call_export("octra_query", &input).unwrap();
            outputs.push((code, output.as_slice().to_vec(), fuel));
            drop(output);
            let used = compute_memory().used();
            let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
            let reset = runtime.instance.get_typed_func::<(), i32>(&runtime.store, "reset").unwrap();
            reset.call(&mut runtime.store, ()).unwrap();
            for _ in 0..3 {
                write.call(&mut runtime.store, (0, 4097)).unwrap();
                write.call(&mut runtime.store, (0, 4097)).unwrap();
                assert_eq!(runtime.store.data().response_bytes.len(), 8194);
                assert_eq!(compute_memory().used(), used + if local { 8194 } else { 0 });
                reset.call(&mut runtime.store, ()).unwrap();
                assert!(runtime.store.data().response_bytes.is_empty());
                assert_eq!(compute_memory().used(), used);
            }
            if !local {
                let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
                write.call(&mut runtime.store, (0, 4097)).unwrap();
                assert_eq!(runtime.store.data().response_bytes.len(), 4097);
                drop(held);
            }
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
        assert_eq!(outputs[0], outputs[1]);
        assert_eq!(outputs[0], outputs[2]);
    }

    #[test]
    fn response_errors() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for (ptr, len) in [(-1, 1), (0, -1), (65_536, 0), (65_536, 1), (i32::MAX, 1)] {
            let mut outputs = Vec::new();
            for (local, compute) in [(false, false), (true, false), (true, true)] {
                let mut runtime = Runtime::instantiate(&response_input(local, compute), false).unwrap();
                let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
                let result = write.call(&mut runtime.store, (ptr, len)).map_err(|error| error.to_string());
                outputs.push((result, runtime.store.get_fuel().unwrap()));
                assert!(runtime.store.data().response_bytes.is_empty());
                drop(runtime);
                assert_eq!(compute_memory().used(), baseline);
            }
            assert_eq!(outputs[0], outputs[1]);
            assert_eq!(outputs[0], outputs[2]);
        }
        for (local, compute) in [(false, false), (true, false), (true, true)] {
            let mut runtime = Runtime::instantiate(&response_input(local, compute), false).unwrap();
            let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
            for _ in 0..32 { write.call(&mut runtime.store, (0, 65_536)).unwrap(); }
            assert_eq!(runtime.store.data().response_bytes.len(), MAX_RESPONSE_BYTES);
            assert_eq!(write.call(&mut runtime.store, (0, 1)).unwrap_err().to_string(), "wasm response exceeds limit");
            assert_eq!(runtime.store.data().response_bytes.len(), MAX_RESPONSE_BYTES);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn response_ffi() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let payload = response_input(true, compute);
            let input = serde_json::to_vec(&json!({
                "action": "execute", "export_name": "octra_query",
                "request_b64": encode_b64(&[7; 4097]),
                "code_b64": payload.code_b64.as_deref(), "hfhe_strict": false,
                "is_view": true, "inspect_policy": !compute,
                "execution_profile": if compute { "compute" } else { "standard" },
            })).unwrap();
            for refused in [true, false] {
                let mut output = std::ptr::null_mut();
                let mut size = 0;
                let mut error = std::ptr::null_mut();
                let mut error_size = 0;
                let mut owner = std::ptr::null_mut();
                let mut call = || octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
                    &mut output, &mut size, &mut error, &mut error_size, &mut owner);
                let status = if refused { native_bytes::tests::refused_alloc(4097, call) } else { call() };
                assert_eq!(status, 0);
                assert_eq!(error_size, 0);
                let value: JsonValue = unsafe {
                    serde_json::from_slice(slice::from_raw_parts(output, size)).unwrap()
                };
                assert_eq!(value["success"], !refused);
                if refused {
                    assert_eq!(value["unavailable"], "response allocation failed");
                    assert!(value["response_b64"].is_null());
                } else {
                    assert!(value["unavailable"].is_null());
                    assert_eq!(decode_b64("response_b64", value["response_b64"].as_str().unwrap()).unwrap(), [7; 4097]);
                }
                assert!(!owner.is_null());
                unsafe { octra_compute_input_drop(owner); }
                octra_circle_wasm_host_free_bytes(error, error_size);
                assert_eq!(compute_memory().used(), baseline);
            }
        }
    }

    #[test]
    fn reply_lifetime() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let payload = response_input(true, true);
        let input = serde_json::to_vec(&json!({
            "action": "execute", "export_name": "octra_query",
            "request_b64": encode_b64(&[7; 4097]),
            "code_b64": payload.code_b64.as_deref(), "hfhe_strict": false,
            "is_view": true, "execution_profile": "compute",
        })).unwrap();
        let mut output = std::ptr::null_mut();
        let mut size = 0;
        let mut error = std::ptr::null_mut();
        let mut error_size = 0;
        let mut owner = std::ptr::null_mut();
        let status = octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
            &mut output, &mut size, &mut error, &mut error_size, &mut owner);
        assert_eq!(status, 0);
        assert!(size > 4097);
        assert!(compute_memory().used() >= baseline + size,
            "encoded reply lost its reservation before the foreign copy");
        let value: JsonValue = unsafe {
            serde_json::from_slice(slice::from_raw_parts(output, size)).unwrap()
        };
        assert_eq!(value["success"], true);
        assert!(!owner.is_null());
        unsafe { octra_compute_input_drop(owner); }
        octra_circle_wasm_host_free_bytes(error, error_size);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn reply_bytes() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            for size in [0, 1, 2, 3, 4097, 65_536] {
                let mut runtime = Runtime::instantiate(&response_input(true, compute), false).unwrap();
                let mut call = finish_call(&mut runtime, "octra_query", &vec![255; size]).unwrap();
                runtime.store.data_mut().events.push(EventJson {
                    topic_b64: "a==".to_owned(), data_b64: "quote\"\n\u{0430}".to_owned(),
                });
                for failed in [false, true] {
                    call.code = if failed { -1 } else { 0 };
                    call.error = failed.then(|| "quote\"\n\u{0430}".to_owned());
                    let expected = serde_json::to_vec(&execute_output(&runtime, call.code,
                        !failed, &call.bytes, call.effort, call.error.clone())).unwrap();
                    let (bytes, count) = native_bytes::tests::large_allocs(4096,
                        || reply::execute(&runtime, &call).unwrap());
                    assert_eq!(bytes.as_slice(), expected);
                    assert_eq!(count, usize::from(bytes.len() >= 4096));
                }
                drop(call);
                drop(runtime);
                assert_eq!(compute_memory().used(), baseline);
            }
        }
    }

    #[test]
    fn reply_modes() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let payload = response_input(false, false);
        let input = serde_json::to_vec(&json!({
            "action": "execute", "export_name": "octra_query",
            "request_b64": encode_b64(&[7; 4097]),
            "code_b64": payload.code_b64.as_deref(), "hfhe_strict": false,
            "is_view": true, "execution_profile": "standard",
        })).unwrap();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
        let mut output = std::ptr::null_mut();
        let mut size = 0;
        let mut error = std::ptr::null_mut();
        let mut error_size = 0;
        let mut owner = std::ptr::null_mut();
        let status = octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
            &mut output, &mut size, &mut error, &mut error_size, &mut owner);
        assert_eq!(status, 0);
        assert!(owner.is_null());
        assert!(size > 4097);
        octra_circle_wasm_host_free_bytes(output, size);
        octra_circle_wasm_host_free_bytes(error, error_size);
        drop(held);
        assert_eq!(compute_memory().used(), baseline);
        let payload = response_input(true, true);
        let input = serde_json::to_vec(&json!({
            "action": "execute", "export_name": "octra_query", "request_b64": "",
            "code_b64": payload.code_b64.as_deref(), "hfhe_strict": false,
            "is_view": true, "execution_profile": "compute",
        })).unwrap();
        let status = octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
            &mut output, &mut size, &mut error, &mut error_size, std::ptr::null_mut());
        assert_eq!(status, 2);
        assert_eq!(size, 0);
        assert!(output.is_null());
        assert_eq!(unsafe { slice::from_raw_parts(error, error_size) }, b"reply owner missing");
        octra_circle_wasm_host_free_bytes(error, error_size);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn reply_describe() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let request = state_request("state_restore", &[&"a".repeat(64)]);
        let code = state_wasm(&request, false, Some(b"{\"name\":\"reply\"}"), false);
        for action in ["validate", "describe"] {
            let input = serde_json::to_vec(&json!({
                "action": action, "code_b64": encode_b64(&code), "inspect_policy": true,
                "is_view": true,
            })).unwrap();
            let mut output = std::ptr::null_mut();
            let mut size = 0;
            let mut error = std::ptr::null_mut();
            let mut error_size = 0;
            let mut owner = std::ptr::null_mut();
            assert_eq!(octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
                &mut output, &mut size, &mut error, &mut error_size, &mut owner), 0);
            assert!(!owner.is_null());
            assert!(compute_memory().used() >= baseline + 2 * size);
            let value: JsonValue = unsafe {
                serde_json::from_slice(slice::from_raw_parts(output, size)).unwrap()
            };
            assert!(value["exports"].is_array());
            if action == "describe" { assert_eq!(value["manifest"]["name"], "reply"); }
            unsafe { octra_compute_input_drop(owner); }
            octra_circle_wasm_host_free_bytes(error, error_size);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    fn pages_input(pages: u64, maximum: u64, local: bool, start: bool) -> Payload<'static> {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
        types.ty().function([], []);
        module.section(&types);
        let mut functions = FunctionSection::new();
        for ty in [0, 1, 0, 2] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: pages, maximum: Some(maximum), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        for (name, index) in [("octra_alloc", 0), ("octra_query", 1), ("grow", 2)] {
            exports.export(name, ExportKind::Func, index);
        }
        module.section(&exports);
        if start {
            module.section(&StartSection { function_index: 3 });
        }
        let mut code = CodeSection::new();
        for _ in 0..2 {
            let mut function = Function::new([]);
            function.instruction(&I::I32Const(0)).instruction(&I::End);
            code.function(&function);
        }
        let mut function = Function::new([]);
        function.instruction(&I::LocalGet(0)).instruction(&I::MemoryGrow(0)).instruction(&I::End);
        code.function(&function);
        let mut function = Function::new([]);
        function.instruction(&I::Unreachable).instruction(&I::End);
        code.function(&function);
        module.section(&code);
        Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()),
            "is_view": true, "inspect_policy": local, "hfhe_strict": false,
            "fuel_limit": 20_000_000,
        })).unwrap()
    }

    #[test]
    fn guest_limit() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let mut input = pages_input(1, 8, false, false);
        input.execution_profile = Some("compute".to_owned());
        for bytes in [0, 1, PAGE_BYTES - 1, PAGE_BYTES + 1,
            (COMPUTE_MAX_INITIAL_PAGES + 1) * PAGE_BYTES]
        {
            input.guest_bytes = Some(bytes);
            assert!(matches!(Runtime::prepare(&input, false, false, None),
                Err(HostFailure::Rejected(reason)) if reason == "compute guest bytes refused"));
            assert_eq!(compute_memory().used(), baseline);
        }
        input.guest_bytes = Some(2 * PAGE_BYTES);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let grow = runtime.instance.get_typed_func::<i32, i32>(&runtime.store, "grow").unwrap();
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        assert_eq!(grow.call(&mut runtime.store, 1).unwrap(), 1);
        assert!(grow.call(&mut runtime.store, 1).is_err(), "caller guest limit ignored");
        assert_eq!(memory.data_size(&runtime.store), 2 * PAGE_BYTES);
        assert_eq!(compute_memory().used(), baseline + 2 * PAGE_BYTES);
        drop(runtime);
        assert_eq!(compute_memory().used(), baseline);
        let mut large = pages_input(3, 8, false, false);
        large.execution_profile = Some("compute".to_owned());
        large.guest_bytes = Some(2 * PAGE_BYTES);
        assert!(Runtime::instantiate(&large, false).is_err(), "initial guest limit ignored");
        assert_eq!(compute_memory().used(), baseline);
        large.execution_profile = None;
        drop(Runtime::instantiate(&large, false).unwrap());
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn guest_pages_quota() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let mut input = pages_input(1, 8, !compute, false);
            input.execution_profile = compute.then(|| "compute".to_owned());
            drop(Runtime::instantiate(&input, false).unwrap());
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
            let (result, count) = native_bytes::tests::large_allocs(PAGE_BYTES,
                || Runtime::prepare(&input, false, false, None));
            assert!(result.is_err(), "guest pages bypassed the shared memory quota");
            assert!(matches!(result, Err(HostFailure::Unavailable(reason)) if reason == "compute memory busy"));
            assert_eq!(count, 0, "guest allocated before its page reservation");
            drop(held);
            let runtime = Runtime::instantiate(&input, false).unwrap();
            assert_eq!(compute_memory().used(), baseline + PAGE_BYTES);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn guest_pages_ffi() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let input = pages_input(1, 8, true, false);
            drop(Runtime::instantiate(&input, false).unwrap());
            let input = serde_json::to_vec(&json!({
                "action": "execute", "export_name": "octra_query", "request_b64": "",
                "code_b64": input.code_b64.as_deref(), "hfhe_strict": false,
                "is_view": true, "inspect_policy": !compute,
                "execution_profile": if compute { "compute" } else { "standard" },
            })).unwrap();
            for full in [true, false] {
                let held = full.then(|| compute_memory()
                    .take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap());
                let mut output = std::ptr::null_mut();
                let mut output_size = 0;
                let mut error = std::ptr::null_mut();
                let mut error_size = 0;
                let mut owner = std::ptr::null_mut();
                let status = octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
                    &mut output, &mut output_size, &mut error, &mut error_size, &mut owner);
                let message = if error_size == 0 { String::new() } else {
                    unsafe { String::from_utf8(slice::from_raw_parts(error, error_size).to_vec()).unwrap() }
                };
                let success = if output_size == 0 { false } else {
                    let value: JsonValue = unsafe {
                        serde_json::from_slice(slice::from_raw_parts(output, output_size)).unwrap()
                    };
                    value["success"] == true
                };
                assert_eq!(owner.is_null(), full);
                unsafe { octra_compute_input_drop(owner); }
                octra_circle_wasm_host_free_bytes(error, error_size);
                drop(held);
                assert_eq!(status, if full { 2 } else { 0 }, "guest resource class lost: {message}");
                assert_eq!(success, !full);
                if full { assert_eq!(message, "compute memory busy"); }
                assert_eq!(compute_memory().used(), baseline);
            }
        }
    }

    #[test]
    fn guest_pages_growth() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let mut runtime = Runtime::instantiate(&pages_input(1, 8, true, false), false).unwrap();
        let grow = runtime.instance.get_typed_func::<i32, i32>(&runtime.store, "grow").unwrap();
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        assert_eq!(grow.call(&mut runtime.store, 1).unwrap(), 1);
        assert_eq!(memory.data_size(&runtime.store), 2 * PAGE_BYTES);
        assert_eq!(compute_memory().used(), baseline + 2 * PAGE_BYTES);
        runtime.store.set_fuel(100).unwrap();
        assert!(grow.call(&mut runtime.store, 1).is_err());
        assert_eq!(memory.data_size(&runtime.store), 2 * PAGE_BYTES);
        assert_eq!(compute_memory().used(), baseline + 2 * PAGE_BYTES,
            "failed growth retained its reservation");
        runtime.store.set_fuel(20_000_000).unwrap();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - compute_memory().used()).unwrap();
        assert!(grow.call(&mut runtime.store, 1).is_err());
        assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
        assert_eq!(memory.data_size(&runtime.store), 2 * PAGE_BYTES);
        drop(held);
        assert!(grow.call(&mut runtime.store, 7).is_err());
        assert_eq!(compute_memory().used(), baseline + 2 * PAGE_BYTES);
        assert_eq!(grow.call(&mut runtime.store, 1).unwrap(), 2);
        drop(runtime);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn guest_pages_lifetime() {
        let _guard = cache_test();
        let input = pages_input(3, 8, true, false);
        drop(Runtime::instantiate(&input, false).unwrap());
        let baseline = compute_memory().used();
        let (_, released) = native_bytes::tests::paused_alloc(3 * PAGE_BYTES, 1, move || {
            let runtime = Runtime::instantiate(&input, false).unwrap();
            drop(runtime);
        }, |_| assert_eq!(compute_memory().used(), baseline + 3 * PAGE_BYTES));
        assert!(released);
        assert_eq!(compute_memory().used(), baseline);
        assert!(Runtime::instantiate(&pages_input(1, 8, true, true), false).is_err());
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn guest_pages_modes() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
        for (local, view, describe) in [(false, true, false), (true, false, false), (true, true, true)] {
            let mut input = pages_input(1, 8, local, false);
            input.is_view = Some(view);
            let runtime = Runtime::instantiate(&input, describe).unwrap();
            assert_eq!(compute_memory().used(), COMPUTE_STORAGE_RESIDENT_LIMIT);
            drop(runtime);
        }
        drop(held);
        assert_eq!(compute_memory().used(), baseline);
    }

    fn request_input(local: bool, compute: bool, encoded: String) -> Payload<'static> {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", "host_response_write", EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(0).function(1);
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1, maximum: Some(8), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 1);
        exports.export("octra_query", ExportKind::Func, 2);
        module.section(&exports);
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        alloc.instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&alloc);
        let mut query = Function::new([]);
        query.instruction(&I::LocalGet(0)).instruction(&I::LocalGet(1))
            .instruction(&I::Call(0)).instruction(&I::Drop)
            .instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&query);
        module.section(&code);
        Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()),
            "is_view": true, "inspect_policy": local, "hfhe_strict": false,
            "execution_profile": if compute { "compute" } else { "standard" },
            "export_name": "octra_query", "request_b64": encoded,
        })).unwrap()
    }

    #[test]
    fn request_input_quota() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for (local, compute) in [(true, false), (false, true)] {
            let input = request_input(local, compute, encode_b64(&vec![7; 4097]));
            drop(Runtime::instantiate(&input, false).unwrap());
            let size = base64::decoded_len_estimate(input.request_b64.as_deref().unwrap().len());
            for free in [0, size - 1] {
                let held = compute_memory()
                    .take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline - free).unwrap();
                let (result, count) = native_bytes::tests::large_allocs(4097,
                    || run_execute(&input));
                assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
                    if reason == "compute memory busy"), "request quota bypass: {result:?}");
                assert_eq!(count, 0, "request allocated before reservation");
                drop(held);
            }
            assert_eq!(run_execute(&input).unwrap()["success"], true);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn request_input_lifetime() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let input = request_input(true, false, encode_b64(&vec![7; 4097]));
        drop(Runtime::instantiate(&input, false).unwrap());
        let size = base64::decoded_len_estimate(input.request_b64.as_deref().unwrap().len());
        let (result, freed) = native_bytes::tests::paused_alloc(size, 1,
            move || run_execute(&input),
            |phase| assert_eq!(compute_memory().used(), baseline + size
                + if phase == native_bytes::tests::AllocPhase::Releasing { PAGE_BYTES } else { 0 }));
        assert_eq!(result.unwrap()["success"], true);
        assert!(freed, "request memory was not released");
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn request_input_errors() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let input = request_input(true, false, encode_b64(&vec![7; 4097]));
        let size = base64::decoded_len_estimate(input.request_b64.as_deref().unwrap().len());
        let result = native_bytes::tests::refused_alloc(size, || run_execute(&input));
        assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
            if reason == "request allocation failed"), "allocation failure lost: {result:?}");
        assert_eq!(compute_memory().used(), baseline);
        for encoded in ["!", "AAAA!", "a===", "AAAAA"] {
            let old = decode_b64("request_b64", encoded).unwrap_err();
            let result = run_execute(&request_input(true, false, encoded.to_owned()));
            assert!(matches!(result, Err(HostFailure::Rejected(ref reason)) if reason == &old),
                "request decode changed: {result:?}");
            assert_eq!(compute_memory().used(), baseline);
        }
        let mut input = input;
        input.export_name = Some("missing".to_owned());
        let result = run_execute(&input).unwrap();
        assert_eq!(result["success"], false);
        assert_eq!(result["error"], "wasm export not found: missing");
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn request_input_modes() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
        for (local, compute, view) in [(false, false, true), (true, false, false),
            (false, true, false), (true, true, false)]
        {
            let mut input = request_input(local, compute, encode_b64(&vec![7; 4097]));
            input.is_view = Some(view);
            let result = run_execute(&input).unwrap();
            assert_eq!(result["success"], true);
            assert_eq!(compute_memory().used(), COMPUTE_STORAGE_RESIDENT_LIMIT);
        }
        drop(held);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn request_input_ffi() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for compute in [false, true] {
            let input = request_input(!compute, compute, encode_b64(&vec![255; 4097]));
            drop(Runtime::instantiate(&input, false).unwrap());
            let input = serde_json::to_vec(&json!({
                "action": "execute", "export_name": "octra_query",
                "request_b64": input.request_b64.as_deref(),
                "code_b64": input.code_b64.as_deref(), "hfhe_strict": false,
                "is_view": true, "inspect_policy": !compute,
                "execution_profile": if compute { "compute" } else { "standard" },
            })).unwrap();
            for full in [true, false] {
                let held = full.then(|| compute_memory()
                    .take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap());
                let mut output = std::ptr::null_mut();
                let mut output_size = 0;
                let mut error = std::ptr::null_mut();
                let mut error_size = 0;
                let mut owner = std::ptr::null_mut();
                let status = octra_circle_wasm_host_run_json(input.as_ptr(), input.len(),
                    &mut output, &mut output_size, &mut error, &mut error_size, &mut owner);
                if full {
                    assert_eq!(status, 2);
                    assert_eq!(output_size, 0);
                    let message = unsafe { slice::from_raw_parts(error, error_size) };
                    assert_eq!(message, b"compute memory busy");
                } else {
                    assert_eq!(status, 0);
                    assert_eq!(error_size, 0);
                    let value: JsonValue = unsafe {
                        serde_json::from_slice(slice::from_raw_parts(output, output_size)).unwrap()
                    };
                    assert_eq!(value["success"], true);
                }
                assert_eq!(owner.is_null(), full);
                unsafe { octra_compute_input_drop(owner); }
                octra_circle_wasm_host_free_bytes(error, error_size);
                drop(held);
                assert_eq!(compute_memory().used(), baseline);
            }
        }
    }

    #[test]
    fn request_input_values() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for size in [0, 1, 2, 3, 31, 4097] {
            let encoded = encode_b64(&vec![255; size]);
            for compute in [false, true] {
                let input = request_input(!compute, compute, encoded.clone());
                let legacy = {
                    let bytes = decode_b64("request_b64", &encoded).unwrap();
                    let mut runtime = Runtime::prepare(&input, false, false, None).unwrap();
                    finish_execute(&mut runtime, "octra_query", &bytes).unwrap()
                };
                let result = run_execute(&input).unwrap();
                assert_eq!(result, legacy);
                assert_eq!(result["response_b64"],
                    if size == 0 { JsonValue::Null } else { json!(encoded) });
                assert_eq!(compute_memory().used(), baseline);
            }
        }
    }

    #[test]
    fn guest_pages_parallel() {
        let _guard = cache_test();
        let input = pages_input(1, 8, true, false);
        drop(Runtime::instantiate(&input, false).unwrap());
        let baseline = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline - 2 * PAGE_BYTES).unwrap();
        let ready = Arc::new(std::sync::Barrier::new(5));
        let release = Arc::new(std::sync::Barrier::new(5));
        let workers: Vec<_> = (0..4).map(|_| {
            let ready = ready.clone();
            let release = release.clone();
            std::thread::spawn(move || {
                let runtime = Runtime::instantiate(&pages_input(1, 8, true, false), false);
                ready.wait();
                release.wait();
                runtime.is_ok()
            })
        }).collect();
        ready.wait();
        let used = compute_memory().used();
        release.wait();
        let accepted = workers.into_iter().filter_map(|worker| worker.join().ok()).filter(|ok| *ok).count();
        assert_eq!(accepted, 2, "parallel guests bypassed the page quota");
        assert_eq!(used, COMPUTE_STORAGE_RESIDENT_LIMIT);
        drop(held);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn local_input_quota() {
        let _guard = INPUT_TEST.lock().unwrap_or_else(|error| error.into_inner());
        unsafe {
            let mut first = std::ptr::null_mut();
            let mut second = std::ptr::null_mut();
            assert_eq!(octra_circle_input_take(LOCAL_INPUT_LIMIT - 1, &mut first), 0);
            assert_eq!(octra_circle_input_take(2, &mut second), 2);
            assert!(second.is_null());
            assert_eq!(octra_circle_input_take(0, &mut second), 0);
            let mut third = std::ptr::null_mut();
            assert_eq!(octra_circle_input_take(0, &mut third), 2);
            octra_circle_input_drop(second);
            octra_circle_input_drop(first);
            assert_eq!(octra_circle_input_take(LOCAL_INPUT_LIMIT, &mut first), 0);
            octra_circle_input_drop(first);
            assert_eq!(octra_circle_input_take(usize::MAX, &mut second), 2);
            assert!(second.is_null());
            assert_eq!(octra_circle_input_take(1, std::ptr::null_mut()), 3);
            octra_circle_input_drop(std::ptr::null_mut());
        }
    }

    #[test]
    fn local_input_alloc() {
        let _guard = INPUT_TEST.lock().unwrap_or_else(|error| error.into_inner());
        unsafe {
            let mut owner = std::ptr::null_mut();
            let result = native_bytes::tests::refused_alloc(
                std::mem::size_of::<native_bytes::Lease>(),
                || octra_circle_input_take(1, &mut owner),
            );
            assert_eq!(result, 4);
            assert!(owner.is_null());
            assert_eq!(octra_circle_input_take(LOCAL_INPUT_LIMIT, &mut owner), 0);
            octra_circle_input_drop(owner);
        }
    }

    static COMPUTE_TEST: Mutex<()> = Mutex::new(());

    pub(super) fn cache_test() -> MutexGuard<'static, ()> {
        COMPUTE_TEST.lock().unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn compute_entry(value: &str) -> ComputeStorageEntry {
        let storage = BTreeMap::from([("weights".to_owned(), Arc::new(value.as_bytes().to_vec().into()))]);
        ComputeStorageEntry {
            root: compute_storage_root(&storage),
            weight: value.len(),
            storage: Arc::new(storage.into()),
            updated_at: 0.0,
        }
    }

    #[test]
    fn compute_input_owner() {
        let _cache = cache_test();
        let size = 4097;
        let baseline = compute_memory().used();
        let ((), freed) = native_bytes::tests::paused_alloc(size, 1, move || unsafe {
            let input = [7; 4097];
            let mut owner = std::ptr::null_mut();
            let mut bytes = std::ptr::null();
            assert_eq!(octra_compute_input_new(input.as_ptr(), size, &mut owner, &mut bytes), 0);
            assert_eq!(slice::from_raw_parts(bytes, size), input);
            octra_compute_input_drop(owner);
        }, |_| assert_eq!(compute_memory().used(), baseline + size));
        assert!(freed, "input allocation was not released");
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn compute_input_refused() {
        let _cache = cache_test();
        let input = vec![7; 4097];
        let pool = native_bytes::Pool::new(input.len() - 1);
        let (reply, count) = native_bytes::tests::large_allocs(input.len(),
            || copy_input(&pool, &input));
        assert_eq!(reply.unwrap_err(), "compute memory busy");
        assert_eq!(count, 0, "input allocated before reservation");
        assert_eq!(pool.used(), 0);
        let baseline = compute_memory().used();
        let run = |input: *const u8, length| unsafe {
            let mut owner = std::ptr::NonNull::<u8>::dangling().as_ptr().cast();
            let mut bytes = std::ptr::NonNull::<u8>::dangling().as_ptr().cast_const();
            let code = octra_compute_input_new(input, length, &mut owner, &mut bytes);
            assert!(owner.is_null());
            assert!(bytes.is_null());
            code
        };
        for (ptr, length) in [
            (std::ptr::null(), 1), (input.as_ptr(), 0),
            (input.as_ptr(), MAX_GUEST_READ_BYTES + 1),
        ] {
            assert_eq!(run(ptr, length), 1);
        }
        let lease = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
        let (code, count) = native_bytes::tests::large_allocs(1,
            || run(input.as_ptr(), input.len()));
        assert_eq!(code, 2);
        assert_eq!(count, 0);
        drop(lease);
        for size in [input.len(), std::mem::size_of::<native_bytes::Value>()] {
            assert_eq!(native_bytes::tests::refused_alloc(size,
                || run(input.as_ptr(), input.len())), 4);
            assert_eq!(compute_memory().used(), baseline);
        }
        unsafe {
            let mut owner = std::ptr::null_mut();
            let mut bytes = std::ptr::null();
            assert_eq!(octra_compute_input_new(input.as_ptr(), input.len(),
                std::ptr::null_mut(), &mut bytes), 1);
            assert_eq!(octra_compute_input_new(input.as_ptr(), input.len(),
                &mut owner, std::ptr::null_mut()), 1);
            assert_eq!(octra_compute_input_new(input.as_ptr(), input.len(), &mut owner, &mut bytes), 0);
            assert_eq!(slice::from_raw_parts(bytes, input.len()), input);
            octra_compute_input_drop(owner);
            octra_compute_input_drop(std::ptr::null_mut());
        }
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn compute_input_shapes() {
        let _cache = cache_test();
        let key = "4".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let mut inputs = vec![
            "{}".to_owned(), "[]".to_owned(), "null".to_owned(), "false".to_owned(),
            "0".to_owned(), "\"text\"".to_owned(), String::new(),
            r#"{"action":"unknown"} "#.to_owned(),
            r#"{"action":"unknown"} null"#.to_owned(),
        ];
        for value in [
            "null", "0", "true", "[]", "{}", "\"\"", "\"YWJj\"",
            "\"Y\\u0057Jj\"", "\"Y\\uD800\"", "\"Y\\uDC00\"",
            "\"Y\\uD800\\uDC00\"", "\"Y\\tWJj\"", "\"Y\\/WJj\"", "\"Y\\qWJj\"",
        ] {
            let prefix = r#"{"action":"unknown","compute_storage_value_b64":"#;
            inputs.push(format!("{prefix}{value}}}"));
            inputs.push(format!("{prefix}{value},\"address\":\"later\\u0061\"}}"));
            inputs.push(format!("{prefix}{value},\"compute_storage_value_b64\":null}}"));
            inputs.push(format!("{prefix}null,\"compute_storage_value_b64\":{value}}}"));
            inputs.push(format!("{prefix}{value},\"compute_storage_value_b\\u00364\":null}}"));
        }
        for length in 0..=35 {
            let mut fields = vec![JsonValue::Null; length];
            if length > 0 { fields[0] = json!("unknown"); }
            if length > 28 { fields[28] = json!("YWJj"); }
            inputs.push(serde_json::to_string(&fields).unwrap());
        }
        let count = inputs.len();
        let mut digest = Sha256::new();
        for input in &inputs {
            let parsed = serde_json::from_str::<Payload>(input)
                .map(|_| "ok".to_owned()).unwrap_or_else(|error| error.to_string());
            let json = run_json(input.as_ptr(), input.len()).unwrap_err().to_string();
            let (built, held, called) = unsafe {
                let mut output = std::ptr::null_mut();
                let mut size = 0;
                let code = octra_compute_build_step(
                    port, input.as_ptr(), input.len(), &mut output, &mut size,
                );
                let body = String::from_utf8(slice::from_raw_parts(output, size).to_vec()).unwrap();
                native_bytes::free_bytes(output, size);
                let lease = octra_compute_lease_new();
                let held = octra_compute_lease_hold(lease, input.as_ptr(), input.len());
                octra_compute_lease_drop(lease);
                let call = call_step::octra_circle_call_new();
                let code_call = call_step::octra_circle_call_step(
                    call, input.as_ptr(), input.len(), &mut output, &mut size,
                );
                let answer = String::from_utf8(slice::from_raw_parts(output, size).to_vec()).unwrap();
                native_bytes::free_bytes(output, size);
                call_step::octra_circle_call_drop(call);
                (format!("{code}:{body}"), held.to_string(), format!("{code_call}:{answer}"))
            };
            for answer in [parsed, json, built, held, called] {
                digest.update((answer.len() as u64).to_be_bytes());
                digest.update(answer.as_bytes());
            }
        }
        unsafe {
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(count, 115);
        assert_eq!(hex::encode(digest.finalize()),
            "4253fd9cf8af91e53509ccb2774025924932fee2631474c6409479a2a253b6b6");
    }

    #[test]
    fn compute_input_copy() {
        let _cache = cache_test();
        let key = "4".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let run = |input: &str| unsafe {
            let mut output = std::ptr::null_mut();
            let mut size = 0;
            let code = octra_compute_build_step(
                port, input.as_ptr(), input.len(), &mut output, &mut size,
            );
            let body = String::from_utf8(slice::from_raw_parts(output, size).to_vec()).unwrap();
            native_bytes::free_bytes(output, size);
            (code, body)
        };
        let value = vec![7; COMPUTE_STORAGE_VALUE_LIMIT];
        let encoded = BASE64.encode(&value);
        let input = format!(r#"{{"action":"compute_storage_put","compute_storage_key":"weights","compute_storage_value_b64":"{encoded}"}}"#);
        let (reply, mut copies) = native_bytes::tests::large_allocs(encoded.len(), || run(&input));
        assert_eq!(reply.0, 0);
        let reply: JsonValue = serde_json::from_str(&reply.1).unwrap();
        assert_eq!(reply["bytes"], value.len());
        assert_eq!(reply["entries"], 1);
        let escaped = format!("\\u{:04x}{}", encoded.as_bytes()[0], &encoded[1..]);
        assert_eq!(run(&input.replace(&encoded, &escaped)).0, 0);
        assert_eq!(run(&input.replace("compute_storage_value_b64", "compute_storage_value_b\\u00364")).0, 0);
        for text in [&input, &input.replace(&encoded, &escaped)] {
            let later = format!("{},\"address\":\"later\\u0061\"}}", &text[..text.len() - 1]);
            assert_eq!(run(&later).0, 0);
        }
        assert_eq!(run(r#"{"action":"compute_storage_put","compute_storage_key":"empty","compute_storage_value_b64":""}"#).0, 0);
        let refused = input.replace("compute_storage_put", "unknown");
        let (reply, count) = native_bytes::tests::large_allocs(encoded.len(), || run(&refused));
        assert_eq!(reply, (1, "compute storage action refused".to_owned()));
        copies += count;
        let refused = input.replace(&encoded, &format!("!{}", &encoded[1..]));
        let (reply, count) = native_bytes::tests::large_allocs(encoded.len(), || run(&refused));
        assert_eq!(reply.0, 1);
        assert!(reply.1.starts_with("invalid compute storage value base64:"));
        copies += count;
        for field in ["0", "true", "[]", "{}", "\"\\uD800\""] {
            let input = format!(r#"{{"action":"compute_storage_put","compute_storage_key":"weights","compute_storage_value_b64":{field}}}"#);
            assert_eq!(run(&input), (1, "compute storage input refused".to_owned()));
        }
        for input in [
            r#"{"action":"compute_storage_put","compute_storage_key":"weights"}"#,
            r#"{"action":"compute_storage_put","compute_storage_key":"weights","compute_storage_value_b64":null}"#,
        ] {
            assert_eq!(run(input), (1, "missing compute storage value".to_owned()));
        }
        let duplicate = format!("{},\"compute_storage_value_b64\":null}}", &input[..input.len() - 1]);
        assert_eq!(run(&duplicate), (1, "compute storage input refused".to_owned()));
        drop(input);
        {
            let table = compute_storage_builders().lock().unwrap();
            assert_eq!(table.data[&key].storage["weights"].as_slice(), value);
            assert!(table.data[&key].storage["empty"].is_empty());
            assert_eq!(table.data[&key].weight, value.len());
        }
        unsafe {
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(copies, 0, "model input copied encoded value");
    }

    fn build_parse(port: usize, input: &str) -> (i32, String) {
        unsafe {
            let mut output = std::ptr::null_mut();
            let mut size = 0;
            let code = octra_compute_build_step(port as *const Arc<ComputeBuild>,
                input.as_ptr(), input.len(), &mut output, &mut size);
            let body = String::from_utf8(slice::from_raw_parts(output, size).to_vec()).unwrap();
            native_bytes::free_bytes(output, size);
            (code, body)
        }
    }

    #[test]
    fn parse_quota() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let key = "6".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let input = format!(r#"{{"action":"compute_storage_put","compute_storage_key":"weights","compute_storage_value_b64":"YWJj","address":"{}"}}"#, "a".repeat(8193));
        let size = payload_memory(input.as_bytes()).unwrap().size();
        for free in [0, size - 1] {
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline - free).unwrap();
            let (reply, allocated) = native_bytes::tests::large_allocs(8193,
                || build_parse(port as usize, &input));
            assert_eq!(reply, (1, "compute memory busy".to_owned()));
            assert_eq!(allocated, 0, "parser allocated before reservation");
            assert!(compute_storage_builders().lock().unwrap().data[&key].storage.is_empty());
            assert_eq!(compute_memory().used(), baseline + held.size());
            drop(held);
        }
        assert_eq!(build_parse(port as usize, &input).0, 0);
        unsafe { octra_compute_build_close(port); octra_compute_build_release(port); }
        build_released(&token);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn parse_lifetime() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let key = "6".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        for escaped in [false, true] {
            let text = if escaped { "\\u0061".repeat(8193) } else { "a".repeat(8193) };
            let input = format!(r#"{{"action":"unknown","address":"{text}"}}"#);
            let size = payload_memory(input.as_bytes()).unwrap().size();
            let handle = port as usize;
            let (reply, freed) = native_bytes::tests::paused_alloc(8193, 1,
                move || build_parse(handle, &input),
                |_| assert_eq!(compute_memory().used(), baseline + size));
            assert_eq!(reply, (1, "compute storage action refused".to_owned()));
            assert!(freed);
            assert_eq!(compute_memory().used(), baseline);
        }
        unsafe { octra_compute_build_close(port); octra_compute_build_release(port); }
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn parse_errors() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let key = "6".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        for input in ["{", r#"{"address":"\uD800"}"#, r#"{"hfhe_caps":["a",false]}"#] {
            assert_eq!(build_parse(port as usize, input),
                (1, "compute storage input refused".to_owned()));
            assert_eq!(compute_memory().used(), baseline);
        }
        let input = r#"{"compute_storage_cache_key":"bad"}"#;
        let lease = octra_compute_lease_new();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
        assert_eq!(unsafe { octra_compute_lease_hold(lease, input.as_ptr(), input.len()) }, 6);
        assert!(unsafe { (*lease).id.is_none() });
        drop(held);
        assert_eq!(unsafe { octra_compute_lease_hold(lease, input.as_ptr(), input.len()) }, 5);
        unsafe {
            octra_compute_lease_drop(lease);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn parse_vectors() {
        for count in [0, 1, 2, 4, 5, 7, 8, 9, 16, 17, 127, 128, 129] {
            let fields = json!({
                "storage_pairs": vec![json!({"key_b64":"a", "value_b64":"b"}); count],
                "hfhe_caps": vec!["cap"; count],
                "hfhe_pubkeys": vec![json!({"addr":"a", "pubkey_b64":"b"}); count],
                "hfhe_receipt_entries": vec![json!({"method":"a", "request_hash":"b",
                    "response_hash":"c", "result":true}); count],
                "public_reads": vec![json!({"snapshot_root":"a", "circle_id":"b",
                    "canonical_path":"c", "offset":0, "max_bytes":1,
                    "metadata_json":"d", "body_b64":"e"}); count],
            });
            let input = fields.to_string();
            let memory = payload_memory(input.as_bytes()).unwrap();
            let parsed: Payload = serde_json::from_str(&input).unwrap();
            let bytes = parsed.storage_pairs.unwrap().capacity() * std::mem::size_of::<StoragePairJson>()
                + parsed.hfhe_caps.unwrap().capacity() * std::mem::size_of::<String>()
                + parsed.hfhe_pubkeys.unwrap().capacity() * std::mem::size_of::<HfhePubkeyJson>()
                + parsed.hfhe_receipt_entries.unwrap().capacity() * std::mem::size_of::<HfheReceiptEntryJson>()
                + parsed.public_reads.unwrap().capacity() * std::mem::size_of::<PublicReadJson>();
            assert!(memory.size() >= bytes + input.len() * 4);
        }
    }

    #[test]
    fn parse_diagnostics() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let key = "6".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let input = format!("{{\"is_view\":\"{}\"}}", "\u{7f}".repeat(3000));
        let error = serde_json::from_str::<Payload>(&input).unwrap_err().to_string();
        assert!(error.len() > 6 * 3000);
        let memory = payload_memory(input.as_bytes()).unwrap();
        assert!(memory.size() >= 3 * error.len() + 4 * input.len());
        let reserved = memory.size();
        drop(memory);
        let (reply, count) = native_bytes::tests::large_allocs(18_000,
            || build_parse(port as usize, &input));
        assert_eq!(reply, (1, "compute storage input refused".to_owned()));
        assert!(count > 0, "diagnostic allocation was not observed");
        let lease = octra_compute_lease_new();
        assert_eq!(unsafe { octra_compute_lease_hold(lease, input.as_ptr(), input.len()) }, 5);
        assert!(unsafe { (*lease).id.is_none() });
        for hold in [false, true] {
            let peak = native_bytes::tests::allocation_peak(|| {
                if hold {
                    assert_eq!(unsafe { octra_compute_lease_hold(lease, input.as_ptr(), input.len()) }, 5);
                } else {
                    assert_eq!(build_parse(port as usize, &input),
                        (1, "compute storage input refused".to_owned()));
                }
            });
            assert!(peak > 4 * input.len(), "diagnostic expansion was not measured");
            assert!(peak <= reserved, "diagnostic allocation exceeded reservation");
            println!("event = parse_memory input = {} peak = {peak} reserved = {reserved} hold = {hold}", input.len());
        }
        unsafe {
            octra_compute_lease_drop(lease);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn code_input_shapes() {
        let mut inputs = vec![
            "{}".to_owned(), "[]".to_owned(), "null".to_owned(), "false".to_owned(),
            "0".to_owned(), "\"text\"".to_owned(), String::new(),
            r#"{"action":"unknown"} "#.to_owned(),
            r#"{"action":"unknown"} null"#.to_owned(),
        ];
        for (field, escaped) in [
            ("code_b64", "co\\u0064e_b64"),
            ("request_b64", "request_b\\u00364"),
        ] {
            for value in [
                "null", "0", "true", "[]", "{}", "\"\"", "\"YWJj\"",
                "\"Y\\u0057Jj\"", "\"Y\\uD800\"", "\"Y\\uDC00\"",
                "\"Y\\uD800\\uDC00\"", "\"Y\\tWJj\"", "\"Y\\/WJj\"", "\"Y\\qWJj\"",
            ] {
                let prefix = format!("{{\"action\":\"unknown\",\"{field}\":");
                inputs.push(format!("{prefix}{value}}}"));
                inputs.push(format!("{prefix}{value},\"address\":\"later\\u0061\"}}"));
                inputs.push(format!("{prefix}{value},\"{field}\":null}}"));
                inputs.push(format!("{prefix}null,\"{field}\":{value}}}"));
                inputs.push(format!("{prefix}{value},\"{escaped}\":null}}"));
            }
        }
        for length in 0..=37 {
            let mut fields = vec![JsonValue::Null; length];
            if length > 0 { fields[0] = json!("unknown"); }
            if length > 1 { fields[1] = json!("YWJj"); }
            if length > 4 { fields[4] = json!("ZGVm"); }
            inputs.push(serde_json::to_string(&fields).unwrap());
        }
        let mut digest = Sha256::new();
        for input in &inputs {
            let parsed = serde_json::from_str::<Payload>(input)
                .map(|_| "ok".to_owned()).unwrap_or_else(|error| error.to_string());
            let ordinary = run_json(input.as_ptr(), input.len()).unwrap_err().to_string();
            let session = unsafe {
                let port = call_step::octra_circle_call_new();
                let mut output = std::ptr::null_mut();
                let mut size = 0;
                let code = call_step::octra_circle_call_step(
                    port, input.as_ptr(), input.len(), &mut output, &mut size,
                );
                let body = String::from_utf8(slice::from_raw_parts(output, size).to_vec()).unwrap();
                native_bytes::free_bytes(output, size);
                call_step::octra_circle_call_drop(port);
                format!("{code}:{body}")
            };
            for answer in [parsed, ordinary, session] {
                digest.update((answer.len() as u64).to_be_bytes());
                digest.update(answer.as_bytes());
            }
        }
        assert_eq!(inputs.len(), 187);
        assert_eq!(hex::encode(digest.finalize()),
            "0ea6808bfccdc95f1a342852e33fe86a3ffb888481488dc4a13243bd36eaed38");
        for length in [34, 35] {
            let fields = vec![JsonValue::Null; length];
            let raw = serde_json::to_string(&fields).unwrap();
            let decoded: Payload = serde_json::from_str(&raw).unwrap();
            assert!(decoded.memory_bytes.is_none());
        }
        let raw = serde_json::to_string(&vec![JsonValue::Null; 36]).unwrap();
        assert!(serde_json::from_str::<Payload>(&raw).is_err());
    }

    #[test]
    fn code_input_copy() {
        let _guard = cache_test();
        let encoded = BASE64.encode(vec![7; 4 * 1024 * 1024]);
        let mut copies = 0;
        for (field, escaped) in [
            ("code_b64", "co\\u0064e_b64"),
            ("request_b64", "request_b\\u00364"),
        ] {
            for name in [field, escaped] {
                let input = format!("{{\"action\":\"unknown\",\"{name}\":\"{encoded}\"}}");
                let (reply, count) = native_bytes::tests::large_allocs(encoded.len(),
                    || run_json(input.as_ptr(), input.len()));
                assert_eq!(reply.unwrap_err().to_string(), "octra_circle_wasm_host: unsupported action: unknown");
                copies += count;
            }
            let input = format!("{{\"{field}\":\"{encoded}\",\"address\":\"later\\u0061\"}}");
            let parsed: Payload = serde_json::from_str(&input).unwrap();
            let value = if field == "code_b64" { parsed.code_b64.as_deref() }
                else { parsed.request_b64.as_deref() };
            assert_eq!(value, Some(encoded.as_str()));
            let escaped = format!("\\u{:04x}{}", encoded.as_bytes()[0], &encoded[1..]);
            let input = input.replace(&encoded, &escaped);
            let parsed: Payload = serde_json::from_str(&input).unwrap();
            let value = if field == "code_b64" { parsed.code_b64.as_deref() }
                else { parsed.request_b64.as_deref() };
            assert_eq!(value, Some(encoded.as_str()));
            assert_eq!(parsed.address.as_deref(), Some("latera"));
        }
        let mut fields = session_module(InspectStage::Unused);
        fields["action"] = json!("execute");
        fields["export_name"] = json!("octra_query");
        fields["request_b64"] = json!("YWJj");
        let input = fields.to_string();
        let expected = run_json(input.as_ptr(), input.len()).unwrap();
        for field in ["code_b64", "request_b64"] {
            let value = fields[field].as_str().unwrap();
            let escaped = format!("\\u{:04x}{}", value.as_bytes()[0], &value[1..]);
            let input = input.replace(value, &escaped);
            assert_eq!(run_json(input.as_ptr(), input.len()).unwrap(), expected);
        }
        println!("event = code_input copies = {copies}");
        assert_eq!(copies, 0, "native parser copied encoded input");
    }

    #[test]
    fn storage_value_quota() {
        let _guard = cache_test();
        let value = vec![7; 4096];
        for profile in ["standard", "manifest", "compute"] {
            let input: Payload = Payload::deserialize(json!({
                "execution_profile": profile, "is_view": true, "hfhe_strict": false,
                "inspect_policy": true,
                "storage_pairs": [{"key_b64": "aw==", "value_b64": BASE64.encode(&value)}],
            })).unwrap();
            let resident = compute_memory().used();
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
            let (result, copies) = native_bytes::tests::large_allocs(value.len(),
                || HostState::from_payload(&input, false));
            assert!(result.is_err(), "inline storage bypassed the shared memory quota");
            assert_eq!(copies, 0, "storage value allocated before reservation");
            assert_eq!(compute_memory().used(), resident + held.size());
            drop(held);
            let state = HostState::from_payload(&input, false).unwrap();
            assert_eq!(state.storage["k"].as_slice(), value);
            assert!(compute_memory().used() >= resident + value.len());
            drop(state);
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn storage_key_quota() {
        let _guard = cache_test();
        let key = "k".repeat(4096);
        let input: Payload = Payload::deserialize(json!({
            "execution_profile": "standard", "is_view": true, "hfhe_strict": false,
            "inspect_policy": true,
            "storage_pairs": [{"key_b64": BASE64.encode(&key), "value_b64": ""}],
        })).unwrap();
        let resident = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(key.len(),
            || HostState::from_payload(&input, false));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(copies, 0);
        assert!(matches!(HostState::from_payload(&input, true),
            Err(HostFailure::Unavailable(_))));
        drop(held);
        let state = HostState::from_payload(&input, false).unwrap();
        assert!(state.storage.contains_key(&key));
        assert!(compute_memory().used() >= resident + key.len());
        drop(state);
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_cache_modes() {
        let _guard = cache_test();
        for profile in ["standard", "manifest"] {
            for local in [false, true] {
                let key = format!("storage-modes-{profile}-{local}");
                let mut input: Payload = Payload::deserialize(json!({
                    "execution_profile": profile, "is_view": true, "hfhe_strict": false,
                    "inspect_policy": local, "storage_cache_key": key,
                    "storage_pairs": [{"key_b64": "aw==", "value_b64": "eA=="}],
                })).unwrap();
                let resident = compute_memory().used();
                let state = HostState::from_payload(&input, false).unwrap();
                let charged = compute_memory().used();
                assert_eq!(charged > resident, local);
                input.storage_pairs = None;
                let cached = HostState::from_payload(&input, false).unwrap();
                assert!(Arc::ptr_eq(&state.storage, &cached.storage));
                assert_eq!(compute_memory().used(), charged);
                input.inspect_policy = Some(!local);
                let error = HostState::from_payload(&input, false).err().unwrap();
                assert!(matches!(error, HostFailure::Rejected(_)));
                assert_eq!(error.to_string(), format!("missing storage cache: {key}"));
                cache_guard(storage_cache(local)).remove(&key);
                drop(state);
                assert_eq!(compute_memory().used(), charged);
                drop(cached);
                assert_eq!(compute_memory().used(), resident);
            }
        }
    }

    #[test]
    fn storage_decode_errors() {
        let _guard = cache_test();
        let mut input: Payload = Payload::deserialize(json!({
            "execution_profile": "standard", "is_view": true, "hfhe_strict": false,
            "inspect_policy": false, "storage_cache_key": "storage-errors",
            "storage_pairs": [{"key_b64": "aw==", "value_b64": "eA=="}],
        })).unwrap();
        let resident = compute_memory().used();
        input.inspect_policy = Some(true);
        let state = HostState::from_payload(&input, false).unwrap();
        let charged = compute_memory().used();
        for (key, value) in [("!", "eA=="), ("/w==", "eA=="), ("aw==", "!")] {
            input.storage_pairs.as_mut().unwrap()[0] = StoragePairJson {
                key_b64: key.to_owned(), value_b64: value.to_owned(),
            };
            input.inspect_policy = Some(false);
            let prior = HostState::from_payload(&input, false).err().unwrap();
            input.inspect_policy = Some(true);
            let local = HostState::from_payload(&input, false).err().unwrap();
            assert!(matches!(local, HostFailure::Rejected(_)));
            assert_eq!(local.to_string(), prior.to_string());
            assert_eq!(compute_memory().used(), charged);
            input.storage_pairs = None;
            let cached = HostState::from_payload(&input, false).unwrap();
            assert!(Arc::ptr_eq(&cached.storage, &state.storage));
            input.storage_pairs = Some(vec![StoragePairJson {
                key_b64: "aw==".to_owned(), value_b64: "eA==".to_owned(),
            }]);
        }
        cache_guard(storage_cache(true)).remove("storage-errors");
        drop(state);
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_copy_quota() {
        let _guard = cache_test();
        let input: Payload = Payload::deserialize(json!({
            "execution_profile": "standard", "is_view": true, "hfhe_strict": false,
            "inspect_policy": true,
            "storage_pairs": [{"key_b64": BASE64.encode("k".repeat(4096)), "value_b64": "eA=="}],
        })).unwrap();
        let resident = compute_memory().used();
        let mut state = HostState::from_payload(&input, false).unwrap();
        let saved = state.storage.clone();
        let root = compute_storage_root(&saved);
        let charged = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - charged).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(4096,
            || StorageEdit(&mut state.storage, true, None).insert("new".to_owned(), b"data".to_vec()));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(copies, 0);
        assert!(Arc::ptr_eq(&saved, &state.storage));
        drop(held);
        let value = saved.values().next().unwrap().clone();
        let mut edit = StorageEdit(&mut state.storage, true, None);
        edit.insert("new".to_owned(), b"data".to_vec()).unwrap();
        assert_eq!(compute_storage_root(&saved), root);
        assert!(!saved.contains_key("new"));
        assert!(state.storage.contains_key("new"));
        assert!(Arc::ptr_eq(&value, state.storage.values().next().unwrap()));
        assert!(compute_memory().used() >= charged + 4096);
        drop(state);
        drop(saved);
        assert!(compute_memory().used() > resident);
        drop(value);
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_job_owners() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let first = native_bytes::Budget::new(1024);
        let second = native_bytes::Budget::new(1024);
        let mut storage = Arc::new(Storage::default());
        for owner in [Some(&first), Some(&second), None] {
            StorageEdit(&mut storage, true, owner).put("a", b"data").unwrap();
            StorageEdit(&mut storage, true, owner).put("b", b"more").unwrap();
            StorageEdit(&mut storage, true, owner).remove("b").unwrap();
            assert_eq!(storage["a"].as_slice(), b"data");
        }
        drop(storage);
        assert_eq!(first.used(), 0);
        assert_eq!(second.used(), 0);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn storage_write_quota() {
        let _guard = cache_test();
        let id = "a".repeat(64);
        let key = state_policy_key(&id, "tombstone");
        let request = state_request("state_restore", &[&id]);
        for pairs in [None, Some(Vec::new())] {
            for probe in [false, true] {
                let mut input = state_module(&request, probe);
                input.storage_pairs = pairs.clone();
                let resident = compute_memory().used();
                let mut runtime = Runtime::instantiate(&input, false).unwrap();
                let root = compute_storage_root(&runtime.store.data().storage);
                let used = compute_memory().used();
                let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
                let result = runtime.call_export("octra_query", &[]);
                assert!(result.is_err(), "local storage write bypassed the memory quota");
                assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
                assert_eq!(compute_storage_root(&runtime.store.data().storage), root);
                assert!(!runtime.store.data().storage.contains_key(&key));
                drop(held);
                drop(runtime);
                assert_eq!(compute_memory().used(), resident);
                let mut runtime = Runtime::instantiate(&input, false).unwrap();
                runtime.call_export("octra_query", &[]).unwrap();
                assert_eq!(runtime.store.data().storage[&key].as_slice(), b"false");
                drop(runtime);
                assert_eq!(compute_memory().used(), resident);
            }
        }
    }

    #[test]
    fn storage_write_modes() {
        let _guard = cache_test();
        let id = "a".repeat(64);
        let key = state_policy_key(&id, "tombstone");
        for profile in ["standard", "manifest", "compute"] {
            for view in [false, true] {
                for inspect in [false, true] {
                    for probe in [false, true] {
                        let mut input = state_module(&state_request("state_restore", &[&id]), probe);
                        input.execution_profile = Some(profile.to_owned());
                        input.is_view = Some(view);
                        input.inspect_policy = Some(inspect);
                        let resident = compute_memory().used();
                        let mut runtime = Runtime::instantiate(&input, false).unwrap();
                        let used = compute_memory().used();
                        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
                        let result = runtime.call_export("octra_query", &[]);
                        let local = view && (inspect || profile == "compute");
                        assert_eq!(result.is_err(), local);
                        if local {
                            assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
                            assert!(!runtime.store.data().storage.contains_key(&key));
                        } else {
                            assert_eq!(runtime.store.data().unavailable, None);
                            assert_eq!(runtime.store.data().storage[&key].as_slice(), b"false");
                        }
                        drop(runtime);
                        drop(held);
                        assert_eq!(compute_memory().used(), resident);
                    }
                }
            }
        }
    }

    #[test]
    fn storage_write_values() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        let mut image = Arc::new(Storage::default());
        StorageEdit(&mut image, true, None).put("k", b"abc").unwrap();
        assert_eq!(compute_memory().used(), resident + 4);
        let saved = image.clone();
        let value = saved["k"].clone();
        StorageEdit(&mut image, true, None).put("k", b"12345").unwrap();
        assert_eq!(compute_memory().used(), resident + 10);
        assert_eq!(saved["k"].as_slice(), b"abc");
        assert_eq!(image["k"].as_slice(), b"12345");
        StorageEdit(&mut image, true, None).remove("k").unwrap();
        assert_eq!(compute_memory().used(), resident + 4);
        drop(image);
        drop(saved);
        assert_eq!(compute_memory().used(), resident + 3);
        assert_eq!(value.as_slice(), b"abc");
        drop(value);
        assert_eq!(compute_memory().used(), resident);
        let mut image = Arc::new(Storage::default());
        for _ in 0..32 {
            StorageEdit(&mut image, true, None).put("empty", b"").unwrap();
            assert_eq!(compute_memory().used(), resident + 5);
            StorageEdit(&mut image, true, None).put("empty", b"ab").unwrap();
            assert_eq!(compute_memory().used(), resident + 7);
            StorageEdit(&mut image, true, None).put("empty", b"").unwrap();
            assert_eq!(compute_memory().used(), resident + 5);
            StorageEdit(&mut image, true, None).remove("empty").unwrap();
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn storage_write_alloc() {
        let _guard = cache_test();
        let key = "k".repeat(1025);
        let value = vec![7; 2051];
        let resident = compute_memory().used();
        let mut image = Arc::new(Storage::default());
        for size in [key.len(), value.len()] {
            let result = native_bytes::tests::refused_alloc(size,
                || StorageEdit(&mut image, true, None).put(&key, &value));
            assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
                if reason == "storage allocation failed"));
            assert!(image.is_empty());
            assert_eq!(compute_memory().used(), resident);
        }
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(key.len(),
            || StorageEdit(&mut image, true, None).put(&key, &value));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(copies, 0);
        drop(held);
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident - key.len()).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(value.len(),
            || StorageEdit(&mut image, true, None).put(&key, &value));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(copies, 0);
        assert!(image.is_empty());
        assert_eq!(compute_memory().used(), resident + held.size());
        drop(held);
        StorageEdit(&mut image, true, None).put(&key, &value).unwrap();
        let saved = image.clone();
        let used = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(key.len(),
            || StorageEdit(&mut image, true, None).put(&key, &value));
        assert!(matches!(result, Err(HostFailure::Unavailable(_))));
        assert_eq!(copies, 0);
        assert!(Arc::ptr_eq(&saved, &image));
        drop(held);
        drop(saved);
        drop(image);
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_copy_failure() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        let source = Arc::new(Storage::from([
            ("a".repeat(1041), Arc::new(b"one".to_vec().into())),
            ("b".repeat(2081), Arc::new(b"two".to_vec().into())),
        ]));
        let root = compute_storage_root(&source);
        let saved = source.clone();
        let (image, freed) = native_bytes::tests::paused_alloc(1041, 1, move || {
            let mut image = source.clone();
            let result = native_bytes::tests::refused_alloc(2081,
                || StorageEdit(&mut image, true, None).put("new", b"data"));
            assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
                if reason == "storage allocation failed"));
            assert!(Arc::ptr_eq(&image, &source));
            image
        }, |_| assert!(compute_memory().used() >= resident + 1041 + 2081));
        assert!(freed);
        assert!(Arc::ptr_eq(&image, &saved));
        assert_eq!(compute_storage_root(&image), root);
        assert!(!image.contains_key("new"));
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_write_free() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        let key = "k".repeat(4097);
        let value = vec![7; 8193];
        for size in [key.len(), value.len()] {
            let key = key.clone();
            let value = value.clone();
            let (_, freed) = native_bytes::tests::paused_alloc(size, 1, move || {
                let mut image = Arc::new(Storage::default());
                StorageEdit(&mut image, true, None).put(&key, &value).unwrap();
                StorageEdit(&mut image, true, None).remove(&key).unwrap();
            }, |_| assert!(compute_memory().used() >= resident + size));
            assert!(freed);
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn storage_write_compute() {
        let _guard = cache_test();
        let key = "3".repeat(64);
        let resident = compute_memory().used();
        let build = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(build, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*build).token().unwrap() };
        let put: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights", "compute_storage_value_b64": BASE64.encode(b"model"),
        })).unwrap();
        put_storage(&token, &put, compute_memory()).unwrap();
        let model = BTreeMap::from([("weights".to_owned(), Arc::new(b"model".to_vec().into()))]);
        let seal: Payload = Payload::deserialize(json!({
            "compute_storage_entries": 1, "compute_storage_bytes": 5,
            "compute_storage_root": compute_storage_root(&model),
        })).unwrap();
        run_compute_storage_seal(&token, &seal).unwrap();
        let entry = compute_storage_cache().lock().unwrap().entries[&key].clone();
        assert_eq!(entry.storage.keys.as_ref().unwrap().size(), "weights".len());
        let mut input = state_module(&state_request("state_restore", &[&"a".repeat(64)]), false);
        input.compute_storage_cache_key = Some(key.clone());
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let used = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
        assert!(runtime.call_export("octra_query", &[]).is_err());
        assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
        assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
        drop(held);
        drop(runtime);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let used = compute_memory().used();
        let output_key = state_policy_key(&"a".repeat(64), "tombstone");
        let held = compute_memory().take(
            COMPUTE_STORAGE_RESIDENT_LIMIT - used - output_key.len() - b"false".len()).unwrap();
        assert!(runtime.call_export("octra_query", &[]).is_err());
        assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
        assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
        assert_eq!(compute_memory().used(), used + held.size());
        drop(held);
        drop(runtime);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        runtime.call_export("octra_query", &[]).unwrap();
        let output = &runtime.store.data().storage;
        assert!(output.keys.is_some());
        assert!(Arc::ptr_eq(&output["weights"], &entry.storage["weights"]));
        assert!(compute_memory().used() >= used + "weights".len());
        assert_eq!(compute_storage_root(&entry.storage), entry.root);
        drop(runtime);
        input.is_view = Some(false);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let used = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
        runtime.call_export("octra_query", &[]).unwrap();
        assert_eq!(runtime.store.data().storage[&output_key].as_slice(), b"false");
        assert_eq!(compute_storage_root(&entry.storage), entry.root);
        assert_eq!(compute_memory().used(), used + held.size());
        drop(held);
        drop(runtime);
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
        drop(entry);
        unsafe {
            octra_compute_build_close(build);
            octra_compute_build_release(build);
        }
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_alloc_failure() {
        let _guard = cache_test();
        let input: Payload = Payload::deserialize(json!({
            "execution_profile": "standard", "is_view": true, "hfhe_strict": false,
            "inspect_policy": true,
            "storage_pairs": [{
                "key_b64": BASE64.encode("k".repeat(4096)),
                "value_b64": BASE64.encode(vec![7; 8192]),
            }],
        })).unwrap();
        let resident = compute_memory().used();
        let pair = &input.storage_pairs.as_ref().unwrap()[0];
        for encoded in [&pair.key_b64, &pair.value_b64] {
            let size = base64::decoded_len_estimate(encoded.len());
            let result = native_bytes::tests::refused_alloc(size,
                || HostState::from_payload(&input, false));
            assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
                if reason == "storage allocation failed"));
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn storage_lifetime() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        for value in [false, true] {
            let input: Payload = Payload::deserialize(json!({
                "execution_profile": "standard", "is_view": true, "hfhe_strict": false,
                "inspect_policy": true,
                "storage_pairs": [{
                    "key_b64": BASE64.encode("k".repeat(4096)),
                    "value_b64": BASE64.encode(vec![7; 8192]),
                }],
            })).unwrap();
            let pair = &input.storage_pairs.as_ref().unwrap()[0];
            let size = base64::decoded_len_estimate(
                if value { pair.value_b64.len() } else { pair.key_b64.len() });
            let (_, freed) = native_bytes::tests::paused_alloc(size, 1, move || {
                let state = HostState::from_payload(&input, false).unwrap();
                let reader = value.then(|| state.storage.values().next().unwrap().clone());
                drop(state);
                drop(reader);
            }, |_| {
                assert!(compute_memory().used() >= resident + size,
                    "storage reservation released before allocation");
            });
            assert!(freed);
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn storage_runtime_modes() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        for profile in ["standard", "manifest"] {
            let mut input = read_module(profile, true, false, false);
            input.storage_pairs = Some(vec![StoragePairJson {
                key_b64: "aw==".to_owned(), value_b64: "eA==".to_owned(),
            }]);
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
            input.inspect_policy = Some(true);
            let error = Runtime::prepare(&input, false, false, None).err().unwrap();
            assert!(matches!(error, HostFailure::Unavailable(_)));
            input.inspect_policy = Some(false);
            let runtime = Runtime::instantiate(&input, false).unwrap();
            assert_eq!(runtime.store.data().storage["k"].as_slice(), b"x");
            assert_eq!(compute_memory().used(), resident + held.size());
            drop(runtime);
            drop(held);
            input.inspect_policy = Some(true);
            let runtime = Runtime::instantiate(&input, false).unwrap();
            assert_eq!(runtime.store.data().storage["k"].as_slice(), b"x");
            drop(runtime);
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn storage_ffi_modes() {
        let _guard = cache_test();
        let code = read_module("standard", true, false, false).code_b64.unwrap();
        let resident = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
        for action in ["validate", "describe", "execute"] {
            for local in [false, true] {
                let input = json!({
                    "action": action, "code_b64": &*code,
                    "export_name": "octra_query", "request_b64": "",
                    "execution_profile": "standard", "is_view": true,
                    "inspect_policy": local, "hfhe_strict": false,
                    "storage_pairs": [{"key_b64": "aw==", "value_b64": "eA=="}],
                }).to_string();
                let result = run_json(input.as_ptr(), input.len());
                if local {
                    assert!(matches!(result, Err(HostFailure::Unavailable(_))));
                } else {
                    assert!(result.is_ok(), "{action}: {result:?}");
                }
                assert_eq!(compute_memory().used(), resident + held.size());
            }
        }
    }

    #[test]
    fn storage_absent_delete() {
        let _guard = cache_test();
        let request = state_request("object_member_detach", &[&"a".repeat(64), "missing"]);
        let mut input = state_module(&request, false);
        input.storage_cache_key = Some("storage-absent".to_owned());
        input.storage_pairs = Some(vec![StoragePairJson {
            key_b64: BASE64.encode("k".repeat(4096)), value_b64: "eA==".to_owned(),
        }]);
        let resident = compute_memory().used();
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let saved = runtime.store.data().storage.clone();
        let used = compute_memory().used();
        let mut response = native_bytes::Value::default();
        response.append(Some(compute_memory()), &frame_bool(true), MAX_RESPONSE_BYTES).unwrap();
        let reply_bytes = compute_memory().used() - used;
        drop(response);
        let held = compute_memory()
            .take(COMPUTE_STORAGE_RESIDENT_LIMIT - compute_memory().used() - reply_bytes).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(4096,
            || finish_execute(&mut runtime, "octra_query", &[]).unwrap());
        assert_eq!(result["success"], true, "{result}");
        assert_eq!(copies, 0);
        assert!(Arc::ptr_eq(&saved, &runtime.store.data().storage));
        drop(held);
        drop(saved);
        drop(runtime);
        cache_guard(storage_cache(true)).remove("storage-absent");
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_expiry_compute() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        let input: Payload = Payload::deserialize(json!({
            "execution_profile": "compute", "is_view": true, "hfhe_strict": false,
            "storage_cache_key": "storage-expiry",
            "storage_pairs": [{"key_b64": "aw==", "value_b64": BASE64.encode(vec![7; PAGE_BYTES])}],
        })).unwrap();
        drop(HostState::from_payload(&input, false).unwrap());
        cache_guard(storage_cache(true)).get_mut("storage-expiry").unwrap().updated_at =
            now_secs() - STORAGE_CACHE_TTL_SECS - 1.0;
        let loaded = compute_memory().used();
        assert!(loaded >= resident + PAGE_BYTES);
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - loaded).unwrap();
        let key = "e".repeat(64);
        compute_cache_call(CacheMessage::Seal(key.clone(), compute_entry("x"))).unwrap();
        let mut input = pages_input(1, 1, true, false);
        input.execution_profile = Some("compute".to_owned());
        input.compute_storage_cache_key = Some(key.clone());
        let runtime = Runtime::prepare(&input, false, false, None).unwrap();
        assert!(!cache_guard(storage_cache(true)).contains_key("storage-expiry"));
        drop(runtime);
        drop(held);
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn storage_input_copy() {
        let _guard = cache_test();
        let value = vec![7; 1024 * 1024];
        let encoded = BASE64.encode(&value);
        let mut copies = 0;
        for profile in ["standard", "manifest", "compute"] {
            for cached in [false, true] {
                let key = format!("input-copy-{profile}-{cached}");
                let mut input: Payload = Payload::deserialize(json!({
                    "execution_profile": profile, "is_view": true, "hfhe_strict": false,
                    "storage_cache_key": cached.then_some(key.clone()),
                    "storage_pairs": [
                        {"key_b64": BASE64.encode(b"weights"), "value_b64": encoded},
                        {"key_b64": BASE64.encode(b"same"), "value_b64": BASE64.encode(b"first")},
                        {"key_b64": BASE64.encode(b"same"), "value_b64": BASE64.encode(b"last")},
                    ],
                })).unwrap();
                let (state, count) = native_bytes::tests::large_allocs(encoded.len(),
                    || HostState::from_payload(&input, false));
                let state = state.unwrap();
                copies += count;
                assert_eq!(state.storage.len(), 2);
                assert_eq!(state.storage["weights"].as_slice(), value);
                assert_eq!(state.storage["same"].as_slice(), b"last");
                let root = compute_storage_root(&state.storage);
                input.storage_pairs.as_mut().unwrap()[0].key_b64 = "!".to_owned();
                let (error, count) = native_bytes::tests::large_allocs(encoded.len(),
                    || HostState::from_payload(&input, false).err().unwrap());
                assert!(error.to_string().starts_with("invalid storage key base64:"));
                copies += count;
                assert_eq!(compute_storage_root(&state.storage), root);
                if cached {
                    input.storage_pairs = None;
                    let read = HostState::from_payload(&input, false).unwrap();
                    assert!(Arc::ptr_eq(&state.storage, &read.storage));
                    input.storage_pairs = Some(Vec::new());
                    let empty = HostState::from_payload(&input, false).unwrap();
                    assert!(empty.storage.is_empty());
                    assert!(!Arc::ptr_eq(&state.storage, &empty.storage));
                    cache_guard(storage_cache(profile == "compute")).remove(&key);
                    input.storage_pairs = None;
                    assert_eq!(HostState::from_payload(&input, false).err().unwrap().to_string(),
                        format!("missing storage cache: {key}"));
                }
                drop(input);
                assert_eq!(state.storage["weights"].as_slice(), value);
            }
        }
        assert_eq!(copies, 0, "storage input copied encoding");
    }

    #[test]
    fn public_input_copy() {
        let _guard = cache_test();
        let root = "a".repeat(64);
        let body = BASE64.encode(vec![7; 65_536]);
        let read = PublicReadJson {
            snapshot_root: root.clone(), circle_id: "circle".to_owned(),
            canonical_path: "/data".to_owned(), offset: 2, max_bytes: 65_536,
            metadata_json: format!("{{\"snapshot_root\":\"{root}\"}}"), body_b64: body.clone(),
        };
        let mut empty_path = read.clone();
        empty_path.canonical_path.clear();
        let mut wrong_root = read.clone();
        wrong_root.metadata_json = "{}".to_owned();
        let cases = [
            (vec![read.clone(); MAX_PUBLIC_READS + 1], "wasm public read count exceeds limit"),
            (vec![empty_path], "invalid wasm public read"),
            (vec![wrong_root], "wasm public read snapshot root mismatch"),
        ];
        let mut copies = 0;
        for profile in ["standard", "manifest", "compute"] {
            let mut input: Payload = Payload::deserialize(json!({
                "execution_profile": profile, "is_view": true, "hfhe_strict": false,
            })).unwrap();
            for (reads, expected) in &cases {
                input.public_reads = Some(reads.clone());
                let (error, count) = native_bytes::tests::large_allocs(body.len(),
                    || HostState::from_payload(&input, false).err().unwrap());
                assert_eq!(&error.to_string(), expected);
                copies += count;
            }
            input.public_reads = Some(vec![read.clone()]);
            let state = HostState::from_payload(&input, false).unwrap();
            let key = public_read_key("circle", "/data", 2, 65_536);
            assert_eq!(state.public_reads.len(), 1);
            assert_eq!(state.public_reads[&key].body_bytes, 65_536);
            assert_eq!(state.public_reads[&key].body_b64, body);
            assert_eq!(state.public_reads[&key].metadata_json, read.metadata_json);
            input.public_reads = Some(vec![read.clone(), read.clone()]);
            assert_eq!(HostState::from_payload(&input, false).err().unwrap().to_string(),
                "duplicate wasm public read");
            input.public_reads.as_mut().unwrap()[1].max_bytes = 0;
            assert_eq!(HostState::from_payload(&input, false).err().unwrap().to_string(),
                "invalid wasm public read");
            input.public_reads = Some(vec![read.clone(); 5]);
            for (index, entry) in input.public_reads.as_mut().unwrap().iter_mut().enumerate() {
                entry.offset = index;
            }
            assert_eq!(HostState::from_payload(&input, false).err().unwrap().to_string(),
                "wasm public read bytes exceed limit");
            drop(input);
            assert_eq!(state.public_reads[&key].body_b64, body);
        }
        assert_eq!(copies, 0, "refused public input copied body");
    }

    fn matrix_runtime(profile: &str) -> Runtime {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 6], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", "host_tensor_matmul_fp", EntityType::Function(0));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [1, 2, 0] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 257, maximum: Some(1025), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 1);
        exports.export("octra_query", ExportKind::Func, 2);
        exports.export("step", ExportKind::Func, 3);
        module.section(&exports);
        let mut code = CodeSection::new();
        for value in [64, 0] {
            let mut function = Function::new([]);
            function.instruction(&I::I32Const(value)).instruction(&I::End);
            code.function(&function);
        }
        let mut function = Function::new([]);
        for index in 0..6 {
            function.instruction(&I::LocalGet(index));
        }
        function.instruction(&I::Call(0)).instruction(&I::End);
        code.function(&function);
        module.section(&code);
        let input = Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "execution_profile": profile,
            "is_view": true, "update_policy": false, "hfhe_strict": false,
            "fuel_limit": 20_000_000,
        })).unwrap();
        Runtime::instantiate(&input, false).unwrap()
    }

    #[test]
    fn matrix_memory() {
        let _guard = cache_test();
        for profile in ["standard", "manifest"] {
            let mut runtime = matrix_runtime(profile);
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            for (rows, cols) in [(2048, 2048), (2048, 1024), (1024, 1024)] {
                let before = runtime.store.get_fuel().unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(8 * 1024 * 1024, || {
                    step.call(&mut runtime.store, (65, 64, 32768, rows, 1, cols))
                });
                if rows * cols * 8 > 16 * 1024 * 1024 {
                    assert_eq!(result.unwrap_err().to_string(), "tensor output exceeds cap");
                } else {
                    assert_eq!(result.unwrap(), rows * cols * 8);
                }
                assert_eq!(before - runtime.store.get_fuel().unwrap(),
                    (rows * cols + (rows + cols) * 8 + 131) as u64);
                assert_eq!(copies, 0, "matrix allocated a separate output buffer");
            }
        }
    }

    #[test]
    fn matrix_refusal() {
        let _guard = cache_test();
        for profile in ["standard", "manifest"] {
            let mut runtime = matrix_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
            for ptr in [-1, end as i32 - 8] {
                let before = runtime.store.get_fuel().unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(8 * 1024 * 1024, || {
                    step.call(&mut runtime.store, (ptr, 64, 32768, 1024, 1, 1024))
                });
                assert_eq!(result.unwrap(), -2);
                assert_eq!(before - runtime.store.get_fuel().unwrap(), 1_065_091);
                assert_eq!(copies, 0, "refused matrix allocated an output buffer");
                assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
            }
        }
    }

    #[test]
    fn matrix_bits() {
        let _guard = cache_test();
        let cases = [
            (vec![1e16, -1e16, 1.0], vec![1.0; 3], 1_f64.to_bits()),
            (vec![1.0, 1e16, -1e16], vec![1.0; 3], 0),
            (vec![f64::from_bits(1)], vec![1.0], 1),
            (vec![f64::MIN_POSITIVE], vec![0.5], 0x0008000000000000),
            (vec![f64::MAX], vec![2.0], 0x7ff0000000000000),
            (vec![-0.0], vec![1.0], 0),
            (vec![f64::from_bits(0x7ff0000000000042)], vec![1.0], 0x7ff8000000000042),
            (vec![f64::from_bits(0xfff0000000000042)], vec![1.0], 0xfff8000000000042),
            (vec![f64::from_bits(0x7ff812345678abcd)], vec![1.0], 0x7ff812345678abcd),
        ];
        for profile in ["standard", "manifest"] {
            let mut runtime = matrix_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            for (lhs, rhs, expected) in &cases {
                for (ptr, values) in [(65, lhs), (256, rhs)] {
                    for (index, value) in values.iter().enumerate() {
                        memory.write(&mut runtime.store, ptr + index * 8, &value.to_le_bytes()).unwrap();
                    }
                }
                assert_eq!(step.call(&mut runtime.store, (513, 65, 256, 1, lhs.len() as i32, 1))
                    .unwrap(), 8);
                assert_eq!(&memory.data(&runtime.store)[513..521], &expected.to_le_bytes());
            }
        }
    }

    #[test]
    fn matrix_values() {
        let _guard = cache_test();
        let values = [0.0_f64, -0.0, 1.0, -2.5, 1e-300, 1e300,
            f64::INFINITY, f64::NEG_INFINITY, f64::from_bits(0x7ff812345678abcd)];
        for profile in ["standard", "manifest"] {
            let mut runtime = matrix_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            for m in 1..=4 {
                for k in 1..=5 {
                    for n in 1..=4 {
                        let lhs: Vec<_> = (0..m * k).map(|i| values[(i + n) % values.len()]).collect();
                        let rhs: Vec<_> = (0..k * n).map(|i| values[(i + m) % values.len()]).collect();
                        let mut expected = Vec::new();
                        for r in 0..m {
                            for c in 0..n {
                                let mut acc = 0.0_f64;
                                for i in 0..k {
                                    acc += lhs[r * k + i] * rhs[i * n + c];
                                }
                                expected.extend_from_slice(&acc.to_le_bytes());
                            }
                        }
                        for ptr in [65, 69, 512, 516, 1025] {
                            let mut image = vec![7_u8; 2048];
                            for (index, value) in lhs.iter().enumerate() {
                                image[65 + index * 8..73 + index * 8]
                                    .copy_from_slice(&value.to_le_bytes());
                            }
                            for (index, value) in rhs.iter().enumerate() {
                                image[512 + index * 8..520 + index * 8]
                                    .copy_from_slice(&value.to_le_bytes());
                            }
                            memory.write(&mut runtime.store, 0, &image).unwrap();
                            let before = runtime.store.get_fuel().unwrap();
                            assert_eq!(step.call(&mut runtime.store,
                                (ptr as i32, 65, 512, m as i32, k as i32, n as i32)).unwrap(),
                                expected.len() as i32);
                            image[ptr..ptr + expected.len()].copy_from_slice(&expected);
                            assert!(&memory.data(&runtime.store)[..image.len()] == image,
                                "matrix result differs rows = {m} cols = {n} depth = {k}");
                            assert_eq!(before - runtime.store.get_fuel().unwrap(),
                                (m * k * n + (m * k + k * n) * 8 + 131) as u64);
                        }
                    }
                }
            }
            let mut image = vec![7_u8; 256];
            for (index, value) in [1.0_f64, 2.0, 3.0, 4.0].iter().enumerate() {
                image[65 + index * 8..73 + index * 8].copy_from_slice(&value.to_le_bytes());
            }
            memory.write(&mut runtime.store, 0, &image).unwrap();
            assert_eq!(step.call(&mut runtime.store, (69, 65, 65, 2, 2, 2)).unwrap(), 32);
            for (index, value) in [7.0_f64, 10.0, 15.0, 22.0].iter().enumerate() {
                image[69 + index * 8..77 + index * 8].copy_from_slice(&value.to_le_bytes());
            }
            assert!(&memory.data(&runtime.store)[..image.len()] == image,
                "aliased matrix result differs");
        }
    }

    #[test]
    fn matrix_errors() {
        let _guard = cache_test();
        for profile in ["standard", "manifest"] {
            let mut runtime = matrix_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
            for ptr in [-1, end as i32 - 8] {
                let before = runtime.store.get_fuel().unwrap();
                assert_eq!(step.call(&mut runtime.store, (ptr, 64, 128, 2, 1, 2)).unwrap(), -2);
                assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
                assert_eq!(before - runtime.store.get_fuel().unwrap(), 167);
            }
            for shape in [(0, 1, 1), (1, -1, 1), (1, 1, 0)] {
                assert_eq!(step.call(&mut runtime.store,
                    (-1, -1, -1, shape.0, shape.1, shape.2)).unwrap(), -1);
            }
            for (left, right, reason) in [
                (-1, 128, "negative pointer"),
                (64, i32::MAX, "guest memory read out of bounds"),
            ] {
                assert_eq!(step.call(&mut runtime.store,
                    (-1, left, right, 2048, 1, 2048)).unwrap_err().to_string(), reason);
            }
            runtime.store.data_mut().max_guest_bytes = 32;
            assert_eq!(step.call(&mut runtime.store, (256, 64, 128, 2, 1, 2)).unwrap(), 32);
            runtime.store.data_mut().max_guest_bytes = 31;
            assert_eq!(step.call(&mut runtime.store, (-1, 64, 128, 2, 1, 2))
                .unwrap_err().to_string(), "tensor output exceeds cap");
            runtime.store.set_fuel(16).unwrap();
            assert_eq!(step.call(&mut runtime.store, (64, 64, 128, 2048, 1, 2048))
                .unwrap_err().to_string(), "wasm fuel exhausted");
            assert_eq!(runtime.store.get_fuel().unwrap(), 0);
            runtime.store.set_fuel(u64::MAX).unwrap();
            assert_eq!(step.call(&mut runtime.store,
                (64, 64, 128, i32::MAX, i32::MAX, i32::MAX))
                .unwrap_err().to_string(), "wasm host work overflow");
        }
    }

    fn tensor_runtime(profile: &str) -> Runtime {
        tensor_program(profile, false)
    }

    fn tensor_program(profile: &str, q24: bool) -> Runtime {
        tensor_modes(profile, q24, profile == "compute", false)
    }

    fn tensor_modes(profile: &str, q24: bool, load_fixed: bool, local: bool) -> Runtime {
        let fixed = profile == "compute";
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32; 3], [ValType::I32]);
        types.ty().function([ValType::I32, ValType::I32, ValType::I32,
            ValType::I32, ValType::I32, ValType::I64], [ValType::I32]);
        types.ty().function([ValType::I32, ValType::I32, ValType::I32,
            ValType::I32, ValType::I32, ValType::I32, ValType::I64], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        let name = if fixed { "host_tensor_residual_add_q24" }
            else { "host_tensor_append_vec_fp" };
        imports.import("octra", name, EntityType::Function(if fixed { 3 } else { 0 }));
        let name = if load_fixed { "host_tensor_load_i8_kv_q24" } else { "host_tensor_load_i8_kv" };
        imports.import("octra", name, EntityType::Function(4));
        let name = if q24 { "host_tensor_linear_i8_kv_q24" }
            else { "host_tensor_linear_i8_kv" };
        imports.import("octra", name, EntityType::Function(5));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [1, 2, 0, 4, 5] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 257, maximum: Some(1025), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 3);
        exports.export("octra_query", ExportKind::Func, 4);
        exports.export("step", ExportKind::Func, 5);
        exports.export("load", ExportKind::Func, 6);
        exports.export("linear", ExportKind::Func, 7);
        module.section(&exports);
        let mut code = CodeSection::new();
        for value in [64, 0] {
            let mut function = Function::new([]);
            function.instruction(&I::I32Const(value)).instruction(&I::End);
            code.function(&function);
        }
        let mut function = Function::new([]);
        for index in 0..if fixed { 3 } else { 4 } {
            function.instruction(&I::LocalGet(index));
        }
        function.instruction(&I::Call(0)).instruction(&I::End);
        code.function(&function);
        let mut function = Function::new([]);
        for index in 0..6 {
            function.instruction(&I::LocalGet(index));
        }
        function.instruction(&I::Call(1)).instruction(&I::End);
        code.function(&function);
        let mut function = Function::new([]);
        for index in 0..7 {
            function.instruction(&I::LocalGet(index));
        }
        function.instruction(&I::Call(2)).instruction(&I::End);
        code.function(&function);
        module.section(&code);
        let input = Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "execution_profile": profile,
            "is_view": true, "update_policy": false, "hfhe_strict": false,
            "inspect_policy": local,
            "fuel_limit": 20_000_000,
        })).unwrap();
        Runtime::instantiate(&input, false).unwrap()
    }

    fn cache_runtime(fixed: bool, local: bool, scope: &str) -> Runtime {
        Runtime::instantiate(&cache_input(fixed, local, scope, false), !local).unwrap()
    }

    fn cache_input(fixed: bool, local: bool, scope: &str, start: bool) -> Payload<'static> {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 5], [ValType::I32]);
        types.ty().function([ValType::I32; 7], [ValType::I32]);
        let mut args = vec![ValType::I32; 9];
        if fixed {
            args.push(ValType::I64);
        }
        types.ty().function(args, [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([], []);
        module.section(&types);
        let mut imports = ImportSection::new();
        let names = if fixed {
            ["host_tensor_session_reset_q24", "host_tensor_session_has_q24",
                "host_tensor_session_append_kv_q24", "host_tensor_session_attention_kv_q24"]
        } else {
            ["host_tensor_session_reset", "host_tensor_session_has",
                "host_tensor_session_append_kv_fp", "host_tensor_session_attention_kv_fp"]
        };
        for (name, ty) in names.into_iter().zip([0, 0, 1, 2]) {
            imports.import("octra", name, EntityType::Function(ty));
        }
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [3, 4, 0, 0, 1, 2, 5] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 4, maximum: Some(4), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        for (name, index) in [("octra_alloc", 4), ("octra_query", 5),
            ("reset", 6), ("has", 7), ("append", 8), ("attention", 9)] {
            exports.export(name, ExportKind::Func, index);
        }
        module.section(&exports);
        if start {
            module.section(&StartSection { function_index: 10 });
        }
        let mut code = CodeSection::new();
        for value in [64, 0] {
            let mut function = Function::new([]);
            function.instruction(&I::I32Const(value)).instruction(&I::End);
            code.function(&function);
        }
        for (index, count) in [(0, 5), (1, 5), (2, 7), (3, if fixed { 10 } else { 9 })] {
            let mut function = Function::new([]);
            for arg in 0..count {
                function.instruction(&I::LocalGet(arg));
            }
            function.instruction(&I::Call(index)).instruction(&I::End);
            code.function(&function);
        }
        let mut startup = Function::new([]);
        for ptr in [0, 1] {
            for value in [ptr, 1, 1, 1024, 1] {
                startup.instruction(&I::I32Const(value));
            }
            startup.instruction(&I::Call(0)).instruction(&I::Drop);
        }
        startup.instruction(&I::Unreachable).instruction(&I::End);
        code.function(&startup);
        module.section(&code);
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(0), [b'a', b'b']);
        module.section(&data);
        Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()),
            "execution_profile": if scope.is_empty() { "standard" } else { "compute" },
            "is_view": true, "inspect_policy": local,
            "address": "", "caller": "",
            "compute_session_scope": (!scope.is_empty())
                .then(|| hex::encode(Sha256::digest(scope.as_bytes()))),
            "update_policy": false, "hfhe_strict": false, "fuel_limit": 20_000_000,
        })).unwrap()
    }

    fn tensor_reset(runtime: &mut Runtime, name: &[u8], count: i32) -> i32 {
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        memory.write(&mut runtime.store, 0, name).unwrap();
        runtime.store.set_fuel(20_000_000).unwrap();
        runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32), i32>(
            &runtime.store, "reset").unwrap()
            .call(&mut runtime.store, (0, name.len() as i32, 1, count, 1)).unwrap()
    }

    fn tensor_has(runtime: &mut Runtime, name: &[u8], count: i32) -> i32 {
        runtime.instance.get_memory(&runtime.store, "memory").unwrap()
            .write(&mut runtime.store, 0, name).unwrap();
        runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32), i32>(
            &runtime.store, "has").unwrap()
            .call(&mut runtime.store, (0, name.len() as i32, 1, count, 1)).unwrap()
    }

    #[test]
    fn tensor_cache_quota() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "quota");
            let resident = compute_memory().used();
            let lease = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
            let (result, count) = native_bytes::tests::large_allocs(8192,
                || tensor_reset(&mut runtime, b"kv", 1024));
            assert_eq!(result, -1, "tensor cache ignored the shared memory quota");
            assert_eq!(count, 0, "tensor buffers allocated before reservation");
            assert_eq!(tensor_has(&mut runtime, b"kv", 1024), 0);
            drop(lease);
            assert_eq!(tensor_reset(&mut runtime, b"kv", 1024), 0);
            assert!(compute_memory().used() >= resident + 16384);
            clear_compute_session_scope(runtime.store.data()).unwrap();
            assert_eq!(compute_memory().used(), resident);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn tensor_cache_lifetime() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "lifetime");
            let resident = compute_memory().used();
            assert_eq!(tensor_reset(&mut runtime, b"kv", 1024), 0);
            let used = compute_memory().used();
            assert!(used >= resident + 16384);
            let lease = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
            let (result, count) = native_bytes::tests::large_allocs(4096,
                || tensor_reset(&mut runtime, b"kv", 512));
            assert_eq!(result, -1);
            assert_eq!(count, 0);
            assert_eq!(tensor_has(&mut runtime, b"kv", 1024), 1,
                "failed replacement removed the previous tensor");
            drop(lease);
            assert_eq!(tensor_reset(&mut runtime, b"kv", 512), 0);
            assert!(compute_memory().used() < used);
            clear_compute_session_scope(runtime.store.data()).unwrap();
            assert_eq!(compute_memory().used(), resident);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn tensor_cache_alloc() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "alloc");
            assert_eq!(tensor_reset(&mut runtime, b"kv", 256), 0);
            let used = compute_memory().used();
            for skip in [0, 1] {
                let result = native_bytes::tests::refused_alloc_after(8192, skip,
                    || tensor_reset(&mut runtime, b"kv", 1024));
                assert_eq!(result, -1);
                assert_eq!(compute_memory().used(), used);
                assert_eq!(tensor_has(&mut runtime, b"kv", 256), 1);
            }
            assert_eq!(tensor_reset(&mut runtime, b"kv", 1024), 0);
            clear_compute_session_scope(runtime.store.data()).unwrap();
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn tensor_cache_count() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "count");
            let limit = if fixed { FIXED_TENSOR_SESSION_CACHE_LIMIT } else { TENSOR_SESSION_CACHE_LIMIT };
            for index in 0..limit {
                assert_eq!(tensor_reset(&mut runtime, index.to_string().as_bytes(), 2), 0);
            }
            let used = compute_memory().used();
            assert_eq!(tensor_reset(&mut runtime, b"extra", 2), -1);
            assert_eq!(compute_memory().used(), used);
            for index in 0..limit {
                assert_eq!(tensor_has(&mut runtime, index.to_string().as_bytes(), 2), 1);
            }
            assert_eq!(tensor_reset(&mut runtime, b"0", 4), 0);
            assert_eq!(tensor_has(&mut runtime, b"0", 4), 1);
            clear_compute_session_scope(runtime.store.data()).unwrap();
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn tensor_cache_standard() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        let mut local = cache_runtime(false, true, "");
        let mut prior = cache_runtime(false, false, "");
        let resident = compute_memory().used();
        assert_eq!(tensor_reset(&mut prior, b"kv", 2), 0);
        assert_eq!(tensor_has(&mut local, b"kv", 2), 0);
        assert_eq!(tensor_reset(&mut local, b"kv", 4), 0);
        let used = compute_memory().used();
        assert!(used >= resident + 64);
        let lease = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
        assert_eq!(tensor_reset(&mut local, b"kv", 8), -1);
        assert_eq!(tensor_has(&mut local, b"kv", 4), 1);
        drop(lease);
        clear_compute_session_prefix("||", Some(&local_session_prefix(local.store.data()))).unwrap();
        assert_eq!(tensor_has(&mut prior, b"kv", 2), 1);
        clear_compute_session_prefix("||", None).unwrap();
        assert_eq!(compute_memory().used(), resident);
        drop(local);
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn tensor_cache_expiry() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "expiry");
            let resident = compute_memory().used();
            assert_eq!(tensor_reset(&mut runtime, b"kv", 1024), 0);
            assert!(compute_memory().used() > resident);
            let expired = now_secs() - TENSOR_SESSION_CACHE_TTL_SECS - 1.0;
            if fixed {
                let key = fixed_tensor_session_namespaced_key(runtime.store.data(), "kv");
                fixed_tensor_store(true).lock().unwrap().get_mut(&key).unwrap().updated_at = expired;
            } else {
                let key = tensor_session_namespaced_key(runtime.store.data(), "kv");
                tensor_store(true).lock().unwrap().get_mut(&key).unwrap().updated_at = expired;
            }
            maintain_compute_session_caches();
            assert_eq!(compute_memory().used(), resident);
            assert_eq!(tensor_has(&mut runtime, b"kv", 1024), 0);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn tensor_cache_start() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for (fixed, scope) in [(false, "start"), (false, ""), (true, "start")] {
            for local in [true, false] {
                let input = cache_input(fixed, local, scope, true);
                let result = Runtime::instantiate(&input, !local);
                assert!(result.err().unwrap().contains("wasm start failed"));
                assert_eq!(compute_memory().used(), baseline,
                    "failed startup retained tensor reservation");
                let mut reader = cache_runtime(fixed, local, scope);
                for key in [b"a", b"b"] {
                    assert_eq!(tensor_has(&mut reader, key, 1024), if local { 0 } else { 1 });
                }
                assert_eq!(tensor_reset(&mut reader, b"c", 2), 0);
                if scope.is_empty() {
                    let prefix = local_session_prefix(reader.store.data());
                    clear_compute_session_prefix("||", local.then_some(prefix.as_str())).unwrap();
                } else {
                    clear_compute_session_scope(reader.store.data()).unwrap();
                }
                drop(reader);
                assert_eq!(compute_memory().used(), baseline);
            }
        }
    }

    #[test]
    fn tensor_cache_isolation() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut local = cache_runtime(fixed, true, "isolation");
            let mut prior = cache_runtime(fixed, false, "isolation");
            assert_eq!(tensor_reset(&mut prior, b"kv", 4), 0);
            assert_eq!(tensor_has(&mut local, b"kv", 4), 0,
                "local tensor reads historical session");
            assert_eq!(tensor_reset(&mut local, b"kv", 8), 0);
            assert_eq!(tensor_has(&mut prior, b"kv", 4), 1);
            clear_compute_session_scope(local.store.data()).unwrap();
            assert_eq!(tensor_has(&mut prior, b"kv", 4), 1,
                "local cleanup removed historical tensor");
            drop(local);
            assert_eq!(compute_memory().used(), baseline);
            let lease = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - baseline).unwrap();
            assert_eq!(tensor_reset(&mut prior, b"kv", 8), 0);
            drop(lease);
            clear_compute_session_scope(prior.store.data()).unwrap();
        }
    }

    #[test]
    fn tensor_cache_calls() {
        let _guard = cache_test();
        let baseline = compute_memory().used();
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "calls");
            assert_eq!(tensor_reset(&mut runtime, b"kv", 2), 0);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let one = if fixed { deterministic_tensor::ONE } else { 1_f64.to_bits() as i64 };
            for ptr in [64, 72] {
                memory.write(&mut runtime.store, ptr, &one.to_le_bytes()).unwrap();
            }
            let append = runtime.instance.get_typed_func::<
                (i32, i32, i32, i32, i32, i32, i32), i32>(&runtime.store, "append").unwrap();
            for position in [0, 1] {
                assert_eq!(append.call(&mut runtime.store, (0, 2, 0, position, 64, 72, 1)).unwrap(), 0);
            }
            if fixed {
                let call = runtime.instance.get_typed_func::<
                    (i32, i32, i32, i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "attention").unwrap();
                assert_eq!(call.call(&mut runtime.store, (0, 2, 64, 80, 2, 0, 1, 1, 1, one)).unwrap(), 8);
            } else {
                let call = runtime.instance.get_typed_func::<
                    (i32, i32, i32, i32, i32, i32, i32, i32, i32), i32>(
                    &runtime.store, "attention").unwrap();
                assert_eq!(call.call(&mut runtime.store, (0, 2, 64, 80, 2, 0, 1, 1, 1)).unwrap(), 8);
            }
            assert_eq!(&memory.data(&runtime.store)[80..88], &one.to_le_bytes());
            clear_compute_session_scope(runtime.store.data()).unwrap();
        }
        assert_eq!(compute_memory().used(), baseline);
    }

    fn attention_runtime(name: &str, count: usize, dim: usize,
        keys: Vec<i64>, values: Vec<i64>) -> Runtime {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        let mut args = vec![ValType::I32; 9];
        args.push(ValType::I64);
        types.ty().function(args, [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", "host_tensor_session_attention_kv_q24", EntityType::Function(0));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [1, 2, 0] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1, maximum: Some(1), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 1);
        exports.export("octra_query", ExportKind::Func, 2);
        exports.export("attention", ExportKind::Func, 3);
        module.section(&exports);
        let mut code = CodeSection::new();
        for value in [64, 0] {
            let mut function = Function::new([]);
            function.instruction(&I::I32Const(value)).instruction(&I::End);
            code.function(&function);
        }
        let mut function = Function::new([]);
        for index in 0..10 {
            function.instruction(&I::LocalGet(index));
        }
        function.instruction(&I::Call(0)).instruction(&I::End);
        code.function(&function);
        module.section(&code);
        let input = Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "execution_profile": "compute",
            "is_view": true, "update_policy": false, "hfhe_strict": false,
            "fuel_limit": 20_000_000,
        })).unwrap();
        let runtime = Runtime::instantiate(&input, false).unwrap();
        let key = fixed_tensor_session_namespaced_key(runtime.store.data(), name);
        fixed_tensor_store(true).lock().unwrap().insert(key, FixedTensorSessionEntry {
            n_layers: 1, max_t: count, kv_dim: dim,
            k_cache: keys, v_cache: values, _lease: None, updated_at: now_secs(),
        });
        runtime
    }

    #[test]
    fn attention_memory() {
        let _guard = cache_test();
        let count = 131_073;
        let one = deterministic_tensor::ONE;
        let name = "attn_mem";
        let mut runtime = attention_runtime(name, count, 1, vec![one; count], vec![one; count]);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let attention = runtime.instance.get_typed_func::<
            (i32, i32, i32, i32, i32, i32, i32, i32, i32, i64), i32>(
            &runtime.store, "attention").unwrap();
        let end = memory.data_size(&runtime.store);
        let output: Vec<_> = [one; 4].iter().flat_map(|value| value.to_le_bytes()).collect();
        for ptr in [-1, end as i32 - 24, 128, 137, 0, 2048] {
            memory.write(&mut runtime.store, 0, name.as_bytes()).unwrap();
            memory.write(&mut runtime.store, 128, &output).unwrap();
            let before = memory.data(&runtime.store).to_vec();
            runtime.store.set_fuel(20_000_000).unwrap();
            runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("attn", None, None));
            let (result, copies) = native_bytes::tests::large_allocs(count * 8, || {
                attention.call(&mut runtime.store,
                    (0, name.len() as i32, 128, ptr, count as i32, 0, 4, 1, 1, one))
            });
            let status = if ptr < 0 || ptr as usize + output.len() > end { -2 } else { 32 };
            assert_eq!(result.unwrap(), status);
            assert_eq!(copies, 1, "attention allocated extra score workspaces");
            let effort = 20_000_000 - runtime.store.get_fuel().unwrap();
            assert_eq!(effort, (count as u64 * 12).div_ceil(64) + 172);
            let mut trace = ComputeTrace::new("attn", None, None);
            trace.record("tensor_session_attention_q24", &[
                &Sha256::digest(name.as_bytes()), &(count as u64).to_le_bytes(),
                &0_u64.to_le_bytes(), &4_u64.to_le_bytes(), &1_u64.to_le_bytes(),
                &1_u64.to_le_bytes(), &one.to_le_bytes(),
                &trace_i64_digest(&[one; 4]), &trace_i64_digest(&[one; 4]),
            ]);
            assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap()
                .finish(status, &[], effort), trace.finish(status, &[], effort));
            let mut expected = before;
            if status == 32 {
                expected[ptr as usize..ptr as usize + output.len()].copy_from_slice(&output);
            }
            assert_eq!(memory.data(&runtime.store), expected);
        }
        let key = fixed_tensor_session_namespaced_key(runtime.store.data(), name);
        fixed_tensor_store(true).lock().unwrap().remove(&key);
    }

    #[test]
    fn attention_errors() {
        let _guard = cache_test();
        let one = deterministic_tensor::ONE;
        let name = "attn_err";
        let mut runtime = attention_runtime(name, 2, 1,
            vec![i64::MIN, i64::MAX], vec![one; 2]);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let attention = runtime.instance.get_typed_func::<
            (i32, i32, i32, i32, i32, i32, i32, i32, i32, i64), i32>(
            &runtime.store, "attention").unwrap();
        memory.write(&mut runtime.store, 0, name.as_bytes()).unwrap();
        memory.write(&mut runtime.store, 128, &0_i64.to_le_bytes()).unwrap();
        memory.write(&mut runtime.store, 136, &one.to_le_bytes()).unwrap();
        memory.write(&mut runtime.store, 256, &[7; 16]).unwrap();
        for ptr in [-1, 256] {
            runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("attn", None, None));
            assert_eq!(attention.call(&mut runtime.store,
                (0, name.len() as i32, 128, ptr, 2, 0, 2, 1, 1, one))
                .unwrap_err().to_string(), "attention score overflow");
            assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
            assert_eq!(&memory.data(&runtime.store)[256..272], &[7; 16]);
        }
        assert_eq!(attention.call(&mut runtime.store,
            (-1, 1, -1, -1, 0, 0, 2, 1, 1, 0)).unwrap(), -1);
        assert_eq!(attention.call(&mut runtime.store,
            (0, name.len() as i32, -1, -1, 2, 0, 2, 1, 1, 0))
            .unwrap_err().to_string(), "negative pointer");
        assert_eq!(attention.call(&mut runtime.store,
            (0, name.len() as i32, 128, -1, 3, 0, 2, 1, 1, 0)).unwrap(), -1);
        assert_eq!(attention.call(&mut runtime.store,
            (0, name.len() as i32, 128, -1, 2, 0, 2, 1, 1, 0))
            .unwrap_err().to_string(), "invalid attention tensor shape");
        let key = fixed_tensor_session_namespaced_key(runtime.store.data(), name);
        fixed_tensor_store(true).lock().unwrap().remove(&key);
        assert_eq!(attention.call(&mut runtime.store,
            (0, name.len() as i32, 128, -1, 2, 0, 2, 1, 1, 0)).unwrap(), -3);
    }

    fn top1_runtime(profile: &str, fixed: bool, view: bool, policy: bool) -> Runtime {
        top1_mode(profile, fixed, view, policy, false)
    }

    fn top1_mode(profile: &str, fixed: bool, view: bool, policy: bool, description: bool) -> Runtime {
        let id = "a".repeat(64);
        let key = state_descriptor_key(&id, "state_class");
        let mut request = state_request("state_describe", &[&id, "AB", "raw", "", "", "q24", ""]);
        let boolean = request.len() - 5;
        request[boolean] = 2;
        let shift = if description { 1 } else { 0 };
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32, ValType::I32, ValType::I32,
            ValType::I32, ValType::I32, ValType::I64, ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([], []);
        module.section(&types);
        let mut imports = ImportSection::new();
        let name = if fixed { "host_tensor_top1_i8_kv_q24" } else { "host_tensor_top1_i8_kv" };
        imports.import("octra", name, EntityType::Function(0));
        if description {
            imports.import("octra", "host_circle_invoke", EntityType::Function(3));
        }
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [1, 2, 0] {
            functions.function(ty);
        }
        if description {
            functions.function(4);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1, maximum: Some(1), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        if description {
            let mut globals = GlobalSection::new();
            globals.global(
                GlobalType { val_type: ValType::I32, mutable: true, shared: false },
                &ConstExpr::i32_const(-9),
            );
            module.section(&globals);
        }
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 1 + shift);
        exports.export("octra_query", ExportKind::Func, 2 + shift);
        exports.export("top1", ExportKind::Func, 3 + shift);
        if description {
            exports.export("status", ExportKind::Global, 0);
        }
        module.section(&exports);
        if description {
            module.section(&StartSection { function_index: 4 + shift });
        }
        let mut code = CodeSection::new();
        let mut function = Function::new([]);
        function.instruction(&I::I32Const(256)).instruction(&I::End);
        code.function(&function);
        let mut function = Function::new([]);
        for value in [65, 0, 1, 2, 2] {
            function.instruction(&I::I32Const(value));
        }
        function.instruction(&I::I64Const(1_f64.to_bits() as i64));
        function.instruction(&I::I32Const(-1));
        function.instruction(&I::Call(0)).instruction(&I::End);
        code.function(&function);
        let mut function = Function::new([]);
        for index in 0..7 {
            function.instruction(&I::LocalGet(index));
        }
        function.instruction(&I::Call(0)).instruction(&I::End);
        code.function(&function);
        if description {
            let mut start = Function::new([]);
            for value in [2048, request.len() as i32, 4096, 1024] {
                start.instruction(&I::I32Const(value));
            }
            start.instruction(&I::Call(1)).instruction(&I::Drop);
            for value in [1024, 0, key.len() as i32, 2, 1] {
                start.instruction(&I::I32Const(value));
            }
            start.instruction(&I::I64Const(1_f64.to_bits() as i64));
            start.instruction(&I::I32Const(-1));
            start.instruction(&I::Call(0));
            start.instruction(&I::GlobalSet(0)).instruction(&I::End);
            code.function(&start);
        }
        module.section(&code);
        if description {
            let mut data = DataSection::new();
            data.active(0, &ConstExpr::i32_const(0), key.bytes());
            data.active(0, &ConstExpr::i32_const(2048), request.iter().copied());
            let value = if fixed { deterministic_tensor::ONE } else { 1_f64.to_bits() as i64 };
            data.active(0, &ConstExpr::i32_const(1024), value.to_le_bytes());
            module.section(&data);
        }
        let input = Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "execution_profile": profile,
            "is_view": view, "update_policy": policy, "hfhe_strict": false,
            "fuel_limit": 20_000_000,
        })).unwrap();
        let mut runtime = Runtime::instantiate(&input, description).unwrap();
        if description {
            return runtime;
        }
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        memory.write(&mut runtime.store, 0, b"w").unwrap();
        let values = if fixed { [deterministic_tensor::ONE, 2 * deterministic_tensor::ONE] }
            else { [1_f64.to_bits() as i64, 2_f64.to_bits() as i64] };
        for (index, value) in values.iter().enumerate() {
            memory.write(&mut runtime.store, 65 + index * 8, &value.to_le_bytes()).unwrap();
        }
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![1, 0, 0, 1].into())),
        ]));
        runtime
    }

    #[test]
    fn top1_refusal() {
        let _guard = cache_test();
        let mut statuses = Vec::new();
        for (profile, fixed) in [("standard", false), ("manifest", false),
            ("compute", false), ("compute", true)] {
            let mut runtime = top1_runtime(profile, fixed, true, true);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let top1 = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i64, i32), i32>(
                    &runtime.store, "top1").unwrap();
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 8, &[7; 8]).unwrap();
            for ptr in [-1, end as i32 - 7, end as i32, i32::MAX] {
                runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("top1", None, None));
                let before = runtime.store.get_fuel().unwrap();
                statuses.push(top1.call(&mut runtime.store,
                    (65, 0, 1, 2, 2, 1_f64.to_bits() as i64, ptr)).unwrap());
                let effort = before - runtime.store.get_fuel().unwrap();
                assert_eq!(effort, if fixed { 150 } else { 153 });
                assert_eq!(&memory.data(&runtime.store)[end - 8..], &[7; 8]);
                let mut expected = ComputeTrace::new("top1", None, None);
                if fixed {
                    let one = deterministic_tensor::ONE;
                    expected.record("tensor_top1_i8_q24", &[
                        &Sha256::digest(b"w"), &2_u64.to_le_bytes(), &2_u64.to_le_bytes(),
                        &one.to_le_bytes(), &trace_i64_digest(&[one, 2 * one]),
                        &1_i32.to_le_bytes(), &(2 * one).to_le_bytes(),
                    ]);
                }
                assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap()
                    .finish(-2, &[], effort), expected.finish(-2, &[], effort));
            }
        }
        assert_eq!(statuses, vec![-2; 16]);
    }

    #[test]
    fn top1_values() {
        let _guard = cache_test();
        for (profile, fixed) in [("standard", false), ("manifest", false),
            ("compute", false), ("compute", true)] {
            let mut runtime = top1_runtime(profile, fixed, true, true);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let top1 = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i64, i32), i32>(
                    &runtime.store, "top1").unwrap();
            for ptr in [0, 65, 69, 73, 128, memory.data_size(&runtime.store) - 8] {
                memory.write(&mut runtime.store, 0, b"w").unwrap();
                let values = if fixed { [deterministic_tensor::ONE, 2 * deterministic_tensor::ONE] }
                    else { [1_f64.to_bits() as i64, 2_f64.to_bits() as i64] };
                for (index, value) in values.iter().enumerate() {
                    memory.write(&mut runtime.store, 65 + index * 8, &value.to_le_bytes()).unwrap();
                }
                assert_eq!(top1.call(&mut runtime.store,
                    (65, 0, 1, 2, 2, 1_f64.to_bits() as i64, ptr as i32)).unwrap(), 1);
                let score = if fixed { 2 * deterministic_tensor::ONE } else { 2_f64.to_bits() as i64 };
                assert_eq!(&memory.data(&runtime.store)[ptr..ptr + 8], &score.to_le_bytes());
            }
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![0; 4].into())),
            ]));
            assert_eq!(top1.call(&mut runtime.store,
                (65, 0, 1, 2, 2, 1_f64.to_bits() as i64, 128)).unwrap(), 0);
            assert_eq!(&memory.data(&runtime.store)[128..136], &0_i64.to_le_bytes());
        }
    }

    #[test]
    fn top1_errors() {
        let _guard = cache_test();
        for (profile, fixed) in [("standard", false), ("compute", true)] {
            let mut runtime = top1_runtime(profile, fixed, true, true);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let top1 = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i64, i32), i32>(
                    &runtime.store, "top1").unwrap();
            memory.write(&mut runtime.store, 128, &[7; 8]).unwrap();
            assert_eq!(top1.call(&mut runtime.store,
                (-1, 0, 1, 2, 0, 1_f64.to_bits() as i64, -1)).unwrap(), -1);
            assert_eq!(top1.call(&mut runtime.store,
                (-1, 0, 1, 2, 2, 1_f64.to_bits() as i64, -1))
                .unwrap_err().to_string(), "negative pointer");
            memory.write(&mut runtime.store, 0, &[255]).unwrap();
            assert_eq!(top1.call(&mut runtime.store,
                (65, 0, 1, 2, 2, 1_f64.to_bits() as i64, -1))
                .unwrap_err().to_string(), "invalid guest utf8");
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::default());
            assert_eq!(top1.call(&mut runtime.store,
                (65, 0, 1, 2, 2, 1_f64.to_bits() as i64, -1)).unwrap(), -3);
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![1; 3].into())),
            ]));
            let result = top1.call(&mut runtime.store,
                (65, 0, 1, 2, 2, 1_f64.to_bits() as i64, -1));
            if fixed {
                assert_eq!(result.unwrap_err().to_string(), "invalid top1 tensor shape");
            } else {
                assert_eq!(result.unwrap(), -1);
            }
            assert_eq!(&memory.data(&runtime.store)[128..136], &[7; 8]);
        }
        let mut runtime = top1_runtime("compute", true, true, true);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let top1 = runtime.instance
            .get_typed_func::<(i32, i32, i32, i32, i32, i64, i32), i32>(
                &runtime.store, "top1").unwrap();
        memory.write(&mut runtime.store, 65, &i64::MAX.to_le_bytes()).unwrap();
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![127].into())),
        ]));
        assert_eq!(top1.call(&mut runtime.store,
            (65, 0, 1, 1, 1, 1_f64.to_bits() as i64, -1))
            .unwrap_err().to_string(), "fixed point overflow");
        assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
    }

    #[test]
    fn top1_description() {
        let _guard = cache_test();
        for (profile, fixed) in [("standard", false), ("compute", true)] {
            let runtime = top1_mode(profile, fixed, false, false, true);
            let status = runtime.instance.get_global(&runtime.store, "status").unwrap();
            assert!(runtime.store.data().is_view);
            assert!(matches!(status.get(&runtime.store), wasmi::Val::I32(1)));
            let key = state_descriptor_key(&"a".repeat(64), "state_class");
            assert_eq!(runtime.store.data().storage[&key].as_slice(), b"AB");
        }
    }

    #[test]
    fn top1_executor() {
        let _guard = cache_test();
        let report = run_compute_self_test().unwrap();
        let mut prior = Sha256::new();
        prior.update(b"octra:resource-compute-executor\0");
        prior.update(hex::decode(report["evidence_root"].as_str().unwrap()).unwrap());
        prior.update((Q24_FUEL_SCALE as u64).to_be_bytes());
        prior.update((Q24_CALL_LIMIT as u64).to_be_bytes());
        prior.update(Q24_WALL_LIMIT_SECS.to_be_bytes());
        prior.update(COMPUTE_MAX_FUEL_LIMIT.to_be_bytes());
        prior.update((COMPUTE_MAX_TENSOR_SESSION_BYTES as u64).to_be_bytes());
        let mut current = prior.clone();
        current.update(b"top1-score-status-v1\0");
        let previous = hex::encode(current.clone().finalize());
        current.update(b"tensor-cache-memory-v1\0");
        current.update((COMPUTE_STORAGE_RESIDENT_LIMIT as u64).to_be_bytes());
        current.update((TENSOR_SESSION_CACHE_LIMIT as u64).to_be_bytes());
        current.update((FIXED_TENSOR_SESSION_CACHE_LIMIT as u64).to_be_bytes());
        let prior_pages = hex::encode(current.clone().finalize());
        current.update(b"guest-pages-v1\0");
        let prior_inputs = hex::encode(current.clone().finalize());
        current.update(b"tensor-input-memory-v1\0");
        let prior_results = hex::encode(current.clone().finalize());
        current.update(b"tensor-result-memory-v1\0");
        let prior_storage = hex::encode(current.clone().finalize());
        current.update(b"inline-storage-memory-v1\0");
        let prior_writes = hex::encode(current.clone().finalize());
        current.update(b"storage-write-memory-v1\0");
        let prior_keys = hex::encode(current.clone().finalize());
        current.update(b"compute-key-memory-v1\0");
        let prior_guest = hex::encode(current.clone().finalize());
        current.update(b"caller-guest-pages\0");
        let mut prior_response = current.clone();
        prior_response.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        current.update(b"response-buffer-memory\0");
        let mut prior_reply = current.clone();
        prior_reply.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        current.update(b"reply-owner-memory\0");
        let mut prior_parse = current.clone();
        prior_parse.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        current.update(b"builder-parse-memory\0");
        let mut prior_session = current.clone();
        prior_session.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        current.update(b"session-shared-memory\0");
        let mut prior_job = current.clone();
        prior_job.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        current.update(b"job-memory-budget\0");
        let mut prior_owner = current.clone();
        prior_owner.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        current.update(b"storage-job-owner\0");
        current.update((COMPUTE_MAX_INITIAL_PAGES as u64).to_be_bytes());
        let prior_nan = current.clone();
        current.update(b"linear-quiet-nan\0");
        current.update(0x7ff8000000000000_u64.to_le_bytes());
        let prior_math = current.clone();
        current.update(b"tensor-fixed-math\0libm-0.2.16\0");
        assert_ne!(report["executor_root"], hex::encode(prior.finalize()));
        assert_ne!(report["executor_root"], previous);
        assert_ne!(report["executor_root"], prior_pages);
        assert_ne!(report["executor_root"], prior_inputs);
        assert_ne!(report["executor_root"], prior_results);
        assert_ne!(report["executor_root"], prior_storage);
        assert_ne!(report["executor_root"], prior_writes);
        assert_ne!(report["executor_root"], prior_keys);
        assert_ne!(report["executor_root"], prior_guest);
        assert_ne!(report["executor_root"], hex::encode(prior_response.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_reply.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_parse.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_session.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_job.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_owner.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_nan.finalize()));
        assert_ne!(report["executor_root"], hex::encode(prior_math.finalize()));
        assert_eq!(report["executor_root"], hex::encode(current.finalize()));
        assert_eq!(report["profile"], "q24-v1");
        println!("event = executor previous = {previous} current = {}", report["executor_root"]);
    }

    #[test]
    fn top1_policies() {
        let _guard = cache_test();
        let mut legacy = top1_runtime("standard", false, false, false);
        let result = finish_execute(&mut legacy, "octra_query", &[]).unwrap();
        assert_eq!(result["error"], "wasm export returned 1");
        for policy in [false, true] {
            for (profile, fixed) in [("standard", false), ("compute", true)] {
                let mut view = top1_runtime(profile, fixed, true, policy);
                let result = finish_execute(&mut view, "octra_query", &[]).unwrap();
                assert_eq!(result["error"], "wasm export returned -2");
                assert_eq!(result["success"], false);
            }
        }
        let mut update = top1_runtime("standard", false, false, true);
        assert_eq!(finish_execute(&mut update, "octra_query", &[]).unwrap_err(),
            "wasm_v1 nonpersistent host imports are disabled for updates");
        let mut update = top1_runtime("compute", true, false, false);
        assert_eq!(finish_execute(&mut update, "octra_query", &[]).unwrap_err(),
            "wasm_v1 compute host imports are disabled for updates");
    }

    #[test]
    fn q24_linear_memory() {
        let _guard = cache_test();
        let count = 131_073;
        let size = count * 8;
        let mut runtime = tensor_program("compute", true);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let linear = runtime.instance
            .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                &runtime.store, "linear").unwrap();
        memory.write(&mut runtime.store, 0, b"w").unwrap();
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![1; count * 2].into())),
        ]));
        let end = memory.data_size(&runtime.store);
        memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
        for value in [deterministic_tensor::ONE, i64::MAX] {
            memory.write(&mut runtime.store, 65, &value.to_le_bytes()).unwrap();
            memory.write(&mut runtime.store, 73, &(-value).to_le_bytes()).unwrap();
            for ptr in [-1, end as i32 - 8, 2048] {
                runtime.store.set_fuel(20_000_000).unwrap();
                runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("q24", None, None));
                let before = runtime.store.get_fuel().unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(size, || {
                    linear.call(&mut runtime.store,
                        (ptr, 65, 0, 1, 2, count as i32, 1_f64.to_bits() as i64))
                });
                let status = if ptr == 2048 { size as i32 } else { -2 };
                assert_eq!(result.unwrap(), status);
                assert_eq!(copies, usize::from(ptr == 2048),
                    "Q24 linear allocated extra output buffers");
                let effort = before - runtime.store.get_fuel().unwrap();
                assert_eq!(effort, (count as u64 * 2).div_ceil(64) + 149);
                let mut trace = ComputeTrace::new("q24", None, None);
                trace.record("tensor_linear_i8_q24", &[
                    &Sha256::digest(b"w"), &2_u64.to_le_bytes(),
                    &(count as u64).to_le_bytes(), &deterministic_tensor::ONE.to_le_bytes(),
                    &trace_i64_digest(&[value, -value]), &trace_i64_digest(&vec![0; count]),
                ]);
                assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap()
                    .finish(status, &[], effort), trace.finish(status, &[], effort));
                assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
                if ptr == 2048 {
                    assert!(memory.data(&runtime.store)[2048..2048 + size]
                        .iter().all(|byte| *byte == 0));
                }
            }
        }
        runtime.store.data_mut().max_guest_bytes = 16;
        runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("q24", None, None));
        runtime.store.set_fuel(20_000_000).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(size, || {
            linear.call(&mut runtime.store,
                (-1, 65, 0, 1, 2, count as i32, 1_f64.to_bits() as i64))
        });
        assert_eq!(result.unwrap_err().to_string(), "tensor output exceeds cap");
        assert_eq!(copies, 0, "oversized Q24 output allocated a result buffer");
        assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
    }

    #[test]
    fn q24_linear_errors() {
        let _guard = cache_test();
        let mut runtime = tensor_program("compute", true);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let linear = runtime.instance
            .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                &runtime.store, "linear").unwrap();
        memory.write(&mut runtime.store, 0, b"w").unwrap();
        memory.write(&mut runtime.store, 65, &i64::MAX.to_le_bytes()).unwrap();
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![0, 1, 127].into())),
        ]));
        for ptr in [-1, 256] {
            for cap in [16, 32] {
                runtime.store.data_mut().max_guest_bytes = cap;
                runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("q24", None, None));
                memory.write(&mut runtime.store, 256, &[7; 24]).unwrap();
                assert_eq!(linear.call(&mut runtime.store,
                    (ptr, 65, 0, 1, 1, 3, (2_f64.powi(38)).to_bits() as i64))
                    .unwrap_err().to_string(), "fixed point overflow");
                assert_eq!(&memory.data(&runtime.store)[256..280], &[7; 24]);
                assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
            }
        }
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![0; 3].into())),
        ]));
        runtime.store.data_mut().max_guest_bytes = 16;
        assert_eq!(linear.call(&mut runtime.store,
            (-1, 65, 0, 1, 1, 3, 1_f64.to_bits() as i64))
            .unwrap_err().to_string(), "tensor output exceeds cap");
        assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![0; 2].into())),
        ]));
        assert_eq!(linear.call(&mut runtime.store,
            (-1, 65, 0, 1, 1, 3, 1_f64.to_bits() as i64))
            .unwrap_err().to_string(), "invalid linear tensor shape");
        runtime.store.data_mut().storage = Arc::new(Storage::default());
        assert_eq!(linear.call(&mut runtime.store,
            (-1, 65, 0, 1, 1, 3, 1_f64.to_bits() as i64)).unwrap(), -3);
        runtime.store.data_mut().is_view = false;
        assert_eq!(linear.call(&mut runtime.store,
            (-1, 65, 0, 1, 1, 3, 1_f64.to_bits() as i64))
            .unwrap_err().to_string(), "compute tensor import requires compute view");
    }

    #[test]
    fn q24_linear_alias() {
        let _guard = cache_test();
        let mut runtime = tensor_program("compute", true);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let linear = runtime.instance
            .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                &runtime.store, "linear").unwrap();
        let input = [deterministic_tensor::ONE, -deterministic_tensor::ONE];
        let expected = [2 * deterministic_tensor::ONE, -2 * deterministic_tensor::ONE];
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![1, 255, 255, 1].into())),
        ]));
        for ptr in [0, 65, 69, 73, 128] {
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            for (index, value) in input.iter().enumerate() {
                memory.write(&mut runtime.store, 65 + index * 8, &value.to_le_bytes()).unwrap();
            }
            runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("q24", None, None));
            assert_eq!(linear.call(&mut runtime.store,
                (ptr, 65, 0, 1, 2, 2, 1_f64.to_bits() as i64)).unwrap(), 16);
            let bytes: Vec<_> = expected.iter().flat_map(|value| value.to_le_bytes()).collect();
            assert_eq!(&memory.data(&runtime.store)[ptr as usize..ptr as usize + 16], bytes);
            let mut trace = ComputeTrace::new("q24", None, None);
            trace.record("tensor_linear_i8_q24", &[
                &Sha256::digest(b"w"), &2_u64.to_le_bytes(), &2_u64.to_le_bytes(),
                &deterministic_tensor::ONE.to_le_bytes(),
                &trace_i64_digest(&input), &trace_i64_digest(&expected),
            ]);
            assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap()
                .finish(16, &[], 0), trace.finish(16, &[], 0));
        }
    }

    #[test]
    fn linear_memory() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            if profile == "compute" {
                memory.grow(&mut runtime.store, 767).unwrap();
            }
            let linear = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "linear").unwrap();
            let cap = runtime.store.data().max_guest_bytes;
            let ptr = if profile == "compute" { 0 } else { 65 };
            for count in [cap / 8 + 1, cap / 8, cap / 16] {
                runtime.store.data_mut().storage = Arc::new(Storage::from([
                    ("w".to_owned(), Arc::new(vec![1; count].into())),
                ]));
                memory.write(&mut runtime.store, 0, b"w").unwrap();
                memory.write(&mut runtime.store, 64, &1_f64.to_le_bytes()).unwrap();
                runtime.store.set_fuel(20_000_000).unwrap();
                let before = runtime.store.get_fuel().unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(4 * 1024 * 1024, || {
                    linear.call(&mut runtime.store, (ptr as i32, 64, 0, 1, 1, count as i32,
                        1_f64.to_bits() as i64))
                });
                if count * 8 > cap {
                    assert_eq!(result.unwrap_err().to_string(), "tensor output exceeds cap");
                } else {
                    assert_eq!(result.unwrap(), (count * 8) as i32);
                    let data = memory.data(&runtime.store);
                    assert_eq!(&data[ptr..ptr + 8], &1_f64.to_le_bytes());
                    assert_eq!(&data[ptr + (count - 1) * 8..ptr + count * 8], &1_f64.to_le_bytes());
                }
                assert_eq!(before - runtime.store.get_fuel().unwrap(), count as u64 + 141);
                assert_eq!(copies, 0, "linear allocated a separate output buffer");
            }
        }
    }

    #[test]
    fn linear_refusal() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "linear").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![1; 1 << 20].into())),
            ]));
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
            for ptr in [-1, end as i32 - 8] {
                let (result, copies) = native_bytes::tests::large_allocs(4 * 1024 * 1024, || {
                    linear.call(&mut runtime.store, (ptr, 64, 0, 1, 1, 1 << 20,
                        1_f64.to_bits() as i64))
                });
                assert_eq!(result.unwrap(), -2);
                assert_eq!(copies, 0, "refused linear allocated an output buffer");
                assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
            }
        }
    }

    #[test]
    fn linear_values() {
        let _guard = cache_test();
        let values = [0.0_f64, -0.0, 1.0, -2.5, 1e-300, 1e300,
            f64::INFINITY, f64::NEG_INFINITY, f64::from_bits(0x7ff812345678abcd)];
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "linear").unwrap();
            for rows in 1..=5 {
                for cols in 1..=8 {
                    let input: Vec<_> = (0..rows).map(|i| values[(i + cols) % values.len()]).collect();
                    let weights: Vec<_> = (0..rows * cols).map(|i| (i as i8 % 7 - 3) as u8).collect();
                    runtime.store.data_mut().storage = Arc::new(Storage::from([
                        ("w".to_owned(), Arc::new(weights.clone().into())),
                    ]));
                    for scale in [1.0_f64, 0.5, -1.0, 0.0, f64::INFINITY] {
                        let mut expected = vec![0_f32; cols];
                        for i in 0..rows {
                            for col in 0..cols {
                                expected[col] += input[i] as f32
                                    * weights[i * cols + col] as i8 as f32 * scale as f32;
                            }
                        }
                        let expected: Vec<_> = expected.iter().map(|value|
                            if profile == "compute" && value.is_nan() {
                                f64::from_bits(0x7ff8000000000000)
                            } else { *value as f64 }).collect();
                        for ptr in [0, 65, 69, 512] {
                            let mut image = vec![7_u8; 2048];
                            image[0] = b'w';
                            for (index, value) in input.iter().enumerate() {
                                image[65 + index * 8..73 + index * 8]
                                    .copy_from_slice(&value.to_le_bytes());
                            }
                            memory.write(&mut runtime.store, 0, &image).unwrap();
                            let before = runtime.store.get_fuel().unwrap();
                            assert_eq!(linear.call(&mut runtime.store,
                                (ptr as i32, 65, 0, 1, rows as i32, cols as i32,
                                    scale.to_bits() as i64)).unwrap(), (expected.len() * 8) as i32);
                            assert_eq!(before - runtime.store.get_fuel().unwrap(),
                                (rows * cols + rows * 8 + 133) as u64);
                            let actual = memory.data(&runtime.store);
                            let end = ptr + expected.len() * 8;
                            assert_eq!(&actual[..ptr], &image[..ptr]);
                            assert_eq!(&actual[end..image.len()], &image[end..]);
                            for (cell, expected) in actual[ptr..end].chunks_exact(8).zip(&expected) {
                                let bits = u64::from_le_bytes(cell.try_into().unwrap());
                                if profile != "compute" && expected.is_nan() {
                                    assert!(f64::from_bits(bits).is_nan());
                                } else {
                                    assert_eq!(bits, expected.to_bits(),
                                        "linear result differs rows = {rows} cols = {cols}");
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn linear_bits() {
        let _guard = cache_test();
        let cases = [
            (vec![16777216.0_f64, -16777216.0, 1.0], 1_f64.to_bits()),
            (vec![16777216.0_f64, 1.0, -16777216.0], 0),
            (vec![f64::MAX], f64::INFINITY.to_bits()),
            (vec![f32::from_bits(1) as f64], (f32::from_bits(1) as f64).to_bits()),
        ];
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "linear").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            for (input, expected) in cases.iter() {
                runtime.store.data_mut().storage = Arc::new(Storage::from([
                    ("w".to_owned(), Arc::new(vec![1; input.len()].into())),
                ]));
                for (index, value) in input.iter().enumerate() {
                    memory.write(&mut runtime.store, 65 + index * 8, &value.to_le_bytes()).unwrap();
                }
                assert_eq!(linear.call(&mut runtime.store,
                    (1024, 65, 0, 1, input.len() as i32, 1, 1_f64.to_bits() as i64)).unwrap(), 8);
                assert_eq!(&memory.data(&runtime.store)[1024..1032], &expected.to_le_bytes());
            }
        }
    }

    #[test]
    fn linear_errors() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "linear").unwrap();
            let image = vec![7_u8; 128];
            memory.write(&mut runtime.store, 0, &image).unwrap();
            assert_eq!(linear.call(&mut runtime.store,
                (-1, 64, 0, 1, 1, 4, 1_f64.to_bits() as i64)).unwrap(), -3);
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("\u{7}".to_owned(), Arc::new(vec![1; 3].into())),
            ]));
            assert_eq!(linear.call(&mut runtime.store,
                (-1, 64, 0, 1, 1, 4, 1_f64.to_bits() as i64)).unwrap(), -1);
            assert_eq!(linear.call(&mut runtime.store,
                (-1, -1, 0, 1, 1, 4, 1_f64.to_bits() as i64))
                .unwrap_err().to_string(), "negative pointer");
            memory.write(&mut runtime.store, 0, &[255]).unwrap();
            assert_eq!(linear.call(&mut runtime.store,
                (-1, 64, 0, 1, 1, 4, 1_f64.to_bits() as i64))
                .unwrap_err().to_string(), "invalid guest utf8");
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![1; 4].into())),
            ]));
            runtime.store.data_mut().max_guest_bytes = 31;
            assert_eq!(linear.call(&mut runtime.store,
                (-1, 64, 0, 1, 1, 4, 1_f64.to_bits() as i64))
                .unwrap_err().to_string(), "tensor output exceeds cap");
            runtime.store.data_mut().max_guest_bytes = 32;
            for ptr in [-1, memory.data_size(&runtime.store) as i32 - 8] {
                assert_eq!(linear.call(&mut runtime.store,
                    (ptr, 64, 0, 1, 1, 4, 1_f64.to_bits() as i64)).unwrap(), -2);
            }
            assert_eq!(&memory.data(&runtime.store)[1..128], &image[1..]);
            assert_eq!(linear.call(&mut runtime.store,
                (256, 64, 0, 1, 1, 4, 1_f64.to_bits() as i64)).unwrap(), 32);
            runtime.store.set_fuel(16).unwrap();
            assert_eq!(linear.call(&mut runtime.store,
                (256, 64, 0, 1, 1, 32, 1_f64.to_bits() as i64))
                .unwrap_err().to_string(), "wasm fuel exhausted");
        }
    }

    #[test]
    fn linear_nan() {
        let _guard = cache_test();
        let words = [0, 0x8000000000000000, 0x3ff0000000000000, 0xbff0000000000000,
            0x7ff0000000000000, 0xfff0000000000000, 0x7ff0000000000042,
            0xfff0000000000042, 0x7ff812345678abcd, 0xfff8fedcba987654];
        let weights = vec![1_u8, 2, 3, 255];
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "linear").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(weights.clone().into())),
            ]));
            let mut digest = Sha256::new();
            for left in words {
                for right in words {
                    for scale in words {
                        let input = [f64::from_bits(left) as f32, f64::from_bits(right) as f32];
                        let mut expected = [0_f32; 2];
                        for i in 0..2 {
                            for col in 0..2 {
                                expected[col] += input[i] * weights[i * 2 + col] as i8 as f32
                                    * f64::from_bits(scale) as f32;
                            }
                        }
                        let expected: Vec<_> = expected.iter().map(|value|
                            if profile == "compute" && value.is_nan() {
                                f64::from_bits(0x7ff8000000000000)
                            } else { *value as f64 }).collect();
                        memory.write(&mut runtime.store, 65, &left.to_le_bytes()).unwrap();
                        memory.write(&mut runtime.store, 73, &right.to_le_bytes()).unwrap();
                        assert_eq!(linear.call(&mut runtime.store,
                            (256, 65, 0, 1, 2, 2, scale as i64)).unwrap(), 16);
                        let actual = &memory.data(&runtime.store)[256..272];
                        for (cell, expected) in actual.chunks_exact(8).zip(&expected) {
                            let bits = u64::from_le_bytes(cell.try_into().unwrap());
                            if profile != "compute" && expected.is_nan() {
                                assert!(f64::from_bits(bits).is_nan());
                            } else {
                                assert_eq!(bits, expected.to_bits(), "linear NaN bytes differ");
                            }
                        }
                        digest.update(actual);
                    }
                }
            }
            println!("event = linear_nan profile = {profile} digest = {digest:x}",
                digest = digest.finalize());
        }
    }

    #[test]
    fn load_memory() {
        let _guard = cache_test();
        let count = 262_145;
        let size = count * 8;
        for (profile, fixed) in [("standard", false), ("manifest", false),
            ("compute", false), ("compute", true)] {
            let mut runtime = tensor_modes(profile, false, fixed, false);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let load = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "load").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![1; count].into())),
            ]));
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
            for ptr in [-1, end as i32 - 8, 2048] {
                let before = runtime.store.get_fuel().unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(size, || {
                    load.call(&mut runtime.store,
                        (ptr, 0, 1, 0, count as i32, 1_f64.to_bits() as i64))
                });
                assert_eq!(result.unwrap(), if ptr == 2048 { size as i32 } else { -2 });
                let work = if fixed { count.div_ceil(64) } else { count };
                assert_eq!(before - runtime.store.get_fuel().unwrap(), work as u64 + 68);
                assert_eq!(copies, 0, "i8 load allocated a result buffer");
                assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
            }
            runtime.store.data_mut().max_guest_bytes = 16;
            let (result, copies) = native_bytes::tests::large_allocs(size, || {
                load.call(&mut runtime.store,
                    (2048, 0, 1, 0, count as i32, 1_f64.to_bits() as i64))
            });
            let reason = if fixed { "tensor output exceeds cap" }
                else { "guest write exceeds cap" };
            assert_eq!(result.unwrap_err().to_string(), reason);
            assert_eq!(copies, 0, "oversized i8 load allocated a result buffer");
        }
    }

    #[test]
    fn load_values() {
        let _guard = cache_test();
        let words = [0_u64, 1, 0x8000000000000000, 0x3ff0000000000000,
            0x3fe0000000000000, 0xbff0000000000000, 0x4250000000000000,
            0x7ff0000000000000, 0xfff0000000000000, 0x7ff0000000000042,
            0x7ff812345678abcd, 0xfff8fedcba987654];
        let weights: Vec<_> = (0..=255).collect();
        for (profile, fixed) in [("standard", false), ("manifest", false),
            ("compute", false), ("compute", true)] {
            let mut runtime = tensor_modes(profile, false, fixed, false);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let load = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "load").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(weights.clone().into())),
            ]));
            let mut digest = Sha256::new();
            for scale in words {
                for (offset, count) in [(0, 256), (1, 255), (127, 2), (255, 1)] {
                    let input = &weights[offset..offset + count];
                    let expected = if fixed {
                        deterministic_tensor::scale_bits_to_q24(scale as i64)
                            .and_then(|scale| deterministic_tensor::load_i8(input, scale))
                            .map(|values| values.into_iter().flat_map(i64::to_le_bytes).collect())
                    } else {
                        Ok(input.iter().flat_map(|byte| {
                            float_ops::bytes((*byte as i8 as f64) * f64::from_bits(scale),
                                profile == "compute")
                        }).collect::<Vec<_>>())
                    };
                    for ptr in [0, 65, 4096] {
                        memory.write(&mut runtime.store, 0, b"w").unwrap();
                        runtime.store.data_mut().compute_trace = (profile == "compute")
                            .then(|| ComputeTrace::new("load", None, None));
                        let result = load.call(&mut runtime.store,
                            (ptr, 0, 1, offset as i32, count as i32, scale as i64));
                        match &expected {
                            Ok(bytes) => {
                                assert_eq!(result.unwrap(), bytes.len() as i32);
                                assert_eq!(&memory.data(&runtime.store)[ptr as usize..ptr as usize + bytes.len()], bytes);
                                digest.update(bytes);
                                if fixed {
                                    let mut trace = ComputeTrace::new("load", None, None);
                                    trace.record("tensor_load_i8_q24", &[
                                        &Sha256::digest(b"w"), &(offset as i32).to_le_bytes(),
                                        &(count as i32).to_le_bytes(),
                                        &deterministic_tensor::scale_bits_to_q24(scale as i64).unwrap().to_le_bytes(),
                                        &Sha256::digest(bytes),
                                    ]);
                                    assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap()
                                        .finish(0, &[], 0), trace.finish(0, &[], 0));
                                }
                            }
                            Err(reason) => {
                                assert_eq!(result.unwrap_err().to_string(), *reason);
                                assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
                                digest.update(reason.as_bytes());
                            }
                        }
                    }
                }
            }
            let digest = hex::encode(digest.finalize());
            if fixed {
                assert_eq!(digest, "45005c93b0e4a60abb5aa804399ca0027c8eac40228c72c1ec81dcc00a6e1bd5");
            }
            println!("event = load_values profile = {profile} fixed = {fixed} digest = {digest}");
        }
    }

    #[test]
    fn load_errors() {
        let _guard = cache_test();
        for (profile, fixed) in [("standard", false), ("manifest", false),
            ("compute", false), ("compute", true)] {
            let mut runtime = tensor_modes(profile, false, fixed, false);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let load = runtime.instance
                .get_typed_func::<(i32, i32, i32, i32, i32, i64), i32>(
                    &runtime.store, "load").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            memory.write(&mut runtime.store, 64, &[7; 32]).unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![0, 1, 127].into())),
            ]));
            let scale = 1_f64.to_bits() as i64;
            for (offset, count) in [(-1, 1), (0, 0), (0, -1), (i32::MAX, 1), (0, 4)] {
                assert_eq!(load.call(&mut runtime.store, (-1, 0, 1, offset, count, scale)).unwrap(), -1);
            }
            assert_eq!(load.call(&mut runtime.store, (-1, -1, 1, 0, 1, scale))
                .unwrap_err().to_string(), "negative pointer");
            memory.write(&mut runtime.store, 0, &[255]).unwrap();
            assert_eq!(load.call(&mut runtime.store, (-1, 0, 1, 0, 1, scale))
                .unwrap_err().to_string(), "invalid guest utf8");
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().max_guest_bytes = 16;
            if fixed {
                assert_eq!(load.call(&mut runtime.store,
                    (-1, 0, 1, 0, 3, (2_f64.powi(38)).to_bits() as i64))
                    .unwrap_err().to_string(), "fixed point overflow");
                assert_eq!(&memory.data(&runtime.store)[64..96], &[7; 32]);
                assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
                runtime.store.data_mut().storage = Arc::new(Storage::from([
                    ("w".to_owned(), Arc::new(vec![0, 1, 254].into())),
                ]));
                assert_eq!(load.call(&mut runtime.store,
                    (-1, 0, 1, 0, 3, (2_f64.powi(38)).to_bits() as i64))
                    .unwrap_err().to_string(), "tensor output exceeds cap");
                runtime.store.data_mut().max_guest_bytes = 24;
                assert_eq!(load.call(&mut runtime.store,
                    (64, 0, 1, 0, 3, (2_f64.powi(38)).to_bits() as i64)).unwrap(), 24);
                let expected: Vec<_> = [0_i64, 1_i64 << 62, i64::MIN].into_iter()
                    .flat_map(i64::to_le_bytes).collect();
                assert_eq!(&memory.data(&runtime.store)[64..88], expected);
                runtime.store.data_mut().max_guest_bytes = 16;
                assert_eq!(load.call(&mut runtime.store, (-1, 0, 1, 0, 3, scale))
                    .unwrap_err().to_string(), "tensor output exceeds cap");
            } else {
                assert_eq!(load.call(&mut runtime.store, (-1, 0, 1, 0, 3, scale)).unwrap(), -2);
            }
            assert_eq!(load.call(&mut runtime.store, (64, 0, 1, 0, 2, scale)).unwrap(), 16);
            runtime.store.data_mut().max_guest_bytes = 15;
            let reason = if fixed { "tensor output exceeds cap" }
                else { "guest write exceeds cap" };
            assert_eq!(load.call(&mut runtime.store, (64, 0, 1, 0, 2, scale))
                .unwrap_err().to_string(), reason);
            let saved = memory.data(&runtime.store)[64..96].to_vec();
            runtime.store.set_fuel(16).unwrap();
            assert_eq!(load.call(&mut runtime.store, (64, 0, 1, 0, 2, scale))
                .unwrap_err().to_string(), "wasm fuel exhausted");
            assert_eq!(&memory.data(&runtime.store)[64..96], saved);
            assert_eq!(runtime.store.get_fuel().unwrap(), 0);
            runtime.store.set_fuel(20_000_000).unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::default());
            assert_eq!(load.call(&mut runtime.store, (-1, 0, 1, 0, 1, scale)).unwrap(), -3);
        }
    }

    #[test]
    fn tensor_input_quota() {
        let _guard = cache_test();
        let size = 4096;
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_modes(profile, false, profile == "compute", true);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let resident = compute_memory().used();
            let args = if profile == "compute" { (64, 65, size / 8, 0) }
                else { (65, 0, 64, size / 8) };
            let end = size as usize + 65;
            let before = memory.data(&runtime.store)[64..end].to_vec();
            for slots in 0..if profile == "compute" { 2 } else { 1 } {
                let room = slots * size as usize;
                let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident - room).unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(size as usize, || {
                    step.call(&mut runtime.store, args)
                });
                assert!(result.is_err(), "tensor input bypassed the shared memory quota");
                assert_eq!(result.unwrap_err().to_string(), "host unavailable");
                assert_eq!(runtime.store.data().unavailable.as_deref(), Some("compute memory busy"));
                assert_eq!(copies, slots, "tensor input allocated before reservation");
                assert_eq!(compute_memory().used(), resident + held.size());
                assert_eq!(&memory.data(&runtime.store)[64..end], before);
                drop(held);
                runtime.store.data_mut().unavailable = None;
            }
            assert_eq!(step.call(&mut runtime.store, args).unwrap(), size);
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn tensor_input_failure() {
        let _guard = cache_test();
        let size = 4096;
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_modes(profile, false, profile == "compute", true);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let args = if profile == "compute" { (64, 65, size / 8, 0) }
                else { (65, 0, 64, size / 8) };
            let resident = compute_memory().used();
            let before = memory.data(&runtime.store)[64..size as usize + 65].to_vec();
            for skip in 0..if profile == "compute" { 2 } else { 1 } {
                let result = native_bytes::tests::refused_alloc_after(size as usize, skip, || {
                    step.call(&mut runtime.store, args)
                });
                assert_eq!(result.unwrap_err().to_string(), "host unavailable");
                assert_eq!(runtime.store.data().unavailable.as_deref(),
                    Some("tensor input allocation failed"));
                assert_eq!(&memory.data(&runtime.store)[64..size as usize + 65], before);
                assert_eq!(compute_memory().used(), resident);
                runtime.store.data_mut().unavailable = None;
            }
        }
    }

    #[test]
    fn tensor_input_lifetime() {
        let _guard = cache_test();
        let size = 4096;
        for profile in ["standard", "manifest", "compute"] {
            let resident = compute_memory().used();
            let (_, freed) = native_bytes::tests::paused_alloc(size, 8, move || {
                let mut runtime = tensor_modes(profile, false, profile == "compute", true);
                let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                    &runtime.store, "step").unwrap();
                let args = if profile == "compute" { (64, 65, size as i32 / 8, 0) }
                    else { (65, 0, 64, size as i32 / 8) };
                assert_eq!(step.call(&mut runtime.store, args).unwrap(), size as i32);
            }, |_| {
                assert_eq!(compute_memory().used(), resident + 257 * PAGE_BYTES + size);
            });
            assert!(freed);
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn tensor_input_fp32() {
        let _guard = cache_test();
        let count = 1024;
        let size = count * 4;
        for profile in ["standard", "manifest", "compute"] {
            let mut runtime = tensor_modes(profile, false, false, true);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32, i64), i32>(
                &runtime.store, "linear").unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![1; count].into())),
            ]));
            let resident = compute_memory().used();
            for room in [0, size] {
                let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident - room).unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(size, || {
                    linear.call(&mut runtime.store, (64, 64, 0, 1, count as i32, 1, 1_f64.to_bits() as i64))
                });
                if room == 0 {
                    assert_eq!(result.unwrap_err().to_string(), "host unavailable");
                    assert_eq!(copies, 0);
                } else {
                    assert_eq!(result.unwrap(), 8);
                    assert_eq!(copies, 1, "FP32 input retained a second full snapshot");
                }
                assert_eq!(compute_memory().used(), resident + held.size());
                drop(held);
                runtime.store.data_mut().unavailable = None;
            }
        }
    }

    fn linear_result(count: usize) -> Runtime {
        let one = deterministic_tensor::ONE;
        let mut runtime = tensor_program("compute", true);
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        memory.write(&mut runtime.store, 0, b"w").unwrap();
        memory.write(&mut runtime.store, 65, &one.to_le_bytes()).unwrap();
        runtime.store.data_mut().storage = Arc::new(Storage::from([
            ("w".to_owned(), Arc::new(vec![1; count].into())),
        ]));
        runtime
    }

    #[test]
    fn tensor_result_quota() {
        let _guard = cache_test();
        for count in [512, 513] {
            let size = count * 8;
            let mut runtime = linear_result(count);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let linear = runtime.instance.get_typed_func::<
                (i32, i32, i32, i32, i32, i32, i64), i32>(
                &runtime.store, "linear").unwrap();
            let resident = compute_memory().used();
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident - 8).unwrap();
            let before = memory.data(&runtime.store)[2048..2048 + size].to_vec();
            let args = (2048, 65, 0, 1, 1, count as i32, 1_f64.to_bits() as i64);
            let (result, copies) = native_bytes::tests::large_allocs(size, || {
                linear.call(&mut runtime.store, args)
            });
            assert!(result.is_err(), "tensor result bypassed the shared memory quota");
            assert_eq!(result.unwrap_err().to_string(), "host unavailable");
            assert_eq!(copies, 0, "tensor result allocated before reservation");
            assert_eq!(&memory.data(&runtime.store)[2048..2048 + size], before);
            assert_eq!(compute_memory().used(), resident + held.size());
            drop(held);
            runtime.store.data_mut().unavailable = None;
            let result = native_bytes::tests::refused_alloc_after(size, 0, || {
                linear.call(&mut runtime.store, args)
            });
            assert_eq!(result.unwrap_err().to_string(), "host unavailable");
            assert_eq!(runtime.store.data().unavailable.as_deref(),
                Some("tensor result allocation failed"));
            assert_eq!(&memory.data(&runtime.store)[2048..2048 + size], before);
            assert_eq!(compute_memory().used(), resident);
            runtime.store.data_mut().unavailable = None;
            assert_eq!(linear.call(&mut runtime.store, args).unwrap(), size as i32);
            for chunk in memory.data(&runtime.store)[2048..2048 + size].chunks_exact(8) {
                assert_eq!(chunk, deterministic_tensor::ONE.to_le_bytes());
            }
            assert_eq!(compute_memory().used(), resident);
        }
    }

    #[test]
    fn tensor_result_lifetime() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        let size = 513 * 8;
        let (_, freed) = native_bytes::tests::paused_alloc(size, 8, move || {
            let value = tensor_result(true, 513, 0_i64, None).unwrap();
            assert_eq!(value.len(), 513);
            assert!(value.iter().all(|cell| *cell == 0));
            assert_eq!(compute_memory().used(), resident + size);
        }, |_| {
            assert_eq!(compute_memory().used(), resident + size);
        });
        assert!(freed);
        assert_eq!(compute_memory().used(), resident);
        assert!(matches!(tensor_result(true, usize::MAX, 0_i64, None),
            Err(HostFailure::Unavailable(_))));
        assert_eq!(compute_memory().used(), resident);
    }

    #[test]
    fn attention_result_quota() {
        let _guard = cache_test();
        let count = 513;
        let name = "result";
        for fixed in [false, true] {
            let mut runtime = cache_runtime(fixed, true, "result");
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let reset = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32), i32>(
                &runtime.store, "reset").unwrap();
            memory.write(&mut runtime.store, 0, name.as_bytes()).unwrap();
            assert_eq!(reset.call(&mut runtime.store, (0, name.len() as i32, 1, count, 1)).unwrap(), 0);
            let one = if fixed { deterministic_tensor::ONE } else { 1_f64.to_bits() as i64 };
            for index in 0..4 {
                memory.write(&mut runtime.store, 128 + index * 8, &one.to_le_bytes()).unwrap();
            }
            if fixed {
                let key = fixed_tensor_session_namespaced_key(runtime.store.data(), name);
                let mut cache = fixed_tensor_store(true).lock().unwrap();
                let entry = cache.get_mut(&key).unwrap();
                entry.k_cache.fill(one);
                entry.v_cache.fill(one);
            } else {
                let key = tensor_session_namespaced_key(runtime.store.data(), name);
                let mut cache = cache_guard(tensor_store(true));
                let entry = cache.get_mut(&key).unwrap();
                entry.k_cache.fill(1.0);
                entry.v_cache.fill(1.0);
            }
            let call = |runtime: &mut Runtime| {
                if fixed {
                    runtime.instance.get_typed_func::<
                        (i32, i32, i32, i32, i32, i32, i32, i32, i32, i64), i32>(
                        &runtime.store, "attention").unwrap().call(&mut runtime.store,
                            (0, name.len() as i32, 128, 256, count, 0, 4, 1, 1, one))
                } else {
                    runtime.instance.get_typed_func::<
                        (i32, i32, i32, i32, i32, i32, i32, i32, i32), i32>(
                        &runtime.store, "attention").unwrap().call(&mut runtime.store,
                            (0, name.len() as i32, 128, 256, count, 0, 4, 1, 1))
                }
            };
            let resident = compute_memory().used();
            let before = memory.data(&runtime.store)[256..288].to_vec();
            runtime.store.data_mut().compute_trace = Some(ComputeTrace::new("result", None, None));
            for room in [32, 64] {
                let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident - room).unwrap();
                let (result, copies) = native_bytes::tests::large_allocs(count as usize * 8,
                    || call(&mut runtime));
                assert_eq!(result.unwrap_err().to_string(), "host unavailable");
                assert_eq!(copies, 0);
                assert_eq!(&memory.data(&runtime.store)[256..288], before);
                assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap().operations, 0);
                assert_eq!(compute_memory().used(), resident + held.size());
                drop(held);
                runtime.store.data_mut().unavailable = None;
            }
            let result = native_bytes::tests::refused_alloc_after(count as usize * 8, 0,
                || call(&mut runtime));
            assert_eq!(result.unwrap_err().to_string(), "host unavailable");
            assert_eq!(runtime.store.data().unavailable.as_deref(),
                Some("tensor result allocation failed"));
            assert_eq!(&memory.data(&runtime.store)[256..288], before);
            assert_eq!(compute_memory().used(), resident);
            runtime.store.data_mut().unavailable = None;
            assert_eq!(call(&mut runtime).unwrap(), 32);
            for chunk in memory.data(&runtime.store)[256..288].chunks_exact(8) {
                if fixed {
                    assert_eq!(chunk, one.to_le_bytes());
                } else {
                    let actual = f64::from_le_bytes(chunk.try_into().unwrap());
                    assert!((actual - 1.0).abs() < 1e-12);
                }
            }
            assert_eq!(compute_memory().used(), resident);
            clear_compute_session_scope(runtime.store.data()).unwrap();
        }
    }

    #[test]
    fn tensor_session_float() {
        let _guard = cache_test();
        let mut runtime = cache_runtime(false, true, "fp");
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        memory.write(&mut runtime.store, 0, b"k").unwrap();
        let reset = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32), i32>(
            &runtime.store, "reset").unwrap();
        let append = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i32, i32), i32>(
            &runtime.store, "append").unwrap();
        let attention = runtime.instance.get_typed_func::<
            (i32, i32, i32, i32, i32, i32, i32, i32, i32), i32>(
            &runtime.store, "attention").unwrap();
        for bits in [0x7ff0000000000042_u64, 0xfff812345678abcd, 0x3ff0000000000000] {
            assert_eq!(reset.call(&mut runtime.store, (0, 1, 1, 1, 2)).unwrap(), 0);
            for (start, values) in [(128, [bits, 1_f64.to_bits()]),
                (256, [bits, 1_f64.to_bits()]), (512, [1_f64.to_bits(); 2])] {
                for (index, value) in values.iter().enumerate() {
                    memory.write(&mut runtime.store, start + index * 8, &value.to_le_bytes()).unwrap();
                }
            }
            assert_eq!(append.call(&mut runtime.store, (0, 1, 0, 0, 256, 512, 2)).unwrap(), 0);
            let key = tensor_session_namespaced_key(runtime.store.data(), "k");
            assert_eq!(cache_guard(tensor_store(true))[&key].k_cache[0].to_bits(), bits);
            assert_eq!(attention.call(&mut runtime.store, (0, 1, 128, 768, 1, 0, 1, 1, 2))
                .unwrap(), 16);
            let expected = if f64::from_bits(bits).is_nan() { float_ops::NAN_BITS }
                else { 1_f64.to_bits() };
            for cell in memory.data(&runtime.store)[768..784].chunks_exact(8) {
                assert_eq!(cell, expected.to_le_bytes());
            }
        }
        clear_compute_session_scope(runtime.store.data()).unwrap();
    }

    #[test]
    fn tensor_io_copy() {
        let _guard = cache_test();
        let size = 2 * 1024 * 1024;
        let count = size / 8;
        for profile in ["standard", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let before = runtime.store.get_fuel().unwrap();
            let args = if profile == "compute" { (64, size + 65, count, 0) }
                else { (64, 0, size + 65, count) };
            let (result, copies) = native_bytes::tests::large_allocs(size as usize, || {
                step.call(&mut runtime.store, args)
            });
            assert_eq!(result.unwrap(), size);
            let fuel = if profile == "compute" { 4_198_531 } else { 2_359_363 };
            assert_eq!(before - runtime.store.get_fuel().unwrap(), fuel);
            let expected = if profile == "compute" { 3 } else { 1 };
            assert_eq!(copies, expected, "tensor I/O copied an intermediate byte buffer");
        }
    }

    #[test]
    fn tensor_io_bits() {
        let _guard = cache_test();
        for profile in ["standard", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let words = [0_u64, 1, u64::MAX, 0x8000000000000000,
                0x7ff0000000000000, 0xfff0000000000000, 0x7ff0000000000001,
                0x7ff812345678abcd];
            let bytes: Vec<_> = words.into_iter().flat_map(u64::to_le_bytes).collect();
            let args = if profile == "compute" { (65, 1024, words.len() as i32, 0) }
                else { (69, 0, 65, words.len() as i32) };
            memory.write(&mut runtime.store, 65, &bytes).unwrap();
            let before = runtime.store.get_fuel().unwrap();
            assert_eq!(step.call(&mut runtime.store, args).unwrap(), bytes.len() as i32);
            let out = if profile == "compute" { 65 } else { 69 };
            assert_eq!(&memory.data(&runtime.store)[out..out + bytes.len()], bytes);
            let fuel = if profile == "compute" { 260 } else { 139 };
            assert_eq!(before - runtime.store.get_fuel().unwrap(), fuel);
            let saved = memory.data(&runtime.store)[64..144].to_vec();
            let args = if profile == "compute" { (65, 1024, 0, 0) } else { (69, 0, 65, 0) };
            assert_eq!(step.call(&mut runtime.store, args).unwrap(), -1);
            assert_eq!(&memory.data(&runtime.store)[64..144], saved);
        }
    }

    #[test]
    fn tensor_io_errors() {
        let _guard = cache_test();
        let size = 2 * 1024 * 1024;
        for profile in ["standard", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            for (ptr, count, reason) in [
                (-1, size / 8, "negative pointer"),
                (i32::MAX, size / 8, "guest memory read out of bounds"),
                (-1, i32::MAX, "tensor input exceeds cap"),
            ] {
                runtime.store.set_fuel(1_u64 << 40).unwrap();
                let args = if profile == "compute" { (ptr, 64, count, 0) }
                    else { (64, 0, ptr, count) };
                let (result, copies) = native_bytes::tests::large_allocs(size as usize, || {
                    step.call(&mut runtime.store, args)
                });
                assert_eq!(result.unwrap_err().to_string(), reason);
                assert_eq!(copies, 0, "invalid tensor input allocated a byte buffer");
                assert!(memory.data(&runtime.store)[64..128].iter().all(|byte| *byte == 0));
            }
            runtime.store.set_fuel(16).unwrap();
            let args = if profile == "compute" { (64, 128, 8, 0) } else { (64, 0, 128, 8) };
            assert_eq!(step.call(&mut runtime.store, args).unwrap_err().to_string(),
                "wasm fuel exhausted");
            assert_eq!(runtime.store.get_fuel().unwrap(), 0);
        }
    }

    #[test]
    fn tensor_io_shapes() {
        let _guard = cache_test();
        let mut runtime = tensor_runtime("standard");
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
            &runtime.store, "step").unwrap();
        let end = memory.data_size(&runtime.store);
        memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
        let before = runtime.store.get_fuel().unwrap();
        assert_eq!(step.call(&mut runtime.store, (end as i32 - 8, 0, 64, 2)).unwrap(), -2);
        assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
        assert_eq!(before - runtime.store.get_fuel().unwrap(), 85);
        assert_eq!(step.call(&mut runtime.store, (-1, 0, 64, 2)).unwrap(), -2);
        runtime.store.data_mut().max_guest_bytes = 16;
        assert_eq!(step.call(&mut runtime.store, (256, 0, 64, 2)).unwrap(), 16);
        assert_eq!(step.call(&mut runtime.store, (256, 0, 64, 3)).unwrap_err().to_string(),
            "tensor input exceeds cap");

        let mut runtime = tensor_runtime("compute");
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
            &runtime.store, "step").unwrap();
        let input: Vec<_> = [1_i64, 2, 3, 4].into_iter().flat_map(i64::to_le_bytes).collect();
        memory.write(&mut runtime.store, 65, &input).unwrap();
        assert_eq!(step.call(&mut runtime.store, (65, 73, 3, 0)).unwrap(), 24);
        let expected: Vec<_> = [3_i64, 5, 7].into_iter().flat_map(i64::to_le_bytes).collect();
        assert_eq!(&memory.data(&runtime.store)[65..89], expected);
        memory.write(&mut runtime.store, 65, &i64::MAX.to_le_bytes()).unwrap();
        memory.write(&mut runtime.store, 73, &1_i64.to_le_bytes()).unwrap();
        assert_eq!(step.call(&mut runtime.store, (65, 73, 1, 0)).unwrap_err().to_string(),
            "residual overflow");
        assert_eq!(&memory.data(&runtime.store)[65..73], &i64::MAX.to_le_bytes());
        runtime.store.data_mut().is_view = false;
        assert_eq!(step.call(&mut runtime.store, (65, 73, 1, 0)).unwrap_err().to_string(),
            "compute tensor import requires compute view");
    }

    #[test]
    fn tensor_io_refusal() {
        let _guard = cache_test();
        let size = 2 * 1024 * 1024;
        let count = size / 8;
        for profile in ["standard", "compute"] {
            let mut runtime = tensor_runtime(profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let step = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "step").unwrap();
            let load = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32, i64), i32>(
                &runtime.store, "load").unwrap();
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
            memory.write(&mut runtime.store, 0, b"w").unwrap();
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("w".to_owned(), Arc::new(vec![1; count].into())),
            ]));
            for ptr in [-1, end as i32 - 8] {
                let trace = runtime.store.data().compute_trace.as_ref().map(|trace| {
                    let mut expected = ComputeTrace {
                        hasher: trace.hasher.clone(), operations: trace.operations,
                    };
                    let mut output = Sha256::new();
                    for _ in 0..count {
                        output.update((1_i64 << 24).to_le_bytes());
                    }
                    expected.record("tensor_load_i8_q24", &[
                        &Sha256::digest(b"w"), &0_i32.to_le_bytes(),
                        &(count as i32).to_le_bytes(), &(1_i64 << 24).to_le_bytes(),
                        &output.finalize(),
                    ]);
                    expected
                });
                let (result, copies) = native_bytes::tests::large_allocs(size, || {
                    if profile == "compute" {
                        load.call(&mut runtime.store,
                            (ptr, 0, 1, 0, count as i32, 1_f64.to_bits() as i64))
                    } else {
                        step.call(&mut runtime.store, (ptr, 0, 64, count as i32))
                    }
                });
                assert_eq!(result.unwrap(), -2);
                assert_eq!(copies, usize::from(profile != "compute"),
                    "refused tensor output allocated a byte buffer");
                assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
                if let Some(trace) = trace {
                    assert_eq!(runtime.store.data().compute_trace.as_ref().unwrap()
                        .finish(-2, &[], 0), trace.finish(-2, &[], 0));
                }
            }
        }
    }

    fn state_request(method: &str, params: &[&str]) -> Vec<u8> {
        let mut bytes = REQUEST_MAGIC.to_vec();
        bytes.extend_from_slice(&(method.len() as u16).to_be_bytes());
        bytes.extend_from_slice(method.as_bytes());
        bytes.extend_from_slice(&(params.len() as u16).to_be_bytes());
        for param in params {
            bytes.push(4);
            bytes.extend_from_slice(&(param.len() as u32).to_be_bytes());
            bytes.extend_from_slice(param.as_bytes());
        }
        bytes
    }

    fn read_module(profile: &str, is_view: bool, policy: bool, session: bool) -> Payload<'static> {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        let names = if session {
            ["host_session_get", "host_session_put", "host_session_get_len"]
        } else {
            ["host_kv_get", "host_kv_put", "host_kv_get_len"]
        };
        for (name, ty) in [(names[0], 0), (names[1], 0), (names[2], 1), ("host_kv_del", 1)] {
            imports.import("octra", name, EntityType::Function(ty));
        }
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [2, 1, 0, 1, 1] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 257, maximum: Some(257), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        for (name, index) in [
            ("octra_alloc", 4), ("octra_query", 5), ("octra_update", 5),
            ("put", 6), ("len", 7), ("del", 8),
        ] {
            exports.export(name, ExportKind::Func, index);
        }
        module.section(&exports);
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        alloc.instruction(&I::I32Const(64)).instruction(&I::End);
        code.function(&alloc);
        let mut get = Function::new([]);
        for op in [
            I::I32Const(0), I::I32Const(1), I::LocalGet(0), I::LocalGet(1),
            I::Call(0), I::End,
        ] {
            get.instruction(&op);
        }
        code.function(&get);
        for (index, params) in [(1, 4), (2, 2), (3, 2)] {
            let mut function = Function::new([]);
            for param in 0..params {
                function.instruction(&I::LocalGet(param));
            }
            function.instruction(&I::Call(index)).instruction(&I::End);
            code.function(&function);
        }
        module.section(&code);
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(0), [b'k']);
        module.section(&data);
        Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "execution_profile": profile,
            "is_view": is_view, "update_policy": policy, "hfhe_strict": false,
            "fuel_limit": 20_000_000,
        })).unwrap()
    }

    #[test]
    fn guest_read_memory() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            let input = read_module(profile, false, false, false);
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let read = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            let cap = runtime.store.data().max_guest_bytes;
            let end = memory.data_size(&runtime.store);
            memory.write(&mut runtime.store, end - 16, &[7; 16]).unwrap();
            for size in [4 * 1024 * 1024, cap] {
                for ptr in [end as i32 - 8, i32::MAX] {
                    runtime.store.set_fuel(size as u64 + 1000).unwrap();
                    let before = runtime.store.get_fuel().unwrap();
                    let (result, copies) = native_bytes::tests::large_allocs(size, || {
                        read.call(&mut runtime.store, (ptr, size as i32))
                    });
                    assert_eq!(result.unwrap_err().to_string(), "guest memory read out of bounds");
                    assert_eq!(before - runtime.store.get_fuel().unwrap(), size as u64 + 67);
                    assert_eq!(copies, 0, "invalid guest read allocated a buffer");
                    assert_eq!(&memory.data(&runtime.store)[end - 16..], &[7; 16]);
                }
            }
        }
    }

    #[test]
    fn guest_read_values() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            let input = read_module(profile, false, false, false);
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "put").unwrap();
            let read = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            let end = memory.data_size(&runtime.store);
            for size in [0, 1, 7, 64, 257, 8193] {
                let ptr = end - size;
                let bytes: Vec<_> = (0..size).map(|index| index as u8).collect();
                memory.write(&mut runtime.store, ptr, &bytes).unwrap();
                let before = runtime.store.get_fuel().unwrap();
                assert_eq!(put.call(&mut runtime.store, (0, 1, ptr as i32, size as i32)).unwrap(), 0);
                assert_eq!(runtime.store.data().storage.get("k").unwrap().as_slice(), bytes);
                assert_eq!(&memory.data(&runtime.store)[ptr..], bytes);
                assert_eq!(before - runtime.store.get_fuel().unwrap(), size as u64 + 132);
            }
            let size = 4 * 1024 * 1024;
            memory.data_mut(&mut runtime.store)[64..64 + size].fill(b'x');
            let (result, copies) = native_bytes::tests::large_allocs(size, || {
                put.call(&mut runtime.store, (0, 1, 64, size as i32))
            });
            assert_eq!(result.unwrap(), 0);
            assert_eq!(copies, 1, "guest value copy count differs");
            let saved = runtime.store.data().storage.get("k").unwrap().clone();
            memory.data_mut(&mut runtime.store)[64..64 + size].fill(b'y');
            assert!(saved.iter().all(|byte| *byte == b'x'));
            let (result, copies) = native_bytes::tests::large_allocs(size, || {
                read.call(&mut runtime.store, (64, size as i32))
            });
            assert_eq!(result.unwrap(), -1);
            assert_eq!(copies, 1, "guest key copy count differs");
            memory.write(&mut runtime.store, 32, &[0xce, 0xbb]).unwrap();
            assert_eq!(put.call(&mut runtime.store, (32, 2, 0, 1)).unwrap(), 0);
            assert_eq!(read.call(&mut runtime.store, (32, 2)).unwrap(), 1);
            assert_eq!(read.call(&mut runtime.store, (end as i32, 0)).unwrap(), -1);
            assert_eq!(put.call(&mut runtime.store, (end as i32, 0, end as i32, 0)).unwrap(), 0);
            assert_eq!(read.call(&mut runtime.store, (end as i32, 0)).unwrap(), 0);
        }
    }

    #[test]
    fn guest_read_errors() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            let input = read_module(profile, false, false, false);
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let read = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "put").unwrap();
            let cap = runtime.store.data().max_guest_bytes as i32;
            let end = memory.data_size(&runtime.store) as i32;
            let cases = [
                (-1, cap + 1, "negative pointer", 3),
                (0, -1, "negative pointer", 3),
                (end, cap + 1, "guest read exceeds cap", 3),
                (end + 1, 0, "guest memory read out of bounds", 67),
                (end, 1, "guest memory read out of bounds", 68),
            ];
            for (ptr, len, reason, spent) in cases {
                runtime.store.set_fuel(1000).unwrap();
                assert_eq!(read.call(&mut runtime.store, (ptr, len)).unwrap_err().to_string(), reason);
                assert_eq!(1000 - runtime.store.get_fuel().unwrap(), spent);
            }
            memory.write(&mut runtime.store, 32, &[255]).unwrap();
            assert_eq!(read.call(&mut runtime.store, (32, 1))
                .unwrap_err().to_string(), "invalid guest utf8");
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("k".to_owned(), Arc::new(b"old".to_vec().into())),
            ]));
            let saved = runtime.store.data().storage.clone();
            assert_eq!(put.call(&mut runtime.store, (0, 1, end, 1))
                .unwrap_err().to_string(), "guest memory read out of bounds");
            assert!(Arc::ptr_eq(&saved, &runtime.store.data().storage));
            assert_eq!(runtime.store.data().storage.get("k").unwrap().as_slice(), b"old");
            runtime.store.set_fuel(66).unwrap();
            assert_eq!(read.call(&mut runtime.store, (end, 1))
                .unwrap_err().to_string(), "wasm fuel exhausted");
            assert_eq!(runtime.store.get_fuel().unwrap(), 0);
            runtime.store.set_fuel(1000).unwrap();
            let mut caller = Caller::from(&mut runtime.store);
            assert_eq!(read_guest_bytes(&mut caller, 0, 0)
                .unwrap_err().to_string(), "wasm_v1 memory export missing");
            assert_eq!(caller.get_fuel().unwrap(), 936);
        }
    }

    #[test]
    fn guest_read_modes() {
        let _guard = cache_test();
        for profile in ["standard", "manifest", "compute"] {
            for view in [false, true] {
                for policy in [false, true] {
                    let input = read_module(profile, view, policy, false);
                    let mut runtime = Runtime::instantiate(&input, false).unwrap();
                    let read = runtime.instance.get_typed_func::<(i32, i32), i32>(
                        &runtime.store, "len").unwrap();
                    let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                        &runtime.store, "put").unwrap();
                    let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
                    runtime.store.data_mut().storage = Arc::new(Storage::from([
                        ("k".to_owned(), Arc::new(b"old".to_vec().into())),
                    ]));
                    let saved = runtime.store.data().storage.clone();
                    let trace = runtime.store.data().compute_trace.as_ref().map(|trace| {
                        ComputeTrace { hasher: trace.hasher.clone(), operations: trace.operations }
                    });
                    let before = runtime.store.get_fuel().unwrap();
                    assert_eq!(read.call(&mut runtime.store, (0, 1)).unwrap(), 3);
                    let result = put.call(&mut runtime.store, (-1, -1, -1, -1));
                    if view {
                        assert_eq!(result.unwrap(), -1);
                    } else {
                        assert_eq!(result.unwrap_err().to_string(), "negative pointer");
                    }
                    let end = memory.data_size(&runtime.store) as i32;
                    assert_eq!(read.call(&mut runtime.store, (end, 1))
                        .unwrap_err().to_string(), "guest memory read out of bounds");
                    let spent = before - runtime.store.get_fuel().unwrap();
                    assert_eq!(spent, 139);
                    assert!(Arc::ptr_eq(&saved, &runtime.store.data().storage));
                    let actual = runtime.store.data().compute_trace.as_ref()
                        .map(|trace| trace.finish(-1, &[], spent));
                    assert_eq!(actual, trace.map(|trace| trace.finish(-1, &[], 139)));
                    println!("event = guest_read profile = {profile} view = {view} policy = {policy} trace = {}",
                        actual.as_deref().unwrap_or("none"));
                }
            }
        }
    }

    #[test]
    fn kv_write_sharing() {
        let _guard = cache_test();
        for profile in ["standard", "compute"] {
            for policy in [false, true] {
                let input = read_module(profile, false, policy, false);
                let mut runtime = Runtime::instantiate(&input, false).unwrap();
                let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                    &runtime.store, "put").unwrap();
                let del = runtime.instance.get_typed_func::<(i32, i32), i32>(
                    &runtime.store, "del").unwrap();
                let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
                let weights = Arc::new(native_bytes::Value::from(vec![7; 1024 * 1024]));
                runtime.store.data_mut().storage = Arc::new(Storage::from([
                    ("weights".to_owned(), weights.clone()),
                    ("k".to_owned(), Arc::new(b"old".to_vec().into())),
                ]));
                let reader = runtime.store.data().storage.clone();
                let root = compute_storage_root(&reader);
                memory.write(&mut runtime.store, 64, b"new").unwrap();
                let (_, allocations) = native_bytes::tests::large_allocs(512 * 1024, || {
                    assert_eq!(put.call(&mut runtime.store, (0, 1, 64, 3)).unwrap(), 0);
                });
                assert_eq!(allocations, 0, "kv write copied model bytes");
                assert!(!Arc::ptr_eq(&runtime.store.data().storage, &reader));
                assert!(Arc::ptr_eq(&runtime.store.data().storage["weights"], &weights));
                assert_eq!(runtime.store.data().storage["k"].as_slice(), b"new");
                assert_eq!(reader["k"].as_slice(), b"old");
                assert_eq!(compute_storage_root(&reader), root);
                let edited = runtime.store.data().storage.clone();
                let (_, allocations) = native_bytes::tests::large_allocs(512 * 1024, || {
                    assert_eq!(del.call(&mut runtime.store, (0, 1)).unwrap(), 0);
                });
                assert_eq!(allocations, 0, "kv delete copied model bytes");
                assert!(!runtime.store.data().storage.contains_key("k"));
                assert_eq!(edited["k"].as_slice(), b"new");
                assert!(Arc::ptr_eq(&runtime.store.data().storage["weights"], &weights));
                memory.write(&mut runtime.store, 0, b"weights").unwrap();
                assert_eq!(put.call(&mut runtime.store, (0, 7, 64, 3)).unwrap(), 0);
                assert_eq!(runtime.store.data().storage["weights"].as_slice(), b"new");
                assert!(weights.iter().all(|byte| *byte == 7));
                assert_eq!(compute_storage_root(&reader), root);
            }
        }
    }

    #[test]
    fn kv_read_memory() {
        let _guard = cache_test();
        for profile in ["standard", "compute"] {
            for is_view in [false, true] {
                for policy in [false, true] {
                    let input = read_module(profile, is_view, policy, false);
                    let mut runtime = Runtime::instantiate(&input, false).unwrap();
                    let get = runtime.instance.get_typed_func::<(i32, i32), i32>(
                        &runtime.store, "octra_query").unwrap();
                    let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
                    let size = 10 * 1024 * 1024;
                    let value = vec![7; size];
                    runtime.store.data_mut().storage = Arc::new(Storage::from([
                        ("k".to_owned(), Arc::new(value.into())),
                    ]));
                    let storage = runtime.store.data().storage.clone();
                    for (ptr, cap) in [
                        (64, -1), (64, 0), (64, size as i32 - 1),
                        (-1, size as i32), (i32::MAX, size as i32),
                        (memory.data_size(&runtime.store) as i32, size as i32),
                        (64, size as i32),
                    ] {
                        memory.write(&mut runtime.store, 64, &[3; 64]).unwrap();
                        let fuel = runtime.store.get_fuel().unwrap();
                        let (reply, count) = native_bytes::tests::large_allocs(size,
                            || get.call(&mut runtime.store, (ptr, cap)).unwrap());
                        let expected = if ptr == 64 && cap == size as i32 { cap } else { -2 };
                        assert_eq!(reply, expected, "read response changed");
                        assert_eq!(fuel - runtime.store.get_fuel().unwrap(), 68,
                            "read fuel changed");
                        assert_eq!(count, 0, "read copied stored value");
                        assert!(Arc::ptr_eq(&storage, &runtime.store.data().storage));
                        if reply == -2 {
                            assert_eq!(&memory.data(&runtime.store)[64..128], &[3; 64],
                                "refused read changed memory");
                            assert_eq!(memory.data(&runtime.store)[0], b'k');
                        }
                    }
                    assert_eq!(&memory.data(&runtime.store)[64..64 + size], storage["k"].as_slice());
                    let fuel = runtime.store.get_fuel().unwrap();
                    let (_, count) = native_bytes::tests::large_allocs(size, || {
                        for _ in 0..128 {
                            assert_eq!(get.call(&mut runtime.store, (64, 0)).unwrap(), -2);
                        }
                    });
                    assert_eq!(count, 0, "repeated refusal copied stored value");
                    assert_eq!(fuel - runtime.store.get_fuel().unwrap(), 128 * 68);
                    assert!(Arc::ptr_eq(&storage, &runtime.store.data().storage));
                }
            }
        }
    }

    #[test]
    fn kv_read_results() {
        let _guard = cache_test();
        for (profile, policy) in [
            ("standard", false), ("standard", true),
            ("compute", false), ("compute", true),
        ] {
            let input = read_module(profile, false, policy, false);
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let get = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "octra_update").unwrap();
            let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "put").unwrap();
            let len = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            let del = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "del").unwrap();
            assert_eq!(get.call(&mut runtime.store, (-1, -1)).unwrap(), -1);
            assert_eq!(len.call(&mut runtime.store, (0, 1)).unwrap(), -1);
            for value in [Vec::new(), b"abcd".to_vec(), vec![9; MAX_GUEST_READ_BYTES]] {
                memory.write(&mut runtime.store, 64, &value).unwrap();
                assert_eq!(put.call(&mut runtime.store, (0, 1, 64, value.len() as i32)).unwrap(), 0);
                assert_eq!(len.call(&mut runtime.store, (0, 1)).unwrap(), value.len() as i32);
                assert_eq!(get.call(&mut runtime.store, (-1, value.len() as i32)).unwrap(), -2);
                assert_eq!(get.call(&mut runtime.store, (64, -1)).unwrap(), -2);
                assert_eq!(get.call(&mut runtime.store, (64, value.len() as i32)).unwrap(), value.len() as i32);
                assert_eq!(&memory.data(&runtime.store)[64..64 + value.len()], &value);
                if value.is_empty() {
                    let end = memory.data_size(&runtime.store) as i32;
                    assert_eq!(get.call(&mut runtime.store, (end, 0)).unwrap(), 0);
                    assert_eq!(get.call(&mut runtime.store, (end + 1, 0)).unwrap(), -2);
                }
            }
            assert_eq!(get.call(&mut runtime.store, (0, MAX_GUEST_READ_BYTES as i32)).unwrap(),
                MAX_GUEST_READ_BYTES as i32);
            assert_eq!(&memory.data(&runtime.store)[..MAX_GUEST_READ_BYTES],
                runtime.store.data().storage["k"].as_slice());
            assert_eq!(get.call(&mut runtime.store, (64, 0)).unwrap(), -1);
            memory.write(&mut runtime.store, 0, b"k").unwrap();
            assert_eq!(del.call(&mut runtime.store, (0, 1)).unwrap(), 0);
            assert_eq!(get.call(&mut runtime.store, (64, 0)).unwrap(), -1);
            let limit = runtime.store.data().max_guest_bytes;
            runtime.store.data_mut().storage = Arc::new(Storage::from([
                ("k".to_owned(), Arc::new(vec![7; limit + 1].into())),
            ]));
            memory.write(&mut runtime.store, 64, &[3; 64]).unwrap();
            let (reply, count) = native_bytes::tests::large_allocs(limit,
                || get.call(&mut runtime.store, (64, limit as i32 + 1)));
            assert!(reply.unwrap_err().to_string().contains("guest write exceeds cap"));
            assert_eq!(count, 0, "oversized read copied stored value");
            assert_eq!(&memory.data(&runtime.store)[64..128], &[3; 64]);
            memory.write(&mut runtime.store, 0, &[255]).unwrap();
            assert!(get.call(&mut runtime.store, (64, 0)).unwrap_err().to_string()
                .contains("invalid guest utf8"));
            assert_eq!(&memory.data(&runtime.store)[64..128], &[3; 64]);
            memory.write(&mut runtime.store, 0, b"k").unwrap();
            runtime.store.set_fuel(64).unwrap();
            assert!(get.call(&mut runtime.store, (64, 0)).unwrap_err().to_string()
                .contains("wasm fuel exhausted"));
        }
    }

    #[test]
    fn job_order() {
        let _guard = cache_test();
        let base = compute_memory().used();
        let mut input = cache_input(true, true, "job-start", true);
        input.export_name = Some("octra_query".to_owned());
        input.request_b64 = Some(TextRef(Cow::Borrowed("!")));
        let expected = decode_b64("request_b64", "!").unwrap_err();
        assert!(matches!(run_execute(&input), Err(HostFailure::Rejected(ref reason))
            if reason == &expected));
        assert_eq!(compute_memory().used(), base);
        input.request_b64 = Some(TextRef(Cow::Borrowed("")));
        assert!(matches!(run_execute(&input), Err(HostFailure::Rejected(ref reason))
            if reason.contains("wasm start failed")));
        assert_eq!(compute_memory().used(), base);
    }

    #[test]
    fn job_layout() {
        let mut fields = vec![JsonValue::Null; 34];
        fields[14] = json!(false);
        fields[21] = json!(true);
        fields[26] = json!("compute");
        let old = Payload::deserialize(JsonValue::Array(fields.clone())).unwrap();
        assert_eq!(old.memory_bytes, None);
        assert_eq!(old.execution_profile.as_deref(), Some("compute"));
        assert!(HostState::from_payload(&old, false).unwrap().budget.is_some());
        fields.push(json!(65536));
        let current = Payload::deserialize(JsonValue::Array(fields)).unwrap();
        assert_eq!(current.memory_bytes, Some(65536));
        for value in [json!(-1), json!("1"), json!(true), json!([])] {
            assert!(Payload::deserialize(json!({"memory_bytes": value})).is_err());
        }
    }

    #[test]
    fn job_pages() {
        let _guard = cache_test();
        let base = compute_memory().used();
        let mut input = request_input(false, true, "".to_owned());
        for limit in [1, 65535] {
            input.memory_bytes = Some(limit);
            assert!(Runtime::instantiate(&input, false).is_err());
            assert_eq!(compute_memory().used(), base);
        }
        input.memory_bytes = Some(65536);
        let runtime = Runtime::instantiate(&input, false).unwrap();
        let quota = runtime.store.data().budget.clone().unwrap();
        assert_eq!(quota.used(), 65536);
        assert_eq!(compute_memory().used(), base + 65536);
        drop(runtime);
        assert_eq!(quota.used(), 0);
        assert_eq!(compute_memory().used(), base);
        for limit in [0, COMPUTE_STORAGE_RESIDENT_LIMIT + 1] {
            input.memory_bytes = Some(limit);
            assert!(matches!(HostState::from_payload(&input, false), Err(HostFailure::Rejected(_))));
        }
        input.memory_bytes = Some(65536);
        input.execution_profile = Some("standard".to_owned());
        assert!(matches!(HostState::from_payload(&input, false), Err(HostFailure::Rejected(_))));
        input.execution_profile = Some("compute".to_owned());
        input.is_view = Some(false);
        assert!(matches!(HostState::from_payload(&input, false), Err(HostFailure::Rejected(_))));
        assert_eq!(compute_memory().used(), base);
    }

    #[test]
    fn job_request() {
        let _guard = cache_test();
        let base = compute_memory().used();
        let mut input = request_input(false, true, encode_b64(&vec![7; 4097]));
        input.memory_bytes = Some(65536 + 4096);
        let result = prepare_execution(&input);
        assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
            if reason == "job memory limit exceeded"));
        assert_eq!(compute_memory().used(), base);
        input.memory_bytes = Some(65536 + 4098);
        let (runtime, request, _) = prepare_execution(&input).unwrap();
        let quota = runtime.store.data().budget.clone().unwrap();
        assert_eq!(quota.used(), 65536 + 4098);
        assert_eq!(request.as_slice(), &[7; 4097]);
        drop(runtime);
        assert_eq!(quota.used(), 4098);
        drop(request);
        assert_eq!(quota.used(), 0);
        assert_eq!(compute_memory().used(), base);
    }

    #[test]
    fn job_response() {
        let _guard = cache_test();
        let base = compute_memory().used();
        let mut input = response_input(false, true);
        input.memory_bytes = Some(65536 + 4096);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(4097,
            || write.call(&mut runtime.store, (0, 4097)));
        assert!(result.is_err());
        assert_eq!(copies, 0);
        assert!(runtime.store.data().response_bytes.is_empty());
        drop(runtime);
        assert_eq!(compute_memory().used(), base);
        input.memory_bytes = Some(65536 + 10000);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let quota = runtime.store.data().budget.clone().unwrap();
        let write = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "write").unwrap();
        write.call(&mut runtime.store, (0, 4097)).unwrap();
        assert_eq!(quota.used(), 65536 + 4097);
        assert!(write.call(&mut runtime.store, (0, 4097)).is_err());
        assert_eq!(runtime.store.data().response_bytes.len(), 4097);
        assert_eq!(quota.used(), 65536 + 4097);
        drop(runtime);
        assert_eq!((quota.used(), compute_memory().used()), (0, base));
    }

    #[test]
    fn job_numeric() {
        let _guard = cache_test();
        let base = compute_memory().used();
        let count = 513;
        let size = count * 8;
        let mut runtime = linear_result(count);
        let quota = runtime.store.data().budget.clone().unwrap();
        let used = quota.used();
        let resident = compute_memory().used();
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let linear = runtime.instance.get_typed_func::<
            (i32, i32, i32, i32, i32, i32, i64), i32>(&runtime.store, "linear").unwrap();
        let args = (2048, 65, 0, 1, 1, count as i32, 1_f64.to_bits() as i64);
        for free in [7, 8, size + 7, size + 8] {
            runtime.store.data_mut().unavailable = None;
            let held = quota.take(COMPUTE_STORAGE_RESIDENT_LIMIT - used - free).unwrap();
            let before = memory.data(&runtime.store)[2048..2048 + size].to_vec();
            let (result, copies) = native_bytes::tests::large_allocs(size,
                || linear.call(&mut runtime.store, args));
            if free < size + 8 {
                assert!(result.unwrap_err().to_string().contains("host unavailable"));
                assert_eq!(copies, 0);
                assert_eq!(&memory.data(&runtime.store)[2048..2048 + size], before);
            } else {
                assert_eq!(result.unwrap(), size as i32);
                assert_eq!(copies, 1);
                for chunk in memory.data(&runtime.store)[2048..2048 + size].chunks_exact(8) {
                    assert_eq!(chunk, deterministic_tensor::ONE.to_le_bytes());
                }
            }
            assert_eq!(quota.used(), used + held.size());
            assert_eq!(compute_memory().used(), resident);
        }
        drop(runtime);
        assert_eq!((quota.used(), compute_memory().used()), (0, base));
    }

    #[test]
    fn job_storage() {
        let _guard = cache_test();
        let base = compute_memory().used();
        let mut input = Payload::deserialize(json!({
            "execution_profile": "compute", "is_view": true, "hfhe_strict": false,
            "storage_cache_key": "job-storage",
            "storage_pairs": [{"key_b64": "aw==", "value_b64": encode_b64(&vec![7; 4097])}],
        })).unwrap();
        let first = HostState::from_payload(&input, false).unwrap();
        let used = compute_memory().used();
        let size = first.storage.uncounted(&native_bytes::Budget::new(8192)).unwrap();
        assert!(size >= 4097);
        input.storage_pairs = None;
        input.memory_bytes = Some(size - 1);
        assert!(matches!(HostState::from_payload(&input, false), Err(HostFailure::Unavailable(_))));
        assert_eq!(compute_memory().used(), used);
        input.memory_bytes = Some(size);
        let second = HostState::from_payload(&input, false).unwrap();
        assert!(Arc::ptr_eq(&first.storage, &second.storage));
        let quota = second.budget.clone().unwrap();
        assert_eq!(quota.used(), size);
        assert_eq!(compute_memory().used(), used);
        drop(second);
        assert_eq!(quota.used(), 0);
        cache_guard(storage_cache(true)).remove("job-storage");
        drop(first);
        assert_eq!(compute_memory().used(), base);
    }

    #[test]
    fn job_session() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let mut writer = session_runtime("compute", true);
        assert_eq!(session_set(&mut writer, b"k", 64, 4097).unwrap(), 0);
        let key = session_namespaced_key(writer.store.data(), "k");
        let mut input = read_module("compute", true, false, true);
        input.address = Some("cache".to_owned());
        input.caller = Some("reader".to_owned());
        input.memory_bytes = Some(257 * 65536 + 2048);
        let mut reader = Runtime::instantiate(&input, false).unwrap();
        let memory = reader.instance.get_memory(&reader.store, "memory").unwrap();
        let get = reader.instance.get_typed_func::<(i32, i32), i32>(
            &reader.store, "octra_query").unwrap();
        let result = get.call(&mut reader.store, (64, 4097));
        assert!(result.unwrap_err().to_string().contains("host unavailable"));
        assert_eq!(reader.store.data().unavailable.as_deref(), Some("compute memory busy"));
        assert_eq!(&memory.data(&reader.store)[64..64 + 4097], &[0; 4097]);
        assert_eq!(session_value(&key, true).unwrap().len(), 4097);
        drop(reader);
        input.memory_bytes = Some(257 * 65536 + 8192);
        let mut reader = Runtime::instantiate(&input, false).unwrap();
        let quota = reader.store.data().budget.clone().unwrap();
        let before = quota.used();
        let get = reader.instance.get_typed_func::<(i32, i32), i32>(
            &reader.store, "octra_query").unwrap();
        assert_eq!(get.call(&mut reader.store, (64, 4097)).unwrap(), 4097);
        assert_eq!(quota.used(), before);
        cache_guard(local_session_cache()).clear();
    }

    #[test]
    fn job_tensor() {
        let _guard = cache_test();
        for fixed in [false, true] {
            let mut input = cache_input(fixed, true, "job-tensor", false);
            let mut writer = Runtime::instantiate(&input, false).unwrap();
            assert_eq!(tensor_reset(&mut writer, b"kv", 2048), 0);
            let used = compute_memory().used();
            input.memory_bytes = Some(4 * 65536 + 4096);
            let mut probe = Runtime::instantiate(&input, false).unwrap();
            let memory = probe.instance.get_memory(&probe.store, "memory").unwrap();
            memory.write(&mut probe.store, 0, b"kv").unwrap();
            let result = if fixed {
                probe.instance.get_typed_func::<
                    (i32, i32, i32, i32, i32, i32, i32, i32, i32, i64), i32>(
                    &probe.store, "attention").unwrap()
                    .call(&mut probe.store, (0, 2, 64, 80, 1, 0, 1, 1, 1, deterministic_tensor::ONE))
            } else {
                probe.instance.get_typed_func::<
                    (i32, i32, i32, i32, i32, i32, i32, i32, i32), i32>(
                    &probe.store, "attention").unwrap()
                    .call(&mut probe.store, (0, 2, 64, 80, 1, 0, 1, 1, 1))
            };
            assert!(result.unwrap_err().to_string().contains("host unavailable"));
            assert_eq!(&memory.data(&probe.store)[80..88], &[0; 8]);
            drop(probe);
            let mut reader = Runtime::instantiate(&input, false).unwrap();
            assert_eq!(tensor_reset(&mut reader, b"other", 2048), -1);
            assert_eq!(tensor_has(&mut reader, b"other", 2048), 0);
            let memory = reader.instance.get_memory(&reader.store, "memory").unwrap();
            memory.write(&mut reader.store, 0, b"kv").unwrap();
            let append = reader.instance.get_typed_func::<
                (i32, i32, i32, i32, i32, i32, i32), i32>(&reader.store, "append").unwrap();
            assert!(append.call(&mut reader.store, (0, 2, 0, 0, 64, 72, 1))
                .unwrap_err().to_string().contains("host unavailable"));
            assert_eq!(tensor_has(&mut reader, b"kv", 2048), 1);
            drop(reader);
            assert_eq!(compute_memory().used(), used);
            input.memory_bytes = Some(4 * 65536 + 65536);
            let mut reader = Runtime::instantiate(&input, false).unwrap();
            let memory = reader.instance.get_memory(&reader.store, "memory").unwrap();
            memory.write(&mut reader.store, 0, b"kv").unwrap();
            let append = reader.instance.get_typed_func::<
                (i32, i32, i32, i32, i32, i32, i32), i32>(&reader.store, "append").unwrap();
            assert_eq!(append.call(&mut reader.store, (0, 2, 0, 0, 64, 72, 1)).unwrap(), 0);
            clear_compute_session_scope(writer.store.data()).unwrap();
        }
    }

    #[test]
    fn session_shared() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let baseline = compute_memory().used();
        for profile in ["standard", "compute"] {
            let mut runtime = session_runtime(profile, true);
            let used = compute_memory().used();
            let size = 4097;
            let full = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used - 2048).unwrap();
            let (reply, copies) = native_bytes::tests::large_allocs(size, || {
                session_set(&mut runtime, b"k", 64, size as i32)
            });
            assert_eq!(reply.unwrap(), -1, "session bypassed shared memory");
            assert_eq!(copies, 0, "refused session allocated its value");
            assert_eq!(local_session_pool().used(), 0);
            assert_eq!(compute_memory().used(), used + full.size());
            drop(full);
            assert_eq!(session_set(&mut runtime, b"k", 64, size as i32).unwrap(), 0);
            let bytes = local_session_pool().used();
            assert_eq!(compute_memory().used(), used + bytes);
            let key = session_namespaced_key(runtime.store.data(), "k");
            let reader = session_value(&key, true).unwrap();
            let full = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used - bytes).unwrap();
            assert_eq!(session_set(&mut runtime, b"k", 64, 1).unwrap(), -1);
            assert_eq!(session_value(&key, true).unwrap().len(), size);
            cache_guard(local_session_cache()).clear();
            assert_eq!(local_session_pool().used(), bytes);
            assert_eq!(compute_memory().used(), COMPUTE_STORAGE_RESIDENT_LIMIT);
            drop(full);
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline + bytes);
            drop(reader);
            assert_eq!((local_session_pool().used(), compute_memory().used()), (0, baseline));
            let mut runtime = session_runtime(profile, true);
            let used = compute_memory().used();
            let reply = native_bytes::tests::refused_alloc(size, || {
                session_set(&mut runtime, b"k", 64, size as i32)
            });
            assert!(reply.unwrap_err().to_string().contains("host unavailable"));
            assert_eq!(runtime.store.data().unavailable.as_deref(), Some("session allocation failed"));
            assert_eq!((local_session_pool().used(), compute_memory().used()), (0, used));
            drop(runtime);
            assert_eq!(compute_memory().used(), baseline);
        }
    }

    #[test]
    fn session_quota() {
        let _guard = cache_test();
        for profile in ["standard", "compute"] {
            cache_guard(local_session_cache()).clear();
            assert_eq!(local_session_pool().used(), 0);
            let mut runtime = session_runtime(profile, true);
            let size = MAX_GUEST_READ_BYTES;
            let count = LOCAL_SESSION_BYTES / size - 1;
            for index in 0..count {
                let key = format!("slot-{index}");
                assert_eq!(session_set(&mut runtime, key.as_bytes(), 64, size as i32).unwrap(), 0);
                assert!(local_session_pool().used() <= LOCAL_SESSION_BYTES);
            }
            let used = local_session_pool().used();
            assert!(used >= count * size, "session allocation accounting absent");
            for key in ["excess", "slot-0"] {
                let (result, copies) = native_bytes::tests::large_allocs(size, || {
                    session_set(&mut runtime, key.as_bytes(), 64, size as i32)
                });
                assert_eq!(result.unwrap(), -1, "session byte quota ignored");
                assert_eq!(copies, 0, "refused session value was copied");
                assert_eq!(local_session_pool().used(), used);
            }
            let key = session_namespaced_key(runtime.store.data(), "slot-0");
            assert_eq!(session_value(&key, true).unwrap().len(), size);
            assert_eq!(session_set(&mut runtime, b"slot-0", 64, 1).unwrap(), 0);
            assert_eq!(session_value(&key, true).unwrap().len(), 1);
            assert!(local_session_pool().used() < used);
            cache_guard(local_session_cache()).clear();
            assert_eq!(local_session_pool().used(), 0);
        }
    }

    #[test]
    fn replay_batch() {
        let _guard = cache_test();
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", "host_session_get_len", EntityType::Function(0));
        imports.import("octra", "host_session_put", EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(2);
        functions.function(0);
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType { minimum: 1, maximum: Some(1), memory64: false,
            shared: false, page_size_log2: None });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 2);
        exports.export("octra_query", ExportKind::Func, 3);
        module.section(&exports);
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        alloc.instruction(&I::I32Const(64)).instruction(&I::End);
        code.function(&alloc);
        let mut query = Function::new([]);
        for op in [I::I32Const(0), I::I32Const(1), I::Call(0), I::I32Const(-1),
            I::I32Eq, I::If(wasm_encoder::BlockType::Empty), I::I32Const(0),
            I::I32Const(1), I::I32Const(1), I::I32Const(1), I::Call(1), I::Drop,
            I::End, I::I32Const(0), I::End]
        { query.instruction(&op); }
        code.function(&query);
        module.section(&code);
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(0), *b"kx");
        module.section(&data);
        let step = json!({"action": "execute", "code_b64": encode_b64(&module.finish()),
            "execution_profile": "compute", "is_view": true, "caller": "reader",
            "address": "owned", "compute_session_scope": "10".repeat(32),
            "export_name": "octra_query", "request_b64": "", "fuel_limit": 10000,
            "hfhe_strict": true});
        let input = json!({"action": "compute_replay", "is_view": true,
            "execution_profile": "compute", "replay_steps": [step.to_string(), step.to_string()]});
        let encoded = input.to_string();
        let first = run_json(encoded.as_ptr(), encoded.len()).unwrap();
        let second = run_json(encoded.as_ptr(), encoded.len()).unwrap();
        assert_eq!(first, second);
        let results: JsonValue = serde_json::from_str(&first).unwrap();
        assert_eq!(results["replies"][0]["compute_exit"], 0);
        assert_eq!(results["replies"][1]["compute_exit"], 0);
        assert!(results["replies"][0]["compute_steps"].as_u64().unwrap()
            > results["replies"][1]["compute_steps"].as_u64().unwrap());
        let mut changed = step.clone();
        changed["caller"] = json!("other");
        let mut invalid = input.clone();
        invalid["replay_steps"][1] = json!(changed.to_string());
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Rejected(reason))
            if reason == "compute replay scope differs"));
        changed = step.clone();
        changed["fuel_limit"] = json!(10_000_001);
        invalid["replay_steps"] = json!([changed.to_string()]);
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Rejected(reason))
            if reason == "compute replay fuel limit"));
        changed = step.clone();
        changed["fuel_limit"] = json!(1);
        invalid["replay_steps"] = json!([step.to_string(), changed.to_string()]);
        let raw = invalid.to_string();
        let output = run_json(raw.as_ptr(), raw.len()).unwrap();
        let output: JsonValue = serde_json::from_str(&output).unwrap();
        assert_eq!(output["replies"][1]["compute_fault"], "fuel");
        invalid["replay_steps"] = json!([changed.to_string(), step.to_string()]);
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Rejected(reason))
            if reason == "compute replay prior call stopped"));
        invalid["replay_steps"] = json!(vec![step.to_string(); 65]);
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Rejected(reason))
            if reason == "compute replay input refused"));
        changed = step.clone();
        changed["code_b64"] = JsonValue::Null;
        invalid["replay_steps"] = json!([changed.to_string()]);
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Rejected(reason))
            if reason == "compute replay code missing"));
        changed = step.clone();
        changed["storage_cache_key"] = json!("shared");
        invalid["replay_steps"] = json!([changed.to_string()]);
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Rejected(reason))
            if reason == "compute replay scope differs"));
        let key = hex::encode(Sha256::digest(b"replay-model"));
        let storage = Arc::new(Storage::default());
        let root = compute_storage_root(&storage);
        compute_cache_call(CacheMessage::Seal(key.clone(), ComputeStorageEntry {
            storage, root: root.clone(), weight: 0, updated_at: 0.0,
        })).unwrap();
        changed = step.clone();
        changed["compute_storage_cache_key"] = json!(key);
        changed["compute_storage_root"] = json!(root);
        changed["compute_storage_entries"] = json!(0);
        changed["compute_storage_bytes"] = json!(0);
        invalid["replay_steps"] = json!([changed.to_string()]);
        let raw = invalid.to_string();
        assert!(run_json(raw.as_ptr(), raw.len()).is_ok());
        changed["compute_storage_root"] = json!("f".repeat(64));
        invalid["replay_steps"] = json!([changed.to_string()]);
        let raw = invalid.to_string();
        assert!(matches!(run_json(raw.as_ptr(), raw.len()), Err(HostFailure::Unavailable(reason))
            if reason == "compute storage cache identity differs"));
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn replay_code() {
        let _guard = cache_test();
        let key = "replay-code".to_owned();
        let mut other = cache_input(true, true, "replay-code", false);
        other.code_cache_key = Some(key.clone());
        let _other = Runtime::prepare(&other, false, false, None).unwrap();
        let mut input = read_module("compute", true, false, true);
        input.code_cache_key = Some(key);
        let mut runtime = Runtime::prepare_owned(&input, false, false, None,
            Some(Arc::new(SessionOwner::default()))).unwrap();
        assert_eq!(session_set(&mut runtime, b"k", 64, 1).unwrap(), 0);
    }

    #[test]
    fn replay_clock() {
        let _guard = cache_test();
        let input = cache_input(true, true, "replay-clock", false);
        let mut runtime = Runtime::prepare_owned(&input, false, false, None,
            Some(Arc::new(SessionOwner::default()))).unwrap();
        runtime.store.data_mut().compute_started_at = Instant::now() - Duration::from_secs(31);
        assert_eq!(tensor_reset(&mut runtime, b"a", 4), 0);
        let mut prior = Runtime::prepare(&input, false, false, None).unwrap();
        prior.store.data_mut().compute_started_at = Instant::now() - Duration::from_secs(31);
        let reset = prior.instance.get_typed_func::<(i32, i32, i32, i32, i32), i32>(
            &prior.store, "reset").unwrap();
        assert!(reset.call(&mut prior.store, (0, 1, 1, 4, 1)).is_err());
    }

    #[test]
    fn replay_memory() {
        let _guard = cache_test();
        let input = read_module("compute", true, false, true);
        let owner = Arc::new(SessionOwner::default());
        let mut runtime = Runtime::prepare_owned(&input, false, false, None, Some(owner)).unwrap();
        let used = compute_memory().used();
        let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
        assert!(session_set(&mut runtime, b"k", 64, 1).is_err());
        assert!(runtime.store.data().unavailable.is_some());
        drop(held);
        for fixed in [false, true] {
            let input = cache_input(fixed, true, "replay-memory", false);
            let mut runtime = Runtime::prepare_owned(&input, false, false, None,
                Some(Arc::new(SessionOwner::default()))).unwrap();
            let used = compute_memory().used();
            let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - used).unwrap();
            let reset = runtime.instance.get_typed_func::<(i32, i32, i32, i32, i32), i32>(
                &runtime.store, "reset").unwrap();
            assert!(reset.call(&mut runtime.store, (0, 1, 1, 4, 1)).is_err());
            assert!(runtime.store.data().unavailable.is_some());
            drop(held);
        }
    }

    #[test]
    fn replay_session_owner() {
        let _guard = cache_test();
        let mut input = read_module("compute", true, false, true);
        input.address = Some("owned".to_owned());
        input.caller = Some("reader".to_owned());
        input.compute_session_scope = Some("10".repeat(32));
        let owner = Arc::new(SessionOwner::default());
        let mut writer = Runtime::prepare_owned(&input, false, false, None, Some(owner.clone())).unwrap();
        let memory = writer.instance.get_memory(&writer.store, "memory").unwrap();
        memory.write(&mut writer.store, 64, b"paid").unwrap();
        assert_eq!(session_set(&mut writer, b"k", 64, 4).unwrap(), 0);
        for entry in cache_guard(&owner.values).values_mut() { entry.updated_at = 0.0; }
        cache_guard(local_session_cache()).clear();
        drop(writer);
        let mut reader = Runtime::prepare_owned(&input, false, false, None, Some(owner.clone())).unwrap();
        let len = reader.instance.get_typed_func::<(i32, i32), i32>(&reader.store, "len").unwrap();
        assert_eq!(len.call(&mut reader.store, (0, 1)).unwrap(), 4);
        let key = session_namespaced_key(reader.store.data(), "k");
        assert_eq!(cache_guard(&owner.values)[&key].value.as_slice(), b"paid");
        let mut other = Runtime::prepare_owned(&input, false, false, None,
            Some(Arc::new(SessionOwner::default()))).unwrap();
        let len = other.instance.get_typed_func::<(i32, i32), i32>(&other.store, "len").unwrap();
        assert_eq!(len.call(&mut other.store, (0, 1)).unwrap(), -1);
        clear_compute_session_scope(reader.store.data()).unwrap();
        assert!(cache_guard(&owner.values).is_empty());
    }

    #[test]
    fn replay_tensor_owner() {
        let _guard = cache_test();
        for fixed in [false, true] {
            let input = cache_input(fixed, true, "owned-tensor", false);
            let owner = Arc::new(SessionOwner::default());
            let mut writer = Runtime::prepare_owned(&input, false, false, None, Some(owner.clone())).unwrap();
            assert_eq!(tensor_reset(&mut writer, b"a", 4), 0);
            if fixed {
                for entry in cache_guard(&owner.fixed).values_mut() { entry.updated_at = 0.0; }
                cache_guard(fixed_tensor_store(true)).clear();
            } else {
                for entry in cache_guard(&owner.tensors).values_mut() { entry.updated_at = 0.0; }
                cache_guard(tensor_store(true)).clear();
            }
            drop(writer);
            let mut reader = Runtime::prepare_owned(&input, false, false, None, Some(owner.clone())).unwrap();
            assert_eq!(tensor_has(&mut reader, b"a", 4), 1);
            let mut other = Runtime::prepare_owned(&input, false, false, None,
                Some(Arc::new(SessionOwner::default()))).unwrap();
            assert_eq!(tensor_has(&mut other, b"a", 4), 0);
            clear_compute_session_scope(reader.store.data()).unwrap();
            assert_eq!(tensor_has(&mut reader, b"a", 4), 0);
        }
    }

    fn session_runtime(profile: &str, local: bool) -> Runtime {
        let mut input = read_module(profile, true, false, true);
        input.address = Some("cache".to_owned());
        input.caller = Some("reader".to_owned());
        input.inspect_policy = Some(local);
        Runtime::instantiate(&input, false).unwrap()
    }

    fn session_set(runtime: &mut Runtime, key: &[u8], ptr: i32, len: i32)
        -> Result<i32, WasmiError>
    {
        runtime.store.set_fuel(DEFAULT_FUEL_LIMIT).unwrap();
        runtime.instance.get_memory(&runtime.store, "memory").unwrap()
            .write(&mut runtime.store, 0, key).unwrap();
        runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
            &runtime.store, "put").unwrap()
            .call(&mut runtime.store, (0, key.len() as i32, ptr, len))
    }

    #[test]
    fn session_lifetime() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let mut runtime = session_runtime("standard", true);
        let size = 1024 * 1024;
        assert_eq!(session_set(&mut runtime, b"k", 64, size).unwrap(), 0);
        let key = session_namespaced_key(runtime.store.data(), "k");
        let saved = session_value(&key, true).unwrap();
        let used = local_session_pool().used();
        cache_guard(local_session_cache()).clear();
        assert_eq!(local_session_pool().used(), used, "reader lost its reservation");
        let full = local_session_pool().take(LOCAL_SESSION_BYTES - used).unwrap();
        let (result, copies) = native_bytes::tests::large_allocs(size as usize, || {
            session_set(&mut runtime, b"k", 64, size)
        });
        assert_eq!(result.unwrap(), -1);
        assert_eq!(copies, 0);
        assert_eq!(saved.len(), size as usize);
        drop(full);
        assert_eq!(local_session_pool().used(), used);
        assert_eq!(session_set(&mut runtime, b"k", 64, 1).unwrap(), 0);
        assert!(local_session_pool().used() > used);
        cache_guard(local_session_cache()).clear();
        assert_eq!(local_session_pool().used(), used);
        drop(saved);
        assert_eq!(local_session_pool().used(), 0);
    }

    #[test]
    fn session_keys() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let mut runtime = session_runtime("standard", true);
        let key = vec![b'x'; 1024 * 1024];
        assert_eq!(session_set(&mut runtime, &key, 0, 0).unwrap(), 0);
        assert!(local_session_pool().used() >= key.len(), "session key was not charged");
        cache_guard(local_session_cache()).clear();
        for index in 0..SESSION_CACHE_LIMIT {
            assert_eq!(session_set(&mut runtime, format!("{index}").as_bytes(), 0, 0).unwrap(), 0);
        }
        assert_eq!(session_set(&mut runtime, b"excess", 0, 0).unwrap(), -1);
        assert_eq!(session_set(&mut runtime, b"0", 0, 0).unwrap(), 0);
        assert_eq!(cache_guard(local_session_cache()).len(), SESSION_CACHE_LIMIT);
        let key = session_namespaced_key(runtime.store.data(), "0");
        cache_guard(local_session_cache()).get_mut(&key).unwrap().updated_at =
            now_secs() - SESSION_CACHE_TTL_SECS - 1.0;
        assert_eq!(session_set(&mut runtime, b"excess", 0, 0).unwrap(), 0);
        assert!(!cache_guard(local_session_cache()).contains_key(&key));
        cache_guard(local_session_cache()).clear();
        assert_eq!(local_session_pool().used(), 0);
    }

    #[test]
    fn session_isolation() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let mut legacy = session_runtime("standard", false);
        let mut local = session_runtime("standard", true);
        assert!(!legacy.store.data().local_sessions);
        assert!(local.store.data().local_sessions);
        let legacy_key = session_namespaced_key(legacy.store.data(), "k");
        assert_eq!(session_set(&mut legacy, b"k", 64, 3).unwrap(), 0);
        let used = local_session_pool().used();
        assert_eq!(session_set(&mut local, b"k", 64, 7).unwrap(), 0);
        assert!(local_session_pool().used() > used);
        assert_eq!(session_value(&legacy_key, false).unwrap().len(), 3);
        let local_key = session_namespaced_key(local.store.data(), "k");
        assert_eq!(session_value(&local_key, true).unwrap().len(), 7);
        cache_guard(local_session_cache()).clear();
        assert_eq!(session_value(&legacy_key, false).unwrap().len(), 3);
        cache_guard(session_cache()).remove(&legacy_key);
        local.store.data_mut().self_bytes = b"a|b".to_vec();
        local.store.data_mut().caller_bytes = b"c".to_vec();
        let first = session_namespaced_key(local.store.data(), "k");
        local.store.data_mut().self_bytes = b"a".to_vec();
        local.store.data_mut().caller_bytes = b"b|c".to_vec();
        assert_ne!(first, session_namespaced_key(local.store.data(), "k"));
        local.store.data_mut().compute_session_scope = Some("scope".to_owned());
        let legacy_key = legacy_session_key(local.store.data(), "k");
        cache_guard(session_cache()).insert(legacy_key.clone(), SessionCacheEntry {
            value: Arc::new(vec![1].into()), updated_at: now_secs(),
        });
        assert_eq!(session_set(&mut local, b"k", 64, 7).unwrap(), 0);
        clear_compute_session_scope(local.store.data()).unwrap();
        assert_eq!(local_session_pool().used(), 0);
        assert!(session_value(&legacy_key, false).is_some());
        cache_guard(session_cache()).remove(&legacy_key);
    }

    #[test]
    fn session_parallel() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let size = 1024 * 1024;
        let held = local_session_pool().take(LOCAL_SESSION_BYTES - size - 1024).unwrap();
        let ready = Arc::new(std::sync::Barrier::new(3));
        let workers: Vec<_> = (0..2).map(|index| {
            let ready = ready.clone();
            std::thread::spawn(move || {
                let mut runtime = session_runtime("standard", true);
                ready.wait();
                session_set(&mut runtime, format!("{index}").as_bytes(), 64, size as i32).unwrap()
            })
        }).collect();
        ready.wait();
        let mut results: Vec<_> = workers.into_iter().map(|worker| worker.join().unwrap()).collect();
        results.sort();
        assert_eq!(results, [-1, 0], "parallel session reservations exceeded quota");
        assert!(local_session_pool().used() <= LOCAL_SESSION_BYTES);
        cache_guard(local_session_cache()).clear();
        drop(held);
        assert_eq!(local_session_pool().used(), 0);
    }

    #[test]
    fn session_errors() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let mut runtime = session_runtime("standard", true);
        assert_eq!(session_set(&mut runtime, b"k", 64, 7).unwrap(), 0);
        let used = local_session_pool().used();
        for (ptr, len, reason) in [
            (-1, 1, "negative pointer"), (0, -1, "negative pointer"),
            (i32::MAX, 1024 * 1024, "guest memory read out of bounds"),
            (0, MAX_GUEST_READ_BYTES as i32 + 1, "guest read exceeds cap"),
        ] {
            let (result, copies) = native_bytes::tests::large_allocs(1024 * 1024, || {
                session_set(&mut runtime, b"k", ptr, len)
            });
            assert!(result.unwrap_err().to_string().contains(reason));
            assert_eq!(copies, 0);
            assert_eq!(local_session_pool().used(), used);
        }
        runtime.store.set_fuel(68).unwrap();
        let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
            &runtime.store, "put").unwrap();
        assert!(put.call(&mut runtime.store, (0, 1, 64, 7)).unwrap_err().to_string()
            .contains("wasm fuel exhausted"));
        assert_eq!(local_session_pool().used(), used);
        let key = session_namespaced_key(runtime.store.data(), "k");
        assert_eq!(session_value(&key, true).unwrap().len(), 7);
        cache_guard(local_session_cache()).clear();
        assert_eq!(local_session_pool().used(), 0);
    }

    #[test]
    fn session_start() {
        let _guard = cache_test();
        cache_guard(local_session_cache()).clear();
        let mut input = session_module(InspectStage::Startup);
        input["action"] = json!("execute");
        input["address"] = json!("start");
        input["caller"] = json!("reader");
        let input: Payload = Payload::deserialize(input).unwrap();
        let runtime = Runtime::instantiate(&input, false).unwrap();
        let key = session_namespaced_key(runtime.store.data(), "inspect_case");
        assert!(runtime.store.data().local_sessions);
        assert!(session_value(&key, true).is_some());
        assert!(session_value("start|reader|inspect_case", false).is_none());
        assert!(local_session_pool().used() > 0);
        cache_guard(local_session_cache()).clear();
        assert_eq!(local_session_pool().used(), 0);
    }

    #[test]
    fn session_read_memory() {
        let _guard = cache_test();
        for profile in ["standard", "compute"] {
            for is_view in [true, false] {
                for policy in [true, false] {
                    let mut input = read_module(profile, is_view, policy, true);
                    input.address = Some("session_read_memory".to_owned());
                    input.caller = Some("caller".to_owned());
                    input.compute_session_scope = (profile == "compute").then(|| "c".repeat(64));
                    let mut runtime = Runtime::instantiate(&input, false).unwrap();
                    let key = session_namespaced_key(runtime.store.data(), "k");
                    let cache = session_store(runtime.store.data().local_sessions);
                    let size = 10 * 1024 * 1024;
                    cache_guard(cache).insert(key.clone(), SessionCacheEntry {
                        value: Arc::new(vec![7; size].into()), updated_at: now_secs(),
                    });
                    let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
                    let get = runtime.instance.get_typed_func::<(i32, i32), i32>(
                        &runtime.store, "octra_query").unwrap();
                    memory.write(&mut runtime.store, 64, &[3; 64]).unwrap();
                    for (ptr, cap, expected) in [
                        (64, -1, -2), (64, 0, -2), (64, size as i32 - 1, -2),
                        (-1, size as i32, -2), (i32::MAX, size as i32, -2),
                        (64, size as i32, size as i32),
                    ] {
                        let fuel = runtime.store.get_fuel().unwrap();
                        let (reply, count) = native_bytes::tests::large_allocs(size,
                            || get.call(&mut runtime.store, (ptr, cap)).unwrap());
                        assert_eq!(reply, expected);
                        assert_eq!(fuel - runtime.store.get_fuel().unwrap(), 68);
                        assert_eq!(count, 0, "session read copied cached value");
                        if reply == -2 {
                            assert_eq!(&memory.data(&runtime.store)[64..128], &[3; 64]);
                            assert_eq!(memory.data(&runtime.store)[0], b'k');
                        }
                    }
                    assert!(memory.data(&runtime.store)[64..64 + size].iter()
                        .all(|byte| *byte == 7));
                    let fuel = runtime.store.get_fuel().unwrap();
                    let (_, count) = native_bytes::tests::large_allocs(size, || {
                        for _ in 0..128 {
                            assert_eq!(get.call(&mut runtime.store, (64, 0)).unwrap(), -2);
                        }
                    });
                    assert_eq!(count, 0, "repeated session read copied cached value");
                    assert_eq!(fuel - runtime.store.get_fuel().unwrap(), 128 * 68);
                    let entry = cache_guard(cache).remove(&key).unwrap();
                    assert!(entry.value.iter().all(|byte| *byte == 7));
                }
            }
        }
    }

    #[test]
    fn session_read_results() {
        let _guard = cache_test();
        for profile in ["standard", "compute"] {
            let mut input = read_module(profile, true, true, true);
            input.address = Some("session_read_results".to_owned());
            input.caller = Some("caller".to_owned());
            input.compute_session_scope = (profile == "compute").then(|| "d".repeat(64));
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let key = session_namespaced_key(runtime.store.data(), "k");
            let cache = session_store(runtime.store.data().local_sessions);
            cache_guard(cache).remove(&key);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let get = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "octra_query").unwrap();
            let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "put").unwrap();
            let len = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            assert_eq!(get.call(&mut runtime.store, (-1, -1)).unwrap(), -1);
            assert_eq!(len.call(&mut runtime.store, (0, 1)).unwrap(), -1);
            for value in [Vec::new(), b"abcd".to_vec(), vec![9; MAX_GUEST_READ_BYTES]] {
                memory.write(&mut runtime.store, 64, &value).unwrap();
                assert_eq!(put.call(&mut runtime.store, (0, 1, 64, value.len() as i32)).unwrap(), 0);
                assert_eq!(len.call(&mut runtime.store, (0, 1)).unwrap(), value.len() as i32);
                assert_eq!(get.call(&mut runtime.store, (-1, value.len() as i32)).unwrap(), -2);
                assert_eq!(get.call(&mut runtime.store, (64, -1)).unwrap(), -2);
                assert_eq!(get.call(&mut runtime.store, (64, value.len() as i32)).unwrap(), value.len() as i32);
                assert_eq!(&memory.data(&runtime.store)[64..64 + value.len()], &value);
                if value.is_empty() {
                    let end = memory.data_size(&runtime.store) as i32;
                    assert_eq!(get.call(&mut runtime.store, (end, 0)).unwrap(), 0);
                    assert_eq!(get.call(&mut runtime.store, (end + 1, 0)).unwrap(), -2);
                }
            }
            assert_eq!(get.call(&mut runtime.store, (0, MAX_GUEST_READ_BYTES as i32)).unwrap(),
                MAX_GUEST_READ_BYTES as i32);
            assert!(memory.data(&runtime.store)[..MAX_GUEST_READ_BYTES].iter()
                .all(|byte| *byte == 9));
            let saved = cache_guard(cache).get(&key).unwrap().value.clone();
            assert_eq!(get.call(&mut runtime.store, (64, 0)).unwrap(), -1);
            memory.write(&mut runtime.store, 0, b"k").unwrap();
            memory.write(&mut runtime.store, 64, b"e").unwrap();
            assert_eq!(put.call(&mut runtime.store, (0, 1, 64, 1)).unwrap(), 0);
            assert_eq!(get.call(&mut runtime.store, (64, 1)).unwrap(), 1);
            assert_eq!(memory.data(&runtime.store)[64], b'e');
            assert_eq!(saved.len(), MAX_GUEST_READ_BYTES);
            assert!(saved.iter().all(|byte| *byte == 9));
            assert_eq!(Arc::strong_count(&saved), 1);
            cache_guard(cache).get_mut(&key).unwrap().updated_at =
                now_secs() - SESSION_CACHE_TTL_SECS - 1.0;
            assert_eq!(get.call(&mut runtime.store, (-1, -1)).unwrap(), -1);
            assert!(!cache_guard(cache).contains_key(&key));
            let limit = runtime.store.data().max_guest_bytes;
            cache_guard(cache).insert(key.clone(), SessionCacheEntry {
                value: Arc::new(vec![7; limit + 1].into()), updated_at: now_secs(),
            });
            memory.write(&mut runtime.store, 64, &[3; 64]).unwrap();
            let (reply, count) = native_bytes::tests::large_allocs(limit,
                || get.call(&mut runtime.store, (64, limit as i32 + 1)));
            assert!(reply.unwrap_err().to_string().contains("guest write exceeds cap"));
            assert_eq!(count, 0, "oversized session read copied cached value");
            assert_eq!(&memory.data(&runtime.store)[64..128], &[3; 64]);
            memory.write(&mut runtime.store, 0, &[255]).unwrap();
            assert!(get.call(&mut runtime.store, (64, 0)).unwrap_err().to_string()
                .contains("invalid guest utf8"));
            assert_eq!(&memory.data(&runtime.store)[64..128], &[3; 64]);
            memory.write(&mut runtime.store, 0, b"k").unwrap();
            runtime.store.set_fuel(64).unwrap();
            assert!(get.call(&mut runtime.store, (64, 0)).unwrap_err().to_string()
                .contains("wasm fuel exhausted"));
            cache_guard(cache).remove(&key);
        }
    }

    #[test]
    fn session_read_snapshot() {
        let _guard = cache_test();
        let mut input = read_module("standard", true, false, true);
        input.address = Some("session_read_snapshot".to_owned());
        input.caller = Some("caller".to_owned());
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let key = session_namespaced_key(runtime.store.data(), "k");
        let size = 10 * 1024 * 1024;
        cache_guard(session_cache()).insert(key.clone(), SessionCacheEntry {
            value: Arc::new(vec![7; size].into()), updated_at: now_secs(),
        });
        let (saved, count) = native_bytes::tests::large_allocs(size,
            || session_value(&key, false).unwrap());
        assert_eq!(count, 0, "session snapshot copied cached value");
        assert_eq!(Arc::strong_count(&saved), 2);
        assert!(session_cache().try_lock().is_ok(), "session snapshot retained cache lock");
        std::thread::spawn(|| {
            let mut input = read_module("standard", true, false, true);
            input.address = Some("session_read_snapshot".to_owned());
            input.caller = Some("caller".to_owned());
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            memory.write(&mut runtime.store, 64, b"new").unwrap();
            let put = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "put").unwrap();
            assert_eq!(put.call(&mut runtime.store, (0, 1, 64, 3)).unwrap(), 0);
        }).join().unwrap();
        assert_eq!(Arc::strong_count(&saved), 1);
        assert!(saved.iter().all(|byte| *byte == 7));
        let current = session_value(&key, false).unwrap();
        assert_eq!(current.as_slice(), b"new");
        assert!(!Arc::ptr_eq(&saved, &current));
        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
        let get = runtime.instance.get_typed_func::<(i32, i32), i32>(
            &runtime.store, "octra_query").unwrap();
        assert_eq!(get.call(&mut runtime.store, (64, 3)).unwrap(), 3);
        assert_eq!(&memory.data(&runtime.store)[64..67], b"new");
        clear_compute_session_prefix("session_read_snapshot|caller|", None).unwrap();
        assert!(session_value(&key, false).is_none());
        assert_eq!(get.call(&mut runtime.store, (-1, -1)).unwrap(), -1);
        let (reply, count) = native_bytes::tests::large_allocs(size,
            || memory.write(&mut runtime.store, 64, saved.as_slice()));
        reply.unwrap();
        assert_eq!(count, 0);
        assert!(memory.data(&runtime.store)[64..64 + size].iter()
            .all(|byte| *byte == 7));
        assert_eq!(Arc::strong_count(&saved), 1);
        assert_eq!(Arc::strong_count(&current), 1);
    }

    fn invoke_input(hfhe: bool, profile: &str, view: bool, policy: bool, value: &str)
        -> (Payload<'static>, Vec<u8>) {
        let id = "a".repeat(64);
        let request = if hfhe {
            state_request("fhe_load_pk", &["owner"])
        } else {
            state_request("state_descriptor_get", &[&id, "state_class"])
        };
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        let names = if hfhe {
            ["host_hfhe_invoke", "host_hfhe_invoke_len"]
        } else {
            ["host_circle_invoke", "host_circle_invoke_len"]
        };
        imports.import("octra", names[0], EntityType::Function(0));
        imports.import("octra", names[1], EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        for ty in [2, 0, 1] {
            functions.function(ty);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 257, maximum: Some(257), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 2);
        exports.export("get", ExportKind::Func, 3);
        exports.export("len", ExportKind::Func, 4);
        exports.export("octra_query", ExportKind::Func, 4);
        module.section(&exports);
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        alloc.instruction(&I::I32Const(64)).instruction(&I::End);
        code.function(&alloc);
        for (index, params) in [(0, 4), (1, 2)] {
            let mut function = Function::new([]);
            for param in 0..params {
                function.instruction(&I::LocalGet(param));
            }
            function.instruction(&I::Call(index)).instruction(&I::End);
            code.function(&function);
        }
        module.section(&code);
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(0), request.iter().copied());
        module.section(&data);
        let mut input: Payload = Payload::deserialize(json!({
            "code_b64": encode_b64(&module.finish()), "execution_profile": profile,
            "is_view": view, "update_policy": policy, "hfhe_strict": false,
            "fuel_limit": 30_000_000,
        })).unwrap();
        if hfhe {
            input.hfhe_caps = Some(vec!["fhe_load_pk".to_owned()]);
            input.hfhe_pubkeys = Some(vec![HfhePubkeyJson {
                addr: "owner".to_owned(), pubkey_b64: value.to_owned(),
            }]);
        } else {
            input.storage_pairs = Some(vec![StoragePairJson {
                key_b64: encode_b64(state_descriptor_key(&id, "state_class").as_bytes()),
                value_b64: encode_b64(value.as_bytes()),
            }]);
        }
        (input, request)
    }

    #[test]
    fn invoke_read_memory() {
        let _guard = cache_test();
        let size = 4 * 1024 * 1024;
        let value = "q".repeat(size);
        let expected = frame_string(value.clone());
        for hfhe in [false, true] {
            for profile in ["standard", "compute"] {
                for view in [false, true] {
                    for policy in [false, true] {
                        let (input, request) = invoke_input(hfhe, profile, view, policy, &value);
                        let mut runtime = Runtime::instantiate(&input, false).unwrap();
                        let len = runtime.instance.get_typed_func::<(i32, i32), i32>(
                            &runtime.store, "len").unwrap();
                        let get = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                            &runtime.store, "get").unwrap();
                        let length = request.len() as i32;
                        assert_eq!(len.call(&mut runtime.store, (0, length)).unwrap(),
                            expected.len() as i32);
                        let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
                        memory.write(&mut runtime.store, 1024, &[3; 64]).unwrap();
                        for (pointer, cap) in [
                            (1024, -1), (-1, 0), (i32::MAX, size as i32), (1024, size as i32),
                        ] {
                            let fuel = runtime.store.get_fuel().unwrap();
                            let (reply, count) = native_bytes::tests::large_allocs(size,
                                || get.call(&mut runtime.store, (0, length, pointer, cap)).unwrap());
                            let spent = fuel - runtime.store.get_fuel().unwrap();
                            assert_eq!(reply, -2);
                            assert_eq!(count, 0, "invoke response copied cache; fuel = {spent}");
                            assert_eq!(spent, request.len() as u64 + 67);
                            assert_eq!(&memory.data(&runtime.store)[1024..1088], &[3; 64]);
                        }
                        let fuel = runtime.store.get_fuel().unwrap();
                        let (_, count) = native_bytes::tests::large_allocs(size, || {
                            for _ in 0..128 {
                                assert_eq!(get.call(&mut runtime.store, (0, length, 1024, 0)).unwrap(), -2);
                            }
                        });
                        assert_eq!(count, 0, "repeated invoke response copied cache");
                        assert_eq!(fuel - runtime.store.get_fuel().unwrap(),
                            128 * (request.len() as u64 + 67));
                        let cache = if hfhe { &runtime.store.data().hfhe_invoke_cache }
                            else { &runtime.store.data().circle_invoke_cache };
                        assert_eq!(cache.as_ref().unwrap().0, request);
                        assert_eq!(cache.as_ref().unwrap().1.as_slice(), expected.as_slice());
                        let cap = runtime.store.data().max_guest_bytes;
                        runtime.store.data_mut().max_guest_bytes = size;
                        let (error, count) = native_bytes::tests::large_allocs(size,
                            || get.call(&mut runtime.store,
                                (0, length, 1024, expected.len() as i32)).unwrap_err());
                        assert!(error.to_string().contains("guest write exceeds cap"));
                        assert_eq!(count, 0);
                        let cache = if hfhe { &runtime.store.data().hfhe_invoke_cache }
                            else { &runtime.store.data().circle_invoke_cache };
                        assert_eq!(cache.as_ref().unwrap().0, request);
                        assert_eq!(cache.as_ref().unwrap().1.as_slice(), expected.as_slice());
                        runtime.store.data_mut().max_guest_bytes = cap;
                        let (reply, count) = native_bytes::tests::large_allocs(size,
                            || get.call(&mut runtime.store,
                                (0, length, 1024, expected.len() as i32)).unwrap());
                        assert_eq!(reply, expected.len() as i32);
                        assert_eq!(count, 0, "successful invoke response copied cache");
                        assert_eq!(&memory.data(&runtime.store)[1024..1024 + expected.len()], &expected);
                        let cache = if hfhe { &runtime.store.data().hfhe_invoke_cache }
                            else { &runtime.store.data().circle_invoke_cache };
                        assert!(cache.is_none());
                    }
                }
            }
        }
    }

    #[test]
    fn invoke_request_memory() {
        let _guard = cache_test();
        let size = 4 * 1024 * 1024;
        let extra = "q".repeat(size);
        let expected = frame_string("abcd");
        for hfhe in [false, true] {
            let (input, request) = invoke_input(hfhe, "standard", true, false, "abcd");
            let long = if hfhe {
                state_request("fhe_load_pk", &["owner", &extra])
            } else {
                state_request("state_descriptor_get", &[&"a".repeat(64), "state_class", &extra])
            };
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let len = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            let get = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "get").unwrap();
            memory.write(&mut runtime.store, 0, &long).unwrap();
            assert_eq!(len.call(&mut runtime.store, (0, long.len() as i32)).unwrap(),
                expected.len() as i32);
            memory.write(&mut runtime.store, 0, &request).unwrap();
            let fuel = runtime.store.get_fuel().unwrap();
            let (reply, count) = native_bytes::tests::large_allocs(size,
                || get.call(&mut runtime.store, (0, request.len() as i32, 1024, 0)).unwrap());
            assert_eq!(reply, -2);
            assert_eq!(count, 0, "invoke mismatch copied cached request");
            let charge = if hfhe { HFHE_INVOKE_FUEL } else { CIRCLE_INVOKE_FUEL };
            assert_eq!(fuel - runtime.store.get_fuel().unwrap(), request.len() as u64 + 67 + charge);
            let cache = if hfhe { &runtime.store.data().hfhe_invoke_cache }
                else { &runtime.store.data().circle_invoke_cache };
            assert_eq!(cache.as_ref().unwrap().0, long);
            memory.write(&mut runtime.store, 0, &long).unwrap();
            let fuel = runtime.store.get_fuel().unwrap();
            let (reply, count) = native_bytes::tests::large_allocs(size,
                || get.call(&mut runtime.store, (0, long.len() as i32, 1024, 0)).unwrap());
            assert_eq!(reply, -2);
            assert_eq!(count, 1, "invoke hit copied cached request");
            assert_eq!(fuel - runtime.store.get_fuel().unwrap(), long.len() as u64 + 67);
        }
    }

    #[test]
    fn invoke_read_results() {
        let _guard = cache_test();
        for hfhe in [false, true] {
            for profile in ["standard", "compute"] {
                for value in ["", "abcd"] {
                    let (input, request) = invoke_input(hfhe, profile, true, true, value);
                    let mut runtime = Runtime::instantiate(&input, false).unwrap();
                    let expected = frame_string(value.to_owned());
                    let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
                    let len = runtime.instance.get_typed_func::<(i32, i32), i32>(
                        &runtime.store, "len").unwrap();
                    let get = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                        &runtime.store, "get").unwrap();
                    let length = request.len() as i32;
                    assert_eq!(len.call(&mut runtime.store, (0, length)).unwrap(), expected.len() as i32);
                    for (pointer, cap) in [(-1, -1), (i32::MAX, 0), (1024, expected.len() as i32 - 1)] {
                        assert_eq!(get.call(&mut runtime.store, (0, length, pointer, cap)).unwrap(), -2);
                    }
                    for (pointer, count) in [(-1, length), (0, -1), (i32::MAX, length)] {
                        assert!(get.call(&mut runtime.store, (pointer, count, 1024, 0)).is_err());
                    }
                    for pointer in [-1, i32::MAX] {
                        assert_eq!(get.call(&mut runtime.store,
                            (0, length, pointer, expected.len() as i32)).unwrap(), -2);
                        let cache = if hfhe { &runtime.store.data().hfhe_invoke_cache }
                            else { &runtime.store.data().circle_invoke_cache };
                        assert!(cache.is_none());
                        assert_eq!(len.call(&mut runtime.store, (0, length)).unwrap(), expected.len() as i32);
                    }
                    let mut other = request.clone();
                    *other.last_mut().unwrap() = b'z';
                    memory.write(&mut runtime.store, 0, &other).unwrap();
                    let error = get.call(&mut runtime.store, (0, length, 1024, 0)).unwrap_err();
                    let message = if hfhe { "hfhe pubkey unavailable" } else { "unsupported state descriptor field" };
                    assert!(error.to_string().contains(message), "{error}");
                    let error = len.call(&mut runtime.store, (0, length)).unwrap_err();
                    assert!(error.to_string().contains(message), "{error}");
                    memory.write(&mut runtime.store, 0, &request).unwrap();
                    let cache = if hfhe { &runtime.store.data().hfhe_invoke_cache }
                        else { &runtime.store.data().circle_invoke_cache };
                    assert_eq!(cache.as_ref().unwrap().0, request);
                    assert_eq!(cache.as_ref().unwrap().1.as_slice(), expected.as_slice());
                    let fuel = runtime.store.get_fuel().unwrap();
                    runtime.store.set_fuel(0).unwrap();
                    assert!(get.call(&mut runtime.store, (0, length, 1024, 0)).is_err());
                    runtime.store.set_fuel(fuel).unwrap();
                    assert_eq!(get.call(&mut runtime.store,
                        (0, length, 0, expected.len() as i32)).unwrap(), expected.len() as i32);
                    assert_eq!(&memory.data(&runtime.store)[..expected.len()], &expected);
                    memory.write(&mut runtime.store, 0, &request).unwrap();
                    let fuel = runtime.store.get_fuel().unwrap();
                    assert_eq!(get.call(&mut runtime.store,
                        (0, length, 1024, expected.len() as i32)).unwrap(), expected.len() as i32);
                    let charge = if hfhe { HFHE_INVOKE_FUEL } else { CIRCLE_INVOKE_FUEL };
                    assert_eq!(fuel - runtime.store.get_fuel().unwrap(), length as u64 + 67 + charge);
                    assert_eq!(&memory.data(&runtime.store)[1024..1024 + expected.len()], &expected);
                }
            }
        }
    }

    #[test]
    fn invoke_read_receipts() {
        let _guard = cache_test();
        for mode in ["capture", "consume"] {
            let (mut input, request) = invoke_input(true, "standard", false, true, "abcd");
            let other = state_request("fhe_load_pk", &["owner", "extra"]);
            let expected = frame_string("abcd");
            let entry = hfhe_receipt_entry(false, "fhe_load_pk", &request, &expected).unwrap();
            let changed = hfhe_receipt_entry(false, "fhe_load_pk", &other, &expected).unwrap();
            input.hfhe_receipt_mode = Some(mode.to_owned());
            if mode == "consume" {
                input.hfhe_receipt_entries = Some(vec![entry.clone(), entry.clone(), changed.clone(), entry.clone()]);
            }
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let len = runtime.instance.get_typed_func::<(i32, i32), i32>(
                &runtime.store, "len").unwrap();
            let get = runtime.instance.get_typed_func::<(i32, i32, i32, i32), i32>(
                &runtime.store, "get").unwrap();
            let length = request.len() as i32;
            for count in [1, 2] {
                assert_eq!(len.call(&mut runtime.store, (0, length)).unwrap(), expected.len() as i32);
                for _ in 0..128 {
                    assert_eq!(get.call(&mut runtime.store, (0, length, 1024, 0)).unwrap(), -2);
                }
                assert_eq!(runtime.store.data().hfhe_receipt_entries, vec![entry.clone(); count]);
                assert_eq!(runtime.store.data().hfhe_receipt_index, if mode == "consume" { count } else { 0 });
            }
            memory.write(&mut runtime.store, 0, &other).unwrap();
            assert_eq!(get.call(&mut runtime.store, (0, other.len() as i32, 1024, 0)).unwrap(), -2);
            assert_eq!(runtime.store.data().hfhe_receipt_entries, vec![entry.clone(), entry.clone(), changed.clone()]);
            memory.write(&mut runtime.store, 0, &request).unwrap();
            assert_eq!(get.call(&mut runtime.store,
                (0, length, 1024, expected.len() as i32)).unwrap(), expected.len() as i32);
            assert_eq!(&memory.data(&runtime.store)[1024..1024 + expected.len()], &expected);
            assert!(runtime.store.data().hfhe_invoke_cache.is_none());
            assert_eq!(runtime.store.data().hfhe_receipt_entries, vec![entry.clone(), entry.clone(), changed.clone()]);
            assert_eq!(runtime.store.data().hfhe_receipt_index, if mode == "consume" { 3 } else { 0 });
            assert_eq!(get.call(&mut runtime.store,
                (0, length, 1024, expected.len() as i32)).unwrap(), expected.len() as i32);
            assert_eq!(runtime.store.data().hfhe_receipt_entries, vec![entry.clone(), entry.clone(), changed, entry]);
            assert_eq!(runtime.store.data().hfhe_receipt_index, if mode == "consume" { 4 } else { 0 });
        }
    }

    fn state_wasm(request: &[u8], probe: bool, manifest: Option<&[u8]>, start: bool) -> Vec<u8> {
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        if probe {
            types.ty().function([ValType::I32; 2], [ValType::I32]);
        } else {
            types.ty().function([ValType::I32; 4], [ValType::I32]);
        }
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        if start {
            types.ty().function([], []);
        }
        module.section(&types);
        let mut imports = ImportSection::new();
        let invoke = if probe { "host_circle_invoke_len" } else { "host_circle_invoke" };
        imports.import("octra", invoke, EntityType::Function(0));
        imports.import("octra", "host_response_write", EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(2).function(1);
        if start {
            functions.function(3);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1, maximum: Some(1), memory64: false,
            shared: false, page_size_log2: None,
        });
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 2);
        exports.export("octra_query", ExportKind::Func, 3);
        if manifest.is_some() {
            exports.export("octra_manifest", ExportKind::Func, 3);
        }
        module.section(&exports);
        if start {
            module.section(&StartSection { function_index: 4 });
        }
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        alloc.instruction(&I::I32Const(2048)).instruction(&I::End);
        code.function(&alloc);
        let mut query = Function::new([(1, ValType::I32)]);
        query.instruction(&I::I32Const(0)).instruction(&I::I32Const(request.len() as i32));
        if !probe {
            query.instruction(&I::I32Const(4096)).instruction(&I::I32Const(1024));
        }
        query.instruction(&I::Call(0)).instruction(&I::LocalSet(2));
        if let Some(body) = manifest {
            query.instruction(&I::I32Const(8192))
                .instruction(&I::I32Const(body.len() as i32))
                .instruction(&I::Call(1)).instruction(&I::Drop);
        } else if !probe {
            for op in [I::I32Const(4096), I::LocalGet(2), I::Call(1), I::Drop] {
                query.instruction(&op);
            }
        }
        query.instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&query);
        if start {
            let mut init = Function::new([]);
            init.instruction(&I::I32Const(0)).instruction(&I::I32Const(request.len() as i32))
                .instruction(&I::I32Const(4096)).instruction(&I::I32Const(1024))
                .instruction(&I::Call(0)).instruction(&I::Drop).instruction(&I::End);
            code.function(&init);
        }
        module.section(&code);
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(0), request.iter().copied());
        if let Some(body) = manifest {
            data.active(0, &ConstExpr::i32_const(8192), body.iter().copied());
        }
        module.section(&data);
        module.finish()
    }

    fn state_module(request: &[u8], probe: bool) -> Payload<'static> {
        Payload::deserialize(json!({
            "code_b64": encode_b64(&state_wasm(request, probe, None, false)),
            "execution_profile": "compute", "is_view": true,
            "hfhe_strict": false, "fuel_limit": 1_000_000,
        })).unwrap()
    }

    #[test]
    fn storage_manifest_quota() {
        let _cache = cache_test();
        let request = state_request("state_restore", &[&"a".repeat(64)]);
        let manifest = json!({"methods": [{"name": "octra_query", "public_reads": [{}]}]});
        let resident = compute_memory().used();
        for profile in ["manifest", "compute"] {
            for local in [false, true] {
                for start in [false, true] {
                    let wasm = state_wasm(&request, false, Some(manifest.to_string().as_bytes()), start);
                    let input: Payload = Payload::deserialize(json!({
                        "code_b64": encode_b64(&wasm), "execution_profile": profile,
                        "inspect_policy": local, "is_view": true, "hfhe_strict": false,
                    })).unwrap();
                    assert_eq!(run_describe(&input).unwrap()["manifest"], manifest);
                    let held = compute_memory().take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
                    let result = run_describe(&input);
                    if local || profile == "compute" {
                        assert!(matches!(result, Err(HostFailure::Unavailable(ref reason))
                            if reason == "compute memory busy"), "description lost resource error: {result:?}");
                    } else {
                        assert_eq!(result.unwrap()["manifest"], manifest);
                    }
                    drop(held);
                    assert_eq!(compute_memory().used(), resident);
                }
            }
        }
    }

    #[test]
    fn compute_state_reads() {
        let _cache = cache_test();
        let key = "7".repeat(64);
        let id = "a".repeat(64);
        let entry = compute_entry(&"x".repeat(1024 * 1024));
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        for (method, params) in [
            ("state_descriptor_get", vec![id.as_str(), "state_class"]),
            ("state_policy_get", vec![id.as_str(), "revoked"]),
            ("balance_cell_get", vec![id.as_str(), "amount_commitment"]),
            ("register_cell_get", vec![id.as_str(), "ciphertext_commitment"]),
            ("balance_binding_get", vec!["alice", "version"]),
            ("register_binding_get", vec![id.as_str(), "version"]),
            ("balance_workflow_get", vec![id.as_str(), "status"]),
            ("register_workflow_get", vec![id.as_str(), "status"]),
            ("object_binding_get", vec![id.as_str(), "status"]),
            ("object_member_get", vec![id.as_str(), "member", "status"]),
            ("object_policy_get", vec![id.as_str(), "revoked"]),
        ] {
            let mut input = state_module(&state_request(method, &params), false);
            input.compute_storage_cache_key = Some(key.clone());
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
            let ((_, output, _), count) = native_bytes::tests::large_allocs(512 * 1024,
                || runtime.call_export("octra_query", &[]).unwrap());
            assert_eq!(output.as_slice(), frame_null(), "query response changed: {method}");
            assert_eq!(count, 0, "state read copied model: {method}");
            assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage),
                "state read detached model: {method}");
        }
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_description_memory() {
        let _cache = cache_test();
        let key = "8".repeat(64);
        let entry = compute_entry(&"x".repeat(1024 * 1024));
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        let mut input = state_module(&state_request("state_descriptor_get", &[&"a".repeat(64), "state_class"]), false);
        input.compute_storage_cache_key = Some(key.clone());
        drop(Runtime::instantiate(&input, false).unwrap());
        let (runtime, count) = native_bytes::tests::large_allocs(512 * 1024,
            || Runtime::instantiate(&input, true).unwrap());
        assert!(runtime.store.data().storage.is_empty());
        assert_eq!(count, 0, "description copied model before clearing");
        assert_eq!(entry.storage["weights"].len(), 1024 * 1024);
        assert!(Arc::ptr_eq(&compute_storage_cache().lock().unwrap().entries[&key].storage,
            &entry.storage));
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_read_unknown() {
        let _cache = cache_test();
        let key = "b".repeat(64);
        let entry = compute_entry(&"x".repeat(1024 * 1024));
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        for method in ["unknown", "state_descriptor_get ", ""] {
            let mut input = state_module(&state_request(method, &[]), false);
            input.compute_storage_cache_key = Some(key.clone());
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let (error, count) = native_bytes::tests::large_allocs(512 * 1024,
                || runtime.call_export("octra_query", &[]).unwrap_err());
            assert_eq!(error, format!("wasm export trapped: unsupported circle host method: {method}"));
            assert_eq!(count, 0, "unknown method copied model");
            assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
        }
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_write_refused() {
        let _cache = cache_test();
        let key = "c".repeat(64);
        let entry = compute_entry(&"x".repeat(1024 * 1024));
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        let mut input = state_module(&state_request("state_restore", &["bad"]), false);
        input.compute_storage_cache_key = Some(key.clone());
        input.is_view = Some(false);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let (error, count) = native_bytes::tests::large_allocs(512 * 1024,
            || runtime.call_export("octra_query", &[]).unwrap_err());
        assert_eq!(error, "wasm export trapped: state_ref must be a 64-char hex string");
        assert_eq!(count, 0, "refused write copied model");
        assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_read_probe() {
        let _cache = cache_test();
        let key = "d".repeat(64);
        let id = "a".repeat(64);
        let entry = compute_entry(&"x".repeat(1024 * 1024));
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        for probe in [false, true] {
            for (method, params, changed) in [
                ("state_descriptor_get", vec![id.as_str(), "state_class"], false),
                ("balance_cell_materialize", vec![id.as_str(), ""], false),
                ("balance_cell_materialize", vec![id.as_str(), "commit", ""], true),
            ] {
                let mut input = state_module(&state_request(method, &params), probe);
                input.compute_storage_cache_key = Some(key.clone());
                input.is_view = Some(false);
                let mut runtime = Runtime::instantiate(&input, false).unwrap();
                let (result, count) = native_bytes::tests::large_allocs(512 * 1024,
                    || runtime.call_export("octra_query", &[]));
                if method == "state_descriptor_get" {
                    let (_, output, _) = result.unwrap();
                    if probe {
                        assert!(output.is_empty());
                        assert_eq!(runtime.store.data().circle_invoke_cache.as_ref().unwrap().1.as_slice(),
                            frame_null().as_slice());
                    } else {
                        assert_eq!(output.as_slice(), frame_null());
                    }
                } else {
                    let field = if changed { "amount_commitment" } else { "ciphertext_commitment" };
                    assert_eq!(result.unwrap_err(), format!("wasm export trapped: {field} must not be empty"));
                }
                assert_eq!(count, 0, "failed write copied model bytes");
                assert_eq!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage), !changed);
                if changed {
                    assert_eq!(runtime.store.data().storage[&balance_cell_key(&id, "ciphertext_commitment")].as_slice(),
                        b"commit");
                }
                assert!(!entry.storage.contains_key(&balance_cell_key(&id, "ciphertext_commitment")));
            }
        }
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_write_copy() {
        let _guard = cache_test();
        let entry = compute_entry(&"x".repeat(1024 * 1024));
        assert_eq!(entry.root,
            "421f4d1b539a674400386ba7e01d09e621d4850a1c637a729a4d78c870728153");
        let mut image = entry.storage.clone();
        let (_, count) = native_bytes::tests::large_allocs(512 * 1024, || {
            let mut edit = StorageEdit(&mut image, true, None);
            edit.remove("absent").unwrap();
        });
        assert_eq!(count, 0, "absent key removal copied model");
        assert!(Arc::ptr_eq(&image, &entry.storage));
        let (_, count) = native_bytes::tests::large_allocs(512 * 1024, || {
            let mut edit = StorageEdit(&mut image, true, None);
            edit.insert("first".to_owned(), b"one".to_vec()).unwrap();
            assert_eq!(edit["first"].as_slice(), b"one");
            assert!(Arc::ptr_eq(&edit["weights"], &entry.storage["weights"]));
            edit.insert("second".to_owned(), b"two".to_vec()).unwrap();
            assert_eq!(edit["first"].as_slice(), b"one");
            assert_eq!(edit["second"].as_slice(), b"two");
            assert!(Arc::ptr_eq(&edit["weights"], &entry.storage["weights"]));
            edit.remove("first").unwrap();
            assert!(!edit.contains_key("first"));
            assert_eq!(edit["second"].as_slice(), b"two");
            assert!(Arc::ptr_eq(&edit["weights"], &entry.storage["weights"]));
            edit.remove("weights").unwrap();
        });
        assert_eq!(count, 0, "shared image copied model bytes");
        assert_eq!(&image.data, &BTreeMap::from([("second".to_owned(), Arc::new(b"two".to_vec().into()))]));
        assert_eq!(entry.storage["weights"].len(), 1024 * 1024);
        assert_eq!(entry.storage.len(), 1);
        assert_eq!(compute_storage_root(&entry.storage), entry.root);
    }

    #[test]
    fn compute_read_values() {
        let _cache = cache_test();
        let key = "9".repeat(64);
        let id = "a".repeat(64);
        let cases = [
            ("state_descriptor_get", vec![id.as_str(), "state_class"],
                state_descriptor_key(&id, "state_class"), "tensor", frame_string("tensor")),
            ("state_policy_get", vec![id.as_str(), "revoked"],
                state_policy_key(&id, "revoked"), "true", frame_bool(true)),
            ("balance_cell_get", vec![id.as_str(), "amount_commitment"],
                balance_cell_key(&id, "amount_commitment"), "abc", frame_string("abc")),
            ("register_cell_get", vec![id.as_str(), "ciphertext_commitment"],
                register_cell_key(&id, "ciphertext_commitment"), "xyz", frame_string("xyz")),
            ("balance_binding_get", vec!["alice", "version"],
                balance_binding_key("alice", "version"), "42", frame_int("42")),
            ("register_binding_get", vec![id.as_str(), "version"],
                register_binding_key(&id, "version"), "7", frame_int("7")),
            ("balance_workflow_get", vec![id.as_str(), "status"],
                balance_workflow_key(&id, "status"), "active", frame_string("active")),
            ("register_workflow_get", vec![id.as_str(), "status"],
                register_workflow_key(&id, "status"), "active", frame_string("active")),
            ("object_binding_get", vec![id.as_str(), "status"],
                object_binding_key(&id, "status"), "live", frame_string("live")),
            ("object_member_get", vec![id.as_str(), "member", "status"],
                object_member_key(&id, "member", "status"), "active", frame_string("active")),
            ("object_policy_get", vec![id.as_str(), "revoked"],
                object_policy_key(&id, "revoked"), "false", frame_bool(false)),
        ];
        let mut stored = compute_entry(&"x".repeat(1024 * 1024)).storage;
        for (_, _, path, value, _) in &cases {
            StorageEdit(&mut stored, false, None).insert(path.clone(), value.as_bytes().to_vec()).unwrap();
        }
        let entry = ComputeStorageEntry {
            root: compute_storage_root(&stored), weight: stored.values().map(|value| value.len()).sum(),
            storage: stored, updated_at: 0.0,
        };
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        for view in [true, false] {
            for (method, params, _, _, expected) in &cases {
                let mut input = state_module(&state_request(method, params), false);
                input.compute_storage_cache_key = Some(key.clone());
                input.is_view = Some(view);
                let mut runtime = Runtime::instantiate(&input, false).unwrap();
                let (_, output, _) = runtime.call_export("octra_query", &[]).unwrap();
                assert_eq!(output.as_slice(), expected, "query value changed: {method}");
                assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
            }
        }
        for (params, expected) in [
            (vec![id.as_str(), "unknown"], "unsupported state descriptor field: unknown"),
            (vec!["bad", "state_class"], "state_ref must be a 64-char hex string"),
            (vec![id.as_str(), ""], "field must not be empty"),
        ] {
            let mut input = state_module(&state_request("state_descriptor_get", &params), false);
            input.compute_storage_cache_key = Some(key.clone());
            let mut runtime = Runtime::instantiate(&input, false).unwrap();
            let (error, count) = native_bytes::tests::large_allocs(512 * 1024,
                || runtime.call_export("octra_query", &[]).unwrap_err());
            assert!(error.contains(expected), "query error changed: {error}");
            assert_eq!(count, 0);
            assert!(Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
        }
        let mut input = state_module(&state_request("state_restore", &[&id]), false);
        input.compute_storage_cache_key = Some(key.clone());
        input.is_view = Some(false);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let (_, output, _) = runtime.call_export("octra_query", &[]).unwrap();
        assert_eq!(output.as_slice(), frame_bool(true));
        assert!(!Arc::ptr_eq(&runtime.store.data().storage, &entry.storage));
        assert_eq!(runtime.store.data().storage[&state_policy_key(&id, "tombstone")].as_slice(), b"false");
        assert!(!entry.storage.contains_key(&state_policy_key(&id, "tombstone")));
        assert_eq!(compute_storage_root(&entry.storage), entry.root);
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_memory_images() {
        let _cache = cache_test();
        let key = "1".repeat(64);
        let other = "2".repeat(64);
        let encoded = BASE64.encode(vec![7; 8192]);
        let size = base64::decoded_len_estimate(encoded.len());
        let resident = size + "weights".len();
        let pool = native_bytes::Pool::new(2 * resident);
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights", "compute_storage_value_b64": encoded,
        })).unwrap();
        let first = octra_compute_build_new();
        let second = octra_compute_build_new();
        unsafe {
            assert_eq!(octra_compute_build_begin(first, key.as_ptr(), key.len()), 0);
            assert_eq!(octra_compute_build_begin(second, other.as_ptr(), other.len()), 0);
        }
        let token = unsafe { (*first).token().unwrap() };
        let next = unsafe { (*second).token().unwrap() };
        put_storage(&token, &payload, &pool).unwrap();
        assert_eq!(pool.used(), resident);
        put_storage(&token, &payload, &pool).unwrap();
        assert_eq!(pool.used(), resident);
        let changed: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights", "compute_storage_value_b64": BASE64.encode(vec![8; 8192]),
        })).unwrap();
        assert_eq!(put_storage(&token, &changed, &pool).unwrap_err(), "compute storage key differs");
        assert_eq!(pool.used(), resident);
        put_storage(&next, &payload, &pool).unwrap();
        assert_eq!(pool.used(), 2 * resident);
        let (error, allocations) = native_bytes::tests::large_allocs(8192,
            || put_storage(&token, &payload, &pool).unwrap_err());
        assert_eq!(error, "compute memory busy");
        assert_eq!(allocations, 0);
        let root = {
            let table = compute_storage_builders().lock().unwrap();
            compute_storage_root(&table.data[&key].storage)
        };
        let seal: Payload = Payload::deserialize(json!({
            "compute_storage_root": root,
            "compute_storage_entries": 1, "compute_storage_bytes": 8192,
        })).unwrap();
        run_compute_storage_seal(&token, &seal).unwrap();
        let read: Payload = Payload::deserialize(json!({"compute_storage_cache_key": key})).unwrap();
        let image = build_storage_from_payload(&read, ExecutionProfile::Compute, true, None).unwrap();
        let value = image["weights"].clone();
        assert_eq!(pool.used(), 2 * resident);
        unsafe {
            octra_compute_build_close(first);
            octra_compute_build_close(second);
            octra_compute_build_release(first);
            octra_compute_build_release(second);
        }
        build_released(&next);
        assert_eq!(pool.used(), resident);
        let third = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(third, key.as_ptr(), key.len()), 0); }
        let newer = unsafe { (*third).token().unwrap() };
        put_storage(&newer, &changed, &pool).unwrap();
        let newer_root = {
            let table = compute_storage_builders().lock().unwrap();
            compute_storage_root(&table.data[&key].storage)
        };
        assert_ne!(newer_root, root);
        let seal: Payload = Payload::deserialize(json!({
            "compute_storage_root": newer_root,
            "compute_storage_entries": 1, "compute_storage_bytes": 8192,
        })).unwrap();
        run_compute_storage_seal(&newer, &seal).unwrap();
        assert_eq!(pool.used(), 2 * resident);
        assert_eq!(compute_storage_root(&image), root);
        compute_cache_call(CacheMessage::Drop(key.clone())).unwrap();
        assert_eq!(pool.used(), resident);
        unsafe {
            octra_compute_build_close(third);
            octra_compute_build_release(third);
        }
        drop(image);
        assert_eq!(pool.used(), size);
        assert!(value.iter().all(|byte| *byte == 7));
        drop(value);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn compute_key_quota() {
        let _cache = cache_test();
        let key = "4".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        for name in ["".to_owned(), "k\0v".to_owned(), "k".repeat(257), "\u{e9}".repeat(129)] {
            let payload: Payload = Payload::deserialize(json!({
                "compute_storage_key": name, "compute_storage_value_b64": "",
            })).unwrap();
            let pool = native_bytes::Pool::new(256);
            assert_eq!(put_storage(&token, &payload, &pool).unwrap_err(), "invalid compute storage key");
            assert_eq!(pool.used(), 0);
        }
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "k".repeat(256), "compute_storage_value_b64": "",
        })).unwrap();
        for size in [0, 1, 255] {
            let pool = native_bytes::Pool::new(size);
            let result = put_storage(&token, &payload, &pool);
            assert!(result.is_err(), "compute key escaped the resident-byte quota");
            assert_eq!(result.unwrap_err(), "compute memory busy");
            assert_eq!(pool.used(), 0);
            assert!(compute_storage_builders().lock().unwrap().data[&key].storage.is_empty());
        }
        let pool = native_bytes::Pool::new(256);
        assert_eq!(put_storage(&token, &payload, &pool).unwrap()["bytes"], 0);
        assert_eq!(pool.used(), 256);
        put_storage(&token, &payload, &pool).unwrap();
        assert_eq!(pool.used(), 256);
        let other: Payload = Payload::deserialize(json!({
            "compute_storage_key": "extra", "compute_storage_value_b64": "",
        })).unwrap();
        assert_eq!(put_storage(&token, &other, &pool).unwrap_err(), "compute memory busy");
        assert_eq!(pool.used(), 256);
        unsafe {
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn compute_key_owner() {
        let _cache = cache_test();
        let key = "5".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let pool = native_bytes::Pool::new(4096);
        let foreign = native_bytes::Pool::new(256);
        let mut payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "\u{e9}", "compute_storage_value_b64": "eA==",
        })).unwrap();
        assert_eq!(put_storage(&token, &payload, &pool).unwrap()["bytes"], 1);
        let used = pool.used();
        assert_eq!(used, 2 + base64::decoded_len_estimate(4));
        put_storage(&token, &payload, &foreign).unwrap();
        assert_eq!(foreign.used(), 0);
        payload.compute_storage_key = Some("other".to_owned());
        assert_eq!(put_storage(&token, &payload, &foreign).unwrap_err(), "compute memory owners differ");
        assert_eq!(foreign.used(), 0);
        assert_eq!(pool.used(), used);
        payload.compute_storage_value_b64 = Some(TextRef(Cow::Borrowed("!!!!")));
        assert!(put_storage(&token, &payload, &pool).is_err());
        assert_eq!(pool.used(), used);
        payload.compute_storage_value_b64 = Some(TextRef(Cow::Borrowed("")));
        for index in 1..COMPUTE_STORAGE_ENTRY_LIMIT {
            payload.compute_storage_key = Some(index.to_string());
            put_storage(&token, &payload, &pool).unwrap();
        }
        let used = pool.used();
        payload.compute_storage_key = Some("full".to_owned());
        assert_eq!(put_storage(&token, &payload, &pool).unwrap_err(), "compute storage entry count exceeds limit");
        assert_eq!(pool.used(), used);
        let table = compute_storage_builders().lock().unwrap();
        assert_eq!(table.data[&key].weight, 1);
        assert_eq!(table.data[&key].storage.len(), COMPUTE_STORAGE_ENTRY_LIMIT);
        drop(table);
        unsafe {
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(pool.used(), 0);
    }

    #[test]
    fn compute_key_lifetime() {
        let _cache = cache_test();
        let key = "6".repeat(64);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "k".repeat(251), "compute_storage_value_b64": "",
        })).unwrap();
        let pool = native_bytes::Pool::new(251);
        let failed = native_bytes::tests::refused_alloc(251, || put_storage(&token, &payload, &pool));
        assert_eq!(failed.unwrap_err(), "compute storage allocation failed");
        assert_eq!(pool.used(), 0);
        assert!(compute_storage_builders().lock().unwrap().data[&key].storage.is_empty());
        let observed = pool.clone();
        let (_, freed) = native_bytes::tests::paused_alloc(251, 1, move || {
            put_storage(&token, &payload, &pool).unwrap();
            let root = {
                let table = compute_storage_builders().lock().unwrap();
                compute_storage_root(&table.data[&key].storage)
            };
            let seal: Payload = Payload::deserialize(json!({
                "compute_storage_root": root, "compute_storage_entries": 1,
                "compute_storage_bytes": 0,
            })).unwrap();
            run_compute_storage_seal(&token, &seal).unwrap();
            let image = compute_storage_cache().lock().unwrap().entries[&key].storage.clone();
            assert_eq!(image.keys.as_ref().unwrap().size(), 251);
            assert_eq!(compute_storage_root(&image), root);
            compute_cache_call(CacheMessage::Drop(key)).unwrap();
            assert_eq!(pool.used(), 251);
            drop(image);
        }, |_| assert_eq!(observed.used(), 251));
        assert!(freed);
        assert_eq!(observed.used(), 0);
        unsafe {
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
    }

    #[test]
    fn compute_memory_close() {
        let _cache = cache_test();
        let key = "1".repeat(64);
        let pool = native_bytes::Pool::new(12);
        let first = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(first, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*first).token().unwrap() };
        assert!(decode_storage(&pool, "!!!!").is_err());
        assert_eq!(pool.used(), 0);
        let value = decode_storage(&pool, &BASE64.encode(b"old")).unwrap();
        assert_eq!(pool.used(), 3);
        unsafe { octra_compute_build_close(first); }
        build_released(&token);
        assert_eq!(pool.used(), 3);
        let second = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(second, key.as_ptr(), key.len()), 0); }
        let next = unsafe { (*second).token().unwrap() };
        assert_ne!(token.id, next.id);
        assert_eq!(commit_storage(&token, "weights", value, &pool).unwrap_err(), "compute storage builder changed");
        assert_eq!(pool.used(), 0);
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights", "compute_storage_value_b64": BASE64.encode(b"new"),
        })).unwrap();
        put_storage(&next, &payload, &pool).unwrap();
        assert_eq!(pool.used(), 10);
        let wrong: Payload = Payload::deserialize(json!({
            "compute_storage_root": "0".repeat(64),
            "compute_storage_entries": 1, "compute_storage_bytes": 3,
        })).unwrap();
        assert_eq!(run_compute_storage_seal(&next, &wrong).unwrap_err(), "compute storage seal differs");
        assert_eq!(pool.used(), 0);
        assert!(!compute_storage_cache().lock().unwrap().entries.contains_key(&key));
        unsafe {
            octra_compute_build_close(first);
            octra_compute_build_close(second);
            octra_compute_build_release(first);
            octra_compute_build_release(second);
        }
    }

    #[test]
    fn compute_memory_race() {
        let _cache = cache_test();
        let key = "9".repeat(64);
        let first = octra_compute_build_new();
        let mut second = unsafe { Box::from_raw(octra_compute_build_new()) };
        unsafe { assert_eq!(octra_compute_build_begin(first, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*first).token().unwrap() };
        let retained = unsafe { Box::from_raw(octra_compute_build_retain(first)) };
        let baseline = compute_memory().used();
        let encoded = BASE64.encode(vec![7; 8192]);
        let size = base64::decoded_len_estimate(encoded.len());
        let input = json!({
            "action": "compute_storage_put",
            "compute_storage_key": "old",
            "compute_storage_value_b64": encoded,
        });
        let parsed = payload_memory(input.to_string().as_bytes()).unwrap().size();
        let mut next = None;
        let mut root = None;
        let (reply, freeing) = native_bytes::tests::paused_alloc(size, 1, move || {
            unsafe { build_step_reply(&*retained, input) }
        }, |phase| {
            match phase {
                native_bytes::tests::AllocPhase::Allocated => {
                    assert_eq!(compute_memory().used(), baseline + size + parsed);
                    unsafe {
                        octra_compute_build_close(first);
                        octra_compute_build_release(first);
                    }
                    build_released(&token);
                    assert_eq!(compute_memory().used(), baseline + size + parsed);
                    unsafe {
                        assert_eq!(octra_compute_build_begin(&*second, key.as_ptr(), key.len()), 0);
                        let (code, _) = build_step_reply(&*second, json!({
                            "action": "compute_storage_put",
                            "compute_storage_key": "new",
                            "compute_storage_value_b64": BASE64.encode(b"new"),
                        }));
                        assert_eq!(code, 0);
                    }
                    next = Some(second.token().unwrap());
                    assert_ne!(token.id, next.as_ref().unwrap().id);
                    let table = compute_storage_builders().lock().unwrap();
                    root = Some(compute_storage_root(&table.data[&key].storage));
                }
                native_bytes::tests::AllocPhase::Releasing => {
                    assert_eq!(compute_memory().used(), baseline + size + parsed + 6,
                        "decode charge released before its buffer");
                }
            }
        });
        assert_eq!(reply, (1, "compute storage builder changed".to_owned()),
            "old generation wrote into replacement");
        assert!(freeing, "decode buffer was not released");
        assert_eq!(compute_memory().used(), baseline + 6);
        {
            let table = compute_storage_builders().lock().unwrap();
            let stored = &table.data[&key];
            assert_eq!(stored.weight, 3);
            assert_eq!(stored.storage.len(), 1);
            assert_eq!(stored.storage["new"].as_slice(), b"new");
            assert!(!stored.storage.contains_key("old"));
            assert_eq!(Some(compute_storage_root(&stored.storage)), root);
            assert_eq!(table.owner.slots[&key], (next.as_ref().unwrap().id, BuildPhase::Writing));
        }
        unsafe { octra_compute_build_close(&mut *second); }
        build_released(&next.unwrap());
        assert_eq!(compute_memory().used(), baseline);
    }

    #[test]
    fn compute_memory_publish() {
        let _cache = cache_test();
        let key = "3".repeat(64);
        let pool = native_bytes::Pool::new(10);
        let port = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*port).token().unwrap() };
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights", "compute_storage_value_b64": BASE64.encode(b"abc"),
        })).unwrap();
        put_storage(&token, &payload, &pool).unwrap();
        let mut work = build_take(&token).unwrap();
        unsafe { octra_compute_build_close(port); }
        assert_eq!(pool.used(), 10);
        let data = work.data.take().unwrap();
        let entry = ComputeStorageEntry {
            root: compute_storage_root(&data.storage), weight: data.weight,
            storage: Arc::new(data.storage), updated_at: 0.0,
        };
        assert_eq!(build_publish(&token, entry).unwrap_err(), "compute storage builder changed");
        assert_eq!(pool.used(), 0);
        assert!(!compute_storage_cache().lock().unwrap().entries.contains_key(&key));
        drop(work);
        unsafe { octra_compute_build_release(port); }
    }

    #[test]
    fn compute_build_repeat() {
        let _cache = cache_test();
        let key = "3".repeat(64);
        let put: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights",
            "compute_storage_value_b64": BASE64.encode(b"x"),
        })).unwrap();
        let first = octra_compute_build_new();
        let second = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(first, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*first).token().unwrap() };
        run_compute_storage_put(&token, &put).unwrap();
        let repeated = build_begin(unsafe { &*(*second) }, &key);
        assert!(repeated.is_err(), "duplicate begin replaced an owned build");
        assert_eq!(compute_storage_builders().lock().unwrap().data[&key].storage["weights"].as_slice(), b"x");
        let drop: Payload = Payload::deserialize(json!({"compute_storage_cache_key": key})).unwrap();
        assert_eq!(run_compute_storage_drop(&drop).unwrap()["status"], "busy");
        unsafe { octra_compute_build_close(first); }
        build_released(&token);
        unsafe {
            assert_eq!(octra_compute_build_begin(second, key.as_ptr(), key.len()), 0);
            assert_eq!(octra_compute_build_begin(first, key.as_ptr(), key.len()), 4);
            assert_eq!(build_step_reply(first, json!({"action": "compute_storage_put"})).0, 1);
        }
        let next = unsafe { (*second).token().unwrap() };
        build_finish(&token);
        assert_eq!(compute_storage_builders().lock().unwrap().owner.slots[&key].0, next.id);
        unsafe {
            octra_compute_build_close(first);
            octra_compute_build_release(first);
            octra_compute_build_close(second);
            octra_compute_build_release(second);
        }
        build_released(&next);
    }

    fn build_released(token: &BuildToken) {
        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            let table = compute_storage_builders().lock().unwrap();
            if table.owner.slots.get(&token.key).map(|row| row.0) != Some(token.id) { return; }
            drop(table);
            assert!(Instant::now() < deadline, "build retirement did not finish");
            std::thread::yield_now();
        }
    }

    unsafe fn build_step_reply(port: *const Arc<ComputeBuild>, json: JsonValue) -> (i32, String) {
        let input = serde_json::to_vec(&json).unwrap();
        let mut output = std::ptr::null_mut();
        let mut size = 0;
        let code = octra_compute_build_step(port, input.as_ptr(), input.len(), &mut output, &mut size);
        let text = String::from_utf8(slice::from_raw_parts(output, size).to_vec()).unwrap();
        native_bytes::free_bytes(output, size);
        (code, text)
    }

    #[test]
    fn compute_build_pressure() {
        let state = BuildOwner::default();
        let (state, first) = state.delta(BuildMessage::Begin("a".to_owned())).unwrap();
        let first = match first { BuildReply::Begun(token) => token, _ => unreachable!() };
        let (state, second) = state.delta(BuildMessage::Begin("b".to_owned())).unwrap();
        let second = match second { BuildReply::Begun(token) => token, _ => unreachable!() };
        let state = state.delta(BuildMessage::Seal(first.clone())).unwrap().0;
        let state = state.delta(BuildMessage::Close(first.clone())).unwrap().0;
        assert!(state.delta(BuildMessage::Begin("c".to_owned())).is_err());
        assert!(state.delta(BuildMessage::Check(second.clone(), BuildPhase::Writing)).is_ok());
        let state = state.delta(BuildMessage::Finish(first.clone())).unwrap().0;
        let (state, next) = state.delta(BuildMessage::Begin("a".to_owned())).unwrap();
        let next = match next { BuildReply::Begun(token) => token, _ => unreachable!() };
        assert!(next.id > first.id);
        let state = state.delta(BuildMessage::Close(first.clone())).unwrap().0;
        let state = state.delta(BuildMessage::Finish(first)).unwrap().0;
        assert!(state.delta(BuildMessage::Check(next, BuildPhase::Writing)).is_ok());
        assert!(state.delta(BuildMessage::Check(second, BuildPhase::Writing)).is_ok());
        let stopped = state.delta(BuildMessage::Stop).unwrap().0;
        assert!(stopped.delta(BuildMessage::Begin("c".to_owned())).is_err());
        let exhausted = BuildOwner { next: i64::MAX as u64, ..BuildOwner::default() };
        assert!(exhausted.delta(BuildMessage::Begin("c".to_owned())).is_err());
    }

    #[test]
    fn compute_retire_error() {
        let _cache = cache_test();
        let key = "6".repeat(64);
        let port = octra_compute_build_new();
        for _ in 0..2 {
            let result = build_start(unsafe { &*(*port) }, &key,
                || Err("compute storage retirement unavailable".to_owned()));
            assert!(result.is_err());
            assert!(!compute_storage_builders().lock().unwrap().owner.slots.contains_key(&key),
                "retirement launch failure retained a builder slot");
        }
        build_begin(unsafe { &*(*port) }, &key).unwrap();
        let token = unsafe { (*port).token().unwrap() };
        unsafe {
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
    }

    #[test]
    fn compute_build_late() {
        let _cache = cache_test();
        let key = "4".repeat(64);
        let first = octra_compute_build_new();
        unsafe { assert_eq!(octra_compute_build_begin(first, key.as_ptr(), key.len()), 0); }
        let token = unsafe { (*first).token().unwrap() };
        let mut work = build_take(&token).unwrap();
        let retained = unsafe { octra_compute_build_retain(first) };
        unsafe {
            octra_compute_build_close(first);
            octra_compute_build_release(first);
        }
        let late = build_publish(&token, compute_entry("x"));
        assert!(matches!(late, Err(reason) if reason == "compute storage builder changed"),
            "closed build published a late image");
        assert!(!compute_storage_cache().lock().unwrap().entries.contains_key(&key));
        assert!(build_begin(unsafe { &*(*retained) }, &key).is_err());
        drop(work.data.take());
        drop(work);
        build_released(&token);
        unsafe { octra_compute_build_release(retained); }
    }

    #[test]
    fn compute_value_size() {
        for size in 0..96 {
            let value = vec![7; size];
            let encoded = BASE64.encode(&value);
            assert_eq!(b64_size(&encoded), size);
            assert_eq!(BASE64.decode(&encoded).unwrap(), value);
        }
        for value in ["", "=", "==", "==="] {
            assert_eq!(b64_size(value), 0, "padding caused size overflow");
        }
        assert_eq!(b64_size("===="), 1);
    }

    #[test]
    fn compute_value_limit() {
        let _cache = cache_test();
        let key = "8".repeat(64);
        let port = octra_compute_build_new();
        build_begin(unsafe { &*(*port) }, &key).unwrap();
        let token = unsafe { (*port).token().unwrap() };
        for size in [
            COMPUTE_STORAGE_VALUE_LIMIT + 1,
            COMPUTE_STORAGE_VALUE_LIMIT + 2,
            COMPUTE_STORAGE_VALUE_LIMIT + 3,
        ] {
            let payload: Payload = Payload::deserialize(json!({
                "compute_storage_key": "weights",
                "compute_storage_value_b64": BASE64.encode(vec![7; size]),
            })).unwrap();
            let (result, allocations) = native_bytes::tests::large_allocs(
                COMPUTE_STORAGE_VALUE_LIMIT,
                || run_compute_storage_put(&token, &payload),
            );
            assert_eq!(result.unwrap_err(), "compute storage value exceeds limit");
            assert_eq!(allocations, 0, "refused value allocated decode buffer");
            let table = compute_storage_builders().lock().unwrap();
            assert!(table.data[&key].storage.is_empty());
            assert_eq!(table.data[&key].weight, 0);
        }
        for value in ["A", "AA", "AAA", "====", "AA=A", "!!!!", "AB=="] {
            let payload: Payload = Payload::deserialize(json!({
                "compute_storage_key": "weights",
                "compute_storage_value_b64": value,
            })).unwrap();
            assert!(run_compute_storage_put(&token, &payload).is_err());
            assert!(compute_storage_builders().lock().unwrap().data[&key].storage.is_empty());
        }
        let value = vec![7; COMPUTE_STORAGE_VALUE_LIMIT];
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "weights",
            "compute_storage_value_b64": BASE64.encode(&value),
        })).unwrap();
        for _ in 0..2 {
            let reply = run_compute_storage_put(&token, &payload).unwrap();
            assert_eq!(reply["bytes"], COMPUTE_STORAGE_VALUE_LIMIT);
            assert_eq!(reply["entries"], 1);
        }
        let root = {
            let table = compute_storage_builders().lock().unwrap();
            assert_eq!(table.data[&key].storage["weights"].as_slice(), value.as_slice());
            compute_storage_root(&table.data[&key].storage)
        };
        unsafe {
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_seal", "compute_storage_root": root,
                "compute_storage_entries": 1,
                "compute_storage_bytes": COMPUTE_STORAGE_VALUE_LIMIT,
            })).0, 0);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(compute_storage_cache().lock().unwrap().entries[&key].storage["weights"].as_slice(), value.as_slice());
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[test]
    fn compute_put_entry_cap() {
        let _cache = cache_test();
        let key = "9".repeat(64);
        let port = octra_compute_build_new();
        build_begin(unsafe { &*(*port) }, &key).unwrap();
        let token = unsafe { (*port).token().unwrap() };
        for index in 0..COMPUTE_STORAGE_ENTRY_LIMIT {
            let payload: Payload = Payload::deserialize(json!({
                "compute_storage_key": index.to_string(),
                "compute_storage_value_b64": "",
            })).unwrap();
            assert_eq!(run_compute_storage_put(&token, &payload).unwrap()["entries"], index + 1);
        }
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "extra",
            "compute_storage_value_b64": BASE64.encode(vec![7; 4096]),
        })).unwrap();
        let (reply, allocations) = native_bytes::tests::large_allocs(
            4096, || run_compute_storage_put(&token, &payload),
        );
        assert_eq!(reply.unwrap_err(), "compute storage entry count exceeds limit");
        let repeat: Payload = Payload::deserialize(json!({
            "compute_storage_key": "0", "compute_storage_value_b64": "",
        })).unwrap();
        assert_eq!(run_compute_storage_put(&token, &repeat).unwrap()["entries"], COMPUTE_STORAGE_ENTRY_LIMIT);
        let root = {
            let table = compute_storage_builders().lock().unwrap();
            let builder = &table.data[&key];
            assert_eq!(builder.storage.len(), COMPUTE_STORAGE_ENTRY_LIMIT);
            assert_eq!(builder.weight, 0);
            compute_storage_root(&builder.storage)
        };
        unsafe {
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_seal", "compute_storage_root": root,
                "compute_storage_entries": COMPUTE_STORAGE_ENTRY_LIMIT, "compute_storage_bytes": 0,
            })).0, 0);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
        assert_eq!(allocations, 0, "full builder allocated decode buffer");
    }

    #[test]
    fn compute_put_finished() {
        let _cache = cache_test();
        let key = "a".repeat(64);
        let port = octra_compute_build_new();
        build_begin(unsafe { &*(*port) }, &key).unwrap();
        let token = unsafe { (*port).token().unwrap() };
        let root = compute_storage_root(&BTreeMap::new());
        unsafe {
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_seal", "compute_storage_root": root,
                "compute_storage_entries": 0, "compute_storage_bytes": 0,
            })).0, 0);
        }
        let payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "extra",
            "compute_storage_value_b64": BASE64.encode(vec![7; 4096]),
        })).unwrap();
        let (reply, allocations) = native_bytes::tests::large_allocs(
            4096, || run_compute_storage_put(&token, &payload),
        );
        assert_eq!(reply.unwrap_err(), "compute storage builder changed");
        unsafe {
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_put", "compute_storage_key": "extra",
                "compute_storage_value_b64": BASE64.encode(b"x"),
            })).0, 1);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert!(compute_storage_cache().lock().unwrap().entries[&key].storage.is_empty());
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
        assert_eq!(allocations, 0, "finished builder allocated decode buffer");
    }

    #[test]
    fn compute_put_byte_cap() {
        let _cache = cache_test();
        let key = "b".repeat(64);
        let port = octra_compute_build_new();
        build_begin(unsafe { &*(*port) }, &key).unwrap();
        let token = unsafe { (*port).token().unwrap() };
        let value = vec![7; COMPUTE_STORAGE_VALUE_LIMIT];
        let mut payload: Payload = Payload::deserialize(json!({
            "compute_storage_key": "0",
            "compute_storage_value_b64": BASE64.encode(&value),
        })).unwrap();
        for index in 0..COMPUTE_STORAGE_BYTE_LIMIT / value.len() {
            payload.compute_storage_key = Some(index.to_string());
            assert_eq!(run_compute_storage_put(&token, &payload).unwrap()["bytes"], (index + 1) * value.len());
        }
        let extra: Payload = Payload::deserialize(json!({
            "compute_storage_key": "extra",
            "compute_storage_value_b64": BASE64.encode(vec![7; 4096]),
        })).unwrap();
        let (reply, allocations) = native_bytes::tests::large_allocs(
            4096, || run_compute_storage_put(&token, &extra),
        );
        assert_eq!(reply.unwrap_err(), "compute storage bytes exceed limit");
        assert_eq!(run_compute_storage_put(&token, &payload).unwrap()["bytes"], COMPUTE_STORAGE_BYTE_LIMIT);
        let root = {
            let table = compute_storage_builders().lock().unwrap();
            let builder = &table.data[&key];
            assert_eq!(builder.weight, COMPUTE_STORAGE_BYTE_LIMIT);
            assert!(!builder.storage.contains_key("extra"));
            assert_eq!(builder.storage["0"].as_slice(), value.as_slice());
            compute_storage_root(&builder.storage)
        };
        unsafe {
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_seal", "compute_storage_root": root,
                "compute_storage_entries": COMPUTE_STORAGE_BYTE_LIMIT / value.len(),
                "compute_storage_bytes": COMPUTE_STORAGE_BYTE_LIMIT,
            })).0, 0);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        build_released(&token);
        assert_eq!(compute_storage_cache().lock().unwrap().entries[&key].storage["0"].as_slice(), value.as_slice());
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
        assert_eq!(allocations, 0, "byte-full builder allocated decode buffer");
    }

    #[test]
    fn compute_build_publish() {
        let _cache = cache_test();
        let key = "5".repeat(64);
        let entry = compute_entry("x");
        let port = octra_compute_build_new();
        unsafe {
            assert_eq!(octra_compute_build_begin(port, key.as_ptr(), key.len()), 0);
            for _ in 0..2 {
                assert_eq!(build_step_reply(port, json!({
                    "action": "compute_storage_put", "compute_storage_key": "weights",
                    "compute_storage_value_b64": BASE64.encode(b"x"),
                })).0, 0);
            }
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_put", "compute_storage_key": "weights",
                "compute_storage_value_b64": BASE64.encode(b"y"),
            })).0, 1);
            assert_eq!(build_step_reply(port, json!({
                "action": "compute_storage_seal", "compute_storage_root": entry.root,
                "compute_storage_entries": 1, "compute_storage_bytes": 1,
            })).0, 0);
            octra_compute_build_close(port);
            octra_compute_build_release(port);
        }
        assert_eq!(compute_storage_cache().lock().unwrap().entries[&key].storage["weights"].as_slice(), b"x");
        compute_cache_call(CacheMessage::Drop(key.clone())).unwrap();
        for action in ["compute_storage_begin", "compute_storage_put", "compute_storage_seal"] {
            assert!(inspect_input(&json!({"action": action, "compute_storage_cache_key": key})).is_err());
        }
    }

    fn compute_seed(state: &ComputeCache, key: &str, entry: &ComputeStorageEntry) -> ComputeCache {
        state.delta(CacheMessage::Seal(key.to_owned(), entry.clone())).unwrap().0
    }

    fn compute_hold(state: &ComputeCache, key: &str) -> (ComputeCache, u64) {
        let entry = state.entries.get(key).unwrap();
        let (state, reply) = state.delta(CacheMessage::Hold {
            key: key.to_owned(), root: entry.root.clone(),
            entries: entry.storage.len(), bytes: entry.weight,
        }).unwrap();
        match reply {
            CacheReply::Held(id) => (state, id),
            _ => panic!("hold reply differs"),
        }
    }

    #[test]
    fn compute_cache_leases() {
        let entry = compute_entry("x");
        let state = compute_seed(&ComputeCache::default(), "a", &entry);
        let (state, first) = compute_hold(&state, "a");
        let (state, second) = compute_hold(&state, "a");
        let (state, reply) = state.delta(CacheMessage::Drop("a".to_owned())).unwrap();
        assert!(matches!(reply, CacheReply::Busy));
        assert!(Arc::ptr_eq(&state.entries["a"].storage, &entry.storage));
        let state = state.delta(CacheMessage::Release(first)).unwrap().0;
        let repeated = state.delta(CacheMessage::Release(first)).unwrap().0;
        assert_eq!(state.leases, repeated.leases);
        assert!(repeated.held("a"));
        let state = repeated.delta(CacheMessage::Release(second)).unwrap().0;
        let (state, third) = compute_hold(&state, "a");
        assert!(third > second);
        let state = state.delta(CacheMessage::Release(second)).unwrap().0;
        assert!(state.held("a"));
        let state = state.delta(CacheMessage::Release(third)).unwrap().0;
        let (state, reply) = state.delta(CacheMessage::Drop("a".to_owned())).unwrap();
        assert!(matches!(reply, CacheReply::Missing));
        assert!(!state.entries.contains_key("a"));
    }

    #[test]
    fn compute_cache_pressure() {
        let entry = compute_entry("x");
        let state = compute_seed(&ComputeCache::default(), "a", &entry);
        let state = compute_seed(&state, "b", &entry);
        let (state, first) = compute_hold(&state, "a");
        let (state, second) = compute_hold(&state, "b");
        let error = state.delta(CacheMessage::Seal("c".to_owned(), entry.clone()));
        assert!(matches!(error, Err(reason) if reason == "compute storage cache busy"));
        assert_eq!(state.entries.len(), COMPUTE_STORAGE_CACHE_LIMIT);
        let state = state.delta(CacheMessage::Release(second)).unwrap().0;
        let state = compute_seed(&state, "c", &entry);
        assert!(state.entries.contains_key("a"));
        assert!(state.entries.contains_key("c"));
        assert!(!state.entries.contains_key("b"));
        let mut state = state.delta(CacheMessage::Release(first)).unwrap().0;
        for _ in 0..COMPUTE_STORAGE_LEASE_LIMIT {
            state = compute_hold(&state, "a").0;
        }
        let denied = state.delta(CacheMessage::Hold {
            key: "a".to_owned(), root: entry.root.clone(), entries: 1, bytes: 1,
        });
        assert!(matches!(denied, Err(reason) if reason == "compute storage leases busy"));
        assert_eq!(state.leases.len(), COMPUTE_STORAGE_LEASE_LIMIT);
    }

    #[test]
    fn compute_cache_identity() {
        let entry = compute_entry("x");
        let state = compute_seed(&ComputeCache::default(), "a", &entry);
        for (root, entries, bytes) in [
            ("0".repeat(64), 1, 1), (entry.root.clone(), 2, 1),
            (entry.root.clone(), 1, 2),
        ] {
            let denied = state.delta(CacheMessage::Hold {
                key: "a".to_owned(), root, entries, bytes,
            });
            assert!(matches!(denied, Err(reason) if reason == "compute storage cache identity differs"));
            assert!(state.leases.is_empty());
        }
        let (state, id) = compute_hold(&state, "a");
        let same = compute_seed(&state, "a", &compute_entry("x"));
        assert!(Arc::ptr_eq(&same.entries["a"].storage, &entry.storage));
        let denied = state.delta(CacheMessage::Seal("a".to_owned(), compute_entry("y")));
        assert!(matches!(denied, Err(reason) if reason == "compute storage cache busy"));
        let state = state.delta(CacheMessage::Release(id)).unwrap().0;
        let replaced = compute_seed(&state, "a", &compute_entry("y"));
        assert_ne!(replaced.entries["a"].root, entry.root);
        let exhausted = ComputeCache { next: i64::MAX as u64, ..state };
        let denied = exhausted.delta(CacheMessage::Hold {
            key: "a".to_owned(), root: entry.root, entries: 1, bytes: 1,
        });
        assert!(matches!(denied, Err(reason) if reason == "compute storage generation exhausted"));
    }

    #[test]
    fn compute_cache_capture() {
        let _cache = cache_test();
        let key = "2".repeat(64);
        let entry = compute_entry("x");
        compute_cache_call(CacheMessage::Seal(key.clone(), entry.clone())).unwrap();
        let input = serde_json::to_vec(&json!({
            "compute_storage_cache_key": key, "compute_storage_root": entry.root,
            "compute_storage_entries": 1, "compute_storage_bytes": 1,
        })).unwrap();
        let port = octra_compute_lease_new();
        unsafe {
            assert_eq!(octra_compute_lease_hold(port, input.as_ptr(), input.len()), 0);
            assert_eq!(octra_compute_lease_hold(port, input.as_ptr(), input.len()), 5);
        }
        let release = json!({"action": "compute_storage_release", "compute_storage_lease": 1});
        assert!(inspect_input(&release).is_err());
        let reply = compute_cache_call(CacheMessage::Drop(key.clone())).unwrap();
        assert!(matches!(reply, CacheReply::Busy));
        let replaced = compute_cache_call(CacheMessage::Seal(key.clone(), compute_entry("y")));
        assert!(matches!(replaced, Err(reason) if reason == "compute storage cache busy"));
        let payload: Payload = serde_json::from_slice(&input).unwrap();
        let captured = build_storage_from_payload(&payload, ExecutionProfile::Compute, true, None).unwrap();
        assert!(Arc::ptr_eq(&captured, &entry.storage));
        unsafe { octra_compute_lease_drop(port); }
        compute_cache_call(CacheMessage::Seal(key.clone(), compute_entry("y"))).unwrap();
        let next = build_storage_from_payload(&payload, ExecutionProfile::Compute, true, None).unwrap();
        assert_eq!(next["weights"].as_slice(), b"y");
        assert_eq!(captured["weights"].as_slice(), b"x");
        compute_cache_call(CacheMessage::Drop(key)).unwrap();
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    enum InspectStage {
        Startup,
        Alloc,
        Manifest,
        Unused,
    }

    fn session_module(stage: InspectStage) -> JsonValue {
        let key = b"inspect_case";
        let cold = b"{\"methods\":[]}";
        let warm = b"{\"methods\":1}";
        let probe = [
            I::I32Const(0), I::I32Const(key.len() as i32), I::Call(1),
            I::I32Const(-1), I::I32Eq, I::GlobalSet(0), I::I32Const(0),
            I::I32Const(key.len() as i32), I::I32Const(32), I::I32Const(1),
            I::Call(0), I::Drop,
        ];
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([ValType::I32; 2], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([], []);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", "host_session_put", EntityType::Function(0));
        imports.import("octra", "host_session_get_len", EntityType::Function(1));
        imports.import("octra", "host_response_write", EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(2).function(1).function(3);
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1,
            maximum: Some(1),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memory);
        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType { val_type: ValType::I32, mutable: true, shared: false },
            &ConstExpr::i32_const(1),
        );
        module.section(&globals);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 3);
        exports.export("octra_manifest", ExportKind::Func, 4);
        exports.export("octra_query", ExportKind::Func, 4);
        exports.export("octra_update", ExportKind::Func, 4);
        module.section(&exports);
        module.section(&StartSection { function_index: 5 });
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        if stage == InspectStage::Alloc {
            for op in &probe {
                alloc.instruction(op);
            }
        }
        alloc.instruction(&I::I32Const(4096)).instruction(&I::End);
        code.function(&alloc);
        let mut manifest = Function::new([]);
        if stage == InspectStage::Manifest {
            for op in &probe {
                manifest.instruction(op);
            }
        }
        for op in [
            I::GlobalGet(0), I::If(BlockType::Empty), I::I32Const(64),
            I::I32Const(cold.len() as i32), I::Call(2), I::Drop, I::Else,
            I::I32Const(128), I::I32Const(warm.len() as i32), I::Call(2), I::Drop,
            I::End, I::I32Const(0), I::End,
        ] {
            manifest.instruction(&op);
        }
        code.function(&manifest);
        let mut start = Function::new([]);
        if stage == InspectStage::Startup {
            for op in &probe {
                start.instruction(op);
            }
        }
        start.instruction(&I::End);
        code.function(&start);
        module.section(&code);
        let mut data = DataSection::new();
        for (offset, bytes) in [
            (0, key.as_slice()), (32, b"1".as_slice()),
            (64, cold.as_slice()), (128, warm.as_slice()),
        ] {
            data.active(0, &ConstExpr::i32_const(offset), bytes.iter().copied());
        }
        module.section(&data);
        json!({
            "action": "describe",
            "code_b64": encode_b64(&module.finish()),
            "execution_profile": "manifest",
            "hfhe_strict": false,
            "is_view": true,
            "inspect_policy": true,
            "fuel_limit": 1_000_000,
        })
    }

    fn inspect_input(input: &JsonValue) -> Result<JsonValue, String> {
        let bytes = serde_json::to_vec(input).unwrap();
        run_json(bytes.as_ptr(), bytes.len())
            .map(|raw| serde_json::from_str(&raw).unwrap()).map_err(|error| error.to_string())
    }

    #[test]
    fn inspect_memory() {
        let _guard = cache_test();
        let resident = compute_memory().used();
        for stage in [InspectStage::Unused, InspectStage::Startup] {
            for action in ["validate", "describe"] {
                let mut input = session_module(stage);
                input["action"] = json!(action);
                input.as_object_mut().unwrap().remove("is_view");
                let expected = inspect_input(&input);
                let held = compute_memory()
                    .take(COMPUTE_STORAGE_RESIDENT_LIMIT - resident).unwrap();
                assert_eq!(inspect_input(&input), expected, "inspection depends on local load");
                input["is_view"] = json!(false);
                assert_eq!(inspect_input(&input), expected);
                input["is_view"] = json!(true);
                assert!(inspect_input(&input).is_err());
                drop(held);
                assert_eq!(compute_memory().used(), resident);
            }
        }
    }

    fn encoded_type(ty: wasmi::core::ValType) -> ValType {
        match ty {
            wasmi::core::ValType::I32 => ValType::I32,
            wasmi::core::ValType::I64 => ValType::I64,
            wasmi::core::ValType::F32 => ValType::F32,
            wasmi::core::ValType::F64 => ValType::F64,
            _ => panic!("unexpected wasm host reference type"),
        }
    }

    #[test]
    fn inspect_imports() {
        let _guard = cache_test();
        let input: Payload =
            Payload::deserialize(session_module(InspectStage::Unused)).unwrap();
        let mut store = Store::new(
            shared_engine(),
            HostState::from_payload(&input, true).unwrap(),
        );
        let mut linker = Linker::new(shared_engine());
        define_imports(&mut store, &mut linker).unwrap();
        let names: BTreeSet<_> = ALLOWED_IMPORTS.iter().copied()
            .filter(|name| inspect_import(name))
            .collect();
        assert_eq!(
            names.len(),
            UPDATE_DISABLED_IMPORTS.len() + COMPUTE_IMPORTS.len(),
        );
        let mut module = WasmModule::new();
        let mut types = TypeSection::new();
        let mut imports = ImportSection::new();
        for (index, name) in names.iter().enumerate() {
            let func = linker.get(&store, "octra", name).unwrap().into_func().unwrap();
            let ty = func.ty(&store);
            types.ty().function(
                ty.params().iter().copied().map(encoded_type),
                ty.results().iter().copied().map(encoded_type),
            );
            imports.import("octra", name, EntityType::Function(index as u32));
        }
        module.section(&types).section(&imports);
        let bytes = module.finish();
        let module = Module::new(shared_engine(), &mut bytes.as_slice()).unwrap();
        seal_imports(&module, &mut store, &mut linker).unwrap();
        for name in names {
            store.data_mut().inspect_error = None;
            let func = linker.get(&store, "octra", name).unwrap().into_func().unwrap();
            let ty = func.ty(&store);
            let args: Vec<_> = ty.params().iter().copied()
                .map(wasmi::Val::default).collect();
            let mut result: Vec<_> = ty.results().iter().copied()
                .map(wasmi::Val::default).collect();
            let reason = format!("nonpersistent wasm import during inspection: {name}");
            assert_eq!(
                func.call(&mut store, &args, &mut result).unwrap_err().to_string(),
                reason,
            );
            assert_eq!(store.data().inspect_error.as_deref(), Some(reason.as_str()));
        }
    }

    #[test]
    fn inspect_sessions() {
        let _guard = cache_test();
        let key = "||inspect_case";
        for stage in [
            InspectStage::Startup, InspectStage::Alloc, InspectStage::Manifest,
        ] {
            cache_guard(session_cache()).remove(key);
            let mut input = session_module(stage);
            input["inspect_policy"] = json!(false);
            let cold = inspect_input(&input).unwrap();
            let warm = inspect_input(&input).unwrap();
            assert_ne!(cold["manifest"], warm["manifest"]);
            cache_guard(session_cache()).remove(key);
            input["inspect_policy"] = json!(true);
            assert_eq!(
                inspect_input(&input).unwrap_err(),
                "nonpersistent wasm import during inspection: host_session_get_len",
            );
            assert!(!cache_guard(session_cache()).contains_key(key));
            let expired = now_secs() - SESSION_CACHE_TTL_SECS - 1.0;
            cache_guard(session_cache()).insert(key.to_owned(), SessionCacheEntry {
                value: Arc::new(vec![7].into()), updated_at: expired,
            });
            assert_eq!(
                inspect_input(&input).unwrap_err(),
                "nonpersistent wasm import during inspection: host_session_get_len",
            );
            let unchanged = cache_guard(session_cache()).get(key)
                .map(|entry| (entry.value.clone(), entry.updated_at));
            assert_eq!(unchanged, Some((Arc::new(vec![7].into()), expired)));
            cache_guard(session_cache()).remove(key);
        }
    }

    #[test]
    fn inspect_update_start() {
        let _guard = cache_test();
        let key = "circle|caller|inspect_case";
        cache_guard(session_cache()).remove(key);
        let mut input = session_module(InspectStage::Startup);
        input["action"] = json!("execute");
        input["export_name"] = json!("octra_update");
        input["request_b64"] = json!("");
        input["address"] = json!("circle");
        input["caller"] = json!("caller");
        input["is_view"] = json!(false);
        input["update_policy"] = json!(true);
        input["inspect_policy"] = json!(false);
        let prior = inspect_input(&input).unwrap_err();
        assert!(cache_guard(session_cache()).contains_key(key));
        cache_guard(session_cache()).remove(key);
        input["inspect_policy"] = json!(true);
        for policy in [false, true] {
            input["update_policy"] = json!(policy);
            assert_eq!(inspect_input(&input).unwrap_err(), prior);
            assert!(
                !cache_guard(session_cache()).contains_key(key),
                "rejected update executed session startup",
            );
        }
    }

    #[test]
    fn inspect_validate() {
        let _guard = cache_test();
        let mut input = session_module(InspectStage::Startup);
        input["action"] = json!("validate");
        assert_eq!(
            inspect_input(&input).unwrap_err(),
            "nonpersistent wasm import during inspection: host_session_get_len",
        );
    }

    #[test]
    fn inspect_controls() {
        let _guard = cache_test();
        let input = session_module(InspectStage::Unused);
        assert!(inspect_input(&input).unwrap()["manifest"].is_object());
        let mut input = session_module(InspectStage::Manifest);
        input["action"] = json!("execute");
        input["export_name"] = json!("octra_query");
        input["request_b64"] = json!("");
        input["address"] = json!("inspect_view");
        input["caller"] = json!("caller");
        for profile in ["standard", "compute"] {
            input["execution_profile"] = json!(profile);
            let scope = "a".repeat(64);
            if profile == "compute" {
                input["compute_session_scope"] = json!(scope);
            }
            let payload: Payload = Payload::deserialize(input.clone()).unwrap();
            let state = HostState::from_payload(&payload, false).unwrap();
            let key = session_namespaced_key(&state, "inspect_case");
            let cache = session_store(state.local_sessions);
            cache_guard(cache).remove(&key);
            let cold = inspect_input(&input).unwrap();
            let warm = inspect_input(&input).unwrap();
            assert_eq!(cold["success"], true);
            assert_eq!(warm["success"], true);
            assert_ne!(cold["response_b64"], warm["response_b64"]);
            cache_guard(cache).remove(&key);
        }
    }

    #[test]
    fn empty_input() {
        for (input, size) in [
            (std::ptr::null(), 0),
            (std::ptr::null(), 1),
            (b"".as_ptr(), 0),
            (b"{}".as_ptr(), 2),
        ] {
            let mut output = std::ptr::null_mut();
            let mut output_size = 0;
            let mut error = std::ptr::null_mut();
            let mut error_size = 0;
            let status = octra_circle_wasm_host_run_json(
                input, size, &mut output, &mut output_size, &mut error, &mut error_size,
                std::ptr::null_mut(),
            );
            assert_eq!(status, 1);
            assert!(output.is_null());
            assert_eq!(output_size, 0);
            assert!(!error.is_null());
            assert!(error_size > 0);
            octra_circle_wasm_host_free_bytes(error, error_size);
        }
    }

    #[test]
    fn execution_mode_is_explicit() {
        for fields in [
            json!({}),
            json!({"is_view": true}),
            json!({"hfhe_strict": true}),
        ] {
            let payload: Payload = Payload::deserialize(fields).unwrap();
            assert!(HostState::from_payload(&payload, false).is_err());
            let state = HostState::from_payload(&payload, true).unwrap();
            assert!(state.is_view);
            assert!(!state.hfhe_strict);
        }
        for strict in [false, true] {
            for view in [false, true] {
                let payload: Payload = Payload::deserialize(json!({
                    "hfhe_strict": strict,
                    "is_view": view
                }))
                .unwrap();
                let state = HostState::from_payload(&payload, false).unwrap();
                assert_eq!((state.hfhe_strict, state.is_view), (strict, view));
            }
        }
    }

    #[test]
    fn math_mode() {
        for math in [false, true] {
            let payload: Payload = Payload::deserialize(json!({
                "hfhe_strict": true,
                "is_view": true,
                "math": math
            }))
            .unwrap();
            assert_eq!(HostState::from_payload(&payload, false).unwrap().math, math);
        }
        let payload: Payload = Payload::deserialize(json!({
            "hfhe_strict": true,
            "is_view": true
        }))
        .unwrap();
        assert!(!HostState::from_payload(&payload, false).unwrap().math);
        assert!(Payload::deserialize(json!({"math": "true"})).is_err());
    }

    fn entry(
        method: &str,
        request: u8,
        response: u8,
        result: Option<bool>,
    ) -> HfheReceiptEntryJson {
        HfheReceiptEntryJson {
            method: method.to_owned(),
            request_hash: format!("{request:064x}"),
            response_hash: format!("{response:064x}"),
            result,
        }
    }

    fn manifest_module(body: &[u8]) -> Vec<u8> {
        let mut wasm = vec![
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0c, 0x02, 0x60, 0x01, 0x7f,
            0x01, 0x7f, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f, 0x03, 0x03, 0x02, 0x00, 0x01, 0x05,
            0x03, 0x01, 0x00, 0x01, 0x07, 0x37, 0x04, 0x06, b'm', b'e', b'm', b'o', b'r', b'y',
            0x02, 0x00, 0x0b, b'o', b'c', b't', b'r', b'a', b'_', b'a', b'l', b'l', b'o', b'c',
            0x00, 0x00, 0x0e, b'o', b'c', b't', b'r', b'a', b'_', b'm', b'a', b'n', b'i', b'f',
            b'e', b's', b't', 0x00, 0x01, 0x0b, b'o', b'c', b't', b'r', b'a', b'_', b'q', b'u',
            b'e', b'r', b'y', 0x00, 0x01, 0x0a, (7 + body.len()) as u8, 0x02, 0x04, 0x00, 0x41,
            0x00, 0x0b, body.len() as u8,
        ];
        wasm.extend_from_slice(body);
        wasm
    }

    fn inspect_manifest(action: &str, wasm: &[u8]) -> JsonValue {
        let input = serde_json::json!({
            "action": action,
            "code_b64": BASE64.encode(wasm),
            "execution_profile": "manifest",
            "is_view": true
        })
        .to_string();
        let output = run_json(input.as_ptr(), input.len()).expect("manifest inspection rejected");
        serde_json::from_str(&output).expect("invalid manifest inspection output")
    }

    #[test]
    fn manifest_budget() {
        assert_eq!(
            ExecutionProfile::Manifest.fuel(Some(COMPUTE_MAX_FUEL_LIMIT)),
            MAX_FUEL_LIMIT
        );
        assert_eq!(
            ExecutionProfile::Manifest.max_tensor_session_bytes(),
            MAX_TENSOR_SESSION_BYTES
        );
        assert_ne!(ExecutionProfile::Manifest, ExecutionProfile::Compute);
    }

    #[test]
    fn manifest_export_failures() {
        let cases = [
            manifest_module(&[0x00, 0x00, 0x0b]),
            manifest_module(&[0x00, 0x41, 0x00, 0x0b]),
            manifest_module(&[0x00, 0x41, 0x01, 0x0b]),
        ];
        for wasm in cases {
            let admitted = inspect_manifest("validate", &wasm);
            let loaded = inspect_manifest("describe", &wasm);
            assert!(admitted["manifest"].is_object());
            assert!(loaded["manifest"].is_null());
            assert_eq!(loaded["exports"], admitted["exports"]);
        }
    }

    #[test]
    fn receipt_input_shapes() {
        let valid = entry("fhe_verify_zero", 1, 2, Some(true));
        assert!(validate_hfhe_receipt_input("consume", &[valid.clone()]).is_ok());
        assert!(validate_hfhe_receipt_input("capture", &[valid.clone()]).is_err());
        assert!(
            validate_hfhe_receipt_input("consume", &[entry("fhe_verify_zero", 1, 2, None)])
                .is_err()
        );
        assert!(
            validate_hfhe_receipt_input("consume", &[entry("fhe_commit", 1, 2, Some(true))])
                .is_err()
        );
    }

    #[test]
    fn receipt_request_identity() {
        let expected = entry("fhe_verify_zero", 1, 2, Some(true));
        assert!(
            match_hfhe_receipt_request(&expected, "fhe_verify_zero", &expected.request_hash)
                .is_ok()
        );
        assert!(
            match_hfhe_receipt_request(&expected, "fhe_verify_bound", &expected.request_hash)
                .is_err()
        );
        assert!(
            match_hfhe_receipt_request(&expected, "fhe_verify_zero", &format!("{:064x}", 3))
                .is_err()
        );
    }

    #[test]
    fn receipt_result_identity() {
        let expected = entry("fhe_verify_zero", 1, 2, Some(true));
        assert!(match_hfhe_receipt_entry(&expected, &expected).is_ok());
        assert!(
            match_hfhe_receipt_entry(&expected, &entry("fhe_verify_zero", 1, 3, Some(true)))
                .is_err()
        );
        assert!(
            match_hfhe_receipt_entry(&expected, &entry("fhe_verify_zero", 1, 2, Some(false)))
                .is_err()
        );
    }

    #[test]
    fn receipt_mode_domains() {
        let prior = hfhe_receipt_entry(false, "fhe_verify_bound", &[1], &frame_bool(true))
            .expect("prior receipt failed");
        let active = hfhe_receipt_entry(true, "fhe_verify_bound", &[1], &frame_bool(true))
            .expect("active receipt failed");
        assert_eq!(
            prior.request_hash,
            "a8624ddf6ccf2769976891fdf5102bdf43a6d8c6aff2f6140804d7a40168f213"
        );
        assert_ne!(prior.request_hash, active.request_hash);
        assert_ne!(prior.response_hash, active.response_hash);
        assert!(
            match_hfhe_receipt_request(&prior, "fhe_verify_bound", &active.request_hash).is_err()
        );
    }

    #[test]
    fn storage_cache_root_limit() {
        let mut cache = HashMap::new();
        for index in 0..32 {
            let key = format!("oct{index:044}:root{index}");
            prepare_storage_cache_insert(&mut cache, &key, 1024);
            cache.insert(
                key,
                StorageCacheEntry {
                    storage: Arc::new(Storage::default()),
                    weight: 1024,
                    updated_at: index as f64,
                },
            );
        }
        assert!(cache.len() <= STORAGE_CACHE_LIMIT);
        assert!(storage_cache_weight(&cache) <= STORAGE_CACHE_BYTE_LIMIT);
        for index in 0..4 {
            let key = format!("oct{:044}:large{index}", 100 + index);
            let weight = 20 * 1024 * 1024;
            prepare_storage_cache_insert(&mut cache, &key, weight);
            cache.insert(
                key,
                StorageCacheEntry {
                    storage: Arc::new(Storage::default()),
                    weight,
                    updated_at: (50 + index) as f64,
                },
            );
        }
        assert!(storage_cache_weight(&cache) <= STORAGE_CACHE_BYTE_LIMIT);
        for index in 0..8 {
            let key = format!("oct{:044}:root{index}", 1);
            prepare_storage_cache_insert(&mut cache, &key, 1024);
            cache.insert(
                key,
                StorageCacheEntry {
                    storage: Arc::new(Storage::default()),
                    weight: 1024,
                    updated_at: (100 + index) as f64,
                },
            );
        }
        let circle_prefix = format!("oct{:044}:", 1);
        let circle_roots = cache
            .keys()
            .filter(|key| key.starts_with(&circle_prefix))
            .count();
        assert_eq!(circle_roots, 1);
    }

    #[test]
    fn compute_session_expiry() {
        let _guard = cache_test();
        let live_key = "compute-session-case-live".to_owned();
        let expired_key = "compute-session-case-expired".to_owned();
        {
            let mut cache = cache_guard(session_cache());
            cache.insert(
                live_key.clone(),
                SessionCacheEntry {
                    value: Arc::new(vec![1].into()),
                    updated_at: now_secs(),
                },
            );
            cache.insert(
                expired_key.clone(),
                SessionCacheEntry {
                    value: Arc::new(vec![2].into()),
                    updated_at: now_secs() - SESSION_CACHE_TTL_SECS - 1.0,
                },
            );
        }

        maintain_compute_session_caches();

        let mut cache = cache_guard(session_cache());
        assert_eq!(
            cache.get(&live_key).map(|entry| entry.value.as_slice()),
            Some(&[1][..])
        );
        assert!(!cache.contains_key(&expired_key));
        cache.remove(&live_key);
    }

    #[test]
    fn compute_session_cleanup() {
        let _guard = cache_test();
        let prefix = "circle|caller|session-a|";
        let scoped_key = format!("{prefix}meta");
        let other_key = "circle|caller|session-b|meta".to_owned();
        {
            let mut cache = cache_guard(session_cache());
            cache.insert(
                scoped_key.clone(),
                SessionCacheEntry {
                    value: Arc::new(vec![1].into()),
                    updated_at: now_secs(),
                },
            );
            cache.insert(
                other_key.clone(),
                SessionCacheEntry {
                    value: Arc::new(vec![2].into()),
                    updated_at: now_secs(),
                },
            );
        }

        clear_compute_session_prefix(prefix, None).expect("session cleanup");

        let mut cache = cache_guard(session_cache());
        assert!(!cache.contains_key(&scoped_key));
        assert_eq!(
            cache.get(&other_key).map(|entry| entry.value.as_slice()),
            Some(&[2][..])
        );
        cache.remove(&other_key);
    }
}