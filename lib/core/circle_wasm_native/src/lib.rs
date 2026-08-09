// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use base64::{engine::general_purpose::STANDARD as BASE64, Engine as _};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::slice;
use std::sync::{Arc, Mutex, MutexGuard, OnceLock};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use wasmi::{
    AsContext, AsContextMut, Caller, Config, Engine, Error as WasmiError, Extern, ExternType, Func,
    Instance, Linker, Module, Store, StoreLimits, StoreLimitsBuilder,
};

mod deterministic_tensor;

const MAX_RESPONSE_BYTES: usize = 2 * 1024 * 1024;
const MAX_SPAWN_PAYLOAD_JSON_BYTES: usize = 2 * 1024 * 1024;
const MAX_ASSET_EFFECT_BODY_B64_BYTES: usize = 1_398_104;
const MAX_GUEST_READ_BYTES: usize = 16 * 1024 * 1024;
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
const COMPUTE_STORAGE_BUILDER_LIMIT: usize = 2;
const COMPUTE_STORAGE_ENTRY_LIMIT: usize = 1024;
const COMPUTE_STORAGE_VALUE_LIMIT: usize = 8 * 1024 * 1024;
const COMPUTE_STORAGE_BYTE_LIMIT: usize = 512 * 1024 * 1024;
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
struct Payload {
    action: Option<String>,
    code_b64: Option<String>,
    code_cache_key: Option<String>,
    export_name: Option<String>,
    request_b64: Option<String>,
    storage_cache_key: Option<String>,
    storage_pairs: Option<Vec<StoragePairJson>>,
    caller: Option<String>,
    address: Option<String>,
    tx_hash: Option<String>,
    current_epoch: Option<i64>,
    hfhe_caps: Option<Vec<String>>,
    hfhe_pubkeys: Option<Vec<HfhePubkeyJson>>,
    hfhe_active_key: Option<HfheActiveKey>,
    hfhe_receipt_mode: Option<String>,
    hfhe_receipt_entries: Option<Vec<HfheReceiptEntryJson>>,
    public_reads: Option<Vec<PublicReadJson>>,
    is_view: Option<bool>,
    update_policy: Option<bool>,
    fuel_limit: Option<u64>,
    execution_profile: Option<String>,
    compute_storage_cache_key: Option<String>,
    compute_storage_key: Option<String>,
    compute_storage_value_b64: Option<String>,
    compute_storage_root: Option<String>,
    compute_storage_entries: Option<usize>,
    compute_storage_bytes: Option<usize>,
    compute_session_scope: Option<String>,
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
    execution_profile: ExecutionProfile,
    max_guest_bytes: usize,
    max_tensor_session_bytes: usize,
    storage: Arc<BTreeMap<String, Vec<u8>>>,
    hfhe_caps: BTreeSet<String>,
    hfhe_pubkeys: HashMap<String, String>,
    hfhe_active_key: Option<HfheActiveKey>,
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
    response_bytes: Vec<u8>,
    response_status: i32,
    events: Vec<EventJson>,
    event_bytes: usize,
    spawns: Vec<SpawnJson>,
    assets: Vec<AssetPutJson>,
    encrypted_assets: Vec<EncryptedAssetPutJson>,
    circle_invoke_cache: Option<(Vec<u8>, Vec<u8>)>,
    hfhe_invoke_cache: Option<(Vec<u8>, Vec<u8>)>,
    unavailable: Option<String>,
    compute_trace: Option<ComputeTrace>,
    compute_session_scope: Option<String>,
    compute_started_at: Instant,
    q24_calls: usize,
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
    value: Vec<u8>,
    updated_at: f64,
}

#[derive(Debug, Clone)]
struct TensorSessionEntry {
    n_layers: usize,
    max_t: usize,
    kv_dim: usize,
    k_cache: Vec<f64>,
    v_cache: Vec<f64>,
    updated_at: f64,
}

#[derive(Debug, Clone)]
struct FixedTensorSessionEntry {
    n_layers: usize,
    max_t: usize,
    kv_dim: usize,
    k_cache: Vec<i64>,
    v_cache: Vec<i64>,
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
    storage: Arc<BTreeMap<String, Vec<u8>>>,
    weight: usize,
    updated_at: f64,
}

#[derive(Debug)]
struct ComputeStorageBuilder {
    storage: BTreeMap<String, Vec<u8>>,
    weight: usize,
    updated_at: f64,
}

#[derive(Debug, Clone)]
struct ComputeStorageEntry {
    storage: Arc<BTreeMap<String, Vec<u8>>>,
    root: String,
    weight: usize,
    updated_at: f64,
}

fn storage_cache() -> &'static Mutex<HashMap<String, StorageCacheEntry>> {
    static STORAGE_CACHE: OnceLock<Mutex<HashMap<String, StorageCacheEntry>>> = OnceLock::new();
    STORAGE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
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

fn compute_storage_builders() -> &'static Mutex<HashMap<String, ComputeStorageBuilder>> {
    static COMPUTE_STORAGE_BUILDERS: OnceLock<Mutex<HashMap<String, ComputeStorageBuilder>>> =
        OnceLock::new();
    COMPUTE_STORAGE_BUILDERS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn compute_storage_cache() -> &'static Mutex<HashMap<String, ComputeStorageEntry>> {
    static COMPUTE_STORAGE_CACHE: OnceLock<Mutex<HashMap<String, ComputeStorageEntry>>> =
        OnceLock::new();
    COMPUTE_STORAGE_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

fn session_cache() -> &'static Mutex<HashMap<String, SessionCacheEntry>> {
    static SESSION_CACHE: OnceLock<Mutex<HashMap<String, SessionCacheEntry>>> = OnceLock::new();
    SESSION_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
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
) -> i32 {
    let result = std::panic::catch_unwind(|| run_json(input_ptr, input_len));
    match result {
        Ok(Ok(output)) => unsafe {
            write_owned_bytes(output.into_bytes(), out_ptr, out_len);
            if !err_ptr.is_null() {
                *err_ptr = std::ptr::null_mut();
            }
            if !err_len.is_null() {
                *err_len = 0;
            }
            0
        },
        Ok(Err(error)) => unsafe {
            write_owned_bytes(error.into_bytes(), err_ptr, err_len);
            if !out_ptr.is_null() {
                *out_ptr = std::ptr::null_mut();
            }
            if !out_len.is_null() {
                *out_len = 0;
            }
            1
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
        drop(Vec::from_raw_parts(ptr, len, len));
    }
}

fn run_json(input_ptr: *const u8, input_len: usize) -> Result<String, String> {
    if input_ptr.is_null() && input_len != 0 {
        return Err("octra_circle_wasm_host: null input".to_owned());
    }
    let input = unsafe { slice::from_raw_parts(input_ptr, input_len) };
    let input_text = std::str::from_utf8(input)
        .map_err(|e| format!("octra_circle_wasm_host: invalid utf8 input: {e}"))?;
    let payload: Payload = serde_json::from_str(input_text)
        .map_err(|e| format!("octra_circle_wasm_host: invalid json input: {e}"))?;
    let action = payload.action.as_deref().unwrap_or("");
    let output = match action {
        "validate" => run_validate(&payload)?,
        "describe" => run_describe(&payload)?,
        "execute" => run_execute(&payload)?,
        "compute_storage_begin" => run_compute_storage_begin(&payload)?,
        "compute_storage_put" => run_compute_storage_put(&payload)?,
        "compute_storage_seal" => run_compute_storage_seal(&payload)?,
        "compute_storage_status" => run_compute_storage_status(&payload)?,
        "compute_storage_drop" => run_compute_storage_drop(&payload)?,
        "compute_self_test" => run_compute_self_test()?,
        _ => {
            return Err(format!(
                "octra_circle_wasm_host: unsupported action: {}",
                if action.is_empty() { "<none>" } else { action }
            ));
        }
    };
    serde_json::to_string(&output)
        .map_err(|e| format!("octra_circle_wasm_host: json encode failed: {e}"))
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
    let executor_root = executor.finalize();
    Ok(json!({
        "evidence_root": hex::encode(evidence_root),
        "executor_root": hex::encode(executor_root),
        "profile": "q24-v1"
    }))
}

fn compute_storage_key(payload: &Payload) -> Result<&str, String> {
    let key = payload
        .compute_storage_cache_key
        .as_deref()
        .ok_or_else(|| "missing compute storage cache key".to_owned())?;
    normalize_hex64(key, "compute storage cache key")?;
    Ok(key)
}

fn remove_oldest_compute_builder(builders: &mut HashMap<String, ComputeStorageBuilder>) {
    let oldest = builders
        .iter()
        .min_by(|left, right| left.1.updated_at.total_cmp(&right.1.updated_at))
        .map(|(key, _)| key.clone());
    if let Some(key) = oldest {
        builders.remove(&key);
    }
}

fn remove_oldest_compute_storage(cache: &mut HashMap<String, ComputeStorageEntry>) {
    let oldest = cache
        .iter()
        .min_by(|left, right| left.1.updated_at.total_cmp(&right.1.updated_at))
        .map(|(key, _)| key.clone());
    if let Some(key) = oldest {
        cache.remove(&key);
    }
}

fn compute_storage_root(storage: &BTreeMap<String, Vec<u8>>) -> String {
    let mut hasher = Sha256::new();
    hasher.update(b"octra:compute_storage_cache:v1\0");
    for (key, value) in storage {
        hasher.update((key.len() as u32).to_be_bytes());
        hasher.update(key.as_bytes());
        hasher.update((value.len() as u64).to_be_bytes());
        hasher.update(Sha256::digest(value));
    }
    hex::encode(hasher.finalize())
}

fn run_compute_storage_begin(payload: &Payload) -> Result<JsonValue, String> {
    let key = compute_storage_key(payload)?.to_owned();
    let mut builders = compute_storage_builders()
        .lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    if !builders.contains_key(&key) && builders.len() >= COMPUTE_STORAGE_BUILDER_LIMIT {
        remove_oldest_compute_builder(&mut builders);
    }
    builders.insert(
        key.clone(),
        ComputeStorageBuilder {
            storage: BTreeMap::new(),
            weight: 0,
            updated_at: now_secs(),
        },
    );
    Ok(json!({"cache_key": key, "status": "building"}))
}

fn run_compute_storage_put(payload: &Payload) -> Result<JsonValue, String> {
    let cache_key = compute_storage_key(payload)?.to_owned();
    let key = payload
        .compute_storage_key
        .as_deref()
        .ok_or_else(|| "missing compute storage key".to_owned())?;
    if key.is_empty() || key.len() > 256 || key.as_bytes().contains(&0) {
        return Err("invalid compute storage key".to_owned());
    }
    let value = decode_b64(
        "compute storage value",
        payload
            .compute_storage_value_b64
            .as_deref()
            .ok_or_else(|| "missing compute storage value".to_owned())?,
    )?;
    if value.len() > COMPUTE_STORAGE_VALUE_LIMIT {
        return Err("compute storage value exceeds limit".to_owned());
    }
    let mut builders = compute_storage_builders()
        .lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?;
    let builder = builders
        .get_mut(&cache_key)
        .ok_or_else(|| format!("missing compute storage builder: {cache_key}"))?;
    match builder.storage.get(key) {
        Some(current) if current == &value => {}
        Some(_) => return Err("compute storage key differs".to_owned()),
        None => {
            if builder.storage.len() >= COMPUTE_STORAGE_ENTRY_LIMIT {
                return Err("compute storage entry count exceeds limit".to_owned());
            }
            let next_weight = builder
                .weight
                .checked_add(value.len())
                .ok_or_else(|| "compute storage bytes overflow".to_owned())?;
            if next_weight > COMPUTE_STORAGE_BYTE_LIMIT {
                return Err("compute storage bytes exceed limit".to_owned());
            }
            builder.storage.insert(key.to_owned(), value);
            builder.weight = next_weight;
        }
    }
    builder.updated_at = now_secs();
    Ok(json!({
        "bytes": builder.weight,
        "cache_key": cache_key,
        "entries": builder.storage.len(),
        "status": "building",
    }))
}

fn run_compute_storage_seal(payload: &Payload) -> Result<JsonValue, String> {
    let cache_key = compute_storage_key(payload)?.to_owned();
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
    let builder = compute_storage_builders()
        .lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?
        .remove(&cache_key)
        .ok_or_else(|| format!("missing compute storage builder: {cache_key}"))?;
    let actual_root = compute_storage_root(&builder.storage);
    if builder.storage.len() != expected_entries
        || builder.weight != expected_bytes
        || actual_root != expected_root
    {
        return Err("compute storage seal differs".to_owned());
    }
    let mut cache = compute_storage_cache()
        .lock()
        .map_err(|_| "compute storage cache poisoned".to_owned())?;
    if !cache.contains_key(&cache_key) && cache.len() >= COMPUTE_STORAGE_CACHE_LIMIT {
        remove_oldest_compute_storage(&mut cache);
    }
    cache.insert(
        cache_key.clone(),
        ComputeStorageEntry {
            storage: Arc::new(builder.storage),
            root: actual_root.clone(),
            weight: builder.weight,
            updated_at: now_secs(),
        },
    );
    Ok(json!({
        "bytes": expected_bytes,
        "cache_key": cache_key,
        "entries": expected_entries,
        "root": actual_root,
        "status": "ready",
    }))
}

fn run_compute_storage_status(payload: &Payload) -> Result<JsonValue, String> {
    let cache_key = compute_storage_key(payload)?.to_owned();
    let mut cache = compute_storage_cache()
        .lock()
        .map_err(|_| "compute storage cache poisoned".to_owned())?;
    match cache.get_mut(&cache_key) {
        Some(entry) => {
            entry.updated_at = now_secs();
            Ok(json!({
                "bytes": entry.weight,
                "cache_key": cache_key,
                "entries": entry.storage.len(),
                "root": entry.root,
                "status": "ready",
            }))
        }
        None => Ok(json!({"cache_key": cache_key, "status": "missing"})),
    }
}

fn run_compute_storage_drop(payload: &Payload) -> Result<JsonValue, String> {
    let cache_key = compute_storage_key(payload)?.to_owned();
    compute_storage_builders()
        .lock()
        .map_err(|_| "compute storage builder poisoned".to_owned())?
        .remove(&cache_key);
    compute_storage_cache()
        .lock()
        .map_err(|_| "compute storage cache poisoned".to_owned())?
        .remove(&cache_key);
    Ok(json!({"cache_key": cache_key, "status": "missing"}))
}

fn run_validate(payload: &Payload) -> Result<JsonValue, String> {
    let runtime = Runtime::instantiate(payload, true)?;
    Ok(json!({
        "exports": runtime.exports,
        "manifest": manifest_from_exports(&runtime.exports),
    }))
}

fn run_describe(payload: &Payload) -> Result<JsonValue, String> {
    let mut runtime = Runtime::instantiate(payload, true)?;
    let manifest = if runtime
        .instance
        .get_func(&runtime.store, "octra_manifest")
        .is_some()
    {
        let (code, response_bytes, _effort_used) = runtime
            .call_export("octra_manifest", &[])
            .map_err(|error| format!("wasm manifest execution failed: {error}"))?;
        if code != 0 {
            return Err(format!("wasm manifest returned {code}"));
        }
        let value = serde_json::from_slice::<JsonValue>(&response_bytes)
            .map_err(|error| format!("invalid wasm manifest json: {error}"))?;
        if !value.is_object() {
            return Err("wasm manifest must be an object".to_owned());
        }
        Some(value)
    } else {
        None
    };
    Ok(json!({
        "exports": runtime.exports,
        "manifest": manifest,
    }))
}

fn run_execute(payload: &Payload) -> Result<JsonValue, String> {
    let export_name = payload
        .export_name
        .as_deref()
        .ok_or_else(|| "missing export_name".to_owned())?;
    let request_b64 = payload
        .request_b64
        .as_deref()
        .ok_or_else(|| "missing request_b64".to_owned())?;
    let request_bytes = decode_b64("request_b64", request_b64)?;
    let mut runtime = Runtime::instantiate(payload, false)?;
    finish_execute(&mut runtime, export_name, &request_bytes)
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
    json!({
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
    })
}

fn finish_execute(
    runtime: &mut Runtime,
    export_name: &str,
    request_bytes: &[u8],
) -> Result<JsonValue, String> {
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
                    Vec::new(),
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
    Ok(execute_output(
        runtime,
        code,
        success,
        &response_bytes,
        effort_used,
        error,
    ))
}

impl Runtime {
    fn instantiate(payload: &Payload, description_only: bool) -> Result<Self, String> {
        let profile = ExecutionProfile::of_payload(payload)?;
        if payload.compute_session_scope.is_some() && profile != ExecutionProfile::Compute {
            return Err("compute session scope requires compute execution profile".to_owned());
        }
        let code_b64_opt = payload.code_b64.as_deref();
        let module_key = match payload.code_cache_key.as_deref() {
            Some(key) if !key.is_empty() => key.to_owned(),
            _ => {
                let code_b64 = code_b64_opt.ok_or_else(|| "missing code_b64".to_owned())?;
                let mut hasher = Sha256::new();
                hasher.update(code_b64.as_bytes());
                format!("{:x}", hasher.finalize())
            }
        };
        let (module, exports, has_update_disabled_imports, has_compute_imports) = {
            let mut guard = cache_guard(module_cache());
            prune_module_cache_locked(&mut guard);
            if let Some(entry) = guard.get_mut(&module_key) {
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
                    module_key.clone(),
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

        if profile == ExecutionProfile::Standard && has_compute_imports {
            let name = module
                .imports()
                .find(|entry| COMPUTE_IMPORTS.contains(&entry.name()))
                .map(|entry| entry.name().to_owned())
                .unwrap_or_else(|| "<unknown>".to_owned());
            return Err(format!("unsupported wasm import: {name}"));
        }

        let mut store = Store::new(shared_engine(), HostState::from_payload(payload)?);
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
        store.limiter(|state| &mut state.limits);
        let fuel_limit = profile.fuel(payload.fuel_limit);
        store
            .set_fuel(fuel_limit)
            .map_err(|e| format!("wasm fuel setup failed: {e}"))?;
        if description_only {
            let data = store.data_mut();
            Arc::make_mut(&mut data.storage).clear();
            data.caller_bytes.clear();
            data.self_bytes.clear();
            data.current_epoch = 0;
            data.is_view = true;
        }

        let mut linker = Linker::new(shared_engine());
        define_imports(&mut store, &mut linker)?;
        let instance = linker
            .instantiate(&mut store, &module)
            .map_err(|e| format!("wasm instantiate failed: {e}"))?
            .start(&mut store)
            .map_err(|e| format!("wasm start failed: {e}"))?;
        let memory = instance
            .get_memory(&store, "memory")
            .ok_or_else(|| "wasm_v1 memory export missing".to_owned())?;
        let initial_pages = memory.data_size(&store) / PAGE_BYTES;
        if initial_pages > profile.max_initial_pages() {
            return Err(format!(
                "wasm_v1 memory exceeds {} pages",
                profile.max_initial_pages()
            ));
        }
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
    ) -> Result<(i32, Vec<u8>, u64), String> {
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
            .map_err(|e| format!("octra_alloc failed: {e}"))?;
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
            .map_err(|e| format!("wasm export trapped: {e}"))?;
        let response_bytes = self.store.data().response_bytes.clone();
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

impl HostState {
    fn from_payload(payload: &Payload) -> Result<Self, String> {
        let execution_profile = ExecutionProfile::of_payload(payload)?;
        let storage = build_storage_from_payload(payload, execution_profile)?;
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
                .memory_size(execution_profile.max_initial_pages() * PAGE_BYTES)
                .memories(1)
                .tables(1)
                .instances(1)
                .trap_on_grow_failure(true)
                .build(),
            execution_profile,
            max_guest_bytes: execution_profile.max_guest_bytes(),
            max_tensor_session_bytes: execution_profile.max_tensor_session_bytes(),
            storage,
            hfhe_caps,
            hfhe_pubkeys,
            hfhe_active_key: payload.hfhe_active_key.clone(),
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
            is_view: payload.is_view.unwrap_or(false),
            response_bytes: Vec::new(),
            response_status: 0,
            events: Vec::new(),
            event_bytes: 0,
            spawns: Vec::new(),
            assets: Vec::new(),
            encrypted_assets: Vec::new(),
            circle_invoke_cache: None,
            hfhe_invoke_cache: None,
            unavailable: None,
            compute_trace: None,
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
    let reads = payload.public_reads.clone().unwrap_or_default();
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
                    metadata_json: read.metadata_json,
                    body_b64: read.body_b64,
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
) -> Result<Arc<BTreeMap<String, Vec<u8>>>, String> {
    if let Some(key) = payload.compute_storage_cache_key.as_deref() {
        if execution_profile != ExecutionProfile::Compute {
            return Err("compute storage requires compute execution profile".to_owned());
        }
        if payload.storage_cache_key.is_some() || payload.storage_pairs.is_some() {
            return Err("compute storage cannot mix storage sources".to_owned());
        }
        normalize_hex64(key, "compute storage cache key")?;
        let mut cache = compute_storage_cache()
            .lock()
            .map_err(|_| "compute storage cache poisoned".to_owned())?;
        match cache.get_mut(key) {
            Some(entry) => {
                entry.updated_at = now_secs();
                return Ok(entry.storage.clone());
            }
            None => return Err(format!("missing compute storage cache: {key}")),
        }
    }
    let cache_key = payload.storage_cache_key.clone();
    let pairs_opt = payload.storage_pairs.clone();

    if pairs_opt.is_none() {
        if let Some(key) = cache_key {
            let mut guard = cache_guard(storage_cache());
            prune_storage_cache_locked(&mut guard);
            if let Some(entry) = guard.get_mut(&key) {
                entry.updated_at = now_secs();
                return Ok(entry.storage.clone());
            }
            return Err(format!("missing storage cache: {key}"));
        }
        return Ok(Arc::new(BTreeMap::new()));
    }

    let mut storage = BTreeMap::new();
    for pair in pairs_opt.unwrap_or_default() {
        let key = String::from_utf8(decode_b64("storage key", &pair.key_b64)?)
            .map_err(|_| "invalid storage key encoding".to_owned())?;
        let value = decode_b64("storage value", &pair.value_b64)?;
        storage.insert(key, value);
    }

    if let Some(key) = cache_key {
        let mut guard = cache_guard(storage_cache());
        let shared = Arc::new(storage);
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

    Ok(Arc::new(storage))
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
                    let chunk = read_guest_bytes(&mut caller, ptr, len)?;
                    let data = caller.data_mut();
                    let total = data.response_bytes.len() + chunk.len();
                    if total > MAX_RESPONSE_BYTES {
                        return Err(WasmiError::new("wasm response exceeds limit"));
                    }
                    data.response_bytes.extend_from_slice(&chunk);
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
                    charge_host_fuel(&mut caller, CIRCLE_INVOKE_FUEL)?;
                    let response_bytes = execute_circle_invoke(&mut caller, &request_bytes)
                        .map_err(WasmiError::new)?;
                    let len = response_bytes.len() as i32;
                    caller.data_mut().circle_invoke_cache = Some((request_bytes, response_bytes));
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
                    let cached = caller.data().circle_invoke_cache.clone();
                    let response_bytes = match cached {
                        Some((cached_req, cached_res)) if cached_req == request_bytes => cached_res,
                        _ => {
                            charge_host_fuel(&mut caller, CIRCLE_INVOKE_FUEL)?;
                            execute_circle_invoke(&mut caller, &request_bytes)
                                .map_err(WasmiError::new)?
                        }
                    };
                    if out_cap < 0 || (out_cap as usize) < response_bytes.len() {
                        return Ok(-2);
                    }
                    let written = write_guest_bytes(&mut caller, out_ptr, &response_bytes)?;
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
                    caller.data_mut().hfhe_invoke_cache = Some((request_bytes, response_bytes));
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
                    let cached = caller.data().hfhe_invoke_cache.clone();
                    let response_bytes = match cached {
                        Some((cached_req, cached_res)) if cached_req == request_bytes => cached_res,
                        _ => {
                            charge_host_fuel(&mut caller, HFHE_INVOKE_FUEL)?;
                            match execute_hfhe_invoke(&mut caller, &request_bytes) {
                                Ok(response) => response,
                                Err(failure) => return Err(host_failure(&mut caller, failure)),
                            }
                        }
                    };
                    if out_cap < 0 || (out_cap as usize) < response_bytes.len() {
                        return Ok(-2);
                    }
                    let written = write_guest_bytes(&mut caller, out_ptr, &response_bytes)?;
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
                    let value = match caller.data().storage.get(&key) {
                        Some(value) => value.clone(),
                        None => return Ok(-1),
                    };
                    if out_cap < 0 || (out_cap as usize) < value.len() {
                        return Ok(-2);
                    }
                    write_guest_bytes(&mut caller, out_ptr, &value)
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
                    Arc::make_mut(&mut caller.data_mut().storage).insert(key, value);
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
                    Arc::make_mut(&mut caller.data_mut().storage).remove(&key);
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
                    let mut guard = cache_guard(session_cache());
                    prune_session_cache_locked(&mut guard);
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
                    let mut guard = cache_guard(session_cache());
                    prune_session_cache_locked(&mut guard);
                    let value = match guard.get(&cache_key) {
                        Some(entry) => entry.value.clone(),
                        None => return Ok(-1),
                    };
                    if out_cap < 0 || (out_cap as usize) < value.len() {
                        return Ok(-2);
                    }
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
                    let value = read_guest_bytes(&mut caller, value_ptr, value_len)?;
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
                    let mut guard = cache_guard(tensor_session_cache());
                    prune_tensor_session_cache_locked(&mut guard);
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
                    let mut guard = cache_guard(tensor_session_cache());
                    prune_tensor_session_cache_locked(&mut guard);
                    guard.insert(
                        cache_key,
                        TensorSessionEntry {
                            n_layers,
                            max_t,
                            kv_dim,
                            k_cache: vec![0.0_f64; total],
                            v_cache: vec![0.0_f64; total],
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
                    let mut guard = fixed_tensor_session_cache()
                        .lock()
                        .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
                    prune_fixed_tensor_session_cache_locked(&mut guard);
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
                    let mut guard = fixed_tensor_session_cache()
                        .lock()
                        .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
                    prune_fixed_tensor_session_cache_locked(&mut guard);
                    guard.insert(
                        cache_key,
                        FixedTensorSessionEntry {
                            n_layers,
                            max_t,
                            kv_dim,
                            k_cache: vec![0_i64; total],
                            v_cache: vec![0_i64; total],
                            updated_at: now_secs(),
                        },
                    );
                    prune_fixed_tensor_session_cache_locked(&mut guard);
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
            || data.compute_started_at.elapsed() > Duration::from_secs(Q24_WALL_LIMIT_SECS)
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
    let mut out = vec![0_u8; len];
    memory
        .read(caller.as_context(), ptr as usize, &mut out)
        .map_err(|_| WasmiError::new("guest memory read out of bounds"))?;
    Ok(out)
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

fn maintain_compute_session_caches() {
    prune_session_cache_locked(&mut cache_guard(session_cache()));
    prune_tensor_session_cache_locked(&mut cache_guard(tensor_session_cache()));
    prune_fixed_tensor_session_cache_locked(&mut cache_guard(fixed_tensor_session_cache()));
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

fn clear_compute_session_prefix(prefix: &str) -> Result<(), String> {
    session_cache()
        .lock()
        .map_err(|_| "session cache poisoned".to_owned())?
        .retain(|key, _| !key.starts_with(prefix));
    let tensor_prefix = format!("tensor|{prefix}");
    tensor_session_cache()
        .lock()
        .map_err(|_| "tensor session cache poisoned".to_owned())?
        .retain(|key, _| !key.starts_with(&tensor_prefix));
    let fixed_prefix = format!("tensor_q24|{prefix}");
    fixed_tensor_session_cache()
        .lock()
        .map_err(|_| "fixed tensor session cache poisoned".to_owned())?
        .retain(|key, _| !key.starts_with(&fixed_prefix));
    Ok(())
}

fn clear_compute_session_scope(data: &HostState) -> Result<(), String> {
    match compute_session_prefix(data) {
        Some(prefix) => clear_compute_session_prefix(&prefix),
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

fn storage_weight(storage: &BTreeMap<String, Vec<u8>>) -> usize {
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

fn read_guest_f64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    len: usize,
) -> Result<Vec<f64>, WasmiError> {
    let byte_len = len
        .checked_mul(8)
        .filter(|value| *value <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("tensor input exceeds cap"))?;
    let raw = read_guest_bytes(caller, ptr, byte_len as i32)?;
    let mut out = Vec::with_capacity(len);
    for chunk in raw.chunks_exact(8) {
        out.push(f64::from_le_bytes([
            chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5], chunk[6], chunk[7],
        ]));
    }
    Ok(out)
}

fn write_guest_f64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    values: &[f64],
) -> Result<i32, WasmiError> {
    let byte_len = values
        .len()
        .checked_mul(8)
        .filter(|value| *value <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("tensor output exceeds cap"))?;
    let mut raw = Vec::with_capacity(byte_len);
    for value in values {
        raw.extend_from_slice(&value.to_le_bytes());
    }
    write_guest_bytes(caller, ptr, &raw)
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
) -> Result<Vec<i64>, WasmiError> {
    require_compute_profile(caller)?;
    let byte_len = len
        .checked_mul(8)
        .filter(|value| *value <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("tensor input exceeds cap"))?;
    let raw = read_guest_bytes(caller, ptr, byte_len as i32)?;
    Ok(raw
        .chunks_exact(8)
        .map(|chunk| {
            i64::from_le_bytes([
                chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5], chunk[6], chunk[7],
            ])
        })
        .collect())
}

fn write_guest_i64_vec(
    caller: &mut Caller<'_, HostState>,
    ptr: i32,
    values: &[i64],
) -> Result<i32, WasmiError> {
    require_compute_profile(caller)?;
    let byte_len = values
        .len()
        .checked_mul(8)
        .filter(|value| *value <= caller.data().max_guest_bytes)
        .ok_or_else(|| WasmiError::new("tensor output exceeds cap"))?;
    let mut raw = Vec::with_capacity(byte_len);
    for value in values {
        raw.extend_from_slice(&value.to_le_bytes());
    }
    write_guest_bytes(caller, ptr, &raw)
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
    let out = {
        let data = caller.data();
        let value = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        let start = off as usize;
        let count = n as usize;
        if start + count > value.len() {
            return Ok(-1);
        }
        let mut raw = Vec::with_capacity(count * 8);
        for byte in &value[start..start + count] {
            let signed = i8::from_ne_bytes([*byte]) as f64;
            raw.extend_from_slice(&(signed * scale).to_le_bytes());
        }
        raw
    };
    write_guest_bytes(caller, dst_ptr, &out)
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
    let mut dst = vec![0.0_f64; m * n];
    for r in 0..m {
        let r_k = r * k;
        let r_n = r * n;
        for c in 0..n {
            let mut acc = 0.0_f64;
            for i in 0..k {
                acc += lhs[r_k + i] * rhs[i * n + c];
            }
            dst[r_n + c] = acc;
        }
    }
    write_guest_f64_vec(caller, dst_ptr, &dst)
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
    let inv_rms = 1.0_f64 / ((sum_sq / n as f64) + 1e-5_f64).sqrt();
    for idx in 0..n {
        values[idx] = gamma[idx] * values[idx] * inv_rms;
    }
    write_guest_f64_vec(caller, addr_ptr, &values)
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
    for value in &mut values {
        let sig = 1.0_f64 / (1.0_f64 + (-*value).exp());
        *value *= sig;
    }
    write_guest_f64_vec(caller, addr_ptr, &values)
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
    write_guest_f64_vec(caller, dst_ptr, &dst)
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
    write_guest_f64_vec(caller, dst_ptr, &dst)
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
    for idx in 0..half {
        let exp_term = (2.0_f64 * idx as f64) / nf;
        let inv_freq = 1.0_f64 / base.powf(exp_term);
        let angle = pf * inv_freq;
        let (s, c) = angle.sin_cos();
        let re = values[idx];
        let im = values[idx + half];
        values[idx] = re * c - im * s;
        values[idx + half] = re * s + im * c;
    }
    write_guest_f64_vec(caller, addr_ptr, &values)
}

fn attention_ctx_from_slices(
    q: &[f64],
    k: &[f64],
    v: &[f64],
    t_total: usize,
    n_q_heads: usize,
    n_kv_heads: usize,
    head_dim: usize,
) -> Vec<f64> {
    let kv_dim = n_kv_heads * head_dim;
    let mut ctx = vec![0.0_f64; n_q_heads * head_dim];
    let group = n_q_heads / n_kv_heads;
    let inv_sqrt_d = 1.0_f64 / (head_dim as f64).sqrt();
    let mut scores = vec![0.0_f64; t_total];
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
        for score in &scores {
            if *score > max_score {
                max_score = *score;
            }
        }
        let mut sum_exp = 0.0_f64;
        for score in &mut scores {
            *score = (*score - max_score).exp();
            sum_exp += *score;
        }
        let inv_sum = 1.0_f64 / sum_exp;
        for score in &mut scores {
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
    ctx
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
    let ctx = attention_ctx_from_slices(&q, &k, &v, t_total, n_q_heads, n_kv_heads, head_dim);
    write_guest_f64_vec(caller, ctx_ptr, &ctx)
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
    write_guest_f64_vec(caller, dst_elem_ptr, &src)
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
    let mut guard = cache_guard(tensor_session_cache());
    prune_tensor_session_cache_locked(&mut guard);
    let entry = match guard.get_mut(&cache_key) {
        Some(entry) => entry,
        None => return Ok(-3),
    };
    let (layer_idx, position, n) = (layer_idx as usize, position as usize, n as usize);
    if layer_idx >= entry.n_layers || position >= entry.max_t || n != entry.kv_dim {
        return Ok(-1);
    }
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
    let kv_dim = n_kv_heads * head_dim;
    let ctx = {
        let mut guard = cache_guard(tensor_session_cache());
        prune_tensor_session_cache_locked(&mut guard);
        let entry = match guard.get_mut(&cache_key) {
            Some(entry) => entry,
            None => return Ok(-3),
        };
        if layer_idx >= entry.n_layers || t_total > entry.max_t || entry.kv_dim != kv_dim {
            return Ok(-1);
        }
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
        )
    };
    write_guest_f64_vec(caller, ctx_ptr, &ctx)
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
    let input = read_guest_f64_vec(caller, in_ptr, in_dim)?;
    let input_f32: Vec<f32> = input.into_iter().map(|value| value as f32).collect();
    let key = read_guest_utf8(caller, key_ptr, key_len)?;
    let scale = bits_to_fp64(scale_bits) as f32;
    let out = {
        let data = caller.data();
        let value = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        if value.len() != in_dim * out_dim {
            return Ok(-1);
        }
        let mut out = vec![0.0_f32; out_dim];
        for i in 0..in_dim {
            let row_off = i * out_dim;
            let input_v = input_f32[i];
            for o in 0..out_dim {
                let signed = i8::from_ne_bytes([value[row_off + o]]) as f32;
                out[o] += input_v * signed * scale;
            }
        }
        out.into_iter()
            .map(|value| value as f64)
            .collect::<Vec<_>>()
    };
    write_guest_f64_vec(caller, out_ptr, &out)
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
    let input = read_guest_f64_vec(caller, in_ptr, cols)?;
    let input_f32: Vec<f32> = input.into_iter().map(|value| value as f32).collect();
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
    let input = read_guest_f64_vec(caller, in_ptr, cols)?;
    let input_f32: Vec<f32> = input.into_iter().map(|value| value as f32).collect();
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
    write_guest_f64_vec(caller, score_out_ptr, &[best_val as f64])?;
    Ok(best_idx as i32)
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
    let output = {
        let data = caller.data();
        let value = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        let start = off as usize;
        let count = n as usize;
        if start
            .checked_add(count)
            .filter(|end| *end <= value.len())
            .is_none()
        {
            return Ok(-1);
        }
        deterministic_tensor::load_i8(&value[start..start + count], scale).map_err(compute_error)?
    };
    let key_digest: [u8; 32] = Sha256::digest(key.as_bytes()).into();
    let output_digest = trace_i64_digest(&output);
    let offset_bytes = off.to_le_bytes();
    let count_bytes = n.to_le_bytes();
    let scale_bytes = scale.to_le_bytes();
    let status = write_guest_i64_vec(caller, dst_ptr, &output)?;
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
    let output = deterministic_tensor::rmsnorm(&values, &gamma).map_err(compute_error)?;
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
    let output = deterministic_tensor::silu(&values).map_err(compute_error)?;
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
    let output = deterministic_tensor::elementwise_mul(&left, &right).map_err(compute_error)?;
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
    let output = deterministic_tensor::residual_add(&left, &right).map_err(compute_error)?;
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
    let output = deterministic_tensor::rope(&values, &frequencies, position as i64)
        .map_err(compute_error)?;
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
    let output = {
        let data = caller.data();
        let weights = match data.storage.get(&key) {
            Some(value) => value,
            None => return Ok(-3),
        };
        deterministic_tensor::linear_i8(&input, weights, out_dim, scale).map_err(compute_error)?
    };
    let key_digest: [u8; 32] = Sha256::digest(key.as_bytes()).into();
    let input_digest = trace_i64_digest(&input);
    let output_digest = trace_i64_digest(&output);
    let input_dimension_bytes = (in_dim as u64).to_le_bytes();
    let output_dimension_bytes = (out_dim as u64).to_le_bytes();
    let scale_bytes = scale.to_le_bytes();
    let status = write_guest_i64_vec(caller, out_ptr, &output)?;
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
    write_guest_i64_vec(caller, score_out_ptr, &[score])?;
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
    Ok(index)
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
    let mut cache = fixed_tensor_session_cache()
        .lock()
        .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
    prune_fixed_tensor_session_cache_locked(&mut cache);
    let entry = match cache.get_mut(&cache_key) {
        Some(entry) => entry,
        None => return Ok(-3),
    };
    let (layer_idx, position, n) = (layer_idx as usize, position as usize, n as usize);
    if layer_idx >= entry.n_layers || position >= entry.max_t || n != entry.kv_dim {
        return Ok(-1);
    }
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
    let kv_dim = kv_heads * head_dim;
    let output = {
        let mut cache = fixed_tensor_session_cache()
            .lock()
            .map_err(|_| WasmiError::new("fixed tensor session cache poisoned"))?;
        prune_fixed_tensor_session_cache_locked(&mut cache);
        let entry = match cache.get_mut(&cache_key) {
            Some(entry) => entry,
            None => return Ok(-3),
        };
        if layer_idx >= entry.n_layers || sequence_len > entry.max_t || entry.kv_dim != kv_dim {
            return Ok(-1);
        }
        let layer_span = entry.max_t * entry.kv_dim;
        let layer_offset = layer_idx * layer_span;
        let used = sequence_len * entry.kv_dim;
        entry.updated_at = now_secs();
        deterministic_tensor::attention_kv(
            &query,
            &entry.k_cache[layer_offset..layer_offset + used],
            &entry.v_cache[layer_offset..layer_offset + used],
            sequence_len,
            query_heads,
            kv_heads,
            head_dim,
            scale,
        )
        .map_err(compute_error)?
    };
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

fn storage_get_utf8(storage: &BTreeMap<String, Vec<u8>>, key: &str) -> Option<String> {
    storage
        .get(key)
        .and_then(|value| String::from_utf8(value.clone()).ok())
}

fn storage_set_utf8(storage: &mut BTreeMap<String, Vec<u8>>, key: String, value: impl ToString) {
    storage.insert(key, value.to_string().into_bytes());
}

fn storage_set_optional_utf8(
    storage: &mut BTreeMap<String, Vec<u8>>,
    key: String,
    value: Option<String>,
) {
    match value {
        Some(value) if !value.is_empty() => {
            storage_set_utf8(storage, key, value);
        }
        _ => {
            storage.remove(&key);
        }
    }
}

fn storage_set_bool(storage: &mut BTreeMap<String, Vec<u8>>, key: String, value: bool) {
    storage_set_utf8(storage, key, normalize_bool_literal(value));
}

fn storage_set_int(storage: &mut BTreeMap<String, Vec<u8>>, key: String, value: impl ToString) {
    storage_set_utf8(storage, key, value.to_string());
}

fn storage_get_bool(storage: &BTreeMap<String, Vec<u8>>, key: &str) -> Option<bool> {
    let raw = storage_get_utf8(storage, key)?;
    match raw.trim().to_lowercase().as_str() {
        "1" | "true" | "yes" => Some(true),
        "0" | "false" | "no" => Some(false),
        _ => None,
    }
}

fn storage_get_int(storage: &BTreeMap<String, Vec<u8>>, key: &str) -> Option<String> {
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

fn increment_stored_version(storage: &mut BTreeMap<String, Vec<u8>>, key: &str) -> String {
    let next = storage_get_int(storage, key)
        .and_then(|value| value.parse::<i64>().ok())
        .map(|value| value + 1)
        .unwrap_or(1);
    let value = next.to_string();
    storage_set_int(storage, key.to_owned(), &value);
    value
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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
    storage: &BTreeMap<String, Vec<u8>>,
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

fn execute_circle_invoke(
    caller: &mut Caller<'_, HostState>,
    req_bytes: &[u8],
) -> Result<Vec<u8>, String> {
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
            return Err("wasm public read bytes exceed limit".to_owned());
        }
        let body_b64 = read.body_b64.clone();
        caller.data_mut().public_read_bytes = next_total;
        return Ok(frame_string(body_b64));
    }
    if request.method == "circle_spawn" {
        if caller.data().is_view {
            return Err("circle spawn disabled in view".to_owned());
        }
        if caller.data().spawns.len() >= 4 {
            return Err("circle spawn cap exceeded".to_owned());
        }
        let payload_json = normalize_non_empty(
            &parse_string_param(&params, 0, "payload_json")?,
            "payload_json",
        )?;
        if payload_json.len() > MAX_SPAWN_PAYLOAD_JSON_BYTES {
            return Err("circle spawn payload exceeds max size".to_owned());
        }
        serde_json::from_str::<JsonValue>(&payload_json)
            .map_err(|e| format!("invalid circle spawn payload json: {e}"))?;
        let owner_mode =
            normalize_non_empty(&parse_string_param(&params, 1, "owner_mode")?, "owner_mode")?;
        if owner_mode != "caller" && owner_mode != "parent" {
            return Err("owner_mode must be caller or parent".to_owned());
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
            return Err("circle asset effects disabled in view".to_owned());
        }
        if caller.data().assets.len() + caller.data().encrypted_assets.len() >= 4 {
            return Err("circle asset effect cap exceeded".to_owned());
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
            return Err("circle asset effect body exceeds max encoded size".to_owned());
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
            return Err("circle encrypted asset effects disabled in view".to_owned());
        }
        if caller.data().assets.len() + caller.data().encrypted_assets.len() >= 4 {
            return Err("circle asset effect cap exceeded".to_owned());
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
            return Err("circle encrypted asset effect body exceeds max encoded size".to_owned());
        }
        if metadata_mode
            .as_ref()
            .map(|value| value != "reveal" && value != "opaque")
            .unwrap_or(false)
        {
            return Err("metadata_mode must be reveal or opaque".to_owned());
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
            return Err("circle encrypted asset effects disabled in view".to_owned());
        }
        if caller.data().assets.len() + caller.data().encrypted_assets.len() >= 4 {
            return Err("circle asset effect cap exceeded".to_owned());
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
            return Err("circle encrypted asset effect body exceeds max encoded size".to_owned());
        }
        if metadata_mode
            .as_ref()
            .map(|value| value != "reveal" && value != "opaque")
            .unwrap_or(false)
        {
            return Err("metadata_mode must be reveal or opaque".to_owned());
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
    let storage = Arc::make_mut(&mut caller.data_mut().storage);
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
            );
            storage_set_utf8(storage, state_descriptor_key(&state_ref, "codec"), codec);
            storage_set_optional_utf8(
                storage,
                state_descriptor_key(&state_ref, "schema_hash"),
                schema_hash,
            );
            storage_set_optional_utf8(
                storage,
                state_descriptor_key(&state_ref, "subject_addr"),
                subject_addr,
            );
            storage_set_utf8(
                storage,
                state_descriptor_key(&state_ref, "hfhe_profile"),
                hfhe_profile,
            );
            storage_set_bool(
                storage,
                state_descriptor_key(&state_ref, "mutable_state"),
                mutable_state,
            );
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
            );
            storage_set_int(
                storage,
                state_policy_key(&state_ref, "activate_after_epoch"),
                activate_after,
            );
            storage_set_int(
                storage,
                state_policy_key(&state_ref, "expire_after_epoch"),
                expire_after,
            );
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), false);
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), false);
            Ok(frame_bool(true))
        }
        "state_release" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage.remove(&state_policy_key(&state_ref, "activate_after_epoch"));
            storage.remove(&state_policy_key(&state_ref, "expire_after_epoch"));
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), false);
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), false);
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
            );
            Ok(frame_bool(true))
        }
        "state_tombstone_apply" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), true);
            Ok(frame_bool(true))
        }
        "state_restore" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "tombstone"), false);
            Ok(frame_bool(true))
        }
        "state_revoke_apply" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), true);
            Ok(frame_bool(true))
        }
        "state_reinstate" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            storage_set_bool(storage, state_policy_key(&state_ref, "revoked"), false);
            Ok(frame_bool(true))
        }
        "state_descriptor_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_state_descriptor_field(storage, &state_ref, &field)
        }
        "state_policy_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_state_policy_field(storage, &state_ref, &field)
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
            );
            storage_set_utf8(
                storage,
                balance_cell_key(&state_ref, "amount_commitment"),
                normalize_non_empty(
                    &parse_string_param(&params, 2, "amount_commitment")?,
                    "amount_commitment",
                )?,
            );
            storage_set_utf8(
                storage,
                balance_cell_key(&state_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 3, "proof_kind")?, "proof_kind")?,
            );
            storage_set_optional_utf8(
                storage,
                balance_cell_key(&state_ref, "proof_receipt_hash"),
                normalize_optional_hex64(
                    parse_optional_string_param(&params, 4)?,
                    "proof_receipt_hash",
                )?,
            );
            Ok(frame_bool(true))
        }
        "balance_cell_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_balance_cell_field(storage, &state_ref, &field)
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
            );
            storage_set_utf8(
                storage,
                register_cell_key(&state_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 2, "proof_kind")?, "proof_kind")?,
            );
            storage_set_optional_utf8(
                storage,
                register_cell_key(&state_ref, "proof_receipt_hash"),
                normalize_optional_hex64(
                    parse_optional_string_param(&params, 3)?,
                    "proof_receipt_hash",
                )?,
            );
            Ok(frame_bool(true))
        }
        "register_cell_get" => {
            let state_ref =
                normalize_hex64(&parse_string_param(&params, 0, "state_ref")?, "state_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_register_cell_field(storage, &state_ref, &field)
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
            );
            let version =
                increment_stored_version(storage, &balance_binding_key(&subject_addr, "version"));
            storage_set_utf8(
                storage,
                balance_binding_key(&subject_addr, "status"),
                status,
            );
            storage_set_utf8(
                storage,
                balance_binding_key(&subject_addr, "last_workflow_ref"),
                workflow_ref,
            );
            Ok(frame_int(version))
        }
        "balance_binding_get" => {
            let subject_addr = normalize_non_empty(
                &parse_string_param(&params, 0, "subject_addr")?,
                "subject_addr",
            )?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_balance_binding_field(storage, &subject_addr, &field)
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
            );
            let version =
                increment_stored_version(storage, &register_binding_key(&register_ref, "version"));
            storage_set_utf8(
                storage,
                register_binding_key(&register_ref, "status"),
                status,
            );
            storage_set_utf8(
                storage,
                register_binding_key(&register_ref, "last_workflow_ref"),
                workflow_ref,
            );
            Ok(frame_int(version))
        }
        "register_binding_get" => {
            let register_ref = normalize_hex64(
                &parse_string_param(&params, 0, "register_ref")?,
                "register_ref",
            )?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_register_binding_field(storage, &register_ref, &field)
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
                );
            }
            Ok(frame_bool(true))
        }
        "balance_workflow_get" => {
            let workflow_ref = normalize_hex64(
                &parse_string_param(&params, 0, "workflow_ref")?,
                "workflow_ref",
            )?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_balance_workflow_field(storage, &workflow_ref, &field)
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
                );
            }
            Ok(frame_bool(true))
        }
        "register_workflow_get" => {
            let workflow_ref = normalize_hex64(
                &parse_string_param(&params, 0, "workflow_ref")?,
                "workflow_ref",
            )?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_register_workflow_field(storage, &workflow_ref, &field)
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
            );
            let version =
                increment_stored_version(storage, &object_binding_key(&object_ref, "version"));
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "last_transition_ref"),
                transition_ref,
            );
            storage_set_utf8(storage, object_binding_key(&object_ref, "status"), status);
            Ok(frame_int(version))
        }
        "object_binding_get" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_object_binding_field(storage, &object_ref, &field)
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
            );
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "member_kind"),
                member_kind,
            );
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "state_class"),
                state_class,
            );
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "codec"),
                codec,
            );
            storage_set_utf8(
                storage,
                object_member_key(&object_ref, &member_ref, "status"),
                status,
            );
            Ok(frame_bool(true))
        }
        "object_member_detach" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let member_ref =
                normalize_non_empty(&parse_string_param(&params, 1, "member_ref")?, "member_ref")?;
            storage.remove(&object_member_key(&object_ref, &member_ref, "state_ref"));
            storage.remove(&object_member_key(&object_ref, &member_ref, "member_kind"));
            storage.remove(&object_member_key(&object_ref, &member_ref, "state_class"));
            storage.remove(&object_member_key(&object_ref, &member_ref, "codec"));
            storage.remove(&object_member_key(&object_ref, &member_ref, "status"));
            Ok(frame_bool(true))
        }
        "object_member_get" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let member_ref =
                normalize_non_empty(&parse_string_param(&params, 1, "member_ref")?, "member_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 2, "field")?, "field")?;
            read_object_member_field(storage, &object_ref, &member_ref, &field)
        }
        "object_policy_define" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_optional_utf8(
                storage,
                object_policy_key(&object_ref, "delivery_key_id"),
                normalize_optional_string(parse_optional_string_param(&params, 1)?),
            );
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "activate_after_epoch"),
                parse_int_param(&params, 2, "activate_after_epoch")?,
            );
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "expire_after_epoch"),
                parse_int_param(&params, 3, "expire_after_epoch")?,
            );
            storage_set_utf8(
                storage,
                object_policy_key(&object_ref, "transition_mode"),
                normalize_non_empty(
                    &parse_string_param(&params, 4, "transition_mode")?,
                    "transition_mode",
                )?,
            );
            storage_set_utf8(
                storage,
                object_policy_key(&object_ref, "required_proof_kind"),
                normalize_non_empty(
                    &parse_string_param(&params, 5, "required_proof_kind")?,
                    "required_proof_kind",
                )?,
            );
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "member_quorum"),
                parse_int_param(&params, 6, "member_quorum")?,
            );
            storage_set_bool(
                storage,
                object_policy_key(&object_ref, "allow_detach"),
                parse_bool_param(&params, 7, "allow_detach")?,
            );
            storage_set_bool(
                storage,
                object_policy_key(&object_ref, "allow_root_state_rotation"),
                parse_bool_param(&params, 8, "allow_root_state_rotation")?,
            );
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), false);
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), false);
            Ok(frame_bool(true))
        }
        "object_policy_release" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage.remove(&object_policy_key(&object_ref, "delivery_key_id"));
            storage.remove(&object_policy_key(&object_ref, "activate_after_epoch"));
            storage.remove(&object_policy_key(&object_ref, "expire_after_epoch"));
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), false);
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), false);
            Ok(frame_bool(true))
        }
        "object_policy_retire" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_int(
                storage,
                object_policy_key(&object_ref, "expire_after_epoch"),
                parse_int_param(&params, 1, "expire_after_epoch")?,
            );
            Ok(frame_bool(true))
        }
        "object_policy_tombstone" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), true);
            Ok(frame_bool(true))
        }
        "object_policy_restore" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "tombstone"), false);
            Ok(frame_bool(true))
        }
        "object_policy_revoke" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), true);
            Ok(frame_bool(true))
        }
        "object_policy_reinstate" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            storage_set_bool(storage, object_policy_key(&object_ref, "revoked"), false);
            Ok(frame_bool(true))
        }
        "object_policy_get" => {
            let object_ref =
                normalize_hex64(&parse_string_param(&params, 0, "object_ref")?, "object_ref")?;
            let field = normalize_non_empty(&parse_string_param(&params, 1, "field")?, "field")?;
            read_object_policy_field(storage, &object_ref, &field)
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
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "previous_state_ref"),
                normalize_hex64(
                    &parse_string_param(&params, 2, "previous_state_ref")?,
                    "previous_state_ref",
                )?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "next_state_ref"),
                normalize_hex64(
                    &parse_string_param(&params, 3, "next_state_ref")?,
                    "next_state_ref",
                )?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "touched_members_hash"),
                normalize_hex64(
                    &parse_string_param(&params, 4, "touched_members_hash")?,
                    "touched_members_hash",
                )?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 5, "proof_kind")?, "proof_kind")?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_receipt_hash"),
                normalize_hex64(
                    &parse_string_param(&params, 6, "proof_receipt_hash")?,
                    "proof_receipt_hash",
                )?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "status"),
                normalize_non_empty(&parse_string_param(&params, 7, "status")?, "status")?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "intent_id"),
                normalize_hex64(&parse_string_param(&params, 8, "intent_id")?, "intent_id")?,
            );
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
                return Err("object transition touched_members_hash mismatch".to_owned());
            }
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "object_ref"),
                object_ref.clone(),
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "previous_state_ref"),
                previous_state_ref,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "next_state_ref"),
                next_state_ref.clone(),
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "touched_members_hash"),
                touched_members_hash,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_kind"),
                normalize_non_empty(&parse_string_param(&params, 6, "proof_kind")?, "proof_kind")?,
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "proof_receipt_hash"),
                normalize_hex64(
                    &parse_string_param(&params, 7, "proof_receipt_hash")?,
                    "proof_receipt_hash",
                )?,
            );
            let status = normalize_non_empty(&parse_string_param(&params, 8, "status")?, "status")?;
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "status"),
                status.clone(),
            );
            storage_set_utf8(
                storage,
                object_transition_key(&transition_ref, "intent_id"),
                normalize_hex64(&parse_string_param(&params, 9, "intent_id")?, "intent_id")?,
            );
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "current_state_ref"),
                next_state_ref,
            );
            let version =
                increment_stored_version(storage, &object_binding_key(&object_ref, "version"));
            storage_set_utf8(
                storage,
                object_binding_key(&object_ref, "last_transition_ref"),
                transition_ref,
            );
            storage_set_utf8(storage, object_binding_key(&object_ref, "status"), status);
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
                        );
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "member_kind"),
                            member_kind,
                        );
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "state_class"),
                            state_class,
                        );
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "codec"),
                            codec,
                        );
                        storage_set_utf8(
                            storage,
                            object_member_key(&object_ref, &member_ref, "status"),
                            status,
                        );
                    }
                    ObjectMemberDelta::Detach { member_ref } => {
                        storage.remove(&object_member_key(&object_ref, &member_ref, "state_ref"));
                        storage.remove(&object_member_key(&object_ref, &member_ref, "member_kind"));
                        storage.remove(&object_member_key(&object_ref, &member_ref, "state_class"));
                        storage.remove(&object_member_key(&object_ref, &member_ref, "codec"));
                        storage.remove(&object_member_key(&object_ref, &member_ref, "status"));
                    }
                }
            }
            Ok(frame_int(version))
        }
        _ => Err(format!(
            "unsupported circle host method: {}",
            request.method
        )),
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
        request_hash: hfhe_receipt_hash(b"octra:circle_hfhe_request:v1", request),
        response_hash: hfhe_receipt_hash(b"octra:circle_hfhe_response:v1", response),
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
    let is_verify = is_hfhe_verify_method(&method);
    if mode == "capture" && caller.data().hfhe_receipt_entries.len() >= MAX_HFHE_RECEIPT_ENTRIES {
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
        let request_hash = hfhe_receipt_hash(b"octra:circle_hfhe_request:v1", req_bytes);
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
            let entry = rejected(hfhe_receipt_entry(&method, req_bytes, &response))?;
            rejected(consume_hfhe_receipt_entry(caller, entry))?;
            return Ok(response);
        }
    } else if !caller.data().is_view && is_verify && mode != "capture" {
        return Err(HostFailure::Rejected(format!(
            "hfhe proof verification requires a preverified receipt: {}",
            method
        )));
    }
    let response = execute_hfhe_direct(caller, req_bytes)?;
    let entry = rejected(hfhe_receipt_entry(&method, req_bytes, &response))?;
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
    Ok(response)
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
            }))?;
            Ok(frame_int(expect_backend_string(value)?))
        }
        "fhe_add" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let lhs_ciphertext = rejected(parse_string_param(&params, 1, "lhs_ciphertext"))?;
            let rhs_ciphertext = rejected(parse_string_param(&params, 2, "rhs_ciphertext"))?;
            let value = call_hfhe_backend(json!({
                "action": "cipher_add",
                "pubkey_b64": pubkey_b64,
                "lhs_ciphertext": lhs_ciphertext,
                "rhs_ciphertext": rhs_ciphertext,
            }))?;
            Ok(frame_string(expect_backend_string(value)?))
        }
        "fhe_sub" => {
            let pubkey_b64 = rejected(parse_string_param(&params, 0, "pubkey_b64"))?;
            let lhs_ciphertext = rejected(parse_string_param(&params, 1, "lhs_ciphertext"))?;
            let rhs_ciphertext = rejected(parse_string_param(&params, 2, "rhs_ciphertext"))?;
            let value = call_hfhe_backend(json!({
                "action": "cipher_sub",
                "pubkey_b64": pubkey_b64,
                "lhs_ciphertext": lhs_ciphertext,
                "rhs_ciphertext": rhs_ciphertext,
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
            }))?;
            Ok(frame_bool(expect_backend_bool(value)?))
        }
        _ => Err(HostFailure::Rejected(format!(
            "unsupported hfhe host method: {}",
            request.method
        ))),
    }
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

unsafe fn write_owned_bytes(bytes: Vec<u8>, ptr_out: *mut *mut u8, len_out: *mut usize) {
    let mut bytes = bytes;
    let len = bytes.len();
    let ptr = if len == 0 {
        std::ptr::null_mut()
    } else {
        let ptr = bytes.as_mut_ptr();
        std::mem::forget(bytes);
        ptr
    };
    if !ptr_out.is_null() {
        *ptr_out = ptr;
    }
    if !len_out.is_null() {
        *len_out = len;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn manifest_profile_has_deterministic_standard_budget() {
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
    fn trapped_manifest_is_rejected_instead_of_falling_back() {
        let wasm = [
            0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0c, 0x02, 0x60, 0x01, 0x7f,
            0x01, 0x7f, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f, 0x03, 0x03, 0x02, 0x00, 0x01, 0x05,
            0x03, 0x01, 0x00, 0x01, 0x07, 0x37, 0x04, 0x06, b'm', b'e', b'm', b'o', b'r', b'y',
            0x02, 0x00, 0x0b, b'o', b'c', b't', b'r', b'a', b'_', b'a', b'l', b'l', b'o', b'c',
            0x00, 0x00, 0x0e, b'o', b'c', b't', b'r', b'a', b'_', b'm', b'a', b'n', b'i', b'f',
            b'e', b's', b't', 0x00, 0x01, 0x0b, b'o', b'c', b't', b'r', b'a', b'_', b'q', b'u',
            b'e', b'r', b'y', 0x00, 0x01, 0x0a, 0x0a, 0x02, 0x04, 0x00, 0x41, 0x00, 0x0b, 0x03,
            0x00, 0x00, 0x0b,
        ];
        let input = serde_json::json!({
            "action": "describe",
            "code_b64": BASE64.encode(wasm),
            "execution_profile": "manifest",
            "is_view": true
        })
        .to_string();
        let error = run_json(input.as_ptr(), input.len()).expect_err("trapped manifest accepted");
        assert!(error.contains("wasm manifest execution failed"), "{error}");
    }

    #[test]
    fn receipt_input_rejects_invalid_shapes() {
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
    fn receipt_request_binds_order_and_payload() {
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
    fn receipt_entry_binds_response_and_result() {
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
    fn storage_cache_replaces_roots_and_stays_bounded() {
        let mut cache = HashMap::new();
        for index in 0..32 {
            let key = format!("oct{index:044}:root{index}");
            prepare_storage_cache_insert(&mut cache, &key, 1024);
            cache.insert(
                key,
                StorageCacheEntry {
                    storage: Arc::new(BTreeMap::new()),
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
                    storage: Arc::new(BTreeMap::new()),
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
                    storage: Arc::new(BTreeMap::new()),
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
    fn compute_invocation_preserves_live_sessions_and_prunes_expired_sessions() {
        let live_key = "compute-session-regression-live".to_owned();
        let expired_key = "compute-session-regression-expired".to_owned();
        {
            let mut cache = cache_guard(session_cache());
            cache.insert(
                live_key.clone(),
                SessionCacheEntry {
                    value: vec![1],
                    updated_at: now_secs(),
                },
            );
            cache.insert(
                expired_key.clone(),
                SessionCacheEntry {
                    value: vec![2],
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
    fn failed_compute_cleanup_is_scoped_to_one_session() {
        let prefix = "circle|caller|session-a|";
        let scoped_key = format!("{prefix}meta");
        let other_key = "circle|caller|session-b|meta".to_owned();
        {
            let mut cache = cache_guard(session_cache());
            cache.insert(
                scoped_key.clone(),
                SessionCacheEntry {
                    value: vec![1],
                    updated_at: now_secs(),
                },
            );
            cache.insert(
                other_key.clone(),
                SessionCacheEntry {
                    value: vec![2],
                    updated_at: now_secs(),
                },
            );
        }

        clear_compute_session_prefix(prefix).expect("session cleanup");

        let mut cache = cache_guard(session_cache());
        assert!(!cache.contains_key(&scoped_key));
        assert_eq!(
            cache.get(&other_key).map(|entry| entry.value.as_slice()),
            Some(&[2][..])
        );
        cache.remove(&other_key);
    }
}