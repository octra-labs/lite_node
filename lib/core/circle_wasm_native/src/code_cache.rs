// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::*;

pub(super) fn module_weight(cache: &HashMap<String, ModuleCacheEntry>) -> usize {
    cache.values().fold(0usize, |total, entry| total.saturating_add(entry.weight))
}

pub(super) fn code_weight(bytes: &[u8]) -> usize {
    use wasmparser::Payload;
    let mut weight = bytes.len().saturating_mul(16).saturating_add(4096);
    for part in wasmparser::Parser::new(0).parse_all(bytes) {
        let (count, cost) = match part {
            Ok(Payload::FunctionSection(items)) => (items.count(), 1024),
            Ok(Payload::TypeSection(items)) => (items.count(), 128),
            Ok(Payload::ImportSection(items)) => (items.count(), 128),
            Ok(Payload::GlobalSection(items)) => (items.count(), 64),
            Ok(Payload::ElementSection(items)) => (items.count(), 64),
            Ok(Payload::DataSection(items)) => (items.count(), 64),
            Err(_) => return usize::MAX,
            _ => (0, 0),
        };
        weight = weight.saturating_add((count as usize).saturating_mul(cost));
    }
    weight
}

pub(super) type CodeBuild = Mutex<HashMap<String, std::sync::Weak<ModuleCacheEntry>>>;

pub(super) fn module_build() -> &'static CodeBuild {
    static BUILD: OnceLock<CodeBuild> = OnceLock::new();
    BUILD.get_or_init(|| Mutex::new(HashMap::new()))
}

fn module_hit(modules: &Mutex<HashMap<String, ModuleCacheEntry>>, key: &str)
    -> Option<ModuleCacheEntry>
{
    let mut cache = cache_guard(modules);
    prune_module_cache_locked(&mut cache);
    cache.get_mut(key).map(|entry| {
        entry.updated_at = now_secs();
        entry.clone()
    })
}

pub(super) fn module_entry(modules: &Mutex<HashMap<String, ModuleCacheEntry>>,
    build: &CodeBuild, key: &str, module_key: &str, code: Option<&str>, float: bool, strict: bool)
    -> Result<Arc<ModuleCacheEntry>, HostFailure>
{
    if let Some(entry) = module_hit(modules, key) { return Ok(Arc::new(entry)); }
    let code = code.ok_or_else(|| format!("missing wasm module cache: {module_key}"))?;
    let mut pending = cache_guard(build);
    if let Some(entry) = module_hit(modules, key) { return Ok(Arc::new(entry)); }
    pending.retain(|_, entry| entry.strong_count() > 0);
    if let Some(entry) = pending.get(key).and_then(std::sync::Weak::upgrade) { return Ok(entry); }
    let bytes = decode_b64("code_b64", code)?;
    if strict { code_shape::check(&bytes)?; }
    let engine = new_engine();
    let bytes = if float { float_ops::prepare(&engine, &bytes)? } else { bytes };
    let module = Arc::new(Module::new(&engine, &mut &bytes[..])
        .map_err(|error| format!("wasm compile failed: {error}"))?);
    validate_imports(&module)?;
    let exports = collect_exports(&module);
    validate_exports(&exports)?;
    let has_update_disabled_imports = module.imports()
        .any(|entry| UPDATE_DISABLED_IMPORTS.contains(&entry.name()));
    let has_compute_imports = module.imports()
        .any(|entry| COMPUTE_IMPORTS.contains(&entry.name()));
    let entry = Arc::new(ModuleCacheEntry { weight: code_weight(&bytes).saturating_add(key.len()), module,
        exports, has_update_disabled_imports, has_compute_imports, updated_at: now_secs() });
    if pending.len() < MODULE_CACHE_LIMIT {
        pending.insert(key.to_owned(), Arc::downgrade(&entry));
    }
    Ok(entry)
}

pub(super) fn keep_module(modules: &Mutex<HashMap<String, ModuleCacheEntry>>,
    key: String, mut entry: ModuleCacheEntry)
    -> bool
{
    if entry.weight > MODULE_CACHE_BYTES { return false; }
    entry.updated_at = now_secs();
    let mut cache = cache_guard(modules);
    cache.entry(key.clone()).or_insert(entry);
    prune_module_cache_locked(&mut cache);
    cache.contains_key(&key)
}