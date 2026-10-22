// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::*;
use super::tests::table_input;

#[test]
fn code_view_bytes() {
    let _guard = tests::cache_test();
    for compute in [false, true] {
        let input = tests::response_input(true, compute);
        let mut runtime = Runtime::instantiate(&input, false).unwrap();
        let call = finish_call(&mut runtime, "octra_query", &[1, 2, 3]).unwrap();
        runtime.code_retained = false;
        let first = reply::execute(&runtime, &call).unwrap();
        runtime.code_retained = true;
        let second = reply::execute(&runtime, &call).unwrap();
        assert_eq!(first.as_slice(), second.as_slice());
        let decoded: JsonValue = serde_json::from_slice(first.as_slice()).unwrap();
        assert!(decoded.get("code_retained").is_none());
    }
}

thread_local! {
    static HOST_WAIT: std::cell::RefCell<Option<Box<dyn FnOnce()>>> = std::cell::RefCell::new(None);
}

pub(super) fn host_wait() {
    HOST_WAIT.with(|hook| { if let Some(run) = hook.borrow_mut().take() { run(); } });
}

#[test]
fn code_start_free() {
    use wasm_encoder::{Module as Wasm, TypeSection, ImportSection, EntityType,
        FunctionSection, MemorySection, MemoryType, ExportSection, ExportKind,
        StartSection, CodeSection, Function, Instruction as I, ValType, DataSection, ConstExpr};
    let _guard = tests::cache_test();
    cache_guard(module_cache()).clear();
    let method = "fhe_encrypt_zero";
    let mut request = REQUEST_MAGIC.to_vec();
    request.extend((method.len() as u16).to_be_bytes());
    request.extend(method.as_bytes());
    request.extend(1_u16.to_be_bytes());
    request.push(4);
    request.extend(0_u32.to_be_bytes());
    let mut module = Wasm::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
    types.ty().function([ValType::I32], [ValType::I32]);
    types.ty().function([], []);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import("octra", "host_hfhe_invoke_len", EntityType::Function(0));
    module.section(&imports);
    let mut funcs = FunctionSection::new();
    funcs.function(1).function(0).function(2);
    module.section(&funcs);
    let mut memory = MemorySection::new();
    memory.memory(MemoryType { minimum: 1, maximum: Some(1), memory64: false,
        shared: false, page_size_log2: None });
    module.section(&memory);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0).export("octra_alloc", ExportKind::Func, 1)
        .export("octra_query", ExportKind::Func, 2);
    module.section(&exports).section(&StartSection { function_index: 3 });
    let mut code = CodeSection::new();
    for _ in 0..2 {
        let mut function = Function::new([]);
        function.instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&function);
    }
    let mut start = Function::new([]);
    start.instruction(&I::I32Const(0)).instruction(&I::I32Const(request.len() as i32))
        .instruction(&I::Call(0)).instruction(&I::Drop).instruction(&I::End);
    code.function(&start);
    module.section(&code);
    let mut data = DataSection::new();
    data.active(0, &ConstExpr::i32_const(0), request);
    module.section(&data);
    let input = Payload::deserialize(json!({"code_b64": encode_b64(&module.finish()),
        "code_cache_key": "slow-start", "is_view": true, "execution_profile": "standard",
        "hfhe_caps": ["fhe_encrypt"], "fuel_limit": 10_000_000, "hfhe_strict": false,
        "hfhe_active_key": {"key_id": "test", "pubkey_b64": "", "seckey_b64": ""}})).unwrap();
    let (entered, ready) = sync_channel(1);
    let (release, wait) = sync_channel(1);
    let slow = std::thread::spawn(move || {
        HOST_WAIT.with(|hook| *hook.borrow_mut() = Some(Box::new(move || {
            entered.send(()).unwrap();
            wait.recv_timeout(Duration::from_secs(60)).unwrap();
        })));
        Runtime::prepare(&input, false, false, None).map(|_| ())
    });
    let started = ready.recv_timeout(Duration::from_secs(30));
    let (done, result) = sync_channel(1);
    let fast = std::thread::spawn(move || {
        done.send(Runtime::instantiate(&table_input(2, "standard", true), true).map(|_| ())).unwrap();
    });
    let output = result.recv_timeout(Duration::from_secs(30));
    let _ = release.send(());
    let slow_result = slow.join().unwrap();
    fast.join().unwrap();
    assert!(started.is_ok(), "guest did not reach host: {slow_result:?}");
    assert!(output.unwrap().is_ok(), "guest start held cold compiler");
    assert!(slow_result.is_err());
    assert!(!cache_guard(module_cache()).contains_key("float_prior:slow-start"));
}

#[test]
fn error_input() {
    for (raw, expected) in [("{}", None), (r#"{"stable_errors":null}"#, None),
        (r#"{"stable_errors":false}"#, Some(false)), (r#"{"stable_errors":true}"#, Some(true))] {
        let input: Payload = serde_json::from_str(raw).unwrap();
        assert_eq!(input.stable_errors, expected);
    }
    for raw in [r#"{"stable_errors":true,"stable_errors":false}"#,
        r#"{"stable_errors":null,"stable_\u0065rrors":true}"#,
        r#"{"stable_errors":1}"#] {
        assert!(serde_json::from_str::<Payload>(raw).is_err());
    }
}

#[test]
fn element_error() {
    let _guard = tests::cache_test();
    for profile in ["standard", "manifest", "compute"] {
        for float in [false, true] {
            let mut input = tests::table_payload(1, profile, true, Some(1));
            input.wire.float_mode = Some(float);
            let first = run_describe(&input).unwrap_err().to_string();
            assert_eq!(first, "wasm instantiate failed: element segment does not fit: offset = 1 amount = 1");
            for _ in 0..3 {
                assert!(run_describe(&table_input(1, profile, true)).is_ok());
                assert_eq!(run_describe(&input).unwrap_err().to_string(), first);
                cache_guard(module_cache()).clear();
                assert_eq!(run_describe(&input).unwrap_err().to_string(), first);
            }
            assert!(run_describe(&tests::table_payload(1, profile, true, Some(0))).is_ok());
            let prior = tests::table_payload(1, profile, false, Some(1));
            let old = run_describe(&prior).unwrap_err().to_string();
            assert!(old.contains("StoreIdx("));
            assert_ne!(run_describe(&prior).unwrap_err().to_string(), old);
        }
    }
}

#[test]
fn code_release() {
    let _guard = tests::cache_test();
    for profile in ["standard", "manifest", "compute"] {
        for float in [false, true] {
            let mut input = table_input(1, profile, true);
            input.wire.float_mode = Some(float);
            let runtime = Runtime::instantiate(&input, true).unwrap();
            let engine = runtime.store.engine().weak();
            cache_guard(module_cache()).clear();
            assert!(engine.upgrade().is_some());
            drop(runtime);
            assert!(engine.upgrade().is_none(), "code retained after last user");
        }
    }
}

#[test]
fn code_owner() {
    let _guard = tests::cache_test();
    for _ in 0..3 {
        let input = table_input(1, "compute", true);
        let owner = Arc::new(SessionOwner::default());
        let runtime = Runtime::prepare_owned(&input, true, false, None, Some(owner.clone())).unwrap();
        let engine = runtime.store.engine().weak();
        drop(runtime);
        assert!(engine.upgrade().is_some());
        drop(owner);
        assert!(engine.upgrade().is_none(), "replay code retained after owner");
    }
}

#[test]
fn code_eviction() {
    let _guard = tests::cache_test();
    cache_guard(module_cache()).clear();
    let input = table_input(1, "standard", true);
    let mut held = Runtime::instantiate(&input, true).unwrap();
    let engine = held.store.engine().weak();
    let mut retired = Vec::new();
    for n in 2..(MODULE_CACHE_LIMIT + 8) {
        let runtime = Runtime::instantiate(&table_input(n as u64, "standard", true), true).unwrap();
        retired.push(runtime.store.engine().weak());
        drop(runtime);
        assert!(cache_guard(module_cache()).len() <= MODULE_CACHE_LIMIT);
    }
    let call = held.instance.get_typed_func::<(), i32>(&held.store, "grow").unwrap();
    assert_eq!(call.call(&mut held.store, ()).unwrap(), 1);
    cache_guard(module_cache()).clear();
    assert!(retired.iter().all(|code| code.upgrade().is_none()));
    assert!(engine.upgrade().is_some());
    drop(held);
    assert!(engine.upgrade().is_none());
}

#[test]
fn code_retry() {
    let _guard = tests::cache_test();
    let mut input = table_input(1, "standard", true);
    input.wire.code_cache_key = Some("code-retry".to_owned());
    let output = run_describe(&input).unwrap();
    let runtime = Runtime::instantiate(&input, true).unwrap();
    let mut cached = table_input(1, "standard", true);
    cached.wire.code_cache_key = input.wire.code_cache_key.clone();
    cached.wire.code_b64 = None;
    let repeat = Runtime::instantiate(&cached, true).unwrap();
    assert!(Engine::same(runtime.store.engine(), repeat.store.engine()));
    drop(repeat);
    drop(runtime);
    cache_guard(module_cache()).clear();
    assert!(Runtime::instantiate(&cached, true).err().unwrap()
        .starts_with("missing wasm module cache: "));
    assert_eq!(run_describe(&input).unwrap(), output);
    let engine = cache_guard(module_cache()).values().next().unwrap().module.engine().weak();
    for entry in cache_guard(module_cache()).values_mut() {
        entry.updated_at = now_secs() - MODULE_CACHE_TTL_SECS - 1.0;
    }
    prune_module_cache_locked(&mut cache_guard(module_cache()));
    assert!(engine.upgrade().is_none(), "expired code retained");
}

fn padded_code(size: usize, tag: &str) -> Payload<'static> {
    use wasm_encoder::{Encode, Section};
    let mut input = table_input(1, "standard", true);
    let mut bytes = decode_b64("code_b64", input.code_b64.as_deref().unwrap()).unwrap();
    let data = vec![0u8; size];
    let section = wasm_encoder::CustomSection { name: Cow::Borrowed(tag), data: Cow::Borrowed(&data) };
    bytes.push(section.id());
    section.encode(&mut bytes);
    input.wire.code_b64 = Some(TextRef(Cow::Owned(encode_b64(&bytes))));
    input
}

#[test]
fn code_failure_cache() {
    let _guard = tests::cache_test();
    for profile in ["standard", "manifest", "compute"] {
        for limit in [false, true] {
            cache_guard(module_cache()).clear();
            let input = tests::table_payload(1, profile, limit, Some(1));
            assert!(Runtime::instantiate(&input, true).is_err());
            assert!(cache_guard(module_cache()).is_empty(), "failed instance retained code");
            let owner = Arc::new(SessionOwner::default());
            let input = tests::table_payload(1, "compute", limit, Some(1));
            assert!(Runtime::prepare_owned(&input, true, false, None, Some(owner.clone())).is_err());
            assert!(cache_guard(&owner.modules).is_empty(), "failed replay retained code");
        }
    }
}

#[test]
fn code_byte_limit() {
    let _guard = tests::cache_test();
    for owned in [false, true] {
        let owner = owned.then(|| Arc::new(SessionOwner::default()));
        let modules = owner.as_ref().map_or_else(|| module_cache(), |o| &o.modules);
        cache_guard(modules).clear();
        let mut engines = Vec::new();
        for index in 0..4 {
            let mut input = padded_code(1024 * 1024, &format!("bytes-{index}"));
            if owned { input.wire.execution_profile = Some("compute".to_owned()); }
            let runtime = Runtime::prepare_owned(&input, true, false, None, owner.clone()).unwrap();
            assert!(runtime.code_retained);
            engines.push(runtime.store.engine().weak());
            drop(runtime);
            assert!(module_weight(&cache_guard(modules)) <= MODULE_CACHE_BYTES, "code weight exceeds cap");
        }
        assert!(engines[0].upgrade().is_none(), "old code survived byte eviction");
        assert!(engines[3].upgrade().is_some());
        cache_guard(modules).clear();
        let mut input = padded_code(MODULE_CACHE_BYTES / 16, "over-cache");
        if owned { input.wire.execution_profile = Some("compute".to_owned()); }
        let mut runtime = Runtime::prepare_owned(&input, true, false, None, owner.clone()).unwrap();
        assert!(!runtime.code_retained);
        let engine = runtime.store.engine().weak();
        assert!(cache_guard(modules).is_empty(), "large code retained");
        let call = runtime.instance.get_typed_func::<(), i32>(&runtime.store, "grow").unwrap();
        assert_eq!(call.call(&mut runtime.store, ()).unwrap(), 1);
        drop(runtime);
        assert!(engine.upgrade().is_none());
        assert!(Runtime::prepare_owned(&input, true, false, None, owner.clone()).is_ok());
    }
}

#[test]
fn code_build_hit() {
    let _guard = tests::cache_test();
    cache_guard(module_cache()).clear();
    let input = table_input(1, "standard", true);
    drop(Runtime::instantiate(&input, true).unwrap());
    let build = cache_guard(module_build());
    let (send, recv) = sync_channel(1);
    let thread = std::thread::spawn(move || {
        send.send(Runtime::instantiate(&input, true).map(|_| ())).unwrap();
    });
    let result = recv.recv_timeout(Duration::from_secs(30));
    drop(build);
    thread.join().unwrap();
    assert!(result.unwrap().is_ok(), "cache hit waited for compiler");
}

#[test]
fn code_weight_fields() {
    use wasm_encoder::{Module as Wasm, TypeSection, FunctionSection, CodeSection, Function, Instruction};
    let make = |count| {
        let mut module = Wasm::new();
        let mut types = TypeSection::new();
        types.ty().function([], []);
        module.section(&types);
        let mut functions = FunctionSection::new();
        let mut code = CodeSection::new();
        for _ in 0..count {
            functions.function(0);
            let mut body = Function::new([]);
            body.instruction(&Instruction::End);
            code.function(&body);
        }
        module.section(&functions).section(&code);
        module.finish()
    };
    let small = make(1);
    let many = make(1000);
    assert!(code_weight(&many) >= code_weight(&small) + 999 * 1024);
    assert_eq!(code_weight(&[0]), usize::MAX);
}

#[test]
fn code_build_owner() {
    let _guard = tests::cache_test();
    let build = cache_guard(module_build());
    let (send, recv) = sync_channel(1);
    let thread = std::thread::spawn(move || {
        let owner = Arc::new(SessionOwner::default());
        let input = table_input(1, "compute", true);
        send.send(Runtime::prepare_owned(&input, true, false, None, Some(owner)).map(|_| ())).unwrap();
    });
    let result = recv.recv_timeout(Duration::from_secs(30));
    drop(build);
    thread.join().unwrap();
    assert!(result.unwrap().is_ok(), "replay waited for ordinary compiler");
}

#[test]
fn code_build_once() {
    let _guard = tests::cache_test();
    cache_guard(module_cache()).clear();
    let input = table_input(1, "standard", true);
    let key = "float_prior:once";
    let entry = module_entry(module_cache(), module_build(), key, "once",
        input.code_b64.as_deref(), false, false).unwrap();
    assert!(module_build().try_lock().is_ok(), "compiler held during guest start");
    assert!(module_cache().try_lock().is_ok(), "compiler holds ready cache");
    let engine = entry.module.engine().clone();
    let (send, recv) = sync_channel(1);
    let thread = std::thread::spawn(move || {
        let entry = module_entry(module_cache(), module_build(), key, "once",
            input.code_b64.as_deref(), false, false).unwrap();
        send.send(entry.module.engine().clone()).unwrap();
    });
    let second = recv.recv_timeout(Duration::from_secs(30)).unwrap();
    thread.join().unwrap();
    assert!(Engine::same(&engine, &second), "same cold code compiled twice");
    keep_module(module_cache(), key.to_owned(), (*entry).clone());
}

#[test]
fn code_build_miss() {
    let _guard = tests::cache_test();
    cache_guard(module_cache()).clear();
    let build = cache_guard(module_build());
    let (send, recv) = sync_channel(1);
    let thread = std::thread::spawn(move || {
        send.send(module_entry(module_cache(), module_build(), "absent", "absent", None, false, false)
            .map(|_| ())).unwrap();
    });
    let result = recv.recv_timeout(Duration::from_secs(30));
    drop(build);
    thread.join().unwrap();
    assert_eq!(result.unwrap().unwrap_err().to_string(), "missing wasm module cache: absent");
}

#[test]
fn code_pending_limit() {
    let _guard = tests::cache_test();
    let modules = Mutex::new(HashMap::new());
    let build = Mutex::new(HashMap::new());
    let input = table_input(1, "standard", true);
    let mut entries = Vec::new();
    for n in 0..(MODULE_CACHE_LIMIT + 8) {
        entries.push(module_entry(&modules, &build, &n.to_string(), "code",
            input.code_b64.as_deref(), false, false).unwrap());
        assert!(cache_guard(&build).len() <= MODULE_CACHE_LIMIT);
    }
    let engines: Vec<_> = entries.iter().map(|entry| entry.module.engine().weak()).collect();
    drop(entries);
    assert!(engines.iter().all(|entry| entry.upgrade().is_none()));
    let entry = module_entry(&modules, &build, "next", "code", input.code_b64.as_deref(), false, false).unwrap();
    assert_eq!(cache_guard(&build).len(), 1);
    assert!(modules.lock().unwrap().is_empty());
    drop(entry);
}

#[test]
fn code_stack_release() {
    use wasm_encoder::{Module as Wasm, TypeSection, FunctionSection, CodeSection, Function,
        Instruction as I, ValType, ExportSection, ExportKind, MemorySection, MemoryType, BlockType};
    let _guard = tests::cache_test();
    cache_guard(module_cache()).clear();
    let mut module = Wasm::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::I32], [ValType::I32]);
    types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
    module.section(&types);
    let mut functions = FunctionSection::new();
    functions.function(0).function(1);
    module.section(&functions);
    let mut memory = MemorySection::new();
    memory.memory(MemoryType { minimum: 1, maximum: Some(1), memory64: false,
        shared: false, page_size_log2: None });
    module.section(&memory);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("octra_alloc", ExportKind::Func, 0);
    exports.export("octra_query", ExportKind::Func, 1);
    module.section(&exports);
    let mut code = CodeSection::new();
    let mut function = Function::new([]);
    for op in [I::LocalGet(0), I::I32Eqz, I::If(BlockType::Result(ValType::I32)), I::I32Const(0),
        I::Else, I::LocalGet(0), I::I32Const(1), I::I32Sub, I::Call(0), I::I32Const(1),
        I::I32Add, I::End, I::End] { function.instruction(&op); }
    code.function(&function);
    let mut query = Function::new([]);
    query.instruction(&I::I32Const(0)).instruction(&I::End);
    code.function(&query);
    module.section(&code);
    let input = Payload::deserialize(json!({"code_b64": encode_b64(&module.finish()),
        "execution_profile": "standard", "is_view": true, "fuel_limit": 100_000})).unwrap();
    let mut runtime = Runtime::instantiate(&input, true).unwrap();
    let call = runtime.instance.get_typed_func::<i32, i32>(&runtime.store, "octra_alloc").unwrap();
    assert_eq!(call.call(&mut runtime.store, 512).unwrap(), 512);
    let engine = runtime.store.engine().weak();
    drop(runtime);
    let engine = engine.upgrade().unwrap();
    assert!(format!("{engine:?}").contains("EngineStacks { stacks: []"), "cached engine retained execution stack");
}