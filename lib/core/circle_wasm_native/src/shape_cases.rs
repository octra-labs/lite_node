// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::*;
use wasm_encoder::{Module as Wasm, TypeSection, FunctionSection, CodeSection, Function,
    Instruction as I, ValType, GlobalSection, GlobalType, ConstExpr, DataSection,
    ElementSection, Elements, ImportSection, EntityType, ExportSection, ExportKind};

fn functions(count: u32, float: bool) -> Vec<u8> {
    let mut module = Wasm::new();
    let mut types = TypeSection::new();
    types.ty().function([ValType::I32], [ValType::I32]);
    types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
    module.section(&types);
    let mut funcs = FunctionSection::new();
    let mut code = CodeSection::new();
    for n in 0..count {
        funcs.function(if n == 1 { 1 } else { 0 });
        let mut body = Function::new([]);
        if float {
            body.instruction(&I::F32Const(f32::NAN)).instruction(&I::Drop);
        }
        body.instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&body);
    }
    module.section(&funcs);
    let mut memory = wasm_encoder::MemorySection::new();
    memory.memory(wasm_encoder::MemoryType { minimum: 1, maximum: Some(1),
        memory64: false, shared: false, page_size_log2: None });
    module.section(&memory);
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0).export("octra_alloc", ExportKind::Func, 0)
        .export("octra_query", ExportKind::Func, 1);
    module.section(&exports).section(&code);
    module.finish()
}

#[test]
fn shape_counts() {
    use code_shape::*;
    for (name, limit) in [("globals", GLOBALS), ("data segments", SEGMENTS),
        ("elements", SEGMENTS), ("types", TYPES), ("imports", IMPORTS), ("exports", EXPORTS),
        ("parameters", ARITY), ("results", ARITY)] {
        for count in [limit, limit + 1] {
            let mut module = Wasm::new();
            match name {
                "globals" => {
                    let mut section = GlobalSection::new();
                    for _ in 0..count {
                        section.global(GlobalType { val_type: ValType::I32, mutable: false, shared: false },
                            &ConstExpr::i32_const(0));
                    }
                    module.section(&section);
                }
                "data segments" => {
                    let mut section = DataSection::new();
                    for _ in 0..count { section.passive([]); }
                    module.section(&section);
                }
                "elements" => {
                    let mut section = ElementSection::new();
                    for _ in 0..count { section.passive(Elements::Functions(Cow::Borrowed(&[]))); }
                    module.section(&section);
                }
                "types" | "parameters" | "results" => {
                    let mut section = TypeSection::new();
                    if name == "types" {
                        for _ in 0..count { section.ty().function([], []); }
                    } else {
                        section.ty().function(
                            vec![ValType::I32; if name == "parameters" { count as usize } else { 0 }],
                            vec![ValType::I32; if name == "results" { count as usize } else { 0 }]);
                    }
                    module.section(&section);
                }
                "imports" => {
                    let mut section = ImportSection::new();
                    for n in 0..count { section.import("octra", &n.to_string(), EntityType::Function(0)); }
                    module.section(&section);
                }
                "exports" => {
                    let mut section = ExportSection::new();
                    for n in 0..count { section.export(&n.to_string(), ExportKind::Func, 0); }
                    module.section(&section);
                }
                _ => unreachable!(),
            }
            let result = check(&module.finish());
            if count == limit { assert_eq!(result, Ok(()), "{name}"); }
            else { assert_eq!(result.unwrap_err(), format!("wasm {name} limit exceeded")); }
        }
    }
}

#[test]
fn shape_locals() {
    for count in [code_shape::LOCALS, code_shape::LOCALS + 1, u32::MAX] {
        let mut module = Wasm::new();
        let mut code = CodeSection::new();
        let mut body = Function::new([(count, ValType::I32)]);
        body.instruction(&I::End);
        code.function(&body);
        module.section(&code);
        assert_eq!(code_shape::check(&module.finish()).is_ok(), count == code_shape::LOCALS);
    }
    let mut module = Wasm::new();
    let mut code = CodeSection::new();
    let mut body = Function::new([(code_shape::LOCALS, ValType::I32), (1, ValType::I64)]);
    body.instruction(&I::End);
    code.function(&body);
    module.section(&code);
    assert_eq!(code_shape::check(&module.finish()).unwrap_err(), "wasm locals limit exceeded");
}

#[test]
fn shape_import_total() {
    for name in ["functions", "globals"] {
        let mut module = Wasm::new();
        let mut imports = ImportSection::new();
        imports.import("octra", "one", if name == "functions" { EntityType::Function(0) }
            else { EntityType::Global(GlobalType { val_type: ValType::I32, mutable: false, shared: false }) });
        module.section(&imports);
        if name == "functions" {
            let mut funcs = FunctionSection::new();
            for _ in 0..code_shape::FUNCTIONS { funcs.function(0); }
            module.section(&funcs);
        } else {
            let mut globals = GlobalSection::new();
            for _ in 0..code_shape::GLOBALS {
                globals.global(GlobalType { val_type: ValType::I32, mutable: false, shared: false },
                    &ConstExpr::i32_const(0));
            }
            module.section(&globals);
        }
        assert_eq!(code_shape::check(&module.finish()).unwrap_err(), format!("wasm {name} limit exceeded"));
    }
}

#[test]
fn shape_modes() {
    let _guard = tests::cache_test();
    for profile in ["standard", "manifest", "compute"] {
        for owned in [false, true] {
            if owned && profile != "compute" { continue; }
            for float in [false, true] {
                for count in [code_shape::FUNCTIONS, code_shape::FUNCTIONS + 1] {
                    let owner = owned.then(|| Arc::new(SessionOwner::default()));
                    let bytes = functions(count, float);
                    let mut input = Payload::deserialize(json!({
                        "code_b64": encode_b64(&bytes), "execution_profile": profile,
                        "float_mode": float, "is_view": true, "fuel_limit": 100_000,
                        "code_cache_key": format!("shape-{profile}-{owned}-{float}-{count}")
                    })).unwrap();
                    for active in [false, true, false, true] {
                        input.code_limit = Some(active);
                        let result = Runtime::prepare_owned(&input, true, false, None, owner.clone());
                        if active && count > code_shape::FUNCTIONS {
                            assert_eq!(result.unwrap_err().to_string(), "wasm functions limit exceeded");
                        } else {
                            assert!(result.is_ok(), "{result:?}");
                        }
                    }
                    input.wire.code_b64 = None;
                    input.code_limit = Some(true);
                    let result = Runtime::prepare_owned(&input, true, false, None, owner);
                    if owned {
                        assert_eq!(result.unwrap_err().to_string(), "compute replay code missing");
                    } else if count > code_shape::FUNCTIONS {
                        assert!(result.unwrap_err().to_string().starts_with("missing wasm module cache:"));
                    } else { assert!(result.is_ok(), "{result:?}"); }
                }
            }
        }
    }
}

#[test]
fn shape_input() {
    for (raw, mode) in [("{}", None), (r#"{"code_limit":null}"#, None),
        (r#"{"code_limit":true}"#, Some(true)), (r#"{"code_limit":false}"#, Some(false))] {
        assert_eq!(serde_json::from_str::<Payload>(raw).unwrap().code_limit, mode);
    }
    for raw in [r#"{"code_limit":true,"code_limit":false}"#,
        r#"{"code_limit":null,"code_\u006cimit":true}"#, r#"{"code_limit":1}"#] {
        assert!(serde_json::from_str::<Payload>(raw).is_err());
    }
    assert!(code_shape::check(b"not wasm").is_err());
    let mut bytes = functions(2, false);
    bytes.pop();
    assert!(code_shape::check(&bytes).is_err());
}

#[test]
fn shape_replay() {
    let _guard = tests::cache_test();
    for count in [code_shape::FUNCTIONS, code_shape::FUNCTIONS + 1] {
        let code = encode_b64(&functions(count, false));
        let step = |active| json!({
            "action": "execute", "code_b64": code, "execution_profile": "compute",
            "is_view": true, "caller": "shape-owner", "address": "shape-code",
            "compute_session_scope": "a".repeat(64), "export_name": "octra_query",
            "request_b64": "", "fuel_limit": 100_000, "hfhe_strict": true,
            "table_limit": true, "stable_errors": true, "code_limit": active
        }).to_string();
        for modes in [(false, false), (false, true), (true, false), (true, true)] {
            let result = compute_replay::run(&json!({"replay_steps": [
                step(modes.0), step(modes.1)]}).to_string());
            if count > code_shape::FUNCTIONS && (modes.0 || modes.1) {
                assert_eq!(result.unwrap_err().to_string(), "wasm functions limit exceeded");
            } else {
                let result = result.unwrap();
                assert_eq!(result["replies"].as_array().unwrap().len(), 2);
                for reply in result["replies"].as_array().unwrap() {
                    assert_eq!(reply["compute_exit"], 0);
                }
            }
        }
    }
}