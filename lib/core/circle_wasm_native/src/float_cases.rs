// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::float_ops;
use wasm_encoder::{
    CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction as I, Module,
    TypeSection, ValType,
};

#[test]
fn prior_simd_rejection() {
    let bytes = program(ValType::I32, &[I::V128Const(0), I::Drop, I::I32Const(0)]);
    let error = wasmi::Module::new(&wasmi::Engine::default(), &bytes).unwrap_err();
    assert!(error.to_string().starts_with("unexpected SIMD opcode"), "{error}");
    let error = wasmi::Module::validate(&wasmi::Engine::default(), &bytes).unwrap_err();
    assert!(error.to_string().starts_with("unexpected SIMD opcode"), "{error}");
    let global = hex::decode("0061736d010000000616017f00fd0c000000000000000000000000000000000b").unwrap();
    let error = wasmi::Module::new(&wasmi::Engine::default(), &global).unwrap_err();
    assert_eq!(error.to_string(), "unexpected SIMD opcode: 0xfd (at offset 0xd)");
}

#[test]
fn float_translation_calls() {
    let bytes = program(ValType::I64, &[
        I::F64Const(0.), I::F64Const(0.), I::F64Div, I::I64ReinterpretF64,
    ]);
    let active = float_ops::prepare(&wasmi::Engine::default(), &bytes).unwrap();
    let body = wasmparser::Parser::new(0).parse_all(&active).find_map(|item| {
        match item.unwrap() {
            wasmparser::Payload::CodeSectionEntry(body) => Some(body),
            _ => None,
        }
    }).unwrap();
    let mut ops = body.get_operators_reader().unwrap();
    let mut calls = 0;
    while !ops.eof() {
        match ops.read().unwrap() {
            wasmparser::Operator::F64Div => panic!("guest arithmetic can fold on host"),
            wasmparser::Operator::Call { .. } => calls += 1,
            _ => (),
        }
    }
    assert_eq!(calls, 1);
}

#[test]
fn float_custom_name() {
    let bytes = program(ValType::I64, &[
        I::F64Const(0.), I::F64Const(0.), I::F64Div, I::I64ReinterpretF64,
    ]);
    let mut named = bytes.clone();
    named.extend([0, 8, 4, b'n', b'a', b'm', b'e', 1, 2, 0xff]);
    wasmi::Module::validate(&wasmi::Engine::default(), &named).unwrap();
    assert_eq!(execute(&bytes, false, true), execute(&named, false, true));
    assert_eq!(execute(&bytes, true, true), execute(&named, true, true));
    let active = float_ops::prepare(&wasmi::Engine::default(), &named).unwrap();
    let sections: Vec<_> = wasmparser::Parser::new(0).parse_all(&active)
        .filter_map(|item| match item.unwrap() {
            wasmparser::Payload::CustomSection(section) => Some(section.data().to_vec()),
            _ => None,
        }).collect();
    assert_eq!(sections, vec![vec![1, 2, 0xff]]);
}

#[test]
fn float_fuel_boundary() {
    for count in [1, 32, 256] {
        let mut ops = vec![I::F64Const(1.)];
        for _ in 0..count {
            ops.extend([I::F64Const(1.), I::F64Add]);
        }
        ops.push(I::I64ReinterpretF64);
        let bytes = program(ValType::I64, &ops);
        let prior = execute(&bytes, false, true);
        let active = execute(&bytes, true, true);
        assert_eq!(prior.0, active.0);
        assert!(active.1 > prior.1);
        let mut config = wasmi::Config::default();
        config.consume_fuel(true);
        let engine = wasmi::Engine::new(&config);
        let prepared = float_ops::prepare(&engine, &bytes).unwrap();
        let module = wasmi::Module::new(&engine, &prepared).unwrap();
        for limit in [active.1 - 1, active.1] {
            let mut store = wasmi::Store::new(&engine, ());
            store.set_fuel(limit).unwrap();
            let instance = wasmi::Linker::new(&engine).instantiate(&mut store, &module)
                .unwrap().start(&mut store).unwrap();
            let result = instance.get_typed_func::<(), i64>(&store, "run").unwrap()
                .call(&mut store, ());
            assert_eq!(result.is_ok(), limit == active.1);
        }
        println!("event = float_budget count = {count} prior = {} active = {}", prior.1, active.1);
    }
}

#[test]
fn float_constant_pool() {
    for count in 1..=48 {
        for double in [false, true] {
            let mut ops = Vec::new();
            for index in 0..count {
                if double {
                    ops.extend([
                        I::F64Const(f64::from_bits(0x7ff8000000000000 + index)),
                        I::I64ReinterpretF64,
                    ]);
                } else {
                    ops.extend([
                        I::F32Const(f32::from_bits(0x7fc00000 + index as u32)),
                        I::I32ReinterpretF32,
                    ]);
                }
            }
            if double {
                ops.extend([I::F64Const(0.), I::F64Const(0.), I::F64Div, I::I64ReinterpretF64]);
                ops.extend((0..count).map(|_| I::I64Xor));
            } else {
                ops.extend([I::F32Const(0.), I::F32Const(0.), I::F32Div, I::I32ReinterpretF32]);
                ops.extend((0..count).map(|_| I::I32Xor));
            }
            let base = if double { 0x7ff8000000000000 } else { 0x7fc00000 };
            let expected = (0..count).fold(base, |value, index| value ^ (base + index));
            let bytes = program(if double { ValType::I64 } else { ValType::I32 }, &ops);
            let (value, fuel) = execute(&bytes, true, double);
            assert_eq!(value, expected);
            assert_eq!(fuel, 14 + count + (count + 2) / 8, "constant-pool fuel differs");
            println!("event = float_pool double = {double} count = {count} bits = {value:016x} fuel = {fuel}");
        }
    }
}

fn program(ty: ValType, ops: &[I<'_>]) -> Vec<u8> {
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([], [ty]);
    module.section(&types);
    let mut functions = FunctionSection::new();
    functions.function(0);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("run", ExportKind::Func, 0);
    module.section(&exports);
    let mut body = Function::new([]);
    for op in ops {
        body.instruction(op);
    }
    body.instruction(&I::End);
    let mut code = CodeSection::new();
    code.function(&body);
    module.section(&code);
    module.finish()
}

fn execute(bytes: &[u8], active: bool, double: bool) -> (u64, u64) {
    let mut config = wasmi::Config::default();
    config.consume_fuel(true);
    let engine = wasmi::Engine::new(&config);
    let bytes = if active {
        float_ops::prepare(&engine, bytes).unwrap()
    } else {
        bytes.to_vec()
    };
    let module = wasmi::Module::new(&engine, &bytes).unwrap();
    let mut store = wasmi::Store::new(&engine, ());
    store.set_fuel(10000).unwrap();
    let instance = wasmi::Linker::new(&engine)
        .instantiate(&mut store, &module)
        .unwrap()
        .start(&mut store)
        .unwrap();
    let value = if double {
        instance
            .get_typed_func::<(), i64>(&store, "run")
            .unwrap()
            .call(&mut store, ())
            .unwrap() as u64
    } else {
        instance
            .get_typed_func::<(), i32>(&store, "run")
            .unwrap()
            .call(&mut store, ())
            .unwrap() as u32 as u64
    };
    let fuel = 10000 - store.get_fuel().unwrap();
    if active {
        println!("event = float_result double = {double} bits = {value:016x} fuel = {fuel}");
    }
    (value, fuel)
}

#[test]
fn float_arithmetic_nan() {
    for payload in [0x7fc00001u32, 0xffa00042, 0xffc00000, 0x7f800001] {
        for op in [
            I::F32Add,
            I::F32Sub,
            I::F32Mul,
            I::F32Div,
            I::F32Min,
            I::F32Max,
        ] {
            for swap in [false, true] {
                let nan = I::F32Const(f32::from_bits(payload));
                let one = I::F32Const(1.);
                let operands = if swap { [one, nan] } else { [nan, one] };
                let bytes = program(
                    ValType::I32,
                    &[
                        operands[0].clone(),
                        operands[1].clone(),
                        op.clone(),
                        I::I32ReinterpretF32,
                    ],
                );
                assert_eq!(execute(&bytes, true, false).0, 0x7fc00000);
            }
        }
        for op in [
            I::F32Ceil,
            I::F32Floor,
            I::F32Trunc,
            I::F32Nearest,
            I::F32Sqrt,
        ] {
            let bytes = program(
                ValType::I32,
                &[
                    I::F32Const(f32::from_bits(payload)),
                    op,
                    I::I32ReinterpretF32,
                ],
            );
            assert_eq!(execute(&bytes, true, false).0, 0x7fc00000);
        }
    }
    for payload in [
        0x7ff8000000000001u64,
        0xfff4000000000042,
        0xfff8000000000000,
        0x7ff0000000000001,
    ] {
        for op in [
            I::F64Add,
            I::F64Sub,
            I::F64Mul,
            I::F64Div,
            I::F64Min,
            I::F64Max,
        ] {
            for swap in [false, true] {
                let nan = I::F64Const(f64::from_bits(payload));
                let one = I::F64Const(1.);
                let operands = if swap { [one, nan] } else { [nan, one] };
                let bytes = program(
                    ValType::I64,
                    &[
                        operands[0].clone(),
                        operands[1].clone(),
                        op.clone(),
                        I::I64ReinterpretF64,
                    ],
                );
                assert_eq!(execute(&bytes, true, true).0, 0x7ff8000000000000);
            }
        }
        for op in [
            I::F64Ceil,
            I::F64Floor,
            I::F64Trunc,
            I::F64Nearest,
            I::F64Sqrt,
        ] {
            let bytes = program(
                ValType::I64,
                &[
                    I::F64Const(f64::from_bits(payload)),
                    op,
                    I::I64ReinterpretF64,
                ],
            );
            assert_eq!(execute(&bytes, true, true).0, 0x7ff8000000000000);
        }
    }
}

#[test]
fn float_conversion_nan() {
    let single = program(
        ValType::I32,
        &[
            I::F64Const(f64::from_bits(0xfff80000000000ff)),
            I::F32DemoteF64,
            I::I32ReinterpretF32,
        ],
    );
    assert_eq!(execute(&single, true, false).0, 0x7fc00000);
    let double = program(
        ValType::I64,
        &[
            I::F32Const(f32::from_bits(0xffc000ff)),
            I::F64PromoteF32,
            I::I64ReinterpretF64,
        ],
    );
    assert_eq!(execute(&double, true, true).0, 0x7ff8000000000000);
    let divide = program(
        ValType::I32,
        &[
            I::F32Const(0.),
            I::F32Const(0.),
            I::F32Div,
            I::I32ReinterpretF32,
        ],
    );
    assert_eq!(execute(&divide, true, false).0, 0x7fc00000);
    let sqrt = program(
        ValType::I64,
        &[I::F64Const(-1.), I::F64Sqrt, I::I64ReinterpretF64],
    );
    assert_eq!(execute(&sqrt, true, true).0, 0x7ff8000000000000);
}

#[test]
fn float_preserve_bits() {
    let engine = wasmi::Engine::default();
    for bits in [
        0u32, 0x80000000, 0x3f800000, 0x7f800000, 0xff800000, 0xffc00001,
    ] {
        let bytes = program(
            ValType::I32,
            &[
                I::I32Const(bits as i32),
                I::F32ReinterpretI32,
                I::F32Neg,
                I::I32ReinterpretF32,
            ],
        );
        assert_eq!(float_ops::prepare(&engine, &bytes).unwrap(), bytes);
        assert_eq!(execute(&bytes, true, false), execute(&bytes, false, false));
        assert_eq!(execute(&bytes, true, false).0, (bits ^ 0x80000000) as u64);
    }
    for bits in [
        0u64,
        0x8000000000000000,
        0x3ff0000000000000,
        0x7ff0000000000000,
        0xfff8000000000001,
    ] {
        let bytes = program(
            ValType::I64,
            &[
                I::I64Const(bits as i64),
                I::F64ReinterpretI64,
                I::F64Abs,
                I::I64ReinterpretF64,
            ],
        );
        assert_eq!(float_ops::prepare(&engine, &bytes).unwrap(), bytes);
        assert_eq!(execute(&bytes, true, true), execute(&bytes, false, true));
        assert_eq!(execute(&bytes, true, true).0, bits & 0x7fffffffffffffff);
    }
}

#[test]
fn float_finite_control() {
    for value in [
        -0.0f32,
        0.0,
        1.25,
        -1.25,
        f32::MIN_POSITIVE,
        f32::INFINITY,
        f32::NEG_INFINITY,
    ] {
        for op in [
            I::F32Add,
            I::F32Sub,
            I::F32Mul,
            I::F32Div,
            I::F32Min,
            I::F32Max,
        ] {
            let bytes = program(
                ValType::I32,
                &[
                    I::F32Const(value),
                    I::F32Const(2.0),
                    op,
                    I::I32ReinterpretF32,
                ],
            );
            assert_eq!(
                execute(&bytes, true, false).0,
                execute(&bytes, false, false).0
            );
        }
    }
    for value in [
        -0.0f64,
        0.0,
        1.25,
        -1.25,
        f64::MIN_POSITIVE,
        f64::INFINITY,
        f64::NEG_INFINITY,
    ] {
        for op in [
            I::F64Add,
            I::F64Sub,
            I::F64Mul,
            I::F64Div,
            I::F64Min,
            I::F64Max,
        ] {
            let bytes = program(
                ValType::I64,
                &[
                    I::F64Const(value),
                    I::F64Const(2.0),
                    op,
                    I::I64ReinterpretF64,
                ],
            );
            assert_eq!(
                execute(&bytes, true, true).0,
                execute(&bytes, false, true).0
            );
        }
    }
}

#[test]
fn float_invalid_call() {
    let bytes = program(
        ValType::I32,
        &[
            I::F32Const(0.),
            I::F32Const(0.),
            I::F32Div,
            I::Call(1),
            I::I32ReinterpretF32,
        ],
    );
    assert!(float_ops::prepare(&wasmi::Engine::default(), &bytes).is_err());
}

fn runtime_program() -> Vec<u8> {
    use wasm_encoder::{
        ConstExpr, DataSection, EntityType, ImportSection, MemArg, MemorySection, MemoryType,
        StartSection,
    };
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([], []);
    types.ty().function([ValType::I32], [ValType::I32]);
    types
        .ty()
        .function([ValType::I32, ValType::I32], [ValType::I32]);
    types.ty().function([ValType::I32; 4], [ValType::I32]);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import("octra", "host_kv_put", EntityType::Function(3));
    module.section(&imports);
    let mut functions = FunctionSection::new();
    for ty in [0, 1, 2, 2] {
        functions.function(ty);
    }
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
    let mut exports = ExportSection::new();
    for (name, index) in [
        ("octra_alloc", 2),
        ("octra_query", 3),
        ("octra_manifest", 3),
        ("octra_update", 4),
    ] {
        exports.export(name, ExportKind::Func, index);
    }
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);
    module.section(&StartSection { function_index: 1 });
    let mem = MemArg {
        offset: 0,
        align: 2,
        memory_index: 0,
    };
    let bodies = [
        vec![
            I::I32Const(0),
            I::F32Const(f32::from_bits(0xffc00042)),
            I::F32Const(1.),
            I::F32Add,
            I::F32Store(mem),
        ],
        vec![I::I32Const(1024)],
        vec![
            I::I32Const(16),
            I::F64Const(f64::from_bits(0xfff8000000000042)),
            I::F64Const(1.),
            I::F64Add,
            I::F64Store(mem),
            I::I32Const(0),
            I::I32Load(mem),
        ],
        vec![
            I::I32Const(0),
            I::I32Const(0),
            I::Call(3),
            I::Drop,
            I::I32Const(32),
            I::I32Const(3),
            I::I32Const(0),
            I::I32Const(24),
            I::Call(0),
        ],
    ];
    let mut code = CodeSection::new();
    for ops in bodies {
        let mut function = Function::new([]);
        for op in ops {
            function.instruction(&op);
        }
        function.instruction(&I::End);
        code.function(&function);
    }
    module.section(&code);
    let mut data = DataSection::new();
    data.active(0, &ConstExpr::i32_const(32), b"nan".iter().copied());
    module.section(&data);
    module.finish()
}

#[test]
fn float_runtime_cache() {
    use super::{Payload, Runtime, BASE64};
    use base64::Engine;
    use serde::Deserialize;
    let bytes = runtime_program();
    let mut payload: Payload = Payload::deserialize(serde_json::json!({
        "code_b64": BASE64.encode(bytes), "code_cache_key": "float_runtime",
        "hfhe_strict": true, "is_view": false, "float_mode": false,
        "execution_profile": "standard", "fuel_limit": 10000
    }))
    .unwrap();
    let mut prior = Runtime::instantiate(&payload, false).unwrap();
    prior.call_export("octra_update", &[]).unwrap();
    let prior_storage = prior.store.data().storage.clone();
    let code = payload.code_b64.take();
    payload.float_mode = Some(true);
    let error = Runtime::instantiate(&payload, false).unwrap_err();
    assert!(error.starts_with("missing wasm module cache:"), "{error}");
    payload.code_b64 = code;
    let mut active = Runtime::instantiate(&payload, false).unwrap();
    let (_, _, effort) = active.call_export("octra_update", &[]).unwrap();
    let stored = active.store.data().storage.get("nan").unwrap();
    assert_eq!(&stored[..4], &0x7fc00000u32.to_le_bytes());
    assert_eq!(&stored[16..24], &0x7ff8000000000000u64.to_le_bytes());
    assert!(effort > 0 && effort < 10000);
    println!(
        "event = float_storage bytes = {} effort = {effort}",
        hex::encode(stored.as_slice())
    );
    let expected = active.store.data().storage.clone();
    payload.code_b64 = None;
    for (mode, expected) in [
        (true, expected.clone()),
        (false, prior_storage),
        (true, expected),
    ] {
        payload.float_mode = Some(mode);
        let mut warm = Runtime::instantiate(&payload, false).unwrap();
        let (_, _, warm_effort) = warm.call_export("octra_update", &[]).unwrap();
        assert_eq!(warm.store.data().storage, expected);
        if mode {
            assert_eq!(warm_effort, effort);
        }
    }
    payload.float_mode = Some(true);
    for profile in ["standard", "manifest", "compute"] {
        payload.float_mode = Some(profile != "compute");
        payload.execution_profile = Some(profile.to_owned());
        let mut description = Runtime::instantiate(&payload, true).unwrap();
        let (value, _, effort) = description.call_export("octra_manifest", &[]).unwrap();
        assert_eq!(value as u32, 0x7fc00000);
        let memory = description
            .instance
            .get_memory(&description.store, "memory")
            .unwrap();
        assert_eq!(
            &memory.data(&description.store)[16..24],
            &0x7ff8000000000000u64.to_le_bytes()
        );
        println!(
            "event = float_view profile = {profile} bits = {:08x} effort = {effort}",
            value as u32
        );
    }
}

fn tensor_runtime(name: &str, args: &[wasmi::Val], profile: &str) -> super::Runtime {
    use base64::Engine;
    use serde::Deserialize;
    use wasm_encoder::{EntityType, ImportSection, MemorySection, MemoryType};
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function(args.iter().map(|arg| match arg {
        wasmi::Val::I32(_) => ValType::I32,
        wasmi::Val::I64(_) => ValType::I64,
        _ => panic!("unexpected tensor argument"),
    }), [ValType::I32]);
    types.ty().function([ValType::I32], [ValType::I32]);
    types.ty().function([ValType::I32; 2], [ValType::I32]);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import("octra", name, EntityType::Function(0));
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(0).function(1).function(2);
    module.section(&functions);
    let mut memory = MemorySection::new();
    memory.memory(MemoryType {
        minimum: 1, maximum: Some(1), memory64: false, shared: false, page_size_log2: None,
    });
    module.section(&memory);
    let mut exports = ExportSection::new();
    exports.export("run", ExportKind::Func, 1);
    exports.export("octra_alloc", ExportKind::Func, 2);
    exports.export("octra_query", ExportKind::Func, 3);
    exports.export("memory", ExportKind::Memory, 0);
    module.section(&exports);
    let mut code = CodeSection::new();
    let mut wrapper = Function::new([]);
    for index in 0..args.len() {
        wrapper.instruction(&I::LocalGet(index as u32));
    }
    wrapper.instruction(&I::Call(0)).instruction(&I::End);
    code.function(&wrapper);
    for value in [1024, 0] {
        let mut body = Function::new([]);
        body.instruction(&I::I32Const(value)).instruction(&I::End);
        code.function(&body);
    }
    module.section(&code);
    let input = super::Payload::deserialize(serde_json::json!({
        "code_b64": super::BASE64.encode(module.finish()), "execution_profile": profile,
        "is_view": true, "float_mode": false, "fuel_limit": 20_000_000,
        "hfhe_strict": false,
    })).unwrap();
    super::Runtime::instantiate(&input, false).unwrap()
}

#[test]
fn tensor_float_vectors() {
    use sha2::{Digest, Sha256};
    use std::sync::Arc;
    use wasmi::Val::{I32, I64};
    let _guard = super::tests::cache_test();
    let cases = [
        ("load_i8_kv", vec![I32(768), I32(0), I32(1), I32(0), I32(2), I64(0)], 768, 2),
        ("matmul_fp", vec![I32(768), I32(256), I32(512), I32(1), I32(2), I32(2)], 768, 2),
        ("rmsnorm_fp", vec![I32(256), I32(2), I32(512)], 256, 2),
        ("silu_fp", vec![I32(256), I32(2)], 256, 2),
        ("elemwise_mul_fp", vec![I32(256), I32(512), I32(2)], 256, 2),
        ("residual_add_fp", vec![I32(256), I32(512), I32(2)], 256, 2),
        ("rope_apply_fp", vec![I32(256), I32(4), I32(3), I64(0)], 256, 4),
        ("attention_kv_fp", vec![I32(256), I32(512), I32(640), I32(768),
            I32(2), I32(1), I32(1), I32(2)], 768, 2),
        ("linear_i8_kv", vec![I32(768), I32(256), I32(0), I32(1), I32(2), I32(2), I64(0)], 768, 2),
        ("top1_i8_kv", vec![I32(256), I32(0), I32(1), I32(1), I32(2), I64(0), I32(768)], 768, 1),
    ];
    let words = [0_u64, 0x8000000000000000, 1, 0x0010000000000000, 0x3ff0000000000000,
        0xbff0000000000000, 0x3fd5555555555555, 0x7fefffffffffffff,
        0x7ff0000000000000, 0xfff0000000000000, 0x7ff0000000000042,
        0xfff0000000000042, 0x7ff812345678abcd, 0xfff8fedcba987654];
    for profile in ["standard", "manifest", "compute"] {
        let mut digest = Sha256::new();
        for (name, args, offset, count) in &cases {
            let mut runtime = tensor_runtime(&format!("host_tensor_{name}"), args, profile);
            let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
            let run = runtime.instance.get_func(&runtime.store, "run").unwrap();
            runtime.store.data_mut().storage = Arc::new(super::Storage::from([
                ("w".to_owned(), Arc::new((if *name == "top1_i8_kv" {
                    vec![1, 255]
                } else { vec![1, 255, 1, 255] }).into())),
            ]));
            for left in words {
                for right in words {
                    memory.write(&mut runtime.store, 0, b"w").unwrap();
                    for start in [256, 512, 640] {
                        for (index, value) in [left, right, left, right].iter().enumerate() {
                            memory.write(&mut runtime.store, start + index * 8, &value.to_le_bytes()).unwrap();
                        }
                    }
                    let mut args = args.clone();
                    for arg in &mut args {
                        if matches!(arg, I64(_)) { *arg = I64(right as i64); }
                    }
                    let mut result = [I32(0)];
                    run.call(&mut runtime.store, &args, &mut result).unwrap();
                    assert!(matches!(result[0], I32(value) if value >= 0), "{name}");
                    let bytes = &memory.data(&runtime.store)[*offset..*offset + count * 8];
                    for cell in bytes.chunks_exact(8) {
                        let bits = u64::from_le_bytes(cell.try_into().unwrap());
                        if profile == "compute" && f64::from_bits(bits).is_nan() {
                            assert_eq!(bits, float_ops::NAN_BITS, "{name}");
                        }
                    }
                    if profile == "compute" { digest.update(bytes); }
                }
            }
        }
        if profile == "compute" {
            let digest = format!("{:x}", digest.finalize());
            assert_eq!(digest, "ad9142c8e0443a30bfdd5a52f2570f56de34d4290d69e58ca35f7c72607aa845");
            println!("event = tensor_float digest = {digest}");
        }
    }
}

#[test]
fn tensor_copy_bits() {
    use wasmi::Val::I32;
    let _guard = super::tests::cache_test();
    let args = [I32(768), I32(0), I32(256), I32(2)];
    let mut runtime = tensor_runtime("host_tensor_append_vec_fp", &args, "compute");
    let memory = runtime.instance.get_memory(&runtime.store, "memory").unwrap();
    let bytes: Vec<_> = [0xfff0000000000042_u64, 0x8000000000000000]
        .into_iter().flat_map(u64::to_le_bytes).collect();
    memory.write(&mut runtime.store, 256, &bytes).unwrap();
    runtime.instance.get_func(&runtime.store, "run").unwrap()
        .call(&mut runtime.store, &args, &mut [I32(0)]).unwrap();
    assert_eq!(&memory.data(&runtime.store)[768..784], bytes);
}

#[test]
fn tensor_math_constants() {
    for (actual, expected) in [
        (float_ops::sqrt(2.0, true), 0x3ff6a09e667f3bcd_u64),
        (float_ops::exp(1.0, true), 0x4005bf0a8b14576a),
        (float_ops::pow(2.0, 0.5, true), 0x3ff6a09e667f3bcd),
        (float_ops::sin_cos(1.0, true).0, 0x3feaed548f090cee),
        (float_ops::sin_cos(1.0, true).1, 0x3fe14a280fb5068c),
    ] {
        assert_eq!(actual.to_bits(), expected);
    }
    for bits in [0, 0x8000000000000000, 1, 0x7fefffffffffffff,
        0x7ff0000000000000, 0xfff0000000000000, 0x7ff0000000000042, 0xfff812345678abcd] {
        let value = f64::from_bits(bits);
        assert_eq!(float_ops::bytes(value, false), bits.to_le_bytes());
        assert_eq!(float_ops::bytes(value, true),
            (if value.is_nan() { float_ops::NAN_BITS } else { bits }).to_le_bytes());
    }
}