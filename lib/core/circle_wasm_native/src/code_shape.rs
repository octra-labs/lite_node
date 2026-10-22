// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use wasmparser::{Parser, Payload, TypeRef};

pub(super) const FUNCTIONS: u32 = 10_000;
pub(super) const GLOBALS: u32 = 1_000;
pub(super) const SEGMENTS: u32 = 1_000;
pub(super) const TYPES: u32 = 10_000;
pub(super) const IMPORTS: u32 = 256;
pub(super) const EXPORTS: u32 = 256;
pub(super) const ARITY: u32 = 32;
pub(super) const LOCALS: u32 = 4_096;

fn cap(count: u32, limit: u32, name: &str) -> Result<(), String> {
    if count <= limit { Ok(()) } else { Err(format!("wasm {name} limit exceeded")) }
}

fn add(total: &mut u32, count: u32, limit: u32, name: &str) -> Result<(), String> {
    *total = total.checked_add(count).ok_or_else(|| format!("wasm {name} limit exceeded"))?;
    cap(*total, limit, name)
}

pub(super) fn check(bytes: &[u8]) -> Result<(), String> {
    let mut functions = 0;
    let mut globals = 0;
    let mut tables = 0;
    let mut memories = 0;
    for part in Parser::new(0).parse_all(bytes) {
        match part.map_err(|_| "wasm section invalid")? {
            Payload::TypeSection(items) => {
                cap(items.count(), TYPES, "types")?;
                for item in items.into_iter_err_on_gc_types() {
                    let ty = item.map_err(|_| "wasm type invalid")?;
                    cap(ty.params().len() as u32, ARITY, "parameters")?;
                    cap(ty.results().len() as u32, ARITY, "results")?;
                }
            }
            Payload::ImportSection(items) => {
                cap(items.count(), IMPORTS, "imports")?;
                for item in items {
                    match item.map_err(|_| "wasm import invalid")?.ty {
                        TypeRef::Func(_) => add(&mut functions, 1, FUNCTIONS, "functions")?,
                        TypeRef::Global(_) => add(&mut globals, 1, GLOBALS, "globals")?,
                        TypeRef::Table(_) => add(&mut tables, 1, 1, "tables")?,
                        TypeRef::Memory(_) => add(&mut memories, 1, 1, "memories")?,
                        _ => (),
                    }
                }
            }
            Payload::FunctionSection(items) => add(&mut functions, items.count(), FUNCTIONS, "functions")?,
            Payload::GlobalSection(items) => add(&mut globals, items.count(), GLOBALS, "globals")?,
            Payload::TableSection(items) => add(&mut tables, items.count(), 1, "tables")?,
            Payload::MemorySection(items) => add(&mut memories, items.count(), 1, "memories")?,
            Payload::ExportSection(items) => cap(items.count(), EXPORTS, "exports")?,
            Payload::ElementSection(items) => cap(items.count(), SEGMENTS, "elements")?,
            Payload::DataSection(items) => cap(items.count(), SEGMENTS, "data segments")?,
            Payload::DataCountSection { count, .. } => cap(count, SEGMENTS, "data segments")?,
            Payload::CodeSectionStart { count, .. } => cap(count, FUNCTIONS, "functions")?,
            Payload::CodeSectionEntry(body) => {
                let mut locals = 0;
                for group in body.get_locals_reader().map_err(|_| "wasm locals invalid")? {
                    let (count, _) = group.map_err(|_| "wasm locals invalid")?;
                    add(&mut locals, count, LOCALS, "locals")?;
                }
            }
            _ => (),
        }
    }
    Ok(())
}