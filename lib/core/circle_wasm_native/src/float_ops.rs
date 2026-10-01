// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use std::convert::Infallible;
use wasm_encoder::reencode::{self, Reencode};
use wasm_encoder::{
    BlockType, CodeSection, Function, FunctionSection, Instruction, Module, TypeSection, ValType,
};
use wasmparser::{Operator, Parser, Payload, TypeRef};

#[derive(Clone, Copy)]
enum Width {
    Single,
    Double,
}

fn operation(op: &Operator<'_>) -> Option<usize> {
    use Operator::*;
    Some(match op {
        F32Ceil => 0, F32Floor => 1, F32Trunc => 2, F32Nearest => 3,
        F32Sqrt => 4, F32Add => 5, F32Sub => 6, F32Mul => 7,
        F32Div => 8, F32Min => 9, F32Max => 10, F32DemoteF64 => 11,
        F64Ceil => 12, F64Floor => 13, F64Trunc => 14, F64Nearest => 15,
        F64Sqrt => 16, F64Add => 17, F64Sub => 18, F64Mul => 19,
        F64Div => 20, F64Min => 21, F64Max => 22, F64PromoteF32 => 23,
        _ => return None,
    })
}

fn operations() -> Vec<(Instruction<'static>, Vec<ValType>, ValType)> {
    use Instruction::*;
    [F32Ceil, F32Floor, F32Trunc, F32Nearest, F32Sqrt,
     F32Add, F32Sub, F32Mul, F32Div, F32Min, F32Max, F32DemoteF64,
     F64Ceil, F64Floor, F64Trunc, F64Nearest, F64Sqrt,
     F64Add, F64Sub, F64Mul, F64Div, F64Min, F64Max, F64PromoteF32]
        .into_iter().enumerate().map(|(index, op)| {
            let output = if index < 12 { ValType::F32 } else { ValType::F64 };
            let inputs = match index {
                5..=10 | 17..=22 => vec![output, output],
                11 => vec![ValType::F64],
                23 => vec![ValType::F32],
                _ => vec![output],
            };
            (op, inputs, output)
        }).collect()
}

fn nan_function(width: Width) -> Function {
    let (ty, compare, nan) = match width {
        Width::Single => (
            ValType::F32,
            Instruction::F32Ne,
            Instruction::F32Const(f32::from_bits(0x7fc00000)),
        ),
        Width::Double => (
            ValType::F64,
            Instruction::F64Ne,
            Instruction::F64Const(f64::from_bits(0x7ff8000000000000)),
        ),
    };
    let mut code = Function::new([]);
    for op in [
        Instruction::LocalGet(0),
        Instruction::LocalGet(0),
        compare,
        Instruction::If(BlockType::Result(ty)),
        nan,
        Instruction::Else,
        Instruction::LocalGet(0),
        Instruction::End,
        Instruction::End,
    ] {
        code.instruction(&op);
    }
    code
}

struct FloatOps {
    functions: u32,
    types: u32,
}

impl Reencode for FloatOps {
    fn parse_custom_section(
        &mut self,
        module: &mut Module,
        section: wasmparser::CustomSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        module.section(&self.custom_section(section));
        Ok(())
    }
    type Error = Infallible;

    fn parse_type_section(
        &mut self,
        types: &mut TypeSection,
        section: wasmparser::TypeSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_type_section(self, types, section)?;
        self.types = types.len();
        types.ty().function([ValType::F32], [ValType::F32]);
        types.ty().function([ValType::F64], [ValType::F64]);
        for (_, inputs, output) in operations() {
            types.ty().function(inputs, [output]);
        }
        Ok(())
    }

    fn parse_function_section(
        &mut self,
        functions: &mut FunctionSection,
        section: wasmparser::FunctionSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_function_section(self, functions, section)?;
        functions.function(self.types);
        functions.function(self.types + 1);
        for index in 0..24 {
            functions.function(self.types + 2 + index);
        }
        Ok(())
    }

    fn parse_code_section(
        &mut self,
        code: &mut CodeSection,
        section: wasmparser::CodeSectionReader<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        reencode::utils::parse_code_section(self, code, section)?;
        code.function(&nan_function(Width::Single));
        code.function(&nan_function(Width::Double));
        for (op, inputs, output) in operations() {
            let mut function = Function::new([]);
            for index in 0..inputs.len() {
                function.instruction(&Instruction::LocalGet(index as u32));
            }
            function.instruction(&op);
            function.instruction(&Instruction::Call(self.functions
                + if output == ValType::F32 { 0 } else { 1 }));
            function.instruction(&Instruction::End);
            code.function(&function);
        }
        Ok(())
    }

    fn parse_function_body(
        &mut self,
        code: &mut CodeSection,
        body: wasmparser::FunctionBody<'_>,
    ) -> Result<(), reencode::Error<Self::Error>> {
        let mut function = self.new_function_with_parsed_locals(&body)?;
        let mut reader = body.get_operators_reader()?;
        while !reader.eof() {
            let op = reader.read()?;
            if let Some(index) = operation(&op) {
                function.instruction(&Instruction::Call(self.functions + 2 + index as u32));
            } else {
                function.instruction(&self.instruction(op)?);
            }
        }
        code.function(&function);
        Ok(())
    }
}

pub fn prepare(engine: &wasmi::Engine, bytes: &[u8]) -> Result<Vec<u8>, String> {
    wasmi::Module::validate(engine, bytes).map_err(|e| format!("wasm validation failed: {e}"))?;
    let mut functions = 0u32;
    let mut needed = false;
    for item in Parser::new(0).parse_all(bytes) {
        match item.map_err(|e| e.to_string())? {
            Payload::ImportSection(section) => {
                for import in section {
                    if matches!(import.map_err(|e| e.to_string())?.ty, TypeRef::Func(_)) {
                        functions = functions
                            .checked_add(1)
                            .ok_or("wasm function count overflow")?;
                    }
                }
            }
            Payload::FunctionSection(section) => {
                functions = functions
                    .checked_add(section.count())
                    .ok_or("wasm function count overflow")?;
            }
            Payload::CodeSectionEntry(body) => {
                let mut ops = body.get_operators_reader().map_err(|e| e.to_string())?;
                while !ops.eof() {
                    needed |= operation(&ops.read().map_err(|e| e.to_string())?).is_some();
                }
            }
            _ => {}
        }
    }
    if !needed {
        return Ok(bytes.to_vec());
    }
    functions
        .checked_add(26)
        .ok_or("wasm function count overflow")?;
    let mut output = Module::new();
    FloatOps {
        functions,
        types: 0,
    }
    .parse_core_module(&mut output, Parser::new(0), bytes)
    .map_err(|e| format!("wasm float translation failed: {e}"))?;
    Ok(output.finish())
}