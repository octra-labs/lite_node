// SPDX-License-Identifier: BSD-3-Clause
// Copyright (c) 2023-2026 Octra Labs <dev@octra.org>

use super::*;
use wasmi::{TypedResumableCall, TypedResumableInvocation, Val};

#[derive(Debug)]
pub(crate) struct Request {
    bytes: Vec<u8>,
    output: Option<(i32, i32)>,
}

pub(crate) fn is_request(bytes: &[u8]) -> bool {
    parse_request_frame(bytes).map(|request| matches!(request.method.as_str(),
        "oct_transfer" | "program_call" | "call_value" | "oct_balance"
    )).unwrap_or(false)
}

fn storage_cost(storage: &BTreeMap<String, Vec<u8>>) -> Result<u64, String> {
    storage.iter().try_fold(0_u64, |cost, (key, value)| {
        let bytes = (key.len() as u64).checked_add(value.len() as u64)
            .ok_or_else(|| "storage bytes overflow".to_owned())?;
        cost.checked_add(32 + bytes.div_ceil(16))
            .ok_or_else(|| "storage effort overflow".to_owned())
    })
}

fn storage_json(storage: &BTreeMap<String, Vec<u8>>) -> JsonValue {
    JsonValue::Array(storage.iter().map(|(key, value)| json!({
        "key_b64": encode_b64(key.as_bytes()),
        "value_b64": encode_b64(value)
    })).collect())
}

pub(crate) fn suspend(
    caller: &mut Caller<'_, HostState>, bytes: Vec<u8>, output: Option<(i32, i32)>,
) -> Result<i32, WasmiError> {
    if caller.data().is_view || caller.data().call_request.is_some() {
        return Err(WasmiError::new("circle call context invalid"));
    }
    let cost = storage_cost(&caller.data().storage).map_err(WasmiError::new)?;
    charge_host_fuel(caller, cost)?;
    caller.data_mut().call_request = Some(Request {bytes, output});
    Err(WasmiError::new("circle call requested"))
}

struct Session {
    runtime: Runtime,
    paused: Option<TypedResumableInvocation<i32>>,
    serial: u64,
}

enum Phase {
    New,
    Paused(Session),
    Closed,
}

pub(crate) struct Port(Mutex<Phase>);

impl Session {
    fn start(payload: &Payload) -> Result<(Self, Result<TypedResumableCall<i32>, WasmiError>), HostFailure> {
        if payload.action.as_deref() != Some("execute")
            || payload.export_name.as_deref() != Some("octra_update")
            || payload.is_view != Some(false) || payload.update_policy != Some(true)
        {
            return Err(HostFailure::Rejected("circle call policy required".to_owned()));
        }
        let mut runtime = Runtime::prepare(payload, false, true)?;
        let request = decode_b64("request_b64", payload.request_b64.as_deref()
            .ok_or_else(|| "circle request missing".to_owned())?)?;
        let alloc = runtime.instance.get_typed_func::<i32, i32>(&runtime.store, "octra_alloc")
            .map_err(|_| "circle allocator missing".to_owned())?;
        let method = runtime.instance.get_typed_func::<(i32, i32), i32>(&runtime.store, "octra_update")
            .map_err(|_| "circle method missing".to_owned())?;
        let allocated = alloc.call(&mut runtime.store, request.len() as i32);
        if let Some(reason) = runtime.store.data().unavailable.as_ref() {
            return Err(HostFailure::Unavailable(reason.clone()));
        }
        let pointer = allocated.map_err(|_| "circle allocation failed".to_owned())?;
        let memory = runtime.instance.get_memory(&runtime.store, "memory")
            .ok_or_else(|| "circle memory missing".to_owned())?;
        if pointer < 0 {
            return Err(HostFailure::Rejected("circle allocation invalid".to_owned()));
        }
        memory.write(&mut runtime.store, pointer as usize, &request)
            .map_err(|_| "circle allocation invalid".to_owned())?;
        runtime.store.data_mut().calls = true;
        let result = method.call_resumable(&mut runtime.store, (pointer, request.len() as i32));
        Ok((Self {runtime, paused: None, serial: 0}, result))
    }

    fn finish(&mut self, code: i32, error: Option<String>) -> Result<(i32, JsonValue), String> {
        let state = self.runtime.store.data();
        if state.hfhe_receipt_mode == "consume"
            && state.hfhe_receipt_index != state.hfhe_receipt_expected.len()
        {
            return Err("hfhe receipt was not fully consumed".to_owned());
        }
        let fuel = self.runtime.store.get_fuel().map_err(|error| error.to_string())?;
        Ok((0, execute_output(&self.runtime, code, code == 0,
            &state.response_bytes, self.runtime.fuel_limit - fuel, error)))
    }

    fn advance(&mut self, result: Result<TypedResumableCall<i32>, WasmiError>)
        -> Result<(i32, JsonValue), String>
    {
        match result {
            Ok(TypedResumableCall::Finished(code)) => {
                self.finish(code, if code == 0 {None} else {Some(format!("wasm export returned {code}"))})
            }
            Ok(TypedResumableCall::Resumable(invocation)) => {
                let Some(request) = self.runtime.store.data().call_request.as_ref() else {
                    return self.finish(-1, Some(invocation.host_error().to_string()));
                };
                self.serial = self.serial.checked_add(1)
                    .ok_or_else(|| "circle call count overflow".to_owned())?;
                let fuel = self.runtime.store.get_fuel().map_err(|error| error.to_string())?;
                let frame = parse_request_frame(&request.bytes)?;
                let params: Vec<_> = frame.params.into_iter().map(|value| match value {
                    FrameValue::Null => json!({"tag": "null"}),
                    FrameValue::Bool(value) => json!({"tag": "bool", "value": value}),
                    FrameValue::Int(value) => json!({"tag": "int", "value": value}),
                    FrameValue::String(value) => json!({"tag": "string", "value": value}),
                }).collect();
                let query = json!({
                    "id": self.serial,
                    "method": frame.method,
                    "params": params,
                    "fuel": fuel,
                    "storage_pairs": storage_json(&self.runtime.store.data().storage),
                    "events": self.runtime.store.data().events.len()
                });
                self.paused = Some(invocation);
                Ok((3, query))
            }
            Err(error) => self.finish(-1, Some(error.to_string())),
        }
    }

    fn resume(&mut self, reply: JsonValue) -> Result<(i32, JsonValue), String> {
        if reply.get("id").and_then(JsonValue::as_u64) != Some(self.serial) {
            return Err("circle reply id mismatch".to_owned());
        }
        if let Some(error) = reply.get("error").and_then(JsonValue::as_str) {
            return self.finish(-1, Some(error.to_owned()));
        }
        let response = decode_b64("response_b64", reply.get("response_b64")
            .and_then(JsonValue::as_str).ok_or_else(|| "circle response missing".to_owned())?)?;
        if response.len() > MAX_RESPONSE_BYTES {
            return self.finish(-1, Some("circle response exceeds limit".to_owned()));
        }
        let pairs = reply.get("storage_pairs").and_then(JsonValue::as_array)
            .ok_or_else(|| "circle storage missing".to_owned())?;
        let mut storage = BTreeMap::new();
        for entry in pairs {
            let key = decode_b64("key_b64", entry.get("key_b64").and_then(JsonValue::as_str)
                .ok_or_else(|| "circle key missing".to_owned())?)?;
            let key = String::from_utf8(key).map_err(|_| "circle key invalid".to_owned())?;
            let value = decode_b64("value_b64", entry.get("value_b64").and_then(JsonValue::as_str)
                .ok_or_else(|| "circle value missing".to_owned())?)?;
            if storage.insert(key, value).is_some() {
                return Err("circle key repeated".to_owned());
            }
        }
        let cost = reply.get("effort").and_then(JsonValue::as_u64)
            .and_then(|effort| effort.checked_add(storage_cost(&storage).ok()?))
            .ok_or_else(|| "circle effort invalid".to_owned())?;
        let fuel = self.runtime.store.get_fuel().map_err(|error| error.to_string())?;
        if cost > fuel {
            self.runtime.store.set_fuel(0).map_err(|error| error.to_string())?;
            return self.finish(-1, Some("circle effort exhausted".to_owned()));
        }
        self.runtime.store.set_fuel(fuel - cost).map_err(|error| error.to_string())?;
        let request = self.runtime.store.data_mut().call_request.take()
            .ok_or_else(|| "circle request missing".to_owned())?;
        self.runtime.store.data_mut().storage = Arc::new(storage);
        let result = match request.output {
            None => {
                let size = response.len() as i32;
                self.runtime.store.data_mut().circle_invoke_cache = Some((request.bytes, response));
                size
            }
            Some((pointer, capacity)) => {
                if capacity < 0 || (capacity as usize) < response.len() {
                    self.runtime.store.data_mut().circle_invoke_cache = Some((request.bytes, response));
                    -2
                } else {
                    let memory = self.runtime.instance.get_memory(&self.runtime.store, "memory")
                        .ok_or_else(|| "circle memory missing".to_owned())?;
                    if pointer < 0 || memory.write(&mut self.runtime.store, pointer as usize, &response).is_err() {
                        return self.finish(-1, Some("circle output pointer invalid".to_owned()));
                    }
                    self.runtime.store.data_mut().circle_invoke_cache = None;
                    response.len() as i32
                }
            }
        };
        let invocation = self.paused.take().ok_or_else(|| "circle session not paused".to_owned())?;
        let next = invocation.resume(&mut self.runtime.store, &[Val::I32(result)]);
        self.advance(next)
    }
}

impl Port {
    fn step(&self, input: &[u8]) -> Result<(i32, String), (i32, String)> {
        let mut phase = self.0.try_lock().map_err(|_| (2, "circle session busy".to_owned()))?;
        let previous = std::mem::replace(&mut *phase, Phase::Closed);
        let (mut session, output) = match previous {
            Phase::New => {
                let payload = serde_json::from_slice::<Payload>(input)
                    .map_err(|error| (1, error.to_string()))?;
                let (mut session, result) = Session::start(&payload).map_err(|error| match error {
                    HostFailure::Rejected(reason) => (1, reason),
                    HostFailure::Unavailable(reason) => (2, reason),
                })?;
                let output = session.advance(result).map_err(|error| (2, error))?;
                (session, output)
            }
            Phase::Paused(mut session) => {
                let reply = serde_json::from_slice::<JsonValue>(input)
                    .map_err(|error| (2, error.to_string()))?;
                let output = session.resume(reply).map_err(|error| (2, error))?;
                (session, output)
            }
            Phase::Closed => return Err((2, "circle session closed".to_owned())),
        };
        let body = serde_json::to_string(&output.1).map_err(|error| (2, error.to_string()))?;
        if output.0 == 3 {
            *phase = Phase::Paused(session);
        } else {
            session.runtime.store.data_mut().call_request = None;
        }
        Ok((output.0, body))
    }
}

#[no_mangle]
pub extern "C" fn octra_circle_call_new() -> *mut Port {
    Box::into_raw(Box::new(Port(Mutex::new(Phase::New))))
}

#[no_mangle]
pub unsafe extern "C" fn octra_circle_call_drop(port: *mut Port) {
    if !port.is_null() {
        drop(Box::from_raw(port));
    }
}

#[no_mangle]
pub unsafe extern "C" fn octra_circle_call_step(
    port: *mut Port, input: *const u8, length: usize, output: *mut *mut u8, size: *mut usize,
) -> i32 {
    let result = std::panic::catch_unwind(|| {
        if port.is_null() || input.is_null() || length > 64 * 1024 * 1024 {
            return Err((2, "circle session input invalid".to_owned()));
        }
        (&*port).step(slice::from_raw_parts(input, length))
    });
    let (code, body) = match result {
        Ok(Ok(result)) => result,
        Ok(Err(error)) => error,
        Err(_) => (2, "circle session failed".to_owned()),
    };
    write_owned_bytes(body.into_bytes(), output, size);
    code
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_encoder::{CodeSection, ConstExpr, DataSection, EntityType, ExportKind,
        ExportSection, Function, FunctionSection, ImportSection, Instruction as I,
        MemorySection, MemoryType, Module, StartSection, TypeSection, ValType};

    fn payload(ops: &[I<'_>]) -> JsonValue {
        module_input(ops, "host_circle_invoke_len", false)
    }

    fn module_input(ops: &[I<'_>], import: &str, start: bool) -> JsonValue {
        let method = "call_value";
        let mut request = REQUEST_MAGIC.to_vec();
        request.extend((method.len() as u16).to_be_bytes());
        request.extend(method.as_bytes());
        request.extend(0_u16.to_be_bytes());
        module_bytes(ops, import, start, &request, &[])
    }

    fn module_bytes(ops: &[I<'_>], import: &str, start: bool,
        request: &[u8], setup: &[I<'_>]) -> JsonValue
    {
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types.ty().function([ValType::I32, ValType::I32], [ValType::I32]);
        types.ty().function([ValType::I32; 4], [ValType::I32]);
        types.ty().function([ValType::I32], [ValType::I32]);
        types.ty().function([], []);
        module.section(&types);
        let mut imports = ImportSection::new();
        imports.import("octra", import, EntityType::Function(0));
        imports.import("octra", "host_circle_invoke", EntityType::Function(1));
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(2);
        functions.function(0);
        if start {
            functions.function(3);
        }
        module.section(&functions);
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {minimum: 1, maximum: Some(1), memory64: false,
            shared: false, page_size_log2: None});
        module.section(&memory);
        let mut exports = ExportSection::new();
        exports.export("memory", ExportKind::Memory, 0);
        exports.export("octra_alloc", ExportKind::Func, 2);
        exports.export("octra_update", ExportKind::Func, 3);
        module.section(&exports);
        if start {
            module.section(&StartSection {function_index: 4});
        }
        let mut code = CodeSection::new();
        let mut alloc = Function::new([]);
        for op in setup {
            alloc.instruction(op);
        }
        alloc.instruction(&I::I32Const(4096)).instruction(&I::End);
        code.function(&alloc);
        let mut update = Function::new([]);
        for op in ops {
            update.instruction(op);
        }
        update.instruction(&I::I32Const(0)).instruction(&I::End);
        code.function(&update);
        if start {
            let mut init = Function::new([]);
            for op in setup {
                init.instruction(op);
            }
            init.instruction(&I::Unreachable).instruction(&I::End);
            code.function(&init);
        }
        module.section(&code);
        let mut data = DataSection::new();
        data.active(0, &ConstExpr::i32_const(0), request.iter().copied());
        module.section(&data);
        json!({"action": "execute", "code_b64": encode_b64(&module.finish()),
            "export_name": "octra_update", "request_b64": "",
            "storage_pairs": [], "hfhe_strict": false, "is_view": false,
            "update_policy": true, "execution_profile": "standard", "fuel_limit": 2_000_000})
    }

    fn next(port: &Port, input: JsonValue) -> Result<(i32, JsonValue), (i32, String)> {
        port.step(&serde_json::to_vec(&input).unwrap()).map(|(code, body)|
            (code, serde_json::from_str(&body).unwrap()))
    }

    fn reply(query: &JsonValue) -> JsonValue {
        json!({"id": query["id"], "response_b64": encode_b64(&frame_int(42)),
            "effort": 10, "storage_pairs": []})
    }

    #[test]
    fn allocator_fault() {
        let method = "fhe_encrypt_zero";
        let mut request = REQUEST_MAGIC.to_vec();
        request.extend((method.len() as u16).to_be_bytes());
        request.extend(method.as_bytes());
        request.extend(1_u16.to_be_bytes());
        request.push(4);
        request.extend(0_u32.to_be_bytes());
        let setup = [I::I32Const(0), I::I32Const(request.len() as i32), I::Call(0), I::Drop];
        for start in [false, true] {
            let mut input = module_bytes(&[], "host_hfhe_invoke_len", start, &request, &setup);
            input["hfhe_caps"] = json!(["fhe_encrypt"]);
            input["fuel_limit"] = json!(10_000_000);
            input["hfhe_active_key"] = json!({"key_id": "test", "pubkey_b64": "", "seckey_b64": ""});
            let port = Port(Mutex::new(Phase::New));
            assert_eq!(next(&port, input).unwrap_err(), (2, "hfhe backend failed".to_owned()));
        }
    }

    #[test]
    fn update_policy() {
        let port = Port(Mutex::new(Phase::New));
        let input = module_input(&[], "host_session_get_len", true);
        let error = next(&port, input).unwrap_err();
        assert_eq!(error, (1, "circle update imports invalid".to_owned()));
        let port = Port(Mutex::new(Phase::New));
        let input = module_input(&[], "host_circle_invoke_len", true);
        assert!(next(&port, input).unwrap_err().1.starts_with("wasm start failed"));
    }

    #[test]
    fn resume_once() {
        let mut ops = Vec::new();
        for _ in 0..2 {
            ops.extend([I::I32Const(0), I::I32Const(19), I::Call(0), I::Drop]);
        }
        for capacity in [0, 128] {
            ops.extend([I::I32Const(0), I::I32Const(19), I::I32Const(2048),
                I::I32Const(capacity), I::Call(1), I::Drop]);
        }
        ops.extend([I::I32Const(0), I::I32Const(19), I::Call(0), I::Drop]);
        let port = Port(Mutex::new(Phase::New));
        let (code, first) = next(&port, payload(&ops)).unwrap();
        assert_eq!(code, 3);
        let (code, second) = next(&port, reply(&first)).unwrap();
        assert_eq!(code, 3);
        assert_eq!(second["id"], 2);
        let (code, result) = next(&port, reply(&second)).unwrap();
        assert_eq!(code, 0);
        assert_eq!(result["success"], true);
        assert!(next(&port, reply(&second)).is_err());
    }

    #[test]
    fn response_limit() {
        let ops = [I::I32Const(0), I::I32Const(19), I::Call(0), I::Drop];
        for length in [MAX_RESPONSE_BYTES - 1, MAX_RESPONSE_BYTES, MAX_RESPONSE_BYTES + 1] {
            let port = Port(Mutex::new(Phase::New));
            let (_, query) = next(&port, payload(&ops)).unwrap();
            let mut answer = reply(&query);
            answer["response_b64"] = json!(encode_b64(&frame_string("f".repeat(length - 10))));
            let (code, result) = next(&port, answer).unwrap();
            assert_eq!(code, 0);
            assert_eq!(result["success"], length <= MAX_RESPONSE_BYTES);
            assert!(result["unavailable"].is_null());
            if length > MAX_RESPONSE_BYTES {
                assert_eq!(result["error"], "circle response exceeds limit");
            }
            assert!(next(&port, reply(&query)).is_err());
        }
    }

    #[test]
    fn reply_checks() {
        let ops = [I::I32Const(0), I::I32Const(19), I::Call(0), I::Drop];
        for invalid in ["id", "effort", "storage_pairs"] {
            let port = Port(Mutex::new(Phase::New));
            let (_, query) = next(&port, payload(&ops)).unwrap();
            let mut answer = reply(&query);
            answer[invalid] = json!(-1);
            assert!(next(&port, answer).is_err());
            assert!(next(&port, reply(&query)).is_err());
        }
        let port = Port(Mutex::new(Phase::New));
        let (_, query) = next(&port, payload(&ops)).unwrap();
        let mut answer = reply(&query);
        answer["effort"] = json!(2_000_000);
        let (code, result) = next(&port, answer).unwrap();
        assert_eq!(code, 0);
        assert_eq!(result["success"], false);
        assert_eq!(result["effort_used"], 2_000_000);
        let port = Port(Mutex::new(Phase::New));
        let mut input = payload(&ops);
        input["update_policy"] = json!(false);
        assert_eq!(next(&port, input).unwrap_err().0, 1);
    }
}