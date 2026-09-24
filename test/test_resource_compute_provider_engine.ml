(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Config = Octra_node_runtime.Resource_compute_provider_config
module Engine = Octra_node_runtime.Resource_compute_provider_engine
module Rpc = Octra_node_runtime.Resource_compute_provider_rpc

let fail message =
  failwith ("test_resource_compute_provider_engine: " ^ message)

let hash char =
  String.make 64 char

let address =
  "oct1111111111111111111111111111111111111111BURN"

let program_raw =
  "wasm"

let program_root =
  Digestif.SHA256.(to_hex (digest_string program_raw))

let request = Rpc.{
  model_epoch = 7L;
  model_state_root = hash '1';
  circle_id = address;
  graph_root = hash '2';
  model_root = hash '3';
  program_root;
  executor_root = hash '4';
}

let model = Engine.{
  snapshots = [request.model_epoch, request.model_state_root];
  circle_id = request.circle_id;
  graph_root = request.graph_root;
  model_root = request.model_root;
  program_root = request.program_root;
  executor_root = request.executor_root;
  cache_key = hash '5';
  entries = 10;
  bytes = 20L;
}

let execute_request = Rpc.{
  request_id = hash '6';
  session_id = hash 'b';
  epoch_id = 8L;
  state_root = hash '7';
  model_epoch = request.model_epoch;
  model_state_root = request.model_state_root;
  circle_id = request.circle_id;
  graph_root = request.graph_root;
  model_root = request.model_root;
  program_root = request.program_root;
  executor_root = request.executor_root;
  caller = address;
  method_name = "complete";
  params_json = "[\"15496\",1]";
  program_b64 = Base64.encode_exn program_raw;
  program_runtime = "wasm_v1";
  storage = [];
  max_output_bytes = 1024;
}

let limits = Config.{
  accelerator = Cpu;
  lanes = 1;
  memory_bytes = 4_294_967_296L;
  max_request_bytes = 65_536;
  max_response_bytes = 262_144;
  timeout_seconds = 300;
}

let deps dropped = Engine.{
  program = (fun ~epoch_id:_ ~state_root:_ ~circle_id ->
    Lwt.return
      (Ok Engine.{
         circle_id;
         code_b64 = Base64.encode_exn program_raw;
         code_hash = program_root;
         runtime = "wasm_v1";
       }));
  storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ -> Lwt.return (Ok []));
  load_model = (fun request ->
    Lwt.return
      (Ok Engine.{
         model with
         snapshots = [request.Rpc.model_epoch, request.model_state_root];
       }));
  drop_model = (fun value ->
    dropped := value.cache_key :: !dropped;
    Lwt.return (Ok ()));
  self_test = (fun () ->
    Lwt.return
      (Ok Rpc.{
         executor_root = request.executor_root;
         evidence_root = hash '8';
         profile = "q24-v1";
       }));
  execute = (fun ~model:_ ~program:_ ~storage:_ _ ->
    Lwt.return
      (Ok Engine.{
         output_json = "\"198\"";
         trace_root = hash '9';
         steps = 100L;
         operations = 20L;
         effort_used = 300;
       }));
}

let prepare_engine (_request : Rpc.prepare) =
  let dropped = ref [] in
  Engine.create ~limits ~deps:(deps dropped), dropped

let test_prepare_and_execute () =
  let engine, _ = prepare_engine request in
  begin
    match Lwt_main.run (Engine.prepare engine request) with
    | Ok prepared when prepared.Rpc.cache_key = model.cache_key -> ()
    | Ok _ -> fail "prepared model changed"
    | Error message -> fail message
  end;
  begin
    match Lwt_main.run (Engine.execute engine execute_request) with
    | Ok executed when executed.Rpc.output_json = "\"198\"" -> ()
    | Ok _ -> fail "execution output changed"
    | Error message -> fail message
  end

let test_unprepared () =
  let engine, _ = prepare_engine request in
  match Lwt_main.run (Engine.execute engine execute_request) with
  | Error "resource compute model not prepared" -> ()
  | _ -> fail "unprepared execution accepted"

let test_wrong_executor () =
  let engine, _ = prepare_engine request in
  let wrong = { request with Rpc.executor_root = hash 'a' } in
  match Lwt_main.run (Engine.prepare engine wrong) with
  | Error "resource compute executor root differs" -> ()
  | _ -> fail "wrong executor accepted"

let test_snapshot_alias () =
  let engine, _ = prepare_engine request in
  let next = Rpc.{
    request with
    model_epoch = 9L;
    model_state_root = hash 'a';
  } in
  begin
    match Lwt_main.run (Engine.prepare engine request) with
    | Error message -> fail message
    | Ok _ -> ()
  end;
  begin
    match Lwt_main.run (Engine.prepare engine next) with
    | Error message -> fail message
    | Ok prepared when prepared.cache_key = model.cache_key -> ()
    | Ok _ -> fail "snapshot alias cache changed"
  end;
  let first_execute = execute_request in
  let next_execute = Rpc.{
    execute_request with
    model_epoch = next.model_epoch;
    model_state_root = next.model_state_root;
  } in
  begin
    match Lwt_main.run (Engine.execute engine first_execute) with
    | Ok _ -> ()
    | Error message -> fail message
  end;
  match Lwt_main.run (Engine.execute engine next_execute) with
  | Ok _ -> ()
  | Error message -> fail message

let test_native_accelerator () =
  let module Config = Octra_node_runtime.Resource_compute_provider_config in
  if Engine.native_accelerator Config.Cpu <> Ok () then
    fail "startup rejected CPU execution";
  List.iter (fun accelerator ->
    let result = Lwt_main.run (Engine.native_self_test ~accelerator ()) in
    let expected = "resource compute accelerator unavailable: " ^
      Config.accelerator_name accelerator in
    if Engine.native_accelerator accelerator <> Error expected then
      fail "startup did not report the unavailable accelerator";
    match result with
    | Error reason when reason = expected -> ()
    | _ -> fail "CPU evidence accepted for an unavailable accelerator")
    Config.[Cuda; Metal; Rocm]

let () =
  test_native_accelerator ();
  test_prepare_and_execute ();
  test_unprepared ();
  test_wrong_executor ();
  test_snapshot_alias ();
  print_endline "test_resource_compute_provider_engine: ok"