(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Config = Octra_node_runtime.Resource_compute_provider_config
module Engine = Octra_node_runtime.Resource_compute_provider_engine
module Rpc = Octra_node_runtime.Resource_compute_provider_rpc
module Store = Octra_core.Store_irmin
module Circle = Octra_core.Circles
module Cache = Octra_node_runtime.Resource_compute_model_cache
module Graph = Octra_core.Resource_compute_graph
module Owner = Cache.Owner

let fail message =
  failwith ("test_compute_engine: " ^ message)

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
  cache_key = Octra_core.Resource_compute_graph.cache_key
    ~circle_id:request.circle_id ~graph_root:request.graph_root;
  cache_root = hash '5';
  entries = 10;
  bytes = 20L;
}

let execute_request = Rpc.{
  memory_bytes = 1_073_741_824;
  guest_bytes = 67_108_864;
  fuel_limit = 2_000_000_000;
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
  load_model = (fun ~cancel:_ request ->
    Lwt.return
      (Ok Engine.{
         model with
         snapshots = [request.Rpc.model_epoch, request.model_state_root];
         circle_id = request.circle_id;
         graph_root = request.graph_root;
         model_root = request.model_root;
         program_root = request.program_root;
         executor_root = request.executor_root;
         cache_key = Graph.cache_key ~circle_id:request.circle_id ~graph_root:request.graph_root;
       }));
  drop_model = (fun value ->
    dropped := value :: !dropped;
    Lwt.return (Ok ()));
  hold_model = (fun _ -> Ok (Octra_core.Circle_wasm_native.lease_new ()));
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
         steps = 300L;
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

let test_fuel_limit () =
  let json = Rpc.execute_to_json execute_request in
  if Rpc.execute_of_json json <> Ok execute_request then fail "fuel wire changed";
  let fields = match json with `Assoc fields -> fields | _ -> assert false in
  List.iter (fun json ->
    if Result.is_ok (Rpc.execute_of_json json) then fail "invalid fuel wire accepted")
    (`Assoc (List.remove_assoc "fuel_limit" fields) :: List.map (fun fuel ->
      `Assoc (("fuel_limit", fuel) :: List.remove_assoc "fuel_limit" fields))
      [`Int 0; `Int (-1); `Int 2_000_000_001; `String "10"]);
  let calls = ref 0 in
  let reported = ref None in
  let base = deps (ref []) in
  let deps = Engine.{base with execute = (fun ~model ~program ~storage request ->
    incr calls;
    let open Lwt.Syntax in
    let+ result = base.execute ~model ~program ~storage request in
    Result.map (fun value -> match !reported with
      | None -> value | Some steps -> {value with Engine.steps}) result)} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* prepared = Engine.prepare engine request in
    if Result.is_error prepared then fail "fuel model prepare";
    let* () = Lwt_list.iter_s (fun fuel_limit ->
      let+ result = Engine.execute engine {execute_request with fuel_limit} in
      match result with
      | Error "resource compute fuel refused" -> ()
      | _ -> fail "invalid fuel accepted") [0; -1; 2_000_000_001] in
    if !calls <> 0 then fail "invalid fuel executed";
    let* result = Engine.execute engine {execute_request with fuel_limit = 299} in
    begin match result with
    | Error "resource compute fuel exceeds requested limit" -> ()
    | _ -> fail "overreported fuel accepted"
    end;
    let* result = Engine.execute engine {execute_request with fuel_limit = 300; request_id = hash 'e'} in
    if Result.is_error result || !calls <> 2 then fail "exact fuel refused";
    Lwt_list.iter_s (fun steps ->
      reported := Some steps;
      let request_id = Digestif.SHA256.(to_hex (digest_string (Int64.to_string steps))) in
      let+ result = Engine.execute engine {execute_request with request_id} in
      match result with
      | Error "resource compute fuel report differs" -> ()
      | Error reason -> fail ("fuel report check blocked: " ^ reason)
      | Ok _ -> fail "inconsistent fuel report accepted") [299L; 0L; 301L; Int64.max_int])
    (fun () -> Engine.stop engine))

let test_native_fuel () =
  let byte value = String.make 1 (Char.chr value) in
  let rec leb value =
    if value < 128 then byte value
    else byte ((value land 127) lor 128) ^ leb (value lsr 7) in
  let text value = leb (String.length value) ^ value in
  let section tag value = byte tag ^ text value in
  let code ?(exit_code = 0) pages = "\000asm\001\000\000\000"
    ^ section 1 "\002\096\002\127\127\001\127\096\001\127\001\127"
    ^ section 2 ("\002" ^ text "octra" ^ text "host_response_write" ^ "\000\000"
      ^ text "octra" ^ text "host_tensor_silu_q24" ^ "\000\000")
    ^ section 3 "\002\001\000"
    ^ section 5 ("\001\000" ^ leb pages)
    ^ section 7 ("\003" ^ text "memory" ^ "\002\000"
      ^ text "octra_alloc" ^ "\000\002" ^ text "octra_query" ^ "\000\003")
    ^ section 10 ("\002" ^ text "\000\065\000\011"
      ^ text ("\001\001\127\065\128\004\065\001\016\001\026"
        ^ "\065\128\004\033\002\003\064"
        ^ "\032\002\065\001\107\034\002\013\000\011"
        ^ "\065\128\002\065\010\016\000\026\065" ^ byte exit_code ^ "\011"))
    ^ section 11 ("\001\000\065\128\002\011" ^ text "OCWS1\000\000\000\000\000") in
  Test_workspace.with_dir "compute-fuel" (fun directory ->
    let store = Lwt_main.run (Store.open_store ~fresh:true directory) in
    let deps = Engine.native_deps ~limits ~store
      ~state_root_at:(fun _ -> None) ~index_root_at:(fun _ -> None) in
    let root =
      let buffer = Buffer.create 128 in
      Buffer.add_string buffer "octra:compute_storage_cache:v1\000";
      Buffer.add_int32_be buffer 7l;
      Buffer.add_string buffer "weights";
      Buffer.add_int64_be buffer 1L;
      Buffer.add_string buffer Digestif.SHA256.(to_raw_string (digest_string "x"));
      Digestif.SHA256.(to_hex (digest_string (Buffer.contents buffer))) in
    let open Lwt.Syntax in
    Lwt_main.run (Lwt.finalize (fun () ->
      let model = {model with cache_root = root; entries = 1; bytes = 1L} in
      let* opened = Cache.native_sink.begin_cache model.cache_key in
      let writer = match opened with Ok value -> value | Error reason -> fail reason in
      let* () = Lwt.finalize (fun () ->
        let* written = writer.Cache.put ~key:"weights" ~value:"x" in
        let* sealed = writer.seal ~root ~entries:1 ~bytes:1 in
        if written <> Ok () || sealed <> Ok () then fail "fuel model setup";
        Lwt.return_unit) (fun () -> writer.close (); Lwt.return_unit) in
      let run ?(pages = 1) ?(memory_bytes = 1_073_741_824) ?(guest_bytes = 67_108_864) fuel_limit =
        let code = code pages in
        let program = Engine.{circle_id = address; code_b64 = Base64.encode_exn code;
          code_hash = Digestif.SHA256.(to_hex (digest_string code)); runtime = "wasm_v1"} in
        deps.execute ~model ~program ~storage:[]
          {execute_request with fuel_limit; guest_bytes; memory_bytes} in
      let* result = run 100_000 in
      let spent = match result with
      | Ok value when value.output_json = "null" && value.effort_used > 32
          && value.effort_used <= 100_000 && value.operations = 1L
          && value.steps = Int64.of_int value.effort_used -> value.effort_used
      | Ok _ -> fail "native fuel result differs"
      | Error reason -> fail reason
      in
      let* exact = run spent in
      if exact <> result then fail "native exact fuel differs";
      let module Bill = Octra_core.Compute_bill in
      let module Protocol = Octra_consensus.Resource_compute_protocol in
      let module Certificate = Octra_consensus.Resource_compute_certificate in
      let module Ledger = Octra_core.Ledger in
      let module Funding = Octra_core.Compute_funding in
      let module Ed = Mirage_crypto_ec.Ed25519 in
      let get = function Ok value -> value | Error reason -> fail reason in
      let key char = match Ed.priv_of_octets (String.make 32 char) with
        | Ok value -> value | Error _ -> fail "payment key" in
      let public key = Ed.pub_to_octets (Ed.pub_of_priv key) in
      let addr key = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn (public key)) in
      let payer = key 'p' and worker = key 'w' in
      let validators = Octra_consensus.C_types.make_validator_set
        [Octra_consensus.C_types.{address = addr worker; pubkey = public worker}] in
      let chain_id = "compute-native-payment" in
      let raw hex = Digestif.SHA256.(to_raw_string (of_hex hex)) in
      let ledger = Ledger.create_from_store store in
      Ledger.add_account ledger (addr payer) (Z.of_int 1_000_000) |> get;
      let* () = Ledger.flush_dirty_lwt ledger in
      let* () = Store.begin_epoch_batch store in
      Ledger.begin_journal ledger |> get;
      let* () = Lwt_list.iter_s (fun (nonce, exit_code, fuel_limit) ->
        let code = code ~exit_code 1 in
        let program = Engine.{circle_id = address; code_b64 = Base64.encode_exn code;
          code_hash = Digestif.SHA256.(to_hex (digest_string code)); runtime = "wasm_v1"} in
        let model = {model with program_root = program.code_hash} in
        let request = {execute_request with caller = addr payer; program_root = program.code_hash;
          memory_bytes = Int64.to_int Bill.model_limit; guest_bytes = 65536; fuel_limit} in
        let terms = Certificate.{unit_price = Some 2L; fee_cap = Some 200_000L;
          memory_bytes = Some request.memory_bytes; guest_bytes = Some request.guest_bytes;
          fuel_limit = Some request.fuel_limit; chain_id; epoch_id = request.epoch_id;
          expires_epoch = 50L; validator_set_root = Octra_consensus.C_config.validator_set_hash validators;
          selection_root = String.make 32 's'; circle_id = request.circle_id;
          state_root = raw request.state_root; model_epoch = request.model_epoch;
          model_state_root = raw request.model_state_root; graph_root = raw request.graph_root;
          model_root = raw request.model_root; program_root = raw request.program_root;
          executor_root = raw request.executor_root; caller = request.caller;
          session_id = raw request.session_id; sequence = Int64.of_int nonce;
          input_hash = Protocol.input_hash ~method_name:request.method_name ~params_json:request.params_json;
          input_bytes = String.length request.params_json; max_output_bytes = request.max_output_bytes} in
        let call = Protocol.{request = terms; valid_until = Some 50L;
          method_name = request.method_name; params_json = request.params_json;
          caller_public_key = public payer; caller_signature = String.make 64 '\000';
          coordinator = addr worker; signature = String.make 64 '\000'} in
        let call = {call with caller_signature = Ed.sign ~key:payer (Protocol.intent_sign_bytes call)} in
        let call = {call with signature = Ed.sign ~key:worker (Protocol.call_sign_bytes call)} in
        let header = Protocol.header call |> get in
        let request = {request with request_id = Digestif.SHA256.(to_hex (of_raw_string (Certificate.request_id terms)))} in
        let permission = Funding.prepare ~call ~target:Funding.escrow ~nonce:(Int64.of_int nonce) |> get in
        let permission = Funding.authorize permission ~signature:(Ed.sign ~key:payer (Funding.sign_bytes permission)) |> get in
        let* held = Funding.hold ~ledger ~chain_id ~epoch:8L ~fee:Z.zero ~call permission in
        ignore (get held);
        let* first = Engine.measure ~model ~program ~storage:[] ~call request in
        let first = get first in
        let* delivered = deps.execute ~model ~program ~storage:[] request in
        let delivered = get delivered in
        if Protocol.output_hash delivered.output_json <> first.output_hash
            || raw delivered.trace_root <> first.trace_root || delivered.steps <> first.steps
            || delivered.operations <> first.operations then fail "native transport differs from measured work";
        let* repeated = Engine.measure ~model ~program ~storage:[] ~call request in
        let repeated = get repeated in
        if first <> repeated then fail "native payment repeat differs";
        let replay_call sequence =
          let call = {call with Protocol.request = {call.request with sequence}} in
          let call = {call with caller_signature = Ed.sign ~key:payer (Protocol.intent_sign_bytes call)} in
          let call = {call with signature = Ed.sign ~key:worker (Protocol.call_sign_bytes call)} in
          let request = {request with Rpc.request_id = Digestif.SHA256.(to_hex (of_raw_string
            (Certificate.request_id call.request)))} in
          call, request in
        let first_call = replay_call 1L in
        let get_replay = function
          | Ok value -> value
          | Error reason -> fail (Octra_core.Circle_wasm_host.error_message reason) in
        let replay_call, replay_request = first_call in
        let* replay_base = Engine.measure ~model ~program ~storage:[]
          ~call:replay_call replay_request in
        let* replayed = Engine.replay ~model ~program [first_call] in
        let replayed = get_replay replayed in
        if replayed <> [get replay_base] then fail "native replay measurement differs";
        let* replay_repeat = Engine.replay ~model ~program [first_call] in
        if replay_repeat <> Ok replayed then fail "native replay repeated output differs";
        let second_call =
          let call = {call with Protocol.request = {call.request with sequence = 2L}} in
          let call = {call with caller_signature = Ed.sign ~key:payer (Protocol.intent_sign_bytes call)} in
          let call = {call with signature = Ed.sign ~key:worker (Protocol.call_sign_bytes call)} in
          call, {request with Rpc.request_id = Digestif.SHA256.(to_hex (of_raw_string
            (Certificate.request_id call.request)))} in
        let* wrong = Engine.replay ~model ~program [second_call] in
        if wrong <> Error (Octra_core.Circle_wasm_host.Rejected "compute replay sequence differs") then
          fail "native replay missing prefix accepted";
        let* unavailable = Engine.replay ~model:{model with cache_root = hash 'f'}
          ~program [first_call] in
        (match unavailable with
        | Error (Octra_core.Circle_wasm_host.Unavailable _) -> ()
        | _ -> fail "native replay model failure lost type");
        let* () = if exit_code = 0 && fuel_limit >= spent then begin
          let* pair = Engine.replay ~model ~program [first_call; second_call] in
          if List.length (get_replay pair) <> 2 then fail "native replay lost a result";
          Lwt.return_unit
        end else Lwt.return_unit in
        let expected = if fuel_limit < spent then Bill.Exhausted
          else if exit_code = 0 then Bill.Complete else Bill.Stopped exit_code in
        if first.outcome <> expected then fail "native payment outcome differs";
        let vote = Certificate.{request_id = first.request_id; node_id = addr worker;
          output_hash = first.output_hash; trace_root = first.trace_root; output_bytes = first.output_bytes;
          steps = first.steps; operations = first.operations; signature = String.make 64 '\000'} in
        let sign vote = {vote with Certificate.signature = Ed.sign ~key:worker (Certificate.vote_sign_bytes vote)} in
        let check vote = Bill.verify ~epoch:8L ~header ~validators ~selected:[addr worker]
          ~votes:[sign vote] ~measured:repeated in
        if Result.is_ok (check {vote with steps = Int64.succ vote.steps}) then
          fail "native payment accepted invented work";
        if Result.is_ok (check {vote with trace_root = String.make 32 'x'}) then
          fail "native payment accepted another trace";
        let bill = check vote |> get in
        let* settled = Funding.settle ~ledger ~chain_id ~epoch:8L bill in
        let charged, refund = get settled in
        if not (Z.equal (Z.add charged refund) (Z.of_int 200_000)) then fail "native payment lost funds";
        let* repeated = Funding.settle ~ledger ~chain_id ~epoch:8L bill in
        if Result.is_ok repeated then fail "native payment repeated";
        let* wrong = Engine.measure ~model ~program ~storage:[] ~call {request with fuel_limit = 1} in
        if Result.is_ok wrong then fail "native payment scope bypassed";
        Printf.printf "event = compute_payment exit = %d fuel = %d steps = %Ld charged = %s refund = %s\n%!"
          exit_code fuel_limit first.steps (Z.to_string charged) (Z.to_string refund);
        Lwt.return_unit) [1, 0, 100_000; 2, 7, 100_000; 3, 0, 1000] in
      let* () = Store.commit_epoch_batch store "compute native payment" in
      Ledger.commit_journal ledger |> get;
      if Ledger.get_total_supply ledger <> Z.of_int 1_000_000 then fail "native payment changed supply";
      let* result = run (spent / 2) in
      begin match result with
      | Ok result when result.output_json = "{\"compute_stop\":\"fuel\"}"
          && result.steps <= Int64.of_int (spent / 2) -> ()
      | _ -> fail "native ignored requested fuel"
      end;
      Printf.printf "event = compute_fuel used = %d limit = %d refused = %d\n%!"
        spent spent (spent / 2);
      let* exact = run ~pages:2 ~guest_bytes:131_072 100_000 in
      if Result.is_error exact then fail "native exact guest refused";
      let* refused = run ~pages:2 ~guest_bytes:65_536 100_000 in
      if Result.is_ok refused then fail "native ignored requested pages";
      Printf.printf "event = compute_pages initial = 131072 limit = 65536 status = refused\n%!";
      let* refused = run ~memory_bytes:65_536 100_000 in
      if Result.is_ok refused then fail "native ignored requested memory";
      let* accepted = run ~memory_bytes:131_072 100_000 in
      if Result.is_error accepted then fail "native memory room refused";
      let* maximum = run ~memory_bytes:Octra_consensus.Resource_compute_certificate.max_memory_bytes 100_000 in
      if Result.is_error maximum then fail "native memory maximum refused";
      Printf.printf "event = compute_memory small = 65536 accepted = 131072 status = pass\n%!";
      Lwt.return_unit) (fun () ->
        let* _ = Cache.native_sink.drop model.cache_key in
        Store.close store)))

let test_guest_limit () =
  let json = Rpc.execute_to_json execute_request in
  if Rpc.execute_of_json json <> Ok execute_request then fail "guest wire changed";
  let fields = match json with `Assoc fields -> fields | _ -> assert false in
  List.iter (fun json ->
    if Result.is_ok (Rpc.execute_of_json json) then fail "invalid memory wire accepted")
    (`Assoc (List.remove_assoc "memory_bytes" fields) :: List.map (fun bytes ->
      `Assoc (("memory_bytes", bytes) :: List.remove_assoc "memory_bytes" fields))
      [`Int 0; `Int (-1); `Int 2_147_483_649; `String "65536"]);
  List.iter (fun json ->
    if Result.is_ok (Rpc.execute_of_json json) then fail "invalid guest wire accepted")
    (`Assoc (List.remove_assoc "guest_bytes" fields) :: List.map (fun bytes ->
      `Assoc (("guest_bytes", bytes) :: List.remove_assoc "guest_bytes" fields))
      [`Int 0; `Int 65_535; `Int 65_537; `Int 67_174_400; `String "65536"]);
  let engine = Engine.create ~limits ~deps:(deps (ref [])) in
  Lwt_main.run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = Lwt_list.iter_s (fun memory_bytes ->
      let+ result = Engine.execute engine {execute_request with memory_bytes} in
      match result with
      | Error "resource compute memory refused" -> ()
      | _ -> fail "invalid memory limit accepted") [0; -1; 2_147_483_649] in
    Lwt_list.iter_s (fun guest_bytes ->
      let open Lwt.Syntax in
      let+ result = Engine.execute engine {execute_request with guest_bytes} in
      match result with
      | Error "resource compute pages refused" -> ()
      | _ -> fail "invalid guest limit accepted") [0; 65_535; 65_537; 67_174_400])
    (fun () -> Engine.stop engine))

let test_wrong_executor () =
  let engine, _ = prepare_engine request in
  let wrong = { request with Rpc.executor_root = hash 'a' } in
  match Lwt_main.run (Engine.prepare engine wrong) with
  | Error "resource compute executor root differs" -> ()
  | _ -> fail "wrong executor accepted"

let test_executor_version () =
  let old_roots = [
    "f6ba1e1d5dbc93cf4b05fdc3b4bc1f721de0904baa2cec4810ebef3a2ce7c843";
    "63c163ff7784ea0e7332a7cef5f7f1d19dfb157ad57433f063e41c459926ae2c";
  ] in
  let current = Lwt_main.run (Engine.native_self_test ()) in
  let current = match current with
    | Ok value -> value
    | Error message -> fail message in
  if List.mem current.Rpc.executor_root old_roots then fail "old executor root retained";
  let reads = ref 0 in
  let base = deps (ref []) in
  let deps = Engine.{base with
    self_test = (fun () -> Lwt.return (Ok current));
    program = (fun ~epoch_id ~state_root ~circle_id ->
      incr reads;
      base.program ~epoch_id ~state_root ~circle_id);
  } in
  let engine = Engine.create ~limits ~deps in
  List.iter (fun old_root ->
    let old = {request with Rpc.executor_root = old_root} in
    match Lwt_main.run (Engine.prepare engine old) with
    | Error "resource compute executor root differs" -> ()
    | _ -> fail "old native executor accepted") old_roots;
  if !reads <> 0 then fail "old executor read model"

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

let seed_compute store code value =
  let open Lwt.Syntax in
  let info = Circle.{
    circle_id = address;
    runtime = Wasm_v1;
    version = 1L;
    owner = address;
    code_hash = sha256_hex code;
    stable_root = zero_hash_hex;
    assets_root = zero_hash_hex;
    privacy_class = Public;
    browser_mode = Native_sealed;
    resource_mode = Public_resources;
    policy_hash = None;
    members_root = None;
    export_policy = None;
    limits = default_limits;
  } in
  let* () = Store.begin_epoch_batch store in
  let* () = Store.set_meta store "last_epoch" "7" in
  let* () = Store.deploy_circle store info in
  let* () = Store.save_circle_program_code_b64 store address (Base64.encode_exn code) in
  let values = Hashtbl.create 1 in
  Hashtbl.add values "value" value;
  let* _ = Store.save_circle_stable_storage store address values in
  let path, path_key = Circle.path_key_of_raw_path "/weights/data" |> Result.get_ok in
  let meta = Circle.{
    path_key;
    canonical_path = path;
    content_type = "application/octet-stream";
    encoding = "identity";
    size_bytes = Int64.of_int (String.length value);
    blob_hash = asset_blob_hash value;
    body_mode = Public_resources;
    plaintext_hash = None;
    key_id = None;
    padding_class = None;
    resource_key = path_key;
    locator_mode = Path_locator;
    slot_ref = None;
    activate_after_epoch = None;
    expire_after_epoch = None;
    metadata_mode = Metadata_reveal;
  } in
  let* () = Store.save_circle_asset_meta store address meta in
  let* () = Store.save_circle_asset_body_b64 store address path_key (Base64.encode_exn value) in
  let* () = Store.commit_epoch_batch store "compute seed" in
  Store.tag_epoch store 7

let refused reason result =
  match Lwt_main.run result with
  | Error message when message = reason -> ()
  | Error message -> fail ("unexpected refusal: " ^ message)
  | Ok _ -> fail ("expected refusal: " ^ reason)

let test_snapshot_matches () =
  let actual = hash 'a' in
  let index_root = hash 'b' in
  let root = Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root:actual ~epoch_index_root:index_root in
  List.iter (fun (label, result, expected) ->
    if result <> expected then fail label) [
      "raw root changed", Store.snapshot_matches ~state_root:actual actual, Ok ();
      "raw root alias accepted", Store.snapshot_matches ~state_root:root actual,
        Error "store snapshot root differs";
      "folded root refused", Store.snapshot_matches ~index_root ~state_root:root actual, Ok ();
      "uppercase root refused",
        Store.snapshot_matches ~index_root ~state_root:(String.uppercase_ascii root) actual, Ok ();
      "other tree accepted", Store.snapshot_matches ~index_root ~state_root:root (hash 'c'),
        Error "store snapshot root differs";
      "other index accepted", Store.snapshot_matches ~index_root:(hash 'c') ~state_root:root actual,
        Error "store snapshot root differs";
    ];
  List.iter (fun index_root ->
    if Store.snapshot_matches ~index_root ~state_root:root actual
       <> Error "store snapshot commitment refused" then
      fail "malformed index accepted") [""; hash 'z'; index_root ^ "00"; String.make 32 'b'];
  List.iter (fun state_root ->
    if Store.snapshot_matches ~index_root ~state_root actual
       <> Error "store snapshot commitment refused" then
      fail "malformed root accepted") [""; hash 'z'; root ^ "00"; String.make 32 'a']

let test_native_root () =
  Test_workspace.with_dir "compute-root" (fun directory ->
    let store = Lwt_main.run (Store.open_store ~fresh:true directory) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store)) (fun () ->
      Lwt_main.run (seed_compute store program_raw "first");
      let snapshot = Lwt_main.run (Store.capture_read_snapshot store) |> Result.get_ok in
      let index_root = hash 'd' in
      let state_root = Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:snapshot.state_root ~epoch_index_root:index_root in
      let state_root_at epoch =
        if epoch = 7L then Some (Octra_node_runtime.Text.hex_to_string state_root)
        else None in
      let index_root_at epoch = if epoch = 7L then Some index_root else None in
      let deps = Engine.native_deps ~limits ~store ~state_root_at ~index_root_at in
      begin
        match Lwt_main.run (Store.capture_read_snapshot_at store ~epoch_id:7L
          ~state_root:snapshot.state_root) with
        | Ok saved when saved.state_root = snapshot.state_root -> ()
        | _ -> fail "raw historical root changed"
      end;
      begin
        match Lwt_main.run (deps.program ~epoch_id:7L ~state_root ~circle_id:address) with
        | Ok program when program.code_b64 = Base64.encode_exn program_raw -> ()
        | Ok _ -> fail "committed program changed"
        | Error message -> fail message
      end;
      begin
        match Lwt_main.run (deps.storage ~epoch_id:7L ~state_root ~circle_id:address) with
        | Ok ["value", "first"] -> ()
        | Ok _ -> fail "committed storage changed"
        | Error message -> fail message
      end;
      let source, fetch = Lwt_main.run (Cache.store_source_at
        store ~epoch_id:7L ~state_root ~index_root) |> Result.get_ok in
      let read_asset source = fetch source ~max_bytes:5 ~circle_id:address ~path:"/weights/data" in
      let check_asset () =
        match Lwt_main.run (read_asset source) with
        | Ok asset when asset.body = "first" && asset.state_root = state_root -> ()
        | Ok _ -> fail "model source read another tree"
        | Error message -> fail message in
      check_asset ();
      refused "resource compute snapshot differs"
        (read_asset {source with Cache.state_root = hash 'c'});
      refused "resource compute snapshot differs"
        (read_asset {source with Cache.epoch_id = 8L});
      let model_request = {request with Rpc.model_state_root = state_root} in
      refused "circle asset missing" (deps.load_model ~cancel:(fst (Lwt.wait ())) model_request);
      let check_deps deps reason =
        refused reason (deps.Engine.program ~epoch_id:7L ~state_root ~circle_id:address);
        refused reason (deps.storage ~epoch_id:7L ~state_root ~circle_id:address);
        refused reason (deps.load_model ~cancel:(fst (Lwt.wait ())) model_request) in
      let without_index = Engine.native_deps ~limits ~store ~state_root_at
        ~index_root_at:(fun _ -> None) in
      check_deps without_index "resource compute snapshot index missing";
      let wrong_index = Engine.native_deps ~limits ~store ~state_root_at
        ~index_root_at:(fun _ -> Some (hash 'c')) in
      check_deps wrong_index "store snapshot root differs";
      let bad_index = Engine.native_deps ~limits ~store ~state_root_at
        ~index_root_at:(fun _ -> Some (index_root ^ "00")) in
      check_deps bad_index "store snapshot commitment refused";
      let wrong_header = Engine.native_deps ~limits ~store ~index_root_at
        ~state_root_at:(fun _ -> Some (Octra_node_runtime.Text.hex_to_string (hash 'c'))) in
      check_deps wrong_header "resource compute snapshot root differs";
      refused "store snapshot branch missing"
        (Cache.store_source_at store ~epoch_id:8L ~state_root ~index_root);
      refused "store snapshot epoch refused"
        (Cache.store_source_at store ~epoch_id:(-1L) ~state_root ~index_root);
      Lwt_main.run (seed_compute store "other" "second");
      check_asset ();
      check_deps deps "store snapshot root differs";
      refused "store snapshot root differs"
        (Cache.store_source_at store ~epoch_id:7L ~state_root ~index_root);
      let current = Lwt_main.run (Store.capture_read_snapshot store) |> Result.get_ok in
      let direct, fetch_current = Lwt_main.run (Cache.store_source store) |> Result.get_ok in
      if direct.Cache.state_root <> current.state_root then fail "raw asset root changed";
      begin
        match Lwt_main.run (fetch_current direct ~max_bytes:6
          ~circle_id:address ~path:"/weights/data") with
        | Ok asset when asset.body = "second" && asset.state_root = current.state_root -> ()
        | _ -> fail "raw asset source changed"
      end;
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Store.begin_epoch_batch store in
        let* () = Store.set_meta store "last_epoch" "8" in
        let* () = Store.commit_epoch_batch store "compute epoch" in
        Store.tag_epoch store 7);
      check_deps deps "store snapshot epoch differs"))

let with_storage ?(alter = fun _ -> Lwt.return_unit) values run =
  Test_workspace.with_dir "compute-storage" (fun directory ->
    let store = Lwt_main.run (Store.open_store ~fresh:true directory) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store)) (fun () ->
      Lwt_main.run (seed_compute store program_raw "first");
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Store.begin_epoch_batch store in
        let* _ = Store.save_circle_stable_storage store address values in
        let* () = alter store in
        let* () = Store.commit_epoch_batch store "compute storage" in
        Store.tag_epoch store 7);
      let snapshot = Lwt_main.run (Store.capture_read_snapshot store) |> Result.get_ok in
      let index_root = hash 'd' in
      let state_root = Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:snapshot.state_root ~epoch_index_root:index_root in
      let state_root_at epoch =
        if epoch = 7L then Some (Octra_node_runtime.Text.hex_to_string state_root)
        else None in
      let index_root_at epoch = if epoch = 7L then Some index_root else None in
      let deps = Engine.native_deps ~limits ~store ~state_root_at ~index_root_at in
      run store snapshot deps state_root))

let test_storage_count () =
  let values = Hashtbl.create 4096 in
  for index = 0 to 4095 do
    Hashtbl.add values (string_of_int index) "x"
  done;
  with_storage values (fun _ snapshot deps state_root ->
    Store.Store.Tree.clear snapshot.Store.tree;
    Store.Store.Tree.reset_counters ();
    refused "resource compute storage entries exceed limit"
      (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address);
    let reads = (Store.Store.Tree.counters ()).contents_find in
    Printf.printf "event = compute_read case = entries contents = %d\n%!" reads;
    if reads > 4 then fail "oversized storage loaded values")

let test_storage_bytes () =
  let values = Hashtbl.create 64 in
  for index = 0 to 63 do
    Hashtbl.add values (Printf.sprintf "%02d" index) (String.make 65_534 'x')
  done;
  with_storage values (fun _ snapshot deps state_root ->
    Store.Store.Tree.clear snapshot.Store.tree;
    Store.Store.Tree.reset_counters ();
    refused "resource compute storage exceeds limit"
      (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address);
    let reads = (Store.Store.Tree.counters ()).contents_find in
    Printf.printf "event = compute_read case = bytes contents = %d\n%!" reads;
    if reads > 35 then fail "storage read continued after byte refusal")

let test_storage_exact () =
  let check values =
    with_storage values (fun store snapshot deps state_root ->
      let expected = Hashtbl.fold (fun key value rows -> (key, value) :: rows) values []
        |> List.sort (fun (a, _) (b, _) ->
          String.compare (Circle.stable_key_hash a) (Circle.stable_key_hash b)) in
      let read () = deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address in
      let check () =
        match Lwt_main.run (read ()) with
        | Ok rows when rows = expected -> ()
        | Ok _ -> fail "storage limit changed complete result"
        | Error message -> fail message in
      check ();
      let legacy = Lwt_main.run (Store.load_circle_stable_storage_at snapshot address) in
      if legacy <> Ok expected then fail "storage default result changed";
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Store.begin_epoch_batch store in
        Store.remove_path store ["circles"; address; "stable"; "by_hash"]);
      check ();
      Store.abort_epoch_batch store) in
  check (Hashtbl.create 0);
  let count = Hashtbl.create 2048 in
  for index = 0 to 2047 do
    Hashtbl.add count (string_of_int index) "x"
  done;
  check count;
  let bytes = Hashtbl.create 32 in
  for index = 0 to 31 do
    Hashtbl.add bytes (Printf.sprintf "%02d" index) (String.make 65_534 '\000')
  done;
  check bytes;
  let bytes = Hashtbl.create 1 in
  Hashtbl.add bytes "bytes" (String.init 256 Char.chr);
  check bytes

let test_storage_inputs () =
  let values = Hashtbl.create 2 in
  Hashtbl.add values "abc" "x";
  Hashtbl.add values "def" "y";
  with_storage values (fun _ snapshot _ _ ->
    let read limits = Store.load_circle_stable_storage_at ~limits snapshot address in
    refused "circle storage bytes exceed limit" (read (2, 7));
    refused "circle storage entries exceed limit" (read (1, 8));
    List.iter (fun limits ->
      refused "circle storage read limits refused" (read limits)) [(-1, 8); (2, -1)];
    match Lwt_main.run (read (2, 8)) with
    | Ok rows when List.length rows = 2 -> ()
    | _ -> fail "checked storage exact bytes refused");
  let values = Hashtbl.create 32 in
  for index = 0 to 31 do
    let size = if index = 0 then 65_535 else 65_534 in
    Hashtbl.add values (Printf.sprintf "%02d" index) (String.make size 'x')
  done;
  with_storage values (fun _ _ deps state_root ->
    refused "resource compute storage exceeds limit"
      (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address))

let test_storage_invalid () =
  let key = Circle.stable_key_hash "value" in
  let location = ["circles"; address; "stable"; "by_hash"; key] in
  let cases = [
    "[", "circle stable entry refused";
    "{}", "circle stable entry refused";
    "{\"raw_key\":\"value\",\"key_hash\":\"x\",\"value\":{\"value_kind\":\"blob_ref\",\"value_blob_hash\":\"x\",\"value_size\":\"bad\"}}",
      "circle stable entry refused";
    "{\"raw_key\":\"value\",\"key_hash\":\"x\",\"value\":{\"value_kind\":\"blob_ref\",\"value_blob_hash\":\"x\",\"value_size\":\"1\"}}",
      "circle stable storage contains non-inline values";
  ] in
  List.iter (fun (raw, reason) ->
    let values = Hashtbl.create 1 in
    Hashtbl.add values "value" "first";
    let alter store = Store.write store location raw in
    with_storage ~alter values (fun _ snapshot deps state_root ->
      refused reason (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address);
      let legacy = Lwt_main.run (Store.load_circle_stable_storage_at snapshot address) in
      let expected = if reason = "circle stable entry refused" then Ok [] else Error reason in
      if legacy <> expected then fail "storage default error changed")) cases;
  let values = Hashtbl.create 1 in
  Hashtbl.add values "value" "first";
  let alter store =
    let open Lwt.Syntax in
    let* () = Store.remove_path store location in
    Store.write store (location @ ["nested"]) "x" in
  with_storage ~alter values (fun _ _ deps state_root ->
    refused "circle stable entry missing"
      (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address))

let test_storage_json () =
  let values = Hashtbl.create 1 in
  Hashtbl.add values "value" "first";
  let entry = Circle.make_stable_entry "value" (Circle.Inline "first") in
  let raw = match Circle.yojson_of_stable_entry entry with
    | `Assoc fields -> `Assoc (("extra", `String (String.make 4096 'x')) :: fields)
      |> Yojson.Safe.to_string
    | _ -> fail "stable entry is not an object" in
  let location = ["circles"; address; "stable"; "by_hash"; entry.key_hash] in
  let alter store = Store.write store location raw in
  with_storage ~alter values (fun _ snapshot deps state_root ->
    refused "circle stable entry size refused"
      (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address);
    let legacy = Lwt_main.run (Store.load_circle_stable_storage_at snapshot address) in
    if legacy <> Ok ["value", "first"] then fail "storage default JSON changed")

let test_storage_index () =
  Test_workspace.with_dir "compute-index" (fun directory ->
    let config = Irmin_pack.Conf.init ~fresh:true
      ~indexing_strategy:Irmin_pack.Indexing_strategy.always directory in
    let repo = Lwt_main.run (Store.Store.Repo.v config) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (Store.Store.Repo.close repo)) (fun () ->
      let store = Lwt_main.run (Store.Store.main repo) in
      let run raw_key value expected =
        let entry = Circle.make_stable_entry raw_key (Circle.Inline value) in
        let raw = Circle.yojson_of_stable_entry entry |> Yojson.Safe.to_string in
        let location = ["circles"; address; "stable"; "by_hash"; entry.key_hash] in
        Lwt_main.run (Store.Store.set_exn ~info:(Store.make_info "compute index")
          store location raw);
        let commit = Lwt_main.run (Store.Store.Head.get store) in
        let tree = Store.Store.Commit.tree commit in
        let row = Lwt_main.run (Store.Store.Tree.find_tree tree location) |> Option.get in
        let key, metadata = match Store.Store.Tree.key row with
          | Some (`Contents (key, metadata)) -> key, metadata
          | _ -> fail "indexed storage key missing" in
        let key = Irmin_pack_unix.Pack_key.(v_indexed (to_hash key)) in
        if Irmin_pack_unix.Pack_key.to_length key <> None then
          fail "storage key already has direct length";
        let row = Store.Store.Tree.shallow repo (`Contents (key, metadata)) in
        let tree = Lwt_main.run
          (Store.Store.Tree.add_tree (Store.Store.Tree.empty ()) location row) in
        let snapshot : Store.read_snapshot = {
          read_repo = repo;
          tree;
          state_root = Irmin.Type.to_string Store.Store.Hash.t (Store.Store.Tree.hash tree);
          commit_hash = Irmin.Type.to_string Store.Store.Hash.t (Store.Store.Commit.hash commit);
          epoch_id = 7L;
        } in
        Store.Store.Tree.reset_counters ();
        let result = Lwt_main.run
          (Store.load_circle_stable_storage_at ~limits:(2048, 2 * 1024 * 1024)
            snapshot address) in
        if result <> expected then fail "indexed storage result changed";
        if Irmin_pack_unix.Pack_key.to_length key = None then
          fail "indexed storage size was not resolved";
        let reads = (Store.Store.Tree.counters ()).contents_find in
        if String.length raw_key > 2 * 1024 * 1024 && reads <> 0 then
          fail "indexed large key loaded before refusal";
        Lwt_main.run (Store.Store.remove_exn ~info:(Store.make_info "compute remove")
          store location) in
      run "value" "first" (Ok ["value", "first"]);
      run (String.make (16 * 1024 * 1024) 'k') ""
        (Error "circle storage bytes exceed limit")))

let test_storage_big_key () =
  let values = Hashtbl.create 1 in
  Hashtbl.add values (String.make (16 * 1024 * 1024) 'k') "";
  let alter store =
    let open Lwt.Syntax in
    let* result = Store.save_circle_stable_storage_checked store address
      Circle.default_limits values in
    match result with
    | Ok _ -> Lwt.return_unit
    | Error message -> fail message in
  with_storage ~alter values (fun _ snapshot deps state_root ->
    let tree = Lwt_main.run (Store.Store.Tree.find_tree snapshot.Store.tree
      ["circles"; address; "stable"; "by_hash"]) |> Option.get in
    let rows = Lwt_main.run (Store.Store.Tree.list tree []) in
    List.iter (fun (_, row) ->
      match Store.Store.Tree.destruct row with
      | `Node _ -> fail "large key row is not contents"
      | `Contents (value, _) ->
        match Store.Store.Tree.Contents.key value with
        | None -> fail "committed storage key missing"
        | Some key ->
          match Irmin_pack_unix.Pack_key.to_length key with
          | Some bytes -> Printf.printf "event = compute_read case = key pack_bytes = %d\n%!" bytes
          | None -> fail "committed storage size missing") rows;
    Store.Store.Tree.clear snapshot.Store.tree;
    Store.Store.Tree.reset_counters ();
    refused "resource compute storage exceeds limit"
      (deps.Engine.storage ~epoch_id:7L ~state_root ~circle_id:address);
    let reads = (Store.Store.Tree.counters ()).contents_find in
    Printf.printf "event = compute_read case = key contents = %d\n%!" reads;
    if reads > 1 then fail "large storage key loaded before refusal")

let test_model_eviction () =
  let first = request in
  let second = Rpc.{ request with graph_root = hash 'c' } in
  let third = Rpc.{ request with graph_root = hash 'd' } in
  let key (request : Rpc.prepare) =
    String.concat "\000"
      ["octra:resource_compute_model_cache:v2"; request.circle_id; request.graph_root]
    |> fun raw -> Digestif.SHA256.(to_hex (digest_string raw)) in
  let root =
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer "octra:compute_storage_cache:v1\000";
    Buffer.add_int32_be buffer 7l;
    Buffer.add_string buffer "weights";
    Buffer.add_int64_be buffer 1L;
    Buffer.add_string buffer Digestif.SHA256.(to_raw_string (digest_string "x"));
    Digestif.SHA256.(to_hex (digest_string (Buffer.contents buffer))) in
  let pause, release = Lwt.wait () in
  let dropped = ref [] in
  let base = deps dropped in
  let deps = Engine.{
    base with
    load_model = (fun ~cancel:_ request ->
      let open Lwt.Syntax in
      let cache_key = key request in
      let* started = Cache.native_sink.begin_cache cache_key in
      let writer = match started with Ok writer -> writer | Error reason -> fail reason in
      let* () = Lwt.finalize (fun () ->
        let* written = writer.Cache.put ~key:"weights" ~value:"x" in
        if written <> Ok () then fail "native model write failed";
        let* sealed = writer.Cache.seal ~root ~entries:1 ~bytes:1 in
        if sealed <> Ok () then fail "native model seal failed";
        Lwt.return_unit)
        (fun () -> writer.Cache.close (); Lwt.return_unit) in
      Lwt.return (Ok Engine.{
        model with
        snapshots = [request.Rpc.model_epoch, request.model_state_root];
        graph_root = request.graph_root;
        cache_key;
        cache_root = root;
        entries = 1;
        bytes = 1L;
      }));
    drop_model = (fun key ->
      let open Lwt.Syntax in
      let* result = Cache.native_drop key in
      Lwt.return (Result.map (fun _ -> ()) result));
    hold_model = Cache.native_hold;
    storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ ->
      let open Lwt.Syntax in
      let* () = pause in
      Lwt.return (Ok []));
  } in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let prepare request =
      let* result = Engine.prepare engine request in
      match result with
      | Ok _ -> Lwt.return_unit
      | Error message -> fail message in
    let* () = prepare first in
    let* () = prepare second in
    let executing = Engine.execute engine execute_request in
    let* () = Lwt.pause () in
    if not (Lwt.is_sleeping executing) then fail "model execution was not paused";
    let drop = Yojson.Safe.to_string (`Assoc [
      "action", `String "compute_storage_drop";
      "compute_storage_cache_key", `String (key first);
    ]) in
    begin match Octra_core.Circle_wasm_native.run_json drop with
    | Ok raw ->
      begin match Yojson.Safe.from_string raw with
      | `Assoc fields when List.assoc_opt "status" fields = Some (`String "busy") -> ()
      | _ -> fail "selected native model was not held"
      end
    | Error reason -> fail reason
    end;
    let* () = prepare second in
    let* () = prepare third in
    let* status = Cache.native_sink.status (key first) in
    begin match status with
    | Ok (Cache.Ready _) -> ()
    | _ -> fail "active model was evicted"
    end;
    let peer = Engine.create ~limits ~deps in
    let* prepared = Engine.prepare peer first in
    begin match prepared with
    | Ok _ -> ()
    | Error reason -> fail reason
    end;
    let* () = Engine.stop peer in
    let* status = Cache.native_sink.status (key first) in
    begin match status with
    | Ok (Cache.Ready _) -> ()
    | _ -> fail "peer stop removed active model"
    end;
    let* dropped = Cache.native_sink.drop (key first) in
    if dropped <> Error "compute storage cache busy" then
      fail "direct drop removed active model";
    Lwt.wakeup_later release ();
    let* result = executing in
    begin match result with
    | Ok _ -> ()
    | Error message -> fail message
    end;
    let peer = Engine.create ~limits ~deps in
    let* first_peer = Engine.prepare peer Rpc.{request with graph_root = hash 'e'} in
    let* next_peer = Engine.prepare peer Rpc.{request with graph_root = hash 'f'} in
    if Result.is_error first_peer || Result.is_error next_peer then
      fail "peer replacement failed";
    let repeat = Rpc.{execute_request with request_id = hash 'e'} in
    let* missing = Engine.execute engine repeat in
    if missing <> Error "compute storage cache missing" then
      fail "peer eviction did not refuse missing model";
    let* () = prepare first in
    let* result = Engine.execute engine repeat in
    begin match result with
    | Ok _ -> ()
    | Error message -> fail message
    end;
    let* () = Engine.stop peer in
    Engine.stop engine)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt_list.iter_s (fun request ->
        let open Lwt.Syntax in
        let* _ = Cache.native_sink.drop (key request) in
        Lwt.return_unit) [first; second; third]))

let test_model_handle () =
  let module Native = Octra_core.Circle_wasm_native in
  let cache_key = hash '1' in
  let root =
    let buffer = Buffer.create 128 in
    Buffer.add_string buffer "octra:compute_storage_cache:v1\000";
    Buffer.add_int32_be buffer 7l;
    Buffer.add_string buffer "weights";
    Buffer.add_int64_be buffer 1L;
    Buffer.add_string buffer Digestif.SHA256.(to_raw_string (digest_string "x"));
    Digestif.SHA256.(to_hex (digest_string (Buffer.contents buffer))) in
  let native_model = Engine.{model with cache_key; cache_root = root; entries = 1; bytes = 1L} in
  let load () = Lwt_main.run (
    let open Lwt.Syntax in
    let* started = Cache.native_sink.begin_cache cache_key in
    let writer = match started with Ok writer -> writer | Error reason -> fail reason in
    Lwt.finalize (fun () ->
      let* written = writer.Cache.put ~key:"weights" ~value:"x" in
      let* sealed = writer.Cache.seal ~root ~entries:1 ~bytes:1 in
      if written <> Ok () || sealed <> Ok () then fail "handle model load failed";
      Lwt.return_unit)
      (fun () -> writer.Cache.close (); Lwt.return_unit)) in
  let hold () = match Cache.native_hold native_model with
    | Ok lease -> lease
    | Error reason -> fail reason in
  let drop () = Lwt_main.run (Cache.native_sink.drop cache_key) in
  let busy () =
    if drop () <> Error "compute storage cache busy" then
      fail "handle released another owner" in
  load ();
  let first = hold () in
  let second = hold () in
  Fun.protect (fun () ->
    Native.lease_close first;
    Native.lease_close first;
    busy ();
    if Native.lease_hold first "{}" <> 5 then fail "closed handle reused";
    let release = Yojson.Safe.to_string (`Assoc [
      "action", `String "compute_storage_release";
      "compute_storage_lease", `Int 1;
    ]) in
    if Result.is_ok (Native.run_json release) then fail "numeric release accepted";
    busy ();
    Native.lease_close second;
    if drop () <> Ok () then fail "last handle did not release";
    load ();
    let lose () =
      let lease = hold () in
      if Native.lease_hold lease "{}" <> 5 then fail "held handle reused";
      raise (Failure "response lost") in
    begin try lose () with Failure reason when reason = "response lost" -> () end;
    Gc.full_major ();
    Gc.full_major ();
    if drop () <> Ok () then fail "lost response retained native lease";
    load ();
    let wrong = Engine.{native_model with cache_root = hash '0'} in
    begin match Cache.native_hold wrong with
    | Error "compute storage cache identity differs" -> ()
    | _ -> fail "handle accepted another root"
    end;
    if drop () <> Ok () then fail "refused hold retained native lease")
    ~finally:(fun () ->
      Native.lease_close first;
      Native.lease_close second;
      ignore (drop ()))

let test_build_handle () =
  let open Lwt.Syntax in
  let key = hash '2' in
  let begin_writer () = Cache.native_sink.begin_cache key in
  let recover () =
    let deadline = Unix.gettimeofday () +. 1. in
    let rec loop () =
      let* result = begin_writer () in
      match result with
      | Ok writer -> Lwt.return writer
      | Error "compute storage builder busy" when Unix.gettimeofday () < deadline ->
        let* () = Lwt_unix.sleep 0.001 in loop ()
      | Error reason -> fail reason in
    loop () in
  Lwt_main.run (let* first = recover () in
    Lwt.finalize (fun () ->
      let* written = first.Cache.put ~key:"weights" ~value:"x" in
      if written <> Ok () then fail "owned builder write failed";
      let* repeated = begin_writer () in
      begin match repeated with
      | Error "compute storage builder busy" -> ()
      | _ -> fail "repeat begin replaced native builder"
      end;
      let* dropped = Cache.native_drop key in
      if dropped <> Ok false then fail "key drop removed owned builder";
      first.Cache.close ();
      first.Cache.close ();
      let* next = recover () in
      Lwt.finalize (fun () ->
        let* old = first.Cache.put ~key:"weights" ~value:"y" in
        if Result.is_ok old then fail "closed builder wrote into another owner";
        let* written = next.Cache.put ~key:"weights" ~value:"z" in
        if written <> Ok () then fail "new builder was damaged";
        Lwt.return_unit)
        (fun () -> next.Cache.close (); Lwt.return_unit))
      (fun () -> first.Cache.close (); Lwt.return_unit));
  let lose () = Lwt_main.run (let* writer = recover () in
    let* written = writer.Cache.put ~key:"weights" ~value:"x" in
    if written <> Ok () then fail "lost builder write failed";
    Lwt.fail (Failure "build response lost")) in
  begin try lose () with Failure reason when reason = "build response lost" -> () end;
  Gc.full_major ();
  Gc.full_major ();
  Lwt_main.run (let* writer = recover () in
    writer.Cache.close ();
    Lwt.return_unit)

let test_prepare_hold () =
  let open Lwt.Syntax in
  let dropped = ref [] in
  let loads = ref 0 in
  let available = ref true in
  let base = deps dropped in
  let callbacks = Engine.{base with
    load_model = (fun ~cancel request -> incr loads; available := true; base.load_model ~cancel request);
    hold_model = (fun model ->
      if !available then base.hold_model model
      else Error "compute storage cache missing");
  } in
  let engine = Engine.create ~limits ~deps:callbacks in
  Lwt_main.run (let* result = Engine.prepare engine request in
    if Result.is_error result || !loads <> 1 then fail "hold model not prepared";
    available := false;
    let* result = Engine.prepare engine request in
    if Result.is_error result || !loads <> 2 then fail "missing warm model not reloaded";
    available := true;
    let* result = Engine.execute engine execute_request in
    if Result.is_error result then fail "warm model recovery failed";
    Engine.stop engine);
  let dropped = ref [] in
  let base = deps dropped in
  let deps = Engine.{base with self_test = (fun () -> Lwt.return (Error "self test failed"))} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (let* result = Engine.prepare engine request in
    if result <> Error "self test failed" then fail "prepare failure changed";
    if !dropped <> [] then fail "preflight removed an unowned model";
    Engine.stop engine)

let loaded (request : Rpc.prepare) = Engine.{
  model with
  snapshots = [request.model_epoch, request.model_state_root];
  circle_id = request.circle_id;
  graph_root = request.graph_root;
  model_root = request.model_root;
  program_root = request.program_root;
  executor_root = request.executor_root;
  cache_key = Graph.cache_key ~circle_id:request.circle_id ~graph_root:request.graph_root;
}

let execute_for ?(id = hash '6') (request : Rpc.prepare) = Rpc.{
  execute_request with
  request_id = id;
  model_epoch = request.model_epoch;
  model_state_root = request.model_state_root;
  circle_id = request.circle_id;
  graph_root = request.graph_root;
  model_root = request.model_root;
  program_root = request.program_root;
  executor_root = request.executor_root;
}

let owner_prepare state request =
  let state, reply = Owner.delta state (Owner.Reserve request) in
  match reply with
  | Error reason -> fail reason
  | Ok (Owner.Ready _) -> state
  | Ok (Owner.Build plan) ->
    if plan.victim <> None then fail "pure preparation unexpectedly needs removal";
    let state, remembered = Owner.delta state (Owner.Remember (plan.token, loaded request)) in
    if remembered <> Ok () then fail "pure preparation failed";
    fst (Owner.delta state (Owner.Finish plan.token))

let owner_acquire state request =
  let state, reply = Owner.delta state (Owner.Acquire request) in
  match reply with
  | Error reason -> fail reason
  | Ok (token, _) -> state, token

let test_model_owner () =
  let second = Rpc.{request with graph_root = hash 'c'} in
  let third = Rpc.{request with graph_root = hash 'd'} in
  let alias = Rpc.{request with program_root = hash 'a'; model_epoch = 9L} in
  let state = Owner.create 2 |> fun state -> owner_prepare state request
    |> fun state -> owner_prepare state alias |> fun state -> owner_prepare state second in
  if List.length state.Owner.models <> 3 then fail "shared model binding was lost";
  let state, lease = owner_acquire state execute_request in
  let state, reply = Owner.delta state (Owner.Reserve third) in
  let plan = match reply with
    | Ok (Owner.Build plan) -> plan
    | _ -> fail "unpinned key could not be reserved" in
  begin match plan.victim with
  | Some victim when victim.cache_key = (loaded second).cache_key -> ()
  | _ -> fail "reserved active physical key"
  end;
  let _, reply = Owner.delta state (Owner.Acquire (execute_for second)) in
  if reply <> Error "resource compute provider busy" then fail "retiring model was acquired";
  let old = Owner.{lease with generation = Int64.pred lease.generation} in
  let unchanged, () = Owner.delta state (Owner.Release (old, true)) in
  if unchanged <> state then fail "old generation released active model";
  let state, dropped = Owner.delta state (Owner.Dropped plan.token) in
  if not dropped || List.length state.models <> 2 then fail "wrong cache group was removed";
  let _, reply = Owner.delta state (Owner.Acquire (execute_for alias)) in
  begin match reply with
  | Ok _ -> ()
  | Error reason -> fail reason
  end;
  let state, () = Owner.delta state (Owner.Release (lease, true)) in
  let unchanged, () = Owner.delta state (Owner.Release (lease, true)) in
  if unchanged <> state then fail "release is not idempotent";
  let _, reply = Owner.delta state (Owner.Acquire execute_request) in
  if reply <> Error "resource compute execution repeated" then fail "execution replay accepted";
  let state, () = Owner.delta state (Owner.Finish plan.token) in
  let state, () = Owner.delta state Owner.Close in
  let _, reply = Owner.delta state (Owner.Reserve third) in
  if reply <> Error "resource compute provider stopped" then fail "closed owner prepared model";
  let state = {state with Owner.closed = false; next = Int64.max_int} in
  let _, reply = Owner.delta state (Owner.Acquire (execute_for ~id:(hash 'd') request)) in
  if reply <> Error "resource compute generation exhausted" then fail "generation wrapped"

let test_two_active_models () =
  let open Lwt.Syntax in
  let second = Rpc.{request with graph_root = hash 'c'} in
  let third = Rpc.{request with graph_root = hash 'd'} in
  let pause, release = Lwt.wait () in
  let dropped = ref [] in
  let base = deps dropped in
  let deps = Engine.{base with storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ ->
    let* () = pause in Lwt.return (Ok []))} in
  let engine = Engine.create ~limits:{limits with Config.lanes = 2} ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let* first = Engine.prepare engine request in
    let* next = Engine.prepare engine second in
    if Result.is_error first || Result.is_error next then fail "two models not prepared";
    let first = Engine.execute engine execute_request in
    let next = Engine.execute engine (execute_for ~id:(hash 'a') second) in
    let* () = Lwt.pause () in
    let* result = Engine.prepare engine third in
    if result <> Error "resource compute provider busy" then fail "active cache overflow accepted";
    if !dropped <> [] then fail "active physical model was dropped";
    Lwt.wakeup_later release ();
    let* first = first in
    let* next = next in
    if Result.is_error first || Result.is_error next then fail "active models failed";
    let* result = Engine.prepare engine third in
    if Result.is_error result || List.length !dropped <> 1 then
      fail "completed model could not be replaced";
    Engine.stop engine)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt.return_unit))

let test_model_cancel () =
  let open Lwt.Syntax in
  let pause, release = Lwt.wait () in
  let dropped = ref [] in
  let base = deps dropped in
  let finished = ref false in
  let started, announce = Lwt.wait () in
  let deps = Engine.{base with execute = (fun ~model ~program ~storage request ->
    if Lwt.is_sleeping started then Lwt.wakeup_later announce ();
    let* () = pause in finished := true;
    base.execute ~model ~program ~storage request)} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let* result = Engine.prepare engine request in
    if Result.is_error result then fail "cancel model not prepared";
    let running = Engine.execute engine execute_request in
    let* () = started in
    Lwt.cancel running;
    let* () = Lwt.pause () in
    let* result = Engine.execute engine (execute_for ~id:(hash 'a') request) in
    if result <> Error "resource compute provider busy" then fail "cancel released physical lane";
    let stopping = Engine.stop engine in
    let* () = Lwt.pause () in
    if not (Lwt.is_sleeping stopping) || !dropped <> [] then fail "stop dropped active cache";
    let* result = Engine.prepare engine request in
    if result <> Error "resource compute provider stopped" then fail "stop accepted new work";
    Lwt.wakeup_later release ();
    let* () = stopping in
    if not !finished || List.length !dropped <> 1 then fail "stop did not complete cleanup";
    Lwt.return_unit)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt.return_unit))

let test_model_failure () =
  let open Lwt.Syntax in
  let dropped = ref [] in
  let base = deps dropped in
  let reads = ref 0 in
  let deps = Engine.{base with storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ ->
    incr reads;
    if !reads = 1 then Lwt.fail (Failure "storage failed") else Lwt.return (Ok []))} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (let* prepared = Engine.prepare engine request in
    if Result.is_error prepared then fail "failure model not prepared";
    let* result = Engine.execute engine execute_request in
    if result <> Error "storage failed" then fail "storage exception changed";
    let* result = Engine.execute engine execute_request in
    if Result.is_error result then fail "storage exception leaked lease";
    Engine.stop engine)

let test_model_timeout () =
  let open Lwt.Syntax in
  let pause, release = Lwt.wait () in
  let base = deps (ref []) in
  let deps = Engine.{base with execute = (fun ~model ~program ~storage request ->
    let* () = pause in base.execute ~model ~program ~storage request)} in
  let engine = Engine.create ~limits:{limits with Config.timeout_seconds = 1} ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let* prepared = Engine.prepare engine request in
    if Result.is_error prepared then fail "timeout model not prepared";
    let* result = Engine.execute engine execute_request in
    if result <> Error "resource compute request expired" then fail "request deadline ignored";
    let* result = Engine.execute engine (execute_for ~id:(hash 'a') request) in
    if result <> Error "resource compute provider busy" then fail "timeout released physical lane";
    Lwt.wakeup_later release ();
    Engine.stop engine)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt.return_unit))

let test_model_pressure () =
  let open Lwt.Syntax in
  let pause, release = Lwt.wait () in
  let base = deps (ref []) in
  let deps = Engine.{base with load_model = (fun ~cancel:_ request ->
    let* () = pause in Lwt.return (Ok (loaded request)))} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let first = Engine.prepare engine request in
    let* () = Lwt.pause () in
    let reads = List.init 64 (fun _ -> Engine.prepare engine request) in
    let* results = Lwt_list.map_s (fun value -> value) reads in
    if not (List.for_all ((=) (Error "resource compute provider busy")) results) then
      fail "preparation pressure was not refused";
    Lwt.wakeup_later release ();
    let* result = first in
    if Result.is_error result then fail "preparation pressure lost control completion";
    Engine.stop engine)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt.return_unit))

let test_model_retiring () =
  let open Lwt.Syntax in
  let second = Rpc.{request with graph_root = hash 'c'} in
  let third = Rpc.{request with graph_root = hash 'd'} in
  let pause, release = Lwt.wait () in
  let started, announce = Lwt.wait () in
  let base = deps (ref []) in
  let deps = Engine.{base with drop_model = (fun key ->
    if key <> (loaded request).cache_key then Lwt.return (Ok ())
    else (
      if Lwt.is_sleeping started then Lwt.wakeup_later announce ();
      let* () = pause in Lwt.return (Ok ())))} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let* first = Engine.prepare engine request in
    let* next = Engine.prepare engine second in
    if Result.is_error first || Result.is_error next then fail "retiring models not prepared";
    let replacing = Engine.prepare engine third in
    let* () = started in
    let* result = Engine.execute engine execute_request in
    if result <> Error "resource compute provider busy" then fail "removing cache acquired";
    let* result = Engine.execute engine (execute_for ~id:(hash 'a') second) in
    if Result.is_error result then fail "unrelated cache execution blocked";
    Lwt.wakeup_later release ();
    let* result = replacing in
    if Result.is_error result then fail "reserved cache replacement failed";
    let* result = Engine.execute engine execute_request in
    if result <> Error "resource compute model not prepared" then fail "removed model still prepared";
    Engine.stop engine)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt.return_unit))

let test_prepare_failure () =
  let open Lwt.Syntax in
  let dropped = ref [] in
  let loads = ref 0 in
  let base = deps dropped in
  let deps = Engine.{base with load_model = (fun ~cancel:_ request ->
    incr loads;
    if !loads = 1 then Lwt.fail (Failure "model failed")
    else Lwt.return (Ok (loaded request)))} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (let* result = Engine.prepare engine request in
    if result <> Error "model failed" then fail "model exception changed";
    if !dropped <> [] then fail "failed preparation removed an unowned model";
    let* result = Engine.prepare engine request in
    if Result.is_error result then fail "model exception leaked preparation reservation";
    let* result = Engine.execute engine execute_request in
    if Result.is_error result then fail "model not available after preparation failure";
    Engine.stop engine)

let test_model_cleanup () =
  let open Lwt.Syntax in
  List.iter (fun raise_error ->
    let drops = ref 0 in
    let base = deps (ref []) in
    let deps = Engine.{base with
      load_model = (fun ~cancel:_ _ -> Lwt.return (Error "load failed"));
      drop_model = (fun _ ->
        incr drops;
        if raise_error then Lwt.fail (Failure "drop failed")
        else Lwt.return (Error "drop failed"));
    } in
    let engine = Engine.create ~limits ~deps in
    Lwt_main.run (let* result = Engine.prepare engine request in
      if result <> Error "load failed" || !drops <> 0 then fail "load failure touched another owner";
      let* result = Engine.prepare engine request in
      if result <> Error "load failed" || !drops <> 0 then fail "load retry touched another owner";
      let* result = Engine.execute engine execute_request in
      if result <> Error "resource compute model not prepared" then fail "load failure allowed execution";
      Engine.stop engine)) [false; true]

let test_model_read_cancel () =
  let open Lwt.Syntax in
  let pause, release = Lwt.wait () in
  let entered, announce = Lwt.wait () in
  let reads = ref 0 in
  let calls = ref 0 in
  let base = deps (ref []) in
  let deps = Engine.{base with
    storage = (fun ~epoch_id:_ ~state_root:_ ~circle_id:_ ->
      incr reads;
      if !reads = 1 then (
        Lwt.wakeup_later announce ();
        let* () = pause in Lwt.return (Ok []))
      else Lwt.return (Ok []));
    execute = (fun ~model ~program ~storage request ->
      incr calls; base.execute ~model ~program ~storage request);
  } in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (Lwt.finalize (fun () ->
    let* result = Engine.prepare engine request in
    if Result.is_error result then fail "read cancel model not prepared";
    let running = Engine.execute engine execute_request in
    let* () = entered in
    Lwt.cancel running;
    let* () = Lwt.pause () in
    let* result = Engine.execute engine execute_request in
    if Result.is_error result || !calls <> 1 then fail "cancelled read retained retry identity";
    let* () = Engine.stop engine in
    Lwt.wakeup_later release ();
    let* () = Lwt.pause () in
    if !calls <> 1 then fail "cancelled read later executed";
    Lwt.return_unit)
    (fun () ->
      if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
      Lwt.return_unit))

let test_model_queue_cancel () =
  let open Lwt.Syntax in
  let calls = ref 0 in
  let base = deps (ref []) in
  let deps = Engine.{base with execute = (fun ~model ~program ~storage request ->
    incr calls; base.execute ~model ~program ~storage request)} in
  let engine = Engine.create ~limits ~deps in
  Lwt_main.run (let* result = Engine.prepare engine request in
    if Result.is_error result then fail "queue cancel model not prepared";
    let running = Engine.execute engine execute_request in
    Lwt.cancel running;
    let* () = Lwt.pause () in
    let* result = Engine.execute engine execute_request in
    if Result.is_error result || !calls <> 1 then fail "cancelled queued request executed";
    Engine.stop engine)

let test_model_read_stop () =
  let open Lwt.Syntax in
  List.iter (fun expire ->
    let pause, release = Lwt.wait () in
    let entered, announce = Lwt.wait () in
    let reads = ref 0 in
    let calls = ref 0 in
    let base = deps (ref []) in
    let deps = Engine.{base with
      program = (fun ~epoch_id ~state_root ~circle_id ->
        incr reads;
        if !reads = 1 then base.program ~epoch_id ~state_root ~circle_id
        else (
          if Lwt.is_sleeping entered then Lwt.wakeup_later announce ();
          let* () = pause in base.program ~epoch_id ~state_root ~circle_id));
      execute = (fun ~model ~program ~storage request ->
        incr calls; base.execute ~model ~program ~storage request);
    } in
    let engine = Engine.create ~limits:{limits with Config.timeout_seconds = 1} ~deps in
    Lwt_main.run (Lwt.finalize (fun () ->
      let* result = Engine.prepare engine request in
      if Result.is_error result then fail "read stop model not prepared";
      let running = Engine.execute engine execute_request in
      let* () = entered in
      let* result = if expire then running else (
        let* () = Engine.stop engine in running) in
      let reason = if expire then "resource compute request expired"
        else "resource compute provider stopped" in
      if result <> Error reason then fail "read not stopped";
      let* () = Engine.stop engine in
      Lwt.wakeup_later release ();
      let* () = Lwt.pause () in
      if !calls <> 0 then fail "stopped read later executed";
      Lwt.return_unit)
      (fun () ->
        if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
        Lwt.return_unit))) [false; true]

let test_prepare_read_stop () =
  let open Lwt.Syntax in
  List.iter (fun expire ->
    let second = Rpc.{request with graph_root = hash 'c'} in
    let third = Rpc.{request with graph_root = hash 'd'} in
    let pause, release = Lwt.wait () in
    let entered, announce = Lwt.wait () in
    let reads = ref 0 in
    let loads = ref 0 in
    let base = deps (ref []) in
    let deps = Engine.{base with
      program = (fun ~epoch_id ~state_root ~circle_id ->
        incr reads;
        if !reads <= 2 then base.program ~epoch_id ~state_root ~circle_id
        else (
          if Lwt.is_sleeping entered then Lwt.wakeup_later announce ();
          let* () = pause in base.program ~epoch_id ~state_root ~circle_id));
      load_model = (fun ~cancel request -> incr loads; base.load_model ~cancel request);
    } in
    let engine = Engine.create ~limits:{limits with Config.timeout_seconds = 1} ~deps in
    Lwt_main.run (Lwt.finalize (fun () ->
      let* first = Engine.prepare engine request in
      let* next = Engine.prepare engine second in
      if Result.is_error first || Result.is_error next then fail "read prepare models missing";
      let pending = Engine.prepare engine third in
      let* () = entered in
      let* result = if expire then pending else (
        let* () = Engine.stop engine in pending) in
      let reason = if expire then "resource compute request expired"
        else "resource compute provider stopped" in
      if result <> Error reason then fail "prepare read not stopped";
      let* () = Engine.stop engine in
      Lwt.wakeup_later release ();
      let* () = Lwt.pause () in
      if !loads <> 2 then fail "stopped preparation later loaded model";
      Lwt.return_unit)
      (fun () ->
        if Lwt.is_sleeping pause then Lwt.wakeup_later release ();
        Lwt.return_unit))) [false; true]

let test_cache_roots () =
  let open Lwt.Syntax in
  let sha raw = Digestif.SHA256.(to_hex (digest_string raw)) in
  let digest domain json = sha (domain ^ "\000" ^ Graph.stable_json json) in
  let chunk_hash = sha "x" in
  let buffer = Buffer.create 128 in
  Buffer.add_string buffer "test.compute.model.1";
  Buffer.add_int16_be buffer 7;
  Buffer.add_string buffer "weights";
  Buffer.add_int64_be buffer 0L;
  Buffer.add_int16_be buffer 1;
  Buffer.add_int16_be buffer 0;
  Buffer.add_int32_be buffer 1l;
  Buffer.add_string buffer chunk_hash;
  let model_root = sha (Buffer.contents buffer) in
  let model_json = `Assoc [
    "chunks", `Int 1;
    "config", `Assoc [];
    "id", `String "weights";
    "raw_bytes", `Int 1;
    "root", `String model_root;
    "root_domain", `String "test.compute.model.1";
    "tensors", `List [`Assoc [
      "chunks", `List [`Int 0]; "name", `String "weights";
      "scale_bits", `Int 0; "shape", `List [`Int 1]; "source_name", `String "weights";
    ]];
  ] in
  let chunks = `List [`Assoc [
    "bytes", `Int 1; "index", `Int 0; "sha256", `String chunk_hash;
    "shard", `Int 0; "shard_offset", `Int 0; "source", `String "weights";
    "stream_offset", `Int 0; "tensor", `String "weights"; "tensor_chunk", `Int 0;
  ]] in
  let shard = [
    "bytes", `Int 1; "chunk_indices", `List [`Int 0]; "index", `Int 0;
    "sha256", `String chunk_hash; "stream_offset", `Int 0;
    "segments", `List [`Assoc [
      "bytes", `Int 1; "index", `Int 0; "offset", `Int 0;
      "path", `String "/model/data/weights"; "sha256", `String chunk_hash;
    ]];
  ] in
  let format = `String "octra-resource-tensor-graph-v1" in
  let layout = `Assoc ["chunks", chunks; "format", format; "model", model_json;
    "shards", `List [`Assoc shard]] in
  let fields = [
    "chunks", chunks; "format", format; "model", model_json;
    "layout_root", `String (digest "octra:resource_tensor_layout:v1" layout);
    "root_circle_id", `String address;
    "shards", `List [`Assoc (("circle_id", `String address) :: shard)];
  ] in
  let graph_root = digest "octra:resource_tensor_graph:v1" (`Assoc fields) in
  let raw = Graph.stable_json (`Assoc (("graph_root", `String graph_root) :: fields)) in
  let graph = match Graph.parse ~limits:Graph.default_limits ~root_circle_id:address raw with
    | Ok graph -> graph
    | Error reason -> fail reason in
  let nested = ref (`Int 7) in
  for _ = 1 to 12000 do nested := `List [!nested] done;
  List.iter (fun config ->
    let model = match model_json with
      | `Assoc fields -> `Assoc (("config", config) :: List.remove_assoc "config" fields)
      | _ -> assert false in
    let layout = match layout with
      | `Assoc fields -> `Assoc (("model", model) :: List.remove_assoc "model" fields)
      | _ -> assert false in
    let fields = ("model", model) :: ("layout_root", `String
      (digest "octra:resource_tensor_layout:v1" layout))
      :: List.remove_assoc "layout_root" (List.remove_assoc "model" fields) in
    let root = digest "octra:resource_tensor_graph:v1" (`Assoc fields) in
    let encoded = Graph.stable_json (`Assoc (("graph_root", `String root) :: fields)) in
    match Graph.parse ~limits:Graph.default_limits ~root_circle_id:address encoded with
    | Ok parsed when parsed.graph_root = root && parsed.model.root = graph.model.root -> ()
    | _ -> fail "deep graph changed hashes")
    [!nested; `Assoc (List.init 12000 (fun i -> string_of_int i, `Int i))];
  let broken = String.make 12000 '[' ^ "0" in
  if Graph.parse ~limits:Graph.default_limits ~root_circle_id:address broken
      <> Error "resource graph JSON refused" then fail "deep invalid JSON accepted";
  List.iter (fun raw ->
    if Graph.parse ~limits:Graph.default_limits ~root_circle_id:address raw
        <> Error "resource graph JSON refused" then fail "graph extension accepted")
    ["(0)"; "<Tag:0>"; "[" ^ String.make 12000 '(' ^ "0"];
  if Graph.stable_json (`Assoc ["z", `Int 1; "a", `List [`String "x"; `Float 1.5]])
      <> "{\"a\":[\"x\",1.5],\"z\":1}" then fail "graph JSON bytes changed";
  let cache = Graph.cache_plan ~limits:Graph.default_limits ~state_root:request.model_state_root graph
    |> Result.get_ok in
  let effects = ref 0 in
  let effect result = incr effects; Lwt.return result in
  let sink = Cache.{
    status = (fun _ -> effect (Ok (Ready {
      root = cache.root; entries = List.length cache.entries; bytes = Int64.to_int cache.bytes;
    })));
    begin_cache = (fun _ -> effect (Ok {
      put = (fun ~key:_ ~value:_ -> effect (Ok ()));
      seal = (fun ~root:_ ~entries:_ ~bytes:_ -> effect (Ok ()));
      close = (fun () -> incr effects);
    }));
    drop = (fun _ -> effect (Ok ()));
  } in
  let snapshot = Cache.{epoch_id = request.model_epoch; state_root = request.model_state_root} in
  let fetch_asset (snapshot : Cache.snapshot) ~max_bytes:_ ~circle_id ~path =
    let content_type, body = match path with
      | "/model/data/weights" -> "application/octet-stream", "x"
      | "/model/shard.json" -> "application/json; charset=utf-8",
        Graph.stable_json (Graph.shard_manifest_json graph (List.hd graph.shards))
      | _ -> "application/json; charset=utf-8", raw in
    Lwt.return (Ok Cache.{
      state_root = snapshot.Cache.state_root; circle_id; path;
      content_type; encoding = "identity";
      browser_mode = "gateway_allowed"; resource_mode = "public_resources";
      blob_hash = sha body; size_bytes = String.length body; body;
    }) in
  let read graph_root model_root sink = Cache.load ~limits:Graph.default_limits
    ~cancel:(fst (Lwt.wait ()))
    ~snapshot ~root_circle_id:address ~graph_root ~model_root ~fetch_asset ~sink in
  refused "resource compute graph root differs" (read (hash 'a') model_root sink);
  refused "resource compute model root differs" (read graph_root (hash 'a') sink);
  if !effects <> 0 then fail "root refusal touched native cache";
  begin match Lwt_main.run (read graph_root model_root sink) with
  | Ok loaded when loaded.Cache.cache.key = cache.key && !effects = 1 -> ()
  | _ -> fail "matching roots did not reuse verified cache"
  end;
  let sink = {sink with Cache.status = (fun _ ->
    effect (Ok (Cache.Ready {root = hash 'a'; entries = 0; bytes = 0})))} in
  let before = !effects in
  refused "resource compute cache identity differs" (read graph_root model_root sink);
  if !effects <> before + 1 then fail "shared mismatched cache was dropped";
  let drops = ref 0 in
  let sink = Cache.{
    status = (fun _ -> Lwt.return (Ok Cache.Missing));
    begin_cache = (fun _ -> Lwt.return (Error "builder busy"));
    drop = (fun _ -> incr drops; Lwt.return (Ok ()));
  } in
  refused "builder busy" (read graph_root model_root sink);
  if !drops <> 0 then fail "refused begin removed another builder";
  List.iter (fun raise_error ->
    let closed = ref 0 in
    let writer = Cache.{
      put = (fun ~key:_ ~value:_ ->
        if raise_error then Lwt.fail (Failure "write failed")
        else Lwt.return (Error "write failed"));
      seal = (fun ~root:_ ~entries:_ ~bytes:_ -> fail "failed writer reached seal");
      close = (fun () -> incr closed);
    } in
    let sink = {sink with Cache.begin_cache = (fun _ -> Lwt.return (Ok writer))} in
    if raise_error then begin
      try ignore (Lwt_main.run (read graph_root model_root sink)); fail "writer exception ignored"
      with Failure reason when reason = "write failed" -> ()
    end else refused "write failed" (read graph_root model_root sink);
    if !closed <> 1 then fail "failed writer was not closed";
    if !drops <> 0 then fail "failed writer removed an unowned key") [false; true];
  let paused, _ = Lwt.task () in
  let closed = ref 0 in
  let sink = {sink with Cache.begin_cache = (fun _ -> Lwt.return (Ok Cache.{
    put = (fun ~key:_ ~value:_ -> Lwt.map (fun () -> Ok ()) paused);
    seal = (fun ~root:_ ~entries:_ ~bytes:_ -> fail "cancelled writer reached seal");
    close = (fun () -> incr closed);
  }))} in
  let pending = read graph_root model_root sink in
  Lwt.cancel pending;
  if !closed <> 1 then fail "cancelled writer was not closed";
  if !drops <> 0 then fail "cancelled writer removed an unowned key";
  List.iter (fun mode ->
    let gate, release = Lwt.wait () in
    let entered, announce = Lwt.wait () in
    let fetch (snapshot : Cache.snapshot) ~max_bytes:_ ~circle_id ~path =
      let body, content_type = match path with
        | "/model/resource-graph.json" -> raw, "application/json; charset=utf-8"
        | "/model/shard.json" ->
          Graph.stable_json (Graph.shard_manifest_json graph (List.hd graph.shards)),
          "application/json; charset=utf-8"
        | "/model/data/weights" -> "x", "application/octet-stream"
        | _ -> fail "unexpected model asset path" in
      let* () = if path = "/model/shard.json" then (
        if Lwt.is_sleeping entered then Lwt.wakeup_later announce ();
        Lwt.protected gate) else Lwt.return_unit in
      Lwt.return (Ok Cache.{
        state_root = snapshot.state_root; circle_id; path; content_type;
        encoding = "identity"; browser_mode = "gateway_allowed";
        resource_mode = "public_resources"; blob_hash = sha body;
        size_bytes = String.length body; body;
      }) in
    let base = deps (ref []) in
    let actual = Rpc.{request with graph_root; model_root} in
    let deps = Engine.{base with load_model = (fun ~cancel request ->
      let* result = Cache.load ~cancel ~limits:Graph.default_limits ~snapshot
        ~root_circle_id:address ~graph_root ~model_root ~fetch_asset:fetch ~sink:Cache.native_sink in
      Lwt.return (Result.map (fun loaded -> Engine.{
        model with graph_root; model_root;
        snapshots = [request.Rpc.model_epoch, request.model_state_root];
        cache_key = loaded.Cache.cache.key; cache_root = loaded.cache.root;
        entries = List.length loaded.cache.entries; bytes = loaded.cache.bytes;
      }) result))} in
    let engine = Engine.create ~limits:{limits with timeout_seconds = 1} ~deps in
    let recover () =
      let deadline = Unix.gettimeofday () +. 1. in
      let rec loop () =
        let* result = Cache.native_sink.begin_cache cache.key in
        match result with
        | Ok writer ->
          writer.Cache.close ();
          let rec retired () =
            let* result = Cache.native_sink.drop cache.key in
            match result with
            | Ok () -> Lwt.return_unit
            | Error "compute storage cache busy" when Unix.gettimeofday () < deadline ->
              let* () = Lwt_unix.sleep 0.001 in retired ()
            | _ -> fail "closed native build was not retired" in
          retired ()
        | Error "compute storage builder busy" when Unix.gettimeofday () < deadline ->
          let* () = Lwt_unix.sleep 0.001 in loop ()
        | _ -> fail "request did not release native build" in
      loop () in
    Lwt_main.run (Lwt.finalize (fun () ->
      let* dropped = Cache.native_sink.drop cache.key in
      if dropped <> Ok () then fail "cancel model was not idle";
      let pending = Engine.prepare engine actual in
      let* () = Lwt.pick [Lwt.protected entered;
        (let* () = Lwt_unix.sleep 2. in fail "model fetch was not reached")] in
      let stopping = match mode with
        | 0 -> Lwt.cancel pending; Lwt.return_unit
        | 1 -> Lwt.return_unit
        | _ -> Engine.stop engine in
      let* result = Lwt.catch (fun () -> pending)
        (function Lwt.Canceled -> Lwt.return (Error "cancelled") | error -> Lwt.fail error) in
      let expected = match mode with
        | 0 -> "cancelled"
        | 1 -> "resource compute request expired"
        | _ -> "resource compute provider stopped" in
      if result <> Error expected then fail "native preparation cancellation differed";
      let* () = recover () in
      if Lwt.is_sleeping gate then Lwt.wakeup_later release ();
      let* () = stopping in
      let* () = Engine.stop engine in
      let* status = Cache.native_sink.status cache.key in
      if status <> Ok Cache.Missing then fail "cancelled engine published a model";
      Lwt.return_unit)
      (fun () ->
        if Lwt.is_sleeping gate then Lwt.wakeup_later release ();
        Engine.stop engine))) [0; 1; 2]

let cache_sample body chunk_sizes segment_sizes mode =
  let sha raw = Digestif.SHA256.(to_hex (digest_string raw)) in
  let digest domain json = sha (domain ^ "\000" ^ Graph.stable_json json) in
  let parts sizes =
    let _, values = List.fold_left (fun (offset, values) size ->
      let value = String.sub body offset size in
      offset + size, (offset, value) :: values) (0, []) sizes in
    List.rev values in
  let values = parts chunk_sizes in
  let chunk_hashes = List.mapi (fun index (_, value) ->
    if mode = 2 && index = 1 then hash 'f' else sha value) values in
  let buffer = Buffer.create 128 in
  Buffer.add_string buffer "test.compute.model.1";
  Buffer.add_int16_be buffer 7;
  Buffer.add_string buffer "weights";
  Buffer.add_int64_be buffer 0L;
  Buffer.add_int16_be buffer (List.length values);
  List.iteri (fun index (_, value) ->
    Buffer.add_int16_be buffer index;
    Buffer.add_int32_be buffer (Int32.of_int (String.length value));
    Buffer.add_string buffer (List.nth chunk_hashes index)) values;
  let chunks = `List (List.mapi (fun index (offset, value) -> `Assoc [
    "bytes", `Int (String.length value); "index", `Int index;
    "sha256", `String (List.nth chunk_hashes index);
    "shard", `Int 0; "shard_offset", `Int offset; "source", `String "weights";
    "stream_offset", `Int offset; "tensor", `String "weights"; "tensor_chunk", `Int index;
  ]) values) in
  let assets = parts segment_sizes |> List.mapi (fun index (offset, value) ->
    let path = Printf.sprintf "/model/data/segment-%d" index in
    path, value, `Assoc [
      "bytes", `Int (String.length value); "index", `Int index;
      "offset", `Int offset; "path", `String path; "sha256", `String (sha value);
    ]) in
  let indices = List.mapi (fun index _ -> `Int index) values in
  let model_json = `Assoc [
    "chunks", `Int (List.length values); "config", `Assoc []; "id", `String "weights";
    "raw_bytes", `Int (String.length body); "root", `String (sha (Buffer.contents buffer));
    "root_domain", `String "test.compute.model.1";
    "tensors", `List [`Assoc [
      "chunks", `List indices; "name", `String "weights"; "scale_bits", `Int 0;
      "shape", `List [`Int (String.length body)]; "source_name", `String "weights";
    ]];
  ] in
  let shard = [
    "bytes", `Int (String.length body); "chunk_indices", `List indices; "index", `Int 0;
    "sha256", `String (if mode = 1 then hash 'f' else sha body); "stream_offset", `Int 0;
    "segments", `List (List.map (fun (_, _, json) -> json) assets);
  ] in
  let format = `String "octra-resource-tensor-graph-v1" in
  let layout = `Assoc ["chunks", chunks; "format", format; "model", model_json;
    "shards", `List [`Assoc shard]] in
  let fields = [
    "chunks", chunks; "format", format; "model", model_json;
    "layout_root", `String (digest "octra:resource_tensor_layout:v1" layout);
    "root_circle_id", `String address;
    "shards", `List [`Assoc (("circle_id", `String address) :: shard)];
  ] in
  let root = digest "octra:resource_tensor_graph:v1" (`Assoc fields) in
  let raw = Graph.stable_json (`Assoc (("graph_root", `String root) :: fields)) in
  let graph = match Graph.parse ~limits:Graph.default_limits ~root_circle_id:address raw with
    | Ok graph -> graph
    | Error reason -> fail reason in
  let manifest = Graph.stable_json (Graph.shard_manifest_json graph (List.hd graph.shards)) in
  let assets = ("/model/resource-graph.json", raw, "application/json; charset=utf-8") ::
    ("/model/shard.json", manifest, "application/json; charset=utf-8") ::
    List.map (fun (path, body, _) -> path, body, "application/octet-stream") assets in
  let assets = List.map (fun (path, body, content_type) -> path, Cache.{
    state_root = request.model_state_root; circle_id = address; path; content_type;
    encoding = "identity"; browser_mode = "gateway_allowed";
    resource_mode = "public_resources"; blob_hash = sha body;
    size_bytes = String.length body; body;
  }) assets in
  let fetch (_ : Cache.snapshot) ~max_bytes:_ ~circle_id ~path =
    if circle_id <> address then fail "cache Circle differs";
    Lwt.return (Ok (List.assoc path assets)) in
  let read ?(cancel = fst (Lwt.wait ())) ?(limits = Graph.default_limits) fetch sink = Cache.load ~cancel
    ~limits
    ~snapshot:Cache.{epoch_id = request.model_epoch; state_root = request.model_state_root}
    ~root_circle_id:address ~graph_root:graph.graph_root ~model_root:graph.model.root
    ~fetch_asset:fetch ~sink in
  graph, values, fetch, read

let test_payment_flow () =
  let module C = Octra_consensus.C_types in
  let module H = Octra_consensus.C_hash in
  let module P = Octra_consensus.Resource_compute_protocol in
  let module Cap = Octra_consensus.Resource_compute_capability in
  let module Sel = Octra_consensus.Resource_compute_selection in
  let module Cert = Octra_consensus.Resource_compute_certificate in
  let module Record = Octra_node_runtime.Compute_record in
  let module Bill = Octra_core.Compute_bill in
  let module Anchor = Octra_bootstrap.Sync_anchor in
  let module Settle = Octra_node_runtime.Compute_settle in
  let run = Lwt_main.run in
  let get = function Ok value -> value | Error reason -> fail reason in
  let key = String.make 32 '\123' in
  let private_key = Mirage_crypto_ec.Ed25519.priv_of_octets key |> Result.get_ok in
  let public_key = Mirage_crypto_ec.Ed25519.(pub_to_octets (pub_of_priv private_key)) in
  let wallet = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn public_key) in
  let sign msg = H.sign_ed25519 ~priv_raw:key ~msg in
  let trusted = C.make_validator_set [C.{address = wallet; pubkey = Base64.encode_exn public_key}] in
  let validators = Anchor.raw_validator_set trusted |> get in
  let chain_id = "compute-flow-test" in
  let byte value = String.make 1 (Char.chr value) in
  let rec leb value = if value < 128 then byte value
    else byte ((value land 127) lor 128) ^ leb (value lsr 7) in
  let text value = leb (String.length value) ^ value in
  let section tag value = byte tag ^ text value in
  let code = "\000asm\001\000\000\000"
    ^ section 1 "\002\096\002\127\127\001\127\096\001\127\001\127"
    ^ section 2 ("\002" ^ text "octra" ^ text "host_response_write" ^ "\000\000"
      ^ text "octra" ^ text "host_tensor_silu_q24" ^ "\000\000")
    ^ section 3 "\002\001\000" ^ section 5 "\001\000\001"
    ^ section 7 ("\003" ^ text "memory" ^ "\002\000"
      ^ text "octra_alloc" ^ "\000\002" ^ text "octra_query" ^ "\000\003")
    ^ section 10 ("\002" ^ text "\000\065\000\011"
      ^ text "\000\065\128\004\065\001\016\001\026\065\128\002\065\010\016\000\026\065\000\011")
    ^ section 11 ("\001\000\065\128\002\011" ^ text "OCWS1\000\000\000\000\000") in
  let graph, _, fetch, _ = cache_sample "abcdefgh" [4; 4] [3; 5] 0 in
  Test_workspace.with_dir "compute-flow" (fun directory ->
    let store = run (Store.open_store ~fresh:true (Filename.concat directory "irmin")) in
    Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
      let module Ledger = Octra_core.Ledger in
      let ledger = Ledger.create_from_store store in
      let initial = Z.of_int 20_000_000 in
      Ledger.add_account ledger wallet initial |> get;
      run (Ledger.flush_dirty_lwt ledger);
      run (seed_compute store code "weights");
      run (Store.begin_epoch_batch store);
      let paths = ["/model/resource-graph.json"; "/model/shard.json";
        "/model/data/segment-0"; "/model/data/segment-1"] in
      List.iter (fun path ->
        let asset = run (fetch Cache.{epoch_id = 7L; state_root = request.model_state_root}
          ~max_bytes:65536 ~circle_id:address ~path) |> get in
        let path, path_key = Circle.path_key_of_raw_path path |> get in
        let meta = Circle.{path_key; canonical_path = path; content_type = asset.content_type;
          encoding = "identity"; size_bytes = Int64.of_int asset.size_bytes;
          blob_hash = asset.blob_hash; body_mode = Public_resources; plaintext_hash = None;
          key_id = None; padding_class = None; resource_key = path_key; locator_mode = Path_locator;
          slot_ref = None; activate_after_epoch = None; expire_after_epoch = None; metadata_mode = Metadata_reveal} in
        run (Store.save_circle_asset_meta store address meta);
        run (Store.save_circle_asset_body_b64 store address path_key (Base64.encode_exn asset.body))) paths;
      let info = run (Store.get_circle_info store address) |> Option.get in
      run (Store.deploy_circle store {info with browser_mode = Circle.Gateway_allowed});
      run (Store.commit_epoch_batch store "compute model");
      run (Store.tag_epoch store 7);
      let snapshot = run (Store.capture_read_snapshot store) |> get in
      let index = hash 'e' in
      let root = Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:snapshot.state_root ~epoch_index_root:index in
      let root_raw = Octra_node_runtime.Text.hex_to_string root in
      let header = C.{proto_version = proto_version_current; chain_id; epoch_id = 7L;
        prev_state_root = String.make 32 'p'; tx_list_hash = H.tx_list_hash [];
        receipt_root = H.receipt_root []; proposed_state_root = root_raw;
        parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = wallet;
        txid_hi = 0L; ts = 7.} in
      let proposal_id = H.proposal_id header in
      let vote = C.{chain_id; epoch_id = 7L; round = 0; vote_type = Precommit;
        proposal_id; validator = wallet; signature = ""} in
      let vote = {vote with signature = sign (H.vote_sign_bytes vote)} in
      let finalize = C.{chain_id; epoch_id = 7L; commit_round = 0; header;
        proposal_id; precommits = [vote]; parent_commit = None} in
      let encoded = Anchor.make ~steps:[] ~finalize ~validator_set:validators |> Anchor.encode in
      let origin = Anchor.verify_origin ~max_bytes:65536 ~trusted ~chain_id
        ~epoch_id:7L ~state_root:root_raw ~set_root:(Octra_consensus.C_config.validator_set_hash validators)
        encoded |> get in
      let state_root_at epoch = if epoch = 7L then Some root_raw else None in
      let index_root_at epoch = if epoch = 7L then Some index else None in
      let limits = {limits with memory_bytes = Bill.model_limit} in
      let deps = Engine.native_deps ~limits ~store ~state_root_at ~index_root_at in
      let executor = run (deps.self_test ()) |> get in
      let offer = P.{chain_id; offered_epoch = 4L; expires_epoch = 15L;
        coordinator = wallet; validator_set_root = Octra_consensus.C_config.validator_set_hash validators;
        circle_id = address; model_epoch = 7L; model_state_root = root_raw;
        graph_root = Octra_node_runtime.Text.hex_to_string graph.graph_root;
        model_root = Octra_node_runtime.Text.hex_to_string graph.model.root;
        program_root = Digestif.SHA256.(to_raw_string (digest_string code));
        executor_root = Octra_node_runtime.Text.hex_to_string executor.executor_root;
        min_memory_bytes = 131072L; request_bytes = 2; response_bytes = 256;
        signature = String.make 64 '\000'} in
      let offer = {offer with signature = sign (P.offer_sign_bytes offer)} in
      let capability = Cap.{chain_id; observed_epoch = 4L; valid_until = 15L;
        validator_set_root = offer.validator_set_root; node_id = wallet; circle_id = address;
        model_epoch = 7L; model_state_root = root_raw; graph_root = offer.graph_root;
        model_root = offer.model_root; program_root = offer.program_root; executor_root = offer.executor_root;
        evidence_root = String.make 32 'e'; accelerator = Cpu; lanes = 1; memory_bytes = Bill.model_limit;
        max_request_bytes = 65536; max_response_bytes = 256; signature = String.make 64 '\000'} in
      let capability = {capability with signature = sign (Cap.sign_bytes capability)} in
      let commitment = Sel.{chain_id; offer_id = P.offer_id offer; commit_epoch = 5L;
        node_id = wallet; graph_root = offer.graph_root; model_root = offer.model_root;
        program_root = offer.program_root; executor_root = offer.executor_root;
        nonce_hash = Sel.nonce_hash "nonce"; signature = String.make 64 '\000'} in
      let commitment = {commitment with signature = sign (Sel.commitment_sign_bytes commitment)} in
      let reveal = Sel.{chain_id; offer_id = commitment.offer_id; commit_epoch = 5L;
        reveal_epoch = 6L; node_id = wallet; graph_root = offer.graph_root;
        model_root = offer.model_root; program_root = offer.program_root; executor_root = offer.executor_root;
        nonce = "nonce"; evidence_root = capability.evidence_root; signature = String.make 64 '\000'} in
      let reveal = {reveal with signature = sign (Sel.reveal_sign_bytes commitment reveal)} in
      let selected = Sel.select ~chain_id ~offer_id:commitment.offer_id ~target_epoch:7L
        ~challenge:(Sel.challenge ~chain_id ~source_epoch:7L ~finality_hash:root_raw)
        ~minimum_delay:P.reveal_delay ~committee_size:1
        ~verify_commitment_signature:(fun _ -> true) ~verify_reveal_signature:(fun _ _ -> true)
        ~verify_evidence:(fun _ -> true) ~commitments:[commitment] ~reveals:[reveal] in
      let request = Cert.{unit_price = Some 2L; fee_cap = Some 200000L;
        memory_bytes = Some 1048576; guest_bytes = Some 65536; fuel_limit = Some 100000;
        chain_id; epoch_id = 7L; expires_epoch = 15L; validator_set_root = offer.validator_set_root;
        selection_root = selected.root; circle_id = address; state_root = root_raw;
        model_epoch = 7L; model_state_root = root_raw; graph_root = offer.graph_root;
        model_root = offer.model_root; program_root = offer.program_root; executor_root = offer.executor_root;
        caller = wallet; session_id = String.make 32 's'; sequence = 1L;
        input_hash = P.input_hash ~method_name:"predict" ~params_json:"[]";
        input_bytes = 2; max_output_bytes = 256} in
      let call = P.{request; valid_until = Some 500L; method_name = "predict"; params_json = "[]";
        caller_public_key = public_key; caller_signature = String.make 64 '\000';
        coordinator = wallet; signature = String.make 64 '\000'} in
      let call = {call with caller_signature = sign (P.intent_sign_bytes call)} in
      let call = {call with signature = sign (P.call_sign_bytes call)} in
      let program_at = Engine.program_checked state_root_at index_root_at store in
      let load_model = Engine.model_checked limits state_root_at index_root_at store in
      let source, fetch_asset = run (Cache.store_source_at store ~epoch_id:7L ~state_root:root ~index_root:index) |> get in
      let load fetch_asset = run (Cache.load_checked ~cancel:(fst (Lwt.wait ()))
        ~limits:(Graph.default_limits) ~snapshot:source ~root_circle_id:address
        ~graph_root:graph.graph_root ~model_root:graph.model.root ~fetch_asset ~sink:Cache.native_sink) in
      let cached = match load fetch_asset with Ok value -> value
        | Error reason -> fail (Octra_core.Circle_wasm_host.error_message reason) in
      let damaged snapshot ~max_bytes ~circle_id ~path =
        let open Lwt.Syntax in
        let+ result = fetch_asset snapshot ~max_bytes ~circle_id ~path in
        Result.map (fun (asset : Cache.asset) ->
          if path <> "/model/data/segment-0" then asset else
            let body = String.make asset.size_bytes 'x' in
            {asset with body; blob_hash = Digestif.SHA256.(to_hex (digest_string body))}) result in
      let hot = load damaged in
      ignore (run (Cache.native_drop cached.cache.key) |> get);
      let cold = load damaged in
      if hot <> cold || hot <> Error (Octra_core.Circle_wasm_host.Rejected "resource segment differs") then
        fail "payment cache bypassed snapshot resources";
      let measured = run (Engine.replay_loaded ~limits ~deps ~program_at ~load_model
        [call, Settle.request call]) |> function
        | Ok [value] -> value | Ok _ -> fail "payment replay count"
        | Error reason -> fail (Octra_core.Circle_wasm_host.error_message reason) in
      let vote = Cert.{request_id = measured.request_id; node_id = wallet; output_hash = measured.output_hash;
        output_bytes = measured.output_bytes; trace_root = measured.trace_root; steps = measured.steps;
        operations = measured.operations; signature = String.make 64 '\000'} in
      let vote = {vote with signature = sign (Cert.vote_sign_bytes vote)} in
      let record = Record.{offer; capabilities = [capability]; commitments = [commitment];
        reveals = [reveal]; call; results = [P.{vote; output_json = "null"}]; validators = Some validators} in
      let raw = Bill.encode_request ~fuel:100000 [Record.encode record] in
      let tx = Octra_core.Transaction.{from = wallet; to_ = Octra_core.Compute_funding.escrow;
        amount = Z.zero; nonce = 1; ou = Bill.fee raw; timestamp = 8.; signature = "";
        public_key = None; message = Some raw; encrypted_data = None; op_type = ComputeSettle} in
      let lane = Octra_core.Resource_lanes.of_op tx.op_type in
      if lane <> Octra_core.Resource_lanes.Program ||
          not (Octra_core.Resource_lanes.within (Octra_core.Resource_lanes.default_budget lane)
            (Octra_core.Resource_lanes.work tx)) then fail "payment cannot enter lane";
      let lookup epoch = if epoch = 7L then Lwt.return_ok Settle.{proof = origin; index}
        else Lwt.return_error "unexpected epoch" in
      let engine _ = deps, program_at, load_model in
      let funded _ = Lwt.return_ok (Bill.Due 8L) in
      let verify ?(lookup = lookup) ?(engine = engine) ?(funded = funded) raw =
        run (Settle.verify ~lookup ~engine ~epoch:8L ~funded raw) in
      let bills = verify raw |> function Ok values -> values
        | Error (Bill.Rejected reason | Bill.Unavailable reason) -> fail reason in
      if List.length bills <> 1 || not (Z.equal (List.hd bills).charged
          (Z.mul (Z.of_int 2) (Z.of_int64 measured.steps))) then fail "payment replay amount";
      if measured.outcome <> Bill.Complete || measured.operations <> 1L then fail "payment native work";
      let module Funding = Octra_core.Compute_funding in
      let module Ledger = Octra_core.Ledger in
      let module Epoch = Octra_core.Epoch_exec in
      let anchor = ref None in
      let fold epoch = Result.map (fun context ->
        Epoch.{context with vm_bytes = Octra_core.Rule_graph.Active; parent = !anchor}) (Epoch.prior_fold epoch) in
      let compute = Settle.make () in
      let backend = Epoch.make_live_backend ~fold ~compute store ledger in
      let env = Epoch.{chain_id; epoch_id = 8; epoch_ts = 8.; proposer_addr = wallet;
        validator_addrs = [wallet]; validator_pubkeys = [wallet, Base64.encode_exn public_key];
        prev_state_root = root; ready_state_root_at = None; ready_max_lag = 0} in
      let terms = Funding.prepare_until ~expires_epoch:500L ~call ~target:Funding.escrow ~nonce:1L |> get in
      let permission = Funding.authorize terms ~signature:(sign (Funding.sign_bytes terms)) |> get in
      let header = P.header call |> get in
      let signed tx = Octra_core.Transaction.sign_with_privkey tx (Base64.encode_exn key) in
      let fund = signed {tx with nonce = 1; op_type = ComputeFund; amount = Z.of_int64 terms.amount;
        ou = Z.of_int 1_000_000; public_key = Some (Base64.encode_exn public_key);
        message = Some (Funding.encode_request ~header permission)} in
      let next = {call with request = {call.request with sequence = 2L}} in
      let next = {next with caller_signature = sign (P.intent_sign_bytes next)} in
      let next = {next with signature = sign (P.call_sign_bytes next)} in
      let next_header = P.header next |> get in
      let next_terms = Funding.prepare_until ~expires_epoch:500L ~call:next ~target:Funding.escrow ~nonce:2L |> get in
      let next_auth = Funding.authorize next_terms ~signature:(sign (Funding.sign_bytes next_terms)) |> get in
      let next_fund = signed {fund with nonce = 2;
        message = Some (Funding.encode_request ~header:next_header next_auth)} in
      let pay = signed {tx with nonce = 3; public_key = Some (Base64.encode_exn public_key)} in
      run (Store.begin_epoch_batch store);
      Ledger.begin_journal ledger |> get;
      let parent = C.{certificate = {chain_id; epoch_id = 7L; commit_round = 0;
        header = finalize.header; proposal_id = finalize.proposal_id; precommits = finalize.precommits};
        validator_set = validators} in
      anchor := Some parent;
      run (Epoch.advance_validator_set ~backend ~env);
      anchor := None;
      let pin header budget = run (Octra_core.Compute_input.prepare store ~max_fee:budget ~header) in
      if Result.is_ok (pin header Z.zero) then fail "payment pinning ignored fee limit";
      if Result.is_ok (pin (P.header {call with signature = String.make 64 '\000'} |> get) fund.ou) then
        fail "payment pinning accepted invalid coordinator";
      if Result.is_ok (pin (P.header {call with request = {call.request with state_root = String.make 32 'x'}} |> get) fund.ou) then
        fail "payment pinning accepted another root";
      if run (Epoch.apply_compute_tx ~inputs:true ~backend ~env fund) <> Ok fund.ou then
        fail "payment funding transaction failed";
      if Result.is_ok (pin header fund.ou) then fail "payment pinning accepted repeated request";
      if run (Epoch.apply_compute_tx ~inputs:true ~backend ~env next_fund) <> Ok next_fund.ou then
        fail "payment continuation funding failed";
      run (Ledger.flush_dirty_lwt ledger);
      run (Store.commit_epoch_batch store "compute pinned inputs");
      Ledger.commit_journal ledger |> get;
      run (Store.remove_tags_before store 8) |> ignore;
      run (Store.begin_epoch_batch store);
      run (Store.remove_path store ["circles"]);
      run (Store.remove_path store ["compute"; "roots"]);
      run (Store.commit_epoch_batch store "compute archived state removed");
      let compact = Filename.concat directory "restored" in
      let head = run (Store.get_commit_hash store) |> Option.get in
      ignore (run (Store.create_compact_store store ~expected_commit:head ~target:compact) |> get);
      let copied = run (Store.open_store compact) in
      Fun.protect ~finally:(fun () -> run (Store.close copied)) (fun () ->
      let store = copied in
      let ledger = Ledger.create_from_store store in
      let backend = Epoch.make_live_backend ~fold ~compute store ledger in
      if Result.is_ok (run (Store.capture_read_snapshot_epoch store ~epoch_id:7L)) then
        fail "payment history branch retained";
      ignore (run (Cache.native_drop cached.cache.key) |> get);
      let late = Epoch.{env with epoch_id = 200; epoch_ts = 200.} in
      let before_root = run (Store.state_hash store) in
      let before_account = Ledger.find ledger wallet in
      let preview_root = run (Octra_core.State_preview.with_preview
        ~compute ~base_store:store ~base_ledger:ledger ~proof_mode:Octra_core.Rule_graph.Prior
        ~fold ~epoch_id:200 ~proposal_id:"compute-payment" (fun view ->
          let open Lwt.Syntax in
          let* () = view.Epoch.begin_batch Octra_core.Rule_graph.Prior in
          Ledger.begin_journal view.ledger |> get;
          let* result = Epoch.apply_compute_effect ~backend:view ~env:late pay in
          if result <> Ok (Epoch.Confirmed pay.ou) then fail "payment preview refused";
          let* () = view.flush_dirty () in
          let* root = Store.get_batch_tree_hash view.store in
          let* () = view.commit_batch () in
          Ledger.commit_journal view.ledger |> get;
          Lwt.return_ok root)) |> get in
      if run (Store.state_hash store) <> before_root || Ledger.find ledger wallet <> before_account then
        fail "payment preview changed live state";
      run (Store.begin_epoch_batch store);
      Ledger.begin_journal ledger |> get;
      if run (Epoch.apply_compute_effect ~backend ~env:late pay) <> Ok (Epoch.Confirmed pay.ou) then
        fail "payment native settlement transaction failed";
      run (Ledger.flush_dirty_lwt ledger);
      if run (Store.get_batch_tree_hash store) <> preview_root then
        fail "payment preview and apply roots differ";
      let paid = run (Funding.read store ~id:measured.request_id) |> get |> Option.get in
      if paid.paid <> Some (200L, (List.hd bills).root) then fail "payment durable root differs";
      if (Ledger.find ledger Funding.escrow).balance <> next_fund.amount then fail "payment escrow differs";
      let account = Ledger.find ledger wallet in
      if account.balance <> Z.sub initial (Z.add next_fund.amount (Z.add next_fund.ou (Z.add fund.ou pay.ou)))
          || account.nonce <> 3 then
        fail "payment native balances differ";
      let repeated = signed {pay with nonce = 4} in
      (match run (Epoch.apply_compute_effect ~backend ~env:late repeated) with
      | Ok (Epoch.Rejected_after_fee rejected) when Z.equal rejected.fee repeated.ou -> ()
      | _ -> fail "payment repeated proof not charged");
      let after = Ledger.find ledger wallet in
      if after.balance <> Z.sub account.balance repeated.ou || after.nonce <> 4
          || (Ledger.find ledger Funding.escrow).balance <> next_fund.amount then
        fail "payment repeated proof changed reserve";
      let returned = signed {next_fund with op_type = ComputeReturn; amount = Z.zero;
        nonce = 5; ou = Z.of_int 1000;
        message = Some Digestif.SHA256.(to_hex (of_raw_string (Cert.request_id next.request)))} in
      let expired = Epoch.{late with epoch_id = 501; epoch_ts = 501.} in
      if run (Epoch.apply_compute_effect ~backend ~env:expired returned) <> Ok (Epoch.Confirmed returned.ou) then
        fail "payment unused continuation refund failed";
      if run (Store.read_tree store (Octra_core.Compute_input.input_path (Cert.request_id next.request))) <> None then
        fail "payment refund retained input";
      if Result.is_error (run (Octra_core.Compute_input.read store ~header)) then
        fail "payment refund removed another request input";
      if run (Store.read_tree store (Octra_core.Compute_input.session_path request)) = None then
        fail "payment refund removed shared model";
      if (Ledger.find ledger Funding.escrow).balance <> Z.zero then fail "payment refund kept reserve";
      run (Store.commit_epoch_batch store "compute payment");
      Ledger.commit_journal ledger |> get;
      let refused raw = match run (compute ~store ~epoch:200L ~funded raw) with Error (Bill.Rejected _) -> ()
        | _ -> fail "payment invalid record accepted" in
      refused (Bill.encode_request ~fuel:99999 [Record.encode record]);
      refused (Bill.encode_request ~fuel:100000 [Record.encode {record with results = []}]);
      refused (Bill.encode_request ~fuel:100000 [Record.encode {record with
        call = {call with signature = String.make 64 '\000'}}]);
      let malformed = Bill.encode_request ~fuel:100000 [Record.encode {record with
        call = {call with signature = String.make 64 '\000'}}] in
      if (match verify ~lookup:(fun _ -> fail "malformed payment reached history") malformed with
          Error (Bill.Rejected _) -> false | _ -> true) then fail "malformed payment was not rejected";
      refused (Bill.encode_request ~fuel:100000 [Record.encode {record with validators = None}]);
      if (match verify ~funded:(fun _ -> Lwt.return_ok (Bill.Paid 8L)) raw with
          Error (Bill.Rejected _) -> false | _ -> true) then fail "paid replay ran";
      if (match verify ~lookup:(fun _ -> Lwt.return_error "missing") raw with
          Error (Bill.Unavailable _) -> false | _ -> true) then fail "missing history accepted";
      let program_at ~epoch_id:_ ~state_root:_ ~circle_id = Lwt.return_ok Engine.{circle_id;
        code_b64 = Base64.encode_exn code; code_hash = Digestif.SHA256.(to_hex (digest_string code));
        runtime = "wasm_v1"} in
      let engine _ = deps, program_at,
        (fun ~cancel:_ _ -> Lwt.return_error (Octra_core.Circle_wasm_host.Unavailable "local pool")) in
      if (match verify ~engine raw with Error (Bill.Unavailable _) -> false | _ -> true) then
        fail "local failure became payment refusal";
      let engine _ = deps, program_at,
        (fun ~cancel:_ _ -> Lwt.return_error (Octra_core.Circle_wasm_host.Rejected "model hash")) in
      if (match verify ~engine raw with Error (Bill.Rejected _) -> false | _ -> true) then
        fail "model failure became local failure")))

let test_cache_stream () =
  let body = String.init 37 (fun index -> Char.chr index) in
  let sizes = [8; 1; 12; 16] in
  let segments = [3; 5; 2; 7; 20] in
  List.iter (fun mode ->
    let graph, values, fetch, read = cache_sample body sizes segments mode in
    let rows = ref [] in
    let sealed = ref 0 in
    let closed = ref 0 in
    let writer = Cache.{
      put = (fun ~key ~value -> rows := (key, value) :: !rows; Lwt.return (Ok ()));
      seal = (fun ~root:_ ~entries:_ ~bytes:_ -> incr sealed; Lwt.return (Ok ()));
      close = (fun () -> incr closed);
    } in
    let sink = Cache.{
      status = (fun _ -> Lwt.return (Ok Missing));
      begin_cache = (fun _ -> Lwt.return (Ok writer));
      drop = (fun _ -> fail "cache stream dropped an unowned key");
    } in
    let result = Lwt_main.run (read fetch sink) in
    if !closed <> 1 then fail "stream writer close differs";
    if mode = 0 then begin
      if Result.is_error result || !sealed <> 1 then fail "stream load failed";
      List.iteri (fun index (_, value) ->
        let key = Printf.sprintf "w:weights:%d" index in
        if List.assoc key !rows <> value then fail "stream chunk bytes differ") values
    end else begin
      let reason = if mode = 1 then "resource shard differs" else "resource chunk differs" in
      if result <> Error reason || !sealed <> 0 then fail "invalid stream published";
      let plan = Graph.cache_plan ~limits:Graph.default_limits
        ~state_root:request.model_state_root graph |> Result.get_ok in
      let result = Lwt_main.run (read fetch Cache.native_sink) in
      if result <> Error reason then fail "native stream accepted invalid hash";
      if Lwt_main.run (Cache.native_sink.status plan.key) <> Ok Cache.Missing then
        fail "invalid stream exposed native image"
    end;
    if mode = 0 then begin
      List.iter (fun throwing ->
        let closed = ref 0 in
        let writer = {writer with Cache.put = (fun ~key ~value ->
          if key = "w:weights:1" then
            if throwing then Lwt.fail (Failure "stream put failed")
            else Lwt.return (Error "stream put failed")
          else writer.put ~key ~value);
          close = (fun () -> incr closed)} in
        let sink = {sink with Cache.begin_cache = (fun _ -> Lwt.return (Ok writer))} in
        if throwing then begin
          try ignore (Lwt_main.run (read fetch sink)); fail "stream exception ignored"
          with Failure reason when reason = "stream put failed" -> ()
        end else if Lwt_main.run (read fetch sink) <> Error "stream put failed" then
          fail "stream put refusal ignored";
        if !closed <> 1 || !sealed <> 1 then fail "failed stream sealed or close differs")
        [false; true];
      let cancel, stop = Lwt.wait () in
      let cancelled_fetch snapshot ~max_bytes ~circle_id ~path =
        if path = "/model/data/segment-2" then begin
          Lwt.wakeup_later stop "stream cancelled";
          fst (Lwt.task ())
        end else fetch snapshot ~max_bytes ~circle_id ~path in
      let plan = Graph.cache_plan ~limits:Graph.default_limits
        ~state_root:request.model_state_root graph |> Result.get_ok in
      if Lwt_main.run (read ~cancel cancelled_fetch Cache.native_sink) <> Error "stream cancelled" then
        fail "stream cancellation ignored";
      if Lwt_main.run (Cache.native_sink.status plan.key) <> Ok Cache.Missing then
        fail "cancelled stream exposed native image";
      List.iter (fun mode ->
        let fetch snapshot ~max_bytes ~circle_id ~path =
          let open Lwt.Syntax in
          let* result = fetch snapshot ~max_bytes ~circle_id ~path in
          if path <> "/model/data/segment-4" then Lwt.return result else
          match result with
          | Error _ as error -> Lwt.return error
          | Ok asset ->
            let asset = match mode with
              | 0 -> {asset with Cache.state_root = hash 'a'}
              | 1 -> {asset with Cache.blob_hash = hash 'a'}
              | _ ->
                let body = String.sub asset.body 0 (String.length asset.body - 1) in
                {asset with Cache.body; size_bytes = String.length body;
                  blob_hash = Digestif.SHA256.(to_hex (digest_string body))} in
            Lwt.return (Ok asset) in
        let reason = match mode with
          | 0 -> "circle asset state root differs"
          | 1 -> "circle asset body hash differs"
          | _ -> "resource segment differs" in
        if Lwt_main.run (read fetch Cache.native_sink) <> Error reason then
          fail "changed stream asset accepted";
        if Lwt_main.run (Cache.native_sink.status plan.key) <> Ok Cache.Missing then
          fail "changed stream asset published") [0; 1; 2];
      if Result.is_error (Lwt_main.run (read fetch Cache.native_sink)) then
        fail "native stream load failed";
      begin match Lwt_main.run (Cache.native_sink.status plan.key) with
      | Ok (Cache.Ready ready) when ready.root = plan.root
          && ready.entries = List.length plan.entries
          && Int64.of_int ready.bytes = plan.bytes -> ()
      | _ -> fail "native stream image differs"
      end;
      if Lwt_main.run (Cache.native_sink.drop plan.key) <> Ok () then
        fail "native stream image not released"
    end) [0; 1; 2]

let test_asset_decode () =
  let response size body = `Assoc [
    "blob_hash", `String Digestif.SHA256.(to_hex (digest_string body));
    "body_b64", `String (Base64.encode_exn body);
    "browser_mode", `String "gateway_allowed";
    "canonical_path", `String "/weights/data";
    "circle_id", `String address;
    "content_type", `String "application/octet-stream";
    "encoding", `String "identity";
    "path_key", `String "data";
    "resource_key", `String "data";
    "resource_mode", `String "public_resources";
    "size_bytes", `String (string_of_int size);
  ] in
  let read max_bytes size body = Cache.asset_of_rpc_json
    ~max_bytes ~state_root:request.model_state_root (response size body) in
  for size = 0 to 32 do
    let body = String.init size (fun index -> Char.chr (index * 7)) in
    begin match read size size body with
    | Ok asset when asset.body = body && asset.size_bytes = size -> ()
    | _ -> fail "exact asset limit refused"
    end;
    if read (size - 1) size body <> Error "circle asset size exceeds limit" then
      fail "asset size limit ignored"
  done;
  if read max_int max_int "x" <> Error "circle asset size differs" then
    fail "encoded limit overflow";
  if read 2 1 "xx" <> Error "circle asset size differs" then
    fail "decoded size not checked";
  let json = response 1 (String.make (1024 * 1024) 'x') in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let result = Cache.asset_of_rpc_json ~max_bytes:1
    ~state_root:request.model_state_root json in
  let allocated = Gc.allocated_bytes () -. before in
  if result <> Error "circle asset encoded size exceeds limit" then
    fail "oversized asset reached decode";
  if allocated > 16_384. then fail "oversized asset allocated decoded bytes"

let test_asset_store () =
  Test_workspace.with_dir "compute-asset" (fun directory ->
    let store = Lwt_main.run (Store.open_store ~fresh:true directory) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store)) (fun () ->
      Lwt_main.run (seed_compute store program_raw "first");
      let snapshot, fetch = Lwt_main.run (Cache.store_source store) |> Result.get_ok in
      let read max_bytes = fetch snapshot ~max_bytes ~circle_id:address ~path:"/weights/data" in
      begin match Lwt_main.run (read 5) with
      | Ok asset when asset.body = "first" -> ()
      | _ -> fail "stored asset exact limit refused"
      end;
      List.iter (fun size -> refused "circle asset size exceeds limit" (read size)) [-1; 0; 4];
      let stored = Lwt_main.run (Store.get_circle_public_asset_at ~max_bytes:5
        (Lwt_main.run (Store.capture_read_snapshot store) |> Result.get_ok)
        address "/weights/data") |> Result.get_ok in
      let path, path_key = Circle.path_key_of_raw_path "/weights/large" |> Result.get_ok in
      let meta = {stored.Store.meta with canonical_path = path; path_key; size_bytes = 1_048_576L} in
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Store.begin_epoch_batch store in
        let* () = Store.save_circle_asset_meta store address meta in
        Store.commit_epoch_batch store "compute asset limit");
      let next, limited = Lwt_main.run (Cache.store_source store) |> Result.get_ok in
      refused "circle asset size exceeds limit"
        (limited next ~max_bytes:5 ~circle_id:address ~path);
      refused "circle asset missing"
        (limited next ~max_bytes:1_048_576 ~circle_id:address ~path);
      let body = Base64.encode_exn (String.make (1024 * 1024) 'x') in
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Store.begin_epoch_batch store in
        let* () = Store.save_circle_asset_body_b64 store address stored.meta.path_key body in
        Store.commit_epoch_batch store "compute asset length");
      let next, limited = Lwt_main.run (Cache.store_source store) |> Result.get_ok in
      refused "circle asset encoded size exceeds limit"
        (limited next ~max_bytes:5 ~circle_id:address ~path:"/weights/data");
      begin match Lwt_main.run (read 5) with
      | Ok asset when asset.body = "first" -> ()
      | _ -> fail "limited asset lost pinned root"
      end;
      let invalid key = match Circle.yojson_of_asset_meta stored.meta with
        | `Assoc fields -> Yojson.Safe.to_string
            (`Assoc ((key, `String "not-an-integer") :: List.remove_assoc key fields))
        | _ -> fail "asset metadata shape differs" in
      List.iter (fun (raw, reason) ->
        Lwt_main.run (let open Lwt.Syntax in
          let* () = Store.begin_epoch_batch store in
          let* () = Store.write store
            ["circles"; address; "assets"; "by_hash"; stored.meta.path_key; "meta"] raw in
          Store.commit_epoch_batch store "compute asset metadata");
        let current, fetch = Lwt_main.run (Cache.store_source store) |> Result.get_ok in
        refused reason (fetch current ~max_bytes:5 ~circle_id:address ~path:"/weights/data")) [
          "{", "circle asset metadata JSON refused";
          invalid "size_bytes", "circle asset metadata integer refused";
          invalid "activate_after_epoch", "circle asset metadata integer refused";
        ]))

let test_cache_limits () =
  let graph, _, fetch, read = cache_sample "abcdef" [3; 3] [2; 4] 0 in
  let snapshot = Cache.{epoch_id = request.model_epoch; state_root = request.model_state_root} in
  let raw = Lwt_main.run (fetch snapshot ~max_bytes:max_int
    ~circle_id:address ~path:"/model/resource-graph.json") |> Result.get_ok in
  let limit = String.length raw.body in
  let limits = {Graph.default_limits with max_graph_bytes = limit} in
  let plan = Graph.cache_plan ~limits ~state_root:snapshot.state_root graph |> Result.get_ok in
  let run changed =
    let writes = ref 0 in
    let seals = ref 0 in
    let closed = ref 0 in
    let begun = ref 0 in
    let writer = Cache.{
      put = (fun ~key ~value:_ ->
        if String.starts_with ~prefix:"w:" key then incr writes;
        Lwt.return (Ok ()));
      seal = (fun ~root:_ ~entries:_ ~bytes:_ -> incr seals; Lwt.return (Ok ()));
      close = (fun () -> incr closed);
    } in
    let sink = Cache.{
      status = (fun _ -> Lwt.return (Ok Missing));
      begin_cache = (fun _ -> incr begun; Lwt.return (Ok writer));
      drop = (fun _ -> fail "limited fetch dropped shared data");
    } in
    let result = Lwt_main.run (read ~limits changed sink) in
    if !closed <> !begun then fail "limited fetch left writer open";
    result, !writes, !seals, !begun in
  let checked snapshot ~max_bytes ~circle_id ~path =
    let expected = match path with
      | "/model/data/segment-0" -> 2
      | "/model/data/segment-1" -> 4
      | _ -> limit in
    if max_bytes <> expected then fail "asset fetch limit not propagated";
    fetch snapshot ~max_bytes ~circle_id ~path in
  begin match run checked with
  | Ok _, 2, 1, 1 -> ()
  | _ -> fail "limited graph load refused"
  end;
  List.iter (fun (path, announced) ->
    let changed snapshot ~max_bytes ~circle_id ~path:actual =
      let open Lwt.Syntax in
      let* result = checked snapshot ~max_bytes ~circle_id ~path:actual in
      Lwt.return (Result.map (fun (asset : Cache.asset) ->
        if actual <> path then asset else
        let body = if announced then asset.body else String.make (max_bytes + 1) ' ' in
        {asset with body; size_bytes = max_bytes + 1}) result) in
    let result, writes, seals, begun = run changed in
    if result <> Error "circle asset size exceeds limit" || writes <> 0 || seals <> 0 then
      fail "oversized model input was consumed";
    if path = "/model/resource-graph.json" && begun <> 0 then
      fail "oversized graph opened writer";
    if Lwt_main.run (read ~limits changed Cache.native_sink)
       <> Error "circle asset size exceeds limit" then fail "native size limit differs";
    if Lwt_main.run (Cache.native_sink.status plan.key) <> Ok Cache.Missing then
      fail "oversized model published native cache") [
        "/model/resource-graph.json", false;
        "/model/shard.json", false;
        "/model/data/segment-0", false;
        "/model/data/segment-0", true;
      ];
  let changed snapshot ~max_bytes ~circle_id ~path =
    let open Lwt.Syntax in
    let* result = checked snapshot ~max_bytes ~circle_id ~path in
    Lwt.return (Result.map (fun (asset : Cache.asset) ->
      if path <> "/model/resource-graph.json" then asset else
      {asset with size_bytes = 0; body = String.make (max_bytes + 1) ' '}) result) in
  if let result, _, _, _ = run changed in result <> Error "circle asset size exceeds limit"
  then fail "understated model size accepted"

let test_cache_alloc () =
  let size = 32 * 1024 * 1024 in
  let body = String.init size (fun index -> Char.chr (index mod 251)) in
  let _, values, fetch, read = cache_sample body
    (List.init 32 (fun _ -> 1024 * 1024))
    (List.init 8 (fun _ -> 4 * 1024 * 1024)) 0 in
  let written = ref 0 in
  let writer = Cache.{
    put = (fun ~key ~value ->
      if String.starts_with ~prefix:"w:" key then begin
        let _, expected = List.nth values !written in
        if value <> expected then fail "large stream chunk differs";
        incr written
      end;
      Lwt.return (Ok ()));
    seal = (fun ~root:_ ~entries:_ ~bytes:_ -> Lwt.return (Ok ()));
    close = (fun () -> ());
  } in
  let sink = Cache.{
    status = (fun _ -> Lwt.return (Ok Missing));
    begin_cache = (fun _ -> Lwt.return (Ok writer));
    drop = (fun _ -> fail "large stream dropped key");
  } in
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  let result = Lwt_main.run (read fetch sink) in
  let allocated = Gc.allocated_bytes () -. before in
  if Result.is_error result || !written <> 32 then fail "large stream did not complete";
  let limit = float_of_int (size * 5 / 2) in
  Printf.printf "event = model_load bytes = %d allocated = %.0f limit = %.0f\n%!"
    size allocated limit;
  if allocated > limit then fail "stream allocated a full shard"

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--graph" then begin
    test_cache_roots ();
    print_endline "status = pass test = graph_stack";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--payment" then begin
    test_native_fuel ();
    test_payment_flow ();
    print_endline "status = pass test = compute_payment";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--cache" then begin
    test_asset_decode ();
    test_asset_store ();
    test_cache_limits ();
    test_cache_roots ();
    test_cache_stream ();
    test_cache_alloc ();
    print_endline "status = pass test = model_cache";
    exit 0
  end;
  test_model_owner ();
  test_model_handle ();
  test_build_handle ();
  test_prepare_hold ();
  test_model_eviction ();
  test_two_active_models ();
  test_model_cancel ();
  test_model_failure ();
  test_model_timeout ();
  test_model_pressure ();
  test_model_retiring ();
  test_prepare_failure ();
  test_model_cleanup ();
  test_model_read_cancel ();
  test_model_queue_cancel ();
  test_model_read_stop ();
  test_prepare_read_stop ();
  test_asset_decode ();
  test_asset_store ();
  test_cache_limits ();
  test_cache_roots ();
  test_cache_stream ();
  test_cache_alloc ();
  test_storage_big_key ();
  test_storage_index ();
  test_storage_json ();
  test_storage_inputs ();
  test_storage_invalid ();
  test_storage_exact ();
  test_storage_bytes ();
  test_storage_count ();
  test_snapshot_matches ();
  test_native_root ();
  test_native_accelerator ();
  test_prepare_and_execute ();
  test_unprepared ();
  test_fuel_limit ();
  test_guest_limit ();
  test_native_fuel ();
  test_payment_flow ();
  test_wrong_executor ();
  test_executor_version ();
  test_snapshot_alias ();
  print_endline "status = pass test = resource_provider"