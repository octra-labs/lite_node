(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Vm = Octra_vm
module Circles = Octra_core.Circles
module Store = Octra_core.Store_irmin
module Circle_program = Octra_circle_runtime.Circle_program
module Circle_exec = Octra_circle_runtime.Circle_exec

let fail msg =
  failwith ("test_circle_program: " ^ msg)

let require condition msg =
  if not condition then fail msg

let rec rm_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> rm_tree (Filename.concat path name));
      Unix.rmdir path
    end else
      Sys.remove path

let rec mkdir_p path =
  if not (Sys.file_exists path) then begin
    let parent = Filename.dirname path in
    if not (String.equal parent path) then mkdir_p parent;
    Unix.mkdir path 0o755
  end

let with_store f =
  let root =
    Filename.concat
      (Sys.getcwd ())
      ("runtime_data/circle_program_" ^ string_of_int (Unix.getpid ()))
  in
  rm_tree root;
  mkdir_p root;
  let store = Lwt_main.run (Store.open_store (Filename.concat root "irmin")) in
  Fun.protect
    ~finally:(fun () ->
      ignore (Lwt_main.run (Store.close store));
      rm_tree root)
    (fun () -> f store)

let key private_key =
  match Mirage_crypto_ec.Ed25519.priv_of_octets private_key with
  | Error _ -> fail "private key rejected"
  | Ok private_key ->
    {
      Vm.Program_attestation.id = "circle-test";
      public_key = Mirage_crypto_ec.Ed25519.pub_to_octets
        (Mirage_crypto_ec.Ed25519.pub_of_priv private_key);
    }

let compile_signed () =
  let result =
    Vm.Oct_compile.compile_program
      "program CircleTyped { fn echo(x: int): int { return x } view fn complete_preview(prompt: string, count: int): string { return \"2\" } }" in
  let private_key = String.make 32 '\042' in
  let trusted_key = key private_key in
  let certificate =
    match Vm.Oct_compile.attest_program
        ~key_id:trusted_key.id
        ~private_key
        result with
    | { program_envelope = Some value; error = None; _ } -> value
    | _ -> fail "Program attestation failed"
  in
  certificate, trusted_key

let save_circle ?(runtime = Circles.Octb) store circle_id owner code_b64 =
  let raw = Base64.decode_exn code_b64 in
  let info = {
    Circles.circle_id;
    runtime;
    version = 1L;
    owner;
    code_hash = Circles.sha256_hex raw;
    stable_root = Circles.zero_hash_hex;
    assets_root = Circles.zero_hash_hex;
    privacy_class = Circles.Public;
    browser_mode = Circles.Native_sealed;
    resource_mode = Circles.Public_resources;
    policy_hash = None;
    members_root = None;
    export_policy = None;
    limits = Circles.default_limits;
  } in
  Lwt_main.run (Store.deploy_circle store info);
  Lwt_main.run (Store.save_circle_program_code_b64 store circle_id code_b64);
  Lwt_main.run (Store.set_circle_asset_usage_bytes store circle_id 0L)

let test_inputs () =
  with_store (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let circle_id = "octCIRCLE_PROGRAM1" in
    let envelope, trusted_key = compile_signed () in
    let code_b64 = Base64.encode_exn envelope in
    save_circle store circle_id owner code_b64;
    begin
      match Lwt_main.run (Circle_program.load store circle_id) with
      | Ok _ -> fail "untrusted Circle Program accepted"
      | Error _ -> ()
    end;
    begin
      match Lwt_main.run (Circle_program.load ~trusted:[trusted_key] store circle_id) with
      | Ok { Circle_program.code = Circle_program.Octb { profile = Vm.Admission.Program _; _ }; _ } ->
        ()
      | Ok _ -> fail "Circle Program profile missing"
      | Error _ -> fail "trusted Circle Program rejected"
    end;
    let bad =
      Lwt_main.run
        (Circle_exec.execute_call
           ~trusted:[trusted_key]
           store
           circle_id
           "echo"
           [`String "7"]
           owner
           Z.zero)
    in
    require (not bad.receipt.success) "strict Circle argument accepted";
    let bad_view =
      Lwt_main.run
        (Circle_exec.execute_view_call
           ~trusted:[trusted_key]
           store
           circle_id
           "echo"
           [`String "7"]
           owner)
    in
    require (not bad_view.success) "strict Circle view argument accepted";
    let good =
      Lwt_main.run
        (Circle_exec.execute_call
           ~trusted:[trusted_key]
           store
           circle_id
           "echo"
           [`Int 7]
           owner
           Z.zero)
    in
    require good.receipt.success "valid Circle Program call rejected";
    match good.receipt.return_value with
    | Some (Vm.Contract_vm.VInt value) ->
      require (Z.equal value (Z.of_int 7))
        ("Circle Program result mismatch: " ^ Z.to_string value)
    | _ -> fail "Circle Program result type mismatch")

let test_view_stop () =
  with_store (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let circle_id = "octCIRCLE_VIEW1" in
    let compiled = Vm.Oct_compile.compile
      "contract ViewCase { view fn echo(): int { return 7 } }" in
    require (compiled.error = None) "view compile failed";
    save_circle store circle_id owner (Base64.encode_exn compiled.bytecode);
    let steps = ref 0 in
    let execute running =
      Lwt_main.run
        (Circle_exec.execute_view_call ~running store circle_id "echo" [] owner)
    in
    let direct = execute (fun () -> incr steps; true) in
    require (direct.success && !steps > 1) "view control did not execute";
    steps := 0;
    let stopped = execute (fun () -> incr steps; !steps <= 1) in
    require (not stopped.success && !steps = 2) "view stop not propagated")

let test_owner () =
  with_store (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let circle_id = "octCIRCLE_OWNER" in
    let compiled = Vm.Oct_compile.compile
      "contract OwnerCase { fn echo(): int { return balance(caller) } }" in
    require (compiled.error = None) "owner compile failed";
    save_circle store circle_id owner (Base64.encode_exn compiled.bytecode);
    let thread = Thread.id (Thread.self ()) in
    let threads = ref [] in
    let ctx = {Vm.Contract_vm.default_ctx with
      get_balance = (fun _ -> threads := Thread.id (Thread.self ()) :: !threads; Z.of_int 7);
    } in
    let update = Lwt_main.run
      (Circle_exec.execute_call ~ctx store circle_id "echo" [] owner Z.zero) in
    require update.receipt.success "owner update failed";
    let view = Lwt_main.run
      (Circle_exec.execute_view_call ~ctx store circle_id "echo" [] owner) in
    require view.success "owner view failed";
    require (!threads = [thread; thread]) "circle state left owner";
    let ctx = {ctx with get_balance = (fun _ ->
      raise (Octra_core.Exec_resource.Unavailable Host))} in
    let check work =
      match Lwt_main.run work with
      | _ -> fail "host failure became a receipt"
      | exception Octra_core.Exec_resource.Unavailable Host -> ()
    in
    check (Circle_exec.execute_call ~ctx store circle_id "echo" [] owner Z.zero);
    check (Circle_exec.execute_view_call ~ctx store circle_id "echo" [] owner))

let test_async_policy () =
  let module VM = Vm.Contract_vm in
  let reached = ref 0 in
  List.iter (fun mode ->
    let module R = Octra_core.Rule_graph in
    let storage = Hashtbl.create 1 in
    let ctx = {VM.default_ctx with proof_exec = mode; async_exec = true;
      call_async = (fun _ _ _ _ _ ->
        incr reached;
        Lwt.return (Ok VM.{return_value = VInt Z.one; effort_used = 0; events = []}))} in
    let ctx = Circle_exec.check_call_storage (Hashtbl.copy storage) storage ctx in
    let state = VM.create_state ~ctx ~caller:"caller" ~origin:"caller"
      ~address:"circle" ~value:Z.zero ~storage () in
    let code = [|VM.LDI (1, VString "any_registered");
      VM.SSTORE ("hfhe_policy:pedersen_mode", 1);
      VM.LDI (1, VString "target"); VM.LDI (2, VString "echo");
      VM.XCALL (0, 1, 2, 3, 0); VM.STOP|] in
    reached := 0;
    let success = Lwt_main.run (VM.run_async state code) in
    require (success = (mode = R.Prior) && !reached = (if mode = R.Prior then 1 else 0))
      "circle transient policy reached child") [Octra_core.Rule_graph.Prior; Active];
  reached := 0;
  let call _ _ _ _ _ =
    incr reached;
    Ok VM.{return_value = VInt Z.one; effort_used = 0; events = []} in
  let deploy _ _ _ _ _ =
    incr reached;
    Ok VM.{spawned_addr = "unexpected"; effort_used = 0; events = []} in
  let ctx = {VM.default_ctx with
    call_contract = call;
    call_async = (fun a b c d e -> Lwt.return (call a b c d e));
    deploy_contract = deploy;
    deploy_async = (fun a b c d e -> Lwt.return (deploy a b c d e));
  } |> Circle_exec.restrict_runtime_exec_ctx in
  let create ctx = VM.create_state ~ctx ~caller:"caller" ~origin:"caller"
    ~address:"circle" ~value:Z.zero ~storage:(Hashtbl.create 0) () in
  List.iter (fun async_exec ->
    let ctx = {ctx with VM.async_exec} in
    List.iter (fun code ->
      let state = create ctx in
      let success = if async_exec then Lwt_main.run (VM.run_async state code)
        else VM.run state code in
      require (not success && state.reverted && !reached = 0)
        "circle inherited external effects") [
        [|VM.LDI (1, VString "target"); VM.LDI (2, VString "echo");
          VM.XCALL (0, 1, 2, 3, 0); VM.STOP|];
        [|VM.LDI (1, VString "OCTB12345678"); VM.SPAWN (0, 1); VM.STOP|];
      ]) [false; true];
  let payload = Circles.{runtime = Octb; privacy_class = Public;
    browser_mode = Native_sealed; resource_mode = Public_resources;
    code_b64 = None; policy_hash = None; members_root = None; export_policy = None;
    limits = default_limits;
  } |> Circles.yojson_of_deploy_payload |> Yojson.Safe.to_string in
  List.iter (fun phase -> List.iter (fun kind ->
    let spawns = ref [] in
    let ctx = Circle_exec.with_circle_spawn
      {ctx with proof_exec = Octra_core.Rule_graph.Active} "circle" "caller" spawns in
    let state = create {ctx with VM.async_exec = true} in
    let code = [|VM.LDI (1, VString (Base64.encode_exn payload));
      VM.LDI (2, VString "caller"); VM.SPAWN2 (0, 1, 2, 1); VM.STOP|] in
    let previous = List.map (fun name -> name, Sys.getenv_opt name)
      ["OCTRA_CHAOS_FAIL_AT"; "OCTRA_CHAOS_FAIL_KIND"] in
    let refused = Fun.protect
      ~finally:(fun () -> List.iter (fun (name, value) ->
        Unix.putenv name (Option.value ~default:"" value)) previous)
      (fun () ->
        Unix.putenv "OCTRA_CHAOS_FAIL_AT" phase;
        Unix.putenv "OCTRA_CHAOS_FAIL_KIND" kind;
        try ignore (Lwt_main.run (VM.run_async state code)); false with
        | Octra_core.Exec_resource.Unavailable _ -> kind <> "cancel"
        | Lwt.Canceled -> kind = "cancel") in
    require (refused && !spawns = []) "circle spawn converted resource fault")
    ["host"; "memory"; "stack"; "cancel"])
    ["circle_payload"; "circle_prepare"];
  let execute async_exec =
    let spawns = ref [] in
    let ctx = Circle_exec.with_circle_spawn ctx "circle" "caller" spawns in
    let state = create {ctx with VM.async_exec} in
    let code = [|VM.LDI (1, VString (Base64.encode_exn payload));
      VM.LDI (2, VString "caller"); VM.SPAWN2 (0, 1, 2, 1); VM.STOP|] in
    let run () = if async_exec then Lwt_main.run (VM.run_async state code)
      else VM.run state code in
    require (run ()) "circle spawn failed";
    let first = state.regs.(0) in
    require (List.length !spawns = 1 && !reached = 0)
      "circle spawn reached external handler";
    for _ = 2 to Circle_exec.spawn_cap do
      state.pc <- 0;
      require (run ()) "circle spawn quota ended early"
    done;
    state.pc <- 0;
    require (not (run ()) && List.length !spawns = Circle_exec.spawn_cap)
      "circle spawn quota exceeded";
    first, !spawns, state.effort_used
  in
  require (execute false = execute true) "circle spawn execution differs"

let test_wasm_keys () =
  let module P = Pvac_ffi in
  let module VM = Vm.Contract_vm in
  let module Policy = Octra_core.Circle_hfhe_policy in
  let module Memory = Vm.Fhe_memory in
  let open Lwt.Syntax in
  let key, secret = P.keygen_from_seed (P.default_params ()) (Bytes.make 32 '\031') in
  let raw = P.serialize_pubkey key |> Bytes.to_string in
  let public = Base64.encode_exn raw in
  let secret = P.serialize_seckey secret |> Bytes.to_string in
  let private_key = Base64.encode_exn secret in
  let key = Octra_core.Fhe_image.of_key key in
  let owner = "oct" ^ String.make 44 '1' in
  let caller = "oct" ^ String.make 44 '2' in
  let absent = "oct" ^ String.make 44 '3' in
  let thread = Thread.id (Thread.self ()) in
  let reads = ref [] in
  let ctx = {VM.default_ctx with
    circle_hfhe_key_id = Some "key";
    allow_fhe_capability = (fun _ -> true);
    get_fhe_pubkey = (fun address ->
      require (Thread.id (Thread.self ()) = thread) "key read left owner";
      reads := !reads @ [address];
      match address with
      | value when value = owner -> Some (VM.Key_value key)
      | value when value = caller -> Some (VM.Key_bytes raw)
      | _ -> None);
    get_fhe_keypair = (fun id ->
      require (Thread.id (Thread.self ()) = thread && id = "key") "key scope differs";
      Some (key, secret));
  } in
  let runtime ctx = Circle_exec.{
    exec_ctx = ctx;
    calls = false;
    policy = {Policy.default with load_pk_mode = Any_registered;
      pk_allowlist = Some [owner; caller; owner; absent]};
    owner;
    active_relay = None;
  } in
  let prepare limit view ctx =
    Circle_exec.wasm_keys ~limit ~view ~circle_id:"circle" (runtime ctx) caller in
  let execute view ctx =
    let* result = prepare 20_000_000 view ctx in
    match result with
    | Ok (public, active, _) -> Lwt.return (public, active)
    | Error error -> fail error in
  let expected = [owner, public; caller, public], Some ("key", public, private_key) in
  let price bytes = (bytes + 15) / 16 in
  let size = Octra_core.Fhe_image.key_size key in
  let cost = 4 * price size + price (String.length raw + size) + price (String.length secret) in
  List.iter (fun view ->
    let memory = Memory.create () in
    let active = {ctx with proof_exec = Octra_core.Rule_graph.Active; fhe_memory = Some memory} in
    let refused = prepare (cost - 1) view active in
    require (Lwt.state refused = Lwt.Return (Error "wasm key effort exceeds limit"))
      "wasm key effort checked after worker dispatch";
    require (Z.equal (Memory.used memory) Z.zero) "refused key effort reserved memory";
    match Lwt_main.run (prepare cost view active) with
    | Ok (public, active, used) ->
      require ((public, active) = expected && used = cost) "wasm key effort or result differs"
    | Error error -> fail error) [false; true];
  require (match Lwt_main.run (prepare 0 false ctx) with
    | Ok (public, active, 0) -> (public, active) = expected
    | _ -> false) "historical wasm key preparation changed";
  List.iter (fun view ->
    reads := [];
    let memory = Memory.create () in
    let pulse = ref 0 in
    let rec heartbeat () =
      let* () = Lwt_unix.sleep 0.001 in
      incr pulse;
      heartbeat () in
    let run () =
      let timer = heartbeat () in
      Lwt.finalize (fun () ->
        let pending = execute view {ctx with fhe_memory = Some memory} in
        require (Lwt.is_sleeping pending) "wasm key preparation did not suspend";
        require (!reads = [owner; caller; absent]) "wasm keys not captured before wait";
        let* result = pending in
        require (!pulse > 0) "wasm keys blocked event loop";
        require (result = expected) "wasm key bytes differ";
        require (!reads = [owner; caller; absent]) "wasm key order differs";
        require (Z.equal (Memory.used memory)
          (Z.add (Memory.key_value key) (Option.get (Memory.key_decode raw))))
          "wasm key memory differs";
        Lwt.return_unit)
        (fun () -> Lwt.cancel timer; Lwt.return_unit) in
    Lwt_main.run (run ())) [false; true];
  let denied = {ctx with allow_fhe_capability = (fun _ -> false);
    get_fhe_pubkey = (fun _ -> fail "denied public key read");
    get_fhe_keypair = (fun _ -> fail "denied secret key read")} in
  require (Lwt_main.run (execute false denied) = ([], None)) "denied key exported";
  let empty = {ctx with get_fhe_keypair = (fun _ -> None);
    get_fhe_pubkey = (fun _ -> Some (VM.Key_bytes "invalid"))} in
  require (Lwt_main.run (execute false empty) = ([], None)) "invalid key exported";
  let memory = Memory.create () in
  require (Memory.reserve memory Memory.limit) "memory setup failed";
  require (Lwt_main.run (execute false {ctx with circle_hfhe_key_id = None;
    fhe_memory = Some memory}) = ([], None)) "memory refusal ignored";
  Lwt_main.run (
    reads := [];
    let pending = execute false ctx in
    require (Lwt.is_sleeping pending) "wasm key cancellation did not suspend";
    Lwt.cancel pending;
    reads := [];
    let fresh_ctx = {ctx with tree_hash = "next"; tx_hash = "next"} in
    let* fresh = execute false fresh_ctx in
    require (fresh = expected) "fresh key preparation failed";
    require (!reads = [owner; caller; absent]) "cancelled key read continued";
    require (Lwt.state pending = Lwt.Fail Lwt.Canceled) "cancelled key preparation returned";
    Lwt.return_unit);
  Lwt_main.run (
    let deadline = Int64.add (Mtime_clock.elapsed_ns ()) 30_000_000_000L in
    let jobs = List.init (Vm.Fhe_queue.capacity + 1) (fun index ->
      let ticket = Vm.Proof_wait.{request = string_of_int index; generation = "circle_keys"} in
      Vm.Fhe_task.run ~ticket ~deadline
        (Vm.Fhe_task.Verify (false, Octra_core.Pvac_verify_protocol.Ping))) in
    let* no_keys = execute false denied in
    require (no_keys = ([], None)) "empty key set consumed queue capacity";
    let* () = Lwt.catch (fun () ->
      let* _ = execute false ctx in
      Lwt.fail_with "busy key preparation accepted") (function
        | Octra_core.Exec_resource.Unavailable Host -> Lwt.return_unit
        | error -> Lwt.fail error) in
    let* values = Lwt.all jobs in
    require (List.for_all ((=) (Ok (Vm.Fhe_task.Verified true))) values)
      "key queue lost work";
    Lwt.return_unit)

let test_preview_trust () =
  with_store (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let circle_id = "octPREVIEW_TRUST" in
    let envelope, trusted_key = compile_signed () in
    save_circle store circle_id owner (Base64.encode_exn envelope);
    let read trusted prompt = Lwt_main.run
      (Circle_exec.execute_view_call ~trusted store circle_id
        "complete_preview" [`String prompt; `Int 1] owner) in
    let trusted = [trusted_key] in
    let direct = Lwt_main.run (Circle_exec.execute_view_call_direct ~trusted
      store circle_id "complete_preview" [`String "1"; `Int 1] owner) in
    require direct.success "direct signed preview failed";
    let first = read trusted "1" in
    require (first.success && first.return_value = direct.return_value)
      "signed preview failed";
    let cached = read trusted "1" in
    require (cached.success && cached.return_value = direct.return_value)
      "cached signed preview failed";
    let refused = read [] "1" in
    require (not refused.success) "preview cache accepted removed trust";
    let other = key (String.make 32 '\043') in
    let refused = read [other] "1" in
    require (not refused.success) "preview cache accepted a different key";
    let key = Circle_exec.preview_cache_key ~trusted ~math:false circle_id owner "1,2" in
    let rec drain () =
      if Hashtbl.mem Circle_exec.preview_session_inflight key then
        Lwt.bind (Lwt_unix.sleep 0.001) drain
      else Lwt.return_unit in
    Lwt_main.run (Lwt.pick [drain ();
      Lwt.bind (Lwt_unix.sleep 10.) (fun () -> Lwt.fail_with "preview prefetch timed out")]);
    require (Circle_exec.preview_cache_lookup key 1 = Some "2")
      "signed preview prefetch did not populate cache";
    let next = Lwt_main.run (Circle_exec.execute_view_call
      ~running:(fun () -> true) ~trusted store circle_id
      "complete_preview" [`String "1,2"; `Int 1] owner) in
    require (next.success && next.effort_used = 0 && next.return_value = direct.return_value)
      "signed preview continuation failed")

let method_info name view execution =
  `Assoc [
    "name", `String name;
    "view", `Bool view;
    "execution", `String execution;
  ]

let wasm_descriptor methods =
  {
    Octra_core.Circle_wasm_host.exports = [];
    manifest = Some (`Assoc ["methods", `List methods]);
  }

let read_module ?hfhe_request manifest response =
  let byte value = String.make 1 (Char.chr value) in
  let rec number limit value =
    if value < limit then byte value
    else byte ((value land 127) lor 128) ^ number limit (value lsr 7) in
  let size = number 128 in
  let text value = size (String.length value) ^ value in
  let vector values = size (List.length values) ^ String.concat "" values in
  let section tag value = byte tag ^ text value in
  let const value = "\065" ^ number 64 value in
  let export name kind index = text name ^ byte kind ^ size index in
  let body instructions = text ("\000" ^ instructions ^ "\011") in
  let data = manifest ^ response ^ Option.value ~default:"" hfhe_request in
  let imports = [text "octra" ^ text "host_response_write" ^ "\000\000"]
    @ (match hfhe_request with
      | None -> []
      | Some _ -> [text "octra" ^ text "host_hfhe_invoke_len" ^ "\000\000"]) in
  let first = List.length imports in
  let invoke = match hfhe_request with
    | None -> ""
    | Some request -> const (String.length manifest + String.length response)
      ^ const (String.length request) ^ "\016\001\026" in
  let write offset length = const offset ^ const length ^ "\016\000\026" ^ const 0 in
  String.concat "" [
    "\000asm\001\000\000\000";
    section 1 (vector ["\096\002\127\127\001\127"; "\096\001\127\001\127"]);
    section 2 (vector imports);
    section 3 (vector [size 1; size 0; size 0]);
    section 5 (vector ["\000\001"]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 first;
      export "octra_query" 0 (first + 1); export "octra_update" 0 (first + 1);
      export "octra_manifest" 0 (first + 2)]);
    section 10 (vector [body (const (String.length data));
      body (invoke ^ write (String.length manifest) (String.length response));
      body (write 0 (String.length manifest))]);
    section 11 (vector ["\000" ^ const 0 ^ "\011" ^ text data]);
  ]

let test_wasm_inputs () =
  let module Host = Octra_core.Circle_wasm_host in
  let module Native = Octra_core.Circle_wasm_native in
  let module Rule = Octra_core.Rule_graph in
  let module Wire = Octra_core.Circle_wasm_codec in
  let module Transcript = Octra_core.Circle_hfhe_transcript in
  require (Native.max_input_bytes = 16_777_216) "no-call input limit changed";
  require (Native.max_call_input_bytes = 67_108_864) "session input limit changed";
  require (Host.input_receipt_bytes = 9233) "receipt input size changed";
  let max_entry = Transcript.{method_name = String.make 64 '\000';
    request_hash = String.make 64 '0'; response_hash = String.make 64 '0'; result = None} in
  let max_entries = List.init Transcript.max_entries (fun _ -> max_entry) in
  require (Transcript.validate max_entries = Ok ()) "receipt size witness invalid";
  require (String.length (Yojson.Safe.to_string (Transcript.entries_json max_entries)) = 9233)
    "receipt size witness changed";
  let request_bytes = Result.get_ok (Wire.encode_request ~method_name:"input" []) in
  let run_case session storage_size receipt =
    let cached_storage = storage_size >= 0 in
    let label = Printf.sprintf "input-%b-%d-%b" session storage_size receipt in
    let hfhe_request = if not receipt then None else Some (Result.get_ok
      (Wire.encode_request ~method_name:"fhe_pedersen"
        [`Int 1; `String (Base64.encode_exn (String.make 32 '\001'))])) in
    let code_b64 = Base64.encode_exn (read_module ?hfhe_request label "OCWS1\000\000\000\000\000") in
    let storage_tbl = Hashtbl.create 1 in
    if storage_size > 0 then Hashtbl.add storage_tbl "value" (String.make storage_size 'v');
    let storage_cache_key = if cached_storage then Some label else None in
    let reset () =
      Hashtbl.reset Host.code_seed_cache;
      Host.clear_storage_payload_cache () in
    let callback _ = failwith "input check invoked callback" in
    let execute ?(hfhe_mode = Transcript.Direct) mode call caller =
      Lwt_main.run (Host.execute ~proof_mode:mode ~call ~code_b64
        ~export_name:"octra_update" ~request_bytes ~storage_tbl ~storage_cache_key
        ~caller ~address:"input" ~tx_hash:"input"
        ~current_epoch:(if mode = Rule.Prior then 1_662_999 else 1_663_000)
        ~hfhe_caps:(if receipt then ["fhe_pedersen"] else [])
        ~hfhe_pubkeys:[] ~hfhe_active_key:None ~hfhe_strict:false
        ~math:false ~float_mode:Rule.Prior ~hfhe_mode
        ~public_reads:[] ~fuel_limit:10_000_000 ~is_view:false ~update_policy:session) in
    let call = if session then Some callback else None in
    let limit = if session then Native.max_call_input_bytes else Native.max_input_bytes in
    let success = function
      | Ok result ->
        require (result.Host.success && result.response_value = Some Wire.Resp_null)
          "input limit execution failed";
        require (Hashtbl.find_opt result.storage_tbl "value"
          = Hashtbl.find_opt storage_tbl "value") "input limit changed storage";
        result.effort_used
      | Error error -> failwith (Host.error_message error) in
    reset ();
    let overhead = match execute Rule.Prior None (String.make Native.max_input_bytes 'x') with
      | Error (Host.Rejected reason) ->
        Scanf.sscanf reason "input too large: bytes=%d limit=%d" (fun bytes cap ->
          require (cap = Native.max_input_bytes) "prior input limit changed";
          bytes - cap)
      | _ -> failwith "prior cold input limit changed" in
    let mode_bytes = String.length (Yojson.Safe.to_string
      (`Assoc ["hfhe_pairs", `Bool true])) - 1 in
    let full_overhead = overhead + 9232 + mode_bytes in
    let caller delta = String.make (limit - full_overhead + delta) 'x' in
    let entries = if not receipt then [] else
      match execute ~hfhe_mode:Transcript.Capture Rule.Active call "seed" with
      | Ok result when result.Host.success ->
        require (List.length result.hfhe_transcript = 1) "capture did not produce a receipt";
        require (Transcript.validate result.hfhe_transcript = Ok ()) "captured receipt invalid";
        result.hfhe_transcript
      | Error error -> failwith (Host.error_message error)
      | Ok result -> failwith ("receipt capture failed: "
        ^ Option.value ~default:"missing error" result.error) in
    let modes = if receipt then [Transcript.Capture; Consume entries]
      else [Transcript.Direct; Capture; Consume []] in
    let accepted_effort = ref None in
    let check phase delta result =
      if delta <= 0 then begin
        let effort = success result in
        if receipt then require ((Result.get_ok result).Host.hfhe_transcript = entries)
          "input receipt changed";
        match !accepted_effort with
        | None -> accepted_effort := Some effort
        | Some expected -> require (effort = expected) "input cache changed effort"
      end
      else require (result = Error (Host.Rejected "input too large"))
        ("active " ^ phase ^ " input limit depends on cache") in
    let prior_caller = String.make (limit - overhead + 1) 'x' in
    reset ();
    ignore (success (execute Rule.Active call "seed"));
    check "warm transport" 1 (execute Rule.Active call prior_caller);
    reset ();
    check "cold transport" 1 (execute Rule.Active call prior_caller);
    List.iter (fun hfhe_mode -> List.iter (fun delta ->
      reset ();
      ignore (success (execute ~hfhe_mode Rule.Active call "seed"));
      require (Host.code_seeded (Host.code_cache_key code_b64)) "code cache not seeded";
      if cached_storage then
        require ((Hashtbl.find Host.storage_payload_cache label).seeded_in_native)
          "storage cache not seeded";
      check "warm" delta (execute ~hfhe_mode Rule.Active call (caller delta));
      if cached_storage then begin
        Hashtbl.reset Host.code_seed_cache;
        check "storage" delta (execute ~hfhe_mode Rule.Active call (caller delta))
      end;
      reset ();
      check "cold" delta (execute ~hfhe_mode Rule.Active call (caller delta));
      if delta > 0 then require (Host.storage_payload_cache_stats () = (0, 0)
        && not (Host.code_seeded (Host.code_cache_key code_b64)))
        "oversized input seeded cache";
      Gc.full_major ()) [-1; 0; 1]) modes;
    require (execute ~hfhe_mode:(Transcript.Consume (max_entry :: max_entries))
      Rule.Active call "seed" = Error (Host.Rejected "input too large"))
      "receipt bytes exceeded reservation";
    let invalid_entry = {max_entry with Transcript.request_hash = "invalid"} in
    List.iter (fun warm ->
      reset ();
      if warm then ignore (success (execute Rule.Active call "seed"));
      require (execute ~hfhe_mode:(Transcript.Consume [invalid_entry])
        Rule.Active call (caller 0) = Error (Host.Rejected "invalid hfhe receipt entry"))
        "invalid receipt bypassed native validation") [false; true];
    reset ();
    let full_bytes = limit + 1 in
    let expected = if session then Error (Host.Unavailable "circle session input invalid")
      else Error (Host.Rejected (Printf.sprintf "input too large: bytes=%d limit=%d"
        full_bytes Native.max_input_bytes)) in
    require (execute Rule.Prior call prior_caller = expected) "prior cold outcome changed";
    ignore (success (execute Rule.Prior call "seed"));
    ignore (success (execute Rule.Prior call prior_caller));
    reset ();
    Gc.full_major ();
    Printf.printf "event = wasm_input session = %b storage = %d receipt = %b status = passed\n%!"
      session storage_size receipt in
  List.iter (fun session ->
    List.iter (fun storage -> run_case session storage false) [-1; 0; 8192];
    run_case session (-1) true) [false; true];
  Printf.printf "event = wasm_inputs status = passed\n%!"

let test_wasm_effort () =
  let module VM = Vm.Contract_vm in
  let module Read = Octra_core.Circle_wasm_public_read in
  let native, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\041') in
  let raw = Pvac_ffi.serialize_pubkey native |> Bytes.to_string in
  let size = Pvac_ffi.pubkey_image_size native in
  let price bytes = (bytes + 15) / 16 in
  let key_cost = price (String.length raw + size) + price size in
  let public = (key (String.make 32 '\042')).Vm.Program_attestation.public_key in
  let owner = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn public) in
  with_store (fun store ->
    let circle = owner in
    let path, path_key = Result.get_ok (Circles.path_key_of_raw_path "/data") in
    let declaration = `Assoc ["circle_id", `String circle; "path", `String path;
      "offset", `Int 0; "max_bytes", `Int 8] in
    let method_info name view = `Assoc ["name", `String name; "view", `Bool view;
      "public_reads", `List [declaration]] in
    let manifest = Yojson.Safe.to_string (`Assoc ["methods", `List [
      method_info "fhe_read" true; method_info "fhe_write" false]]) in
    let install value =
      let response = "OCWS1\003\000\000\000\001" ^ value in
      let code = read_module manifest response in
      save_circle ~runtime:Circles.Wasm_v1 store circle owner (Base64.encode_exn code) in
    install "7";
    let storage = Hashtbl.create 1 in
    Hashtbl.add storage Octra_core.Circle_hfhe_policy.require_live_key_policy_key "false";
    ignore (Lwt_main.run (Store.save_circle_stable_storage store circle storage));
    let data = String.make 100 'a' in
    let meta = Circles.{path_key; canonical_path = path; content_type = "text/plain";
      encoding = "identity"; size_bytes = 100L; blob_hash = asset_blob_hash data;
      body_mode = Public_resources; plaintext_hash = None; key_id = None;
      padding_class = None; resource_key = path_key; locator_mode = Path_locator;
      slot_ref = None; activate_after_epoch = None; expire_after_epoch = None;
      metadata_mode = Metadata_reveal} in
    Lwt_main.run (Store.save_circle_asset_meta store circle meta);
    let save data = Lwt_main.run (Store.save_circle_asset_body_b64 store circle path_key data) in
    save (Base64.encode_exn data);
    let ctx mode = {VM.default_ctx with proof_exec = mode;
      fhe_memory = Some (Vm.Fhe_memory.create ()); current_epoch = 1_663_000;
      get_fhe_pubkey = (fun _ -> Some (VM.Key_bytes raw))} in
    let run view mode limit =
      if view then Lwt_main.run (Circle_exec.execute_view_call ~ctx:(ctx mode) ~limit
        store circle "fhe_read" [] owner)
      else (Lwt_main.run (Circle_exec.execute_call ~ctx:(ctx mode) ~limit
        store circle "fhe_write" [] owner Z.zero)).receipt in
    let before = run false Octra_core.Rule_graph.Prior 20_000_000 in
    require before.success "historical wasm call failed";
    let after = run false Octra_core.Rule_graph.Active 20_000_000 in
    require after.success "priced wasm call failed";
    let reads = ref Z.zero in
    let charge cost = reads := Z.add !reads cost; true in
    let cells = Lwt_main.run (Store.load_circle_stable_storage ~charge store circle) |> Result.get_ok in
    let io = Z.add !reads Z.(add
      (mul (of_int 3) (Vm.Program_journal.storage_effort cells))
      (Vm.Program_journal.write_effort cells)) |> Z.to_int in
    let output = Z.add (Vm.Program_journal.storage_effort cells)
      (Vm.Program_journal.write_effort cells) |> Z.to_int in
    require (after.effort_used - before.effort_used = key_cost + (100 - 8) * Read.byte_effort + io)
      "wasm key read and execution costs do not compose";
    List.iter (fun view ->
      let effort = after.effort_used - if view then io else 0 in
      let input = if view then 0 else io - output in
      let receipt = run view Octra_core.Rule_graph.Active effort in
      require (receipt.success && receipt.effort_used = effort)
        "wasm exact allowance failed";
      let short = effort - 1 in
      let refused = run view Octra_core.Rule_graph.Active short in
      require (not refused.success && refused.effort_used = short)
        "wasm exhausted execution returned effort";
      let refused = run view Octra_core.Rule_graph.Active (input + key_cost + Read.base_effort + 399) in
      require (not refused.success && refused.effort_used = input + key_cost + Read.base_effort
        && refused.error = Some "wasm public read effort exceeds limit")
        "wasm public read exceeded remaining allowance";
      save "invalid";
      let refused = run view Octra_core.Rule_graph.Active (input + key_cost + Read.base_effort + 399) in
      require (refused.error = Some "wasm public read effort exceeds limit")
        "wasm read body before reserving effort";
      save (Base64.encode_exn data)) [false; true];
    install "x";
    List.iter (fun view ->
      let receipt = run view Octra_core.Rule_graph.Active 20_000_000 in
      let effort = after.effort_used - if view then io else output in
      require (not receipt.success && receipt.effort_used = effort
        && receipt.error = Some "wasm response integer is invalid")
        "invalid wasm response lost execution effort") [false; true])

let test_compute_manifest () =
  let descriptor =
    wasm_descriptor [
      method_info "status" true "standard";
      method_info "complete" true "compute";
    ]
  in
  let methods =
    match Circle_program.methods_of_wasm_descriptor descriptor with
    | Ok methods -> methods
    | Error e -> fail e
  in
  require
    (Circle_program.execution_for_method methods "status" = Circle_program.Standard)
    "standard method changed execution class";
  require
    (Circle_program.execution_for_method methods "complete" = Circle_program.Compute)
    "compute method lost execution class";
  require
    (Circle_program.execution_for_method methods "missing" = Circle_program.Standard)
    "undeclared method gained compute execution";
  let compute_json =
    methods
    |> List.find (fun method_info -> String.equal method_info.Circle_program.name "complete")
    |> Circle_program.yojson_of_method_info
  in
  require
    (Yojson.Safe.Util.member "execution" compute_json = `String "compute")
    "compute execution missing from descriptor";
  match
    Circle_program.methods_of_wasm_descriptor
      (wasm_descriptor [method_info "mutate" false "compute"])
  with
  | Error "compute method must be view-only" -> ()
  | Error e -> fail ("unexpected compute update error: " ^ e)
  | Ok _ -> fail "compute update method accepted"

let test_cache_program_identity () =
  let info stable_root version code_hash = {
    Circles.circle_id = "octCACHE";
    runtime = Circles.Wasm_v1;
    version;
    owner = "octOWNER";
    code_hash;
    stable_root;
    assets_root = Circles.zero_hash_hex;
    privacy_class = Circles.Public;
    browser_mode = Circles.Native_sealed;
    resource_mode = Circles.Public_resources;
    policy_hash = None;
    members_root = None;
    export_policy = None;
    limits = Circles.default_limits;
  } in
  let first = Circle_program.loaded_cache_key "octCACHE" (info "root-a" 1L "code-a") in
  let written = Circle_program.loaded_cache_key "octCACHE" (info "root-b" 1L "code-a") in
  let upgraded = Circle_program.loaded_cache_key "octCACHE" (info "root-b" 2L "code-b") in
  require (String.equal first written)
    "stable storage write invalidated immutable program cache";
  require (not (String.equal first upgraded))
    "program upgrade reused prior manifest cache"

let () =
  if Array.exists (String.equal "--wasm-inputs") Sys.argv then test_wasm_inputs ()
  else begin
  test_inputs ();
  test_view_stop ();
  test_owner ();
  test_async_policy ();
  test_wasm_keys ();
  test_wasm_effort ();
  test_preview_trust ();
  test_compute_manifest ();
  test_cache_program_identity ();
  test_wasm_inputs ();
  Printf.printf "status = pass test = circle_program strict_inputs = true\n%!"
  end