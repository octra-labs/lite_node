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
    let info = Option.get (Lwt_main.run (Store.get_circle_info store circle_id)) in
    let scope = Circle_exec.preview_scope Vm.Contract_vm.default_ctx info in
    let key = Circle_exec.preview_cache_key ~scope ~trusted ~math:false circle_id owner "1,2" in
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

let read_module ?hfhe_request ?inspect_stage ?event ?(compute = false) manifest response =
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
  let plain = manifest ^ response ^ Option.value ~default:"" hfhe_request in
  let contents = plain ^ Option.fold ~none:"" ~some:(fun (topic, value) -> topic ^ value) event in
  let data = if compute then contents ^ String.make 8 '\000' else contents in
  let imports = [text "octra" ^ text "host_response_write" ^ "\000\000"]
    @ (match hfhe_request with
      | None -> []
      | Some _ -> [text "octra" ^ text "host_hfhe_invoke_len" ^ "\000\000"])
    @ (if compute then [text "octra" ^ text "host_tensor_silu_q24" ^ "\000\000"] else [])
    @ (match inspect_stage with
      | None -> []
      | Some _ -> [text "octra" ^ text "host_session_put" ^ "\000\002"])
    @ (match event with
      | None -> []
      | Some _ -> [text "octra" ^ text "host_emit_event" ^ "\000\002"]) in
  let first = List.length imports in
  let session = const 0 ^ const 1 ^ const 0 ^ const 1
    ^ "\016" ^ size (first - (if Option.is_none event then 1 else 2)) ^ "\026" in
  let inspect stage = if inspect_stage = Some stage then session else "" in
  let invoke = match hfhe_request with
    | None -> ""
    | Some request -> const (String.length manifest + String.length response)
      ^ const (String.length request) ^ "\016\001\026" in
  let math = if compute then const (String.length contents) ^ const 1
    ^ "\016" ^ size (if Option.is_none hfhe_request then 1 else 2) ^ "\026" else "" in
  let emit = match event with
    | None -> ""
    | Some (topic, value) -> const (String.length plain) ^ const (String.length topic)
      ^ const (String.length plain + String.length topic) ^ const (String.length value)
      ^ "\016" ^ size (first - 1) ^ "\026" in
  let write offset length = const offset ^ const length ^ "\016\000\026" ^ const 0 in
  String.concat "" [
    "\000asm\001\000\000\000";
    section 1 (vector (["\096\002\127\127\001\127"; "\096\001\127\001\127"]
      @ (if Option.is_none inspect_stage && Option.is_none event then [] else
        ["\096\004\127\127\127\127\001\127"; "\096\000\000"])));
    section 2 (vector imports);
    section 3 (vector ([size 1; size 0; size 0]
      @ (if inspect_stage = Some 0 then [size 3] else [])));
    section 5 (vector ["\000\001"]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 first;
      export "octra_query" 0 (first + 1); export "octra_update" 0 (first + 1);
      export "octra_manifest" 0 (first + 2)]);
    (if inspect_stage = Some 0 then section 8 (size (first + 3)) else "");
    section 10 (vector ([body (inspect 1 ^ const (String.length data));
      body (inspect 4 ^ invoke ^ math ^ emit ^ write (String.length manifest) (String.length response));
      body (inspect 2 ^ write 0 (String.length manifest))]
      @ (if inspect_stage = Some 0 then [body session] else [])));
    section 11 (vector ["\000" ^ const 0 ^ "\011" ^ text data]);
  ]

let test_wasm_events () =
  let module VM = Vm.Contract_vm in
  let module B = Vm.Byte_work in
  with_store (fun store ->
    let owner = "oct11111111111111111111111111111111111111111111" in
    let circle = "octWASM_EVENTS" in
    let manifest = "{\"methods\":[{\"name\":\"update\",\"view\":false}]}" in
    let response = "OCWS1\003\000\000\000\0017" in
    let code = read_module ~event:("log", "\000\n\"\\") manifest response in
    save_circle ~runtime:Circles.Wasm_v1 store circle owner (Base64.encode_exn code);
    let run budget = Lwt_main.run (Circle_exec.execute_call
      ~ctx:{VM.default_ctx with byte_work = budget}
      store circle "update" [] owner Z.zero) |> fun result -> result.Circle_exec.receipt in
    let prior = run None in
    require (prior.success && List.length prior.events = 1)
      "prior wasm event control failed";
    let bytes = Vm.Receipt_view.program_events_json prior.events
      |> Yojson.Safe.to_string |> String.length in
    let budget = B.create B.consensus_limits in
    let charged = run (Some budget) in
    require (charged.success && charged.events = prior.events
      && charged.return_value = prior.return_value)
      "wasm event accounting changed result";
    require (B.event_left budget = 4_194_304 - bytes - 19)
      "wasm event or result bytes omitted";
    require (charged.effort_used > prior.effort_used) "wasm event work not charged";
    List.iter (fun remaining ->
      let budget = B.create B.consensus_limits in
      require (Option.is_some (B.output budget ~used:0 ~limit:1_000_000
        (4_194_304 - remaining))) "wasm quota setup failed";
      let result = run (Some budget) in
      require (result.success = (remaining = bytes + 19)) "wasm receipt quota differs";
      if not result.success then require (result.return_value = None
        && result.error = Some "circle receipt effort exceeds limit")
        "wasm quota refusal lost") [bytes - 1; bytes + 18; bytes + 19];
    let budget = B.create B.consensus_limits in
    for _ = 1 to 256 do
      require (Option.is_some (B.emit budget ~used:0 ~limit:1_000_000 0))
        "wasm event count setup failed"
    done;
    require (not (run (Some budget)).success) "wasm event count ignored")

let test_wasm_inspect () =
  let module Host = Octra_core.Circle_wasm_host in
  let module Rule = Octra_core.Rule_graph in
  let manifest = "{\"methods\":[]}" in
  let response = "OCWS1\000\000\000\000\000" in
  let fields = ["action", `String "describe"] in
  require (Host.inspect_fields Rule.Prior fields = fields)
    "prior inspection changed payload";
  let refused = function
    | Error (Host.Rejected reason) ->
      require (Host.string_starts_with
        ~prefix:"nonpersistent wasm import during inspection:" reason)
        "inspection refusal class changed"
    | Error (Host.Unavailable _) -> fail "inspection became unavailable"
    | Ok _ -> fail "inspection accepted session effect" in
  with_store (fun store ->
    let owner = "octINSPECT_OWNER" in
    List.iter (fun stage ->
      let circle_id = "octINSPECT" ^ string_of_int stage in
      let code_b64 = Base64.encode_exn (read_module ~inspect_stage:stage manifest response) in
      save_circle ~runtime:Circles.Wasm_v1 store circle_id owner code_b64;
      let read mode = Lwt_main.run (Circle_program.load ~inspect_mode:mode store circle_id) in
      refused (Lwt_main.run (Host.describe ~inspect_mode:Rule.Active code_b64));
      if stage = 0 then begin
        require (Result.is_ok (Lwt_main.run (Host.validate code_b64)))
          "prior startup inspection changed";
        refused (Lwt_main.run (Host.validate ~inspect_mode:Rule.Active code_b64))
      end;
      refused (read Rule.Active);
      require (Result.is_ok (read Rule.Prior)) "prior descriptor inspection changed";
      refused (read Rule.Active);
      require (Result.is_ok (read Rule.Prior)) "strict refusal polluted prior cache";
      let view = Lwt_main.run (Circle_exec.execute_view_call_direct
        ~inspect_mode:Rule.Active store circle_id "query" [] owner) in
      require (not view.success && Option.is_some view.error)
        "view bypassed descriptor inspection";
      let call = Lwt_main.run (Circle_exec.execute_call
        ~inspect_mode:Rule.Active store circle_id "update" [] owner Z.zero) in
      require (not call.receipt.success) "update bypassed descriptor inspection"
    ) [0; 1; 2];
    let code_b64 = Base64.encode_exn (read_module ~inspect_stage:3 manifest response) in
    let circle_id = "octINSPECT_UNUSED" in
    save_circle ~runtime:Circles.Wasm_v1 store circle_id owner code_b64;
    require (Result.is_ok (Lwt_main.run (Host.describe ~inspect_mode:Rule.Active code_b64)))
      "unused session import rejected";
    let strict = Lwt_main.run (Circle_program.load ~inspect_mode:Rule.Active store circle_id) in
    require (Result.is_ok strict) "strict descriptor control rejected";
    let prior = Lwt_main.run (Circle_program.load store circle_id) in
    require (Result.is_ok prior) "prior descriptor control rejected";
    require ((Result.get_ok strict).code != (Result.get_ok prior).code)
      "prior reused strict descriptor identity";
    let repeated = Lwt_main.run (Circle_program.load ~inspect_mode:Rule.Active store circle_id) in
    require (Result.is_ok repeated && (Result.get_ok strict).code == (Result.get_ok repeated).code)
      "strict descriptor cache lost its identity";
    let ctx = {Vm.Contract_vm.default_ctx with proof_exec = Rule.Prior} in
    let view = Lwt_main.run (Circle_exec.execute_view_call_direct
      ~ctx ~inspect_mode:Rule.Active store circle_id "query" [] owner) in
    require view.success "strict query control rejected");
  let prior = Circle_exec.preview_cache_key ~scope:"scope" ~trusted:[] ~math:false "circle" "caller" "1" in
  let strict = Circle_exec.preview_cache_key ~inspect_mode:Rule.Active
    ~scope:"scope" ~trusted:[] ~math:false "circle" "caller" "1" in
  require (prior <> strict) "preview cache crossed inspection policy"

let test_inspect_tx () =
  let module Epoch = Octra_core.Epoch_exec in
  let module Rule = Octra_core.Rule_graph in
  with_store (fun store ->
    let owner = "octINSPECT_TX_OWNER" in
    let ledger = Octra_core.Ledger.create store in
    require (Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) = Ok ())
      "inspection account creation failed";
    let backend = Epoch.make_live_backend store ledger in
    Lwt_main.run (backend.begin_batch Rule.Prior);
    let tx target op_type message = Octra_core.Transaction.{
      from = owner; to_ = target; amount = Z.zero; nonce = 1;
      ou = Z.of_int 10; timestamp = 0.; signature = ""; public_key = None;
      message = Some (Yojson.Safe.to_string message); op_type; encrypted_data = None;
    } in
    let payload code = Circles.{
      runtime = Wasm_v1; privacy_class = Public; browser_mode = Native_sealed;
      resource_mode = Public_resources; code_b64 = Some code;
      policy_hash = None; members_root = None; export_policy = None;
      limits = default_limits;
    } in
    let intact () =
      let account = Option.get (Octra_core.Ledger.find_opt ledger owner) in
      require (Z.equal account.balance (Z.of_int 1_000) && account.nonce = 0)
        "refused inspection consumed funds or nonce" in
    let refused = function
      | Error ("circle_runtime_invalid", _) -> ()
      | Error (_, reason) -> fail reason
      | Ok _ -> fail "transaction accepted session inspection" in
    let good = Base64.encode_exn (read_module "{\"methods\":[]}" "OCWS1\000\000\000\000\000") in
    let circle_id = "octINSPECT_UPDATE" in
    save_circle ~runtime:Circles.Wasm_v1 store circle_id owner good;
    List.iter (fun stage ->
      let code = Base64.encode_exn (read_module ~inspect_stage:stage
        "{\"methods\":[]}" "OCWS1\000\000\000\000\000") in
      let data = payload code in
      let target = Circles.circle_id_of_deploy ~deployer:owner ~nonce:1 data in
      let deploy = tx target Octra_core.Transaction.CircleDeploy
        (Circles.yojson_of_deploy_payload data) in
      refused (Lwt_main.run (Epoch.process_circle_operation_tx
        ~inspect_mode:Rule.Active ~backend ~current_epoch:0 deploy));
      require (not (Lwt_main.run (Store.circle_exists store target)))
        "refused inspection created circle";
      intact ();
      let update = tx circle_id Octra_core.Transaction.CircleProgramUpdate
        (Circles.yojson_of_program_update_payload {Circles.code_b64 = code}) in
      refused (Lwt_main.run (Epoch.process_circle_operation_tx
        ~inspect_mode:Rule.Active ~backend ~current_epoch:0 update));
      intact ();
      require (Lwt_main.run (Store.get_circle_program_code_b64 store circle_id) = Some good)
        "refused inspection changed circle code"
    ) [0; 1; 2];
    let data = payload good in
    let target = Circles.circle_id_of_deploy ~deployer:owner ~nonce:1 data in
    let deploy = tx target Octra_core.Transaction.CircleDeploy
      (Circles.yojson_of_deploy_payload data) in
    require (Result.is_ok (Lwt_main.run (Epoch.process_circle_operation_tx
      ~inspect_mode:Rule.Active ~backend ~current_epoch:0 deploy)))
      "strict deploy control rejected";
    Lwt_main.run (backend.commit_batch ()))

let test_inspect_epoch () =
  let module Epoch = Octra_core.Epoch_exec in
  let module Rule = Octra_core.Rule_graph in
  let module Tx = Octra_core.Transaction in
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Option.get (Rule.vm_bytes_activation_for_chain chain_id) in
  let rules = Rule.create ~chain_id ~root_at:(fun epoch ->
    match Rule.root_after_floor ~chain_id ~floor_epoch:plan.activation_epoch ~epoch with
    | Some root -> Rule.Root root
    | None -> Rule.Missing) in
  List.iter (fun (epoch, mode) ->
    List.iter (fun route ->
      List.iter (fun operation ->
      List.iter (fun session -> with_store (fun store ->
        let update = operation = 1 in
        let call = operation = 2 in
        let owner = "oct11111111111111111111111111111111111111111111" in
        let ledger = Octra_core.Ledger.create store in
        require (Octra_core.Ledger.add_account ledger owner (Z.of_int 1000) = Ok ())
          "inspection balance failed";
        let scheduled = if epoch < plan.activation_epoch then Rule.Prior else Rule.Active in
        require (Rule.vm_bytes rules ~epoch = Ok scheduled)
          "inspection schedule differs";
        let policy = match Octra_node_runtime.Set_rule.policy rules epoch with
          | Ok value -> value
          | Error reason -> fail ("set byte policy: " ^ reason) in
        require (policy.vm_bytes = scheduled) "set policy byte schedule differs";
        let fold epoch = Epoch.prior_fold epoch |> Result.map (fun value ->
          {value with Epoch.vm_bytes = mode;
            proof_exec = if call then Rule.Active else value.Epoch.proof_exec}) in
        let backend = Epoch.make_live_backend ~fold store ledger in
        Lwt_main.run (backend.begin_batch Rule.Prior);
        let code = Base64.encode_exn (read_module
          ?inspect_stage:(if session then Some 0 else None)
          "{\"methods\":[{\"name\":\"update\",\"view\":false}]}"
          "OCWS1\003\000\000\000\0017") in
        let data = Circles.{runtime = Wasm_v1; privacy_class = Public;
          browser_mode = Native_sealed; resource_mode = Public_resources;
          code_b64 = Some code; policy_hash = None; members_root = None;
          export_policy = None; limits = default_limits} in
        let target = Circles.circle_id_of_deploy ~deployer:owner ~nonce:1 data in
        let initial = Base64.encode_exn (read_module
          "{\"methods\":[]}" "OCWS1\000\000\000\000\000") in
        if update then save_circle ~runtime:Circles.Wasm_v1 store target owner initial;
        if call then save_circle ~runtime:Circles.Wasm_v1 store target owner code;
        let op_type, message = if call then Tx.CircleCall, `List [] else if update then
          Tx.CircleProgramUpdate,
          Circles.yojson_of_program_update_payload {Circles.code_b64 = code}
        else Tx.CircleDeploy, Circles.yojson_of_deploy_payload data in
        let transaction = Tx.{from = owner; to_ = target; amount = Z.zero;
          nonce = 1; ou = Z.of_int 10; timestamp = 0.; signature = "";
          public_key = None; encrypted_data = if call then Some "update" else None; op_type;
          message = Some (Yojson.Safe.to_string message)} in
        let env = Epoch.{chain_id; epoch_id = epoch; proposer_addr = owner;
          validator_addrs = []; validator_pubkeys = []; prev_state_root = "";
          epoch_ts = 0.; ready_state_root_at = None; ready_max_lag = -1} in
        let result = if route = 1 then
          Lwt_main.run (Epoch.process_standard_tx_with_wasm_profile
            ~wasm_profile:Octra_core.Circle_wasm_host.Manifest ~backend ~env transaction)
          |> Result.map (fun _ -> ())
        else if route = 0 then
          Lwt_main.run (Octra_node_runtime.Consensus_vm_transition.process_tx
            ~circle_mode:Rule.Prior ~wasm_compute_mode:Rule.Active
            ~program_trust:Vm.Program_trust.empty ~object_cost:false
            ~backend ~env transaction) |> fun result ->
          Result.bind result (function
            | Epoch.Confirmed _ -> Ok ()
            | Epoch.Rejected_after_fee result -> Error (result.error_type, result.reason))
        else
          let module Sender = Octra_node_runtime.Consensus_epoch_apply_sender_live in
          let path = Printf.sprintf "runtime_data/circle_program_%d/chain" (Unix.getpid ()) in
          let chaindata = Octra_core.Store_chaindata.open_chaindata path in
          let accepted = ref false in
          let refused = ref ("sender", "rejected") in
          Octra_core.Store_chaindata.begin_batch chaindata;
          Fun.protect ~finally:(fun () ->
            Octra_core.Store_chaindata.abort_batch chaindata;
            Octra_core.Store_chaindata.close chaindata)
            (fun () ->
              Lwt_main.run (Sender.run Sender.{
                ledger; store; chaindata; program_trust = Vm.Program_trust.empty;
                object_cost = false; cipher_mode = Octra_core.Rule_graph.Prior;
                wallet_addr = owner; pre_state_hash = ""; fold;
                standard_env = (fun () -> env); current_epoch = (fun () -> epoch);
                max_fhe_per_epoch = 10; max_stealth_per_epoch = 10; max_stealth_defer = 1;
                stealth_inline_verify_allowed = false; fhe_in_epoch_counter = ref 0;
                stealth_in_epoch_counter = ref 0; stealth_defer_count = Hashtbl.create 1;
                deferred_stealth_txs = ref []; confirmed_fees = ref Z.zero;
                short_addr = Fun.id; log_rejected = (fun _ kind reason -> refused := kind, reason);
                confirm_tx = (fun _ -> accepted := true);
                notify_new_account = (fun _ -> ()); notify_confirmed = (fun _ _ -> ());
                notify_rejected = (fun _ _ -> ());
                legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ ->
                  fail "unexpected private replay");
                private_field_policy = Octra_core.Private_ledger.Unique_fields;
                private_result_policy = (fun _ -> Octra_core.Private_result_policy.Recoverable);
              } [transaction]);
              if !accepted then Ok () else Error !refused) in
        require (Result.is_ok result = (not session || mode = Rule.Prior))
          (Printf.sprintf "inspection epoch = %d route = %d operation = %d session = %b result = %s"
            epoch route operation session
            (match result with Ok () -> "accepted" | Error (kind, reason) -> kind ^ ": " ^ reason));
        require (Lwt_main.run (Store.circle_exists store target)
          = (update || call || Result.is_ok result))
          "consensus inspection state differs";
        if update then require
          (Lwt_main.run (Store.get_circle_program_code_b64 store target)
            = Some (if Result.is_ok result then code else initial))
          "consensus inspection changed refused code";
        Store.abort_epoch_batch store)) [false; true])
        (if route = 1 then [0; 1] else [0; 1; 2])) [0; 1; 2])
    (List.concat_map (fun epoch -> [epoch, Rule.Prior; epoch, Rule.Active])
      [plan.activation_epoch - 1; plan.activation_epoch; plan.activation_epoch + 1])

let test_tx_code_reads () =
  let module Epoch = Octra_core.Epoch_exec in
  let module Tx = Octra_core.Transaction in
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let module View = Octra_node_runtime.Tx_view in
  let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
    (String.make 32 '\047')) in
  let private_key = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets secret) in
  let public_key = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets
    (Mirage_crypto_ec.Ed25519.pub_of_priv secret)) in
  let owner = Octra_core.Crypto.Address.address_from_pubkey public_key in
  with_store (fun store ->
    let ledger = Octra_core.Ledger.create store in
    require (Octra_core.Ledger.add_account ledger owner (Z.of_int 1_000) = Ok ())
      "private code account creation failed";
    let backend = Epoch.make_live_backend store ledger in
    let code = Base64.encode_exn (read_module "{\"methods\":[]}"
      "OCWS1\000\000\000\000\000") in
    List.iteri (fun index privacy_class ->
      let nonce = index * 2 + 1 in
      let data = Circles.{runtime = Wasm_v1; privacy_class; browser_mode = Native_sealed;
        resource_mode = Public_resources; code_b64 = Some code; policy_hash = None;
        members_root = None; export_policy = None; limits = default_limits} in
      let circle_id = Circles.circle_id_of_deploy ~deployer:owner ~nonce data in
      let message = Yojson.Safe.to_string (Circles.yojson_of_deploy_payload data) in
      let tx = Tx.sign_with_privkey Tx.{from = owner; to_ = circle_id; amount = Z.zero;
        nonce; ou = Z.of_int 10; timestamp = 0.; signature = "";
        public_key = Some public_key; message = Some message; op_type = CircleDeploy;
        encrypted_data = None} private_key in
      require (Tx.verify tx public_key) "private deploy signature failed";
      Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
      Lwt_main.run (Store.set_meta store "last_epoch" "0");
      require (Result.is_ok (Lwt_main.run (Epoch.process_circle_operation_tx
        ~inspect_mode:Octra_core.Rule_graph.Active ~backend ~current_epoch:0 tx)))
        "private code deployment failed";
      Lwt_main.run (backend.commit_batch ());
      let public = Lwt_main.run (Read.program_params store (`List [`String circle_id])) in
      require (Result.is_ok public = (privacy_class = Circles.Public))
        "private code public read policy changed";
      let signature = Auth.message ~op:"octra_circle_program" ~circle_id ~addr:owner ~subject:""
        |> Mirage_crypto_ec.Ed25519.sign ~key:secret |> Base64.encode_exn in
      let params = `List [`String circle_id; `String owner; `String public_key; `String signature] in
      let signed = Result.get_ok (Lwt_main.run (Read.program_auth_params store params)) in
      require (Yojson.Safe.Util.member "code_b64" signed = `String code)
        "private code owner read changed";
      let raw = Tx.to_yojson tx |> Yojson.Safe.to_string in
      let response = Result.get_ok (View.transaction_lookup_response
        ~decode_message:Octra_node_runtime.Text.decode_message_if_hex ~hash:(Tx.hash tx)
        (View.Lookup_confirmed (1, raw))) in
      let expected = if privacy_class = Circles.Public then `String message else `Null in
      require (Yojson.Safe.Util.member "message" response = expected)
        "private deploy transaction exposed code";
      require (Tx.to_yojson tx |> Yojson.Safe.to_string = raw)
        "private transaction bytes changed";
      let update = Tx.sign_with_privkey {tx with nonce = nonce + 1;
        op_type = Tx.CircleProgramUpdate;
        message = Some (Yojson.Safe.to_string
          (Circles.yojson_of_program_update_payload {Circles.code_b64 = code}))}
        private_key in
      Lwt_main.run (backend.begin_batch Octra_core.Rule_graph.Prior);
      require (Result.is_ok (Lwt_main.run (Epoch.process_circle_operation_tx
        ~inspect_mode:Octra_core.Rule_graph.Active ~backend ~current_epoch:0 update)))
        "private code update failed";
      Lwt_main.run (backend.commit_batch ());
      let fields = View.tx_fields ~decode_message:Octra_node_runtime.Text.decode_message_if_hex update in
      require (List.assoc "message" fields = `Null) "private code update exposed bytes";
      let signed = Result.get_ok (Lwt_main.run (Read.program_auth_params store params)) in
      require (Yojson.Safe.Util.member "code_b64" signed = `String code)
        "private code update lost owner read";
      let wrong = `List [`String circle_id; `String owner; `String public_key; `String "invalid"] in
      require (Result.is_error (Lwt_main.run (Read.program_auth_params store wrong)))
        "private code accepted wrong signature")
      [Circles.Public; Circles.Scoped; Circles.Sealed; Circles.Circle])

let test_inspect_rpc () =
  let module Read = Octra_node_runtime.Circle_read_rpc in
  let module Auth = Octra_node_runtime.Circle_auth in
  let module Rule = Octra_core.Rule_graph in
  let secret = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
    (String.make 32 '\043')) in
  let public_key = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets
    (Mirage_crypto_ec.Ed25519.pub_of_priv secret)) in
  let owner = Octra_core.Crypto.Address.address_from_pubkey public_key in
  let view action params store =
    let running, stop = Vm.Contract_rpc.view_clock () in
    action store params ~view_ctx:Vm.Contract_vm.default_ctx ~running ~stop in
  let routes = Read.dispatch Read.{
    store_read = (fun action params store -> action store params);
    program_read = (fun action params store ->
      action ~profile:(fun ~epoch -> Ok Vm.Contract_rpc.{
        epoch; point_ops = false; math = false; object_cost = false;
        int_work = Vm.Int_work.Prior; fhe_work = Rule.Prior;
        proof_exec = Rule.Prior; wasm_float = Rule.Prior;
      }) store params);
    epoch_read = (fun action params store -> action store params ~current_epoch:0);
    public_asset_read = (fun action params store ->
      action store params ~current_epoch:0 ~reveal_sensitive_fields:false);
    circle_view = view (Read.view_call_public_params ?snapshot:None ~trusted:[]);
    circle_view_auth = view (Read.view_call_auth ?snapshot:None ~trusted:[]);
  } in
  with_store (fun store ->
    let circle_id = "octINSPECT_RPC" in
    Lwt_main.run (Octra_core.Store_irmin.set_meta store "last_epoch" "0");
    let sign op subject =
      let message = Auth.message ~op ~circle_id ~addr:owner ~subject in
      let signature = Base64.encode_exn
        (Mirage_crypto_ec.Ed25519.sign ~key:secret message) in
      [`String owner; `String public_key; `String signature] in
    let info = [`String circle_id] in
    let info_auth = info @ sign "octra_circle_program_info" "" in
    let query = [ `String circle_id; `String "query"; `List [] ] in
    let query_auth storage = query @ sign "octra_circle_view"
      (Auth.view_subject ~method_name:"query" ~call_params:[] ~include_storage:storage)
      @ [`Bool storage] in
    let calls = [
      "circle_program_info", info;
      "octra_circleProgramInfo", info;
      "circle_program_info_auth", info_auth;
      "octra_circleProgramInfoAuth", info_auth;
      "circle_view", query;
      "octra_circleView", query;
      "circle_view_auth", query_auth false;
      "octra_circleViewAuth", query_auth true;
    ] in
    let run (name, params) =
      Lwt_main.run ((List.assoc name routes) (`List params) store) in
    let save ?inspect_stage ?(execution = "standard") () =
      let manifest = Yojson.Safe.to_string (`Assoc ["methods",
        `List [method_info "query" true execution]]) in
      let raw = read_module ?inspect_stage ~compute:(execution = "compute") manifest
        "OCWS1\000\000\000\000\000" in
      save_circle ~runtime:Circles.Wasm_v1 store circle_id owner (Base64.encode_exn raw) in
    List.iter (fun stage ->
      save ~inspect_stage:stage ();
      require (Result.is_ok (Lwt_main.run (Circle_program.load store circle_id)))
        "RPC prior cache control rejected";
      List.iter (fun ((name, _) as call) ->
        match run call with
        | Error error ->
          let reason = "nonpersistent wasm import during inspection: host_session_put" in
          require (error.Octra_core.Rpc.message = reason
            || error.message = "invalid circle wasm: " ^ reason)
            ("RPC inspection refusal route = " ^ name ^ " error = " ^ error.message)
        | Ok _ -> fail "RPC inspection accepted session effect") calls
    ) [0; 1; 2];
    List.iter (fun stage ->
      save ~inspect_stage:stage ();
      List.iter (fun call ->
        require (Result.is_ok (run call)) "RPC query control rejected") calls)
      [3; 4];
    save ~inspect_stage:4 ~execution:"compute" ();
    require (Result.is_ok (run ("circle_program_info", info)))
      "RPC compute descriptor control rejected";
    List.iter (fun call ->
      match run call with
      | Error error ->
        require (error.Octra_core.Rpc.message = "circle compute method requires compute executor")
          "RPC compute refusal class changed"
      | Ok _ -> fail "RPC enabled compute method")
      ["circle_view", query; "circle_view_auth", query_auth false];
    let compute = Lwt_main.run (Circle_exec.execute_compute_view_call
      ~inspect_mode:Rule.Active ~ctx:{Vm.Contract_vm.default_ctx with math = true}
      store circle_id "query" [] owner) in
    require compute.success ("strict compute executor error = "
      ^ Option.value ~default:"none" compute.error);
    save ~inspect_stage:4 ();
    let storage = Hashtbl.create 1 in
    Hashtbl.add storage "value" "visible";
    ignore (Lwt_main.run (Store.save_circle_stable_storage store circle_id storage));
    let public_view = Result.get_ok (run ("circle_view", query)) in
    require (Yojson.Safe.Util.member "storage" public_view = `Null)
      "public RPC view exposed storage";
    let owner_view = Result.get_ok (run ("circle_view_auth", query_auth true)) in
    require (Yojson.Safe.Util.member "storage" owner_view =
      `Assoc ["value", `String "visible"]) "owner RPC storage response differs";
    let altered = query @ sign "octra_circle_view"
      (Auth.view_subject ~method_name:"query" ~call_params:[] ~include_storage:false)
      @ [`Bool true] in
    begin match run ("circle_view_auth", altered) with
    | Error error ->
      require (error.Octra_core.Rpc.message = "signature verification failed")
        "RPC signed storage flag refusal differs"
    | Ok _ -> fail "RPC accepted altered storage flag"
    end;
    let info = Option.get (Lwt_main.run (Store.get_circle_info store circle_id)) in
    Lwt_main.run (Store.deploy_circle store {info with privacy_class = Circles.Scoped});
    List.iter (fun call ->
      require (Result.is_error (run call)) "private RPC read lost authorization")
      ["circle_program_info", [`String circle_id]; "circle_view", query];
    List.iter (fun call ->
      require (Result.is_ok (run call)) "private RPC owner control rejected")
      ["circle_program_info_auth", info_auth; "circle_view_auth", query_auth true];
    let other_key = Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets
      (String.make 32 '\044')) in
    let other = Mirage_crypto_ec.Ed25519.pub_of_priv other_key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn
      |> Octra_core.Crypto.Address.address_from_pubkey in
    Lwt_main.run (Store.deploy_circle store {info with
      privacy_class = Circles.Scoped; owner = other});
    List.iter (fun call ->
      require (Result.is_error (run call)) "foreign RPC reader accepted")
      ["circle_program_info_auth", info_auth; "circle_view_auth", query_auth true];
    require (Result.is_error (run ("circle_program_info_auth", [`String circle_id])))
      "RPC inspection bypassed signature check")

let test_preview_code () =
  let module Rule = Octra_core.Rule_graph in
  with_store (fun store ->
    let owner = "octPREVIEW_OWNER" in
    let circle_id = "octPREVIEW_CODE" in
    let manifest = "{\"methods\":[]}" in
    let code ?inspect_stage value = Base64.encode_exn
      (read_module ?inspect_stage manifest ("OCWS1\004\000\000\000\001" ^ value)) in
    let save code_b64 = save_circle ~runtime:Circles.Wasm_v1 store circle_id owner code_b64 in
    let read () = Lwt_main.run (Circle_exec.execute_view_call
      ~running:(fun () -> true) ~inspect_mode:Rule.Active
      store circle_id "complete_preview" [`String "1"; `Int 1] owner) in
    save (code "2");
    let first = read () in
    require (first.success && first.return_value = Some (Vm.Contract_vm.VString "2"))
      "preview code control failed";
    let cached = read () in
    require (cached.success && cached.effort_used = 0) "preview code control not cached";
    save (code "3");
    let replaced = read () in
    require (replaced.success && replaced.return_value = Some (Vm.Contract_vm.VString "3")
      && replaced.effort_used > 0) "preview reused replaced code";
    save (code ~inspect_stage:2 "4");
    let refused = read () in
    require (not refused.success && Option.is_some refused.error)
      "preview cache bypassed replacement inspection")

let test_preview_root () =
  let module Rule = Octra_core.Rule_graph in
  let module Policy = Octra_core.Circle_hfhe_policy in
  let module Public = Octra_core.Circle_wasm_public_read in
  with_store (fun store ->
    let owner = (key (String.make 32 '\047')).Vm.Program_attestation.public_key
      |> Base64.encode_exn |> Octra_core.Crypto.Address.address_from_pubkey in
    let circle_id = owner in
    let path, path_key = Result.get_ok (Circles.path_key_of_raw_path "/value") in
    let declaration = `Assoc ["circle_id", `String circle_id; "path", `String path;
      "offset", `Int 0; "max_bytes", `Int 1] in
    let manifest = Yojson.Safe.to_string (`Assoc ["methods", `List [
      `Assoc ["name", `String "complete_preview"; "view", `Bool true;
        "public_reads", `List [declaration]]]]) in
    let put epoch value access =
      Lwt_main.run (Store.begin_epoch_batch store);
      Lwt_main.run (Store.set_meta store "last_epoch" (string_of_int epoch));
      let code = read_module manifest
        ("OCWS1\004\000\000\000\001" ^ value) in
      save_circle ~runtime:Circles.Wasm_v1 store circle_id owner (Base64.encode_exn code);
      let cells = Hashtbl.create 2 in
      Hashtbl.add cells "value" value;
      Hashtbl.add cells Policy.cipher_arithmetic_mode_key access;
      Hashtbl.add cells Policy.require_live_key_policy_key "false";
      Hashtbl.add cells (Circles.slot_policy_revoked_key path_key)
        (string_of_bool (epoch = 90));
      ignore (Lwt_main.run (Store.save_circle_stable_storage store circle_id cells));
      let meta = Circles.{path_key; canonical_path = path; content_type = "text/plain";
        encoding = "identity"; size_bytes = 1L; blob_hash = asset_blob_hash value;
        body_mode = Public_resources; plaintext_hash = None; key_id = None;
        padding_class = None; resource_key = path_key; locator_mode = Path_locator;
        slot_ref = None; activate_after_epoch = None; expire_after_epoch = None;
        metadata_mode = Metadata_reveal} in
      Lwt_main.run (Store.save_circle_asset_meta store circle_id meta);
      Lwt_main.run (Store.save_circle_asset_body_b64 store circle_id path_key
        (Base64.encode_exn (if epoch = 90 then "invalid" else value))) in
    put 50 "2" "deny";
    Lwt_main.run (Store.commit_epoch_batch store "preview seed");
    let first = Lwt_main.run (Store.capture_read_snapshot store) |> Result.get_ok in
    put 70 "3" "owner_only";
    Lwt_main.run (Store.commit_epoch_batch store "preview next");
    let second = Lwt_main.run (Store.capture_read_snapshot store) |> Result.get_ok in
    put 90 "4" "deny";
    let ctx (snapshot : Store.read_snapshot) = {Vm.Contract_vm.default_ctx with
      tree_hash = snapshot.Store.state_root;
      current_epoch = Int64.to_int snapshot.epoch_id + 1} in
    let read snapshot = Lwt_main.run (Circle_exec.execute_view_call ~snapshot
      ~running:(fun () -> true) ~inspect_mode:Rule.Active ~ctx:(ctx snapshot)
      store circle_id "complete_preview" [`String "1"; `Int 1] owner) in
    let check snapshot value =
      let result = read snapshot in
      require (result.success && result.return_value = Some (Vm.Contract_vm.VString value))
        ("preview read another root error = " ^ Option.value ~default:"none" result.error) in
    check first "2";
    let cached = read first in
    require (cached.success && cached.effort_used = 0) "fixed preview was not cached";
    check second "3";
    check first "2";
    let declarations = Public.declarations_of_manifest
      (Some (Yojson.Safe.from_string manifest)) |> Result.get_ok in
    List.iter (fun (snapshot, value) ->
      let bundle = Lwt_main.run (Public.load ~snapshot store
        (Int64.succ snapshot.Store.epoch_id) declarations) |> Result.get_ok in
      match bundle.snapshots with
      | [read] ->
        require (read.body = value && read.snapshot_root = Circles.h256_hex
          "circle_public_read_snapshot_v1" [snapshot.state_root])
          "public read used another root"
      | _ -> fail "public read set differs") [first, "2"; second, "3"];
    List.iter (fun (snapshot, access) ->
      let runtime = Lwt_main.run (Circle_exec.load_runtime_hfhe_ctx ~snapshot
        (ctx snapshot) store circle_id owner) |> Result.get_ok in
      require (runtime.exec_ctx.allow_fhe_capability Vm.Contract_vm.Fhe_cipher_arithmetic_cap = access)
        "preview FHE policy used another root") [first, false; second, true];
    require (Lwt_main.run (Store.read_circle_stable_inline_value store circle_id "value") = Some "4")
      "preview changed pending storage";
    require (Result.is_error (Lwt_main.run (Public.load store 91L declarations)))
      "pending asset refusal control failed";
    Store.abort_epoch_batch store)

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
      Lwt_main.run (Host.execute ~inspect_mode:Rule.Prior ~proof_mode:mode ~call ~code_b64
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
    let before = Gc.allocated_bytes () in
    let result = execute Rule.Active call prior_caller in
    let allocated = Gc.allocated_bytes () -. before in
    check "cold transport" 1 result;
    require (allocated < 4_194_304.) "input size check allocated encoded JSON";
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
  else if Array.exists (String.equal "--events") Sys.argv then test_wasm_events ()
  else if Array.exists (String.equal "--inspect") Sys.argv then begin
    test_wasm_inspect ();
    test_inspect_tx ();
    test_inspect_epoch ();
    test_tx_code_reads ();
    test_inspect_rpc ();
    test_preview_code ();
    test_preview_root ();
    test_preview_trust ()
  end
  else begin
  test_inputs ();
  test_view_stop ();
  test_owner ();
  test_async_policy ();
  test_wasm_keys ();
  test_wasm_effort ();
  test_wasm_events ();
  test_preview_trust ();
  test_compute_manifest ();
  test_cache_program_identity ();
  test_wasm_inspect ();
  test_inspect_tx ();
  test_inspect_epoch ();
  test_tx_code_reads ();
  test_inspect_rpc ();
  test_preview_code ();
  test_preview_root ();
  test_wasm_inputs ();
  Printf.printf "status = pass test = circle_program strict_inputs = true\n%!"
  end