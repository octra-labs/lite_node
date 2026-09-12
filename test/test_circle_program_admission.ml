(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Vm = Octra_vm
module Circles = Octra_core.Circles
module Store = Octra_core.Store_irmin
module Circle_program = Octra_circle_runtime.Circle_program
module Circle_exec = Octra_circle_runtime.Circle_exec

let fail msg =
  failwith ("test_circle_program_admission: " ^ msg)

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
      ("runtime_data/circle_program_admission_" ^ string_of_int (Unix.getpid ()))
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
      "program CircleTyped { fn echo(x: int): int { return x } }" in
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

let save_circle store circle_id owner code_b64 =
  let raw = Base64.decode_exn code_b64 in
  let info = {
    Circles.circle_id;
    runtime = Circles.Octb;
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

let test_admission_and_inputs () =
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
  test_admission_and_inputs ();
  test_view_stop ();
  test_compute_manifest ();
  test_cache_program_identity ();
  Printf.printf "circle_program_admission = 1\nstrict_circle_inputs = 1\nPASS\n%!"