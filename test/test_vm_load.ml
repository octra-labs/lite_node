(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_vm
module S = Octra_core.Store_irmin

let check value reason = if not value then failwith reason
let run = Lwt_main.run
let broken name = Array.exists (String.equal name) Sys.argv

let caches () =
  let source = V.Bytecode.encode [|V.Contract_vm.LDI (0, V.Contract_vm.VInt Z.one); V.Contract_vm.STOP|] in
  let first = V.Code_check.decode source |> Result.get_ok in
  for index = 1 to 80 do
    let raw = V.Bytecode.encode [|V.Contract_vm.LDI (0, V.Contract_vm.VInt (Z.of_int index)); V.Contract_vm.STOP|] in
    ignore (V.Code_check.decode ~view:(not (broken "cache")) raw |> Result.get_ok)
  done;
  check (first == (V.Code_check.decode source |> Result.get_ok)) "read traffic evicted execution image"

let loading () =
  Test_workspace.with_dir "vm_load" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let compiled = V.Oct_compile.compile_program
        "program Meter { public view fn read(): int { return 17 } }" in
      check (compiled.error = None) "program compile failed";
      let raw = Option.get compiled.program_envelope in
      let encoded = Base64.encode_exn raw in
      let address = "oct" ^ String.make 44 '1' in
      run (S.deploy_contract store ~address ~code_hash:Digestif.SHA256.(digest_string raw |> to_hex)
        ~version:"1" ~owner:address ~ctype:"CUSTOM" ~check:"source" ~bytecode_b64:encoded);
      let execute active method_name limit =
        let ctx = {V.Contract_vm.default_ctx with scalar_work = active; point_ops = true} in
        V.Contract.execute_call ~ctx ~limit ~journal:(V.Program_journal.create ())
          store address method_name [] address Z.zero in
      let original = V.Code_check.decode_program_source ~point_ops:true raw |> Result.get_ok in
      for index = 1 to 80 do
        let compiled = V.Oct_compile.compile_program
          (Printf.sprintf "program Evict { public view fn read(): int { return %d } }" index) in
        check (compiled.error = None) "cache eviction source failed";
        ignore (V.Code_check.decode_program_source ~point_ops:true
          (Option.get compiled.program_envelope) |> Result.get_ok)
      done;
      let active = not (broken "load") in
      let first = execute active "read" 1_000_000 in
      let loaded = V.Code_check.decode_program_source ~point_ops:true raw |> Result.get_ok in
      check (original != loaded) "first call reused warm image";
      let second = execute active "read" 1_000_000 in
      check (loaded == (V.Code_check.decode_program_source ~point_ops:true raw |> Result.get_ok))
        "second call lost warm image";
      let old = execute false "read" 1_000_000 in
      check (old.success && first.success && first = second) "cold and warm calls differ";
      check (old.return_value = first.return_value) "call output changed";
      let quoted = V.Scalar_work.load (String.length encoded) in
      check (Z.geq (Z.of_int (first.effort_used - old.effort_used)) quoted)
        "program loading is free";
      let absent = execute active "absent" 1_000_000 in
      check (not absent.success && Z.geq (Z.of_int absent.effort_used) quoted)
        "failed call discarded loading charge";
      let refused = execute active "read" 100 in
      check (not refused.success && refused.effort_used = 100) "low allowance accepted";
      let exhausted = V.Contract.call_work ~active:true ~limit:100 ~spent:99
        (Some ("source", encoded)) [] in
      check (exhausted = None) "journal allowance restored";
      let malformed = String.make 70_000 '9' in
      check (V.Contract.priced_load ~active:true ~limit:100 ~spent:99
        ~trusted:[] ~point_ops:true (Some ("source", Base64.encode_exn malformed)) = None)
        "invalid image accepted"))

let constants () =
  let raw = V.Bytecode.encode
    [|V.Contract_vm.LDI (0, V.Contract_vm.VInt (Z.shift_left Z.one 230_000)); V.Contract_vm.STOP|] in
  check (V.Code_check.scalar_cost raw = None) "wide constant admitted";
  check (Result.is_error (V.Code_check.decode ~view:true raw)) "view parsed wide constant";
  check (Result.is_ok (V.Code_check.decode raw)) "historical constant refused"

let compilation_cache () =
  let source index = Printf.sprintf "program Image { public view fn read(): int { return %d } }" index in
  let compiled = V.Program_package.compile_sources_at ~metered:true ~loops:false
    ~compiler:V.Program_package.Protocol ~point_ops:true ~main:"main.aml"
    ~sources:[V.Program_package.{path = "main.aml"; body = source 0}] |> Result.get_ok in
  let first = V.Code_check.decode_program_source ~point_ops:true compiled.envelope |> Result.get_ok in
  for index = 1 to 80 do
    if broken "compile_cache" then ignore (V.Program_package.compile_sources_at ~metered:true
      ~loops:false ~compiler:V.Program_package.Protocol ~point_ops:true ~main:"main.aml"
      ~sources:[V.Program_package.{path = "main.aml"; body = source index}] |> Result.get_ok)
    else ignore (run (V.Contract_rpc.compile_program_source ~point_ops:true (source index)) |> Result.get_ok)
  done;
  check (first == (V.Code_check.decode_program_source ~point_ops:true compiled.envelope |> Result.get_ok))
    "RPC compilation evicted execution image"

let () =
  caches ();
  loading ();
  constants ();
  compilation_cache ();
  Printf.printf "event = vm_load status = passed\n"