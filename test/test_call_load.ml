(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module V = Octra_vm
module R = C.Exec_resource
module S = C.Store_irmin

let check reason value = if not value then failwith reason
let run = Lwt_main.run

let clear_images () =
  for index = 0 to 64 do
    let raw = V.Bytecode.encode V.Contract_vm.[|LDI (0, VInt (Z.of_int index)); STOP|] in
    check "cache input refused" (Result.is_ok (V.Code_check.decode ~point_ops:true raw))
  done

let () =
  let body = "program Load { public fn read(): int { let result = orbit[8000] from 0 with once x: int => x + 1 return result } }" in
  let package = V.Program_package.compile ~main:"main.aml"
    ~sources:[{path = "main.aml"; body}]
    |> Result.map_error V.Program_package.error_message in
  let raw = (match package with Ok value -> value | Error reason -> failwith reason).envelope in
  let image = V.Program_envelope.decode raw |> Result.get_ok in
  check "load program too small" (Array.length (V.Bytecode.decode_exn image.code) > 20_000);
  let encoded = Base64.encode_exn raw in
  clear_images ();
  Test_workspace.with_dir "call_load" (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let address = "oct" ^ String.make 44 '1' in
      run (S.deploy_contract store ~address ~code_hash:Digestif.SHA256.(digest_string raw |> to_hex)
        ~version:"1" ~owner:address ~ctype:"CUSTOM" ~check:"source" ~bytecode_b64:encoded);
      let root = run (S.state_hash store) in
      let ctx = {V.Contract_vm.default_ctx with point_ops = true; scalar_work = true;
        proof_exec = C.Rule_graph.Active} in
      let journal = V.Program_journal.create () in
      let saved = V.Program_journal.snapshot journal in
      let call () = V.Contract.execute_call_async ~ctx ~limit:10_000_000 ~journal
        store address "absent" [] address Z.zero in
      let cap = 16 * String.length raw + 1_000_000 in
      Gc.full_major ();
      let before = Gc.allocated_bytes () and counter = Mtime_clock.counter () in
      let pending = call () in
      let allocated = Gc.allocated_bytes () -. before in
      let elapsed = Mtime.Span.to_float_ns (Mtime_clock.count counter) /. 1e6 in
      Printf.printf "event = call_load input_bytes = %d allocated = %.0f cap = %d return_ms = %.3f\n%!"
        (String.length raw) allocated cap elapsed;
      check "cold call blocked scheduler"
        (allocated < float_of_int cap);
      check "loading changed journal" (V.Program_journal.snapshot journal = saved);
      let first = run pending in
      let second = V.Contract.execute_call ~ctx ~limit:10_000_000 ~journal
        store address "absent" [] address Z.zero in
      check "async call result differs" (first = second && not first.success
        && first.error = Some "method not found");
      clear_images ();
      let current = ref true in
      let accepted = ref false in
      let pending = R.select ~running:(fun () -> !current) (fun () ->
        R.run ~hash:"loading" (fun () ->
          let open Lwt.Syntax in
          let* _ = call () in
          accepted := true;
          Lwt.return_unit)) in
      current := false;
      let refused = run (Lwt.catch (fun () -> Lwt.map (fun () -> false) pending)
        (function R.Exhausted ("loading", R.Time) -> Lwt.return_true
          | error -> Lwt.fail error)) in
      check "closed call accepted load" (refused && not !accepted);
      clear_images ();
      let busy = Lwt_preemptive.nbthreadsbusy () in
      let pending = call () in
      Lwt.cancel pending;
      let cancelled = run (Lwt.catch (fun () -> Lwt.map (fun _ -> false) pending)
        (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error)) in
      check "call cancellation lost" cancelled;
      check "cancelled load retained worker" (Lwt_preemptive.nbthreadsbusy () = busy);
      check "loading retained journal changes" (V.Program_journal.snapshot journal = saved);
      check "loading changed store" (run (S.state_hash store) = root);
      Printf.printf "event = call_load status = passed\n%!"))