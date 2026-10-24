(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_vm

let check value reason = if not value then failwith reason
let get = function Ok value -> value | Error e -> failwith (Code_check.error_message e)

let calls () =
  let source = "program Cached { public view fn read(): int { return 17 } }" in
  let compiled = Oct_compile.compile_program source in
  let raw = Option.get compiled.program_envelope in
  let first = get (Code_check.decode_program_source ~point_ops:true raw) in
  let second = get (Code_check.decode_program_source ~point_ops:true (String.sub raw 0 (String.length raw))) in
  check (first == second) "successful code checked again";
  check (Result.is_error (Code_check.decode_program ~point_ops:true raw))
    "source cache bypassed compiler signature";
  let a = Code_check.code first and b = Code_check.code second in
  check (a != b && a = b) "checked instruction array shared";
  a.(0) <- Contract_vm.REVERT;
  check (Code_check.code (get (Code_check.decode_program_source ~point_ops:true raw)) = b)
    "checked code mutated";
  let state code =
    let value = Local_vm.run ~trace:false (Local_vm.config ~view:true ~method_name:"read" ~args:[] ()) code
      |> Result.get_ok in
    check (value.stop = Local_vm.Returned && value.result = Contract_vm.VInt (Z.of_int 17))
      "checked program execution differs";
    value.effort, value.result, value.storage in
  check (state (Code_check.code first) = state (Code_check.code second)) "warm effort differs";
  let encoded = Base64.encode_exn raw in
  check (Result.is_error (Octra_circle_runtime.Circle_program.decode_bytecode encoded))
    "circle source cache bypassed trust"

let retention () =
  let cache = Code_cache.create () in
  let key raw = Code_cache.key ~mode:0 ~point_ops:false ~trusted:[] raw in
  let count = ref 0 in
  let read raw = Code_cache.run cache (key raw) (fun () -> incr count; Ok (String.make 1 'x')) in
  ignore (read "first"); ignore (read "first");
  check (!count = 1) "cache missed identical input";
  for i = 1 to 64 do ignore (read (string_of_int i)) done;
  ignore (read "first"); check (!count = 66) "cache entry capacity ignored";
  let large = String.make (17 * 1024 * 1024) 'a' in
  ignore (read large); ignore (read "first");
  ignore (read (String.make (17 * 1024 * 1024) 'b'));
  let before = !count in ignore (read large);
  check (!count = before + 1) "cache byte capacity ignored";
  for _ = 1 to 2 do
    ignore (Code_cache.run cache (key "error") (fun () -> incr count; Error "refused"))
  done;
  check (!count = before + 3) "cache retained error";
  (try ignore (Code_cache.run cache (key "exception") (fun () -> raise Out_of_memory))
   with Out_of_memory -> ());
  check (read "after" = Ok "x") "exception retained cache lock"

let performance () =
  let raw = Bytecode.encode (Array.concat [
    [|Contract_vm.LDI (0, Contract_vm.VInt Z.one)|];
    Array.make 1600 (Contract_vm.ADD (0, 0, 0)); [|Contract_vm.STOP|]]) in
  let measure () =
    let before = Gc.allocated_bytes () and started = Sys.time () in
    let value = get (Code_check.decode_deploy raw) in
    value, Gc.allocated_bytes () -. before, Sys.time () -. started in
  let first, cold, cold_time = measure () in
  let second, warm, warm_time = measure () in
  check (first == second && warm < cold /. 10.) "warm program repeated verification work";
  Printf.printf "checked_bytes = %d cold_alloc = %.0f warm_alloc = %.0f cold_s = %.6f warm_s = %.6f\n%!"
    (String.length raw) cold warm cold_time warm_time

let expanded () =
  let regs = List.init 255 (fun _ -> 0) in
  let raw = Bytecode.encode (Array.append [|Contract_vm.STOP|]
    (Array.make 5800 (Contract_vm.EMIT ("e", regs)))) in
  let first = get (Code_check.decode_deploy ~point_ops:true raw) in
  let second = get (Code_check.decode_deploy ~point_ops:true raw) in
  check (Code_check.effects first == Code_check.effects second)
    "large decoded image repeated verification";
  check (Code_check.code first = Code_check.code second) "large checked image changed"

let () = calls (); retention (); performance (); expanded (); print_endline "status = pass test = check_cache"