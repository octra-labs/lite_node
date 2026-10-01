(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external failed_initialize : unit -> unit = "caml_zk_failed_initialize"
external failed_verify : bytes -> bytes -> bytes -> bool = "caml_zk_failed_verify"

let refuses run =
  match run () with
  | _ -> failwith "mcl failure was accepted"
  | exception Failure reason ->
    if reason <> "mcl initialization failed" then failwith reason

let () =
  Zk_ffi.initialize ();
  Zk_ffi.initialize ();
  List.iter (fun () ->
    refuses failed_initialize;
    refuses (fun () -> failed_verify Bytes.empty Bytes.empty Bytes.empty)) [(); ()];
  if Zk_ffi.groth16_verify_bn254 Bytes.empty Bytes.empty Bytes.empty then
    failwith "invalid proof was accepted";
  Printf.printf "event = zk_start status = passed\n"