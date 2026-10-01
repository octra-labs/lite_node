(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

exception Backend_unavailable of string

external initialize_raw : unit -> unit = "caml_zk_initialize"

external verify_raw : bytes -> bytes -> bytes -> bool
  = "caml_zk_groth16_verify_bn254"

let initialize () =
  try initialize_raw () with Failure reason -> raise (Backend_unavailable reason)

let groth16_verify_bn254 key proof inputs =
  initialize ();
  verify_raw key proof inputs