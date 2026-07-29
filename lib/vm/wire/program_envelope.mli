(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  code : string;
  cert : string;
}

type error

val is_program : string -> bool
val encode : code:string -> cert:string -> (string, error) result
val decode : string -> (t, error) result
val error_message : error -> string