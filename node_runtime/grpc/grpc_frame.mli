(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Compressed
  | Extra
  | Incomplete
  | Too_large

val encode : string -> string

val decode :
  max_message:int ->
  string ->
  (bytes, error) result