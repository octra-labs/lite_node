(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val version : int

val rules_id : string

val hash :
  (string -> string option) ->
  string

val validate :
  (string -> string option) ->
  (unit, string) result