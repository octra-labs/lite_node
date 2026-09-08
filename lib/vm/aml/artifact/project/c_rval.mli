(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type error =
  | Header
  | Bits
  | Size of int * int
  | Form

val make : C_type.t -> C_eval.value -> (t, error) result
val typ : t -> C_type.t
val value : t -> C_eval.value
val encode : t -> string
val decode : string -> (t, error) result
val equal : t -> t -> bool
val text : error -> string