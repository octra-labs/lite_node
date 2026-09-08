(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type error =
  | Empty
  | Size of int
  | Point of int
  | Span of int
  | Bits

val max : int
val make : C_lex.span array -> (t, error) result
val spans : t -> C_lex.span array
val length : t -> int
val get : t -> int -> C_lex.span
val equal : t -> t -> bool
val enc : t -> C_bin.bits
val dec : C_bin.bits -> t option
val text : error -> string