(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type slot = private {
  id : C_term.id;
  mul : C_type.mul;
  live : bool;
}

type t

type error =
  | Empty
  | Size of int
  | Row of int * int
  | Id of int * C_term.id
  | Mul of int * C_term.id
  | Dup of int * C_term.id
  | Bits

val slot : C_term.id -> C_type.mul -> bool -> slot
val make : slot list array -> (t, error) result
val rows : t -> slot list array
val length : t -> int
val get : t -> int -> slot list
val equal : t -> t -> bool
val enc : t -> C_bin.bits
val dec : C_bin.bits -> t option
val text : error -> string