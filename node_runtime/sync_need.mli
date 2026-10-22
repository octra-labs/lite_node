(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cause =
  | Root
  | Journal
  | Conflict
  | Range

type t = {
  cause : cause;
  epoch : int;
  head : int;
  target : int64 option;
}

val label : cause -> string

val cause : string -> cause option

val root : epoch:int -> head:int -> t

val journal : epoch:int -> head:int -> t

val conflict : epoch:int -> head:int -> t

val lost : head:int -> target:int64 -> t option

val range : head:int -> target:int64 -> limit:int -> t option

val valid : t -> bool

val equal : t -> t -> bool

val covers : t -> t -> bool

val carry : t -> t -> t

val snapshot_ready : head:int -> limit:int -> t -> bool

val history_limit : retained:int -> string option -> int

val range_ready : head:int -> limit:int -> t -> bool