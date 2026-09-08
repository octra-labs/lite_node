(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kind =
  | Load
  | Move
  | Plus
  | Times
  | Quotient
  | Remainder
  | Negate
  | Absolute
  | Same
  | Different
  | Less
  | Greater
  | Join
  | Minus
  | Size
  | Slice
  | Cap_close
  | Jump
  | Jump_if
  | Mark
  | Noop
  | Stop

type frame = private {
  pc : int;
  kind : kind;
  span : C_lex.span;
  slots : C_live.slot list;
}

type t = private {
  code : C_mach.code;
  frames : frame array;
  shape : C_type.t;
  row : C_eff.atom list;
  eff : C_eff.atom list;
  used : C_limit.t;
  res : C_limit.t;
  depth : Z.t;
  result : C_emit.lit;
}

type cause =
  | Source
  | Feed
  | Machine
  | Slots
  | Map
  | Run

val make : string -> (t, cause) result
val make_feed : string -> C_feed.t -> (t, cause) result
val make_in : string -> C_eval.value list -> (t, cause) result
val replay : t -> C_emit.lit list option
val text : cause -> string