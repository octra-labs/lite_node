(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type op =
  | Load of C_emit.lit
  | Stop

type frame = {
  pc : int;
  op : op;
  reg : C_emit.lit option;
  effort : int;
  halted : bool;
  span : C_lex.span;
}

type t = private {
  frames : frame list;
  result : C_emit.lit;
  octb : string;
}

type error =
  | Emit of C_emit.error
  | Shape
  | Result

val make : string -> (t, error) result
val replay : frame list -> (C_emit.lit, error) result
val text : error -> string