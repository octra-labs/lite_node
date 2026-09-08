(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lit =
  | Bool of bool
  | Int of Z.t
  | Bytes of string
  | Data of C_rval.t
  | Cap of C_nat.t * C_nat.t

type op =
  | Load of lit
  | Stop

type t = private {
  lit : lit;
  ops : op list;
  octb : string;
  span : C_lex.span;
}

type error =
  | Source of C_parse.error
  | Inputs of int
  | Effects of C_eff.atom list
  | Run of C_eval.error
  | Plan of C_eff.atom list
  | Value of C_type.t * C_eval.value

val compile : string -> (t, error) result
val text : error -> string