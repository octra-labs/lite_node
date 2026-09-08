(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kind = Data | Res
type par
type actual
type fn

type error =
  | Dup of string
  | Count of int * int
  | Arity of string * int * int
  | Kind of string * kind * C_type.kind
  | Name of string
  | Decl of C_decl.error
  | Raw of C_raw.error
  | Spec of C_spec.error

val par : C_syn.name -> kind -> par
val fn : par list -> C_spec.fn -> (fn, error) result
val name : fn -> C_syn.name
val pars : fn -> par list
val pname : par -> C_syn.name
val pkind : par -> kind
val base : fn -> C_spec.fn
val actuals : C_idx.env -> par list -> C_decl.typ list ->
  (actual list, error) result
val compare_actuals : actual list -> actual list -> int
val mono : C_idx.env -> C_syn.name -> fn -> actual list -> C_nat.t list ->
  (C_fun.fn, error) result
val inst : C_idx.env -> C_syn.name -> fn -> C_decl.typ list -> C_idx.t list ->
  (actual list * C_nat.t list * C_fun.fn, error) result
val text : error -> string