(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bind = {
  name : C_syn.name;
  mul : C_type.mul;
  typ : C_decl.typ;
}

type arr = {
  mul : C_type.mul;
  caps : bind list;
  arg : bind;
  out : C_decl.typ;
  eff : C_eff.t;
  lim : C_limit.t option;
}

type fn = {
  name : C_syn.name;
  pars : C_syn.name list;
  laws : C_law.set;
  arr : arr;
  body : C_raw.t;
}

type error =
  | Dup of string
  | Count of int * int
  | Arity of string * int * int
  | Idx of C_idx.error
  | Law of C_law.error
  | Decl of C_decl.error
  | Raw of C_raw.error
  | Fun of C_fun.error

val bind : C_syn.name -> C_type.mul -> C_decl.typ -> bind
val arr : C_type.mul -> bind list -> bind -> C_decl.typ -> arr
val marks : arr -> C_eff.t -> arr
val under : arr -> C_limit.t -> arr
val fn : C_syn.name -> C_syn.name list -> arr -> C_raw.t -> (fn, error) result
val require : C_law.set -> fn -> fn
val args : C_idx.env -> C_idx.t list -> (C_nat.t list, error) result
val mono : C_idx.env -> C_syn.name -> fn -> C_nat.t list ->
  (C_fun.fn, error) result
val inst : C_idx.env -> C_syn.name -> fn -> C_idx.t list ->
  (C_nat.t list * C_fun.fn, error) result
val text : error -> string