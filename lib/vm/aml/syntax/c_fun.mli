(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type arr = {
  mul : C_type.mul;
  caps : C_syn.bind list;
  arg : C_syn.bind;
  out : C_syn.typ;
  eff : C_eff.t;
  lim : C_limit.t option;
}

type fn = {
  name : C_syn.name;
  arr : arr;
  body : C_syn.t;
}

type t =
  | Ret of C_syn.t
  | Let of C_syn.bind * C_syn.t * t
  | If of C_syn.t * t * t
  | Call of C_syn.bind * C_syn.name * C_syn.t list * C_syn.t * t

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Dup of string
  | Fn of string
  | Direct of string
  | Mode of string
  | Arity of string * int * int
  | Out of string
  | Atom of string
  | Mark of string * C_eff.t * C_eff.t
  | Limit of string
  | Over of string * C_limit.t * C_limit.t
  | Fns of int * int
  | Depth of int * int
  | Nodes of int * int

val arr : C_type.mul -> C_syn.bind list -> C_syn.bind -> C_syn.typ -> arr
val marks : arr -> C_eff.t -> arr
val under : arr -> C_limit.t -> arr
val fn : C_syn.name -> arr -> C_syn.t -> fn
val def : fn -> (unit, error) result
val defs : fn list -> (unit, error) result
val direct : fn -> bool
val apply : fn -> C_syn.t list -> (C_syn.t, error) result
val names :
  C_syn.bind list -> fn list -> t ->
  ((C_term.id * string) list, error) result
val lower : C_syn.bind list -> fn list -> t -> (C_low.prog, error) result
val check : C_syn.bind list -> fn list -> t -> (C_check.info, error) result
val text : error -> string