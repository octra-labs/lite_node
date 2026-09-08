(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type scope = private {
  chain : string;
  prog : string;
  root : string;
}

type token = private {
  scope : scope;
  kind : C_nat.t;
  id : C_nat.t;
  rev : C_nat.t;
}

type cell = private {
  kind : C_nat.t;
  id : C_nat.t;
  rev : C_nat.t;
  live : bool;
}

type state

type out = private {
  eval : C_eval.out;
  state : state;
  tokens : token list;
}

type field =
  | Chain
  | Prog
  | Root
  | Kind
  | Id
  | Rev

type error =
  | Nat of field * Z.t
  | Length of field * int
  | Scope
  | Order of (C_nat.t * C_nat.t) * (C_nat.t * C_nat.t)
  | Exists of (C_nat.t * C_nat.t)
  | Absent of (C_nat.t * C_nat.t)
  | Stale of C_nat.t * C_nat.t * C_nat.t * C_nat.t
  | Tdup of (C_nat.t * C_nat.t)
  | Tmiss of (C_nat.t * C_nat.t)
  | Textra of (C_nat.t * C_nat.t)
  | Vdup of (C_nat.t * C_nat.t)
  | Vdepth of int * int
  | Vnodes of int * int
  | Icount of int * int
  | Scount of int * int
  | New of (C_nat.t * C_nat.t)
  | Rev_max of (C_nat.t * C_nat.t)
  | Eval of C_eval.error

val scope : chain:string -> prog:string -> root:string -> (scope, error) result
val scope_equal : scope -> scope -> bool
val token : scope -> kind:Z.t -> id:Z.t -> rev:Z.t -> (token, error) result
val cell : kind:Z.t -> id:Z.t -> rev:Z.t -> live:bool -> (cell, error) result
val empty : scope -> state
val scope_of : state -> scope
val view : state -> scope * cell list
val of_cells : scope -> cell list -> (state, error) result
val issue : state -> kind:Z.t -> id:Z.t -> (state * token, error) result
val current : state -> token -> bool
val take : state -> token -> keep:bool -> (state * token option, error) result

val run :
  ?fuel:Z.t ->
  state ->
  token list ->
  (C_term.bind * C_eval.value) list ->
  C_term.t ->
  (out, error) result

val token_text : token -> string
val text : error -> string