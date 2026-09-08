(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

type out = private {
  eval : C_eval.out;
  state : bits;
  tokens : bits list;
  root : string;
}

type error =
  | Program
  | State
  | Input of int
  | Token of int
  | Arity of int * int
  | Live
  | Static of C_check.error
  | Session of C_sess.error
  | Root
  | Seal

val verify : program:bits -> prior:string -> state:bits -> root:string -> bool

val run :
  program:bits ->
  prior:string ->
  state:bits ->
  inputs:bits list ->
  tokens:bits list ->
  (out, error) result

val text : error -> string