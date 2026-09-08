(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t
type env
type rel = Eq | Le

type view =
  | VLit of C_nat.t
  | VVar of C_syn.name
  | VAdd of t * t
  | VMul of t * t
  | VMax of t * t
  | VSub of t * t

type error =
  | Free of string
  | Dup of string
  | Nat of Z.t
  | Depth of int * int
  | Nodes of int * int
  | Vars of int * int
  | False of rel * C_nat.t * C_nat.t

val lit : Z.t -> t option
val exact : C_nat.t -> t
val var : C_syn.name -> t
val add : t -> t -> t
val mul : t -> t -> t
val max : t -> t -> t
val sub : t -> t -> t
val view : t -> view

val empty : env
val env : (C_syn.name * Z.t) list -> (env, error) result
val put : env -> C_syn.name -> t -> (env, error) result
val eval : env -> t -> (C_nat.t, error) result
val eq : env -> t -> t -> (bool, error) result
val le : env -> t -> t -> (bool, error) result
val hold : env -> rel -> t -> t -> (unit, error) result
val text : error -> string