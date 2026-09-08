(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type field
type decl
type item
type pick

type error =
  | Data of C_data.error
  | Low of C_low.error
  | Check of C_check.error
  | Empty
  | Count of int * int
  | Dup of string
  | Bind_dup of string
  | Items of int * int
  | Order of string * string
  | Type of string
  | Mode of string * C_type.mul * C_type.mul
  | Id

val field : C_syn.name -> C_syn.typ -> field
val item : C_syn.name -> C_syn.t -> item
val pick : C_syn.name -> C_syn.bind -> pick
val decl : bool list -> C_syn.name -> field list -> (decl, error) result
val name : decl -> C_syn.name
val bits : decl -> bool list
val fields : decl -> (C_syn.name * C_syn.typ) list
val typ : decl -> C_syn.typ
val make : decl -> item list -> (C_syn.t, error) result
val split : decl -> C_syn.t -> pick list -> C_syn.t -> (C_syn.t, error) result
val lower : C_syn.bind list -> decl -> C_syn.t -> pick list -> C_syn.t ->
  (C_low.prog, error) result
val check : C_syn.bind list -> decl -> C_syn.t -> pick list -> C_syn.t ->
  (C_check.info, error) result
val text : error -> string