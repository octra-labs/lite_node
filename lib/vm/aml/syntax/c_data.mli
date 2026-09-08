(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ctor
type decl
type arm

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Empty
  | Tag of int * int
  | Count of int * int
  | Dup of string
  | Ctor of string
  | Arms of int * int
  | Order of string * string
  | Type of string
  | Mode of string * C_type.mul * C_type.mul
  | Id

val ctor : C_syn.name -> C_syn.typ -> ctor
val tag_len : int
val decl : bool list -> C_syn.name -> ctor list -> (decl, error) result
val name : decl -> C_syn.name
val bits : decl -> bool list
val ctors : decl -> (C_syn.name * C_syn.typ) list
val arm : C_syn.name -> C_syn.bind -> C_syn.t -> arm
val typ : decl -> C_syn.typ
val make : decl -> C_syn.name -> C_syn.t -> (C_syn.t, error) result
val case : decl -> C_syn.t -> arm list -> (C_syn.t, error) result
val lower : C_syn.bind list -> decl -> C_syn.t -> arm list ->
  (C_low.prog, error) result
val check : C_syn.bind list -> decl -> C_syn.t -> arm list ->
  (C_check.info, error) result
val text : error -> string