(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t
type veil

type fdecl =
  | Mono of C_fun.fn
  | Spec of C_spec.fn
  | Poly of C_poly.fn

type cause =
  | Lex of C_lex.error
  | Held of C_lex.hold
  | Need of C_lex.form list * C_lex.form
  | Name of string
  | Size_name of string
  | Size_dup of string
  | Idx of C_idx.error
  | Decl of C_decl.error
  | Fun of C_fun.error
  | Inst of C_spec.error
  | Poly_error of C_poly.error
  | Data of C_data.error
  | Rec of C_rec.error
  | Quant of C_quant.error
  | Weave of C_weave.error
  | Braid of C_braid.error
  | Loom of C_loom.error
  | Orbit of C_orbit.error
  | Wake of C_wake.error
  | Rift of C_rift.error
  | Private
  | Fhe of C_fhe.error
  | Hfhe of C_hfhe.error
  | Hpar of C_hpar.error
  | Word of string * string
  | Veil_dup of string
  | Data_name of string
  | Data_dup of string
  | Shape_name of string
  | Shape_dup of string
  | Tag of Z.t
  | Tag_dup of Z.t
  | Mark_nat of Z.t
  | Mark_dup of string
  | Under_nat of Z.t
  | Empty
  | Perm of C_perm.error
  | Low of C_low.error
  | Check of C_check.error * string option
  | Depth of int * int
  | Nodes of int * int

type error = {
  cause : cause;
  span : C_lex.span;
}

val parse : string -> (t, error) result
val name : t -> C_syn.name
val binds : t -> (C_syn.bind list, C_decl.error) result
val perms : t -> C_perm.set
val dtypes : t -> (Z.t * C_data.decl) list
val rtypes : t -> (Z.t * C_rec.decl) list
val decls : t -> fdecl list
val forms : t -> C_fun.fn list
val body : t -> C_fun.t
val body_marks : t -> C_lex.span option list
val body_span : t -> C_lex.span
val veils : t -> veil list
val veil_name : veil -> C_syn.name
val veil_info : veil -> C_fhe.info
val veil_param : veil -> C_fhe.param
val veil_host : veil -> C_fhe.host
val veil_stage : veil -> C_hfhe.info option
val veil_link : veil -> C_hpar.link option
val veil_count : t -> int
val veil_depth : t -> C_nat.t
val res : t -> C_limit.t -> C_limit.t
val lower : t -> (C_low.prog, error) result
val compile : t -> (C_low.prog * C_check.info, error) result
val check : t -> (C_check.info, error) result
val source : string -> (C_low.prog, error) result
val text : error -> string