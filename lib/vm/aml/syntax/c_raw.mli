(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bind = {
  name : C_syn.name;
  mul : C_type.mul;
  typ : C_decl.typ;
}

type arm
type item
type pick

type t =
  | KUnit
  | KBool of bool
  | KInt of Z.t
  | KBytes of string
  | KVec of C_decl.typ * t list
  | KSeq of C_idx.t * C_decl.typ * t list
  | Var of C_syn.name
  | Let of bind * t * t
  | If of t * t * t
  | Pair of t * t
  | Unpair of t * bind * bind * t
  | Fst of t
  | Snd of t
  | Inl of t * C_decl.typ
  | Inr of C_decl.typ * t
  | Case of t * bind * t * bind * t
  | Act of C_syn.atom * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Neg of t
  | Abs of t
  | Fit of C_decl.typ * t
  | Wide of t
  | Length of t
  | Eq of C_decl.typ * t * t
  | Cmp of C_syn.rel * t * t
  | Cat of t * t
  | Take of Z.t * t
  | Drop of Z.t * t
  | Vcat of t * t
  | At of Z.t * t
  | Uncons of t
  | Vfold of t * t * fold
  | Step of t * t
  | Close of t
  | Dmake of C_data.decl * C_syn.name * t
  | Dcase of C_data.decl * t * arm list
  | Rmake of C_rec.decl * item list
  | Rsplit of C_rec.decl * t * pick list * t
  | Quant of C_quant.mode * t * bind * t
  | Weave of C_idx.t * C_decl.typ * t * bind * t
  | Braid of C_idx.t * C_decl.typ * t * t * bind * bind * t
  | Loom of C_idx.t * C_decl.typ * bind * t
  | Orbit of C_idx.t * t * bind * t
  | Orbit_to of C_idx.t * t * t * bind * t
  | Wake of C_idx.t * t * bind * t
  | Rift of C_idx.t * C_idx.t * C_decl.typ * t

and fold = {
  fitem : bind;
  fstate : bind;
  fbody : t;
}

type error =
  | Idx of C_idx.error
  | Decl of C_decl.error
  | Data of C_data.error
  | Rec of C_rec.error
  | Quant_error of C_quant.error
  | Weave_error of C_weave.error
  | Braid_error of C_braid.error
  | Loom_error of C_loom.error
  | Orbit_error of C_orbit.error
  | Wake_error of C_wake.error
  | Rift_error of C_rift.error
  | Fresh
  | Depth of int * int
  | Nodes of int * int

val bind : C_syn.name -> C_type.mul -> C_decl.typ -> bind
val fold : bind -> bind -> t -> fold
val arm : C_syn.name -> bind -> t -> arm
val item : C_syn.name -> t -> item
val pick : C_syn.name -> bind -> pick
val arm_view : arm -> C_syn.name * bind * t
val item_view : item -> C_syn.name * t
val pick_view : pick -> C_syn.name * bind
val of_syn : C_syn.t -> t
val elab_trace : C_idx.env -> t -> (C_syn.t * C_syn.t list, error) result
val elab : C_idx.env -> t -> (C_syn.t, error) result
val text : error -> string