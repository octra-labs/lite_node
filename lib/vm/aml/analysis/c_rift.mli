(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type step
type path

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Need of C_type.t * C_type.t
  | Path of int * int
  | Fresh
  | Nodes of Z.t
  | Depth of int

val step : C_syn.name -> C_syn.name -> step
val path : C_syn.name -> step list -> path
val build : path -> C_nat.t -> C_nat.t -> C_syn.typ -> C_syn.t ->
  (C_syn.t, error) result
val make : C_nat.t -> C_nat.t -> C_syn.typ -> C_syn.t ->
  (C_syn.t, error) result
val lower : C_syn.bind list -> C_nat.t -> C_nat.t -> C_syn.typ -> C_syn.t ->
  (C_low.prog, error) result
val check : C_syn.bind list -> C_nat.t -> C_nat.t -> C_syn.typ -> C_syn.t ->
  (C_check.info, error) result
val text : error -> string