(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Nodes of Z.t
  | Depth of int

type stat = {
  nodes : int;
  depth : int;
}

val stat : C_syn.t -> (stat, error) result
val fit : Z.t -> int -> (unit, error) result
val clear : C_syn.name list -> C_syn.bind list -> C_syn.t list ->
  C_syn.name -> bool
val clearn : C_syn.name list -> C_syn.bind list -> C_syn.t list ->
  C_syn.name list -> bool
val pick : C_syn.name list -> C_syn.bind list -> C_syn.t list ->
  C_syn.name option
val pickn : int -> C_syn.name list -> C_syn.bind list -> C_syn.t list ->
  C_syn.name list option