(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type typ =
  | Unit
  | Bool
  | Int
  | Num of C_type.sign * C_idx.t
  | Var of C_syn.name
  | Bytes of C_idx.t
  | Vec of C_idx.t * typ
  | Seq of C_idx.t * typ
  | Cap of Z.t
  | Pair of typ * typ
  | Result of typ * typ
  | Exact of C_syn.typ

type input

type error =
  | Name of string
  | Dup of string
  | Nat of Z.t
  | Depth of int * int
  | Nodes of int * int
  | Mode of string * C_type.mul
  | Count of int * int
  | Idx of C_idx.error
  | Low of C_low.error

val input : string -> C_type.mul -> typ -> input
val typ : typ -> (C_syn.typ, error) result
val typ_in : C_idx.env -> typ -> (C_syn.typ, error) result
val binds : input list -> (C_syn.bind list, error) result
val binds_in : C_idx.env -> input list -> (C_syn.bind list, error) result
val prog : input list -> C_syn.t -> (C_low.prog, error) result
val prog_in : C_idx.env -> input list -> C_syn.t -> (C_low.prog, error) result
val text : error -> string