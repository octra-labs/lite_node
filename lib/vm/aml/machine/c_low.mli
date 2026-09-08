(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Nat of Z.t
  | Free of string
  | Dup of string
  | Type
  | Count
  | Depth of int * int
  | Nodes of int * int
  | Inputs of int * int

type prog = {
  inputs : C_term.bind list;
  term : C_term.t;
}

val term : C_syn.t -> (C_term.t, error) result
val typ : C_syn.typ -> (C_type.t, error) result
val prog : C_syn.bind list -> C_syn.t -> (prog, error) result
val names :
  C_syn.bind list -> C_syn.t -> (C_term.id * string) list option
val text : error -> string