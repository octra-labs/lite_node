(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type spec = {
  name : string;
  typ : C_type.t;
}

type error =
  | Header
  | Bits
  | Size of int * int
  | Form
  | Count of int * int
  | Cap of C_nat.t * C_nat.t
  | Spec of string
  | Lex of C_lex.error
  | Need of C_lex.form * C_lex.form * C_lex.span
  | Name of string * string * C_lex.span
  | Nat of Z.t * C_lex.span
  | Type of string * C_type.t * C_lex.span
  | Inputs of int * int
  | Input of C_term.id * C_type.t
  | Machine of C_term.id * C_type.t

val spec : string -> C_type.t -> spec
val specs : C_syn.bind list -> (spec list, error) result
val make : C_eval.value list -> (t, error) result
val values : t -> C_eval.value list
val typed : C_type.t -> C_eval.value -> bool
val shaped : C_eval.value -> bool
val encode : t -> string
val decode : string -> (t, error) result
val parse_value : C_type.t -> string -> (C_eval.value, error) result
val parse : spec list -> string -> (t, error) result
val attach : C_term.bind list -> t -> ((C_term.bind * C_eval.value) list, error) result
val term_of : C_type.t -> C_eval.value -> C_term.t option
val close : C_term.bind list -> t -> C_term.t -> (C_term.t, error) result
val text : error -> string