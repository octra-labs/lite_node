(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type value =
  | Unit
  | Bool of bool
  | Int of Z.t
  | Bytes of string
  | Vec of value array
  | Cap of C_nat.t * C_nat.t
  | Enc of C_nat.t * C_nat.t * C_fp.t
  | Pair of value * value
  | Inl of value
  | Inr of value

type origin = private
  | Direct
  | Held of C_nat.t * C_nat.t

type action = private {
  atom : C_eff.atom;
  payload : value;
  origin : origin;
}

type out = {
  value : value;
  plan : C_eff.atom list;
  actions : action list;
  steps : Z.t;
  work : Z.t;
  row : C_eff.t;
  limit : Z.t;
  work_limit : Z.t;
}

type exec = private {
  rule : C_rule.id;
  out : out;
}

type error =
  | Static of C_check.error
  | Missing of C_term.id
  | Need_bool
  | Need_int
  | Divide_zero
  | Modulo_zero
  | Need_pair
  | Need_sum
  | Need_bytes
  | Need_vec
  | Need_cap
  | Need_seq
  | Byte_index of C_nat.t * C_nat.t
  | Vec_index of C_nat.t * C_nat.t
  | Input of C_term.id
  | Input_type of C_term.id * C_type.t
  | Input_depth of C_term.id * int * int
  | Input_nodes of C_term.id * int * int
  | Cap_dup of C_nat.t * C_nat.t
  | Fuel of Z.t * Z.t
  | Cost of Z.t * Z.t
  | Work of Z.t * Z.t
  | Effects of C_eff.t * C_eff.t

type rule_error =
  | Rule of C_rule.error
  | Run of error

val max_cost : Z.t
val equal : value -> value -> bool
val zero : C_type.t -> value option
val typed : C_type.t -> value -> bool
val atoms : action list -> C_eff.atom list
val direct : C_eff.atom -> value -> action
val held : C_eff.atom -> value -> C_nat.t -> C_nat.t -> action
val int_text : Z.t -> string
val value_text : value -> string
val run : ?fuel:Z.t -> C_term.t -> (out, error) result
val run_in : ?fuel:Z.t -> (C_term.bind * value) list -> C_term.t -> (out, error) result
val run_at : C_rule.schedule -> epoch:Z.t -> C_term.t -> (exec, rule_error) result
val run_in_at :
  C_rule.schedule -> epoch:Z.t -> (C_term.bind * value) list -> C_term.t ->
  (exec, rule_error) result
val text : error -> string
val rule_text : rule_error -> string