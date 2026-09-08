(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Unknown of C_term.id
  | Used of C_term.id
  | Ghost of C_term.id
  | Shadow of C_term.id
  | Need of C_type.t * C_type.t
  | Bad of C_type.t
  | Byte_index of C_nat.t * C_nat.t
  | Vec_index of C_nat.t * C_nat.t
  | Seq_size of C_nat.t * C_nat.t
  | Range_type of C_type.t
  | Size
  | Mode of C_term.id * C_type.mul
  | Split of C_term.id list
  | Repeat of C_term.id list
  | Erase of C_term.id list
  | Unused of C_term.id
  | Impure of C_term.id
  | Access of C_type.t
  | Drop_res of C_type.t
  | Depth of int * int
  | Nodes of int * int
  | Inputs of int * int

type origin =
  | Direct
  | Held of C_nat.t

type site = private {
  atom : C_eff.atom;
  payload : C_type.t;
  origin : origin;
}

type flow = private
  | Pure
  | Action of site
  | Seq of flow * flow
  | Fork of flow * flow
  | Loop of C_nat.t * flow

type location = private {
  index : int;
  site : site;
  count : Z.t;
}

type info = {
  typ : C_type.t;
  eff : C_eff.t;
  res : C_limit.t;
  flow : flow;
}

type selected = private {
  rule : C_rule.id;
  info : info;
}

type failure = {
  error : error;
  term : C_term.t option;
}

type rule_error =
  | Rule of C_rule.error
  | Check of error

val max_depth : int
val max_nodes : int
val max_inputs : int
val check : C_term.t -> (info, error) result
val check_in : C_term.bind list -> C_term.t -> (info, error) result
val check_in_located :
  C_term.bind list -> C_term.t -> (info, failure) result
val check_at : C_rule.schedule -> epoch:Z.t -> C_term.t -> (selected, rule_error) result
val check_in_at :
  C_rule.schedule -> epoch:Z.t -> C_term.bind list -> C_term.t ->
  (selected, rule_error) result
val locations : info -> location list
val sites : info -> site list
val text : error -> string
val text_named : (C_term.id -> string option) -> error -> string
val rule_text : rule_error -> string