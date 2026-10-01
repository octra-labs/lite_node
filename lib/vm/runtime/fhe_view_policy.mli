(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type shape = {
  slots : int;
  layers : int;
  edges : int;
}

val multiplication_effort :
  left:shape ->
  right:shape ->
  int option

val additional_effort :
  left:shape ->
  right:shape ->
  int option

type op = Copy | Join | Product

val consensus_id : string
val sample_factor : int
val sampling : rows:int -> columns:int -> weight:int -> noise:int -> branches:int -> int option

val work : op -> base:int -> left_words:int -> right_words:int -> product_words:int -> sample_work:int ->
  left:shape -> right:shape -> int option

val plan : op -> base:int -> left_words:int -> right_words:int -> product_words:int -> sample_work:int ->
  left:shape -> right:shape -> (int * Z.t) option