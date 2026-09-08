(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type sign = Pos | Neg

type rule =
  | Base
  | Prod of C_nat.t * C_nat.t

type layer = private {
  tag : C_nat.t;
  rule : rule;
}

type edge = private {
  layer : C_nat.t;
  index : C_nat.t;
  sign : sign;
  weight : Z.t list;
  sigma : bool list;
}

type cfg = private {
  basis : C_nat.t;
  slots : C_nat.t;
  sigma : C_nat.t;
  edge_cap : Z.t;
}

type graph = private {
  layers : layer list;
  edges : edge list;
}

type info = private {
  graph : graph;
  merged : bool;
  layer_count : Z.t;
  edge_count : Z.t;
  removed : Z.t;
}

type error = Cfg | Layer | Edge | Shape

val cfg :
  basis:C_nat.t -> slots:C_nat.t -> sigma:C_nat.t -> edge_cap:Z.t ->
  (cfg, error) result

val base : tag:C_nat.t -> layer
val prod : tag:C_nat.t -> left:C_nat.t -> right:C_nat.t -> layer

val edge :
  layer:C_nat.t -> index:C_nat.t -> sign:sign -> weight:Z.t list ->
  sigma:bool list -> (edge, error) result

val graph : layer list -> edge list -> graph
val valid : cfg -> graph -> bool
val mul_depth : graph -> (C_nat.t, error) result
val norm : cfg -> graph -> (info, error) result
val text : error -> string