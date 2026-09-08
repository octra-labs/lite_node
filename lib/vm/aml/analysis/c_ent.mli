(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rate = private {
  num : Z.t;
  shift : C_nat.t;
}

type profile = private {
  basis : C_nat.t;
  max_depth : C_nat.t;
  base : Z.t;
  slope : Z.t;
  r2 : rate;
  r3 : rate;
}

type plan = private {
  depth : C_nat.t;
  cap : Z.t;
  n2 : Z.t;
  n3 : Z.t;
  groups : Z.t;
  edges : Z.t;
}

type mode =
  | Binary64
  | Integer

type limit = private {
  rule : C_rule.id;
  mode : mode;
  plan : plan;
}

type error =
  | Profile
  | Depth of Z.t * Z.t
  | Rule of C_rule.error

val prod : profile
val valid : profile -> bool
val plan : profile -> C_nat.t -> (plan, error) result
val mode : C_rule.id -> mode
val bind : C_rule.id -> profile -> C_nat.t -> (limit, error) result
val bind_at :
  C_rule.schedule -> epoch:Z.t -> profile -> C_nat.t -> (limit, error) result
val text : error -> string