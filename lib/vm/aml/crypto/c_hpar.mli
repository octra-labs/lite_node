(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type set = private {
  id : C_nat.t;
  key : C_nat.t;
  fbits : Z.t;
  basis : Z.t;
  rows : Z.t;
  cols : Z.t;
  hwt : Z.t;
  xwt : Z.t;
  ewt : Z.t;
  edges : Z.t;
  ln : Z.t;
  lt : Z.t;
  tn : Z.t;
  td : Z.t;
  prf : Z.t;
  adm : C_hfhe.adm;
  cert : C_hfhe.cert;
}

type seal
type catalog

type cap = private {
  hbits : Z.t;
  hwords : Z.t;
  perms : Z.t;
  roots : Z.t;
  swords : Z.t;
  ywords : Z.t;
  twords : Z.t;
  ewords : Z.t;
  evals : Z.t;
}

type link = private {
  set : set;
  param : C_fhe.param;
  fhe : C_fhe.info;
  hfhe : C_hfhe.info;
  cap : cap;
  graph : C_graph.cfg;
  ent : C_ent.plan;
}

type error =
  | Set
  | Cdup_id of C_nat.t
  | Cdup_key of C_nat.t
  | Ccount of int * int
  | No_set of C_nat.t
  | Fhe of C_fhe.error
  | Hfhe of C_hfhe.error
  | Graph of C_graph.error
  | Ent of C_ent.error
  | Key of C_nat.t * C_nat.t
  | Room of C_nat.t * C_nat.t

val prod : set
val prod_cat : catalog
val valid : set -> bool
val seal : set -> (seal, error) result
val caps : set -> cap
val catalog : seal list -> (catalog, error) result
val select : catalog -> C_nat.t -> (set, error) result
val check :
  set -> C_fhe.catalog -> C_fhe.profile -> C_fhe.env ->
  C_hfhe.input list -> C_fhe.t -> (link, error) result
val check_cat :
  catalog -> C_fhe.catalog -> C_fhe.profile -> C_fhe.env ->
  C_hfhe.input list -> C_fhe.t -> (link, error) result
val text : error -> string