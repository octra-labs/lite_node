(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type adm = private {
  id : C_nat.t;
  terms : C_nat.t;
  out : C_nat.t;
  slots : C_nat.t;
  queries : C_nat.t;
  alpha : C_nat.t;
  row : C_nat.t;
}

type proof = Field

type pplan = private {
  rounds : Z.t;
  sound : Z.t;
  commits : Z.t;
  opens : Z.t;
  trace : Z.t;
  cbytes : Z.t;
  obytes : Z.t;
  tbytes : Z.t;
  total : Z.t;
}

type cert = private {
  proof : proof;
  bits : Z.t;
  traces : Z.t;
  tags : Z.t;
  bytes : Z.t;
  plan : pplan;
}

type shape = private {
  terms : C_nat.t;
  slots : C_nat.t;
  query : C_nat.t;
  tags : C_nat.t;
}

type input
type env

type info = private {
  shape : shape;
  peak : C_nat.t;
  peak_tags : C_nat.t;
  resets : C_nat.t;
}

type field =
  | Terms
  | Slots
  | Query
  | Tags
  | Resets

type error =
  | Nat of field * Z.t
  | Zero of field
  | Idup of string
  | Icount of int * int
  | Free of string
  | Limit of field * C_nat.t * C_nat.t
  | Slot_need of C_nat.t * C_nat.t
  | Re_limit of C_nat.t * C_nat.t
  | Cert_invalid
  | Cert_limit of field * Z.t * Z.t
  | Depth_max of int * int
  | Nodes of int * int

val prod : adm
val prod_cert : cert
val input : C_syn.name -> terms:Z.t -> slots:Z.t -> query:Z.t -> input
val env : adm -> input list -> (env, error) result
val adm_ok : adm -> bool
val cert_ok : cert -> bool
val check : adm -> cert -> env -> C_fhe.t -> (info, error) result
val shape_text : shape -> string
val text : error -> string