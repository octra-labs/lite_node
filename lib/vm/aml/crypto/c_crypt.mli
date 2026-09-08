(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type cipher = private {
  key : C_nat.t;
  rem : C_nat.t;
  value : C_fp.t;
}

type input
type env

type run = private {
  info : C_fhe.info;
  param : C_fhe.param;
  cipher : cipher;
}

type error =
  | Fhe of C_fhe.error
  | Free of string
  | Need of cipher * cipher
  | Mul_zero of C_nat.t
  | Re_need of C_nat.t * C_nat.t
  | Type of cipher * C_fhe.enc

val input :
  C_syn.name -> key:Z.t -> rem:Z.t -> value:Z.t -> input

val make : key:Z.t -> rem:Z.t -> value:Z.t -> (cipher, error) result
val equal : cipher -> cipher -> bool
val view : cipher -> C_nat.t * C_nat.t * C_fp.t
val core : cipher -> C_type.t * C_eval.value

val env : C_fhe.profile -> input list -> (env, error) result
val plain : env -> C_fhe.t -> C_fp.t option
val eval : C_fhe.profile -> env -> C_fhe.t -> (cipher, error) result
val exec :
  C_fhe.profile -> C_fhe.catalog -> env -> C_fhe.t -> (run, error) result
val text : error -> string