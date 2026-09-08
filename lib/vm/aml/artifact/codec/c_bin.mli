(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = bool list

type code =
  | Num of Z.t
  | Int of Z.t
  | Nil
  | Cons of code * code
  | Tag of Z.t * code

type reject =
  | DivideZero
  | ModuloZero

type snap =
  | Refuse
  | Reject of reject
  | Accept of C_type.t * C_eff.atom list * C_limit.t * C_eval.value
      * C_eff.atom list * C_limit.t

val num : C_nat.t -> code option
val get_num : code -> C_nat.t option
val ty_code : C_type.t -> code option
val ty_get : code -> C_type.t option
val atom_code : C_eff.atom -> code option
val atom_get : code -> C_eff.atom option
val list_code : ('a -> code option) -> 'a list -> code option
val list_get : (code -> 'a option) -> code -> 'a list option
val value_code : C_eval.value -> code option
val value_get : code -> C_eval.value option
val res_code : C_limit.t -> code option
val res_get : code -> C_limit.t option

val enc_ty : C_type.t -> bits option
val dec_ty : bits -> C_type.t option
val enc_value : C_eval.value -> bits option
val dec_value : bits -> C_eval.value option
val enc_row : C_eff.atom list -> bits option
val dec_row : bits -> C_eff.atom list option
val enc_res : C_limit.t -> bits option
val dec_res : bits -> C_limit.t option
val enc_snap : snap -> bits option
val dec_snap : bits -> snap option
val enc_code : code -> bits option
val dec_code : bits -> code option
val pack : bits -> string option
val unpack : int -> string -> bits option
val transcript : bits list -> string option