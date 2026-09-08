(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type id = C_nat.t

type bind = {
  id : id;
  mul : C_type.mul;
  typ : C_type.t;
}

type rel = Lt | Le | Gt | Ge

type t =
  | Unit
  | Bool of bool
  | Int of Z.t
  | Narrow of C_type.t * Z.t
  | Bytes of string
  | Vec of C_type.t * t list
  | Seq of C_nat.t * C_type.t * t list
  | Var of id
  | Let of bind * t * t
  | If of t * t * t
  | Pair of t * t
  | Unpair of t * bind * bind * t
  | Fst of t
  | Snd of t
  | Inl of t * C_type.t
  | Inr of C_type.t * t
  | Case of t * bind * t * bind * t
  | Act of C_eff.atom * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Neg of t
  | Abs of t
  | Fit of C_type.t * t
  | Wide of t
  | Length of t
  | Eq of C_type.t * t * t
  | Cmp of rel * t * t
  | Cat of t * t
  | Take of C_nat.t * t
  | Drop of C_nat.t * t
  | Vcat of t * t
  | At of C_nat.t * t
  | Uncons of t
  | Vfold of t * t * fold
  | Step of t * t
  | Close of t

and fold = {
  item : bind;
  state : bind;
  body : t;
}

val bind : id -> C_type.mul -> C_type.t -> bind
val fold : bind -> bind -> t -> fold