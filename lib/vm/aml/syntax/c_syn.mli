(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type name

val name : string -> name option
val slot : C_nat.t -> name
val dslot : C_nat.t -> name
val name_text : name -> string
val name_equal : name -> name -> bool

type typ =
  | TUnit
  | TBool
  | TInt
  | TNum of C_type.sign * Z.t
  | TBytes of Z.t
  | TVec of Z.t * typ
  | TSeq of Z.t * typ
  | TCap of Z.t
  | TPair of typ * typ
  | TSum of typ * typ

type rel = Lt | Le | Gt | Ge

type atom =
  | ARead of Z.t
  | AWrite of Z.t
  | AEmit of Z.t
  | AFail of Z.t
  | AClose of Z.t

type bind = {
  name : name;
  mul : C_type.mul;
  typ : typ;
}

type t =
  | KUnit
  | KBool of bool
  | KInt of Z.t
  | KBytes of string
  | KVec of typ * t list
  | KSeq of C_nat.t * typ * t list
  | Var of name
  | Let of bind * t * t
  | If of t * t * t
  | Pair of t * t
  | Unpair of t * bind * bind * t
  | Fst of t
  | Snd of t
  | Inl of t * typ
  | Inr of typ * t
  | Case of t * bind * t * bind * t
  | Act of atom * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Neg of t
  | Abs of t
  | Fit of typ * t
  | Wide of t
  | Length of t
  | Eq of typ * t * t
  | Cmp of rel * t * t
  | Cat of t * t
  | Take of Z.t * t
  | Drop of Z.t * t
  | Vcat of t * t
  | At of Z.t * t
  | Uncons of t
  | Vfold of t * t * fold
  | Step of t * t
  | Close of t

and fold = {
  item : bind;
  state : bind;
  body : t;
}

val bind : name -> C_type.mul -> typ -> bind
val fold : bind -> bind -> t -> fold
val cmp : rel -> t -> t -> t
val has : name -> t -> bool