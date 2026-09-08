(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type name = string

let name_max = C_rule.local.name

let alpha code =
  code >= Char.code 'a' && code <= Char.code 'z'
  || code >= Char.code 'A' && code <= Char.code 'Z'

let digit code = code >= Char.code '0' && code <= Char.code '9'

let name text =
  let len = String.length text in
  let first code = alpha code || code = Char.code '_' in
  let rest code = first code || digit code in
  let rec walk index =
    index = len || rest (Char.code (String.get text index)) && walk (index + 1)
  in
  if len = 0 || len > name_max || not (first (Char.code (String.get text 0))) then None
  else if walk 1 then Some text
  else None

let slot id = "$" ^ C_nat.text id
let dslot id = "$d" ^ C_nat.text id
let name_text name = name
let name_equal = String.equal

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

let bind name mul typ = { name; mul; typ }
let fold item state body = { item; state; body }

let cmp rel left right = Cmp (rel, left, right)

let push values rest = List.fold_left (fun out value -> value :: out) rest values

let has name term =
  let rec walk = function
    | [] -> false
    | Var value :: _ when name_equal name value -> true
    | Let (item, _, _) :: _ when name_equal name item.name -> true
    | Unpair (_, left, right, _) :: _
        when name_equal name left.name || name_equal name right.name -> true
    | Case (_, left, _, right, _) :: _
        when name_equal name left.name || name_equal name right.name -> true
    | Vfold (_, _, item) :: _
        when name_equal name item.item.name || name_equal name item.state.name -> true
    | term :: rest ->
      let rest =
        match term with
        | KUnit | KBool _ | KInt _ | KBytes _ | Var _ -> rest
        | KVec (_, values) | KSeq (_, _, values) -> push values rest
        | Let (_, value, body)
        | Pair (value, body)
        | Add (value, body)
        | Sub (value, body)
        | Mul (value, body)
        | Div (value, body)
        | Mod (value, body)
        | Eq (_, value, body)
        | Cmp (_, value, body)
        | Cat (value, body)
        | Vcat (value, body)
        | Step (value, body) -> value :: body :: rest
        | If (guard, yes, no) -> guard :: yes :: no :: rest
        | Unpair (pair, _, _, body) -> pair :: body :: rest
        | Fst value | Snd value | Inl (value, _) | Inr (_, value)
        | Act (_, value) | Neg value | Abs value | Fit (_, value)
        | Wide value | Length value | Take (_, value)
        | Drop (_, value) | At (_, value)
        | Uncons value | Close value -> value :: rest
        | Case (value, _, yes, _, no) -> value :: yes :: no :: rest
        | Vfold (vector, seed, item) -> vector :: seed :: item.body :: rest
      in
      walk rest
  in
  walk [term]