(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mul = Zero | One | Many
type kind = Data | Res
type sign = Signed | Unsigned

type t =
  | Unit
  | Bool
  | Int
  | Num of sign * C_nat.t
  | Bytes of C_nat.t
  | Vec of C_nat.t * t
  | Seq of C_nat.t * t
  | Cap of C_nat.t
  | Enc of C_nat.t * C_nat.t
  | Pair of t * t
  | Sum of t * t

let max_depth = C_rule.local.ty_depth
let max_nodes = C_rule.local.ty_nodes
let max_bits = 256

let equal left right =
  let rec walk = function
    | [] -> true
    | (left, right) :: rest ->
      match left, right with
      | Unit, Unit | Bool, Bool | Int, Int -> walk rest
      | Num (left_sign, left_bits), Num (right_sign, right_bits) ->
        left_sign = right_sign
        && C_nat.equal left_bits right_bits
        && walk rest
      | Bytes left, Bytes right | Cap left, Cap right ->
        C_nat.equal left right && walk rest
      | Enc (left_key, left_rem), Enc (right_key, right_rem) ->
        C_nat.equal left_key right_key
        && C_nat.equal left_rem right_rem
        && walk rest
      | Vec (left_len, left), Vec (right_len, right) ->
        C_nat.equal left_len right_len && walk ((left, right) :: rest)
      | Seq (left_cap, left), Seq (right_cap, right) ->
        C_nat.equal left_cap right_cap && walk ((left, right) :: rest)
      | Pair (la, lb), Pair (ra, rb) | Sum (la, lb), Sum (ra, rb) ->
        walk ((la, ra) :: (lb, rb) :: rest)
      | _ -> false
  in
  walk [left, right]

let rec eq_work = function
  | Unit | Bool | Int | Num _ | Cap _ | Enc _ -> Z.one
  | Bytes len -> Z.succ (C_nat.to_z len)
  | Vec (len, elem) ->
    Z.succ (Z.mul (C_nat.to_z len) (eq_work elem))
  | Seq (cap, elem) ->
    Z.succ (Z.mul (C_nat.to_z cap) (eq_work elem))
  | Pair (left, right) ->
    Z.succ (Z.add (eq_work left) (eq_work right))
  | Sum (left, right) ->
    Z.succ (Z.max (eq_work left) (eq_work right))

let mul_text = function
  | Zero -> "0"
  | One -> "1"
  | Many -> "many"

type part =
  | Typ of t
  | Text of string

let text typ =
  let out = C_text.make () in
  let rec walk = function
    | [] -> ()
    | _ when C_text.full out -> ()
    | Text value :: rest ->
      C_text.add out value;
      walk rest
    | Typ Unit :: rest -> C_text.add out "unit"; walk rest
    | Typ Bool :: rest -> C_text.add out "bool"; walk rest
    | Typ Int :: rest -> C_text.add out "int"; walk rest
    | Typ (Num (Signed, bits)) :: rest ->
      C_text.add out ("sint[" ^ C_nat.text bits ^ "]");
      walk rest
    | Typ (Num (Unsigned, bits)) :: rest ->
      C_text.add out ("uint[" ^ C_nat.text bits ^ "]");
      walk rest
    | Typ (Bytes len) :: rest ->
      C_text.add out ("bytes[" ^ C_nat.text len ^ "]");
      walk rest
    | Typ (Vec (len, elem)) :: rest ->
      walk (Text "vec[" :: Text (C_nat.text len) :: Text ", "
        :: Typ elem :: Text "]" :: rest)
    | Typ (Seq (cap, elem)) :: rest ->
      walk (Text "seq[" :: Text (C_nat.text cap) :: Text ", "
        :: Typ elem :: Text "]" :: rest)
    | Typ (Cap id) :: rest ->
      C_text.add out ("cap[" ^ C_nat.text id ^ "]");
      walk rest
    | Typ (Enc (key, rem)) :: rest ->
      C_text.add out ("enc[" ^ C_nat.text key ^ "," ^ C_nat.text rem ^ "]");
      walk rest
    | Typ (Pair (left, right)) :: rest ->
      walk (Text "(" :: Typ left :: Text " * " :: Typ right :: Text ")" :: rest)
    | Typ (Sum (left, right)) :: rest ->
      walk (Text "(" :: Typ left :: Text " + " :: Typ right :: Text ")" :: rest)
  in
  walk [Typ typ];
  C_text.get out

let nodes typ =
  let rec walk nodes = function
    | [] -> Some nodes
    | (depth, typ) :: rest ->
      if depth > max_depth || nodes >= max_nodes then None
      else
        let next = depth + 1 in
        match typ with
        | Unit | Bool | Int -> walk (nodes + 1) rest
        | Num (_, bits) ->
          if C_nat.to_int bits > 0 && C_nat.to_int bits <= max_bits then
            walk (nodes + 1) rest
          else None
        | Bytes len | Cap len ->
          if C_nat.valid len then walk (nodes + 1) rest else None
        | Enc (key, rem) ->
          if C_nat.valid key && C_nat.valid rem then
            walk (nodes + 1) rest
          else None
        | Vec (len, elem) ->
          if C_nat.valid len then
            walk (nodes + 1) ((next, elem) :: rest)
          else None
        | Seq (cap, elem) ->
          if C_nat.valid cap then
            walk (nodes + 1) ((next, elem) :: rest)
          else None
        | Pair (left, right) | Sum (left, right) ->
          walk (nodes + 1) ((next, left) :: (next, right) :: rest)
  in
  walk 0 [0, typ]

let plain typ =
  let rec walk = function
    | [] -> true
    | Unit :: rest | Bool :: rest | Int :: rest | Num _ :: rest
    | Bytes _ :: rest -> walk rest
    | Vec (_, elem) :: rest | Seq (_, elem) :: rest -> walk (elem :: rest)
    | Pair (left, right) :: rest | Sum (left, right) :: rest ->
      walk (left :: right :: rest)
    | Cap _ :: _ | Enc _ :: _ -> false
  in
  walk [typ]

let valid typ =
  let rec seq = function
    | [] -> true
    | Seq (_, elem) :: _ when not (plain elem) -> false
    | Vec (_, elem) :: rest | Seq (_, elem) :: rest -> seq (elem :: rest)
    | Pair (left, right) :: rest | Sum (left, right) :: rest ->
      seq (left :: right :: rest)
    | _ :: rest -> seq rest
  in
  Option.is_some (nodes typ) && seq [typ]

let repr = function
  | Unit -> Unit
  | Bool -> Bool
  | Int | Num _ -> Int
  | Bytes len -> Bytes len
  | Vec (len, elem) -> Vec (len, elem)
  | Seq (cap, elem) -> Pair (Int, Vec (cap, elem))
  | Cap kind -> Cap kind
  | Enc (key, rem) -> Enc (key, rem)
  | Pair (left, right) -> Pair (left, right)
  | Sum (left, right) -> Sum (left, right)

let add_len left right =
  C_nat.add left right

let kind typ =
  let rec walk = function
    | [] -> Data
    | Unit :: rest | Bool :: rest | Int :: rest | Num _ :: rest
    | Bytes _ :: rest
    | Enc _ :: rest -> walk rest
    | Vec (len, _) :: rest when C_nat.equal len C_nat.zero -> walk rest
    | Cap _ :: _ -> Res
    | Vec (_, elem) :: rest | Seq (_, elem) :: rest -> walk (elem :: rest)
    | Pair (left, right) :: rest | Sum (left, right) :: rest ->
      walk (left :: right :: rest)
  in
  walk [typ]

let equatable typ =
  let rec walk = function
    | [] -> true
    | Unit :: rest | Bool :: rest | Int :: rest | Num _ :: rest
    | Bytes _ :: rest -> walk rest
    | Vec (len, _) :: rest when C_nat.equal len C_nat.zero -> walk rest
    | Cap _ :: _ | Enc _ :: _ -> false
    | Vec (_, elem) :: rest | Seq (_, elem) :: rest -> walk (elem :: rest)
    | Pair (left, right) :: rest | Sum (left, right) :: rest ->
      walk (left :: right :: rest)
  in
  walk [typ]

let range sign bits =
  let width = C_nat.to_int bits in
  if width < 1 || width > max_bits then None
  else
    match sign with
    | Unsigned -> Some (Z.zero, Z.pred (Z.shift_left Z.one width))
    | Signed ->
      let half = Z.shift_left Z.one (width - 1) in
      Some (Z.neg half, Z.pred half)

let admits typ value =
  match typ with
  | Int -> true
  | Num (sign, bits) ->
    begin
      match range sign bits with
      | Some (low, high) -> Z.leq low value && Z.leq value high
      | None -> false
    end
  | Unit | Bool | Bytes _ | Vec _ | Seq _ | Cap _ | Enc _ | Pair _ | Sum _ ->
    false