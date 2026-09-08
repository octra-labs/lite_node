(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type atom =
  | Read of C_nat.t
  | Write of C_nat.t
  | Emit of C_nat.t
  | Fail of C_nat.t
  | Close of C_nat.t

let key = function
  | Read id -> 0, id
  | Write id -> 1, id
  | Emit id -> 2, id
  | Fail id -> 3, id
  | Close id -> 4, id

let valid atom =
  match atom with
  | Read id | Write id | Emit id | Fail id | Close id -> C_nat.valid id

module Atom = struct
  type t = atom
  let compare left right =
    match key left, key right with
    | (left_tag, left_id), (right_tag, right_id) ->
      let order = Int.compare left_tag right_tag in
      if order = 0 then C_nat.compare left_id right_id else order
end

let compare = Atom.compare

module Row = Set.Make (Atom)

type t = Row.t

let empty = Row.empty
let one = Row.singleton
let add = Row.add
let union = Row.union
let of_list = List.fold_left (fun row atom -> Row.add atom row) Row.empty
let to_list = Row.elements
let equal = Row.equal
let subset = Row.subset

let atom_text = function
  | Read id -> "read<" ^ C_nat.text id ^ ">"
  | Write id -> "write<" ^ C_nat.text id ^ ">"
  | Emit id -> "emit<" ^ C_nat.text id ^ ">"
  | Fail id -> "fail<" ^ C_nat.text id ^ ">"
  | Close id -> "close<" ^ C_nat.text id ^ ">"

let text row =
  let out = C_text.make () in
  C_text.add out "{";
  let rec walk first values =
    match values () with
    | Seq.Nil -> C_text.add out "}"
    | Seq.Cons (atom, rest) when not (C_text.full out) ->
      if not first then C_text.add out ",";
      C_text.add out (atom_text atom);
      walk false rest
    | Seq.Cons _ -> ()
  in
  walk true (Row.to_seq row);
  C_text.get out