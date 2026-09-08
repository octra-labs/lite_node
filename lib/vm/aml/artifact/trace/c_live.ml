(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type slot = {
  id : C_term.id;
  mul : C_type.mul;
  live : bool;
}

type t = {
  rows : slot list array;
  bits : C_bin.bits;
}

type error =
  | Empty
  | Size of int
  | Row of int * int
  | Id of int * C_term.id
  | Mul of int * C_term.id
  | Dup of int * C_term.id
  | Bits

let max = (4 * C_rule.local.tm_nodes) + 2
let row_max = C_rule.local.tm_nodes
let slot id mul live = { id; mul; live }

let rec same_bits left right =
  match left, right with
  | [], [] -> true
  | lhead :: ltail, rhead :: rtail when Bool.equal lhead rhead ->
    same_bits ltail rtail
  | _ -> false

let equal left right =
  same_bits left.bits right.bits

let rec has id = function
  | [] -> false
  | item :: rest -> C_nat.equal id item.id || has id rest

let mode = function
  | { mul = C_type.Zero; live = false; _ } -> 0
  | { mul = C_type.Zero; live = true; _ } -> 1
  | { mul = C_type.One; live = false; _ } -> 2
  | { mul = C_type.One; live = true; _ } -> 3
  | { mul = C_type.Many; live = false; _ } -> 4
  | { mul = C_type.Many; live = true; _ } -> 5

let slot_code value =
  C_bin.Tag (Z.zero,
    C_bin.Cons (C_bin.Num (C_nat.to_z value.id),
      C_bin.Cons (C_bin.Num (Z.of_int (mode value)), C_bin.Nil)))

let row_code values =
  Option.map (fun body -> C_bin.Tag (Z.one, body))
    (C_bin.list_code (fun value -> Some (slot_code value)) values)

let map_code values =
  C_bin.list_code row_code (Array.to_list values)
  |> Option.map (fun body -> C_bin.Tag (Z.of_int 2, body))

let row at values =
  let rec walk seen = function
    | [] -> Ok ()
    | item :: rest ->
      if not (C_nat.valid item.id) then Error (Id (at, item.id))
      else if item.mul = C_type.Zero
          || (item.mul = C_type.Many && not item.live) then
        Error (Mul (at, item.id))
      else if has item.id seen then Error (Dup (at, item.id))
      else walk (item :: seen) rest
  in
  let size = List.length values in
  if size > row_max then Error (Row (at, size)) else walk [] values

let make values =
  let size = Array.length values in
  let rec walk at =
    if at = size then
      begin
        match map_code values with
        | None -> Error Bits
        | Some shape ->
          match C_bin.enc_code shape with
        | Some bits -> Ok { rows = Array.copy values; bits }
        | None -> Error Bits
      end
    else
      match row at values.(at) with
      | Ok () -> walk (at + 1)
      | Error error -> Error error
  in
  if size = 0 then Error Empty
  else if size > max then Error (Size size)
  else walk 0

let rows value = Array.copy value.rows
let length value = Array.length value.rows
let get value at = value.rows.(at)
let enc value = value.bits

let slot_get = function
  | C_bin.Tag (tag,
      C_bin.Cons (C_bin.Num id,
        C_bin.Cons (C_bin.Num kind, C_bin.Nil))) when Z.equal tag Z.zero ->
    begin
      match C_nat.make id with
      | None -> None
      | Some id ->
        if Z.equal kind Z.zero then Some (slot id C_type.Zero false)
        else if Z.equal kind Z.one then Some (slot id C_type.Zero true)
        else if Z.equal kind (Z.of_int 2) then Some (slot id C_type.One false)
        else if Z.equal kind (Z.of_int 3) then Some (slot id C_type.One true)
        else if Z.equal kind (Z.of_int 4) then Some (slot id C_type.Many false)
        else if Z.equal kind (Z.of_int 5) then Some (slot id C_type.Many true)
        else None
    end
  | _ -> None

let row_get = function
  | C_bin.Tag (tag, body) when Z.equal tag Z.one ->
    C_bin.list_get slot_get body
  | _ -> None

let map_get = function
  | C_bin.Tag (tag, body) when Z.equal tag (Z.of_int 2) ->
    Option.map Array.of_list (C_bin.list_get row_get body)
  | _ -> None

let dec input =
  match C_bin.dec_code input with
  | None -> None
  | Some shape ->
    begin
      match map_get shape with
      | None -> None
      | Some raw ->
        begin
          match make raw with
          | Ok value when same_bits value.bits input -> Some value
          | Ok _ | Error _ -> None
        end
    end

let text = function
  | Empty -> "liveness map is empty"
  | Size size ->
    Printf.sprintf "liveness map size = %d maximum = %d" size max
  | Row (at, size) ->
    Printf.sprintf "liveness row size = %d maximum = %d pc = %d"
      size row_max at
  | Id (at, id) ->
    "liveness map id is invalid pc = " ^ string_of_int at
    ^ " id = " ^ C_nat.text id
  | Mul (at, id) ->
    "liveness map multiplicity is invalid pc = " ^ string_of_int at
    ^ " id = " ^ C_nat.text id
  | Dup (at, id) ->
    "liveness map id is repeated pc = " ^ string_of_int at
    ^ " id = " ^ C_nat.text id
  | Bits -> "liveness map bit image exceeds the local limit"