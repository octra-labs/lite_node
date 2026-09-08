(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type field =
  | Nat
  | Str
  | Text
  | Ty_depth
  | Ty_nodes
  | Tm_depth
  | Tm_nodes
  | Inputs
  | Fuel
  | Name
  | Tag

type limits = {
  nat : int;
  str : int;
  text : int;
  ty_depth : int;
  ty_nodes : int;
  tm_depth : int;
  tm_nodes : int;
  inputs : int;
  fuel : int;
  name : int;
  tag : int;
}

type id = {
  version : int;
  limits : limits;
}

type t = {
  id : id;
  activate : Z.t;
}

type error =
  | Limit of field * int
  | Shape
  | Version of int
  | Epoch of Z.t
  | Absent of Z.t
  | Unsupported of id
  | Dup_id of id
  | Dup_epoch of Z.t
  | Count of int * int

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let fields value = [
  Nat, value.nat;
  Str, value.str;
  Text, value.text;
  Ty_depth, value.ty_depth;
  Ty_nodes, value.ty_nodes;
  Tm_depth, value.tm_depth;
  Tm_nodes, value.tm_nodes;
  Inputs, value.inputs;
  Fuel, value.fuel;
  Name, value.name;
  Tag, value.tag;
]

let valid value =
  let rec positive = function
    | [] -> Ok ()
    | (field, actual) :: _ when actual <= 0 -> Error (Limit (field, actual))
    | _ :: rest -> positive rest
  in
  let* () = positive (fields value) in
  if value.nat > value.str
    || value.text > value.str
    || value.ty_depth > value.tm_depth
    || value.ty_nodes > value.tm_nodes
    || value.inputs > value.tm_nodes
    || value.fuel > value.nat
    || value.name > value.text
    || value.tag > value.nat
  then Error Shape
  else Ok value

let limits ~nat ~str ~text ~ty_depth ~ty_nodes ~tm_depth ~tm_nodes
    ~inputs ~fuel ~name ~tag =
  valid {
    nat;
    str;
    text;
    ty_depth;
    ty_nodes;
    tm_depth;
    tm_nodes;
    inputs;
    fuel;
    name;
    tag;
  }

let local =
  match limits
      ~nat:1_000_000 ~str:16_777_211 ~text:1024
      ~ty_depth:256 ~ty_nodes:4096 ~tm_depth:4096 ~tm_nodes:100_000
      ~inputs:4096 ~fuel:1_000_000 ~name:64 ~tag:12 with
  | Ok value -> value
  | Error _ -> invalid_arg "local rule limits"

let same_limits left right =
  left.nat = right.nat
  && left.str = right.str
  && left.text = right.text
  && left.ty_depth = right.ty_depth
  && left.ty_nodes = right.ty_nodes
  && left.tm_depth = right.tm_depth
  && left.tm_nodes = right.tm_nodes
  && left.inputs = right.inputs
  && left.fuel = right.fuel
  && left.name = right.name
  && left.tag = right.tag

let id_values value = value.version :: List.map snd (fields value.limits)

let rec ints_equal left right =
  match left, right with
  | [], [] -> true
  | l :: ls, r :: rs -> l = r && ints_equal ls rs
  | _ -> false

let same_id left right = ints_equal (id_values left) (id_values right)

let rule ~version ~activate limits =
  let* limits = valid limits in
  if version <= 0 then Error (Version version)
  else if Z.sign activate < 0 then Error (Epoch activate)
  else Ok { id = { version; limits }; activate }

module Id = struct
  type nonrec t = id

  let rec compare_values left right =
    match left, right with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | l :: ls, r :: rs ->
        let order = Int.compare l r in
        if order = 0 then compare_values ls rs else order

  let compare left right = compare_values (id_values left) (id_values right)
end

module Epoch = struct
  type t = Z.t
  let compare = Z.compare
end

module Id_map = Map.Make (Id)
module Epoch_map = Map.Make (Epoch)

type schedule = {
  ids : unit Id_map.t;
  epochs : t Epoch_map.t;
}

let schedule values =
  let rec walk count ids epochs = function
    | [] -> Ok { ids; epochs }
    | _ when count = local.inputs ->
        Error (Count (local.inputs, local.inputs + 1))
    | value :: rest ->
        if Id_map.mem value.id ids then Error (Dup_id value.id)
        else if Epoch_map.mem value.activate epochs then
          Error (Dup_epoch value.activate)
        else
          walk (count + 1)
            (Id_map.add value.id () ids)
            (Epoch_map.add value.activate value epochs)
            rest
  in
  walk 0 Id_map.empty Epoch_map.empty values

let at schedule ~epoch =
  if Z.sign epoch < 0 then Error (Epoch epoch)
  else
    let earlier, exact, _ = Epoch_map.split epoch schedule.epochs in
    match exact with
    | Some value -> Ok (Some value)
    | None ->
        begin
          match Epoch_map.max_binding_opt earlier with
          | None -> Ok None
          | Some (_, value) -> Ok (Some value)
        end

let id value = value.id
let version value = value.version
let id_limits value = value.limits
let activate value = value.activate
let rule_limits value = value.id.limits
let supported value = same_limits value.id.limits local

let select schedule ~epoch =
  let* value = at schedule ~epoch in
  match value with
  | None -> Error (Absent epoch)
  | Some value when supported value -> Ok value
  | Some value -> Error (Unsupported value.id)

let id_text value =
  String.concat ":" ("aml" :: List.map string_of_int (id_values value))

let field_text = function
  | Nat -> "natural"
  | Str -> "string"
  | Text -> "diagnostic"
  | Ty_depth -> "type depth"
  | Ty_nodes -> "type nodes"
  | Tm_depth -> "term depth"
  | Tm_nodes -> "term nodes"
  | Inputs -> "inputs"
  | Fuel -> "fuel"
  | Name -> "name"
  | Tag -> "data tag"

let text = function
  | Limit (field, actual) ->
      Printf.sprintf "rule limit is not positive field = %s actual = %d"
        (field_text field) actual
  | Shape -> "rule limit relations are invalid"
  | Version actual -> Printf.sprintf "rule version is invalid actual = %d" actual
  | Epoch actual -> "rule epoch is negative actual = " ^ Z.to_string actual
  | Absent actual -> "rule is absent epoch = " ^ Z.to_string actual
  | Unsupported id -> "rule is unsupported id = " ^ id_text id
  | Dup_id id -> "rule identifier repeats id = " ^ id_text id
  | Dup_epoch epoch -> "rule activation repeats epoch = " ^ Z.to_string epoch
  | Count (limit, actual) ->
      Printf.sprintf "rule count limit = %d actual = %d" limit actual