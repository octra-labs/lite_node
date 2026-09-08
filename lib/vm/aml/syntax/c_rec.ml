(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type field = {
  name : C_syn.name;
  typ : C_syn.typ;
}

type item = {
  name : C_syn.name;
  value : C_syn.t;
}

type pick = {
  name : C_syn.name;
  bind : C_syn.bind;
}

type decl = {
  name : C_syn.name;
  fields : field list;
  prod : C_syn.typ;
  data : C_data.decl;
}

type error =
  | Data of C_data.error
  | Low of C_low.error
  | Check of C_check.error
  | Empty
  | Count of int * int
  | Dup of string
  | Bind_dup of string
  | Items of int * int
  | Order of string * string
  | Type of string
  | Mode of string * C_type.mul * C_type.mul
  | Id

let field name typ = { name; typ }
let item name value = { name; value }
let pick name bind = { name; bind }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let rec prod = function
  | [] -> None
  | [item] -> Some item.typ
  | item :: rest ->
      begin
        match prod rest with
        | Some right -> Some (C_syn.TPair (item.typ, right))
        | None -> None
      end

let unique values name error =
  let rec walk seen = function
    | [] -> Ok ()
    | item :: rest ->
        let value = name item in
        if List.exists (C_syn.name_equal value) seen then
          Error (error (C_syn.name_text value))
        else walk (value :: seen) rest
  in
  walk [] values

let decl bits name (fields : field list) =
  let count = List.length fields in
  if count = 0 then Error Empty
  else if count > C_check.max_inputs then
    Error (Count (C_check.max_inputs, count))
  else
    let* () = unique fields (fun item -> item.name) (fun name -> Dup name) in
    match prod fields with
    | None -> Error Empty
    | Some prod ->
        begin
          match C_data.decl bits name [C_data.ctor name prod] with
          | Ok data -> Ok { name; fields; prod; data }
          | Error error -> Error (Data error)
        end

let name (decl : decl) = decl.name
let bits (decl : decl) = C_data.bits decl.data
let fields (decl : decl) =
  List.map (fun (item : field) -> item.name, item.typ) decl.fields
let typ decl = C_data.typ decl.data

let order (names : field list) values get =
  let expected = List.length names in
  let actual = List.length values in
  if expected <> actual then Error (Items (expected, actual))
  else
    let rec walk (names : field list) values =
      match names, values with
      | [], [] -> Ok ()
      | field :: rest, value :: tail ->
          let got = get value in
          if C_syn.name_equal field.name got then walk rest tail
          else Error (Order (C_syn.name_text field.name, C_syn.name_text got))
      | _ -> Error (Items (expected, actual))
    in
    walk names values

let rec pack = function
  | [] -> None
  | [item] -> Some item.value
  | item :: rest ->
      begin
        match pack rest with
        | Some right -> Some (C_syn.Pair (item.value, right))
        | None -> None
      end

let make decl (items : item list) =
  let* () = order decl.fields items (fun item -> item.name) in
  match pack items with
  | None -> Error Empty
  | Some value ->
      begin
        match C_data.make decl.data decl.name value with
        | Ok value -> Ok value
        | Error error -> Error (Data error)
      end

let needed typ =
  match C_type.kind typ with
  | C_type.Data -> C_type.Many
  | C_type.Res -> C_type.One

let picks (fields : field list) (values : pick list) =
  let* () = order fields values (fun item -> item.name) in
  let* () = unique values (fun item -> item.bind.name)
    (fun name -> Bind_dup name) in
  let rec walk fields values =
    match fields, values with
    | [], [] -> Ok ()
    | field :: rest, item :: tail ->
        let* expected = Result.map_error (fun error -> Low error)
          (C_low.typ field.typ) in
        let* actual = Result.map_error (fun error -> Low error)
          (C_low.typ item.bind.typ) in
        if not (C_type.equal expected actual) then
          Error (Type (C_syn.name_text field.name))
        else
          let mode = needed expected in
          if item.bind.mul <> mode then
            Error (Mode (C_syn.name_text field.name, mode, item.bind.mul))
          else walk rest tail
    | _ -> Error (Items (List.length fields, List.length values))
  in
  walk fields values

let next seed =
  match C_nat.add seed C_nat.one with
  | Some value -> Ok value
  | None -> Error Id

let rec open_fields seed (fields : field list) (picks : pick list) value body =
  match fields, picks with
  | [_], [item] -> Ok (C_syn.Let (item.bind, value, body))
  | _ :: rest, item :: tail ->
      begin
        match prod rest with
        | None -> Error Empty
        | Some right ->
            let name = C_syn.dslot seed in
            let* right_core = Result.map_error (fun error -> Low error)
              (C_low.typ right) in
            let bind = C_syn.bind name (needed right_core) right in
            let* seed = next seed in
            let* body = open_fields seed rest tail (C_syn.Var name) body in
            Ok (C_syn.Unpair (value, item.bind, bind, body))
      end
  | _ -> Error Empty

let split decl value values body =
  let* () = picks decl.fields values in
  let* prod_core = Result.map_error (fun error -> Low error)
    (C_low.typ decl.prod) in
  let* two = next C_nat.one in
  let* three = next two in
  let payload = C_syn.bind (C_syn.dslot two) (needed prod_core) decl.prod in
  let* body = open_fields three decl.fields values
    (C_syn.Var payload.name) body in
  match C_data.case decl.data value [C_data.arm decl.name payload body] with
  | Ok value -> Ok value
  | Error error -> Error (Data error)

let lower inputs decl value values body =
  let* term = split decl value values body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs decl value values body =
  let* prog = lower inputs decl value values body in
  Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term)

let text = function
  | Data error -> C_data.text error
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Empty -> "shape fields empty"
  | Count (max, actual) ->
      "shape field count max = " ^ string_of_int max ^ " actual = "
      ^ string_of_int actual
  | Dup name -> "duplicate shape field = " ^ name
  | Bind_dup name -> "duplicate shape binder = " ^ name
  | Items (expected, actual) ->
      "shape fields expected = " ^ string_of_int expected ^ " actual = "
      ^ string_of_int actual
  | Order (expected, actual) ->
      "shape field expected = " ^ expected ^ " actual = " ^ actual
  | Type name -> "shape field type changed = " ^ name
  | Mode (name, expected, actual) ->
      "shape field mode name = " ^ name ^ " expected = "
      ^ C_type.mul_text expected ^ " actual = " ^ C_type.mul_text actual
  | Id -> "shape binder id outside profile"