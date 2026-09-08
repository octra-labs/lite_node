(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ctor = {
  name : C_syn.name;
  arg : C_syn.typ;
}

type decl = {
  bits : bool list;
  tag : C_syn.typ;
  mark : C_syn.t;
  name : C_syn.name;
  ctors : ctor list;
  sum : C_syn.typ;
  typ : C_syn.typ;
}

type arm = {
  name : C_syn.name;
  bind : C_syn.bind;
  body : C_syn.t;
}

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Empty
  | Tag of int * int
  | Count of int * int
  | Dup of string
  | Ctor of string
  | Arms of int * int
  | Order of string * string
  | Type of string
  | Mode of string * C_type.mul * C_type.mul
  | Id

let ctor name arg = { name; arg }
let arm name bind body = { name; bind; body }
let tag_len = C_rule.local.tag

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let rec sum (values : ctor list) =
  match values with
  | [] -> None
  | [item] -> Some item.arg
  | item :: rest ->
      begin
        match sum rest with
        | Some right -> Some (C_syn.TSum (item.arg, right))
        | None -> None
      end

let unique (values : ctor list) =
  let rec walk seen (values : ctor list) =
    match values with
    | [] -> Ok ()
    | item :: rest ->
        if List.exists (C_syn.name_equal item.name) seen then
          Error (Dup (C_syn.name_text item.name))
        else walk (item.name :: seen) rest
  in
  walk [] values

let types (values : ctor list) =
  let rec walk (values : ctor list) =
    match values with
    | [] -> Ok ()
    | item :: rest ->
        let* _ = Result.map_error (fun error -> Low error) (C_low.typ item.arg) in
        walk rest
  in
  walk values

let rec tag = function
  | [] -> C_syn.TBytes Z.zero, C_syn.KBytes ""
  | bit :: rest ->
      let typ, value = tag rest in
      if bit then
        C_syn.TPair (C_syn.TBool, typ), C_syn.Pair (C_syn.KBool true, value)
      else
        C_syn.TPair (C_syn.TUnit, typ), C_syn.Pair (C_syn.KUnit, value)

let decl bits name (ctors : ctor list) =
  let count = List.length ctors in
  if count = 0 then Error Empty
  else if List.length bits <> tag_len then
    Error (Tag (tag_len, List.length bits))
  else if count > C_check.max_inputs then
    Error (Count (C_check.max_inputs, count))
  else
    let* () = unique ctors in
    let* () = types ctors in
    match sum ctors with
    | None -> Error Empty
    | Some sum ->
        let tag, mark = tag bits in
        let typ = C_syn.TPair (tag, sum) in
        let* _ = Result.map_error (fun error -> Low error) (C_low.typ typ) in
        Ok { bits; tag; mark; name; ctors; sum; typ }

let name (decl : decl) = decl.name
let bits (decl : decl) = decl.bits
let ctors (decl : decl) =
  List.map (fun (item : ctor) -> item.name, item.arg) decl.ctors
let typ decl = decl.typ

let rec inject name value (values : ctor list) =
  match values with
  | [] -> Error (Ctor (C_syn.name_text name))
  | [item] ->
      if C_syn.name_equal name item.name then Ok value
      else Error (Ctor (C_syn.name_text name))
  | item :: rest ->
      begin
        match sum rest with
        | None -> Error Empty
        | Some right when C_syn.name_equal name item.name ->
            Ok (C_syn.Inl (value, right))
        | Some _ ->
            let* out = inject name value rest in
            Ok (C_syn.Inr (item.arg, out))
      end

let make decl name value =
  let* value = inject name value decl.ctors in
  Ok (C_syn.Pair (decl.mark, value))

let needed typ =
  match C_type.kind typ with
  | C_type.Data -> C_type.Many
  | C_type.Res -> C_type.One

let arms (ctors : ctor list) (values : arm list) =
  let expected = List.length ctors in
  let actual = List.length values in
  if expected <> actual then Error (Arms (expected, actual))
  else
    let rec walk (pairs : ctor list * arm list) =
      match pairs with
      | [], [] -> Ok ()
      | item :: rest, arm :: arm_rest ->
          if not (C_syn.name_equal item.name arm.name) then
            Error (Order (C_syn.name_text item.name, C_syn.name_text arm.name))
          else
            let* arg = Result.map_error (fun error -> Low error)
              (C_low.typ item.arg) in
            let* bind = Result.map_error (fun error -> Low error)
              (C_low.typ arm.bind.typ) in
            if not (C_type.equal arg bind) then
              Error (Type (C_syn.name_text item.name))
            else
              let mode = needed arg in
              if arm.bind.mul <> mode then
                Error (Mode (C_syn.name_text item.name, mode, arm.bind.mul))
              else walk (rest, arm_rest)
      | _ -> Error (Arms (expected, actual))
    in
    walk (ctors, values)

let rec branch seed (ctors : ctor list) (arms : arm list) value =
  match ctors, arms with
  | [_], [arm] -> Ok (C_syn.Let (arm.bind, value, arm.body))
  | _ :: rest, arm :: arm_rest ->
      begin
        match sum rest with
        | None -> Error Empty
        | Some right ->
            begin
              match C_nat.add seed C_nat.one with
              | None -> Error Id
              | Some next ->
                  let bind = C_syn.bind (C_syn.dslot seed) C_type.One right in
                  let* no = branch next rest arm_rest (C_syn.Var bind.name) in
                  Ok (C_syn.Case (value, arm.bind, arm.body, bind, no))
            end
      end
  | _ -> Error Empty

let case decl value values =
  let* () = arms decl.ctors values in
  match C_nat.add C_nat.one C_nat.one with
  | None -> Error Id
  | Some seed ->
      let tag = C_syn.bind (C_syn.dslot C_nat.zero) C_type.Many decl.tag in
      let sum = C_syn.bind (C_syn.dslot C_nat.one) C_type.One decl.sum in
      let* body = branch seed decl.ctors values (C_syn.Var sum.name) in
      Ok (C_syn.Unpair (value, tag, sum, body))

let lower inputs decl value values =
  let* body = case decl value values in
  Result.map_error (fun error -> Low error) (C_low.prog inputs body)

let check inputs decl value values =
  let* prog = lower inputs decl value values in
  Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term)

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Empty -> "data constructors empty"
  | Tag (expected, actual) ->
      "data tag bits expected = " ^ string_of_int expected ^ " actual = "
      ^ string_of_int actual
  | Count (max, actual) ->
      "constructor count max = " ^ string_of_int max ^ " actual = "
      ^ string_of_int actual
  | Dup name -> "duplicate constructor = " ^ name
  | Ctor name -> "unknown constructor = " ^ name
  | Arms (expected, actual) ->
      "match arms expected = " ^ string_of_int expected ^ " actual = "
      ^ string_of_int actual
  | Order (expected, actual) ->
      "match arm expected = " ^ expected ^ " actual = " ^ actual
  | Type name -> "match payload type changed = " ^ name
  | Mode (name, expected, actual) ->
      "match payload mode name = " ^ name ^ " expected = "
      ^ C_type.mul_text expected ^ " actual = " ^ C_type.mul_text actual
  | Id -> "match binder id outside profile"