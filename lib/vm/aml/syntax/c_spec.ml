(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bind = {
  name : C_syn.name;
  mul : C_type.mul;
  typ : C_decl.typ;
}

type arr = {
  mul : C_type.mul;
  caps : bind list;
  arg : bind;
  out : C_decl.typ;
  eff : C_eff.t;
  lim : C_limit.t option;
}

type fn = {
  name : C_syn.name;
  pars : C_syn.name list;
  laws : C_law.set;
  arr : arr;
  body : C_raw.t;
}

type error =
  | Dup of string
  | Count of int * int
  | Arity of string * int * int
  | Idx of C_idx.error
  | Law of C_law.error
  | Decl of C_decl.error
  | Raw of C_raw.error
  | Fun of C_fun.error

let bind name mul typ = { name; mul; typ }
let arr mul caps arg out = { mul; caps; arg; out; eff = C_eff.empty; lim = None }
let marks arr eff = { arr with eff }
let under arr lim = { arr with lim = Some lim }
let require laws item = { item with laws }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let unique values =
  let rec walk seen = function
    | [] -> Ok ()
    | name :: rest ->
        if List.exists (C_syn.name_equal name) seen then
          Error (Dup (C_syn.name_text name))
        else walk (name :: seen) rest
  in
  walk [] values

let valid item =
  let count = List.length item.pars in
  if count > C_check.max_inputs then
    Error (Count (C_check.max_inputs, count))
  else
    let values = List.map (fun (bind : bind) -> bind.name)
      (item.arr.caps @ [item.arr.arg]) in
    unique (item.pars @ values)

let fn name pars arr body =
  let item = { name; pars; laws = C_law.empty; arr; body } in
  let* () = valid item in
  Ok item

let rec eval env out = function
  | [] -> Ok (List.rev out)
  | term :: rest ->
      let* value = Result.map_error (fun error -> Idx error)
        (C_idx.eval env term) in
      eval env (value :: out) rest

let args env terms = eval env [] terms

let rec env_put env pars values =
  match pars, values with
  | [], [] -> Ok env
  | par :: pars, value :: values ->
      let raw = C_nat.to_z value in
      begin
        match C_idx.lit raw with
        | None -> Error (Idx (C_idx.Nat raw))
        | Some term ->
            let* env = Result.map_error (fun error -> Idx error)
              (C_idx.put env par term) in
            env_put env pars values
      end
  | _, _ -> Error (Count (List.length pars, List.length values))

let bind_in env item =
  let* typ = Result.map_error (fun error -> Decl error)
    (C_decl.typ_in env item.typ) in
  Ok (C_syn.bind item.name item.mul typ)

let rec binds_in env out = function
  | [] -> Ok (List.rev out)
  | item :: rest ->
      let* item = bind_in env item in
      binds_in env (item :: out) rest

let mono base name item values =
  let* () = valid item in
  let expected = List.length item.pars in
  let actual = List.length values in
  if expected <> actual then
    Error (Arity (C_syn.name_text item.name, expected, actual))
  else
    let* env = env_put base item.pars values in
    let* () = Result.map_error (fun error -> Law error)
      (C_law.hold env item.laws) in
    let* caps = binds_in env [] item.arr.caps in
    let* arg = bind_in env item.arr.arg in
    let* out = Result.map_error (fun error -> Decl error)
      (C_decl.typ_in env item.arr.out) in
    let* body = Result.map_error (fun error -> Raw error)
      (C_raw.elab env item.body) in
    let spec = C_fun.marks (C_fun.arr item.arr.mul caps arg out) item.arr.eff in
    let spec = Option.fold ~none:spec ~some:(C_fun.under spec) item.arr.lim in
    let fn = C_fun.fn name spec body in
    let* () = Result.map_error (fun error -> Fun error) (C_fun.def fn) in
    Ok fn

let inst base name item actuals =
  let* values = args base actuals in
  let* fn = mono base name item values in
  Ok (values, fn)

let text = function
  | Dup name -> "duplicate form binder = " ^ name
  | Count (limit, actual) ->
      Printf.sprintf "form size count limit = %d actual = %d" limit actual
  | Arity (name, expected, actual) ->
      Printf.sprintf "form size arity name = %s expected = %d actual = %d"
        name expected actual
  | Idx error -> C_idx.text error
  | Law error -> C_law.text error
  | Decl error -> C_decl.text error
  | Raw error -> C_raw.text error
  | Fun error -> C_fun.text error