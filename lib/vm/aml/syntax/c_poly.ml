(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type kind = Data | Res

type par = {
  pname : C_syn.name;
  pkind : kind;
}

type actual = {
  atyp : C_type.t;
  araw : C_decl.typ;
}

type fn = {
  pars : par list;
  base : C_spec.fn;
}

type error =
  | Dup of string
  | Count of int * int
  | Arity of string * int * int
  | Kind of string * kind * C_type.kind
  | Name of string
  | Decl of C_decl.error
  | Raw of C_raw.error
  | Spec of C_spec.error

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let par pname pkind = { pname; pkind }
let name (item : fn) = item.base.name
let pars (item : fn) = item.pars
let pname (item : par) = item.pname
let pkind (item : par) = item.pkind
let base (item : fn) = item.base

let unique values =
  let rec walk seen = function
    | [] -> Ok ()
    | item :: rest ->
        if List.exists (C_syn.name_equal item) seen then
          Error (Dup (C_syn.name_text item))
        else walk (item :: seen) rest
  in
  walk [] values

let fn pars (base : C_spec.fn) =
  let count = List.length pars in
  if count > C_check.max_inputs then Error (Count (C_check.max_inputs, count))
  else
    let binds = base.arr.caps @ [base.arr.arg] in
    let names = List.map (fun item -> item.pname) pars
      @ base.pars @ List.map (fun (item : C_spec.bind) -> item.name) binds in
    let* () = unique names in
    Ok { pars; base }

let rec type_order left right =
  let tag = function
    | C_type.Unit -> 0
    | C_type.Bool -> 1
    | C_type.Int -> 2
    | C_type.Num _ -> 3
    | C_type.Bytes _ -> 4
    | C_type.Vec _ -> 5
    | C_type.Seq _ -> 6
    | C_type.Cap _ -> 7
    | C_type.Enc _ -> 8
    | C_type.Pair _ -> 9
    | C_type.Sum _ -> 10
  in
  let order = Int.compare (tag left) (tag right) in
  if order <> 0 then order
  else
    match left, right with
    | C_type.Unit, C_type.Unit
    | C_type.Bool, C_type.Bool
    | C_type.Int, C_type.Int -> 0
    | C_type.Num (ls, lb), C_type.Num (rs, rb) ->
        let order = compare ls rs in
        if order = 0 then C_nat.compare lb rb else order
    | C_type.Bytes lhs, C_type.Bytes rhs
    | C_type.Cap lhs, C_type.Cap rhs -> C_nat.compare lhs rhs
    | C_type.Enc (lk, lr), C_type.Enc (rk, rr) ->
        let order = C_nat.compare lk rk in
        if order = 0 then C_nat.compare lr rr else order
    | C_type.Vec (ln, lhs), C_type.Vec (rn, rhs) ->
        let order = C_nat.compare ln rn in
        if order = 0 then type_order lhs rhs else order
    | C_type.Seq (ln, lhs), C_type.Seq (rn, rhs) ->
        let order = C_nat.compare ln rn in
        if order = 0 then type_order lhs rhs else order
    | C_type.Pair (la, lb), C_type.Pair (ra, rb)
    | C_type.Sum (la, lb), C_type.Sum (ra, rb) ->
        let order = type_order la ra in
        if order = 0 then type_order lb rb else order
    | _ -> 0

let compare_actuals left right =
  let rec walk left right =
    match left, right with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | lhs :: ls, rhs :: rs ->
        let order = type_order lhs.atyp rhs.atyp in
        if order = 0 then walk ls rs else order
  in
  walk left right

let kind_text = function Data -> "data" | Res -> "res"

let native_kind = function
  | C_type.Data -> Data
  | C_type.Res -> Res

let actual base par raw =
  let* typ = Result.map_error (fun error -> Decl error) (C_decl.typ_in base raw) in
  let* value = Result.map_error
    (fun error -> Decl (C_decl.Low error)) (C_low.typ typ) in
  let got = native_kind (C_type.kind value) in
  if got = par.pkind then Ok { atyp = value; araw = C_decl.Exact typ }
  else Error (Kind (C_syn.name_text par.pname, par.pkind, C_type.kind value))

let actuals base pars values =
  let expected = List.length pars in
  let got = List.length values in
  if expected <> got then Error (Arity ("kind", expected, got))
  else
    let rec walk out pars values =
      match pars, values with
      | [], [] -> Ok (List.rev out)
      | par :: ps, value :: vs ->
          let* value = actual base par value in
          walk (value :: out) ps vs
      | _, _ -> Error (Arity ("kind", List.length pars, List.length values))
    in
    walk [] pars values

let find env name =
  let rec walk = function
    | [] -> None
    | (key, value) :: _ when C_syn.name_equal key name -> Some value
    | _ :: rest -> walk rest
  in
  walk env

let rec typ env = function
  | C_decl.Var name ->
      begin
        match find env name with
        | Some value -> Ok value
        | None -> Error (Name (C_syn.name_text name))
      end
  | C_decl.Vec (len, elem) ->
      let* elem = typ env elem in
      Ok (C_decl.Vec (len, elem))
  | C_decl.Seq (cap, elem) ->
      let* elem = typ env elem in
      Ok (C_decl.Seq (cap, elem))
  | C_decl.Pair (left, right) ->
      let* left = typ env left in
      let* right = typ env right in
      Ok (C_decl.Pair (left, right))
  | C_decl.Result (good, bad) ->
      let* good = typ env good in
      let* bad = typ env bad in
      Ok (C_decl.Result (good, bad))
  | (C_decl.Unit | C_decl.Bool | C_decl.Int | C_decl.Num _ | C_decl.Bytes _
    | C_decl.Cap _ | C_decl.Exact _) as value -> Ok value

let bind env (item : C_raw.bind) =
  let* typ = typ env item.typ in
  Ok (C_raw.bind item.name item.mul typ)

let rec term env = function
  | C_raw.KUnit -> Ok C_raw.KUnit
  | C_raw.KBool value -> Ok (C_raw.KBool value)
  | C_raw.KInt value -> Ok (C_raw.KInt value)
  | C_raw.KBytes value -> Ok (C_raw.KBytes value)
  | C_raw.KVec (raw, values) ->
      let* raw = typ env raw in
      let* values = terms env [] values in
      Ok (C_raw.KVec (raw, values))
  | C_raw.KSeq (cap, raw, values) ->
      let* raw = typ env raw in
      let* values = terms env [] values in
      Ok (C_raw.KSeq (cap, raw, values))
  | C_raw.Var name -> Ok (C_raw.Var name)
  | C_raw.Let (item, value, body) ->
      let* item = bind env item in
      let* value = term env value in
      let* body = term env body in
      Ok (C_raw.Let (item, value, body))
  | C_raw.If (guard, yes, no) ->
      let* guard = term env guard in
      let* yes = term env yes in
      let* no = term env no in
      Ok (C_raw.If (guard, yes, no))
  | C_raw.Pair (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Pair (left, right))
  | C_raw.Unpair (value, left, right, body) ->
      let* value = term env value in
      let* left = bind env left in
      let* right = bind env right in
      let* body = term env body in
      Ok (C_raw.Unpair (value, left, right, body))
  | C_raw.Fst value ->
      let* value = term env value in
      Ok (C_raw.Fst value)
  | C_raw.Snd value ->
      let* value = term env value in
      Ok (C_raw.Snd value)
  | C_raw.Inl (value, raw) ->
      let* value = term env value in
      let* raw = typ env raw in
      Ok (C_raw.Inl (value, raw))
  | C_raw.Inr (raw, value) ->
      let* raw = typ env raw in
      let* value = term env value in
      Ok (C_raw.Inr (raw, value))
  | C_raw.Case (value, left, yes, right, no) ->
      let* value = term env value in
      let* left = bind env left in
      let* yes = term env yes in
      let* right = bind env right in
      let* no = term env no in
      Ok (C_raw.Case (value, left, yes, right, no))
  | C_raw.Act (atom, body) ->
      let* body = term env body in
      Ok (C_raw.Act (atom, body))
  | C_raw.Add (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Add (left, right))
  | C_raw.Sub (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Sub (left, right))
  | C_raw.Mul (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Mul (left, right))
  | C_raw.Div (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Div (left, right))
  | C_raw.Mod (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Mod (left, right))
  | C_raw.Neg value ->
      let* value = term env value in
      Ok (C_raw.Neg value)
  | C_raw.Abs value ->
      let* value = term env value in
      Ok (C_raw.Abs value)
  | C_raw.Fit (raw, value) ->
      let* raw = typ env raw in
      let* value = term env value in
      Ok (C_raw.Fit (raw, value))
  | C_raw.Wide value ->
      let* value = term env value in
      Ok (C_raw.Wide value)
  | C_raw.Length value ->
      let* value = term env value in
      Ok (C_raw.Length value)
  | C_raw.Eq (raw, left, right) ->
      let* raw = typ env raw in
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Eq (raw, left, right))
  | C_raw.Cmp (rel, left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Cmp (rel, left, right))
  | C_raw.Cat (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Cat (left, right))
  | C_raw.Take (at, value) ->
      let* value = term env value in
      Ok (C_raw.Take (at, value))
  | C_raw.Drop (at, value) ->
      let* value = term env value in
      Ok (C_raw.Drop (at, value))
  | C_raw.Vcat (left, right) ->
      let* left = term env left in
      let* right = term env right in
      Ok (C_raw.Vcat (left, right))
  | C_raw.At (at, value) ->
      let* value = term env value in
      Ok (C_raw.At (at, value))
  | C_raw.Uncons value ->
      let* value = term env value in
      Ok (C_raw.Uncons value)
  | C_raw.Vfold (vector, seed, fold) ->
      let* vector = term env vector in
      let* seed = term env seed in
      let* item = bind env fold.fitem in
      let* state = bind env fold.fstate in
      let* body = term env fold.fbody in
      Ok (C_raw.Vfold (vector, seed, C_raw.fold item state body))
  | C_raw.Step (cap, value) ->
      let* cap = term env cap in
      let* value = term env value in
      Ok (C_raw.Step (cap, value))
  | C_raw.Close value ->
      let* value = term env value in
      Ok (C_raw.Close value)
  | C_raw.Dmake (decl, name, value) ->
      let* value = term env value in
      Ok (C_raw.Dmake (decl, name, value))
  | C_raw.Dcase (decl, value, arms) ->
      let* value = term env value in
      let* arms = arm_list env [] arms in
      Ok (C_raw.Dcase (decl, value, arms))
  | C_raw.Rmake (decl, values) ->
      let* values = item_list env [] values in
      Ok (C_raw.Rmake (decl, values))
  | C_raw.Rsplit (decl, value, picks, body) ->
      let* value = term env value in
      let* picks = pick_list env [] picks in
      let* body = term env body in
      Ok (C_raw.Rsplit (decl, value, picks, body))
  | C_raw.Quant (mode, source, item, body) ->
      let* source = term env source in
      let* item = bind env item in
      let* body = term env body in
      Ok (C_raw.Quant (mode, source, item, body))
  | C_raw.Weave (count, raw, source, item, body) ->
      let* raw = typ env raw in
      let* source = term env source in
      let* item = bind env item in
      let* body = term env body in
      Ok (C_raw.Weave (count, raw, source, item, body))
  | C_raw.Braid (count, raw, left, right, first, second, body) ->
      let* raw = typ env raw in
      let* left = term env left in
      let* right = term env right in
      let* first = bind env first in
      let* second = bind env second in
      let* body = term env body in
      Ok (C_raw.Braid (count, raw, left, right, first, second, body))
  | C_raw.Loom (count, raw, item, body) ->
      let* raw = typ env raw in
      let* item = bind env item in
      let* body = term env body in
      Ok (C_raw.Loom (count, raw, item, body))
  | C_raw.Orbit (count, seed, item, body) ->
      let* seed = term env seed in
      let* item = bind env item in
      let* body = term env body in
      Ok (C_raw.Orbit (count, seed, item, body))
  | C_raw.Orbit_to (count, turns, seed, item, body) ->
      let* turns = term env turns in
      let* seed = term env seed in
      let* item = bind env item in
      let* body = term env body in
      Ok (C_raw.Orbit_to (count, turns, seed, item, body))
  | C_raw.Wake (count, seed, item, body) ->
      let* seed = term env seed in
      let* item = bind env item in
      let* body = term env body in
      Ok (C_raw.Wake (count, seed, item, body))
  | C_raw.Rift (cut, rest, raw, source) ->
      let* raw = typ env raw in
      let* source = term env source in
      Ok (C_raw.Rift (cut, rest, raw, source))

and terms env out = function
  | [] -> Ok (List.rev out)
  | value :: rest ->
      let* value = term env value in
      terms env (value :: out) rest

and arm_list env out = function
  | [] -> Ok (List.rev out)
  | value :: rest ->
      let name, item, body = C_raw.arm_view value in
      let* item = bind env item in
      let* body = term env body in
      arm_list env (C_raw.arm name item body :: out) rest

and item_list env out = function
  | [] -> Ok (List.rev out)
  | value :: rest ->
      let name, body = C_raw.item_view value in
      let* body = term env body in
      item_list env (C_raw.item name body :: out) rest

and pick_list env out = function
  | [] -> Ok (List.rev out)
  | value :: rest ->
      let name, item = C_raw.pick_view value in
      let* item = bind env item in
      pick_list env (C_raw.pick name item :: out) rest

let bind_spec env (item : C_spec.bind) =
  let* typ = typ env item.typ in
  Ok (C_spec.bind item.name item.mul typ)

let rec bind_specs env out = function
  | [] -> Ok (List.rev out)
  | item :: rest ->
      let* item = bind_spec env item in
      bind_specs env (item :: out) rest

let subst env (base : C_spec.fn) =
  let* caps = bind_specs env [] base.arr.caps in
  let* arg = bind_spec env base.arr.arg in
  let* out = typ env base.arr.out in
  let* body = term env base.body in
  let arr = C_spec.marks (C_spec.arr base.arr.mul caps arg out) base.arr.eff in
  let arr = Option.fold ~none:arr ~some:(C_spec.under arr) base.arr.lim in
  let* item = Result.map_error (fun error -> Spec error)
    (C_spec.fn base.name base.pars arr body) in
  Ok (C_spec.require base.laws item)

let mono size_env name (item : fn) actuals sizes =
  let expected = List.length item.pars in
  let got = List.length actuals in
  if expected <> got then
    Error (Arity (C_syn.name_text item.base.name, expected, got))
  else
    let env = List.map2 (fun par value -> par.pname, value.araw)
      item.pars actuals in
    let* spec = subst env item.base in
    Result.map_error (fun error -> Spec error)
      (C_spec.mono size_env name spec sizes)

let inst base name (item : fn) types sizes =
  let* actuals = actuals base item.pars types in
  let* values = Result.map_error (fun error -> Spec error)
    (C_spec.args base sizes) in
  let* fn = mono base name item actuals values in
  Ok (actuals, values, fn)

let text = function
  | Dup name -> "duplicate form kind binder = " ^ name
  | Count (limit, actual) ->
      Printf.sprintf "form kind count limit = %d actual = %d" limit actual
  | Arity (name, expected, actual) ->
      Printf.sprintf "form kind arity name = %s expected = %d actual = %d"
        name expected actual
  | Kind (name, expected, actual) ->
      "form kind mismatch name = " ^ name ^ " expected = "
      ^ kind_text expected ^ " actual = " ^ kind_text (native_kind actual)
  | Name name -> "unknown form kind binder = " ^ name
  | Decl error -> C_decl.text error
  | Raw error -> C_raw.text error
  | Spec error -> C_spec.text error