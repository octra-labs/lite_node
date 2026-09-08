(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type arr = {
  mul : C_type.mul;
  caps : C_syn.bind list;
  arg : C_syn.bind;
  out : C_syn.typ;
  eff : C_eff.t;
  lim : C_limit.t option;
}

type fn = {
  name : C_syn.name;
  arr : arr;
  body : C_syn.t;
}

type t =
  | Ret of C_syn.t
  | Let of C_syn.bind * C_syn.t * t
  | If of C_syn.t * t * t
  | Call of C_syn.bind * C_syn.name * C_syn.t list * C_syn.t * t

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Dup of string
  | Fn of string
  | Direct of string
  | Mode of string
  | Arity of string * int * int
  | Out of string
  | Atom of string
  | Mark of string * C_eff.t * C_eff.t
  | Limit of string
  | Over of string * C_limit.t * C_limit.t
  | Fns of int * int
  | Depth of int * int
  | Nodes of int * int

let arr mul caps arg out = { mul; caps; arg; out; eff = C_eff.empty; lim = None }
let marks arr eff = { arr with eff }
let under arr lim = { arr with lim = Some lim }
let fn name arr body = { name; arr; body }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let rec find name = function
  | [] -> None
  | item :: _ when C_syn.name_equal name item.name -> Some item
  | _ :: rest -> find name rest

let mode caps =
  let rec walk linear = function
    | [] -> Ok (if linear then C_type.One else C_type.Many)
    | (item : C_syn.bind) :: rest ->
      begin
        match item.mul with
        | C_type.Zero -> Error (Mode (C_syn.name_text item.name))
        | C_type.One -> walk true rest
        | C_type.Many -> walk linear rest
      end
  in
  walk false caps

let unique values =
  let rec walk seen = function
    | [] -> Ok ()
    | (item : C_syn.bind) :: rest ->
      if List.exists (C_syn.name_equal item.name) seen then
        Error (Dup (C_syn.name_text item.name))
      else walk (item.name :: seen) rest
  in
  walk [] values

let limit_ok = function
  | None -> true
  | Some value -> C_limit.valid value

let valid item =
  let values = item.arr.caps @ [item.arr.arg] in
  let* actual = mode item.arr.caps in
  if actual <> item.arr.mul then Error (Mode (C_syn.name_text item.name))
  else if not (List.for_all C_eff.valid (C_eff.to_list item.arr.eff)) then
    Error (Atom (C_syn.name_text item.name))
  else if not (limit_ok item.arr.lim) then
    Error (Limit (C_syn.name_text item.name))
  else
    let* () = unique values in
    let* out = Result.map_error (fun error -> Low error) (C_low.typ item.arr.out) in
    let* prog = Result.map_error (fun error -> Low error)
      (C_low.prog values item.body) in
    let* info = Result.map_error (fun error -> Check error)
      (C_check.check_in prog.inputs prog.term) in
    if not (C_type.equal info.typ out) then
      Error (Out (C_syn.name_text item.name))
    else if not (C_eff.subset info.eff item.arr.eff) then
      Error (Mark (C_syn.name_text item.name, item.arr.eff, info.eff))
    else
      match item.arr.lim with
      | None -> Ok ()
      | Some lim when C_limit.le info.res lim -> Ok ()
      | Some lim -> Error (Over (C_syn.name_text item.name, lim, info.res))

let def = valid

let defs values =
  let rec limit count = function
    | [] -> Ok ()
    | _ when count = C_check.max_inputs ->
      Error (Fns (C_check.max_inputs, count + 1))
    | _ :: rest -> limit (count + 1) rest
  in
  let rec walk seen = function
    | [] -> Ok ()
    | item :: rest ->
      if List.exists (C_syn.name_equal item.name) seen then
        Error (Dup (C_syn.name_text item.name))
      else
        let* () = valid item in
        walk (item.name :: seen) rest
  in
  let* () = limit 0 values in
  walk [] values

let rec drop name = function
  | [] -> []
  | (key, _) :: rest when C_syn.name_equal name key -> drop name rest
  | item :: rest -> item :: drop name rest

let drops names env = List.fold_left (fun env name -> drop name env) env names

let rec rename env = function
  | C_syn.KUnit -> C_syn.KUnit
  | C_syn.KBool value -> C_syn.KBool value
  | C_syn.KInt value -> C_syn.KInt value
  | C_syn.KBytes value -> C_syn.KBytes value
  | C_syn.KVec (typ, values) ->
    C_syn.KVec (typ, List.map (rename env) values)
  | C_syn.KSeq (cap, typ, values) ->
    C_syn.KSeq (cap, typ, List.map (rename env) values)
  | C_syn.Var name ->
    begin
      match List.find_opt (fun (key, _) -> C_syn.name_equal name key) env with
      | Some (_, name) -> C_syn.Var name
      | None -> C_syn.Var name
    end
  | C_syn.Let (item, value, body) ->
    C_syn.Let (item, rename env value, rename (drop item.name env) body)
  | C_syn.If (guard, yes, no) ->
    C_syn.If (rename env guard, rename env yes, rename env no)
  | C_syn.Pair (left, right) -> C_syn.Pair (rename env left, rename env right)
  | C_syn.Unpair (pair, left, right, body) ->
    C_syn.Unpair (rename env pair, left, right,
      rename (drops [left.name; right.name] env) body)
  | C_syn.Fst value -> C_syn.Fst (rename env value)
  | C_syn.Snd value -> C_syn.Snd (rename env value)
  | C_syn.Inl (value, typ) -> C_syn.Inl (rename env value, typ)
  | C_syn.Inr (typ, value) -> C_syn.Inr (typ, rename env value)
  | C_syn.Case (value, left, yes, right, no) ->
    C_syn.Case (rename env value, left, rename (drop left.name env) yes,
      right, rename (drop right.name env) no)
  | C_syn.Act (atom, body) -> C_syn.Act (atom, rename env body)
  | C_syn.Add (left, right) -> C_syn.Add (rename env left, rename env right)
  | C_syn.Sub (left, right) -> C_syn.Sub (rename env left, rename env right)
  | C_syn.Mul (left, right) -> C_syn.Mul (rename env left, rename env right)
  | C_syn.Div (left, right) -> C_syn.Div (rename env left, rename env right)
  | C_syn.Mod (left, right) -> C_syn.Mod (rename env left, rename env right)
  | C_syn.Neg value -> C_syn.Neg (rename env value)
  | C_syn.Abs value -> C_syn.Abs (rename env value)
  | C_syn.Fit (typ, value) -> C_syn.Fit (typ, rename env value)
  | C_syn.Wide value -> C_syn.Wide (rename env value)
  | C_syn.Length value -> C_syn.Length (rename env value)
  | C_syn.Eq (typ, left, right) ->
    C_syn.Eq (typ, rename env left, rename env right)
  | C_syn.Cmp (rel, left, right) ->
    C_syn.Cmp (rel, rename env left, rename env right)
  | C_syn.Cat (left, right) -> C_syn.Cat (rename env left, rename env right)
  | C_syn.Take (len, value) -> C_syn.Take (len, rename env value)
  | C_syn.Drop (len, value) -> C_syn.Drop (len, rename env value)
  | C_syn.Vcat (left, right) -> C_syn.Vcat (rename env left, rename env right)
  | C_syn.At (index, value) -> C_syn.At (index, rename env value)
  | C_syn.Uncons value -> C_syn.Uncons (rename env value)
  | C_syn.Vfold (vector, seed, fold) ->
    C_syn.Vfold (rename env vector, rename env seed,
      C_syn.fold fold.item fold.state
        (rename (drops [fold.item.name; fold.state.name] env) fold.body))
  | C_syn.Step (cap, value) -> C_syn.Step (rename env cap, rename env value)
  | C_syn.Close cap -> C_syn.Close (rename env cap)

let aliases values =
  let rec walk index out = function
    | [] -> Ok (List.rev out)
    | (item : C_syn.bind) :: rest ->
      begin
        match C_nat.of_int index with
        | Some id ->
          let alias = C_syn.bind (C_syn.slot id) item.mul item.typ in
          walk (index + 1) (alias :: out) rest
        | None -> Error (Low C_low.Count)
      end
  in
  walk 0 [] values

let call item caps arg =
  let formals = item.arr.caps @ [item.arr.arg] in
  let actuals = caps @ [arg] in
  let* aliases = aliases formals in
  let env = List.map2 (fun formal alias -> formal.C_syn.name, alias.C_syn.name)
    formals aliases in
  let body = rename env item.body in
  Ok (List.fold_right2
    (fun formal actual body -> C_syn.Let (formal, actual, body))
    aliases actuals body)

let data typ =
  match C_low.typ typ with
  | Ok typ -> C_type.kind typ = C_type.Data
  | Error _ -> false

let direct item =
  C_eff.equal item.arr.eff C_eff.empty
  && data item.arr.out
  && List.for_all
    (fun (value : C_syn.bind) -> value.mul = C_type.Many)
    (item.arr.caps @ [item.arr.arg])

let apply item actuals =
  let expected = List.length item.arr.caps + 1 in
  let actual = List.length actuals in
  if expected <> actual then
    Error (Arity (C_syn.name_text item.name, expected, actual))
  else
    let rec split caps = function
      | [arg] -> Ok (List.rev caps, arg)
      | value :: rest -> split (value :: caps) rest
      | [] -> Error (Arity (C_syn.name_text item.name, expected, actual))
    in
    let* caps, arg = split [] actuals in
    call item caps arg

let same_type left right =
  match C_low.typ left, C_low.typ right with
  | Ok left, Ok right -> C_type.equal left right
  | _ -> false

type job =
  | Syn of int * C_syn.t
  | Fun of int * t

let push depth values rest =
  List.fold_left (fun out value -> Syn (depth, value) :: out) rest values

let shape fns body =
  let rec walk count = function
    | [] -> Ok ()
    | _ when count > C_check.max_nodes ->
      Error (Nodes (C_check.max_nodes, count))
    | Syn (depth, _) :: _ | Fun (depth, _) :: _
        when depth > C_check.max_depth ->
      Error (Depth (C_check.max_depth, depth))
    | Syn (depth, term) :: rest ->
      let next = depth + 1 in
      let rest =
        match term with
        | C_syn.KUnit | C_syn.KBool _ | C_syn.KInt _ | C_syn.KBytes _
        | C_syn.Var _ -> rest
        | C_syn.KVec (_, values) | C_syn.KSeq (_, _, values) ->
          push next values rest
        | C_syn.Let (_, value, body)
        | C_syn.Pair (value, body)
        | C_syn.Add (value, body)
        | C_syn.Sub (value, body)
        | C_syn.Mul (value, body)
        | C_syn.Div (value, body)
        | C_syn.Mod (value, body)
        | C_syn.Eq (_, value, body)
        | C_syn.Cmp (_, value, body)
        | C_syn.Cat (value, body)
        | C_syn.Vcat (value, body)
        | C_syn.Step (value, body) ->
          Syn (next, value) :: Syn (next, body) :: rest
        | C_syn.If (guard, yes, no) ->
          Syn (next, guard) :: Syn (next, yes) :: Syn (next, no) :: rest
        | C_syn.Unpair (pair, _, _, body) ->
          Syn (next, pair) :: Syn (next, body) :: rest
        | C_syn.Fst value | C_syn.Snd value
        | C_syn.Inl (value, _) | C_syn.Inr (_, value)
        | C_syn.Act (_, value) | C_syn.Neg value | C_syn.Abs value
        | C_syn.Fit (_, value) | C_syn.Wide value | C_syn.Length value
        | C_syn.Take (_, value)
        | C_syn.Drop (_, value) | C_syn.At (_, value)
        | C_syn.Uncons value | C_syn.Close value -> Syn (next, value) :: rest
        | C_syn.Case (value, _, yes, _, no) ->
          Syn (next, value) :: Syn (next, yes) :: Syn (next, no) :: rest
        | C_syn.Vfold (vector, seed, fold) ->
          Syn (next, vector) :: Syn (next, seed) :: Syn (next, fold.body) :: rest
      in
      walk (count + 1) rest
    | Fun (depth, term) :: rest ->
      begin
        match term with
        | Ret term -> walk count (Syn (depth, term) :: rest)
        | Let (_, value, body) ->
          let next = depth + 1 in
          walk (count + 1) (Syn (next, value) :: Fun (next, body) :: rest)
        | If (guard, yes, no) ->
          let next = depth + 1 in
          walk (count + 1)
            (Syn (next, guard) :: Fun (next, yes) :: Fun (next, no) :: rest)
        | Call (result, name, caps, arg, rest_term) ->
          begin
            match find name fns with
            | None -> Error (Fn (C_syn.name_text name))
            | Some item ->
              let expected = List.length item.arr.caps in
              let actual = List.length caps in
              if expected <> actual then
                Error (Arity (C_syn.name_text name, expected, actual))
              else if not (same_type result.typ item.arr.out) then
                Error (Out (C_syn.name_text name))
              else
                let call_depth = depth + 1 in
                let body_depth = call_depth + expected + 1 in
                let actuals = caps @ [arg] in
                let rec actual_jobs offset values out =
                  match values with
                  | [] -> out
                  | value :: values ->
                    actual_jobs (offset + 1) values
                      (Syn (call_depth + offset + 1, value) :: out)
                in
                let jobs =
                  Syn (body_depth, item.body)
                  :: Fun (depth + 1, rest_term)
                  :: actual_jobs 0 actuals rest
                in
                walk (count + expected + 2) jobs
          end
      end
  in
  walk 0 [Fun (0, body)]

let rec expand fns = function
  | Ret term -> Ok term
  | Let (bind, value, body) ->
    let* body = expand fns body in
    Ok (C_syn.Let (bind, value, body))
  | If (guard, yes, no) ->
    let* yes = expand fns yes in
    let* no = expand fns no in
    Ok (C_syn.If (guard, yes, no))
  | Call (result, name, caps, arg, rest) ->
    begin
      match find name fns with
      | None -> Error (Fn (C_syn.name_text name))
      | Some item ->
        let expected = List.length item.arr.caps in
        let actual = List.length caps in
        if expected <> actual then
          Error (Arity (C_syn.name_text name, expected, actual))
        else if not (same_type result.typ item.arr.out) then
          Error (Out (C_syn.name_text name))
        else
          let* rest = expand fns rest in
          let* value = call item caps arg in
          Ok (C_syn.Let (result, value, rest))
    end

let resolve fns body =
  let* () = defs fns in
  let* () = shape fns body in
  expand fns body

let names inputs fns body =
  let* body = resolve fns body in
  match C_low.names inputs body with
  | Some values -> Ok values
  | None -> Error (Low C_low.Count)

let lower inputs fns body =
  let* body = resolve fns body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs body)

let check inputs fns body =
  let* prog = lower inputs fns body in
  Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term)

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Dup name -> "duplicate name = " ^ name
  | Fn name -> "unknown function = " ^ name
  | Direct name -> "function requires explicit use = " ^ name
  | Mode name -> "function mode invalid = " ^ name
  | Arity (name, expected, actual) ->
    "function arity name = " ^ name ^ " expected = " ^ string_of_int expected
    ^ " actual = " ^ string_of_int actual
  | Out name -> "function result type changed = " ^ name
  | Atom name -> "function mark atom invalid = " ^ name
  | Mark (name, declared, actual) ->
    "function marks exceeded name = " ^ name ^ " declared = "
    ^ C_eff.text declared ^ " actual = " ^ C_eff.text actual
  | Limit name -> "function under invalid = " ^ name
  | Over (name, declared, actual) ->
    "function under exceeded name = " ^ name ^ " declared = "
    ^ C_limit.text declared ^ " actual = " ^ C_limit.text actual
  | Fns (max, actual) ->
    "function count max = " ^ string_of_int max ^ " actual = "
    ^ string_of_int actual
  | Depth (max, actual) ->
    "function depth max = " ^ string_of_int max ^ " actual = "
    ^ string_of_int actual
  | Nodes (max, actual) ->
    "function nodes max = " ^ string_of_int max ^ " actual = "
    ^ string_of_int actual