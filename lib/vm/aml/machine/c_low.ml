(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Nat of Z.t
  | Free of string
  | Dup of string
  | Type
  | Count
  | Depth of int * int
  | Nodes of int * int
  | Inputs of int * int

type prog = {
  inputs : C_term.bind list;
  term : C_term.t;
}

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let nat value =
  match C_nat.make value with
  | Some value -> Ok value
  | None -> Error (Nat value)

let rec typ_raw = function
  | C_syn.TUnit -> Ok C_type.Unit
  | C_syn.TBool -> Ok C_type.Bool
  | C_syn.TInt -> Ok C_type.Int
  | C_syn.TNum (sign, bits) ->
    let* bits = nat bits in
    Ok (C_type.Num (sign, bits))
  | C_syn.TBytes len ->
    let* len = nat len in
    Ok (C_type.Bytes len)
  | C_syn.TVec (len, elem) ->
    let* len = nat len in
    let* elem = typ_raw elem in
    Ok (C_type.Vec (len, elem))
  | C_syn.TSeq (cap, elem) ->
    let* cap = nat cap in
    let* elem = typ_raw elem in
    Ok (C_type.Seq (cap, elem))
  | C_syn.TCap kind ->
    let* kind = nat kind in
    Ok (C_type.Cap kind)
  | C_syn.TPair (left, right) ->
    let* left = typ_raw left in
    let* right = typ_raw right in
    Ok (C_type.Pair (left, right))
  | C_syn.TSum (left, right) ->
    let* left = typ_raw left in
    let* right = typ_raw right in
    Ok (C_type.Sum (left, right))

let typ_shape value =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, _) :: _ when depth > C_type.max_depth ->
      Error Type
    | _ when nodes >= C_type.max_nodes -> Error Type
    | (depth, value) :: rest ->
      let next = depth + 1 in
      match value with
      | C_syn.TUnit | C_syn.TBool | C_syn.TInt | C_syn.TNum _ ->
        walk (nodes + 1) rest
      | C_syn.TBytes len | C_syn.TCap len ->
        let* _ = nat len in
        walk (nodes + 1) rest
      | C_syn.TVec (len, elem) ->
        let* _ = nat len in
        walk (nodes + 1) ((next, elem) :: rest)
      | C_syn.TSeq (cap, elem) ->
        let* _ = nat cap in
        walk (nodes + 1) ((next, elem) :: rest)
      | C_syn.TPair (left, right) | C_syn.TSum (left, right) ->
        walk (nodes + 1) ((next, left) :: (next, right) :: rest)
  in
  walk 0 [0, value]

let typ value =
  let* () = typ_shape value in
  let* value = typ_raw value in
  if C_type.valid value then Ok value else Error Type

let atom = function
  | C_syn.ARead kind ->
    let* kind = nat kind in
    Ok (C_eff.Read kind)
  | C_syn.AWrite kind ->
    let* kind = nat kind in
    Ok (C_eff.Write kind)
  | C_syn.AEmit kind ->
    let* kind = nat kind in
    Ok (C_eff.Emit kind)
  | C_syn.AFail kind ->
    let* kind = nat kind in
    Ok (C_eff.Fail kind)
  | C_syn.AClose kind ->
    let* kind = nat kind in
    Ok (C_eff.Close kind)

let rel = function
  | C_syn.Lt -> C_term.Lt
  | C_syn.Le -> C_term.Le
  | C_syn.Gt -> C_term.Gt
  | C_syn.Ge -> C_term.Ge

let rec find name = function
  | [] -> None
  | (key, id) :: _ when C_syn.name_equal name key -> Some id
  | _ :: rest -> find name rest

let rec has name = function
  | [] -> false
  | (key, _) :: _ when C_syn.name_equal name key -> true
  | _ :: rest -> has name rest

let fresh next =
  match C_nat.add next C_nat.one with
  | Some after -> Ok (next, after)
  | None -> Error Count

let bind next (bind : C_syn.bind) =
  let* typ = typ bind.typ in
  let* id, after = fresh next in
  Ok (C_term.bind id bind.mul typ, after)

let distinct left right =
  if C_syn.name_equal left.C_syn.name right.C_syn.name then
    Error (Dup (C_syn.name_text left.C_syn.name))
  else Ok ()

let push depth values rest =
  List.fold_left (fun out value -> (depth, value) :: out) rest values

let shape term =
  let rec walk nodes = function
    | [] -> Ok ()
    | (depth, _) :: _ when depth > C_check.max_depth ->
      Error (Depth (C_check.max_depth, depth))
    | _ when nodes >= C_check.max_nodes ->
      Error (Nodes (C_check.max_nodes, nodes + 1))
    | (depth, term) :: rest ->
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
          (next, value) :: (next, body) :: rest
        | C_syn.If (guard, yes, no) ->
          (next, guard) :: (next, yes) :: (next, no) :: rest
        | C_syn.Unpair (pair, _, _, body) ->
          (next, pair) :: (next, body) :: rest
        | C_syn.Fst value | C_syn.Snd value
        | C_syn.Inl (value, _) | C_syn.Inr (_, value)
        | C_syn.Act (_, value) | C_syn.Neg value | C_syn.Abs value
        | C_syn.Fit (_, value) | C_syn.Wide value | C_syn.Length value
        | C_syn.Take (_, value)
        | C_syn.Drop (_, value) | C_syn.At (_, value)
        | C_syn.Uncons value | C_syn.Close value ->
          (next, value) :: rest
        | C_syn.Case (value, _, yes, _, no) ->
          (next, value) :: (next, yes) :: (next, no) :: rest
        | C_syn.Vfold (vector, seed, fold) ->
          (next, vector) :: (next, seed) :: (next, fold.body) :: rest
      in
      walk (nodes + 1) rest
  in
  walk 0 [0, term]

let rec list env next out = function
  | [] -> Ok (List.rev out, next)
  | value :: rest ->
    let* value, next = lower env next value in
    list env next (value :: out) rest

and lower env next = function
  | C_syn.KUnit -> Ok (C_term.Unit, next)
  | C_syn.KBool value -> Ok (C_term.Bool value, next)
  | C_syn.KInt value -> Ok (C_term.Int value, next)
  | C_syn.KBytes value -> Ok (C_term.Bytes value, next)
  | C_syn.KVec (elem, values) ->
    let* elem = typ elem in
    let* values, next = list env next [] values in
    Ok (C_term.Vec (elem, values), next)
  | C_syn.KSeq (cap, elem, values) ->
    let* elem = typ elem in
    let* values, next = list env next [] values in
    Ok (C_term.Seq (cap, elem, values), next)
  | C_syn.Var name ->
    begin
      match find name env with
      | Some id -> Ok (C_term.Var id, next)
      | None -> Error (Free (C_syn.name_text name))
    end
  | C_syn.Let (item, value, body) ->
    let* value, next = lower env next value in
    let* core, next = bind next item in
    let* body, next = lower ((item.name, core.id) :: env) next body in
    Ok (C_term.Let (core, value, body), next)
  | C_syn.If (guard, yes, no) ->
    let* guard, next = lower env next guard in
    let* yes, next = lower env next yes in
    let* no, next = lower env next no in
    Ok (C_term.If (guard, yes, no), next)
  | C_syn.Pair (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Pair (left, right), next)
  | C_syn.Unpair (pair, left, right, body) ->
    let* () = distinct left right in
    let* pair, next = lower env next pair in
    let* left_core, next = bind next left in
    let* right_core, next = bind next right in
    let body_env =
      (right.name, right_core.id) :: (left.name, left_core.id) :: env
    in
    let* body, next = lower body_env next body in
    Ok (C_term.Unpair (pair, left_core, right_core, body), next)
  | C_syn.Fst value ->
    let* value, next = lower env next value in
    Ok (C_term.Fst value, next)
  | C_syn.Snd value ->
    let* value, next = lower env next value in
    Ok (C_term.Snd value, next)
  | C_syn.Inl (value, right) ->
    let* value, next = lower env next value in
    let* right = typ right in
    Ok (C_term.Inl (value, right), next)
  | C_syn.Inr (left, value) ->
    let* left = typ left in
    let* value, next = lower env next value in
    Ok (C_term.Inr (left, value), next)
  | C_syn.Case (value, left, yes, right, no) ->
    let* value, next = lower env next value in
    let* left_core, next = bind next left in
    let* yes, next = lower ((left.name, left_core.id) :: env) next yes in
    let* right_core, next = bind next right in
    let* no, next = lower ((right.name, right_core.id) :: env) next no in
    Ok (C_term.Case (value, left_core, yes, right_core, no), next)
  | C_syn.Act (action, body) ->
    let* action = atom action in
    let* body, next = lower env next body in
    Ok (C_term.Act (action, body), next)
  | C_syn.Add (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Add (left, right), next)
  | C_syn.Sub (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Sub (left, right), next)
  | C_syn.Mul (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Mul (left, right), next)
  | C_syn.Div (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Div (left, right), next)
  | C_syn.Mod (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Mod (left, right), next)
  | C_syn.Neg value ->
    let* value, next = lower env next value in
    Ok (C_term.Neg value, next)
  | C_syn.Abs value ->
    let* value, next = lower env next value in
    Ok (C_term.Abs value, next)
  | C_syn.Fit (target, value) ->
    let* target = typ target in
    let* value, next = lower env next value in
    Ok (C_term.Fit (target, value), next)
  | C_syn.Wide value ->
    let* value, next = lower env next value in
    Ok (C_term.Wide value, next)
  | C_syn.Length value ->
    let* value, next = lower env next value in
    Ok (C_term.Length value, next)
  | C_syn.Eq (kind, left, right) ->
    let* kind = typ kind in
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Eq (kind, left, right), next)
  | C_syn.Cmp (order, left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Cmp (rel order, left, right), next)
  | C_syn.Cat (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Cat (left, right), next)
  | C_syn.Take (len, value) ->
    let* len = nat len in
    let* value, next = lower env next value in
    Ok (C_term.Take (len, value), next)
  | C_syn.Drop (len, value) ->
    let* len = nat len in
    let* value, next = lower env next value in
    Ok (C_term.Drop (len, value), next)
  | C_syn.Vcat (left, right) ->
    let* left, next = lower env next left in
    let* right, next = lower env next right in
    Ok (C_term.Vcat (left, right), next)
  | C_syn.At (index, value) ->
    let* index = nat index in
    let* value, next = lower env next value in
    Ok (C_term.At (index, value), next)
  | C_syn.Uncons value ->
    let* value, next = lower env next value in
    Ok (C_term.Uncons value, next)
  | C_syn.Vfold (vector, seed, fold) ->
    let* () = distinct fold.item fold.state in
    let* vector, next = lower env next vector in
    let* seed, next = lower env next seed in
    let* item, next = bind next fold.item in
    let* state, next = bind next fold.state in
    let body_env =
      (fold.state.name, state.id) :: (fold.item.name, item.id) :: env
    in
    let* body, next = lower body_env next fold.body in
    Ok (C_term.Vfold (vector, seed, C_term.fold item state body), next)
  | C_syn.Step (cap, value) ->
    let* cap, next = lower env next cap in
    let* value, next = lower env next value in
    Ok (C_term.Step (cap, value), next)
  | C_syn.Close cap ->
    let* cap, next = lower env next cap in
    Ok (C_term.Close cap, next)

let term term =
  let* () = shape term in
  let* term, _ = lower [] C_nat.zero term in
  Ok term

let rec name_order term tail =
  match term with
  | C_syn.KUnit | C_syn.KBool _ | C_syn.KInt _ | C_syn.KBytes _
  | C_syn.Var _ -> tail
  | C_syn.KVec (_, values) | C_syn.KSeq (_, _, values) ->
    List.fold_right name_order values tail
  | C_syn.Let (bind, value, body) ->
    name_order value (bind.name :: name_order body tail)
  | C_syn.If (guard, yes, no) ->
    name_order guard (name_order yes (name_order no tail))
  | C_syn.Pair (left, right)
  | C_syn.Add (left, right)
  | C_syn.Sub (left, right)
  | C_syn.Mul (left, right)
  | C_syn.Div (left, right)
  | C_syn.Mod (left, right)
  | C_syn.Eq (_, left, right)
  | C_syn.Cmp (_, left, right)
  | C_syn.Cat (left, right)
  | C_syn.Vcat (left, right)
  | C_syn.Step (left, right) ->
    name_order left (name_order right tail)
  | C_syn.Unpair (pair, left, right, body) ->
    name_order pair (left.name :: right.name :: name_order body tail)
  | C_syn.Fst value | C_syn.Snd value | C_syn.Inl (value, _)
  | C_syn.Inr (_, value) | C_syn.Act (_, value) | C_syn.Neg value
  | C_syn.Abs value | C_syn.Fit (_, value) | C_syn.Wide value
  | C_syn.Length value | C_syn.Take (_, value) | C_syn.Drop (_, value)
  | C_syn.At (_, value) | C_syn.Uncons value | C_syn.Close value ->
    name_order value tail
  | C_syn.Case (value, left, yes, right, no) ->
    name_order value
      (left.name :: name_order yes (right.name :: name_order no tail))
  | C_syn.Vfold (vector, seed, fold) ->
    name_order vector
      (name_order seed
        (fold.item.name :: fold.state.name :: name_order fold.body tail))

let names inputs term =
  let ordered =
    List.fold_right
      (fun (bind : C_syn.bind) tail -> bind.name :: tail)
      inputs
      (name_order term [])
  in
  let rec number index out = function
    | [] -> Some (List.rev out)
    | name :: rest ->
      begin
        match C_nat.of_int index with
        | Some id ->
          number (index + 1) ((id, C_syn.name_text name) :: out) rest
        | None -> None
      end
  in
  number 0 [] ordered

let rec inputs env next out = function
  | [] -> Ok (env, next, List.rev out)
  | (input : C_syn.bind) :: rest ->
    if has input.name env then Error (Dup (C_syn.name_text input.name))
    else
      let* core, next = bind next input in
      inputs ((input.name, core.id) :: env) next (core :: out) rest

let input_limit values =
  let rec walk count = function
    | [] -> Ok ()
    | _ when count = C_check.max_inputs ->
      Error (Inputs (C_check.max_inputs, count + 1))
    | _ :: rest -> walk (count + 1) rest
  in
  walk 0 values

let prog values body =
  let* () = input_limit values in
  let* () = shape body in
  let* env, next, values = inputs [] C_nat.zero [] values in
  let* term, _ = lower env next body in
  Ok { inputs = values; term }

let text = function
  | Nat value -> "natural outside profile = " ^ Z.to_string value
  | Free name -> "free name = " ^ name
  | Dup name -> "duplicate name = " ^ name
  | Type -> "type outside profile"
  | Count -> "binder count outside profile"
  | Depth (max, actual) ->
    "term depth max = " ^ string_of_int max ^ " actual = " ^ string_of_int actual
  | Nodes (max, actual) ->
    "term nodes max = " ^ string_of_int max ^ " actual = " ^ string_of_int actual
  | Inputs (max, actual) ->
    "input count max = " ^ string_of_int max ^ " actual = " ^ string_of_int actual