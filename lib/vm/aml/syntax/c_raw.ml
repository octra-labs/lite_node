(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bind = {
  name : C_syn.name;
  mul : C_type.mul;
  typ : C_decl.typ;
}

type arm = {
  aname : C_syn.name;
  abind : bind;
  abody : t;
}

and item = {
  iname : C_syn.name;
  ivalue : t;
}

and pick = {
  pname : C_syn.name;
  pbind : bind;
}

and t =
  | KUnit
  | KBool of bool
  | KInt of Z.t
  | KBytes of string
  | KVec of C_decl.typ * t list
  | KSeq of C_idx.t * C_decl.typ * t list
  | Var of C_syn.name
  | Let of bind * t * t
  | If of t * t * t
  | Pair of t * t
  | Unpair of t * bind * bind * t
  | Fst of t
  | Snd of t
  | Inl of t * C_decl.typ
  | Inr of C_decl.typ * t
  | Case of t * bind * t * bind * t
  | Act of C_syn.atom * t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Mod of t * t
  | Neg of t
  | Abs of t
  | Fit of C_decl.typ * t
  | Wide of t
  | Length of t
  | Eq of C_decl.typ * t * t
  | Cmp of C_syn.rel * t * t
  | Cat of t * t
  | Take of Z.t * t
  | Drop of Z.t * t
  | Vcat of t * t
  | At of Z.t * t
  | Uncons of t
  | Vfold of t * t * fold
  | Step of t * t
  | Close of t
  | Dmake of C_data.decl * C_syn.name * t
  | Dcase of C_data.decl * t * arm list
  | Rmake of C_rec.decl * item list
  | Rsplit of C_rec.decl * t * pick list * t
  | Quant of C_quant.mode * t * bind * t
  | Weave of C_idx.t * C_decl.typ * t * bind * t
  | Braid of C_idx.t * C_decl.typ * t * t * bind * bind * t
  | Loom of C_idx.t * C_decl.typ * bind * t
  | Orbit of C_idx.t * t * bind * t
  | Orbit_to of C_idx.t * t * t * bind * t
  | Wake of C_idx.t * t * bind * t
  | Rift of C_idx.t * C_idx.t * C_decl.typ * t

and fold = {
  fitem : bind;
  fstate : bind;
  fbody : t;
}

type error =
  | Idx of C_idx.error
  | Decl of C_decl.error
  | Data of C_data.error
  | Rec of C_rec.error
  | Quant_error of C_quant.error
  | Weave_error of C_weave.error
  | Braid_error of C_braid.error
  | Loom_error of C_loom.error
  | Orbit_error of C_orbit.error
  | Wake_error of C_wake.error
  | Rift_error of C_rift.error
  | Fresh
  | Depth of int * int
  | Nodes of int * int

let bind name mul typ = { name; mul; typ }
let fold fitem fstate fbody = { fitem; fstate; fbody }
let arm aname abind abody = { aname; abind; abody }
let item iname ivalue = { iname; ivalue }
let pick pname pbind = { pname; pbind }
let arm_view item = item.aname, item.abind, item.abody
let item_view item = item.iname, item.ivalue
let pick_view item = item.pname, item.pbind

let bind_of_syn item = bind item.C_syn.name item.mul (C_decl.Exact item.typ)

let rec of_syn = function
  | C_syn.KUnit -> KUnit
  | C_syn.KBool value -> KBool value
  | C_syn.KInt value -> KInt value
  | C_syn.KBytes value -> KBytes value
  | C_syn.KVec (typ, values) ->
      KVec (C_decl.Exact typ, List.map of_syn values)
  | C_syn.KSeq (cap, typ, values) ->
      KSeq (C_idx.exact cap, C_decl.Exact typ, List.map of_syn values)
  | C_syn.Var name -> Var name
  | C_syn.Let (item, value, body) ->
      Let (bind_of_syn item, of_syn value, of_syn body)
  | C_syn.If (guard, yes, no) -> If (of_syn guard, of_syn yes, of_syn no)
  | C_syn.Pair (left, right) -> Pair (of_syn left, of_syn right)
  | C_syn.Unpair (value, left, right, body) ->
      Unpair (of_syn value, bind_of_syn left, bind_of_syn right, of_syn body)
  | C_syn.Fst value -> Fst (of_syn value)
  | C_syn.Snd value -> Snd (of_syn value)
  | C_syn.Inl (value, right) -> Inl (of_syn value, C_decl.Exact right)
  | C_syn.Inr (left, value) -> Inr (C_decl.Exact left, of_syn value)
  | C_syn.Case (value, left, yes, right, no) ->
      Case (of_syn value, bind_of_syn left, of_syn yes,
        bind_of_syn right, of_syn no)
  | C_syn.Act (atom, value) -> Act (atom, of_syn value)
  | C_syn.Add (left, right) -> Add (of_syn left, of_syn right)
  | C_syn.Sub (left, right) -> Sub (of_syn left, of_syn right)
  | C_syn.Mul (left, right) -> Mul (of_syn left, of_syn right)
  | C_syn.Div (left, right) -> Div (of_syn left, of_syn right)
  | C_syn.Mod (left, right) -> Mod (of_syn left, of_syn right)
  | C_syn.Neg value -> Neg (of_syn value)
  | C_syn.Abs value -> Abs (of_syn value)
  | C_syn.Fit (typ, value) -> Fit (C_decl.Exact typ, of_syn value)
  | C_syn.Wide value -> Wide (of_syn value)
  | C_syn.Length value -> Length (of_syn value)
  | C_syn.Eq (typ, left, right) ->
      Eq (C_decl.Exact typ, of_syn left, of_syn right)
  | C_syn.Cmp (rel, left, right) -> Cmp (rel, of_syn left, of_syn right)
  | C_syn.Cat (left, right) -> Cat (of_syn left, of_syn right)
  | C_syn.Take (at, value) -> Take (at, of_syn value)
  | C_syn.Drop (at, value) -> Drop (at, of_syn value)
  | C_syn.Vcat (left, right) -> Vcat (of_syn left, of_syn right)
  | C_syn.At (at, value) -> At (at, of_syn value)
  | C_syn.Uncons value -> Uncons (of_syn value)
  | C_syn.Vfold (vector, seed, item) ->
      Vfold (of_syn vector, of_syn seed,
        fold (bind_of_syn item.item) (bind_of_syn item.state)
          (of_syn item.body))
  | C_syn.Step (cap, value) -> Step (of_syn cap, of_syn value)
  | C_syn.Close value -> Close (of_syn value)

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let push depth values rest =
  List.fold_left (fun out value -> (depth, value) :: out) rest values

let stat term =
  let rec walk nodes high = function
    | [] -> Ok ()
    | (depth, term) :: rest ->
        if depth > C_rule.local.tm_depth then
          Error (Depth (C_rule.local.tm_depth, depth))
        else if nodes = C_rule.local.tm_nodes then
          Error (Nodes (C_rule.local.tm_nodes, nodes + 1))
        else
          let next = depth + 1 in
          let rest =
            match term with
            | KUnit | KBool _ | KInt _ | KBytes _ | Var _ -> rest
            | KVec (_, values) | KSeq (_, _, values) -> push next values rest
            | Let (_, value, body)
            | Pair (value, body)
            | Add (value, body)
            | Sub (value, body)
            | Mul (value, body)
            | Div (value, body)
            | Mod (value, body)
            | Eq (_, value, body)
            | Cmp (_, value, body)
            | Cat (value, body)
            | Vcat (value, body)
            | Step (value, body) -> (next, value) :: (next, body) :: rest
            | If (guard, yes, no) ->
                (next, guard) :: (next, yes) :: (next, no) :: rest
            | Unpair (value, _, _, body) ->
                (next, value) :: (next, body) :: rest
            | Fst value | Snd value | Inl (value, _) | Inr (_, value)
            | Act (_, value) | Neg value | Abs value | Fit (_, value)
            | Wide value | Length value | Take (_, value)
            | Drop (_, value) | At (_, value)
            | Uncons value | Close value | Dmake (_, _, value) ->
                (next, value) :: rest
            | Case (value, _, yes, _, no) ->
                (next, value) :: (next, yes) :: (next, no) :: rest
            | Vfold (vector, seed, item) ->
                (next, vector) :: (next, seed) :: (next, item.fbody) :: rest
            | Dcase (_, value, arms) ->
                let bodies = List.map (fun item -> item.abody) arms in
                (next, value) :: push next bodies rest
            | Rmake (_, items) ->
                push next (List.map (fun item -> item.ivalue) items) rest
            | Rsplit (_, value, _, body) ->
                (next, value) :: (next, body) :: rest
            | Quant (_, source, _, body)
            | Weave (_, _, source, _, body)
            | Orbit (_, source, _, body)
            | Wake (_, source, _, body) ->
                (next, source) :: (next, body) :: rest
            | Orbit_to (_, count, source, _, body) ->
                (next, count) :: (next, source) :: (next, body) :: rest
            | Rift (_, _, _, source) -> (next, source) :: rest
            | Braid (_, _, left, right, _, _, body) ->
                (next, left) :: (next, right) :: (next, body) :: rest
            | Loom (_, _, _, body) -> (next, body) :: rest
          in
          walk (nodes + 1) (max high depth) rest
  in
  walk 0 0 [0, term]

let typ env value =
  Result.map_error (fun error -> Decl error) (C_decl.typ_in env value)

let bind_in env item =
  let* typ = typ env item.typ in
  Ok (C_syn.bind item.name item.mul typ)

let index env value =
  Result.map_error (fun error -> Idx error) (C_idx.eval env value)

let node value trace = Ok (value, value :: trace)

let rec terms env trace out = function
  | [] -> Ok (List.rev out, trace)
  | item :: rest ->
      let* item, trace = term env trace item in
      terms env trace (item :: out) rest

and arms env trace out = function
  | [] -> Ok (List.rev out, trace)
  | item :: rest ->
      let* bind = bind_in env item.abind in
      let* body, trace = term env trace item.abody in
      arms env trace (C_data.arm item.aname bind body :: out) rest

and items env trace out = function
  | [] -> Ok (List.rev out, trace)
  | item :: rest ->
      let* value, trace = term env trace item.ivalue in
      items env trace (C_rec.item item.iname value :: out) rest

and picks env out = function
  | [] -> Ok (List.rev out)
  | item :: rest ->
      let* bind = bind_in env item.pbind in
      picks env (C_rec.pick item.pname bind :: out) rest

and term env trace = function
  | KUnit -> node C_syn.KUnit trace
  | KBool value -> node (C_syn.KBool value) trace
  | KInt value -> node (C_syn.KInt value) trace
  | KBytes value -> node (C_syn.KBytes value) trace
  | KVec (raw, values) ->
      let* elem = typ env raw in
      let* values, trace = terms env trace [] values in
      node (C_syn.KVec (elem, values)) trace
  | KSeq (raw_cap, raw, values) ->
      let* cap = index env raw_cap in
      let* elem = typ env raw in
      let* values, trace = terms env trace [] values in
      node (C_syn.KSeq (cap, elem, values)) trace
  | Var name -> node (C_syn.Var name) trace
  | Let (item, value, body) ->
      let* item = bind_in env item in
      let* value, trace = term env trace value in
      let* body, trace = term env trace body in
      node (C_syn.Let (item, value, body)) trace
  | If (guard, yes, no) ->
      let* guard, trace = term env trace guard in
      let* yes, trace = term env trace yes in
      let* no, trace = term env trace no in
      node (C_syn.If (guard, yes, no)) trace
  | Pair (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Pair (left, right)) trace
  | Unpair (value, left, right, body) ->
      let* value, trace = term env trace value in
      let* left = bind_in env left in
      let* right = bind_in env right in
      let* body, trace = term env trace body in
      node (C_syn.Unpair (value, left, right, body)) trace
  | Fst value ->
      let* value, trace = term env trace value in
      node (C_syn.Fst value) trace
  | Snd value ->
      let* value, trace = term env trace value in
      node (C_syn.Snd value) trace
  | Inl (value, raw) ->
      let* value, trace = term env trace value in
      let* right = typ env raw in
      node (C_syn.Inl (value, right)) trace
  | Inr (raw, value) ->
      let* left = typ env raw in
      let* value, trace = term env trace value in
      node (C_syn.Inr (left, value)) trace
  | Case (value, left, yes, right, no) ->
      let* value, trace = term env trace value in
      let* left = bind_in env left in
      let* yes, trace = term env trace yes in
      let* right = bind_in env right in
      let* no, trace = term env trace no in
      node (C_syn.Case (value, left, yes, right, no)) trace
  | Act (atom, value) ->
      let* value, trace = term env trace value in
      node (C_syn.Act (atom, value)) trace
  | Add (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Add (left, right)) trace
  | Sub (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Sub (left, right)) trace
  | Mul (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Mul (left, right)) trace
  | Div (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Div (left, right)) trace
  | Mod (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Mod (left, right)) trace
  | Neg value ->
      let* value, trace = term env trace value in
      node (C_syn.Neg value) trace
  | Abs value ->
      let* value, trace = term env trace value in
      node (C_syn.Abs value) trace
  | Fit (raw, value) ->
      let* typ = typ env raw in
      let* value, trace = term env trace value in
      node (C_syn.Fit (typ, value)) trace
  | Wide value ->
      let* value, trace = term env trace value in
      node (C_syn.Wide value) trace
  | Length value ->
      let* value, trace = term env trace value in
      node (C_syn.Length value) trace
  | Eq (raw, left, right) ->
      let* typ = typ env raw in
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Eq (typ, left, right)) trace
  | Cmp (rel, left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.cmp rel left right) trace
  | Cat (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Cat (left, right)) trace
  | Take (at, value) ->
      let* value, trace = term env trace value in
      node (C_syn.Take (at, value)) trace
  | Drop (at, value) ->
      let* value, trace = term env trace value in
      node (C_syn.Drop (at, value)) trace
  | Vcat (left, right) ->
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      node (C_syn.Vcat (left, right)) trace
  | At (at, value) ->
      let* value, trace = term env trace value in
      node (C_syn.At (at, value)) trace
  | Uncons value ->
      let* value, trace = term env trace value in
      node (C_syn.Uncons value) trace
  | Vfold (vector, seed, item) ->
      let* vector, trace = term env trace vector in
      let* seed, trace = term env trace seed in
      let* element = bind_in env item.fitem in
      let* state = bind_in env item.fstate in
      let* body, trace = term env trace item.fbody in
      node (C_syn.Vfold (vector, seed, C_syn.fold element state body)) trace
  | Step (cap, value) ->
      let* cap, trace = term env trace cap in
      let* value, trace = term env trace value in
      node (C_syn.Step (cap, value)) trace
  | Close value ->
      let* value, trace = term env trace value in
      node (C_syn.Close value) trace
  | Dmake (decl, name, value) ->
      let* value, trace = term env trace value in
      let* value = Result.map_error (fun error -> Data error)
        (C_data.make decl name value) in
      node value trace
  | Dcase (decl, value, values) ->
      let* value, trace = term env trace value in
      let* values, trace = arms env trace [] values in
      let* value = Result.map_error (fun error -> Data error)
        (C_data.case decl value values) in
      node value trace
  | Rmake (decl, values) ->
      let* values, trace = items env trace [] values in
      let* value = Result.map_error (fun error -> Rec error)
        (C_rec.make decl values) in
      node value trace
  | Rsplit (decl, value, values, body) ->
      let* value, trace = term env trace value in
      let* values = picks env [] values in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Rec error)
        (C_rec.split decl value values body) in
      node value trace
  | Quant (mode, source, item, body) ->
      let* source, trace = term env trace source in
      let* item = bind_in env item in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Quant_error error)
        (C_quant.make mode source item body) in
      node value trace
  | Weave (raw_count, raw_out, source, item, body) ->
      let* count = index env raw_count in
      let* out = typ env raw_out in
      let* source, trace = term env trace source in
      let* item = bind_in env item in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Weave_error error)
        (C_weave.make count out source item body) in
      node value trace
  | Braid (raw_count, raw_out, left, right, first, second, body) ->
      let* count = index env raw_count in
      let* out = typ env raw_out in
      let* left, trace = term env trace left in
      let* right, trace = term env trace right in
      let* first = bind_in env first in
      let* second = bind_in env second in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Braid_error error)
        (C_braid.make count out left right first second body) in
      node value trace
  | Loom (raw_count, raw_out, item, body) ->
      let* count = index env raw_count in
      let* out = typ env raw_out in
      let* item = bind_in env item in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Loom_error error)
        (C_loom.make count out item body) in
      node value trace
  | Orbit (raw_count, seed, item, body) ->
      let* count = index env raw_count in
      let* seed, trace = term env trace seed in
      let* item = bind_in env item in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Orbit_error error)
        (C_orbit.make count seed item body) in
      node value trace
  | Orbit_to (raw_count, turns, seed, item, body) ->
      let* count = index env raw_count in
      let* turns, trace = term env trace turns in
      let* seed, trace = term env trace seed in
      let* item = bind_in env item in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Orbit_error error)
        (C_orbit.upto count turns seed item body) in
      node value trace
  | Wake (raw_count, seed, item, body) ->
      let* count = index env raw_count in
      let* seed, trace = term env trace seed in
      let* item = bind_in env item in
      let* body, trace = term env trace body in
      let* value = Result.map_error (fun error -> Wake_error error)
        (C_wake.make count seed item body) in
      node value trace
  | Rift (raw_cut, raw_rest, raw_elem, source) ->
      let* cut = index env raw_cut in
      let* rest = index env raw_rest in
      let* elem = typ env raw_elem in
      let* source, trace = term env trace source in
      let* value = Result.map_error (fun error -> Rift_error error)
        (C_rift.make cut rest elem source) in
      node value trace

let elab_trace env value =
  let* () = stat value in
  let* term, trace = term env [] value in
  Ok (term, List.rev trace)

let elab env value = Result.map fst (elab_trace env value)

let text = function
  | Idx error -> C_idx.text error
  | Decl error -> C_decl.text error
  | Data error -> C_data.text error
  | Rec error -> C_rec.text error
  | Quant_error error -> C_quant.text error
  | Weave_error error -> C_weave.text error
  | Braid_error error -> C_braid.text error
  | Loom_error error -> C_loom.text error
  | Orbit_error error -> C_orbit.text error
  | Wake_error error -> C_wake.text error
  | Rift_error error -> C_rift.text error
  | Fresh -> "private name space exhausted"
  | Depth (limit, actual) ->
      "raw term depth limit = " ^ string_of_int limit ^ " actual = "
      ^ string_of_int actual
  | Nodes (limit, actual) ->
      "raw term nodes limit = " ^ string_of_int limit ^ " actual = "
      ^ string_of_int actual