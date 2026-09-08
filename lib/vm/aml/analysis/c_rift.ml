(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type step = {
  head : C_syn.name;
  tail : C_syn.name;
}

type path = {
  source : C_syn.name;
  steps : step list;
}

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Need of C_type.t * C_type.t
  | Path of int * int
  | Fresh
  | Nodes of Z.t
  | Depth of int

type spec = {
  elem : C_type.t;
  total : C_nat.t;
}

let step head tail = { head; tail }
let path source steps = { source; steps }

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let fin = function
  | C_fin.Nodes actual -> Nodes actual
  | C_fin.Depth actual -> Depth actual

let mul typ =
  match C_type.kind typ with
  | C_type.Data -> C_type.Many
  | C_type.Res -> C_type.One

let fit cut source =
  let* stat = Result.map_error fin (C_fin.stat source) in
  let cut_z = C_nat.to_z cut in
  let nodes = Z.add (Z.of_int (stat.nodes + 4)) (Z.mul (Z.of_int 4) cut_z) in
  let body_depth =
    if C_nat.equal cut C_nat.zero then 1 else C_nat.to_int cut + 2
  in
  let depth = 1 + max stat.depth body_depth in
  Result.map_error fin (C_fin.fit nodes depth)

let prep cut rest elem source =
  let* elem = Result.map_error (fun error -> Low error) (C_low.typ elem) in
  let* total =
    match C_type.add_len cut rest with
    | Some total -> Ok total
    | None -> Error (Low (C_low.Nat (Z.add (C_nat.to_z cut) (C_nat.to_z rest))))
  in
  let* () = fit cut source in
  Ok { elem; total }

let names path =
  let add out step = step.tail :: step.head :: out in
  path.source :: List.rev (List.fold_left add [] path.steps)

let vec elem held =
  C_syn.KVec (elem, List.rev_map (fun name -> C_syn.Var name) held)

let rec body steps left elem elem_core held source =
  match steps with
  | [] -> Ok (C_syn.Pair (vec elem held, C_syn.Var source))
  | item :: more ->
    let* tail =
      match C_nat.sub left C_nat.one with
      | Some tail -> Ok tail
      | None -> Error (Low (C_low.Nat (C_nat.to_z left)))
    in
    let tail_core = C_type.Vec (tail, elem_core) in
    let head_bind = C_syn.bind item.head (mul elem_core) elem in
    let tail_bind = C_syn.bind item.tail (mul tail_core)
      (C_syn.TVec (C_nat.to_z tail, elem)) in
    let* next = body more tail elem elem_core (item.head :: held) item.tail in
    Ok (C_syn.Unpair (C_syn.Uncons (C_syn.Var source), head_bind, tail_bind,
      next))

let term path spec elem source =
  let source_core = C_type.Vec (spec.total, spec.elem) in
  let source_bind = C_syn.bind path.source (mul source_core)
    (C_syn.TVec (C_nat.to_z spec.total, elem)) in
  let* body = body path.steps spec.total elem spec.elem [] path.source in
  Ok (C_syn.Let (source_bind, source, body))

let build path cut rest elem source =
  let* spec = prep cut rest elem source in
  let expected = C_nat.to_int cut in
  let actual = List.length path.steps in
  if actual <> expected then Error (Path (expected, actual))
  else if C_fin.clearn [] [] [source] (names path) then
    term path spec elem source
  else Error Fresh

let rec steps out = function
  | head :: tail :: rest -> steps ({ head; tail } :: out) rest
  | [] -> Some (List.rev out)
  | _ -> None

let make cut rest elem source =
  let* spec = prep cut rest elem source in
  let count = 1 + (2 * C_nat.to_int cut) in
  match C_fin.pickn count [] [] [source] with
  | Some (source_name :: private_names) ->
    begin
      match steps [] private_names with
      | Some steps -> term { source = source_name; steps } spec elem source
      | None -> Error Fresh
    end
  | _ -> Error Fresh

let lower inputs cut rest elem source =
  let* term = make cut rest elem source in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs cut rest elem source =
  let* prog = lower inputs cut rest elem source in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  let* elem = Result.map_error (fun error -> Low error) (C_low.typ elem) in
  let expected = C_type.Pair (C_type.Vec (cut, elem), C_type.Vec (rest, elem)) in
  if C_type.equal info.typ expected then Ok info
  else Error (Need (expected, info.typ))

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Need (expected, actual) ->
    "rift type expected = " ^ C_type.text expected
    ^ " actual = " ^ C_type.text actual
  | Path (expected, actual) ->
    "rift path expected = " ^ string_of_int expected
    ^ " actual = " ^ string_of_int actual
  | Fresh -> "rift private name space exhausted"
  | Nodes actual ->
    "rift node limit = " ^ string_of_int C_check.max_nodes
    ^ " actual = " ^ Z.to_string actual
  | Depth actual ->
    "rift depth limit = " ^ string_of_int C_check.max_depth
    ^ " actual = " ^ string_of_int actual