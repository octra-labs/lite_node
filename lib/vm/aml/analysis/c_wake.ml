(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Need of C_type.t * C_type.t
  | Data of C_type.t
  | Fresh
  | Nodes of Z.t
  | Depth of int

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let fin = function
  | C_fin.Nodes actual -> Nodes actual
  | C_fin.Depth actual -> Depth actual

let data typ =
  if C_type.kind typ = C_type.Data then Ok () else Error (Data typ)

let body_check item term =
  let* state_typ = Result.map_error (fun error -> Low error)
    (C_low.typ item.C_syn.typ) in
  let* () = data state_typ in
  let* prog = Result.map_error (fun error -> Low error)
    (C_low.prog [item] term) in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  if C_type.equal info.typ state_typ then Ok state_typ
  else Error (Need (state_typ, info.typ))

let depth count seed body =
  if C_nat.equal count C_nat.zero then seed + 1
  else
    let rounds = 2 * C_nat.to_int count in
    max (seed + 2) (rounds + max body 1)

let fit count seed body =
  let* seed_stat = Result.map_error fin (C_fin.stat seed) in
  let* body_stat = Result.map_error fin (C_fin.stat body) in
  let copies = C_nat.to_z count in
  let nodes =
    if C_nat.equal count C_nat.zero then Z.of_int (seed_stat.nodes + 2)
    else Z.add (Z.of_int seed_stat.nodes)
      (Z.mul copies (Z.of_int (body_stat.nodes + 6)))
  in
  Result.map_error fin (C_fin.fit nodes
    (depth count seed_stat.depth body_stat.depth))

let rec chain left keep typ seed item body =
  if left = 0 then C_syn.KVec (typ, [])
  else
    let bind = C_syn.bind keep C_type.Many typ in
    let next = C_syn.Let (item, seed, body) in
    let head = C_syn.KVec (typ, [C_syn.Var keep]) in
    let tail = chain (left - 1) keep typ (C_syn.Var keep) item body in
    C_syn.Let (bind, next, C_syn.Vcat (head, tail))

let term keep count seed item body =
  if C_nat.equal count C_nat.zero then
    C_syn.Let (C_syn.bind keep C_type.Zero item.C_syn.typ, seed,
      C_syn.KVec (item.typ, []))
  else chain (C_nat.to_int count) keep item.typ seed item body

let prep count seed item body =
  if not (C_nat.valid count) then Error (Low (C_low.Nat (C_nat.to_z count)))
  else
    let* _ = body_check item body in
    fit count seed body

let build keep count seed item body =
  let* () = prep count seed item body in
  if C_fin.clear [] [item] [seed; body] keep then
    Ok (term keep count seed item body)
  else Error Fresh

let make count seed item body =
  let* () = prep count seed item body in
  match C_fin.pick [] [item] [seed; body] with
  | Some keep -> Ok (term keep count seed item body)
  | None -> Error Fresh

let lower inputs count seed item body =
  let* term = make count seed item body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs count seed item body =
  let* prog = lower inputs count seed item body in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  let* state_typ = Result.map_error (fun error -> Low error)
    (C_low.typ item.C_syn.typ) in
  let expected = C_type.Vec (count, state_typ) in
  if C_type.equal info.typ expected then Ok info
  else Error (Need (expected, info.typ))

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Need (expected, actual) ->
      "wake type expected = " ^ C_type.text expected
      ^ " actual = " ^ C_type.text actual
  | Data typ -> "wake data type expected actual = " ^ C_type.text typ
  | Fresh -> "wake private name space exhausted"
  | Nodes actual ->
      "wake node limit = " ^ string_of_int C_check.max_nodes
      ^ " actual = " ^ Z.to_string actual
  | Depth actual ->
      "wake depth limit = " ^ string_of_int C_check.max_depth
      ^ " actual = " ^ string_of_int actual