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

let body_check out item term =
  let* item_typ = Result.map_error (fun error -> Low error) (C_low.typ item.C_syn.typ) in
  let* out_typ = Result.map_error (fun error -> Low error) (C_low.typ out) in
  let* () = data item_typ in
  let* () = data out_typ in
  let* prog = Result.map_error (fun error -> Low error) (C_low.prog [item] term) in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  if C_type.equal info.typ out_typ then Ok out_typ
  else Error (Need (out_typ, info.typ))

let fit count source body =
  let* source_stat = Result.map_error fin (C_fin.stat source) in
  let* body_stat = Result.map_error fin (C_fin.stat body) in
  let copies = C_nat.to_z count in
  let nodes = Z.add (Z.of_int (source_stat.nodes + 2))
    (Z.mul copies (Z.of_int (body_stat.nodes + 3))) in
  let depth =
    if C_nat.equal count C_nat.zero then 1 + source_stat.depth
    else max (1 + source_stat.depth) (max 4 (3 + body_stat.depth))
  in
  Result.map_error fin (C_fin.fit nodes depth)

let values count source item body =
  let rec walk left index out =
    if left = 0 then List.rev out
    else
      let value = C_syn.At (Z.of_int index, C_syn.Var source) in
      walk (left - 1) (index + 1) (C_syn.Let (item, value, body) :: out)
  in
  walk (C_nat.to_int count) 0 []

let term source_name count out source item body =
  let source_bind =
    C_syn.bind source_name C_type.Many
      (C_syn.TVec (C_nat.to_z count, item.C_syn.typ))
  in
  C_syn.Let (source_bind, source,
    C_syn.KVec (out, values count source_name item body))

let prep count out source item body_term =
  if not (C_nat.valid count) then Error (Low (C_low.Nat (C_nat.to_z count)))
  else
    let* out_typ = body_check out item body_term in
    let* () = fit count source body_term in
    Ok out_typ

let build source_name count out source item body_term =
  let* _ = prep count out source item body_term in
  if C_fin.clear [] [item] [source; body_term] source_name then
    Ok (term source_name count out source item body_term)
  else Error Fresh

let make count out source item body_term =
  let* _ = prep count out source item body_term in
  match C_fin.pick [] [item] [source; body_term] with
  | Some source_name -> Ok (term source_name count out source item body_term)
  | None -> Error Fresh

let lower inputs count out source item body =
  let* term = make count out source item body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs count out source item body =
  let* prog = lower inputs count out source item body in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  let* out_typ = Result.map_error (fun error -> Low error) (C_low.typ out) in
  let expected = C_type.Vec (count, out_typ) in
  if C_type.equal info.typ expected then Ok info
  else Error (Need (expected, info.typ))

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Need (expected, actual) ->
      "weave type expected = " ^ C_type.text expected
      ^ " actual = " ^ C_type.text actual
  | Data typ -> "weave data type expected actual = " ^ C_type.text typ
  | Fresh -> "weave private name space exhausted"
  | Nodes actual ->
      "weave node limit = " ^ string_of_int C_check.max_nodes
      ^ " actual = " ^ Z.to_string actual
  | Depth actual ->
      "weave depth limit = " ^ string_of_int C_check.max_depth
      ^ " actual = " ^ string_of_int actual