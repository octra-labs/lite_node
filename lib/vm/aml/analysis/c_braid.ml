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

let body_check out first second term =
  let* first_typ = Result.map_error (fun error -> Low error)
    (C_low.typ first.C_syn.typ) in
  let* second_typ = Result.map_error (fun error -> Low error)
    (C_low.typ second.C_syn.typ) in
  let* out_typ = Result.map_error (fun error -> Low error) (C_low.typ out) in
  let* () = data first_typ in
  let* () = data second_typ in
  let* () = data out_typ in
  let* prog = Result.map_error (fun error -> Low error)
    (C_low.prog [first; second] term) in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  if C_type.equal info.typ out_typ then Ok out_typ
  else Error (Need (out_typ, info.typ))

let fit count left right body =
  let* left_stat = Result.map_error fin (C_fin.stat left) in
  let* right_stat = Result.map_error fin (C_fin.stat right) in
  let* body_stat = Result.map_error fin (C_fin.stat body) in
  let copies = C_nat.to_z count in
  let base = left_stat.nodes + right_stat.nodes + 3 in
  let nodes = Z.add (Z.of_int base)
    (Z.mul copies (Z.of_int (body_stat.nodes + 6))) in
  let depth =
    let source_depth = max (1 + left_stat.depth) (2 + right_stat.depth) in
    if C_nat.equal count C_nat.zero then max source_depth 2
    else max source_depth (max 6 (5 + body_stat.depth))
  in
  Result.map_error fin (C_fin.fit nodes depth)

let values count left_name right_name first second body =
  let rec walk left index out =
    if left = 0 then List.rev out
    else
      let first_value = C_syn.At (Z.of_int index, C_syn.Var left_name) in
      let second_value = C_syn.At (Z.of_int index, C_syn.Var right_name) in
      let value = C_syn.Let (first, first_value,
        C_syn.Let (second, second_value, body)) in
      walk (left - 1) (index + 1) (value :: out)
  in
  walk (C_nat.to_int count) 0 []

let term left_name right_name count out left right first second body =
  let left_bind = C_syn.bind left_name C_type.Many
    (C_syn.TVec (C_nat.to_z count, first.C_syn.typ)) in
  let right_bind = C_syn.bind right_name C_type.Many
    (C_syn.TVec (C_nat.to_z count, second.C_syn.typ)) in
  C_syn.Let (left_bind, left,
    C_syn.Let (right_bind, right,
      C_syn.KVec (out,
        values count left_name right_name first second body)))

let prep count out left right first second body =
  if not (C_nat.valid count) then Error (Low (C_low.Nat (C_nat.to_z count)))
  else
    let* out_typ = body_check out first second body in
    let* () = fit count left right body in
    Ok out_typ

let clear left_name right_name first second left right body =
  let binds = [first; second] in
  let terms = [left; right; body] in
  C_fin.clear [right_name] binds terms left_name
  && C_fin.clear [left_name] binds terms right_name

let build left_name right_name count out left right first second body =
  let* _ = prep count out left right first second body in
  if clear left_name right_name first second left right body then
    Ok (term left_name right_name count out left right first second body)
  else Error Fresh

let make count out left right first second body =
  let* _ = prep count out left right first second body in
  let binds = [first; second] in
  let terms = [left; right; body] in
  match C_fin.pick [] binds terms with
  | None -> Error Fresh
  | Some left_name ->
    begin
      match C_fin.pick [left_name] binds terms with
      | None -> Error Fresh
      | Some right_name ->
        Ok (term left_name right_name count out left right first second body)
    end

let lower inputs count out left right first second body =
  let* term = make count out left right first second body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs count out left right first second body =
  let* prog = lower inputs count out left right first second body in
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
      "braid type expected = " ^ C_type.text expected
      ^ " actual = " ^ C_type.text actual
  | Data typ -> "braid data type expected actual = " ^ C_type.text typ
  | Fresh -> "braid private name space exhausted"
  | Nodes actual ->
      "braid node limit = " ^ string_of_int C_check.max_nodes
      ^ " actual = " ^ Z.to_string actual
  | Depth actual ->
      "braid depth limit = " ^ string_of_int C_check.max_depth
      ^ " actual = " ^ string_of_int actual