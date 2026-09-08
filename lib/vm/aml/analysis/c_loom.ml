(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Need of C_type.t * C_type.t
  | Data of C_type.t
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
  let* item_typ = Result.map_error (fun error -> Low error)
    (C_low.typ item.C_syn.typ) in
  let* out_typ = Result.map_error (fun error -> Low error) (C_low.typ out) in
  let* () =
    if C_type.equal item_typ C_type.Int then Ok ()
    else Error (Need (C_type.Int, item_typ))
  in
  let* () = data out_typ in
  let* prog = Result.map_error (fun error -> Low error)
    (C_low.prog [item] term) in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  if C_type.equal info.typ out_typ then Ok out_typ
  else Error (Need (out_typ, info.typ))

let fit count body =
  let* body_stat = Result.map_error fin (C_fin.stat body) in
  let copies = C_nat.to_z count in
  let nodes = Z.add Z.one
    (Z.mul copies (Z.of_int (body_stat.nodes + 2))) in
  let depth =
    if C_nat.equal count C_nat.zero then 0
    else max 2 (2 + body_stat.depth)
  in
  Result.map_error fin (C_fin.fit nodes depth)

let values count item body =
  let rec walk left index out =
    if left = 0 then List.rev out
    else
      let value = C_syn.Let (item, C_syn.KInt (Z.of_int index), body) in
      walk (left - 1) (index + 1) (value :: out)
  in
  walk (C_nat.to_int count) 0 []

let make count out item body =
  if not (C_nat.valid count) then Error (Low (C_low.Nat (C_nat.to_z count)))
  else
    let* _ = body_check out item body in
    let* () = fit count body in
    Ok (C_syn.KVec (out, values count item body))

let lower inputs count out item body =
  let* term = make count out item body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs count out item body =
  let* prog = lower inputs count out item body in
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
      "loom type expected = " ^ C_type.text expected
      ^ " actual = " ^ C_type.text actual
  | Data typ -> "loom data type expected actual = " ^ C_type.text typ
  | Nodes actual ->
      "loom node limit = " ^ string_of_int C_check.max_nodes
      ^ " actual = " ^ Z.to_string actual
  | Depth actual ->
      "loom depth limit = " ^ string_of_int C_check.max_depth
      ^ " actual = " ^ string_of_int actual