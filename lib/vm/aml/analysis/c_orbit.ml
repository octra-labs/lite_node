(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Need of C_type.t * C_type.t
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

let body_check item term =
  let* state_typ = Result.map_error (fun error -> Low error)
    (C_low.typ item.C_syn.typ) in
  let* prog = Result.map_error (fun error -> Low error)
    (C_low.prog [item] term) in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  if C_type.equal info.typ state_typ then Ok state_typ
  else Error (Need (state_typ, info.typ))

let fit count seed body =
  let* seed_stat = Result.map_error fin (C_fin.stat seed) in
  let* body_stat = Result.map_error fin (C_fin.stat body) in
  let copies = C_nat.to_z count in
  let nodes =
    if C_nat.equal count C_nat.zero then Z.of_int (seed_stat.nodes + 2)
    else Z.add (Z.of_int seed_stat.nodes)
      (Z.mul copies (Z.of_int (body_stat.nodes + 1)))
  in
  let depth =
    if C_nat.equal count C_nat.zero then seed_stat.depth + 1
    else C_nat.to_int count + max seed_stat.depth body_stat.depth
  in
  Result.map_error fin (C_fin.fit nodes depth)

let seed_as item seed =
  let bind = C_syn.bind item.C_syn.name C_type.One item.typ in
  C_syn.Let (bind, seed, C_syn.Var bind.name)

let term count seed item body =
  if C_nat.equal count C_nat.zero then seed_as item seed
  else
    let rec walk left out =
      if left = 0 then out
      else walk (left - 1) (C_syn.Let (item, out, body))
    in
    walk (C_nat.to_int count) seed

let make count seed item body =
  if not (C_nat.valid count) then Error (Low (C_low.Nat (C_nat.to_z count)))
  else
    let* _ = body_check item body in
    let* () = fit count seed body in
    Ok (term count seed item body)

let fit_upto count turns seed body guard =
  let* turns = Result.map_error fin (C_fin.stat turns) in
  let* seed = Result.map_error fin (C_fin.stat seed) in
  let* body = Result.map_error fin (C_fin.stat body) in
  let* guard = Result.map_error fin (C_fin.stat guard) in
  let step_nodes = Z.of_int (guard.nodes + body.nodes + 3) in
  let loop_nodes =
    if C_nat.equal count C_nat.zero then Z.of_int (seed.nodes + 2)
    else Z.add (Z.of_int seed.nodes)
      (Z.mul (C_nat.to_z count) step_nodes)
  in
  let nodes = Z.add Z.one (Z.add (Z.of_int turns.nodes) loop_nodes) in
  let branch_depth = 1 + max guard.depth body.depth in
  let loop_depth =
    if C_nat.equal count C_nat.zero then seed.depth + 1
    else C_nat.to_int count + max seed.depth branch_depth
  in
  Result.map_error fin (C_fin.fit nodes (1 + max turns.depth loop_depth))

let upto count turns seed item body =
  if not (C_nat.valid count) then Error (Low (C_low.Nat (C_nat.to_z count)))
  else
    let* _ = body_check item body in
    match C_fin.pickn 1 [] [item] [turns; seed; body] with
    | Some [turn_name] ->
        let turn_bind = C_syn.bind turn_name C_type.Many C_syn.TInt in
        let guard index = C_syn.cmp C_syn.Lt
          (C_syn.KInt (Z.of_int index)) (C_syn.Var turn_name) in
        let* () = fit_upto count turns seed body (guard 0) in
        let rec walk index left out =
          if left = 0 then out
          else
            let next = C_syn.If (guard index, body, C_syn.Var item.C_syn.name) in
            walk (index + 1) (left - 1) (C_syn.Let (item, out, next))
        in
        let loop =
          if C_nat.equal count C_nat.zero then seed_as item seed
          else walk 0 (C_nat.to_int count) seed
        in
        let term = C_syn.Let (turn_bind, turns,
          loop) in
        begin
          match C_fin.stat term with
          | Ok _ -> Ok term
          | Error error -> Error (fin error)
        end
    | _ -> Error Fresh

let lower inputs count seed item body =
  let* term = make count seed item body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs count seed item body =
  let* prog = lower inputs count seed item body in
  let* info = Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term) in
  let* expected = Result.map_error (fun error -> Low error)
    (C_low.typ item.C_syn.typ) in
  if C_type.equal info.typ expected then Ok info
  else Error (Need (expected, info.typ))

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Need (expected, actual) ->
      "orbit type expected = " ^ C_type.text expected
      ^ " actual = " ^ C_type.text actual
  | Fresh -> "orbit private name space exhausted"
  | Nodes actual ->
      "orbit node limit = " ^ string_of_int C_check.max_nodes
      ^ " actual = " ^ Z.to_string actual
  | Depth actual ->
      "orbit depth limit = " ^ string_of_int C_check.max_depth
      ^ " actual = " ^ string_of_int actual