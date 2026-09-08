(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mode = Every | Any | Count | Sum

type error =
  | Low of C_low.error
  | Check of C_check.error
  | Fresh

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let same = C_syn.name_equal

let clear name source item body =
  not (same name item.C_syn.name)
  && not (C_syn.has name source)
  && not (C_syn.has name body)

let state_type = function
  | Every | Any -> C_syn.TBool
  | Count | Sum -> C_syn.TInt

let mark_type = function
  | Every | Any | Count -> C_syn.TBool
  | Sum -> C_syn.TInt

let seed = function
  | Every -> C_syn.KBool true
  | Any -> C_syn.KBool false
  | Count | Sum -> C_syn.KInt Z.zero

let expand state mark mode source item body =
  if same state mark || not (clear state source item body)
    || not (clear mark source item body) then Error Fresh
  else
    let state_bind = C_syn.bind state C_type.Many (state_type mode) in
    let mark_bind = C_syn.bind mark C_type.Many (mark_type mode) in
    let branch =
      match mode with
      | Every -> C_syn.If (C_syn.Var state, C_syn.Var mark, C_syn.KBool false)
      | Any -> C_syn.If (C_syn.Var state, C_syn.KBool true, C_syn.Var mark)
      | Count ->
        C_syn.If (C_syn.Var mark,
          C_syn.Add (C_syn.Var state, C_syn.KInt Z.one), C_syn.Var state)
      | Sum -> C_syn.Add (C_syn.Var state, C_syn.Var mark)
    in
    let term = C_syn.Let (mark_bind, body, branch) in
    Ok (C_syn.Vfold (source, seed mode, C_syn.fold item state_bind term))

let make mode source item body =
  let rec seek index =
    if index + 1 >= C_check.max_inputs then Error Fresh
    else
      match C_nat.of_int index, C_nat.of_int (index + 1) with
      | Some left, Some right ->
        let state = C_syn.dslot left in
        let mark = C_syn.dslot right in
        begin
          match expand state mark mode source item body with
          | Ok value -> Ok value
          | Error Fresh -> seek (index + 2)
          | Error _ as error -> error
        end
      | _ -> Error Fresh
  in
  seek 0

let lower inputs mode source item body =
  let* term = make mode source item body in
  Result.map_error (fun error -> Low error) (C_low.prog inputs term)

let check inputs mode source item body =
  let* prog = lower inputs mode source item body in
  Result.map_error (fun error -> Check error)
    (C_check.check_in prog.inputs prog.term)

let text = function
  | Low error -> C_low.text error
  | Check error -> C_check.text error
  | Fresh -> "vector predicate names exhausted"