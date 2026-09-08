(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bits = C_bin.bits

type out = {
  eval : C_eval.out;
  state : bits;
  tokens : bits list;
  root : string;
}

type error =
  | Program
  | State
  | Input of int
  | Token of int
  | Arity of int * int
  | Live
  | Static of C_check.error
  | Session of C_sess.error
  | Root
  | Seal

let ( let* ) value next =
  match value with
  | Ok value -> next value
  | Error error -> Error error

let values inputs =
  let rec walk index out = function
    | [] -> Ok (List.rev out)
    | _ when index = C_rule.local.inputs -> Error (Input index)
    | first :: rest ->
        begin
          match C_bin.dec_value first with
          | Some value -> walk (index + 1) (value :: out) rest
          | None -> Error (Input index)
        end
  in
  walk 0 [] inputs

let tokens inputs =
  let rec walk index out = function
    | [] -> Ok (List.rev out)
    | _ when index = C_check.max_nodes -> Error (Token index)
    | first :: rest ->
        begin
          match C_sbin.dec_tok first with
          | Some token -> walk (index + 1) (token :: out) rest
          | None -> Error (Token index)
        end
  in
  walk 0 [] inputs

let pair binds values =
  let rec walk out binds values =
    match binds, values with
    | [], [] -> Ok (List.rev out)
    | bind :: more_binds, value :: more_values ->
        walk ((bind, value) :: out) more_binds more_values
    | _ -> Error (Arity (List.length binds, List.length values))
  in
  walk [] binds values

let key_compare (left_kind, left_id) (right_kind, right_id) =
  let order = C_nat.compare left_kind right_kind in
  if order = 0 then C_nat.compare left_id right_id else order

let key_equal (left_kind, left_id) (right_kind, right_id) =
  C_nat.equal left_kind right_kind && C_nat.equal left_id right_id

let live state =
  let _, cells = C_sess.view state in
  List.filter_map
    (fun (cell : C_sess.cell) ->
      if cell.live then Some (cell.kind, cell.id) else None)
    cells

let token_keys values =
  List.map
    (fun (value : C_sess.token) -> value.kind, value.id)
    values
  |> List.sort key_compare

let live_ok state tokens =
  List.equal key_equal (live state) (token_keys tokens)

let domain state root =
  let prior, cells = C_sess.view state in
  match C_sess.scope ~chain:prior.chain ~prog:prior.prog ~root with
  | Error error -> Error (Session error)
  | Ok scope ->
      begin
        match C_sess.of_cells scope cells with
        | Error error -> Error (Session error)
        | Ok state -> Ok (scope, state)
      end

let token scope (value : C_sess.token) =
  match C_sess.token scope
      ~kind:(C_nat.to_z value.C_sess.kind)
      ~id:(C_nat.to_z value.C_sess.id)
      ~rev:(C_nat.to_z value.C_sess.rev) with
  | Ok value -> Ok value
  | Error error -> Error (Session error)

let seal state (values : C_sess.token list) root =
  let* scope, state = domain state root in
  let rec walk out = function
    | [] -> Ok (state, List.rev out)
    | first :: rest ->
        let* first = token scope first in
        walk (first :: out) rest
  in
  walk [] values

let encode_tokens values =
  let rec walk out = function
    | [] -> Ok (List.rev out)
    | first :: rest ->
        begin
          match C_sbin.enc_tok first with
          | Some bits -> walk (bits :: out) rest
          | None -> Error Seal
        end
  in
  walk [] values

let verify ~program ~prior ~state ~root =
  match C_sbin.dec_state state with
  | None -> false
  | Some sealed ->
      let scope, cells = C_sess.view sealed in
      if String.length root <> 32
        || not (String.equal scope.prog (C_root.image program))
        || not (String.equal scope.root root)
      then false
      else
        match C_sess.scope ~chain:scope.chain ~prog:scope.prog ~root:prior with
        | Error _ -> false
        | Ok prior_scope ->
            begin
              match C_sess.of_cells prior_scope cells with
              | Error _ -> false
              | Ok open_state ->
                  begin
                    match C_sbin.enc_state open_state with
                    | None -> false
                    | Some bits ->
                        begin
                          match C_root.root ~program ~prior ~state:bits with
                          | Some found -> String.equal found root
                          | None -> false
                        end
                  end
            end

let run ~program ~prior ~state ~inputs ~tokens:token_bits =
  let* program_value =
    match C_pbin.dec program with Some value -> Ok value | None -> Error Program
  in
  let* state_value =
    match C_sbin.dec_state state with Some value -> Ok value | None -> Error State
  in
  let* () =
    match C_root.root ~program ~prior ~state with
    | Some _ -> Ok ()
    | None -> Error Root
  in
  let* values = values inputs in
  let* token_values = tokens token_bits in
  let* () = if live_ok state_value token_values then Ok () else Error Live in
  let* inputs = pair program_value.C_low.inputs values in
  let* info =
    match C_check.check_in program_value.inputs program_value.term with
    | Ok value -> Ok value
    | Error error -> Error (Static error)
  in
  let* session =
    match C_sess.run ~fuel:info.res.steps state_value token_values inputs
        program_value.term with
    | Ok value -> Ok value
    | Error error -> Error (Session error)
  in
  let* open_bits =
    match C_sbin.enc_state session.state with
    | Some value -> Ok value
    | None -> Error Seal
  in
  let* root =
    match C_root.root ~program ~prior ~state:open_bits with
    | Some value -> Ok value
    | None -> Error Root
  in
  let* state_value, token_values = seal session.state session.tokens root in
  let* state =
    match C_sbin.enc_state state_value with
    | Some value -> Ok value
    | None -> Error Seal
  in
  let* tokens = encode_tokens token_values in
  if verify ~program ~prior ~state ~root then
    Ok { eval = session.eval; state; tokens; root }
  else Error Seal

let text = function
  | Program -> "turn program refused"
  | State -> "turn state refused"
  | Input index -> Printf.sprintf "turn input refused index = %d" index
  | Token index -> Printf.sprintf "turn token refused index = %d" index
  | Arity (binds, values) ->
      Printf.sprintf "turn input arity binds = %d values = %d" binds values
  | Live -> "turn live capability set differs"
  | Static error -> "turn checker refused reason = " ^ C_check.text error
  | Session error -> "turn session refused reason = " ^ C_sess.text error
  | Root -> "turn root refused"
  | Seal -> "turn seal refused"