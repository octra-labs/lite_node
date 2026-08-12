(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let start rules =
  match Octra_core.Rule_graph.set_fold_activation rules with
  | None -> 0L
  | Some activation -> Int64.of_int activation.Octra_core.Rule_graph.activation_epoch

let resolve rules ~chain_id ~parent epoch =
  match Octra_core.Rule_graph.set_fold rules ~epoch with
  | Error fault -> Error (Octra_core.Rule_graph.fault_message fault)
  | Ok Octra_core.Rule_graph.Prior ->
    Ok Octra_core.Epoch_exec.{
      mode = Octra_core.Rule_graph.Prior;
      start = start rules;
      parent;
      members = [];
    }
  | Ok Octra_core.Rule_graph.Active ->
    begin
      match parent with
      | None -> Error "validator set fold parent commit missing"
      | Some commit ->
        begin
          match Octra_core.Set_fold.read_parent ~chain_id commit with
          | Error _ as error -> error
          | Ok (_, _, members) ->
            Ok Octra_core.Epoch_exec.{
              mode = Octra_core.Rule_graph.Active;
              start = start rules;
              parent;
              members;
            }
        end
    end

let bind rules ~chain_id ~parent ~epoch =
  resolve rules ~chain_id ~parent epoch
  |> Result.map (fun ctx target ->
    if target = epoch then Ok ctx
    else Error "validator set fold epoch mismatch")