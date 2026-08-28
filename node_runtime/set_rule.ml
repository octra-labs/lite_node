(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let start rules =
  match Octra_core.Rule_graph.set_fold_activation rules with
  | None -> 0L
  | Some activation -> Int64.of_int activation.Octra_core.Rule_graph.activation_epoch

type policy = {
  ready_mode : Octra_core.Rule_graph.mode;
  ready_ref_mode : Octra_core.Rule_graph.mode;
  live_mode : Octra_core.Rule_graph.mode;
  seat_mode : Octra_core.Rule_graph.mode;
  open_mode : Octra_core.Rule_graph.mode;
  account_mode : Octra_core.Rule_graph.mode;
  cap_mode : Octra_core.Set_fold.cap_mode;
}

let policy rules epoch =
  match
    Octra_core.Rule_graph.validator_ready rules ~epoch,
    Octra_core.Rule_graph.ready_ref rules ~epoch,
    Octra_core.Rule_graph.set_live rules ~epoch,
    Octra_core.Rule_graph.set_fold_cap rules ~epoch,
    Octra_core.Rule_graph.set_open rules ~epoch,
    Octra_core.Rule_graph.account_pack rules ~epoch
  with
  | Error fault, _, _, _, _, _
  | _, Error fault, _, _, _, _
  | _, _, Error fault, _, _, _
  | _, _, _, Error fault, _, _
  | _, _, _, _, Error fault, _
  | _, _, _, _, _, Error fault -> Error (Octra_core.Rule_graph.fault_message fault)
  | Ok ready_mode, Ok ready_ref_mode, Ok live_mode, Ok seat_mode, Ok open_mode,
    Ok account_mode ->
    let cap_mode =
      match seat_mode with
      | Octra_core.Rule_graph.Prior -> Octra_core.Set_fold.Reject
      | Octra_core.Rule_graph.Active -> Octra_core.Set_fold.Prune
    in
    Ok {
      ready_mode;
      ready_ref_mode;
      live_mode;
      seat_mode;
      open_mode;
      account_mode;
      cap_mode;
    }

let resolve rules ~chain_id ~parent epoch =
  match Octra_core.Rule_graph.set_fold rules ~epoch, policy rules epoch with
  | _, Error error -> Error error
  | Error fault, _ -> Error (Octra_core.Rule_graph.fault_message fault)
  | Ok Octra_core.Rule_graph.Prior, Ok policy ->
    Ok Octra_core.Epoch_exec.{
      mode = Octra_core.Rule_graph.Prior;
      ready_mode = policy.ready_mode;
      ready_ref_mode = policy.ready_ref_mode;
      live_mode = policy.live_mode;
      seat_mode = policy.seat_mode;
      open_mode = policy.open_mode;
      account_mode = policy.account_mode;
      cap_mode = policy.cap_mode;
      ready_config_hash = Octra_core.Rule_graph.ready_config_hash rules;
      start = start rules;
      parent;
      members = [];
    }
  | Ok Octra_core.Rule_graph.Active, Ok policy ->
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
              ready_mode = policy.ready_mode;
              ready_ref_mode = policy.ready_ref_mode;
              live_mode = policy.live_mode;
              seat_mode = policy.seat_mode;
              open_mode = policy.open_mode;
              account_mode = policy.account_mode;
              cap_mode = policy.cap_mode;
              ready_config_hash = Octra_core.Rule_graph.ready_config_hash rules;
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