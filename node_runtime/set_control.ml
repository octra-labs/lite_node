(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let eligible ~mode ~head ~bonded_epoch ~snapshot tx =
  if head < 0L then Set_post.Paused
  else if Octra_core.Tx_staging.duty_expired ~mode ~head:(Some head) tx then
    Set_post.Expired
  else match Octra_core.Validator_registry.ready_payload_of_message tx.Octra_core.Transaction.message with
    | Error _ -> Set_post.Expired
    | Ok ready when ready.head_epoch > head -> Set_post.Paused
    | Ok _ ->
      match snapshot with
      | Ok snapshot when Int64.of_int snapshot.Status_read_rpc.head_epoch = head ->
        begin match snapshot.candidate with
        | None -> Set_post.Expired
        | Some candidate when candidate.bonded_epoch <> bonded_epoch
                              || candidate.exit_epoch <> None -> Set_post.Expired
        | Some _ -> Set_post.Eligible
        end
      | _ -> Set_post.Eligible

let bonded ~control ~address ~pubkey snapshot =
  match snapshot with
  | Error _ as error -> error
  | Ok { Status_read_rpc.candidate = None; _ } -> Ok false
  | Ok { Status_read_rpc.candidate = Some candidate; chain_id; _ } ->
    if candidate.Octra_core.Validator_admission.exit_epoch <> None then Ok false
    else
      Octra_core.Validator_control.status control Octra_core.Validator_intent.{
        chain_id; address; pubkey; bonded_epoch = candidate.bonded_epoch;
      }
      |> Result.map Option.is_none