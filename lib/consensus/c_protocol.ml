(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let parent_legacy_version = 3
let parent_current_version = 4
let activation_env = "OCTRA_PROPOSAL_PROTOCOL_ACTIVATION_EPOCH"

let activation_epoch_of getenv =
  match getenv activation_env with
  | None
  | Some "" ->
    Ok 0L
  | Some raw ->
    begin
      match Int64.of_string_opt raw with
      | Some epoch when Int64.compare epoch 0L >= 0 -> Ok epoch
      | _ -> Error "invalid proposal protocol activation epoch"
    end

let activation_epoch_of_exn getenv =
  match activation_epoch_of getenv with
  | Ok epoch -> epoch
  | Error error -> invalid_arg error

let version_at ~activation_epoch epoch_id =
  if Int64.compare epoch_id activation_epoch < 0 then
    parent_legacy_version
  else
    parent_current_version

let version_for_epoch epoch_id =
  version_at
    ~activation_epoch:(activation_epoch_of_exn Sys.getenv_opt)
    epoch_id

let supported_version version =
  version = parent_legacy_version || version = parent_current_version

let consensus_id getenv =
  activation_epoch_of_exn getenv
  |> Int64.to_string