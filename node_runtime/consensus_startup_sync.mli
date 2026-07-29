(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply_replay : Consensus_replay.plan -> unit Lwt.t;
  apply_join :
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    reward:Consensus_reward_attribution.t ->
    epoch_ts:float ->
    unit Lwt.t;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

type apply_finalized =
  ?override_ordered_txs:Octra_core.Transaction.t list ->
  ?override_receipts_json:string list ->
  ?override_proposer_info:Octra_core.Epochlog.proposer_info ->
  ?override_reward:Consensus_reward_attribution.t ->
  ?override_epoch_ts:float ->
  now:float ->
  elapsed:float ->
  unit ->
  unit Lwt.t

type apply_callbacks = {
  replay : Consensus_replay.plan -> unit Lwt.t;
  join :
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    proposer_info:Octra_core.Epochlog.proposer_info option ->
    reward:Consensus_reward_attribution.t ->
    epoch_ts:float ->
    unit Lwt.t;
}

type node_deps = {
  env : string -> string option;
  expected_validator_set_hash : int64 -> (string, string) result;
  fetch_json : string -> Yojson.Safe.t Lwt.t;
  current_epoch : unit -> int;
  head : unit -> Octra_core.Head_manifest.t option;
  next_txid : unit -> int64;
  finality : Consensus_finality_state.callbacks;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  write_entry : Octra_consensus.Finality_log.entry -> unit;
  apply_finalized : apply_finalized;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  data_dir : string;
  consensus_role : string;
  chain_id : string;
  validator : string;
  validator_pubkey : string;
  priv_b64 : string;
  exit_error : unit -> unit;
  exit_success : unit -> unit;
}

val apply_callbacks :
  now:(unit -> float) ->
  apply_finalized ->
  apply_callbacks

val node_deps :
  node_deps ->
  deps

val replay_deps : deps -> Consensus_replay.one_shot_deps

val join_wiring : deps -> Consensus_join_rpc.node_runtime_wiring

val run_replay : deps -> unit Lwt.t

val run_join : deps -> unit Lwt.t

val run : deps -> unit Lwt.t

val run_node : node_deps -> unit Lwt.t