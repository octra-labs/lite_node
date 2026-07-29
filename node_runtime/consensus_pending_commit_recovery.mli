(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type decision =
  | Confirmed_elsewhere of {
      agreed : int;
      quorum : int;
    }
  | All_peers_missing of {
      responses : int;
    }
  | Inconclusive of {
      agreed : int;
      peers : int;
      responses : int;
      quorum : int;
    }

type recovered = {
  proposal : Octra_consensus.C_types.propose;
  vote : Octra_consensus.C_types.vote;
  tx_hashes : string list;
  txs : Octra_core.Transaction.t list;
  receipts_json : string list;
}

type restore_result =
  | Restored
  | Legacy
  | Invalid of string

val raw32_of_hex : string -> string option

val decide :
  validator_count:int ->
  peer_quorum:int ->
  promised_root_raw:string option ->
  Octra_consensus.C_driver.epoch_root_response_record list ->
  decision

val recover_record :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  Octra_core.Wal.pending_commit ->
  (recovered, string) result

type deps = {
  read_pending_commits : unit -> Octra_core.Wal.pending_commit list;
  head_epoch : unit -> int;
  query_epoch_root :
    epoch_id:int64 ->
    Octra_consensus.C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target : target_epoch:int64 -> reason:string -> unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
  restore_pending : Octra_core.Wal.pending_commit -> restore_result;
}

type 'driver driver_runtime = {
  read_pending_commits : unit -> Octra_core.Wal.pending_commit list;
  head : unit -> Octra_core.Head_manifest.t option;
  query_epoch_root :
    'driver ->
    epoch_id:int64 ->
    Octra_consensus.C_driver.epoch_root_response_record list Lwt.t;
  run_catchup_to_target :
    'driver ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  delete_pending_commit : epoch_id:int -> round:int -> unit;
  restore_pending :
    'driver ->
    Octra_core.Wal.pending_commit ->
    restore_result;
  validator_count : int;
  peer_quorum : int;
}

type node_driver_runtime = {
  data_dir : string;
  query_timeout : float;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  chain_id : string;
  validator_set : Octra_consensus.C_types.validator_set;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  validator_count : int;
  quorum : int;
}

val driver_runtime :
  read_pending_commits:(unit -> Octra_core.Wal.pending_commit list) ->
  head:(unit -> Octra_core.Head_manifest.t option) ->
  query_epoch_root:('driver -> epoch_id:int64 -> Octra_consensus.C_driver.epoch_root_response_record list Lwt.t) ->
  run_catchup_to_target:('driver -> target_epoch:int64 -> reason:string -> unit Lwt.t) ->
  delete_pending_commit:(epoch_id:int -> round:int -> unit) ->
  restore_pending:('driver -> Octra_core.Wal.pending_commit -> restore_result) ->
  validator_count:int ->
  quorum:int ->
  'driver driver_runtime

val node_driver_runtime :
  node_driver_runtime ->
  Octra_consensus.C_driver.t driver_runtime

val head_epoch_of_manifest :
  Octra_core.Head_manifest.t option ->
  int

val deps_of_driver_runtime :
  'driver driver_runtime ->
  'driver ->
  deps

val run_once :
  deps ->
  validator_count:int ->
  peer_quorum:int ->
  bool Lwt.t

val run_with_driver :
  'driver driver_runtime ->
  'driver ->
  bool Lwt.t