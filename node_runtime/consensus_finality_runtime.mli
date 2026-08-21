(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type replay_deps = {
  current_epoch : unit -> int;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
  read_local_root_raw : unit -> string Lwt.t;
}

type deps = {
  apply : Consensus_finalized_apply.node_deps;
  replay : replay_deps;
}

type node_deps = {
  check_finality : Octra_consensus.C_types.finalize -> unit;
  write_finality : Octra_consensus.C_types.finalize -> unit;
  persist_finality_certificate :
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit;
  persist_finality_bundle :
    Octra_consensus.C_types.finalize ->
    Consensus_finality_journal.bundle ->
    unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle_for_pid :
    string ->
    (string list * Octra_core.Transaction.t list * string list) option;
  header_has_empty_bundle : Octra_consensus.C_types.epoch_header -> bool;
  store_empty_bundle : Octra_consensus.C_types.epoch_header -> unit;
  driver : unit -> Octra_consensus.C_driver.t option;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  store_proposal_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  current_epoch : unit -> int;
  committed_head_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  commit_finality_journal : unit -> unit;
  remove_pending_finalized : epoch:int -> unit;
  set_state_attested : head:int -> root:string -> unit;
  apply_timeout_seconds : float;
  bundle_wait_expired : epoch_id:int64 -> unit;
  bundle_wait_recovered : epoch_id:int64 -> unit;
  require_sync : Sync_need.t -> unit;
  fatal_exit : unit -> unit;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
}

type node_runtime = {
  data_dir : string;
  bundles : Consensus_bundle_cache.node_runtime;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_state : Consensus_proposal_state.t;
  catchup_queue : Consensus_catchup_queue.t;
  consensus_finalized : bool ref;
  current_epoch : int ref;
  committed_head_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  apply_timeout_seconds : float;
  require_sync : Sync_need.t -> unit;
  fatal_exit : unit -> unit;
  catchup_active : bool ref;
  runtime_state : Consensus_runtime_state.t;
  finality : Consensus_finality_state.callbacks;
}

type t = {
  apply_finalized :
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit Lwt.t;
  drain_pending : unit -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

val create : deps -> t

val create_node : node_deps -> t

val node_deps_of_runtime : node_runtime -> node_deps

val create_node_runtime : node_runtime -> t