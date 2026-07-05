(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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
  write_finality : Octra_consensus.C_types.finalize -> unit;
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
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  deactivate_gap : unit -> unit;
  set_consensus_finalized : bool -> unit;
  current_epoch : unit -> int;
  sleep : float -> unit Lwt.t;
  read_pre_finalize_root : unit -> string option;
  read_commit_root : unit -> string option Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  remove_pending_finalized : epoch:int -> unit;
  fatal_exit : unit -> unit;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  finality : Consensus_finality_state.callbacks;
}

type t = {
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  drain_pending : unit -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
}

val create : deps -> t

val create_node : node_deps -> t