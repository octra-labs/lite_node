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


module Transaction = Octra_core.Transaction

type gates = {
  consensus_mode : unit -> bool;
  voting : unit -> bool;
  state_attested : unit -> bool;
  p2p_upgrade_ready : unit -> bool;
  catchup_active : unit -> bool;
  catchup_gap_active : unit -> bool;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
  prev_root_streak : unit -> int;
  set_prev_root_streak : int -> unit;
  state_root_streak : unit -> int;
  set_state_root_streak : int -> unit;
}

type node_gates_input = {
  consensus_mode : bool;
  voting : bool;
  p2p_upgrade_ready : unit -> bool;
  catchup_active : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  runtime_state : Consensus_runtime_state.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
}

type deps = {
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  validator_set : Octra_consensus.C_types.validator_set;
  gates : gates;
  proposal_limits : Consensus_proposal.limits;
  layera_diag_live : unit -> bool;
  layera_env : unit -> string option;
  layera_fallback_addr : string;
  layera_meta : string -> string;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  sleep : float -> unit Lwt.t;
  quarantine_mismatch_threshold : int;
  staging_txs : unit -> Transaction.t list;
  staging_epoch_txs : unit -> Transaction.t list;
  staging_total : unit -> int;
  remove_rejected : string list -> unit;
  notify_staging_update : unit -> unit;
  run_preverify : Transaction.t list -> Octra_core.Preverify_worker.batch Lwt.t;
  proposal_bundles : Consensus_bundle_cache.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  driver_ref : Octra_consensus.C_driver.t option ref;
  public_key_for_tx : Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Transaction.t -> pubkey:string -> bool;
  validator_pubkeys_fallback : int64 -> (string * string) list;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  root_to_raw32 : string -> string;
  current_epoch : unit -> int;
  current_round : unit -> int;
  committed_head_epoch : unit -> int;
  finality : Consensus_finality_state.callbacks;
  read_prev_ledger_root : unit -> string option Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  proposer : unit -> string;
  head_txid_hi : unit -> int64 option;
  set_proposal : Transaction.t list -> string list -> unit;
  current_tx_hashes : unit -> string list;
  mark_unsynced : unit -> unit;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  now : unit -> float;
  observer_mode : bool;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  set_catchup_active : bool -> unit;
  apply_finalized : Octra_consensus.C_types.finalize -> unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
  driver_read_deps : Consensus_driver_read.deps;
  scheduled_validator_set_config :
    Octra_consensus.C_driver.scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
}

val can_vote :
  deps ->
  bool

val node_gates :
  node_gates_input ->
  gates

val config :
  deps ->
  Octra_consensus.C_driver.config