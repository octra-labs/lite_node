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


type effects = {
  prepare : Consensus_epoch_commit.prepare_effects;
  stage : Consensus_epoch_apply_guard.stage_batch_effects;
  post_root : Consensus_epoch_apply_guard.post_root_effects;
  commit : Consensus_epoch_commit.commit_effects;
  rollback :
    epoch_id:int ->
    start_txid:int64 ->
    tx_count:int ->
    Consensus_epoch_commit.rollback_effects;
  failure :
    rollback:(unit -> bool) ->
    Consensus_epoch_commit.failure_effects;
  log_boundary : Consensus_epoch_commit.boundary_log -> unit;
}

type input = {
  epoch_id : int;
  current_epoch : int;
  consensus_mode : bool;
  layera_diag : bool;
  replay_trace : bool;
  pre_state_hash : string;
  pre_consensus_root : string;
  expected_root : string option;
  parent_commit : string;
  proposer_addr : string;
  proposer_info : Octra_core.Epochlog.proposer_info;
  proposer_source : string;
  round : int;
  validators : int;
  validators_sha : string;
  ordered_txs_count : int;
  confirmed_txs : Octra_core.Transaction.t list;
  confirmed_count : int;
  confirmed_fees : Z.t;
  plan : Octra_core.Epoch_exec.reward_plan;
  prev_supply : Z.t;
  emission_remaining : Z.t;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
  epoch_receipts_json : string list;
  account_addrs : string list;
  short : string -> string;
  find_account : string -> Consensus_epoch_apply_guard.account_view option;
  progress : Consensus_epoch_commit.commit_progress;
}

type result = {
  post_state_hash : string;
  post_consensus_root : string;
}

type node_effects = {
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  chaindata : Octra_core.Store_chaindata.t;
  finality_state : Consensus_finality_state.t;
  irmin_last_epoch : unit -> int;
  exit : unit -> unit;
}

val node_effects :
  node_effects ->
  effects

val run :
  effects ->
  input ->
  result Lwt.t