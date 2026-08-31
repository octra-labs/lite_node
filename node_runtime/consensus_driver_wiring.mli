(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type gates = {
  consensus_mode : unit -> bool;
  voting : unit -> bool;
  state_attested : unit -> bool;
  p2p_upgrade_ready : unit -> bool;
  catchup_active : unit -> bool;
  catchup_gap_active : unit -> bool;
  pending_finalized : unit -> bool;
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
  pending_finalized : unit -> bool;
  runtime_state : Consensus_runtime_state.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
}

type node_gates_runtime = {
  consensus_mode : bool;
  voting : bool;
  consensus_config_hash : string ref;
  p2p_config : P2p_config.t;
  current_epoch : unit -> int;
  log_blocked : string -> int -> unit;
  catchup_active : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  pending_finalized : unit -> bool;
  runtime_state : Consensus_runtime_state.t;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
}

type epoch_normalizer_runtime = {
  current_epoch : int ref;
  committed_head_epoch : unit -> int;
  warn :
    source:string ->
    current:int ->
    committed_head:int ->
    expected_next:int ->
    unit;
}

type node_standard_adapter_runtime = {
  getenv : string -> string option;
  get_meta : string -> string option;
  wallet_addr : string;
  wallet_pub : string;
  find_account : string -> Octra_core.Ledger.account option;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  read_prev_ledger_root : unit -> string option Lwt.t;
  next_txid : unit -> int64;
  proposal_state : Consensus_proposal_state.t;
  catchup_active : bool ref;
  staging_epoch_capacity : Z.t;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  validator_pubkeys_for_epoch :
    wallet_addr:string ->
    wallet_pub:string ->
    epoch:int ->
    (string * string) list;
}

type standard_adapters = {
  layera_diag_live : unit -> bool;
  layera_env : unit -> string option;
  layera_fallback_addr : string;
  layera_meta : string -> string;
  public_key_for_tx : Octra_core.Transaction.t -> string option;
  verify_address_pubkey : addr:string -> pubkey:string -> bool;
  verify_tx_signature : Octra_core.Transaction.t -> pubkey:string -> bool;
  validator_pubkeys_fallback : int64 -> (string * string) list;
  prev_eic_root : unit -> string;
  next_txid : unit -> int64;
  read_prev_ledger_root : unit -> string option Lwt.t;
  staging_txs : unit -> Octra_core.Transaction.t list;
  staging_epoch_txs : unit -> Octra_core.Transaction.t list;
  staging_total : unit -> int;
  proposer : unit -> string;
  head_txid_hi : unit -> int64 option;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  current_tx_hashes : unit -> string list;
  mark_unsynced : unit -> unit;
  write_pending : Octra_core.Wal.pending_commit -> unit;
  set_catchup_active : bool -> unit;
}

type proposal_preview_runtime = {
  chain_id : string;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  warn : string -> unit;
  run_preview :
    Consensus_proposal.build_preview_request ->
    reward:Consensus_reward_attribution.t ->
    env:Octra_core.Epoch_exec.env ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
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
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
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
  load_parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
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
  apply_finalized :
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
  driver_read_deps : Consensus_driver_read.deps;
  scheduled_validator_set_config :
    Octra_consensus.C_driver.scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
}

type config_with_standard_input = {
  standard : standard_adapters;
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  validator_set : Octra_consensus.C_types.validator_set;
  gates : gates;
  proposal_limits : Consensus_proposal.limits;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  sleep : float -> unit Lwt.t;
  quarantine_mismatch_threshold : int;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_bundles : Consensus_bundle_cache.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  root_to_raw32 : string -> string;
  current_epoch : unit -> int;
  current_round : unit -> int;
  committed_head_epoch : unit -> int;
  load_parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
  finality : Consensus_finality_state.callbacks;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  now : unit -> float;
  observer_mode : bool;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  apply_finalized :
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit Lwt.t;
  replay_stashed_while_safe : source:string -> unit Lwt.t;
  driver_read_deps : Consensus_driver_read.deps;
  scheduled_validator_set_config :
    Octra_consensus.C_driver.scheduled_validator_set_config option;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
}

type node_driver_config_runtime = {
  standard : node_standard_adapter_runtime;
  chain_id : string;
  my_addr : string;
  sign_fn : string -> string;
  validator_set : Octra_consensus.C_types.validator_set;
  gates : gates;
  proposal_limits : Consensus_proposal.limits;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  sleep : float -> unit Lwt.t;
  quarantine_mismatch_threshold : int;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_bundles : Consensus_bundle_cache.t;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  driver_ref : Octra_consensus.C_driver.t option ref;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  root_to_raw32 : string -> string;
  current_epoch : unit -> int;
  current_round : unit -> int;
  committed_head_epoch : unit -> int;
  load_parent_commit :
    epoch_id:int64 ->
    (Octra_consensus.C_types.parent_commit option, string) result;
  verify_parent_commit :
    epoch_id:int64 ->
    Octra_consensus.C_types.parent_commit option ->
    (unit, string) result;
  finality : Consensus_finality_state.callbacks;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  now : unit -> float;
  observer_mode : bool;
  queue_catchup_target : target_epoch:int64 -> reason:string -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  apply_finalized :
    validator_set:Octra_consensus.C_types.validator_set ->
    Octra_consensus.C_types.finalize ->
    unit Lwt.t;
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

val p2p_upgrade_ready_of_runtime :
  node_gates_runtime ->
  unit ->
  bool

val node_gates_of_runtime :
  node_gates_runtime ->
  gates

val normalize_next_epoch_for_head :
  epoch_normalizer_runtime ->
  source:string ->
  unit

val node_standard_adapters :
  node_standard_adapter_runtime ->
  standard_adapters

val preview_with_optional_catch :
  catch_exn:bool ->
  warn:(string -> unit) ->
  (unit -> ('a, string) result Lwt.t) ->
  ('a, string) result Lwt.t

val node_proposal_preview :
  proposal_preview_runtime ->
  ?catch_exn:bool ->
  Consensus_proposal.build_preview_request ->
  (Octra_core.Epoch_exec.exec_result, string) result Lwt.t

val config :
  deps ->
  Octra_consensus.C_driver.config

val config_with_standard :
  config_with_standard_input ->
  Octra_consensus.C_driver.config

val node_driver_config :
  node_driver_config_runtime ->
  Octra_consensus.C_driver.config