(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type wallet = {
  address : string;
  pub : string;
  priv : string;
}

type p2p_refs = {
  consensus_config_hash : string ref;
  consensus_validator_set : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option ref;
  set_swarm : Octra_net.P2p_swarm.t -> unit;
}

type deps = {
  env : string -> string option;
  env_int : string -> int -> int;
  data_dir : string;
  chain_id : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  consensus_mode : bool;
  voting : bool;
  observer : bool;
  consensus_port : int;
  consensus_peers : string list;
  role_label : string;
  wallet : wallet;
  p2p_refs : p2p_refs;
  current_epoch : int ref;
  consensus_finalized : bool ref;
  catchup_active : bool ref;
  catchup_queue : Consensus_catchup_queue.t;
  runtime_state : Consensus_runtime_state.t;
  liveness_state : Consensus_liveness.state ref;
  driver_ref : Octra_consensus.C_driver.t option ref;
  on_driver : Octra_consensus.C_driver.t -> unit;
  proposal_state : Consensus_proposal_state.t;
  proposal_bundles : Consensus_bundle_cache.t;
  bundle_runtime : Consensus_bundle_cache.node_runtime;
  proposal_limits : Consensus_proposal.limits;
  current_round : unit -> int;
  finality : Consensus_finality_state.callbacks;
  catchup_queue_node : Consensus_catchup_shell.node_queue;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  get_meta : string -> string option;
  read_persistent_pending : unit -> string option Lwt.t;
  root_of_head_hash : string -> string;
  root_to_raw32 : string -> string;
  raw_to_hex : string -> string;
  read_prev_ledger_root : unit -> string option Lwt.t;
  find_account : string -> Octra_core.Ledger.account option;
  build_preverify : Consensus_preverify_role.build;
  validate_preverify : Consensus_preverify_role.validate;
  proposal_preview :
    ?catch_exn:bool ->
    Consensus_proposal.build_preview_request ->
    (Octra_core.Epoch_exec.exec_result, string) result Lwt.t;
  apply_catchup_record :
    Consensus_catchup_shell.validated_record ->
    unit Lwt.t;
  catchup_base_eic : unit -> string;
  next_txid : unit -> int64;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  committed_epoch_root_raw : int -> string option;
  committed_head_epoch : unit -> int;
  read_local_root_raw : unit -> string Lwt.t;
  read_local_ledger_root_raw : unit -> string Lwt.t;
  cached_root : unit -> Consensus_driver_read.cached_root;
  clear_state_attested : unit -> unit;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  validator_pubkeys_for_epoch :
    wallet_addr:string ->
    wallet_pub:string ->
    epoch:int ->
    (string * string) list;
  proposal_capacity : Z.t;
  quarantine_mismatch_threshold : int;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
  quarantine_poll_sec : float;
  liveness_stall_sec : float;
  state_readable : unit -> bool;
  sleep : float -> unit Lwt.t;
  now : unit -> float;
  exit_error : unit -> unit;
}

val committed_reads :
  readable:(unit -> bool) ->
  Consensus_driver_read.deps ->
  Consensus_driver_read.deps

val validator_state_height :
  committed_head_epoch:(unit -> int) ->
  int64

val enabled :
  int ->
  bool

val run : deps -> unit