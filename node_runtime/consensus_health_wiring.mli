(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  sleep : float -> unit Lwt.t;
  catchup_active : unit -> bool;
  set_catchup_active : bool -> unit;
  quarantine_active : unit -> bool;
  replay_stashed : source:string -> unit Lwt.t;
  probe_health : unit -> unit Lwt.t;
  reset_liveness : source:string -> unit Lwt.t;
}

type launch_deps = {
  start_driver : unit -> unit Lwt.t;
  startup_probe : unit -> unit Lwt.t;
  poll_loop : unit -> unit Lwt.t;
  pending_recovery : unit -> bool Lwt.t;
  hold_startup : unit -> unit;
  log_started : unit -> unit;
}

type state_refs = {
  catchup : bool ref;
  quarantine : bool ref;
}

type runtime_state_ops_deps = {
  state : Consensus_runtime_state.t;
  current_epoch : unit -> int;
  warn_quarantine : epoch:int -> reason:string -> unit;
  info_quarantine : epoch:int -> reason:string -> unit;
}

type node_runtime_state_ops_deps = {
  state : Consensus_runtime_state.t;
  current_epoch : unit -> int;
}

type runtime_state_ops = {
  clear_state_attested : unit -> unit;
  set_state_attested : head:int -> root:string -> unit;
  attested_head : int -> bool;
  mark_quarantine : string -> unit;
  clear_quarantine : string -> unit;
}

type 'driver driver_deps = {
  sleep : float -> unit Lwt.t;
  state : state_refs;
  hold_startup : unit -> unit;
  replay_stashed : source:string -> unit Lwt.t;
  start_driver : 'driver -> unit Lwt.t;
  probe_health : 'driver -> unit Lwt.t;
  reset_liveness : 'driver -> source:string -> unit Lwt.t;
  pending_recovery : 'driver -> bool Lwt.t;
  poll_interval : float;
  pending_delay : float;
  log_started : unit -> unit;
}

type driver_probe_deps = {
  env_int : string -> int -> int;
  getenv : string -> string option;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
  normalize_next_epoch_for_head : source:string -> unit;
  committed_head_epoch : unit -> int;
  current_epoch : unit -> int;
  catchup_next_target : unit -> int64 option;
  attested_head : int -> bool;
  clear_state_attested : unit -> unit;
  set_catchup_in_progress : bool -> unit;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  read_local_root_raw : unit -> string Lwt.t;
  committed_epoch_root_raw : int -> string option;
  drain_pending_finalized : unit -> unit Lwt.t;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  ahead_streak : unit -> int;
  incr_ahead_streak : unit -> unit;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
}

type fork_repair_runtime = {
  committed_head_epoch : unit -> int;
  rewind_allowed : target:int -> head:int -> bool;
  target_matches : target:int -> root:string -> bool;
  empty_after : target:int -> head:int -> bool;
  finality_target_ready : int -> (unit, string) result;
  run_empty : target:int -> root:string -> Octra_core.Fork_head_repair.result Lwt.t;
  rewind_finality : int -> (unit, string) result;
  drop_finality_after : int -> int;
  prune_after_epoch : int -> unit;
  set_current_epoch : int -> unit;
  set_state_attested : head:int -> root:string -> unit;
  set_catchup_in_progress : bool -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
}

type node_fork_repair_runtime = {
  chain_id : string;
  committed_head_epoch : unit -> int;
  data_dir : string;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  finality : Consensus_finality_state.callbacks;
  current_epoch : int ref;
  catchup_active : bool ref;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
}

type node_driver_probe_runtime = {
  env_int : string -> int -> int;
  getenv : string -> string option;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
  normalize_next_epoch_for_head : source:string -> unit;
  committed_head_epoch : unit -> int;
  current_epoch : unit -> int;
  catchup_queue : Consensus_catchup_queue.t;
  catchup_active : bool ref;
  runtime_state : Consensus_runtime_state.t;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  read_local_root_raw : unit -> string Lwt.t;
  committed_epoch_root_raw : int -> string option;
  drain_pending_finalized : unit -> unit Lwt.t;
  fork_repair : fork_repair_runtime;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
}

type 'driver liveness_deps = {
  state : Consensus_liveness.state ref;
  snapshot : 'driver -> Consensus_liveness.driver_snapshot;
  expected : unit -> int64;
  now : unit -> float;
  stall_sec : float;
  observer : unit -> bool;
  voting : unit -> bool;
  catchup_active : unit -> bool;
  quarantine_active : unit -> bool;
  state_attested : unit -> bool;
  pending_finalized : unit -> bool;
  proposal_active : 'driver -> bool;
  log_reset : Consensus_liveness.reset -> unit;
  realign_progress :
    'driver ->
    height:int64 ->
    round:int ->
    unit Lwt.t;
}

type node_liveness_deps = {
  state : Consensus_liveness.state ref;
  current_epoch : unit -> int;
  now : unit -> float;
  stall_sec : float;
  observer : unit -> bool;
  voting : unit -> bool;
  catchup_active : unit -> bool;
  runtime_state : Consensus_runtime_state.t;
  finality : Consensus_finality_state.callbacks;
}

type node_driver_health_runtime = {
  env_int : string -> int -> int;
  getenv : string -> string option;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
  normalize_next_epoch_for_head : source:string -> unit;
  committed_head_epoch : unit -> int;
  current_epoch : unit -> int;
  catchup_queue : Consensus_catchup_queue.t;
  catchup_active : bool ref;
  runtime_state : Consensus_runtime_state.t;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  read_local_root_raw : unit -> string Lwt.t;
  committed_epoch_root_raw : int -> string option;
  drain_pending_finalized : unit -> unit Lwt.t;
  fork_repair : fork_repair_runtime;
  run_catchup_to_target :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  liveness_state : Consensus_liveness.state ref;
  now : unit -> float;
  stall_sec : float;
  observer : unit -> bool;
  voting : unit -> bool;
  finality : Consensus_finality_state.callbacks;
}

type node_driver_health_deps = {
  probe : driver_probe_deps;
  liveness : Octra_consensus.C_driver.t liveness_deps;
}

type consensus_driver_runtime = {
  sleep : float -> unit Lwt.t;
  state : state_refs;
  hold_startup : unit -> unit;
  replay_stashed : source:string -> unit Lwt.t;
  probe : driver_probe_deps;
  liveness : Octra_consensus.C_driver.t liveness_deps;
  pending_recovery : Octra_consensus.C_driver.t -> bool Lwt.t;
  poll_interval : float;
  pending_delay : float;
  log_started : unit -> unit;
}

type node_consensus_driver_runtime = {
  sleep : float -> unit Lwt.t;
  catchup_active : bool ref;
  runtime_state : Consensus_runtime_state.t;
  replay_stashed : source:string -> unit Lwt.t;
  probe : driver_probe_deps;
  liveness : Octra_consensus.C_driver.t liveness_deps;
  pending_recovery : Octra_consensus.C_driver.t -> bool Lwt.t;
  poll_interval : float;
  pending_delay : float;
  role_label : string;
  validator_count : int;
  quorum : int;
}

val runtime_state_ops :
  runtime_state_ops_deps ->
  runtime_state_ops

val node_runtime_state_ops :
  node_runtime_state_ops_deps ->
  runtime_state_ops

val maybe_reset_liveness :
  'driver liveness_deps ->
  'driver ->
  source:string ->
  unit Lwt.t

val log_liveness_reset :
  Consensus_liveness.reset ->
  unit

val node_liveness_deps :
  node_liveness_deps ->
  Octra_consensus.C_driver.t liveness_deps

val node_driver_health_deps :
  node_driver_health_runtime ->
  node_driver_health_deps

val snapshot_policy_threshold :
  getenv:(string -> string option) ->
  int

val peer_state_label :
  Octra_consensus.C_driver.peer_state_record ->
  string

val peer_snapshot_text :
  Octra_consensus.C_driver.peer_state_record list ->
  string

val node_fork_repair_runtime :
  node_fork_repair_runtime ->
  fork_repair_runtime

val fork_repair_deps :
  fork_repair_runtime ->
  Octra_consensus.C_driver.t ->
  Consensus_health_shell.fork_repair_deps

val repair_empty_fork_with_driver :
  fork_repair_runtime ->
  Octra_consensus.C_driver.t ->
  target_epoch:int64 ->
  target_root:string ->
  required:int ->
  current_root_quorum:bool ->
  bool Lwt.t

val node_driver_probe_deps :
  node_driver_probe_runtime ->
  driver_probe_deps

val driver_probe_config :
  driver_probe_deps ->
  Octra_consensus.C_driver.t ->
  Consensus_health_shell.config

val driver_probe_deps :
  driver_probe_deps ->
  Octra_consensus.C_driver.t ->
  Consensus_health_shell.deps

val run_driver_probe :
  driver_probe_deps ->
  Octra_consensus.C_driver.t ->
  unit Lwt.t

val startup_probe :
  ?delay:float ->
  deps ->
  unit Lwt.t

val after :
  deps ->
  delay:float ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t

val await_pending_recovery :
  deps ->
  delay:float ->
  (unit -> bool Lwt.t) ->
  bool Lwt.t

val poll_once :
  deps ->
  unit Lwt.t

val poll_step :
  deps ->
  unit Lwt.t

val poll_loop :
  deps ->
  interval:float ->
  unit Lwt.t

val launch :
  launch_deps ->
  unit

val launch_health :
  deps ->
  hold_startup:(unit -> unit) ->
  start_driver:(unit -> unit Lwt.t) ->
  poll_interval:float ->
  pending_delay:float ->
  pending_recovery:(unit -> bool Lwt.t) ->
  log_started:(unit -> unit) ->
  unit

val health_deps_of_driver :
  'driver driver_deps ->
  'driver ->
  deps

val launch_driver :
  'driver driver_deps ->
  'driver ->
  unit

val consensus_driver_deps :
  consensus_driver_runtime ->
  Octra_consensus.C_driver.t driver_deps

val node_consensus_driver_runtime :
  node_consensus_driver_runtime ->
  consensus_driver_runtime

val launch_consensus_driver :
  consensus_driver_runtime ->
  Octra_consensus.C_driver.t ->
  unit

val launch_node_consensus_driver :
  node_consensus_driver_runtime ->
  Octra_consensus.C_driver.t ->
  unit