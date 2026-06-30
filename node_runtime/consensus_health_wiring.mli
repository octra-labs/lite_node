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
  pending_recovery : unit -> unit Lwt.t;
  log_started : unit -> unit;
}

type state_refs = {
  catchup : bool ref;
  quarantine : bool ref;
}

type 'driver driver_deps = {
  sleep : float -> unit Lwt.t;
  state : state_refs;
  replay_stashed : source:string -> unit Lwt.t;
  start_driver : 'driver -> unit Lwt.t;
  probe_health : 'driver -> unit Lwt.t;
  reset_liveness : 'driver -> source:string -> unit Lwt.t;
  pending_recovery : 'driver -> unit Lwt.t;
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
  ahead_streak : unit -> int;
  incr_ahead_streak : unit -> unit;
  repair_empty_fork :
    Octra_consensus.C_driver.t ->
    target_epoch:int64 ->
    target_root:string ->
    required:int ->
    current_root_quorum:bool ->
    bool Lwt.t;
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
  log_reset : Consensus_liveness.reset -> unit;
  realign_height : 'driver -> int64 -> unit Lwt.t;
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

type consensus_driver_runtime = {
  sleep : float -> unit Lwt.t;
  state : state_refs;
  replay_stashed : source:string -> unit Lwt.t;
  probe : driver_probe_deps;
  liveness : Octra_consensus.C_driver.t liveness_deps;
  pending_recovery : Octra_consensus.C_driver.t -> unit Lwt.t;
  poll_interval : float;
  pending_delay : float;
  log_started : unit -> unit;
}

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

val snapshot_policy_threshold :
  getenv:(string -> string option) ->
  int

val peer_state_label :
  Octra_consensus.C_driver.peer_state_record ->
  string

val peer_snapshot_text :
  Octra_consensus.C_driver.peer_state_record list ->
  string

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

val poll_once :
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
  start_driver:(unit -> unit Lwt.t) ->
  poll_interval:float ->
  pending_delay:float ->
  pending_recovery:(unit -> unit Lwt.t) ->
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

val launch_consensus_driver :
  consensus_driver_runtime ->
  Octra_consensus.C_driver.t ->
  unit