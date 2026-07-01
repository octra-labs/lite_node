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

let maybe_reset_liveness (deps : 'driver liveness_deps) driver ~source =
  let result =
    Consensus_liveness.record_snapshot
      !(deps.state)
      (deps.snapshot driver)
      ~expected:(deps.expected ())
      ~now:(deps.now ())
      ~source
      ~stall_sec:deps.stall_sec
      ~observer:(deps.observer ())
      ~voting:(deps.voting ())
      ~catchup_active:(deps.catchup_active ())
      ~quarantine_active:(deps.quarantine_active ())
      ~state_attested:(deps.state_attested ())
      ~pending_finalized:(deps.pending_finalized ())
  in
  deps.state := result.state;
  match result.reset with
  | None ->
    Lwt.return_unit
  | Some reset ->
    deps.log_reset reset;
    deps.realign_height driver reset.expected

let log_liveness_reset (reset : Consensus_liveness.reset) =
  Octra_log.warn "consensus"
    "liveness realign height = %Ld round = %d step = %s source = %s stall_sec = %.0f state_age = %.0f height_age = %.0f resets = %d"
    reset.Consensus_liveness.height
    reset.round
    reset.step
    reset.source
    reset.stall_sec
    reset.state_age
    reset.height_age
    reset.resets

let node_liveness_deps (deps : node_liveness_deps) =
  {
    state = deps.state;
    snapshot = Consensus_liveness.driver_snapshot;
    expected = (fun () ->
      Int64.of_int (deps.current_epoch ()));
    now = deps.now;
    stall_sec = deps.stall_sec;
    observer = deps.observer;
    voting = deps.voting;
    catchup_active = deps.catchup_active;
    quarantine_active = (fun () ->
      Consensus_runtime_state.quarantine_active deps.runtime_state);
    state_attested = (fun () ->
      Consensus_runtime_state.state_attested deps.runtime_state);
    pending_finalized = (fun () ->
      deps.finality.has_finalized (deps.current_epoch ()));
    log_reset = log_liveness_reset;
    realign_height = (fun driver expected ->
      Octra_consensus.C_driver.realign_height driver expected);
  }

let snapshot_policy_threshold ~getenv =
  match getenv "OCTRA_CATCHUP_MAX_LAG" with
  | Some s -> (try int_of_string s with _ -> 5000)
  | None -> 5000

let peer_state_label (record : Octra_consensus.C_driver.peer_state_record) =
  let root =
    match record.Octra_consensus.C_driver.state_root with
    | Some s -> Consensus_proposal.raw_hex8 s
    | None -> "-"
  in
  Printf.sprintf "peer = %s head = %Ld check = %Ld root = %s source = %s"
    (String.sub
       record.responder_addr
       0
       (min 12 (String.length record.responder_addr)))
    record.head_epoch
    record.checked_epoch
    root
    record.source

let peer_snapshot_text records =
  records
  |> List.map peer_state_label
  |> String.concat ","

let driver_probe_config (deps : driver_probe_deps) driver =
  let vs =
    driver.Octra_consensus.C_driver.engine
      .Octra_consensus.C_engine.vs
  in
  Consensus_health_shell.{
    active_f = vs.Octra_consensus.C_types.f;
    validator_count = vs.Octra_consensus.C_types.n;
    state_attest_configured =
      deps.env_int "OCTRA_STATE_ATTEST_MIN_PEERS" 0;
    snapshot_policy_threshold =
      snapshot_policy_threshold ~getenv:deps.getenv;
    soft_catchup_max_lag = deps.soft_catchup_max_lag;
    quarantine_ahead_streak_threshold =
      deps.quarantine_ahead_streak_threshold;
    quarantine_ahead_grace_epochs = deps.quarantine_ahead_grace_epochs;
    quarantine_ahead_drift_tolerance =
      deps.quarantine_ahead_drift_tolerance;
  }

let driver_probe_deps (deps : driver_probe_deps) driver =
  Consensus_health_shell.{
    normalize_next_epoch_for_head = deps.normalize_next_epoch_for_head;
    committed_head_epoch = deps.committed_head_epoch;
    current_epoch = deps.current_epoch;
    catchup_next_target = deps.catchup_next_target;
    attested_head = deps.attested_head;
    clear_state_attested = deps.clear_state_attested;
    set_catchup_in_progress = deps.set_catchup_in_progress;
    set_state_attested = deps.set_state_attested;
    clear_quarantine = deps.clear_quarantine;
    mark_quarantine = deps.mark_quarantine;
    query_epoch_root = (fun ~epoch_id ~timeout_seconds ->
      Octra_consensus.C_driver.query_epoch_root
        driver
        ~epoch_id
        ~timeout_seconds);
    read_local_root_raw = deps.read_local_root_raw;
    committed_epoch_root_raw = deps.committed_epoch_root_raw;
    peer_snapshot = (fun () ->
      Octra_consensus.C_driver.peer_state_snapshot driver
      |> peer_snapshot_text);
    drain_pending_finalized = deps.drain_pending_finalized;
    wake_ready = (fun () ->
      Octra_consensus.C_driver.wake_ready driver);
    repair_empty_fork = (fun ~target_epoch ~target_root ~required
        ~current_root_quorum ->
      deps.repair_empty_fork
        driver
        ~target_epoch
        ~target_root
        ~required
        ~current_root_quorum);
    run_catchup_to_target = (fun ~target_epoch ~reason ->
      deps.run_catchup_to_target driver ~target_epoch ~reason);
    quarantine_active = deps.quarantine_active;
    ahead_streak = deps.ahead_streak;
    incr_ahead_streak = deps.incr_ahead_streak;
  }

let run_driver_probe deps driver =
  Consensus_health_shell.run
    (driver_probe_config deps driver)
    (driver_probe_deps deps driver)

let startup_probe ?(delay = 5.0) (deps : deps) =
  let open Lwt.Syntax in
  deps.set_catchup_active true;
  let* () = deps.sleep delay in
  deps.set_catchup_active false;
  let* () = deps.probe_health () in
  deps.reset_liveness ~source:"startup_probe"

let after (deps : deps) ~delay f =
  let open Lwt.Syntax in
  let* () = deps.sleep delay in
  f ()

let poll_once (deps : deps) =
  let open Lwt.Syntax in
  if deps.catchup_active () then
    Lwt.return_unit
  else
    let* () =
      if deps.quarantine_active () then
        deps.replay_stashed ~source:"quarantine_poll"
      else
        Lwt.return_unit
    in
    let* () = deps.probe_health () in
    deps.reset_liveness ~source:"health_poll"

let rec poll_loop (deps : deps) ~interval =
  let open Lwt.Syntax in
  let* () = deps.sleep interval in
  let* () = poll_once deps in
  poll_loop deps ~interval

let launch (deps : launch_deps) =
  Lwt.async deps.start_driver;
  Lwt.async deps.startup_probe;
  Lwt.async deps.poll_loop;
  Lwt.async deps.pending_recovery;
  deps.log_started ()

let launch_health (deps : deps) ~start_driver ~poll_interval ~pending_delay ~pending_recovery
    ~log_started =
  launch {
    start_driver;
    startup_probe = (fun () -> startup_probe deps);
    poll_loop = (fun () -> poll_loop deps ~interval:poll_interval);
    pending_recovery = (fun () -> after deps ~delay:pending_delay pending_recovery);
    log_started;
  }

let health_deps_of_driver (deps : 'driver driver_deps) driver =
  {
    sleep = deps.sleep;
    catchup_active = (fun () -> !(deps.state.catchup));
    set_catchup_active = (fun active -> deps.state.catchup := active);
    quarantine_active = (fun () -> !(deps.state.quarantine));
    replay_stashed = deps.replay_stashed;
    probe_health = (fun () -> deps.probe_health driver);
    reset_liveness = (fun ~source ->
      deps.reset_liveness driver ~source);
  }

let launch_driver (deps : 'driver driver_deps) driver =
  launch_health
    (health_deps_of_driver deps driver)
    ~start_driver:(fun () -> deps.start_driver driver)
    ~poll_interval:deps.poll_interval
    ~pending_delay:deps.pending_delay
    ~pending_recovery:(fun () -> deps.pending_recovery driver)
    ~log_started:deps.log_started

let consensus_driver_deps deps =
  {
    sleep = deps.sleep;
    state = deps.state;
    replay_stashed = deps.replay_stashed;
    start_driver = Octra_consensus.C_driver.start;
    probe_health = (fun driver ->
      run_driver_probe deps.probe driver);
    reset_liveness = (fun driver ~source ->
      maybe_reset_liveness deps.liveness driver ~source);
    pending_recovery = deps.pending_recovery;
    poll_interval = deps.poll_interval;
    pending_delay = deps.pending_delay;
    log_started = deps.log_started;
  }

let launch_consensus_driver deps driver =
  launch_driver (consensus_driver_deps deps) driver