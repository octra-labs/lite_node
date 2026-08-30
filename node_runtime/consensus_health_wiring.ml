(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Log = Octra_log

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

let runtime_state_ops (deps : runtime_state_ops_deps) =
  {
    clear_state_attested = (fun () ->
      Consensus_runtime_state.clear_state_attested deps.state);
    set_state_attested = (fun ~head ~root ->
      Consensus_runtime_state.set_state_attested deps.state ~head ~root);
    attested_head = Consensus_runtime_state.attested_head deps.state;
    mark_quarantine = (fun reason ->
      let epoch = deps.current_epoch () in
      if
        Consensus_runtime_state.enter_quarantine
          deps.state
          ~epoch
          ~reason
      then
        deps.warn_quarantine ~epoch ~reason);
    clear_quarantine = (fun reason ->
      let epoch = deps.current_epoch () in
      match
        Consensus_runtime_state.clear_quarantine deps.state ~evidence:reason
      with
      | Consensus_runtime_state.Cleared ->
        deps.info_quarantine ~epoch ~reason
      | Consensus_runtime_state.Inactive -> ()
      | Consensus_runtime_state.Refused active_reason ->
        Octra_log.warn "consensus"
          "event = quarantine_clear_refused active_reason = %s evidence = %s epoch = %d"
          active_reason reason epoch);
  }

let node_runtime_state_ops (deps : node_runtime_state_ops_deps) =
  runtime_state_ops
    {
      state = deps.state;
      current_epoch = deps.current_epoch;
      warn_quarantine = (fun ~epoch ~reason ->
        Octra_log.warn "consensus"
          "event = self_quarantine_enter epoch = %d reason = %s"
          epoch
          reason);
      info_quarantine = (fun ~epoch ~reason ->
        Octra_log.info "consensus"
          "event = self_quarantine_exit epoch = %d reason = %s"
          epoch
          reason);
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
      ~proposal_active:(deps.proposal_active driver)
  in
  deps.state := result.state;
  match result.reset with
  | None ->
    Lwt.return_unit
  | Some reset ->
    deps.log_reset reset;
    deps.realign_progress
      driver
      ~height:reset.expected
      ~round:reset.round

let log_liveness_reset (reset : Consensus_liveness.reset) =
  let step_timeout_sec =
    Option.value reset.Consensus_liveness.step_timeout_sec ~default:0.0
  in
  Octra_log.warn "consensus"
    "liveness realign height = %Ld round = %d step = %s source = %s stall_sec = %.0f step_timeout_sec = %.0f state_age = %.0f height_age = %.0f resets = %d"
    reset.Consensus_liveness.height
    reset.round
    reset.step
    reset.source
    reset.stall_sec
    step_timeout_sec
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
    proposal_active = Octra_consensus.C_driver.proposal_work_active;
    log_reset = log_liveness_reset;
    realign_progress = (fun driver ~height ~round ->
      Octra_consensus.C_driver.realign_progress driver ~height ~round);
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

let node_fork_repair_runtime (runtime : node_fork_repair_runtime) =
  let entry target =
    Octra_consensus.Finality_log.find
      (Octra_consensus.Finality_log.index runtime.data_dir)
      target
  in
  let with_entry target action =
    if not (Consensus_finality_journal.committed runtime.data_dir) then Ok ()
    else
      match entry target with
      | None -> Error "finality entry is missing"
      | Some value -> action value
  in
  {
    committed_head_epoch = runtime.committed_head_epoch;
    rewind_allowed = (fun ~target ~head ->
      Octra_consensus.C_quorum_policy.rewind_allowed
        ~chain_id:runtime.chain_id
        ~from_epoch:(Int64.of_int head)
        ~to_epoch:(Int64.of_int target));
    target_matches = (fun ~target ~root ->
      Octra_core.Fork_head_repair.target_matches
        runtime.chaindata
        ~target
        ~root);
    empty_after = (fun ~target ~head ->
      Octra_core.Fork_head_repair.empty_after
        runtime.chaindata
        ~target
        ~head);
    finality_target_ready = (fun target ->
      with_entry target (fun entry ->
        Consensus_finality_journal.committed_for
          ~chain_id:runtime.chain_id
          ~entry
          runtime.data_dir));
    run_empty = (fun ~target ~root ->
      Octra_core.Fork_head_repair.run_empty
        ~data_dir:runtime.data_dir
        ~store:runtime.store
        ~chaindata:runtime.chaindata
        ~target
        ~root);
    rewind_finality = (fun target ->
      with_entry target (fun entry ->
        Consensus_finality_journal.rewind_committed
          ~chain_id:runtime.chain_id
          ~entry
          runtime.data_dir));
    drop_finality_after =
      Octra_consensus.Finality_log.drop_after runtime.data_dir;
    prune_after_epoch = runtime.finality.prune_after_epoch;
    set_current_epoch = (fun epoch ->
      runtime.current_epoch := epoch);
    set_state_attested = runtime.set_state_attested;
    set_catchup_in_progress = (fun active ->
      runtime.catchup_active := active);
    clear_quarantine = runtime.clear_quarantine;
    mark_quarantine = runtime.mark_quarantine;
  }

let fork_repair_deps (runtime : fork_repair_runtime) driver =
  Consensus_health_shell.{
    committed_head_epoch = runtime.committed_head_epoch;
    rewind_allowed = runtime.rewind_allowed;
    target_matches = runtime.target_matches;
    empty_after = runtime.empty_after;
    run_empty = runtime.run_empty;
    finality_target_ready = runtime.finality_target_ready;
    rewind_finality = runtime.rewind_finality;
    drop_finality_after = runtime.drop_finality_after;
    prune_after_epoch = runtime.prune_after_epoch;
    set_current_epoch = runtime.set_current_epoch;
    set_state_attested = runtime.set_state_attested;
    set_catchup_in_progress = runtime.set_catchup_in_progress;
    clear_quarantine = runtime.clear_quarantine;
    mark_quarantine = runtime.mark_quarantine;
    start_height = Octra_consensus.C_driver.start_height driver;
    wake_ready = (fun () ->
      Octra_consensus.C_driver.wake_ready driver);
  }

let repair_empty_fork_with_driver (runtime : fork_repair_runtime) driver
    ~target_epoch ~target_root ~required ~current_root_quorum =
  Consensus_health_shell.repair_empty_fork
    (fork_repair_deps runtime driver)
    ~target_epoch
    ~target_root
    ~required
    ~current_root_quorum

let node_driver_probe_deps (runtime : node_driver_probe_runtime) =
  {
    env_int = runtime.env_int;
    getenv = runtime.getenv;
    soft_catchup_max_lag = runtime.soft_catchup_max_lag;
    quarantine_ahead_streak_threshold =
      runtime.quarantine_ahead_streak_threshold;
    quarantine_ahead_grace_epochs =
      runtime.quarantine_ahead_grace_epochs;
    quarantine_ahead_drift_tolerance =
      runtime.quarantine_ahead_drift_tolerance;
    normalize_next_epoch_for_head =
      runtime.normalize_next_epoch_for_head;
    committed_head_epoch = runtime.committed_head_epoch;
    current_epoch = runtime.current_epoch;
    catchup_next_target = (fun () ->
      Consensus_catchup_queue.target runtime.catchup_queue);
    attested_head = Consensus_runtime_state.attested_head runtime.runtime_state;
    clear_state_attested = (fun () ->
      Consensus_runtime_state.clear_state_attested runtime.runtime_state);
    set_catchup_in_progress = (fun active ->
      runtime.catchup_active := active);
    set_state_attested = runtime.set_state_attested;
    clear_quarantine = runtime.clear_quarantine;
    mark_quarantine = runtime.mark_quarantine;
    read_local_root_raw = runtime.read_local_root_raw;
    committed_epoch_root_raw = runtime.committed_epoch_root_raw;
    drain_pending_finalized = runtime.drain_pending_finalized;
    quarantine_active = (fun () ->
      Consensus_runtime_state.quarantine_active runtime.runtime_state);
    quarantine_reason = (fun () ->
      Consensus_runtime_state.quarantine_reason runtime.runtime_state);
    ahead_streak = (fun () ->
      Consensus_runtime_state.ahead_streak runtime.runtime_state);
    incr_ahead_streak = (fun () ->
      Consensus_runtime_state.incr_ahead_streak runtime.runtime_state);
    run_catchup_to_target = (fun driver ~target_epoch ~reason ->
      runtime.run_catchup_to_target driver ~target_epoch ~reason);
  }

let node_driver_health_deps (runtime : node_driver_health_runtime) =
  {
    probe =
      node_driver_probe_deps
        ({
          env_int = runtime.env_int;
          getenv = runtime.getenv;
          soft_catchup_max_lag = runtime.soft_catchup_max_lag;
          quarantine_ahead_streak_threshold =
            runtime.quarantine_ahead_streak_threshold;
          quarantine_ahead_grace_epochs =
            runtime.quarantine_ahead_grace_epochs;
          quarantine_ahead_drift_tolerance =
            runtime.quarantine_ahead_drift_tolerance;
          normalize_next_epoch_for_head =
            runtime.normalize_next_epoch_for_head;
          committed_head_epoch = runtime.committed_head_epoch;
          current_epoch = runtime.current_epoch;
          catchup_queue = runtime.catchup_queue;
          catchup_active = runtime.catchup_active;
          runtime_state = runtime.runtime_state;
          set_state_attested = runtime.set_state_attested;
          clear_quarantine = runtime.clear_quarantine;
          mark_quarantine = runtime.mark_quarantine;
          read_local_root_raw = runtime.read_local_root_raw;
          committed_epoch_root_raw = runtime.committed_epoch_root_raw;
          drain_pending_finalized = runtime.drain_pending_finalized;
          fork_repair = runtime.fork_repair;
          run_catchup_to_target = runtime.run_catchup_to_target;
        } : node_driver_probe_runtime);
    liveness =
      node_liveness_deps
        ({
          state = runtime.liveness_state;
          current_epoch = runtime.current_epoch;
          now = runtime.now;
          stall_sec = runtime.stall_sec;
          observer = runtime.observer;
          voting = runtime.voting;
          catchup_active = (fun () -> !(runtime.catchup_active));
          runtime_state = runtime.runtime_state;
          finality = runtime.finality;
        } : node_liveness_deps);
  }

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
        ~wait_for:Octra_consensus.C_driver.Consensus_quorum
        driver
        ~epoch_id
        ~timeout_seconds);
    root_consensus_quorum = (fun ~epoch_id ~root responses ->
      Octra_consensus.C_driver.epoch_root_consensus_quorum
        driver
        ~epoch_id
        ~root
        responses);
    read_local_root_raw = deps.read_local_root_raw;
    committed_epoch_root_raw = deps.committed_epoch_root_raw;
    peer_snapshot = (fun () ->
      Octra_consensus.C_driver.peer_state_snapshot driver
      |> peer_snapshot_text);
    drain_pending_finalized = deps.drain_pending_finalized;
    wake_ready = (fun () ->
      Octra_consensus.C_driver.wake_ready driver);
    run_catchup_to_target = (fun ~target_epoch ~reason ->
      deps.run_catchup_to_target driver ~target_epoch ~reason);
    quarantine_active = deps.quarantine_active;
    quarantine_reason = deps.quarantine_reason;
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

let rec await_pending_recovery (deps : deps) ~delay recover =
  let open Lwt.Syntax in
  let* ready = recover () in
  if ready then Lwt.return_true
  else
    let* () = deps.sleep delay in
    await_pending_recovery deps ~delay recover

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

let poll_step deps =
  Lwt.catch
    (fun () -> poll_once deps)
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn ->
        Log.error "consensus"
          "event = health_poll_failed error = %s"
          (Printexc.to_string exn);
        Lwt.return_unit)

let rec poll_loop (deps : deps) ~interval =
  let open Lwt.Syntax in
  let* () = deps.sleep interval in
  let* () = poll_step deps in
  poll_loop deps ~interval

let launch (deps : launch_deps) =
  deps.hold_startup ();
  Lwt.async (fun () ->
    let open Lwt.Syntax in
    let* ready = deps.pending_recovery () in
    if not ready then Lwt.return_unit
    else
      let* () = deps.start_driver () in
      let* () = deps.startup_probe () in
      Lwt.async deps.poll_loop;
      deps.log_started ();
      Lwt.return_unit)

let launch_health (deps : deps) ~hold_startup ~start_driver ~poll_interval
    ~pending_delay ~pending_recovery ~log_started =
  launch {
    start_driver;
    startup_probe = (fun () -> startup_probe deps);
    poll_loop = (fun () -> poll_loop deps ~interval:poll_interval);
    pending_recovery = (fun () ->
      await_pending_recovery deps ~delay:pending_delay pending_recovery);
    hold_startup;
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
    ~hold_startup:deps.hold_startup
    ~start_driver:(fun () -> deps.start_driver driver)
    ~poll_interval:deps.poll_interval
    ~pending_delay:deps.pending_delay
    ~pending_recovery:(fun () -> deps.pending_recovery driver)
    ~log_started:deps.log_started

let consensus_driver_deps (deps : consensus_driver_runtime) =
  {
    sleep = deps.sleep;
    state = deps.state;
    hold_startup = deps.hold_startup;
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

let node_consensus_driver_runtime runtime =
  {
    sleep = runtime.sleep;
    state = {
      catchup = runtime.catchup_active;
      quarantine =
        Consensus_runtime_state.quarantine_active_ref runtime.runtime_state;
    };
    hold_startup = runtime.probe.clear_state_attested;
    replay_stashed = runtime.replay_stashed;
    probe = runtime.probe;
    liveness = runtime.liveness;
    pending_recovery = runtime.pending_recovery;
    poll_interval = runtime.poll_interval;
    pending_delay = runtime.pending_delay;
    log_started = (fun () ->
      Octra_log.info "init"
        "event = consensus_start role = %s validators = %d quorum = %d"
        runtime.role_label
        runtime.validator_count
        runtime.quorum);
  }

let launch_consensus_driver deps driver =
  launch_driver (consensus_driver_deps deps) driver

let launch_node_consensus_driver runtime driver =
  launch_consensus_driver (node_consensus_driver_runtime runtime) driver