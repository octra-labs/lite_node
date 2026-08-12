(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type runtime = {
  driver_config : Consensus_driver_wiring.node_driver_config_runtime;
  validator_set : Octra_consensus.C_types.validator_set;
  swarm : Octra_net.P2p_swarm.t;
  activate_validator_set :
    Octra_consensus.C_types.validator_set ->
    string ->
    unit Lwt.t;
  driver_ref : Octra_consensus.C_driver.t option ref;
  on_driver : Octra_consensus.C_driver.t -> unit;
  start_height : int64;
  sleep : float -> unit Lwt.t;
  health : Consensus_health_wiring.node_driver_health_runtime;
  pending : Consensus_pending_commit_recovery.node_driver_runtime;
  recovery_pending : unit -> bool;
  poll_interval : float;
  pending_delay : float;
  role_label : string;
}

type health_runtime_input = {
  validator_set : Octra_consensus.C_types.validator_set;
  sleep : float -> unit Lwt.t;
  replay_stashed : source:string -> unit Lwt.t;
  health : Consensus_health_wiring.node_driver_health_runtime;
  pending : Consensus_pending_commit_recovery.node_driver_runtime;
  recovery_pending : unit -> bool;
  poll_interval : float;
  pending_delay : float;
  role_label : string;
}

let recover_before_start ~recover_pending ~replay_stashed ~recovery_pending =
  let open Lwt.Syntax in
  let* () = replay_stashed ~source:"startup_finality" in
  if recovery_pending () then
    Lwt.return_false
  else
    recover_pending ()

let health_runtime (input : health_runtime_input) =
  let health = Consensus_health_wiring.node_driver_health_deps input.health in
  let pending =
    Consensus_pending_commit_recovery.node_driver_runtime input.pending
  in
  let pending_recovery driver =
    recover_before_start
      ~recover_pending:(fun () ->
        Consensus_pending_commit_recovery.run_with_driver pending driver)
      ~replay_stashed:input.replay_stashed
      ~recovery_pending:input.recovery_pending
  in
  let validator_set = input.validator_set in
  Consensus_health_wiring.{
    sleep = input.sleep;
    catchup_active = input.health.catchup_active;
    runtime_state = input.health.runtime_state;
    replay_stashed = input.replay_stashed;
    probe = health.probe;
    liveness = health.liveness;
    pending_recovery;
    poll_interval = input.poll_interval;
    pending_delay = input.pending_delay;
    role_label = input.role_label;
    validator_count = validator_set.n;
    quorum = validator_set.quorum;
  }

let health_runtime_of_node (runtime : runtime) =
  health_runtime
    {
      validator_set = runtime.validator_set;
      sleep = runtime.sleep;
      replay_stashed = runtime.driver_config.replay_stashed_while_safe;
      health = runtime.health;
      pending = runtime.pending;
      recovery_pending = runtime.recovery_pending;
      poll_interval = runtime.poll_interval;
      pending_delay = runtime.pending_delay;
      role_label = runtime.role_label;
    }

let create_driver runtime =
  let config =
    Consensus_driver_wiring.node_driver_config runtime.driver_config
  in
  Octra_consensus.C_driver.create
    ~config
    ~validator_set:runtime.validator_set
    ~swarm:runtime.swarm
    ~start_height:runtime.start_height

let publish slot on_driver driver =
  slot := Some driver;
  on_driver driver

let run runtime =
  let driver = create_driver runtime in
  Octra_consensus.C_driver.set_validator_set_activation_handler
    driver
    runtime.activate_validator_set;
  publish runtime.driver_ref runtime.on_driver driver;
  Consensus_health_wiring.launch_node_consensus_driver
    (health_runtime_of_node runtime)
    driver;
  driver