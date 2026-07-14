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


type runtime = {
  driver_config : Consensus_driver_wiring.node_driver_config_runtime;
  validator_set : Octra_consensus.C_types.validator_set;
  swarm : Octra_net.P2p_swarm.t;
  driver_ref : Octra_consensus.C_driver.t option ref;
  start_height : int64;
  sleep : float -> unit Lwt.t;
  health : Consensus_health_wiring.node_driver_health_runtime;
  pending : Consensus_pending_commit_recovery.node_driver_runtime;
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
  poll_interval : float;
  pending_delay : float;
  role_label : string;
}

let health_runtime (input : health_runtime_input) =
  let health = Consensus_health_wiring.node_driver_health_deps input.health in
  let pending =
    Consensus_pending_commit_recovery.node_driver_runtime input.pending
  in
  let validator_set = input.validator_set in
  Consensus_health_wiring.{
    sleep = input.sleep;
    catchup_active = input.health.catchup_active;
    runtime_state = input.health.runtime_state;
    replay_stashed = input.replay_stashed;
    probe = health.probe;
    liveness = health.liveness;
    pending_recovery =
      Consensus_pending_commit_recovery.run_with_driver pending;
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

let run runtime =
  let driver = create_driver runtime in
  runtime.driver_ref := Some driver;
  Consensus_health_wiring.launch_node_consensus_driver
    (health_runtime_of_node runtime)
    driver;
  driver