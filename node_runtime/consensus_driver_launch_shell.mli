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

val health_runtime :
  health_runtime_input ->
  Consensus_health_wiring.node_consensus_driver_runtime

val health_runtime_of_node :
  runtime ->
  Consensus_health_wiring.node_consensus_driver_runtime

val create_driver :
  runtime ->
  Octra_consensus.C_driver.t

val run :
  runtime ->
  Octra_consensus.C_driver.t