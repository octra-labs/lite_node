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
  data_dir : string;
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

val recover_before_start :
  recover_pending:(unit -> bool Lwt.t) ->
  replay_stashed:(source:string -> unit Lwt.t) ->
  recovery_pending:(unit -> bool) ->
  bool Lwt.t

val health_runtime :
  health_runtime_input ->
  Consensus_health_wiring.node_consensus_driver_runtime

val health_runtime_of_node :
  runtime ->
  Consensus_health_wiring.node_consensus_driver_runtime

val pending_vote_wires :
  data_dir:string ->
  epoch_id:int64 ->
  (string list, string) result

val create_driver :
  runtime ->
  Octra_consensus.C_driver.t

val publish :
  'a option ref ->
  ('a -> unit) ->
  'a ->
  unit

val run :
  runtime ->
  Octra_consensus.C_driver.t