(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type network = {
  chain_id : string;
  consensus_mode : bool;
  consensus_port : int;
  consensus_peers : string list;
}

type wallet = {
  address : string;
  priv_b64 : string;
  pub_b64 : string;
}

type install = {
  set_consensus_config_hash : string -> unit;
  set_consensus_validator_set : Octra_consensus.C_types.validator_set -> unit;
  set_scheduled_validator_set : Octra_consensus.C_config.scheduled option -> unit;
  set_swarm : Octra_net.P2p_swarm.t -> unit;
}

type deps = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  current_height : unit -> int64;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
}

type node_request = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  current_height : unit -> int64;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
  chain_id : string;
  consensus_mode : bool;
  consensus_port : int;
  consensus_peers : string list;
  address : string;
  priv_b64 : string;
  pub_b64 : string;
}

type admission_deps = {
  getenv : string -> string option;
  activation_epoch : unit -> int64 option;
  info : string -> unit;
  warn : string -> unit;
  refuse : string list -> unit;
}

type persistent_update_deps = {
  read_pending : unit -> string option Lwt.t;
  read_marker : string -> string option Lwt.t;
  warn : string -> unit;
  current_height : unit -> int64;
}

type node_view = {
  validator_config : Validator_config.t;
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_validator_set_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  consensus_config_hash : string;
  p2p_config : P2p_config.t;
  readiness_requirements : Octra_core.Validator_ready_policy.requirements;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
  swarm_params : P2p_config.swarm_params;
  swarm : Octra_net.P2p_swarm.t;
}

type node_start_runtime = {
  getenv : string -> string option;
  info : string -> unit;
  warn : string -> unit;
  fatal : string -> unit;
  read_active_validator_meta : unit -> string option;
  read_pending_validator_meta : unit -> string option;
  read_head_hash : unit -> string option;
  root_of_head_hash : string -> string;
  install : install;
  activation_epoch : unit -> int64 option;
  chain_id : string;
  consensus_mode : bool;
  consensus_port : int;
  consensus_peers : string list;
  address : string;
  priv_b64 : string;
  pub_b64 : string;
  voting : bool;
  role_label : string;
  read_persistent_pending : unit -> string option Lwt.t;
  read_persistent_marker : string -> string option Lwt.t;
  current_height : unit -> int64;
}

type node_start = {
  view : node_view;
  load_scheduled_validator_set_config :
    unit ->
    Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t;
  activate_validator_set :
    Octra_consensus.C_types.validator_set ->
    string ->
    unit Lwt.t;
}

val raw32_zero : string

val install_refs :
  consensus_config_hash:string ref ->
  consensus_validator_set:Octra_consensus.C_types.validator_set ref ->
  scheduled_validator_set:Octra_consensus.C_config.scheduled option ref ->
  set_swarm:(Octra_net.P2p_swarm.t -> unit) ->
  install

val current_height :
  deps ->
  int64

val best_root :
  deps ->
  string

val node_stack_deps :
  deps ->
  network ->
  wallet ->
  P2p_config.node_stack_deps

val build :
  deps ->
  network ->
  wallet ->
  (P2p_config.node_stack, string) result

val deps_of_request :
  node_request ->
  deps

val network_of_request :
  node_request ->
  network

val wallet_of_request :
  node_request ->
  wallet

val stack_view :
  P2p_config.node_stack ->
  node_view

val build_node_view :
  node_request ->
  (node_view, string) result

val request_of_runtime :
  node_start_runtime ->
  node_request

val admit_validator_startup :
  admission_deps ->
  address:string ->
  voting:bool ->
  role_label:string ->
  Validator_config.t ->
  unit

val load_persistent_update :
  persistent_update_deps ->
  runtime:Octra_core.Validator_ready_policy.runtime ->
  requirements:Octra_core.Validator_ready_policy.requirements ->
  Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t

val admission_deps_of_runtime :
  node_start_runtime ->
  admission_deps

val persistent_update_deps_of_runtime :
  node_start_runtime ->
  persistent_update_deps

val load_runtime_persistent_update :
  node_start_runtime ->
  node_view ->
  Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t

val start_node :
  node_start_runtime ->
  node_start

val load_irmin_persistent_update :
  store:Octra_core.Store_irmin.t ->
  warn:(string -> unit) ->
  current_height:(unit -> int64) ->
  runtime:Octra_core.Validator_ready_policy.runtime ->
  requirements:Octra_core.Validator_ready_policy.requirements ->
  Octra_consensus.C_driver.scheduled_validator_set_config option Lwt.t