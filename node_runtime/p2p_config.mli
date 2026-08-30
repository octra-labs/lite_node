(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  config_hash : string;
  binary_hash : string;
  require_binary_hash : bool;
  upgrade_plan : Octra_net.P2p_upgrade_plan.t option;
  handshake_allowed_pubkeys : string list;
  validator_pubkeys : string list;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
}

type swarm_params = {
  listen_port : int;
  chain_id : string;
  node_id : string;
  node_addr : string;
  pubkey_raw : string;
  consensus_config_hash : string;
  bootstrap_peers : string list;
  max_peers : int;
  sign_fn : string -> string;
  best_epoch_fn : unit -> int64;
  best_root_fn : unit -> string;
  handshake : t;
}

type identity = {
  priv_raw_32 : string;
  pub_raw_32 : string;
  node_id : string;
  sign_fn : string -> string;
}

type env = {
  binary_hash_value : string option;
  require_binary_hash : bool;
  allow_dynamic_peers : bool;
  validator_ready_strict : bool;
  validator_ready_min_shadow_epochs : int;
}

type upgrade_log =
  | Upgrade_scheduled of {
      activate_epoch : int64;
      binary_hash : string;
      config_hash : string;
    }
  | Rollback_scheduled of {
      activate_epoch : int64;
      binary_hash : string;
      config_hash : string;
    }

type startup_config = {
  validator : Validator_config.t;
  env : env;
  upgrade_plan : Octra_net.P2p_upgrade_plan.t option;
  handshake : t;
  readiness_requirements : Octra_core.Validator_ready_policy.requirements;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
  upgrade_logs : upgrade_log list;
}

type startup_runtime = {
  active_vs : Octra_consensus.C_types.validator_set;
  scheduled_driver_config : Octra_consensus.C_driver.scheduled_validator_set_config option;
  light_scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  consensus_config_hash : string;
  handshake : t;
  readiness_requirements : Octra_core.Validator_ready_policy.requirements;
  readiness_runtime : Octra_core.Validator_ready_policy.runtime;
}

type node_startup_install_deps = {
  info : string -> unit;
  warn : string -> unit;
  set_consensus_config_hash : string -> unit;
  set_consensus_validator_set : Octra_consensus.C_types.validator_set -> unit;
  set_scheduled_validator_set : Octra_consensus.C_config.scheduled option -> unit;
}

type node_swarm_start_deps = {
  info : string -> unit;
  set_swarm : Octra_net.P2p_swarm.t -> unit;
}

type node_swarm_start = {
  swarm : Octra_net.P2p_swarm.t;
  params : swarm_params;
}

type node_stack_deps = {
  getenv : string -> string option;
  chain_id : string;
  consensus_mode : bool;
  current_height : int64;
  chain_active_raw : string option;
  chain_pending_raw : string option;
  chain_pending_entries : string list;
  install : node_startup_install_deps;
  swarm : node_swarm_start_deps;
  listen_port : int;
  node_addr : string;
  priv_b64 : string;
  pub_b64 : string;
  bootstrap_peers : string list;
  best_epoch_fn : unit -> int64;
  best_root_fn : unit -> string;
}

type node_stack = {
  startup : startup_config;
  runtime : startup_runtime;
  swarm_start : node_swarm_start;
}

type upgrade_readiness_state

type upgrade_readiness =
  | Upgrade_ready of upgrade_readiness_state
  | Upgrade_not_ready of {
      state : upgrade_readiness_state;
      reason : string;
      should_log : bool;
    }

val identity :
  priv_b64:string ->
  pub_b64:string ->
  identity

val env_of_getenv : (string -> string option) -> env

val normalize_binary_hash : string option -> (string, string) result

val handshake_allowed_pubkeys :
  allow_dynamic_peers:bool ->
  string list ->
  string list

val build :
  chain_id:string ->
  consensus_config_hash:string ->
  allowed_pubkeys:string list ->
  binary_hash_value:string option ->
  require_binary_hash:bool ->
  allow_dynamic_peers:bool ->
  upgrade_plan:Octra_net.P2p_upgrade_plan.t option ->
  (t, string) result

val build_from_env :
  env ->
  chain_id:string ->
  consensus_config_hash:string ->
  allowed_pubkeys:string list ->
  upgrade_plan:Octra_net.P2p_upgrade_plan.t option ->
  (t, string) result

val readiness_requirements :
  env ->
  Octra_core.Validator_ready_policy.requirements

val upgrade_logs :
  Octra_net.P2p_upgrade_plan.t option ->
  upgrade_log list

val upgrade_log_message :
  upgrade_log ->
  string

val startup_config :
  env:env ->
  chain_id:string ->
  consensus_mode:bool ->
  current_height:int64 ->
  current_entries:string list ->
  next_entries:string list ->
  chain_pending_entries:string list ->
  next_activation_epoch:int64 option ->
  ?program_trust_hash:string ->
  ?runtime_profile_hash:string ->
  ?active_raw:string ->
  ?pending_raw:string ->
  unit ->
  (startup_config, string) result

val node_startup_config :
  getenv:(string -> string option) ->
  chain_id:string ->
  consensus_mode:bool ->
  current_height:int64 ->
  chain_active_raw:string option ->
  chain_pending_raw:string option ->
  chain_pending_entries:string list ->
  (startup_config, string) result

val install_node_startup_config :
  node_startup_install_deps ->
  current_height:int64 ->
  startup_config ->
  startup_runtime

val upgrade_readiness_state :
  upgrade_readiness_state

val upgrade_ready :
  upgrade_readiness_state ->
  epoch:int64 ->
  consensus_config_hash:string ->
  t ->
  upgrade_readiness

val upgrade_ready_checker :
  log_blocked:(string -> unit) ->
  epoch:(unit -> int64) ->
  consensus_config_hash:string ->
  t ->
  unit ->
  bool

val swarm_params :
  listen_port:int ->
  chain_id:string ->
  node_addr:string ->
  identity:identity ->
  consensus_config_hash:string ->
  bootstrap_peers:string list ->
  best_epoch_fn:(unit -> int64) ->
  best_root_fn:(unit -> string) ->
  handshake:t ->
  swarm_params

val startup_swarm_params :
  listen_port:int ->
  chain_id:string ->
  node_addr:string ->
  identity:identity ->
  bootstrap_peers:string list ->
  best_epoch_fn:(unit -> int64) ->
  best_root_fn:(unit -> string) ->
  startup_config ->
  swarm_params

val node_startup_swarm_params :
  listen_port:int ->
  chain_id:string ->
  node_addr:string ->
  priv_b64:string ->
  pub_b64:string ->
  bootstrap_peers:string list ->
  best_epoch_fn:(unit -> int64) ->
  best_root_fn:(unit -> string) ->
  startup_config ->
  swarm_params

val swarm_config : swarm_params -> Octra_net.P2p_swarm.config

val create_swarm : swarm_params -> Octra_net.P2p_swarm.t

val start_node_swarm :
  node_swarm_start_deps ->
  listen_port:int ->
  chain_id:string ->
  node_addr:string ->
  priv_b64:string ->
  pub_b64:string ->
  bootstrap_peers:string list ->
  best_epoch_fn:(unit -> int64) ->
  best_root_fn:(unit -> string) ->
  startup_config ->
  node_swarm_start

val node_stack :
  node_stack_deps ->
  (node_stack, string) result