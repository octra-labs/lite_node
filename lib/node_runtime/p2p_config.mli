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


type t = {
  binary_hash : string;
  require_binary_hash : bool;
  upgrade_plan : Octra_net.P2p_upgrade_plan.t option;
  handshake_allowed_pubkeys : string list;
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

val swarm_config : swarm_params -> Octra_net.P2p_swarm.config

val create_swarm : swarm_params -> Octra_net.P2p_swarm.t