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

let normalize_binary_hash = function
  | None -> Ok (String.make 32 '\x00')
  | Some raw -> Octra_net.P2p_upgrade_plan.normalize_hash "OCTRA_BINARY_HASH" raw

let handshake_allowed_pubkeys ~allow_dynamic_peers allowed_pubkeys =
  if allow_dynamic_peers then [] else allowed_pubkeys

let build ~chain_id ~consensus_config_hash ~allowed_pubkeys
    ~binary_hash_value ~require_binary_hash ~allow_dynamic_peers ~upgrade_plan =
  match normalize_binary_hash binary_hash_value with
  | Error e -> Error e
  | Ok binary_hash ->
    let handshake_allowed_pubkeys =
      handshake_allowed_pubkeys ~allow_dynamic_peers allowed_pubkeys
    in
    let readiness_runtime = Octra_core.Validator_ready_policy.{
      chain_id;
      binary_hash = Text.raw_to_hex binary_hash;
      config_hash = Text.raw_to_hex consensus_config_hash;
    } in
    Ok {
      binary_hash;
      require_binary_hash;
      upgrade_plan;
      handshake_allowed_pubkeys;
      readiness_runtime;
    }

let swarm_config (params : swarm_params) =
  Octra_net.P2p_swarm.{
    listen_port = params.listen_port;
    chain_id = params.chain_id;
    node_id = params.node_id;
    node_addr = params.node_addr;
    pubkey_raw = params.pubkey_raw;
    consensus_config_hash = params.consensus_config_hash;
    binary_hash = params.handshake.binary_hash;
    require_binary_hash = params.handshake.require_binary_hash;
    upgrade_plan = params.handshake.upgrade_plan;
    allowed_pubkeys = params.handshake.handshake_allowed_pubkeys;
    bootstrap_peers = params.bootstrap_peers;
    max_peers = params.max_peers;
    sign_fn = params.sign_fn;
    best_epoch_fn = params.best_epoch_fn;
    best_root_fn = params.best_root_fn;
  }

let create_swarm params =
  Octra_net.P2p_swarm.create (swarm_config params)