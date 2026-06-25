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


type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

type read_ctx = {
  ledger : Octra_core.Ledger.t;
  store : Octra_core.Store_irmin.t;
  chaindata : Octra_core.Store_chaindata.t;
  tree_ref : Octra_core.Tree.t ref;
  validator_address : string;
  validator_pubkey : string;
  validator_priv_b64 : string;
  chain_id : string;
  config_hash : string;
  validator_view_pub : string;
  validator_set : Octra_consensus.C_types.validator_set;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  current_epoch : int ref;
  total_tx_count : int ref;
  encrypted : unit -> Z.t;
  swarm : unit -> Octra_net.P2p_swarm.t option;
  driver_ref : Octra_consensus.C_driver.t option ref;
}

type 'handler dispatch_adapters = {
  status_read : (Yojson.Safe.t -> read_ctx -> rpc_result) -> 'handler;
}

val signer :
  chain_id:string ->
  validator_addr:string ->
  validator_pubkey:string ->
  validator_priv_b64:string ->
  config_hash:string ->
  Light_rpc.signer

val node_version :
  unit ->
  rpc_result

val validator_view_pubkey :
  validator_view_pub:string ->
  validator_address:string ->
  rpc_result

val epoch_tags :
  Octra_core.Store_irmin.t ->
  rpc_result

val signed_root :
  Light_rpc.signer ->
  Octra_core.Head_manifest.t option ->
  rpc_result

val validator_set_proof :
  chain_id:string ->
  config_hash:string ->
  driver_ref:Octra_consensus.C_driver.t option ref ->
  validator_set:Octra_consensus.C_types.validator_set ->
  scheduled_validator_set:Octra_consensus.C_config.scheduled option ->
  rpc_result

val consensus_peer_states :
  swarm:Octra_net.P2p_swarm.t option ->
  driver_ref:Octra_consensus.C_driver.t option ref ->
  rpc_result

val node_status :
  tree_ref:Octra_core.Tree.t ref ->
  validator:string ->
  rpc_result

val node_stats :
  ledger:Octra_core.Ledger.t ->
  chaindata:Octra_core.Store_chaindata.t ->
  current_epoch:int ->
  total_confirmed:int ->
  encrypted:Z.t ->
  rpc_result

val node_version_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val validator_view_pubkey_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val epoch_tags_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val signed_root_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val account_proof_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val validator_set_proof_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val epoch_proof_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val tx_inclusion_proof_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val consensus_peer_states_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val node_status_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val node_stats_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val node_metrics_params :
  Yojson.Safe.t ->
  read_ctx ->
  rpc_result

val core_dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list

val proof_dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list

val dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list