(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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
  program_trust_hash : string option;
  runtime_profile_hash : string option;
  validator_view_pub : string;
  validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
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

val runtime_version :
  chain_id:string ->
  validator_address:string ->
  program_trust_hash:string option ->
  runtime_profile_hash:string option ->
  validator_set_ref:Octra_consensus.C_types.validator_set ref ->
  scheduled_validator_set_ref:Octra_consensus.C_config.scheduled option ref ->
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
  program_trust_hash:string option ->
  runtime_profile_hash:string option ->
  validator_set_ref:Octra_consensus.C_types.validator_set ref ->
  scheduled_validator_set_ref:Octra_consensus.C_config.scheduled option ref ->
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

val runtime_version_params :
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