(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val node_version :
  unit ->
  Yojson.Safe.t

val runtime_version :
  source_commit:string option ->
  binary_hash:string option ->
  consensus_profile:int ->
  consensus_rules_id:string ->
  runtime_profile_hash:string option ->
  config_hash:string ->
  chain_id:string ->
  validator:string ->
  Yojson.Safe.t

val validator_view_pubkey :
  validator_view_pub:string ->
  validator_address:string ->
  Yojson.Safe.t

val epoch_tags :
  count:int ->
  min_epoch:int ->
  max_epoch:int ->
  keep_epochs:int ->
  Yojson.Safe.t

val validator_enrollment :
  head_epoch:int ->
  address:string ->
  pubkey:string ->
  Octra_core.Validator_admission.candidate option ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val consensus_peer_states :
  now:float ->
  diag:Octra_net.P2p_peer_diag.t option ->
  peer_records:Octra_consensus.C_driver.peer_state_record list option ->
  voting:bool ->
  voting_reason:string option ->
  round_state:Octra_consensus.C_driver.round_state option ->
  round_peers:Yojson.Safe.t list ->
  round_votes:Octra_consensus.C_driver.round_votes option ->
  round_agreed:bool ->
  Yojson.Safe.t

val node_status :
  epoch:int ->
  validator:string ->
  roots:int ->
  timestamp:float ->
  head:Octra_core.Head_manifest.t option ->
  Yojson.Safe.t

val node_stats :
  current_epoch:int ->
  total_accounts:int ->
  active_accounts:int ->
  true_total:Z.t ->
  encrypted:Z.t ->
  max_supply:Z.t ->
  total_confirmed:int ->
  staging:int ->
  recent_tx_count:int ->
  latest_epochs:int list ->
  head:Octra_core.Head_manifest.t option ->
  Yojson.Safe.t

val validator_set_proof :
  chain_id:string ->
  config_hash:string ->
  ?program_trust_hash:string ->
  ?runtime_profile_hash:string ->
  ?scheduled:Octra_consensus.C_config.scheduled ->
  Octra_consensus.C_engine.validator_set ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result