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


val node_version :
  unit ->
  Yojson.Safe.t

val validator_view_pubkey :
  validator_view_pub:string ->
  validator_address:string ->
  Yojson.Safe.t

val epoch_tags :
  tags:int list ->
  keep_epochs:int ->
  Yojson.Safe.t

val consensus_peer_states :
  now:float ->
  diag:Octra_net.P2p_peer_diag.t option ->
  peer_records:Octra_consensus.C_driver.peer_state_record list option ->
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
  ?scheduled:Octra_consensus.C_config.scheduled ->
  Octra_consensus.C_engine.validator_set ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result