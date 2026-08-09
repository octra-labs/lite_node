(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val opt_hex :
  string option ->
  Yojson.Safe.t

val opt_int :
  int option ->
  Yojson.Safe.t

val opt_string :
  string option ->
  Yojson.Safe.t

val json_short :
  int ->
  Yojson.Safe.t ->
  string

val peer_of_headers :
  cf_connecting_ip:string ->
  x_real_ip:string ->
  x_forwarded_for:string ->
  string

val user_agent :
  string ->
  string

val signed_root :
  validator_pubkey:string ->
  Octra_core.Head_manifest.t ->
  Octra_consensus.C_light_root.t ->
  Yojson.Safe.t

val account_witness :
  inclusion:bool ->
  Octra_consensus.C_light_account.t ->
  Yojson.Safe.t

val account_irmin_proof :
  addr:string ->
  Octra_core.Store_irmin.account_merkle_proof ->
  exists:bool ->
  Yojson.Safe.t

val account_proof :
  root:Yojson.Safe.t ->
  exists:bool ->
  account:Yojson.Safe.t ->
  irmin_proof:Yojson.Safe.t ->
  Yojson.Safe.t

val validator :
  weighted:bool ->
  Octra_consensus.C_light_validator_set.validator ->
  Yojson.Safe.t

val scheduled_validator :
  Octra_consensus.C_light_validator_set.scheduled option ->
  Yojson.Safe.t

val validator_set_proof :
  Octra_consensus.C_light_validator_set.t ->
  Yojson.Safe.t

val light_epoch :
  Octra_consensus.C_light_epoch.t ->
  Yojson.Safe.t

val epoch_proof :
  Octra_consensus.C_light_epoch.t ->
  Yojson.Safe.t

val tx_json_value :
  string ->
  Yojson.Safe.t

val tx_inclusion_proof :
  Octra_consensus.C_light_epoch.tx_inclusion ->
  tx_json:string ->
  Yojson.Safe.t

val head_fields :
  Octra_core.Head_manifest.t option ->
  (string * Yojson.Safe.t) list

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
  display_supply:Z.t ->
  encrypted_supply:Z.t ->
  max_supply:Z.t ->
  total_confirmed:int ->
  staging:int ->
  recent_tx_count:int ->
  latest_epochs:int list ->
  head:Octra_core.Head_manifest.t option ->
  Yojson.Safe.t

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

val balance :
  addr:string ->
  account:Octra_core.Ledger.account ->
  pending_nonce:int ->
  Yojson.Safe.t

val account_public_balance_or_zero :
  Octra_core.Ledger.account option ->
  string

val account_encrypted_balance_or_zero :
  Octra_core.Ledger.account option ->
  string

val nonce :
  addr:string ->
  account:Octra_core.Ledger.account ->
  Yojson.Safe.t

val public_key :
  addr:string ->
  public_key:string ->
  Yojson.Safe.t

val validate_address :
  raw:string ->
  is_valid:bool ->
  Yojson.Safe.t

val supply :
  display_supply:Z.t ->
  encrypted_supply:Z.t ->
  max_supply:Z.t ->
  emission_remaining:Z.t ->
  retired_supply:Z.t ->
  Yojson.Safe.t

val storage_assoc :
  ?limit:int ->
  (string * string) list ->
  Yojson.Safe.t

val storage_sizes :
  (string * string) list ->
  Yojson.Safe.t

val storage_value :
  key:string ->
  value:string ->
  limit:int ->
  Yojson.Safe.t

val storage_missing :
  key:string ->
  Yojson.Safe.t

val program_storage_dump :
  address:string ->
  (string * string) list ->
  Yojson.Safe.t

val program_bytecode :
  address:string ->
  bytecode_b64:string ->
  code_hash:string ->
  Yojson.Safe.t

val program_info :
  address:string ->
  version:string ->
  code_hash:string ->
  balance:string ->
  owner:string ->
  Yojson.Safe.t

val program_abi_saved :
  Yojson.Safe.t

val pvac_pubkey :
  addr:string ->
  string option ->
  Yojson.Safe.t

val pvac_already_registered :
  addr:string ->
  Yojson.Safe.t

val pvac_status :
  addr:string ->
  Octra_core.Pvac_registry.status ->
  Yojson.Safe.t

val pvac_migration_status :
  addr:string ->
  cipher:string ->
  epoch:int ->
  owner_migration_mode:Octra_core.Rule_graph.mode ->
  Octra_core.Pvac_migration.status ->
  Octra_core.Pvac_migration_entitlement.t ->
  Yojson.Safe.t

val encrypted_cipher :
  addr:string ->
  cipher:string ->
  Yojson.Safe.t

val encrypted_balance :
  addr:string ->
  cipher:string ->
  has_pvac_pubkey:bool ->
  Yojson.Safe.t

val public_key_registration_ignored_bft :
  addr:string ->
  Yojson.Safe.t

val public_key_registered :
  addr:string ->
  Yojson.Safe.t

val account_view_pubkey :
  addr:string ->
  view_pubkey:string option ->
  reason:string option ->
  Yojson.Safe.t

val stealth_outputs :
  from_epoch:int ->
  outputs:Yojson.Safe.t list ->
  Yojson.Safe.t

val stealth_outputs_page :
  from_epoch:int ->
  before_id:int64 option ->
  outputs:Yojson.Safe.t list ->
  next_before_id:int64 option ->
  has_more:bool ->
  scanned:int ->
  Yojson.Safe.t

val stealth_outputs_by_id :
  requested:int ->
  outputs:Yojson.Safe.t list ->
  Yojson.Safe.t

val compile_assembly :
  bytecode_b64:string ->
  bytecode_size:int ->
  instructions:int ->
  Yojson.Safe.t

val compile_aml :
  bytecode_b64:string ->
  bytecode_size:int ->
  instructions:int ->
  abi_json:string ->
  version:string ->
  disasm:string ->
  verification_json:string ->
  certificate_json:string ->
  Yojson.Safe.t

val program_address :
  address:string ->
  deployer:string ->
  nonce:int ->
  Yojson.Safe.t

val total_transactions :
  confirmed:int ->
  staging:int ->
  Yojson.Safe.t

val submit_accepted :
  tx_hash:string ->
  nonce:int ->
  ou_cost:Z.t ->
  Yojson.Safe.t

val private_transfer_pending :
  tx_hash:string ->
  Yojson.Safe.t

val submit_batch_decode_error :
  reason:string ->
  Yojson.Safe.t

val submit_batch_rejected :
  reason:string ->
  nonce:int ->
  Yojson.Safe.t

val submit_batch_accepted :
  tx_hash:string ->
  nonce:int ->
  Yojson.Safe.t

val submit_batch :
  Yojson.Safe.t list ->
  Yojson.Safe.t

val peer_diag :
  Octra_net.P2p_peer_diag.t option ->
  now:float ->
  Yojson.Safe.t * Yojson.Safe.t list

val consensus_peer :
  now:float ->
  Octra_consensus.C_driver.peer_state_record ->
  Yojson.Safe.t

val consensus_peer_states :
  enabled:bool ->
  peers:Yojson.Safe.t list ->
  scores:Yojson.Safe.t list ->
  diag:Yojson.Safe.t ->
  Yojson.Safe.t