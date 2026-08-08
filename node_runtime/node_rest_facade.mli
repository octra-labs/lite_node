(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Transaction = Octra_core.Transaction
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata
module Tree = Octra_core.Tree
module Wallet = Octra_core.Crypto.Wallet

type runtime = {
  swarm_ref : Octra_net.P2p_swarm.t option ref;
  preverify_admit : Transaction.t -> (unit, string) result;
  save_drops : Octra_core.Tx_staging.drop_record list -> unit;
  find_drop : string -> Octra_core.Tx_drop.row option;
  drops_by_addr :
    string ->
    limit:int ->
    offset:int ->
    Octra_core.Tx_drop.row list;
}

val run_s :
  'a Lwt.t ->
  'a

val notify_staging_update :
  ?total_txs:int ->
  ?total_ou:Z.t ->
  ?max_ou:Z.t ->
  unit ->
  unit

val sweep_low_fee_stealth :
  unit ->
  int

val add_tx_to_staging :
  ?relay:bool ->
  ?bft_mode:bool ->
  runtime ->
  Ledger.t ->
  Transaction.t ->
  (string, string) result

val max_timestamp_drift :
  float

val validate_and_submit_tx :
  runtime ->
  Ledger.t ->
  Transaction.t ->
  (string, string * string) result

val list_saved_epochs :
  Store_chaindata.t ->
  int list

val start :
  runtime ->
  port:int ->
  data_dir:string ->
  store:Store_irmin.t ->
  ledger:Ledger.t ->
  tree_ref:Tree.t ref ->
  wallet:Wallet.t ->
  chain_id:string ->
  consensus_config_hash_ref:string ref ->
  consensus_validator_set_ref:Octra_consensus.C_types.validator_set ref ->
  scheduled_validator_set_ref:Octra_consensus.C_config.scheduled option ref ->
  current_epoch:int ref ->
  total_tx_count:int ref ->
  validator_view_sk:string ->
  validator_view_pub:string ->
  program_trust:Octra_vm.Program_trust.t ->
  migration_entitlements:Octra_core.Pvac_migration_entitlement.t ->
  chaindata:Store_chaindata.t ->
  consensus_driver_ref:Octra_consensus.C_driver.t option ref ->
  epoch_visibility:Epoch_visibility.t ->
  resource_compute:Resource_compute_service.t ->
  unit Lwt.t

val start_task :
  runtime ->
  port:int ->
  data_dir:string ->
  store:Store_irmin.t ->
  ledger:Ledger.t ->
  tree_ref:Tree.t ref ->
  wallet:Wallet.t ->
  chain_id:string ->
  consensus_config_hash_ref:string ref ->
  consensus_validator_set_ref:Octra_consensus.C_types.validator_set ref ->
  scheduled_validator_set_ref:Octra_consensus.C_config.scheduled option ref ->
  current_epoch:int ref ->
  total_tx_count:int ref ->
  validator_view_sk:string ->
  validator_view_pub:string ->
  program_trust:Octra_vm.Program_trust.t ->
  migration_entitlements:Octra_core.Pvac_migration_entitlement.t ->
  chaindata:Store_chaindata.t ->
  consensus_driver_ref:Octra_consensus.C_driver.t option ref ->
  epoch_visibility:Epoch_visibility.t ->
  resource_compute:Resource_compute_service.t ->
  unit ->
  unit Lwt.t