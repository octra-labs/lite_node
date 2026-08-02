(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Wallet = Octra_core.Crypto.Wallet
module Ledger = Octra_core.Ledger
module Store_irmin = Octra_core.Store_irmin
module Store_chaindata = Octra_core.Store_chaindata
module Tree = Octra_core.Tree

type deps = {
  validate : Submit_rpc.validate;
  encrypted_supply : unit -> Z.t;
  notify_staging_update : unit -> unit;
  bft_mode : unit -> bool;
  account_path_profile_enabled : bool;
  swarm : unit -> Octra_net.P2p_swarm.t option;
  find_drop : string -> Octra_core.Tx_drop.row option;
  drops_by_addr :
    string ->
    limit:int ->
    offset:int ->
    Octra_core.Tx_drop.row list;
}

type config = {
  port : int;
  data_dir : string;
  store : Store_irmin.t;
  ledger : Ledger.t;
  tree_ref : Tree.t ref;
  wallet : Wallet.t;
  chain_id : string;
  consensus_config_hash_ref : string ref;
  consensus_validator_set_ref : Octra_consensus.C_types.validator_set ref;
  scheduled_validator_set_ref : Octra_consensus.C_config.scheduled option ref;
  current_epoch : int ref;
  total_tx_count : int ref;
  validator_view_sk : string;
  validator_view_pub : string;
  program_trust : Octra_vm.Program_trust.t;
  migration_entitlements : Octra_core.Pvac_migration_entitlement.t;
  chaindata : Store_chaindata.t;
  consensus_driver_ref : Octra_consensus.C_driver.t option ref;
  epoch_visibility : Epoch_visibility.t;
  deps : deps;
}

val run_s :
  'a Lwt.t ->
  'a

val start :
  config ->
  unit Lwt.t