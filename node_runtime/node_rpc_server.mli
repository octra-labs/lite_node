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
}

type config = {
  port : int;
  data_dir : string;
  store : Store_irmin.t;
  ledger : Ledger.t;
  tree_ref : Tree.t ref;
  wallet : Wallet.t;
  chain_id : string;
  consensus_config_hash : string;
  consensus_validator_set : Octra_consensus.C_types.validator_set;
  scheduled_validator_set : Octra_consensus.C_config.scheduled option;
  current_epoch : int ref;
  total_tx_count : int ref;
  validator_view_sk : string;
  validator_view_pub : string;
  chaindata : Store_chaindata.t;
  consensus_driver_ref : Octra_consensus.C_driver.t option ref;
  deps : deps;
}

val run_s :
  'a Lwt.t ->
  'a

val start :
  config ->
  unit Lwt.t