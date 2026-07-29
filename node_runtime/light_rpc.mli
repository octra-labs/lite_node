(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type signer = {
  chain_id : string;
  validator_addr : string;
  validator_pubkey : string;
  validator_priv_b64 : string;
  config_hash : string;
}

val sign_head :
  signer ->
  Octra_core.Head_manifest.t ->
  (Octra_consensus.C_light_root.t, string) result

val signed_root :
  signer ->
  Octra_core.Head_manifest.t ->
  (Yojson.Safe.t, string) result

val account_proof_addr :
  Yojson.Safe.t ->
  (string, Octra_core.Rpc.rpc_error) result

val epoch_proof_id :
  Yojson.Safe.t ->
  (int, Octra_core.Rpc.rpc_error) result

val tx_inclusion_hash :
  Yojson.Safe.t ->
  (string, Octra_core.Rpc.rpc_error) result

val account_witness :
  chain_id:string ->
  addr:string ->
  Octra_core.Head_manifest.t ->
  string option ->
  (Yojson.Safe.t, string) result

val account_proof :
  signer ->
  store:Octra_core.Store_irmin.t ->
  addr:string ->
  Octra_core.Head_manifest.t ->
  (Yojson.Safe.t, string) result Lwt.t

val account_proof_params :
  signer ->
  store:Octra_core.Store_irmin.t ->
  head:Octra_core.Head_manifest.t option ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val epoch_proof_of_header :
  chain_id:string ->
  Octra_core.Epochlog.epoch_header ->
  string list ->
  (Octra_consensus.C_light_epoch.t, string) result

val collect_epoch_tx_hashes :
  (int64 -> (string * int * string) option) ->
  epoch_id:int ->
  start_txid:int64 ->
  tx_count:int ->
  string list option

val epoch_proof :
  chain_id:string ->
  Octra_core.Store_chaindata.t ->
  int ->
  (Octra_consensus.C_light_epoch.t, string) result

val epoch_proof_params :
  chain_id:string ->
  Octra_core.Store_chaindata.t ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val tx_inclusion_of_epoch :
  tx_hash:string ->
  Octra_consensus.C_light_epoch.t ->
  (Octra_consensus.C_light_epoch.tx_inclusion, string) result

val tx_inclusion_proof :
  chain_id:string ->
  Octra_core.Store_chaindata.t ->
  string ->
  (Octra_consensus.C_light_epoch.tx_inclusion * string, string) result

val tx_inclusion_proof_params :
  chain_id:string ->
  Octra_core.Store_chaindata.t ->
  Yojson.Safe.t ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t