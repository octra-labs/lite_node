(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type total = private {
  address : string;
  first_epoch : int;
  last_epoch : int;
  state_root : string;
  source_cipher_hash : string;
  sum : Octra_core.Claim_sum.summary;
}

val total :
  t -> Octra_core.Store_irmin.t -> Octra_core.Store_chaindata.t ->
  address:string -> first_epoch:int -> last_epoch:int ->
  max_txs:int -> max_records:int -> (total, string) result

val read :
  chain_id:string ->
  config_hash:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  exporter_set:Octra_consensus.C_types.validator_set ->
  certificate:State_sync_manifest.certificate ->
  first_epoch:int ->
  max_epochs:int ->
  read_finality:(int64 -> (Octra_consensus.C_types.finalize, string) result) ->
  (t, string) result

val math : t -> epoch:int -> (bool, string) result

val verify :
  t ->
  Octra_core.Store_irmin.t ->
  Octra_core.Store_chaindata.t ->
  send_epoch:int ->
  claim_epoch:int ->
  send_index:int64 ->
  claim_index:int64 ->
  max_txs:int ->
  (Octra_core.Claim_history.checked, string) result