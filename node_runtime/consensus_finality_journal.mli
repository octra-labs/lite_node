(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type bundle = {
  tx_hashes : string list;
  txs : Octra_core.Transaction.t list;
  receipts_json : string list;
}

type record = {
  finalize : Octra_consensus.C_types.finalize;
  validator_set : Octra_consensus.C_types.validator_set;
  bundle : bundle option;
}

type read_result =
  | Missing
  | Valid of record
  | Invalid of string

val history_limit : int64

val persist_certificate :
  string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize ->
  unit

val persist_bundle :
  string ->
  Octra_consensus.C_types.finalize ->
  bundle ->
  unit

val read_validated :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  string ->
  read_result

val read_committed_validated :
  chain_id:string ->
  entry:Octra_consensus.Finality_log.entry ->
  string ->
  read_result

val read_committed_epoch :
  chain_id:string ->
  epoch:int64 ->
  string ->
  read_result

val read_committed_epoch_validated :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  epoch:int64 ->
  string ->
  read_result

val read_history_epoch_validated :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  epoch:int64 ->
  string ->
  read_result

val replayable :
  record ->
  (record, string) result

val read_replay_backlog :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  head_epoch:int64 ->
  head_root:string ->
  string ->
  (record list, string) result

val committed_for :
  chain_id:string ->
  entry:Octra_consensus.Finality_log.entry ->
  string ->
  (unit, string) result

val rewind_committed :
  chain_id:string ->
  entry:Octra_consensus.Finality_log.entry ->
  string ->
  (unit, string) result

val restore_committed_from_history :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  epoch:int64 ->
  string ->
  (record, string) result

val remove :
  string ->
  unit

val pending :
  string ->
  bool

val promote :
  string ->
  unit

val promote_applied :
  string ->
  epoch:int64 ->
  state_root:string ->
  unit

val committed :
  string ->
  bool