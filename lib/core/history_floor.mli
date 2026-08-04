(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create :
  chain_id:string ->
  epoch:int ->
  state_root:string ->
  ledger_state_root:string ->
  txid_hi:int64 ->
  config_hash:string ->
  validator_set_hash:string ->
  epoch_index_hash:string ->
  epoch_index_root:string ->
  parent_commit:Octra_consensus.C_types.parent_commit ->
  (t, string) result

val chain_id : t -> string
val epoch : t -> int
val state_root : t -> string
val ledger_state_root : t -> string
val txid_hi : t -> int64
val next_txid : t -> int64
val config_hash : t -> string
val validator_set_hash : t -> string
val epoch_index_hash : t -> string
val epoch_index_root : t -> string

val parent_commit :
  t ->
  Octra_consensus.C_types.parent_commit

val to_string : t -> string
val of_string : string -> (t, string) result