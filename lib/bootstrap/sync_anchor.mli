(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type source =
  | Pending
  | Active

type step = {
  source : source;
  finalize : Octra_consensus.C_types.finalize;
  ledger_state_root : string;
  epoch_index_root : string;
  update : string;
  proof : string;
}

type t

val make :
  steps:step list ->
  finalize:Octra_consensus.C_types.finalize ->
  validator_set:Octra_consensus.C_types.validator_set ->
  t

val steps : t -> step list

val validator_set :
  t ->
  Octra_consensus.C_types.validator_set

val encode : t -> string
val decode : string -> (t, string) result

val derive :
  validator_set:Octra_consensus.C_types.validator_set ->
  t ->
  (Octra_consensus.C_types.validator_set, string) result

val raw_validator_set :
  Octra_consensus.C_types.validator_set ->
  (Octra_consensus.C_types.validator_set, string) result

val encoded_validator_set :
  Octra_consensus.C_types.validator_set ->
  (Octra_consensus.C_types.validator_set, string) result

val finalize_hash : Octra_consensus.C_types.finalize -> string

val verify_finalize :
  chain_id:string ->
  validator_set:Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize ->
  (unit, string) result

val verify_checkpoint :
  State_sync_checkpoint.body ->
  t ->
  (unit, string) result

val verify :
  validator_set:Octra_consensus.C_types.validator_set ->
  State_sync_checkpoint.body ->
  string ->
  (t, string) result