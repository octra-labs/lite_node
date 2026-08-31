(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = {
  max_fhe : int;
  max_stealth : int;
}

type t

val consensus_id : string

val create :
  preverify:Preverify_commit.t option ->
  legacy_replay:
    (epoch:int ->
     address:string ->
     cipher:string ->
     Pvac_legacy_public_replay.decision) ->
  ledger:Ledger.t ->
  epoch_id:int ->
  owner_migration_mode:Rule_graph.mode ->
  proof_mode:Rule_graph.mode ->
  field_policy:Private_ledger.field_policy ->
  result_policy:Private_result_policy.t ->
  limits:limits ->
  t

val process :
  t ->
  backend:Epoch_exec.backend ->
  env:Epoch_exec.env ->
  Transaction.t ->
  (Z.t, string * string) result Lwt.t