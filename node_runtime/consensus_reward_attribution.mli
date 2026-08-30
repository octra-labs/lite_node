(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Octra_core.Epoch_exec.reward_attribution = {
  proposer_addr : string;
  proposer_public_key : string option;
  validators : Octra_core.Epoch_exec.reward_validator list;
}

val full_set :
  proposer_addr:string ->
  validator_pubkeys:(string * string) list ->
  t

val resolve_for_epoch :
  epoch_id:int64 ->
  proposer_addr:string ->
  validator_pubkeys:(string * string) list ->
  Octra_consensus.C_types.parent_commit option ->
  (t, string) result

val of_parent_commit :
  Octra_consensus.C_types.parent_commit ->
  (t, string) result

val to_source :
  t ->
  (Octra_consensus.C_types.reward_source, string) result

val of_source :
  Octra_consensus.C_types.reward_source ->
  (t, string) result

val bind_finality :
  validator_set:Octra_consensus.C_types.validator_set ->
  Octra_consensus.C_types.finalize ->
  t ->
  (t, string) result

val epoch_source :
  validator_activation_epoch:int option ->
  validator_pubkeys:(string * string) list ->
  Octra_core.Epochlog.epoch_header ->
  (Octra_consensus.C_types.reward_source, string) result