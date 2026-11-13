(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type entry = {
  address : string;
  pubkey : string;
  bond : Z.t;
  bonded_epoch : int64;
  ready_epoch : int64 option;
  exit_epoch : int64 option;
}

type member = {
  address : string;
  pubkey : string;
  weight : Z.t;
}

type parameters = {
  min_bond : Z.t;
  max_validators : int;
  activation_delay : int64;
  unbonding_epochs : int64;
}

type snapshot = {
  source_epoch : int64;
  activate_epoch : int64;
  validators : member list;
  total_weight : Z.t;
  quorum_weight : Z.t;
  fingerprint : string;
}

val validate_parameters : parameters -> (unit, string) result
val validate_entry : entry -> (unit, string) result
val quorum_weight : Z.t -> (Z.t, string) result
val select_members :
  ?incumbents:string list -> parameters -> source_epoch:int64 -> entry list -> member list
val check_weight_change : before:member list -> after:member list -> (unit, string) result
val snapshot :
  ?incumbents:string list ->
  parameters ->
  activate_epoch:int64 ->
  entry list ->
  (snapshot, string) result
val leader :
  snapshot ->
  seed:string ->
  epoch_id:int64 ->
  round:int ->
  (member, string) result
val withdraw_epoch :
  parameters ->
  entry ->
  (int64, string) result
val can_withdraw :
  parameters ->
  current_epoch:int64 ->
  entry ->
  (bool, string) result