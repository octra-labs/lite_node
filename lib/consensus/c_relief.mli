(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mark = {
  height : int64;
  round : int;
  activate_epoch : int64;
  source_hash : string;
  target_hash : string;
  fingerprint : string;
  proof : C_codec.round_sync list;
}

type decision =
  | Wait
  | Refuse of string
  | Apply of mark

val activation_epoch : int64
val min_round : int

val same_plan : mark -> mark -> bool

val decide :
  chain_id:string ->
  height:int64 ->
  current:C_types.validator_set ->
  activate_epoch:int64 ->
  target:C_types.validator_set ->
  fingerprint:string ->
  proof:C_codec.round_sync list ->
  decision

val restore :
  chain_id:string ->
  height:int64 ->
  current:C_types.validator_set ->
  activate_epoch:int64 ->
  target:C_types.validator_set ->
  fingerprint:string ->
  mark ->
  (C_types.validator_set option, string) result