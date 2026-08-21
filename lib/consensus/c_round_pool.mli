(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : unit -> t

val clear : t -> unit

val add :
  t ->
  C_codec.round_sync ->
  unit

val add_at :
  t ->
  current:int ->
  C_codec.round_sync ->
  unit

val add_local :
  t ->
  C_codec.round_sync ->
  unit

val reply :
  t ->
  epoch_id:int64 ->
  validator:string ->
  (C_codec.round_sync * float) option

val sent_reply :
  t ->
  epoch_id:int64 ->
  after_round:int ->
  through_round:int ->
  C_codec.round_sync option

val sent_range :
  t ->
  epoch_id:int64 ->
  after_round:int ->
  through_round:int ->
  C_codec.round_sync list

val witness :
  t ->
  chain_id:string ->
  epoch_id:int64 ->
  after_round:int ->
  through_round:int ->
  validator_set:C_types.validator_set ->
  C_codec.round_sync list