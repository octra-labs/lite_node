(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val disk : data_dir:string -> t

val memory : unit -> t

val keep :
  t ->
  verify:(C_codec.round_sync -> bool) ->
  C_codec.round_sync ->
  (C_codec.round_sync, string) result

val load :
  t ->
  chain_id:string ->
  validator:string ->
  epoch_id:int64 ->
  verify:(C_codec.round_sync -> bool) ->
  (C_codec.round_sync list, string) result

val prune :
  t ->
  through_epoch:int64 ->
  (unit, string) result