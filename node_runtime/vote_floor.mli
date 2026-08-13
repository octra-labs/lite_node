(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val check :
  data_dir:string ->
  chain_id:string ->
  validator:string ->
  pubkey:string ->
  through_epoch:int64 ->
  round:int ->
  (int, string) result

val set :
  data_dir:string ->
  chain_id:string ->
  validator:string ->
  pubkey:string ->
  through_epoch:int64 ->
  round:int ->
  (int, string) result

val check_sync :
  data_dir:string ->
  chain_id:string ->
  validator:string ->
  pubkey:string ->
  through_epoch:int64 ->
  round_min:int ->
  sync:string ->
  (int, string) result

val set_sync :
  data_dir:string ->
  chain_id:string ->
  validator:string ->
  pubkey:string ->
  through_epoch:int64 ->
  round_min:int ->
  sync:string ->
  (int, string) result