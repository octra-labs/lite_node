(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val disk : data_dir:string -> t
val memory : unit -> t

val floor :
  t ->
  (int64 option, string) result

val set_floor :
  t ->
  through_epoch:int64 ->
  (unit, string) result

val round_mark :
  t ->
  ((int64 * int) option, string) result

val set_round_mark :
  t ->
  epoch_id:int64 ->
  round:int ->
  (unit, string) result

val check :
  t ->
  C_types.vote ->
  (unit, string) result

val keep :
  t ->
  C_types.vote ->
  (C_types.vote, string) result

val max_round :
  t ->
  chain_id:string ->
  validator:string ->
  epoch_id:int64 ->
  (int option, string) result

val prune :
  t ->
  through_epoch:int64 ->
  (unit, string) result