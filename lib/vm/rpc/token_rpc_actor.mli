(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type error =
  | Busy
  | Stopped
  | Read_failed

type stats = {
  cache_entries : int;
  cache_bytes : int;
  queued : int;
  generation : int64;
}

val cache_byte_limit : int
val cache_entry_limit : int
val data_channel_limit : int

val data_admitted :
  queued:int ->
  bool

val create :
  store:Octra_core.Store_irmin.t ->
  unit ->
  t

val query :
  t ->
  holder:string ->
  offset:int ->
  limit:int ->
  (Yojson.Safe.t, error) result Lwt.t

val stats :
  t ->
  stats Lwt.t

val shutdown :
  t ->
  unit Lwt.t