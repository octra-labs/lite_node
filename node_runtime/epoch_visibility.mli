(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type 'a attempt =
  | Applied of 'a
  | Busy

val create :
  unit ->
  t

val begin_apply :
  t ->
  (unit, string) result

val finish_apply :
  t ->
  (unit, string) result

val try_apply :
  t ->
  (unit -> 'a Lwt.t) ->
  'a attempt Lwt.t

val read :
  ?retries:int ->
  t ->
  (unit -> 'a Lwt.t) ->
  'a option Lwt.t

val is_applying :
  t ->
  bool