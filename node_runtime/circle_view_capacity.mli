(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : limit:int -> t
val active : t -> int
val with_slot :
  t ->
  busy:(unit -> 'a Lwt.t) ->
  (unit -> 'a Lwt.t) ->
  'a Lwt.t