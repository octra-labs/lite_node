(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : unit -> t

val run :
  t ->
  relevant:(unit -> bool) ->
  (unit -> 'a option Lwt.t) ->
  'a option Lwt.t