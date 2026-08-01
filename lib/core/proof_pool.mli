(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type priority = Compute_pool.priority =
  | Required
  | Speculative

val capacity : int

val capacity_of_getenv : (string -> string option) -> int

val try_run :
  ?priority:priority ->
  (unit -> 'a) ->
  'a option Lwt.t

val run :
  ?priority:priority ->
  (unit -> 'a) ->
  'a Lwt.t