(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : unit -> t
val start : t -> delay:float -> duration:float -> unit
val window : unit -> (float * float) option

val run :
  t ->
  relevant:(unit -> bool) ->
  (unit -> 'a option Lwt.t) ->
  'a option Lwt.t