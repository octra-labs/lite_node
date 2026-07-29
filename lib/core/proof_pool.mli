(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val capacity : int

val run :
  (unit -> 'a) ->
  'a Lwt.t