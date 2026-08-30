(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'a step =
  | Next of 'a
  | Stop

val run : ('a -> 'a step Lwt.t) -> 'a -> unit Lwt.t