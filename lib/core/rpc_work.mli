(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type state = private {next : int; active : int list}
type message = Start | Finish of int
type effect = Enter of int | Busy | Leave

val capacity : int
val initial : state
val step : state -> message -> state * effect
val run : busy:(unit -> 'a Lwt.t) -> (unit -> 'a Lwt.t) -> 'a Lwt.t