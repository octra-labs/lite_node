(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type usage = {regular : int; duty : int}
type limits

val zero : usage
val limits : regular:int -> sender:int -> duty:int -> duty_sender:int -> (limits, string) result
val of_env : (string -> string option) -> (limits, string) result
val measure : Transaction.t -> (int, string) result
val of_tx : Transaction.t -> int -> usage
val add : usage -> usage -> usage
val remove : usage -> usage -> usage
val plan : limits -> total:usage -> sender:usage -> removed:usage -> added:usage -> (unit, string) result