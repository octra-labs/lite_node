(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

exception Close_failed of string

val create : release:(unit -> unit) -> t
val acquire : t -> (unit -> 'a) -> ('a -> unit) -> 'a
val guard : t -> (unit -> 'a) -> 'a
val close : t -> unit
val protect : close:(unit -> unit) -> (unit -> 'a) -> 'a