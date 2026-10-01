(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type resource = Memory | Stack

exception Unavailable of resource
exception Exhausted of string * resource

val protect : ('a -> 'b) -> 'a -> 'b
val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
val run : hash:string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val name : resource -> string