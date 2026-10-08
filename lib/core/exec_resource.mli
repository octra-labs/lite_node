(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type resource = Memory | Stack | Host

exception Unavailable of resource
exception Exhausted of string * resource

val protect : ('a -> 'b) -> 'a -> 'b
val bind : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val catch : (unit -> 'a Lwt.t) -> (exn -> 'a Lwt.t) -> 'a Lwt.t
val detach : ('a -> 'b) -> 'a -> 'b Lwt.t
val run : hash:string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val name : resource -> string