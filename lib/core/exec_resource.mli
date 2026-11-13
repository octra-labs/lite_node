(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type resource = Memory | Stack | Host | Time
type allowance = Operation of float | Completion of float

exception Unavailable of resource
exception Exhausted of string * resource
exception Window_expired of string

val select : ?limit:float -> ?scale:(string -> float -> allowance) ->
  ?record:(string -> float -> unit) ->
  ?clock:(unit -> float) -> running:(unit -> bool) ->
  (unit -> 'a Lwt.t) -> 'a Lwt.t
val work : (unit -> 'a) -> 'a
val interrupt : ?charge:bool -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val queued : ((unit -> unit) -> 'a Lwt.t) -> 'a Lwt.t
val preview : ?replay:(string -> bool) -> ?allow:(string -> bool) ->
  ?omit:(string -> unit) ->
  accept:(unit -> bool) -> defer:(string -> resource -> unit) ->
  (unit -> 'a Lwt.t) -> ('a * bool) Lwt.t
val next : ?hash:string -> unit -> bool
val defer : string -> resource -> bool
val omit : string -> bool

val protect : ('a -> 'b) -> 'a -> 'b
val bind : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val catch : (unit -> 'a Lwt.t) -> (exn -> 'a Lwt.t) -> 'a Lwt.t
val detach : ?launch:(('a -> ('b, exn) result * float) ->
  'a -> (('b, exn) result * float) Lwt.t) -> ('a -> 'b) -> 'a -> 'b Lwt.t
val joined : ?launch:(('a -> ('b, exn) result * float) ->
  'a -> (('b, exn) result * float) Lwt.t) -> ('a -> 'b) -> 'a -> 'b Lwt.t
val run : hash:string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
val name : resource -> string
val await : ?steps:int -> 'a Lwt.t -> 'a