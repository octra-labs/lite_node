(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type token = { id : int; generation : int }
type job = { token : token; started : float; deadline : float; background : bool; stopping : bool }
type phase = Open | Closing | Closed
type 'a state = {
  phase : phase;
  generation : int;
  issued : int;
  active : job option;
  cached : (float * 'a) option;
  sample : (float * 'a) option;
  attempted : bool;
  next_at : float;
  readers : (int * float) list;
}
type 'a message =
  | Ask of int * float * bool
  | Refresh of float
  | Invalidate
  | Cancel of int
  | Tick of float
  | Completed of token * float * bool * 'a option
  | Stop
type 'a effect = Reply of int * 'a option | Run of token | Interrupt of token

val empty : 'a state
val delta : 'a state -> 'a message -> 'a state * 'a effect list

type 'a t
val monotonic : unit -> float
val create : ?clock:(unit -> float) -> ?retry:('a -> bool) ->
  valid:('a -> bool) -> (background:bool -> deadline:float -> 'a Lwt.t) -> 'a t
val query : 'a t -> 'a option Lwt.t
type 'a snapshot = Starting | Read_failed | Expired of float | Snapshot of float * 'a
val snapshot : 'a t -> 'a snapshot
val start : 'a t -> unit
val close : 'a t -> bool Lwt.t
val child : int -> (unit -> string) -> 'a
val read_child : mode:string -> string list -> string Lwt.t