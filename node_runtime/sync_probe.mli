(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type token = { id : int; generation : int }
type job = { token : token; deadline : float; stopping : bool }
type phase = Open | Closing | Closed
type 'a state = {
  phase : phase;
  generation : int;
  issued : int;
  active : job option;
  cached : (float * 'a) option;
  next_at : float;
  readers : (int * float) list;
}
type 'a message =
  | Ask of int * float * bool
  | Cancel of int
  | Tick of float
  | Completed of token * float * bool * 'a option
  | Stop
type 'a effect = Reply of int * 'a option | Run of token | Interrupt of token

val empty : 'a state
val delta : 'a state -> 'a message -> 'a state * 'a effect list

type 'a t
val monotonic : unit -> float
val create : ?clock:(unit -> float) -> valid:('a -> bool) -> (unit -> 'a Lwt.t) -> 'a t
val query : 'a t -> 'a option Lwt.t
val close : 'a t -> bool Lwt.t
val read_child : (unit -> string) -> string Lwt.t