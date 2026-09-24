(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'a item = {
  key : string;
  value : 'a;
  expires : float;
}

type 'a state = {
  generation : int;
  serial : int;
  active : (int * 'a item) option;
  queue : 'a item list;
  seen : string list;
  closed : bool;
}

type outcome = Sent | Expired | Failed of string | Cancelled

type 'a message =
  | Offer of int * float * 'a item
  | Finished of int * float * outcome
  | Advance of int * float
  | Close
  | Open of int

type 'a effect =
  | Send of int * 'a item
  | Cancel
  | Dropped of string

val capacity : int
val lifetime : float
val empty : 'a state
val step : 'a state -> 'a message -> 'a state * 'a effect list

type 'a t

val create :
  now:(unit -> float) ->
  send:('a -> unit Lwt.t) ->
  wait:(float -> unit Lwt.t) ->
  warn:(string -> unit) ->
  'a t
val offer : 'a t -> generation:int -> key:string -> 'a -> unit
val progress : 'a t -> generation:int -> unit
val close : 'a t -> unit
val open_ : 'a t -> generation:int -> unit