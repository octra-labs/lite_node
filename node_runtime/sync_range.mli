(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type query = {
  from_epoch : int64;
  max_epochs : int;
  part : int option;
  hash : string option;
  head : Octra_core.Head_manifest.t option;
  pubkeys : (string * string) list;
  activation : int option;
}

type loaded = {
  body : string;
  status : string;
  records : int;
  encoded : Octra_bootstrap.Range_part.encoded option;
}
type error = Busy | Expired | Stopped | Changed | Missing | Invalid of string
type reply = (loaded, error) result
type listener = Waiting | Released
type cache

type job = {
  id : int64;
  generation : int64;
  query : query;
  deadline : float;
  listener : listener;
  cached : cache option;
}

type state
type message =
  | Ask of int64 * query * float
  | Completed of int64 * int64 * float * reply
  | Cancel of int64
  | Tick of float
  | Stop

type effect = Run of job | Interrupt of int64 | Reply of int64 * reply

val capacity : int
val lifetime : float
val retention : float
val empty : state
val delta : state -> message -> state * effect list
val pending : state -> int

type deps = {
  now : unit -> float;
  read : cancelled:(unit -> bool) -> query -> reply Lwt.t;
}

type t
val create : deps -> t
val load : t -> query -> reply Lwt.t
val shutdown : t -> unit Lwt.t