(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type failure = Retry of string | Wait of string | Refused of string

val http_failure : int -> failure
val rpc_failure : Yojson.Safe.t -> failure

type deps = {
  now : unit -> float;
  wait : float -> unit Lwt.t;
  staged : string -> bool;
  landed : Octra_core.Transaction.t -> bool;
  post : Octra_core.Transaction.t -> (unit, failure) result Lwt.t;
  warn : string -> unit;
}

type eligibility = Eligible | Paused | Expired

type retry = {
  eligible : Octra_core.Transaction.t -> eligibility;
  post : current:(unit -> bool) -> Octra_core.Transaction.t -> (unit, failure) result Lwt.t;
  retain : bool;
}

type t

val period : float
val create : deps -> t
val put : ?retry:retry -> t -> hash:string -> Octra_core.Transaction.t -> unit
val pending : t -> bool
val tick : t -> unit
val stop : t -> unit