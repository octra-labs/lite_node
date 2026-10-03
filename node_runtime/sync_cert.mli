(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type trust = {
  validators : Octra_consensus.C_types.validator_set;
  exporters : Octra_consensus.C_types.validator_set;
  chain : string;
  config : string;
}

type bytes = {
  raw : string;
  digest : string;
}

type query = {
  path : string;
  trust : trust;
  trust_hash : string;
}

type loaded = {
  raw : string;
  certificate : Octra_bootstrap.State_sync_manifest.certificate;
}

type error = Busy | Expired | Stopped | Invalid of string
type reply = (loaded, error) result
type listener = Waiting | Timed_out | Cancelled

type job = {
  id : int64;
  generation : int64;
  query : query;
  deadline : float;
  listener : listener;
}

type state

type message =
  | Ask of int64 * query * float
  | Read of int64 * int64 * float * (bytes, string) result
  | Checked of int64 * int64 * float * (Octra_bootstrap.State_sync_manifest.certificate, string) result
  | Parsed of int64 * int64 * float * (Octra_bootstrap.State_sync_manifest.certificate, string) result
  | Cancel of int64
  | Tick of float
  | Stop

type effect =
  | Read_file of job
  | Verify of job * string
  | Parse of job * string
  | Interrupt of int64
  | Reply of int64 * reply

type stats = {
  active : int;
  queued : int;
  cached : int;
  generation : int64;
}

val capacity : int
val cache_capacity : int
val lifetime : float
val negative_seconds : float
val empty : state
val delta : state -> message -> state * effect list
val stats : state -> stats
val trust_hash : trust -> string
val reason : error -> string
val retryable : error -> bool

type deps = {
  now : unit -> float;
  read : string -> (string, string) result Lwt.t;
  verify : cancelled:(unit -> bool) -> trust -> string -> (Octra_bootstrap.State_sync_manifest.certificate, string) result Lwt.t;
}

type t

val create : deps -> t
val load : t -> path:string -> trust -> reply Lwt.t
val shutdown : t -> unit Lwt.t