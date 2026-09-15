(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type admission =
  | Accepted
  | Busy
  | Stopped

type action =
  | Pulse
  | Appeal of Octra_core.Set_fold.proof

type sample = {
  epoch : int64;
  active : bool;
  bonded : bool;
}

type point = { epoch : int64; head : int64 option; finalized : bool }

type refusal = Moved | Uncommitted | Finalized

type stats = {
  queued : int;
  appeals : int;
  generation : int64;
  sent : int64;
}

type deps = {
  sample : unit -> sample;
  peers : unit -> int;
  send : epoch:int64 -> action -> (unit, string) result Lwt.t;
  warn : string -> unit;
}

type t

val plan : epoch:int64 -> point -> (int64, refusal) result
val reason : refusal -> string
val stream_capacity : int
val appeal_capacity : int
val notify :
  t ->
  epoch:int64 ->
  (Octra_consensus.C_types.vote * Octra_consensus.C_types.parent_commit) option ->
  admission
val wake : t -> head:int -> admission
val create : deps -> t
val stats : t -> stats Lwt.t
val shutdown : t -> unit Lwt.t