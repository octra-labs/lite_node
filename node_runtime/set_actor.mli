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

type stats = {
  queued : int;
  appeals : int;
  generation : int64;
  sent : int64;
}

type deps = {
  sample : unit -> sample;
  send : action -> (unit, string) result Lwt.t;
  warn : string -> unit;
}

type t

val stream_capacity : int
val appeal_capacity : int
val notify :
  t ->
  epoch:int64 ->
  (Octra_consensus.C_types.vote * Octra_consensus.C_types.parent_commit) option ->
  admission
val create : deps -> t
val stats : t -> stats Lwt.t
val shutdown : t -> unit Lwt.t