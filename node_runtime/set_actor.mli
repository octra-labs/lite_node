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
  bonded : (bool, string) result;
}

type point = { epoch : int64; head : int64 option; finalized : bool }

type refusal = Moved | Uncommitted | Finalized

type fault = Control | Receipt | Transport | Send | Internal

type stage = Available | Attempt | Marked | Expired | Unread | Capacity | Overload | Closed

type observation = {
  stage : stage;
  epoch : int64;
  proof : Octra_core.Set_fold.proof;
}

val stage_name : stage -> string
val proof_key : Octra_core.Set_fold.proof -> string

val event : fault -> string

type stats = {
  queued : int;
  appeals : int;
  generation : int64;
  sent : int64;
}

type deps = {
  sample : unit -> sample;
  read : epoch:int64 -> (Octra_core.Set_fold.receipt, string) result Lwt.t;
  peers : unit -> int;
  send : epoch:int64 -> action -> (unit, fault * string) result Lwt.t;
  warn : fault -> string -> unit;
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
val create : ?observe:(observation -> unit) -> deps -> t
val stats : t -> stats Lwt.t
val shutdown : t -> unit Lwt.t