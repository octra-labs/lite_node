(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type queued = {
  target_epoch : int64;
  reason : string;
}

type snapshot = {
  target_epoch : int64 option;
  reason : string;
  gap_active : bool;
}

val create : unit -> t

val snapshot : t -> snapshot

val target : t -> int64 option

val gap_active : t -> bool

val activate_gap : t -> unit

val deactivate_gap : t -> unit

val clear_all : t -> unit

val queue :
  t ->
  target_epoch:int64 ->
  reason:string ->
  snapshot

val queue_gap :
  t ->
  target_epoch:int64 ->
  reason:string ->
  snapshot

val take_if_after :
  t ->
  head:int64 ->
  queued option

val target_label :
  snapshot ->
  string