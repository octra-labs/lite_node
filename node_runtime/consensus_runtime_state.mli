(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type clear_result =
  | Cleared
  | Inactive
  | Refused of string

val create : unit -> t
val state_attested : t -> bool
val quarantine_active : t -> bool
val quarantine_active_ref : t -> bool ref
val quarantine_reason : t -> string
val quarantine_since_epoch : t -> int
val prev_root_streak : t -> int
val set_prev_root_streak : t -> int -> unit
val state_root_streak : t -> int
val set_state_root_streak : t -> int -> unit
val ahead_streak : t -> int
val incr_ahead_streak : t -> unit
val clear_state_attested : t -> unit
val set_state_attested : t -> head:int -> root:string -> unit
val attested_head : t -> int -> bool
val root_attestation_recovers : string -> bool
val enter_quarantine : t -> epoch:int -> reason:string -> bool
val clear_quarantine : t -> evidence:string -> clear_result