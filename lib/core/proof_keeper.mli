(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type token = {generation : int; id : int}
type recovery = Proof_warm.recovery_ticket
type outcome = Accepted of Pvac_verify_protocol.response | Recheck of token option
  | Recovery of recovery | Cancelled | Timed_out
type t
type stats = {parent : int option; accepted : int; prepared : int; rechecked : int; reused : int; refused : int}

val reserve_mb : int
val min_memory_mb : int
val waiting_result : ?retiring:bool -> Proof_warm.phase -> control:Proof_link.control -> now:float -> deadline:float -> outcome option
val create : ?now:(unit -> float) -> memory:int -> string -> t
val request_stop : t -> unit
val stop : t -> unit
val phase : t -> Proof_warm.phase
val stats : t -> stats
val run : t -> control:Proof_link.control Atomic.t -> deadline:float -> Pvac_verify_protocol.request -> outcome
val settle : t -> token -> Proof_warm.confirmation -> unit
val recover : t -> recovery -> Proof_warm.recovery_result -> unit
val current : string -> t option
val footprint : unit -> int
val processes : unit -> int list
val shared : memory:int -> string -> t option