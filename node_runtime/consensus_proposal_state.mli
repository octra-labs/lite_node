(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create : unit -> t
val set : t -> Octra_core.Transaction.t list -> string list -> unit
val reset_empty_received : t -> unit
val reset_unsynced : t -> unit
val mark_unsynced : t -> unit
val txs : t -> Octra_core.Transaction.t list
val tx_hashes : t -> string list
val received : t -> bool