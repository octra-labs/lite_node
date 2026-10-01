(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val normalize :
  sender_pk:string option ->
  Transaction.t ->
  (Transaction.t, string * string) result

val consensus_id : string

val check_epoch :
  chain_id:string -> epoch:int64 -> Transaction.t list -> (unit, string) result

val check_rule :
  Rule_graph.t -> epoch:int -> Transaction.t list -> (unit, string) result

val check_outcome :
  chain_id:string -> epoch:int64 -> receipts:string list ->
  Transaction.t list -> (unit, string) result