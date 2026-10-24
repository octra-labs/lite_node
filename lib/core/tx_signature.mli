(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Historical | Network of string
val at_plan : chain_id:string -> activation_epoch:int64 option -> epoch:int64 -> t
val at : chain_id:string -> epoch:int64 -> t
val bytes : t -> Transaction.t -> string
val verify : t -> Transaction.t -> string -> bool
val sign : t -> Transaction.t -> string -> Transaction.t
val check : t -> Transaction.t -> (unit, string) result
val check_active : t -> Transaction.t list -> (unit, string) result
val consensus_id : string