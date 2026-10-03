(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val previous :
  chain_id:string -> epoch:int64 ->
  lookup:(string -> Octra_core.Transaction.t list option) ->
  Octra_consensus.C_types.parent_commit option -> bool option

val allow :
  epoch:int64 -> previous:bool option ->
  Octra_core.Transaction.t list -> bool

val select :
  ordered:bool ->
  epoch:int64 ->
  previous:bool option ->
  ordinary:Octra_core.Transaction.t list ->
  Octra_core.Transaction.t list ->
  Octra_core.Transaction.t list