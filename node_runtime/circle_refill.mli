(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val without : Octra_core.Transaction.t list -> Octra_core.Transaction.t list

val first : Octra_core.Transaction.t list -> Octra_core.Transaction.t list

val before :
  excluded:Octra_core.Transaction.t list ->
  Octra_core.Transaction.t list -> Octra_core.Transaction.t list

val through :
  rejected:Octra_core.Transaction.t list ->
  Octra_core.Transaction.t list -> Octra_core.Transaction.t list

val select :
  selected:Octra_core.Transaction.t list ->
  confirmed:Octra_core.Transaction.t list ->
  rejected:int -> Octra_core.Transaction.t list ->
  Octra_core.Transaction.t list option

val work :
  selected:Octra_core.Transaction.t list ->
  rejected:Octra_core.Transaction.t list ->
  Octra_core.Transaction.t list -> Octra_core.Transaction.t list option