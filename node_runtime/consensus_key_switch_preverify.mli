(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val create :
  Octra_core.Ledger.t ->
  t

val admit :
  t ->
  Octra_core.Transaction.t ->
  Octra_core.Private_ledger.prepared Octra_core.Preverify_availability.t

val observe :
  t ->
  Octra_core.Transaction.t ->
  Octra_core.Private_ledger.prepared Octra_core.Preverify_availability.t Lwt.t

val await :
  t ->
  Octra_core.Transaction.t ->
  Octra_core.Private_ledger.prepared Octra_core.Preverify_availability.t Lwt.t

val retain :
  t ->
  (string -> bool) ->
  unit

val stats :
  t ->
  Consensus_preverify_pool.stats