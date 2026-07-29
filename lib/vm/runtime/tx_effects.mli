(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

exception Commit_failed of string

val create :
  ledger:Octra_core.Ledger.t ->
  store:Octra_core.Store_irmin.t ->
  t

val value : t -> Value_journal.t
val program : t -> Program_journal.t
val balance : t -> string -> Z.t
val debit : t -> string -> Z.t -> int -> (unit, string) result
val apply : t -> Call_plan.value_effect -> bool
val ensure_account : t -> string -> (unit, string) result
val commit : t -> (unit, string) result
val commit_exn : t -> unit
val discard : t -> unit