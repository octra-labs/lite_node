(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val resolve :
  find_account:(string -> Octra_core.Ledger.account option) ->
  Octra_core.Transaction.t ->
  string option