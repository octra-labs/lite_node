(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val admit :
  store:Octra_core.Store_irmin.t ->
  program_trust:Octra_vm.Program_trust.t ->
  Octra_core.Transaction.t ->
  (unit, string * string) result Lwt.t