(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val process_tx :
  backend:Octra_core.Epoch_exec.backend ->
  env:Octra_core.Epoch_exec.env ->
  program_trust:Octra_vm.Program_trust.t ->
  Octra_core.Transaction.t ->
  (Octra_core.Epoch_exec.tx_effect, string * string) result Lwt.t