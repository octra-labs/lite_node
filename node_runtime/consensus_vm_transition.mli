(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val preverify_circle :
  backend:Octra_core.Epoch_exec.backend ->
  env:Octra_core.Epoch_exec.env ->
  program_trust:Octra_vm.Program_trust.t ->
  Octra_core.Transaction.t ->
  (Octra_circle_runtime.Circle_exec.hfhe_binding, string) result Lwt.t

val process_tx :
  ?preverify:Octra_core.Preverify_commit.t ->
  backend:Octra_core.Epoch_exec.backend ->
  env:Octra_core.Epoch_exec.env ->
  program_trust:Octra_vm.Program_trust.t ->
  Octra_core.Transaction.t ->
  (Octra_core.Epoch_exec.tx_effect, string * string) result Lwt.t