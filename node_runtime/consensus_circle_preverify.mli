(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type runtime = {
  store : Octra_core.Store_irmin.t;
  ledger : Octra_core.Ledger.t;
  program_trust : Octra_vm.Program_trust.t;
  env : pre_state_root:string -> Octra_core.Epoch_exec.env;
}

val run :
  runtime ->
  pre_state_hash:string ->
  pre_state_root:string ->
  Octra_core.Transaction.t ->
  (Octra_core.Preverify_receipt.circle_state, string) result Lwt.t