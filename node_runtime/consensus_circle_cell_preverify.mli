(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type runtime = {
  store : Octra_core.Store_irmin.t;
  env : pre_state_root:string -> Octra_core.Epoch_exec.env;
}

val run :
  runtime ->
  pre_state_hash:string ->
  pre_state_root:string ->
  Octra_core.Transaction.t ->
  (string, string) result Lwt.t