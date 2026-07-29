(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type atomic_deps = {
  skip_recovery : unit -> bool;
  run_recovery : unit -> Octra_core.Startup_recovery.result;
  txlog_position : unit -> int * int;
  tx_count : unit -> int;
  set_total_tx_count : int -> unit;
}

type reconciliation =
  | Reconciled of int
  | Reconciliation_skipped of {
      irmin_last_epoch : int;
      chain_last_epoch : int;
    }
  | Reconciliation_mismatch of {
      irmin_last_epoch : int;
      chain_last_epoch : int;
    }

type reconciliation_deps = {
  irmin_last_epoch : unit -> int;
  chain_last_epoch : unit -> int;
  skip_reconcile : unit -> bool;
  exit_fatal : unit -> unit;
}

val marker_label :
  string option ->
  string

val reconciliation :
  irmin_last_epoch:int ->
  chain_last_epoch:int ->
  skip_reconcile:bool ->
  reconciliation

val run_atomic_recovery :
  atomic_deps ->
  unit

val run_reconciliation :
  reconciliation_deps ->
  unit