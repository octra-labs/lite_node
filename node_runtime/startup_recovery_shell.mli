(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


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