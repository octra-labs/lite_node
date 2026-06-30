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

let marker_label = function
  | Some s -> s
  | None -> "none"

let log_recovery r =
  if r.Octra_core.Startup_recovery.index_repaired > 0 then
    Octra_log.warn "init"
      "STARTUP RECOVERY: marker_phase=%s repaired=%d txlog_last=%d irmin_last=%d->%d boundary_ok=%b"
      (marker_label r.marker_phase)
      r.index_repaired
      r.txlog_last_epoch
      r.irmin_last_epoch_before
      r.irmin_last_epoch_after
      r.boundary_ok
  else
    Octra_log.info "init"
      "STARTUP RECOVERY: clean state (txlog_last = %d irmin_last = %d marker=%s)"
      r.txlog_last_epoch
      r.irmin_last_epoch_after
      (marker_label r.marker_phase)

let run_atomic_recovery deps =
  if deps.skip_recovery () then
    Octra_log.warn "init" "OCTRA_SKIP_RECOVERY = 1 - atomic startup recovery DISABLED"
  else begin
    let recovery = deps.run_recovery () in
    log_recovery recovery;
    let post_recovery_txlog_seg, post_recovery_txlog_off =
      deps.txlog_position ()
    in
    Octra_log.info "init" "txlog post-recovery current_seg = %d current_off = %d"
      post_recovery_txlog_seg post_recovery_txlog_off;
    deps.set_total_tx_count (deps.tx_count ())
  end

let reconciliation ~irmin_last_epoch ~chain_last_epoch ~skip_reconcile =
  if irmin_last_epoch = chain_last_epoch then
    Reconciled irmin_last_epoch
  else if skip_reconcile then
    Reconciliation_skipped {
      irmin_last_epoch;
      chain_last_epoch;
    }
  else
    Reconciliation_mismatch {
      irmin_last_epoch;
      chain_last_epoch;
    }

let log_mismatch ~irmin_last_epoch ~chain_last_epoch =
  Octra_log.stderr "[INIT FATAL] REFUSING TO START\n";

let run_reconciliation deps =
  match reconciliation
          ~irmin_last_epoch:(deps.irmin_last_epoch ())
          ~chain_last_epoch:(deps.chain_last_epoch ())
          ~skip_reconcile:(deps.skip_reconcile ()) with
  | Reconciled epoch ->
    Octra_log.info "init" "RECONCILIATION OK: both epoch boundaries at %d" epoch
  | Reconciliation_skipped { irmin_last_epoch; chain_last_epoch } ->
    Octra_log.warn "init"
      "RECONCILIATION SKIPPED via OCTRA_SKIP_RECONCILE = 1 (irmin = %d chain = %d)"
      irmin_last_epoch chain_last_epoch;
    Octra_log.warn "init" "OPERATOR has accepted responsibility for the mismatch"
  | Reconciliation_mismatch { irmin_last_epoch; chain_last_epoch } ->
    log_mismatch ~irmin_last_epoch ~chain_last_epoch;
    deps.exit_fatal ()