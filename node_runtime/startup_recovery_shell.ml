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

let marker_label = function
  | Some s -> s
  | None -> "none"

let log_recovery r =
  if r.Octra_core.Startup_recovery.index_repaired > 0 then
    Octra_log.warn "init"
      "event = startup_recovery status = repaired marker_phase = %s repaired = %d txlog_last = %d irmin_before = %d irmin_after = %d boundary_ok = %b"
      (marker_label r.marker_phase)
      r.index_repaired
      r.txlog_last_epoch
      r.irmin_last_epoch_before
      r.irmin_last_epoch_after
      r.boundary_ok
  else
    Octra_log.info "init"
      "event = startup_recovery status = clean txlog_last = %d irmin_last = %d marker = %s"
      r.txlog_last_epoch
      r.irmin_last_epoch_after
      (marker_label r.marker_phase)

let run_atomic_recovery deps =
  if deps.skip_recovery () then
    Octra_log.warn "init"
      "event = startup_recovery status = disabled env = OCTRA_SKIP_RECOVERY"
  else begin
    let recovery = deps.run_recovery () in
    log_recovery recovery;
    let post_recovery_txlog_seg, post_recovery_txlog_off =
      deps.txlog_position ()
    in
    Octra_log.info "init"
      "event = txlog_position phase = post_recovery seg = %d off = %d"
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
  Octra_log.fatal "init"
    "event = storage_boundary_mismatch irmin_last = %d chaindata_last = %d action = refuse_start hint = restore_snapshot_or_replay"
    irmin_last_epoch
    chain_last_epoch

let run_reconciliation deps =
  match reconciliation
          ~irmin_last_epoch:(deps.irmin_last_epoch ())
          ~chain_last_epoch:(deps.chain_last_epoch ())
          ~skip_reconcile:(deps.skip_reconcile ()) with
  | Reconciled epoch ->
    Octra_log.info "init" "event = reconciliation status = ok epoch = %d" epoch
  | Reconciliation_skipped { irmin_last_epoch; chain_last_epoch } ->
    Octra_log.warn "init"
      "event = reconciliation status = skipped irmin_last = %d chain_last = %d env = OCTRA_SKIP_RECONCILE"
      irmin_last_epoch chain_last_epoch
  | Reconciliation_mismatch { irmin_last_epoch; chain_last_epoch } ->
    log_mismatch ~irmin_last_epoch ~chain_last_epoch;
    deps.exit_fatal ()