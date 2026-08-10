(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Private_gate = Consensus_epoch_apply_private_gate
module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type deps = {
  gate : unit -> Private_gate.reject option;
  apply : unit -> Private_ledger.key_switch_outcome Lwt.t;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_key_switch :
    event:string ->
    Private_ledger.key_switch_rejection ->
    unit Lwt.t;
  log_applied : Private_ledger.key_switch_apply -> unit;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type tx_deps = {
  gate : unit -> Private_gate.reject option;
  needs_legacy_audit : Transaction.t -> bool;
  legacy_replay : string -> Octra_core.Pvac_legacy_public_replay.decision;
  apply_tx :
    ?legacy_public_replay:Octra_core.Pvac_legacy_public_replay.decision ->
    Transaction.t ->
    Private_ledger.key_switch_outcome Lwt.t;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_key_switch :
    event:string ->
    Private_ledger.key_switch_rejection ->
    unit Lwt.t;
  log_applied : Private_ledger.key_switch_apply -> unit;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type live_tx_args = {
  gate : unit -> Private_gate.reject option;
  needs_legacy_audit : Transaction.t -> bool;
  legacy_replay : string -> Octra_core.Pvac_legacy_public_replay.decision;
  apply_tx :
    ?legacy_public_replay:Octra_core.Pvac_legacy_public_replay.decision ->
    Transaction.t ->
    Private_ledger.key_switch_outcome Lwt.t;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  record_rejected : Transaction.t -> string -> string -> unit;
  continue_after_reject : consume_nonce:bool -> unit Lwt.t;
  short_addr : string -> string;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

type live_ledger_tx_args = {
  ledger : Octra_core.Ledger.t;
  field_policy : Private_ledger.field_policy;
  legacy_replay : string -> Octra_core.Pvac_legacy_public_replay.decision;
  gate : unit -> Private_gate.reject option;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  record_rejected : Transaction.t -> string -> string -> unit;
  continue_after_reject : consume_nonce:bool -> unit Lwt.t;
  short_addr : string -> string;
  incr_fhe : unit -> unit;
  confirm : unit -> unit Lwt.t;
}

let rejected_event (r : Private_ledger.key_switch_rejection) =
  if String.equal r.failure.tag "key_switch_failed" then
    "key_switch_failed"
  else
    "key_switch_rejected"

let run (deps : deps) =
  let open Lwt.Syntax in
  match deps.gate () with
  | Some r ->
    deps.reject_gate r
  | None ->
    let* outcome = deps.apply () in
    match outcome with
    | Private_ledger.Key_switch_rejected r ->
      deps.reject_key_switch ~event:(rejected_event r) r
    | Private_ledger.Key_switch_applied plan ->
      deps.log_applied plan;
      deps.incr_fhe ();
      deps.confirm ()

let legacy_replay (deps : tx_deps) (tx : Transaction.t) =
  if deps.needs_legacy_audit tx then
    Some (deps.legacy_replay tx.from)
  else
    None

let run_tx (deps : tx_deps) tx =
  run {
    gate = deps.gate;
    apply = (fun () ->
      match legacy_replay deps tx with
      | None ->
        deps.apply_tx tx
      | Some replay ->
        deps.apply_tx ~legacy_public_replay:replay tx);
    reject_gate = deps.reject_gate;
    reject_key_switch = deps.reject_key_switch;
    log_applied = deps.log_applied;
    incr_fhe = deps.incr_fhe;
    confirm = deps.confirm;
  }

let live_tx_deps (args : live_tx_args) tx =
  {
    gate = args.gate;
    needs_legacy_audit = args.needs_legacy_audit;
    legacy_replay = args.legacy_replay;
    apply_tx = args.apply_tx;
    reject_gate = args.reject_gate;
    reject_key_switch = (fun ~event r ->
      Log.error "epoch" "event = %s addr = %s reason = %s"
        event
        (args.short_addr tx.Transaction.from)
        r.Private_ledger.failure.reason;
      args.record_rejected tx r.failure.tag r.failure.reason;
      args.continue_after_reject ~consume_nonce:r.consume_nonce);
    log_applied = (fun key_plan ->
      Log.info "epoch"
        "event = key_switch addr = %s old_key = %s new_key = %s"
        (args.short_addr tx.Transaction.from)
        key_plan.Private_ledger.old_key_hash
        key_plan.new_key_hash);
    incr_fhe = args.incr_fhe;
    confirm = args.confirm;
  }

let run_live_tx args tx =
  run_tx (live_tx_deps args tx) tx

let run_live_ledger_tx args tx =
  run_live_tx
    {
      gate = args.gate;
      needs_legacy_audit =
        Private_ledger.key_switch_requests_legacy_audit
          ~field_policy:args.field_policy;
      legacy_replay = args.legacy_replay;
      apply_tx = (fun ?legacy_public_replay tx ->
        Private_ledger.apply_key_switch
          ~field_policy:args.field_policy
          ?legacy_public_replay
          args.ledger
          tx);
      reject_gate = args.reject_gate;
      record_rejected = args.record_rejected;
      continue_after_reject = args.continue_after_reject;
      short_addr = args.short_addr;
      incr_fhe = args.incr_fhe;
      confirm = args.confirm;
    }
    tx