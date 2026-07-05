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

let legacy_replay deps (tx : Transaction.t) =
  if deps.needs_legacy_audit tx then
    Some (deps.legacy_replay tx.from)
  else
    None

let run_tx deps tx =
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