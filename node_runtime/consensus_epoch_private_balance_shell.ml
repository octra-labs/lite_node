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

type deps = {
  op : string;
  gate : unit -> Private_gate.reject option;
  with_kat : string -> (unit -> unit Lwt.t) -> unit Lwt.t;
  plan : unit -> (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  log_failure : Private_ledger.failure -> unit;
  trace : Private_ledger.balance_plan -> unit;
  mark_debit : unit -> unit;
  incr_fhe : unit -> unit;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_failure : Private_ledger.failure -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type balance_op_deps = {
  gate : unit -> Private_gate.reject option;
  with_kat : string -> (unit -> unit Lwt.t) -> unit Lwt.t;
  plan : unit -> (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  log_failure : Private_ledger.failure -> unit;
  trace_delta : string -> Private_ledger.balance_plan -> unit;
  mark_debit : unit -> unit;
  incr_fhe : unit -> unit;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_failure : Private_ledger.failure -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type kat_deps = {
  kat_state : string -> Private_ledger.kat;
  backfill : string -> unit;
  log_backfill : addr:string -> op:string -> unit;
  reject : Private_gate.notify_reject -> unit Lwt.t;
}

let run (deps : deps) =
  match deps.gate () with
  | Some r ->
    deps.reject_gate r
  | None ->
    deps.with_kat deps.op (fun () ->
      let open Lwt.Syntax in
      let* plan_result = deps.plan () in
      match plan_result with
      | Error e ->
        deps.log_failure e;
        deps.reject_failure e
      | Ok plan ->
        deps.trace plan;
        deps.mark_debit ();
        deps.incr_fhe ();
        deps.confirm ())

let run_op ~op ~delta deps =
  run {
    op;
    gate = deps.gate;
    with_kat = deps.with_kat;
    plan = deps.plan;
    log_failure = deps.log_failure;
    trace = deps.trace_delta delta;
    mark_debit = deps.mark_debit;
    incr_fhe = deps.incr_fhe;
    reject_gate = deps.reject_gate;
    reject_failure = deps.reject_failure;
    confirm = deps.confirm;
  }

let run_encrypt deps =
  run_op ~op:"encrypt" ~delta:"encrypt_delta" deps

let run_decrypt deps =
  run_op ~op:"decrypt" ~delta:"decrypt_delta" deps

let with_kat (deps : kat_deps) ~addr op apply =
  match Private_gate.kat_action ~op (deps.kat_state addr) with
  | Private_gate.Kat_backfill_then_apply ->
    deps.log_backfill ~addr ~op;
    deps.backfill addr;
    apply ()
  | Private_gate.Kat_apply ->
    apply ()
  | Private_gate.Kat_reject r ->
    deps.reject r