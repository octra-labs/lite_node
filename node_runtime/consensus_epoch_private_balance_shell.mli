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

val run : deps -> unit Lwt.t

val run_encrypt : balance_op_deps -> unit Lwt.t

val run_decrypt : balance_op_deps -> unit Lwt.t

val with_kat :
  kat_deps ->
  addr:string ->
  string ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t