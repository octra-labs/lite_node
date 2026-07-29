(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Private_gate = Consensus_epoch_apply_private_gate
module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

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

type live_balance_op_args = {
  tx : Transaction.t;
  gate : unit -> Private_gate.reject option;
  with_kat : string -> (unit -> unit Lwt.t) -> unit Lwt.t;
  plan : unit -> (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  log_failure : Private_ledger.failure -> unit;
  trace_cipher : string -> string -> string -> string -> unit;
  mark_debit : unit -> unit;
  incr_fhe : unit -> unit;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_failure : Private_ledger.failure -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type live_ledger_balance_op_args = {
  ledger : Octra_core.Ledger.t;
  tx : Transaction.t;
  gate : unit -> Private_gate.reject option;
  plan : unit -> (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  log_failure : Private_ledger.failure -> unit;
  trace_cipher : string -> string -> string -> string -> unit;
  mark_debit : unit -> unit;
  incr_fhe : unit -> unit;
  reject_gate : Private_gate.reject -> unit Lwt.t;
  reject_failure : Private_ledger.failure -> unit Lwt.t;
  short_addr : string -> string;
  confirm : unit -> unit Lwt.t;
}

val run : deps -> unit Lwt.t

val run_encrypt : balance_op_deps -> unit Lwt.t

val run_decrypt : balance_op_deps -> unit Lwt.t

val live_balance_op_deps :
  live_balance_op_args ->
  balance_op_deps

val live_ledger_balance_op_deps :
  live_ledger_balance_op_args ->
  balance_op_deps

val with_kat :
  kat_deps ->
  addr:string ->
  string ->
  (unit -> unit Lwt.t) ->
  unit Lwt.t