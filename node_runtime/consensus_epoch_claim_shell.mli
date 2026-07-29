(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Private_ledger = Octra_core.Private_ledger
module Transaction = Octra_core.Transaction

type deps = {
  fee : Z.t;
  nonce : int;
  claim_plan : unit -> (Private_ledger.claim_plan, Private_ledger.failure) result Lwt.t;
  debit : Z.t -> int -> (unit, string) result;
  balance_plan :
    Private_ledger.claim_plan ->
    (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  trace : Private_ledger.balance_plan -> unit;
  update : string -> (unit, string) result;
  tx_hash : unit -> string;
  mark : int -> string -> (unit, string) result Lwt.t;
  log_accept : int -> unit;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type tx_deps = {
  claim_plan :
    Transaction.t ->
    (Private_ledger.claim_plan, Private_ledger.failure) result Lwt.t;
  debit : Transaction.t -> Z.t -> int -> (unit, string) result;
  balance_plan :
    Transaction.t ->
    Private_ledger.claim_plan ->
    (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  trace : Transaction.t -> Private_ledger.balance_plan -> unit;
  update : Transaction.t -> string -> (unit, string) result;
  mark : int -> string -> (unit, string) result Lwt.t;
  log_accept : Transaction.t -> int -> unit;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type live_tx_args = {
  claim_plan :
    Transaction.t ->
    (Private_ledger.claim_plan, Private_ledger.failure) result Lwt.t;
  debit : Transaction.t -> Z.t -> int -> (unit, string) result;
  balance_plan :
    Transaction.t ->
    Private_ledger.claim_plan ->
    (Private_ledger.balance_plan, Private_ledger.failure) result Lwt.t;
  trace_cipher : string -> string -> string -> string -> unit;
  update : Transaction.t -> string -> (unit, string) result;
  mark : int -> string -> (unit, string) result Lwt.t;
  short_addr : string -> string;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

type live_ledger_tx_args = {
  ledger : Octra_core.Ledger.t;
  current_epoch : unit -> int;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
  trace_cipher : string -> string -> string -> string -> unit;
  short_addr : string -> string;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

val run : deps -> unit Lwt.t
val run_tx : tx_deps -> Transaction.t -> unit Lwt.t
val live_tx_deps : live_tx_args -> tx_deps
val live_ledger_tx_deps : live_ledger_tx_args -> tx_deps
val log_accept : string -> int -> unit