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
  field_policy : Private_ledger.field_policy;
  current_epoch : unit -> int;
  private_result_policy :
    int ->
    Octra_core.Private_result_policy.t;
  trace_cipher : string -> string -> string -> string -> unit;
  short_addr : string -> string;
  reject : string -> string -> unit Lwt.t;
  confirm : unit -> unit Lwt.t;
}

let run (deps : deps) =
  let open Lwt.Syntax in
  let* claim_result = deps.claim_plan () in
  match claim_result with
  | Error e ->
    deps.reject e.Private_ledger.tag e.reason
  | Ok claim ->
    match deps.debit deps.fee deps.nonce with
    | Error err ->
      deps.reject "insufficient_balance" err
    | Ok () ->
      let* balance_result = deps.balance_plan claim in
      match balance_result with
      | Error e ->
        deps.reject e.Private_ledger.tag e.reason
      | Ok plan ->
        deps.trace plan;
        match deps.update plan.Private_ledger.next_cipher with
        | Error e ->
          failwith (Printf.sprintf "claim_update_failed reason = %s" e)
        | Ok () ->
          let tx_hash = deps.tx_hash () in
          let* mark_result = deps.mark claim.claim_output_id tx_hash in
          match mark_result with
          | Error e ->
            failwith ("claim_mark_failed reason = " ^ e)
          | Ok () ->
            deps.log_accept claim.claim_output_id;
            deps.confirm ()

let run_tx (deps : tx_deps) tx =
  run {
    fee = tx.Transaction.ou;
    nonce = tx.nonce;
    claim_plan = (fun () -> deps.claim_plan tx);
    debit = deps.debit tx;
    balance_plan = deps.balance_plan tx;
    trace = deps.trace tx;
    update = deps.update tx;
    tx_hash = (fun () -> Transaction.hash tx);
    mark = deps.mark;
    log_accept = deps.log_accept tx;
    reject = deps.reject;
    confirm = deps.confirm;
  }

let log_accept claimant output_id =
  Log.info "epoch" "event = stealth_claim_v5 claimant = %s output_id = %d"
    claimant output_id

let live_tx_deps args =
  {
    claim_plan = args.claim_plan;
    debit = args.debit;
    balance_plan = args.balance_plan;
    trace = (fun tx plan ->
      args.trace_cipher
        "claim_private"
        tx.Transaction.from
        plan.Private_ledger.current_cipher
        plan.next_cipher);
    update = args.update;
    mark = args.mark;
    log_accept = (fun tx output_id ->
      log_accept (args.short_addr tx.Transaction.from) output_id);
    reject = args.reject;
    confirm = args.confirm;
  }

let live_ledger_tx_deps args =
  live_tx_deps {
    claim_plan =
      Octra_core.Private_ledger.claim_plan
        ~field_policy:args.field_policy
        args.ledger;
    debit = (fun tx fee nonce ->
      Octra_core.Ledger.debit args.ledger tx.Transaction.from fee nonce);
    balance_plan = (fun tx plan ->
      Octra_core.Private_ledger.claim_balance_plan
        ~result_policy:(args.private_result_policy (args.current_epoch ()))
        args.ledger
        tx
        plan);
    trace_cipher = args.trace_cipher;
    update = (fun tx cipher ->
      Octra_core.Ledger.update_enc_balance args.ledger tx.Transaction.from cipher);
    mark = Octra_core.Ledger.mark_stealth_claimed args.ledger;
    short_addr = args.short_addr;
    reject = args.reject;
    confirm = args.confirm;
  }