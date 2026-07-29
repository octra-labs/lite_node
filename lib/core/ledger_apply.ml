(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module L = Ledger
module T = Transaction

type accepted_kind =
  | Applied_standard
  | Applied_op01_burn

type accepted = {
  created_account : string option;
  kind : accepted_kind;
}

type rejection = {
  created_account : string option;
  tag : string;
  reason : string;
  notify_reason : string;
}

type outcome =
  | Accepted of accepted
  | Rejected of rejection

let reject ?created_account tag reason notify_reason =
  Rejected { created_account; tag; reason; notify_reason }

let accept ?created_account kind =
  Accepted { created_account; kind }

let apply_standard ledger tx =
  let fee = tx.T.ou in
  let total_cost = Z.add tx.T.amount fee in
  match L.debit ledger tx.T.from total_cost tx.T.nonce with
  | Error err -> reject "insufficient_balance" err err
  | Ok () ->
    let created_account =
      if L.mem ledger tx.T.to_ then
        None
      else begin
        ignore (L.add_account ledger tx.T.to_ Z.zero);
        Some tx.T.to_
      end
    in
    match L.credit ledger tx.T.to_ tx.T.amount with
    | Error err -> reject ?created_account "supply_violation" err err
    | Ok () -> accept ?created_account Applied_standard

let apply_op01_burn ledger tx =
  match L.apply_op01_burn ledger ~from:tx.T.from ~to_:tx.T.to_ tx.T.amount tx.T.nonce with
  | Error err -> reject "op01_burn_rejected" err err
  | Ok () -> accept Applied_op01_burn

let apply_epoch_public ledger tx =
  match tx.T.op_type with
  | T.Standard -> apply_standard ledger tx
  | T.Op01Burn -> apply_op01_burn ledger tx
  | T.PrivateOp ->
    reject "deprecated"
      "PrivateOp is disabled — use StealthOp V5"
      "PrivateOp is disabled — use StealthOp V5"
  | T.RecryptOp ->
    reject "deprecated"
      "RecryptOp is disabled"
      "RecryptOp is disabled"
  | T.EncryptOp | T.DecryptOp | T.KeySwitch ->
    reject "self_only_operation"
      "encrypt/decrypt/key_switch must have from == to"
      "EncryptOp/DecryptOp/KeySwitch must have from == to"
  | _ ->
    reject "unsupported_epoch_public_op"
      "operation is not handled by public epoch apply"
      "operation is not handled by public epoch apply"