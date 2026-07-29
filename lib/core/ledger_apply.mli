(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

val apply_epoch_public :
  Ledger.t ->
  Transaction.t ->
  outcome