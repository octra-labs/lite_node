(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module Epochlog = Octra_core.Epochlog

type role =
  | Proposer
  | Validator
  | Proposer_validator

let role_label = function
  | Proposer -> "proposer"
  | Validator -> "validator"
  | Proposer_validator -> "proposer_validator"

let role_of (credit : Epoch_exec.reward_credit) =
  match credit.proposer, credit.validator with
  | true, true -> Proposer_validator
  | true, false -> Proposer
  | false, true -> Validator
  | false, false -> invalid_arg "reward recipient has no role"

let recipient (credit : Epoch_exec.reward_credit) =
  if Z.leq credit.amount Z.zero then
    None
  else
    Some Epochlog.{
      reward_addr = credit.address;
      reward_role = role_label (role_of credit);
      reward_amount = Z.to_string credit.amount;
    }

let recipients credits =
  List.filter_map recipient credits