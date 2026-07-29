(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module Epochlog = Octra_core.Epochlog

type role =
  | Proposer
  | Validator
  | Proposer_validator

val role_label : role -> string
val role_of : Epoch_exec.reward_credit -> role
val recipients :
  Epoch_exec.reward_credit list ->
  Epochlog.reward_recipient list