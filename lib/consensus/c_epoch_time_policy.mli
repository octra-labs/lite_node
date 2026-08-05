(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

val activation_for_chain : string -> activation option

val rule_for_epoch :
  chain_id:string ->
  epoch_id:int64 ->
  Epoch_time.rule