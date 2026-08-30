(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

val activation_for_chain : string -> activation option
val active : chain_id:string -> epoch_id:int64 -> bool
val consensus_id : chain_id:string -> string
val rewind_allowed :
  chain_id:string ->
  from_epoch:int64 ->
  to_epoch:int64 ->
  bool