(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

type rule = Prior | Capped | Stake

type t

val make : count_epoch:int64 -> stake_epoch:int64 option -> t
val for_chain : string -> t
val select : t -> int64 -> rule
val id_at : t -> chain_id:string -> epoch_id:int64 -> string
val epochs : t -> int64 list

val rule_at : count_epoch:int64 -> stake_epoch:int64 option -> epoch_id:int64 -> rule
val rule : chain_id:string -> epoch_id:int64 -> rule
val rewind_at :
  count_epoch:int64 -> stake_epoch:int64 option ->
  from_epoch:int64 -> to_epoch:int64 -> bool

val activation_for_chain : string -> activation option
val stake_activation_for_chain : string -> activation option
val active : chain_id:string -> epoch_id:int64 -> bool
val consensus_id : chain_id:string -> string
val consensus_id_at : chain_id:string -> epoch_id:int64 -> string
val linear_epoch : chain_id:string -> int64 option
val linear_weight : chain_id:string -> epoch_id:int64 -> bool
val count_floor_required : chain_id:string -> epoch_id:int64 -> bool
val count_cap_required : chain_id:string -> epoch_id:int64 -> bool
val rewind_allowed :
  chain_id:string ->
  from_epoch:int64 ->
  to_epoch:int64 ->
  bool