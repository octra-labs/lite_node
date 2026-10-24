(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t
type polc_receipt

val polc_saved : polc_receipt -> policy:C_quorum_policy.t -> chain_id:string ->
  epoch_id:int64 -> validator_set:C_types.validator_set -> round:int -> proposal_id:string -> bool

val polc_save : ?policy:C_quorum_policy.t -> ?sync:(string -> (unit, string) result) ->
  ?keep_rounds:int list ->
  t -> chain_id:string -> epoch_id:int64 -> validator_set:C_types.validator_set ->
  locked_round:int -> C_types.vote list -> (polc_receipt, string) result

val disk : data_dir:string -> t
val memory : unit -> t

val polc_keep : ?policy:C_quorum_policy.t -> ?sync:(string -> (unit, string) result) ->
  t -> chain_id:string -> epoch_id:int64 ->
  validator_set:C_types.validator_set -> locked_round:int -> C_types.vote list -> (unit, string) result

val polc_ready : ?policy:C_quorum_policy.t -> chain_id:string -> epoch_id:int64 ->
  validator_set:C_types.validator_set -> C_types.vote list -> bool

val polc_load : ?policy:C_quorum_policy.t -> t -> chain_id:string -> epoch_id:int64 ->
  validator_set:C_types.validator_set -> (C_types.vote list, string) result

val floor :
  t ->
  (int64 option, string) result

val set_floor :
  t ->
  through_epoch:int64 ->
  (unit, string) result

val round_mark :
  t ->
  ((int64 * int) option, string) result

val set_round_mark :
  t ->
  epoch_id:int64 ->
  round:int ->
  (unit, string) result

val check :
  t ->
  C_types.vote ->
  (unit, string) result

val keep :
  t ->
  C_types.vote ->
  (C_types.vote, string) result

val find_statement :
  t ->
  chain_id:string ->
  validator:string ->
  epoch_id:int64 ->
  round:int ->
  vote_type:C_types.vote_type ->
  proposal_id:string ->
  (C_types.vote option, string) result

val max_round :
  t ->
  chain_id:string ->
  validator:string ->
  epoch_id:int64 ->
  (int option, string) result

val round_votes :
  t -> chain_id:string -> validator:string -> epoch_id:int64 -> round:int ->
  (C_types.vote list, string) result

val prune :
  t ->
  through_epoch:int64 ->
  (unit, string) result