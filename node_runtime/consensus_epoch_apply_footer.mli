(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec

type meta = {
  emission_remaining : Z.t;
  prev_supply : Z.t;
  supply_retired : Z.t;
}

type trace = {
  replay : bool;
  layera : bool;
}

type validator_context = {
  active : string list;
  count : int;
  sha : string;
}

type node_deps = {
  get_meta : string -> string option;
  set_meta : string -> string -> unit Lwt.t;
  policy : Octra_core.Emission_policy.t;
  schedule : Octra_core.Emission_schedule.t;
  legacy_total : string option;
  public_supply : unit -> Z.t;
  apply_footer :
    Epoch_exec.env ->
    Consensus_reward_attribution.t ->
    Epoch_exec.reward_plan ->
    unit Lwt.t;
  log : string -> unit;
}

type node_request = {
  chain_id : string;
  epoch_id : int;
  epoch_ts : float;
  proposer_addr : string;
  validator_pubkeys : (string * string) list;
  reward : Consensus_reward_attribution.t;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
  confirmed_fees : Z.t;
  short : string -> string;
}

type node_result = {
  meta : meta;
  plan : Epoch_exec.reward_plan;
  reward_recipients : Octra_core.Epochlog.reward_recipient list;
}

val z_of_meta : string -> string option -> (Z.t, string) result
val meta :
  supply_retired:string option ->
  emission_remaining:string option ->
  total_supply:string option ->
  (meta, string) result
val reward_plan :
  validator_count:int ->
  confirmed_fees:Z.t ->
  meta ->
  (Epoch_exec.reward_plan, string) result
val trace : env:(string -> string option) -> trace
val validators_sha :
  hash:(string -> string -> string) ->
  raw_to_hex:(string -> string) ->
  string list ->
  string
val validator_context :
  hash:(string -> string -> string) ->
  raw_to_hex:(string -> string) ->
  (string * string) list ->
  validator_context
val replay_proposer_line :
  epoch_id:int ->
  proposer_source:string ->
  proposer:string ->
  validators_sha:string ->
  string
val emit_replay_proposer :
  trace ->
  emit:(string -> unit) ->
  epoch_id:int ->
  proposer_source:string ->
  proposer:string ->
  validators_sha:string ->
  unit
val reward_line :
  fees:Z.t ->
  proposer:string ->
  short:(string -> string) ->
  validator_count:int ->
  Epoch_exec.reward_plan ->
  string
val env :
  chain_id:string ->
  epoch_id:int ->
  epoch_ts:float ->
  proposer_addr:string ->
  validator_pubkeys:(string * string) list ->
  ready_state_root_at:(int -> string option Lwt.t) ->
  ready_max_lag:int ->
  Epoch_exec.env

val run_node :
  node_deps ->
  node_request ->
  node_result Lwt.t