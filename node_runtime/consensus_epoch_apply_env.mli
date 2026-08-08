(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec

type pre_state_effects = {
  ledger_hash : unit -> string Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
}

type pre_state = {
  ledger_root : string;
  consensus_root : string;
}

type node_env = {
  chain_id : string;
  current_epoch : unit -> int;
  epoch_ts : int -> float option;
  driver : unit -> Octra_consensus.C_driver.t option;
  validator_fallback : int -> (string * string) list;
  ready_state_root_at : int -> string option Lwt.t;
  ready_max_lag : int;
}

val read_pre_state :
  pre_state_effects ->
  pre_state Lwt.t

val validator_pubkeys :
  driver:Octra_consensus.C_driver.t option ->
  fallback:(unit -> (string * string) list) ->
  (string * string) list

val current_validator_pubkeys :
  node_env ->
  (string * string) list

val resolve_epoch_ts :
  consensus_mode:bool ->
  node_env ->
  int ->
  (float, string) result

val standard :
  chain_id:string ->
  epoch_id:int ->
  proposer_addr:string ->
  validator_pubkeys:(string * string) list ->
  prev_state_root:string ->
  ready_state_root_at:(int -> string option Lwt.t) ->
  ready_max_lag:int ->
  Epoch_exec.env

val standard_for_proposer :
  node_env ->
  proposer_addr:string ->
  prev_state_root:string ->
  Epoch_exec.env