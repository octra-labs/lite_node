(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type state

type sample = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  now : float;
  source : string;
  stall_sec : float;
  step_timeout_sec : float option;
  observer : bool;
  voting : bool;
  catchup_active : bool;
  quarantine_active : bool;
  state_attested : bool;
  pending_finalized : bool;
  proposal_active : bool;
}

type reset = {
  height : int64;
  round : int;
  step : string;
  expected : int64;
  source : string;
  stall_sec : float;
  step_timeout_sec : float option;
  state_age : float;
  height_age : float;
  resets : int;
}

type result = {
  state : state;
  reset : reset option;
}

type driver_snapshot = {
  height : int64;
  round : int;
  step : string;
  step_timeout_sec : float option;
}

val create : now:float -> state

val record : state -> sample -> result

val step_label : Octra_consensus.C_types.round_step -> string

val driver_snapshot :
  Octra_consensus.C_driver.t ->
  driver_snapshot

val record_snapshot :
  state ->
  driver_snapshot ->
  expected:int64 ->
  now:float ->
  source:string ->
  stall_sec:float ->
  observer:bool ->
  voting:bool ->
  catchup_active:bool ->
  quarantine_active:bool ->
  state_attested:bool ->
  pending_finalized:bool ->
  proposal_active:bool ->
  result