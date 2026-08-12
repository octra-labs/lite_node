(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type finalized_state =
  | No_trigger
  | Missing_header
  | Cached_bundle
  | Empty_bundle
  | Missing_bundle of {
      target_epoch : int64;
    }

type action =
  | Wait
  | Apply
  | Clear_stale_trigger
  | Store_empty_bundle_and_apply
  | Queue_missing_bundle of {
      target_epoch : int64;
      reason : string;
    }

type t = {
  action : action;
  log_tick : bool;
  next_sleep : float;
}

type 'header prepared = {
  state : finalized_state;
  empty_bundle_header : 'header option;
  commit_round : int option;
  finalize : Octra_consensus.C_types.finalize option;
}

val finalized_state :
  consensus_mode:bool ->
  consensus_finalized:bool ->
  current_epoch:int ->
  find_finalized:(int -> Octra_consensus.C_types.finalize option) ->
  cached_bundle_for_pid:(string -> bool) ->
  header_has_empty_bundle:(Octra_consensus.C_types.epoch_header -> bool) ->
  Octra_consensus.C_types.epoch_header prepared

val plan :
  consensus_mode:bool ->
  epoch_duration:float ->
  elapsed:float ->
  finalized_state:finalized_state ->
  t

val should_apply : action -> bool

val apply_action :
  current_epoch:int ->
  clear_trigger:bool ->
  store_empty_bundle:(unit -> unit) ->
  queue_missing_bundle:(target_epoch:int64 -> reason:string -> unit) ->
  warn:(string -> unit) ->
  clear_finalized:(unit -> unit) ->
  action ->
  unit