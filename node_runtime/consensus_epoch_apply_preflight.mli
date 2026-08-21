(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type effects = {
  begin_store_batch : unit -> unit Lwt.t;
  begin_chaindata_batch : unit -> unit;
  ledger_hash : unit -> string Lwt.t;
  cached_head : unit -> Octra_core.Head_manifest.t option;
  expected_prev_root : int -> string option;
  fatal : string -> unit;
  require_sync : Sync_need.t -> unit;
  exit : unit -> unit;
}

type request = {
  epoch_id : int;
}

type result = {
  pre_state : Consensus_epoch_apply_env.pre_state;
}

type fault = {
  head_epoch : int;
  head_root : string;
  live_root : string;
}

val check_head :
  epoch:int ->
  root:string ->
  Octra_core.Head_manifest.t option ->
  (unit, fault) Stdlib.result

val run :
  effects ->
  request ->
  result Lwt.t