(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_driver = Octra_consensus.C_driver

type config = {
  active_f : int;
  validator_count : int;
  state_attest_configured : int;
  snapshot_policy_threshold : int;
  soft_catchup_max_lag : int;
  quarantine_ahead_streak_threshold : int;
  quarantine_ahead_grace_epochs : int;
  quarantine_ahead_drift_tolerance : int;
}

type deps = {
  normalize_next_epoch_for_head : source:string -> unit;
  committed_head_epoch : unit -> int;
  current_epoch : unit -> int;
  catchup_next_target : unit -> int64 option;
  attested_head : int -> bool;
  clear_state_attested : unit -> unit;
  set_catchup_in_progress : bool -> unit;
  set_state_attested : head:int -> root:string -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  query_epoch_root :
    epoch_id:int64 ->
    timeout_seconds:float ->
    C_driver.epoch_root_response_record list Lwt.t;
  read_local_root_raw : unit -> string Lwt.t;
  committed_epoch_root_raw : int -> string option;
  peer_snapshot : unit -> string;
  drain_pending_finalized : unit -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
  repair_empty_fork :
    target_epoch:int64 ->
    target_root:string ->
    required:int ->
    current_root_quorum:bool ->
    bool Lwt.t;
  run_catchup_to_target :
    target_epoch:int64 ->
    reason:string ->
    unit Lwt.t;
  quarantine_active : unit -> bool;
  quarantine_reason : unit -> string;
  ahead_streak : unit -> int;
  incr_ahead_streak : unit -> unit;
}

type fork_repair_deps = {
  committed_head_epoch : unit -> int;
  rewind_allowed : target:int -> head:int -> bool;
  target_matches : target:int -> root:string -> bool;
  empty_after : target:int -> head:int -> bool;
  finality_target_ready : int -> (unit, string) result;
  run_empty : target:int -> root:string -> Octra_core.Fork_head_repair.result Lwt.t;
  rewind_finality : int -> (unit, string) result;
  drop_finality_after : int -> int;
  prune_after_epoch : int -> unit;
  set_current_epoch : int -> unit;
  set_state_attested : head:int -> root:string -> unit;
  set_catchup_in_progress : bool -> unit;
  clear_quarantine : string -> unit;
  mark_quarantine : string -> unit;
  start_height : int64 -> unit Lwt.t;
  wake_ready : unit -> unit Lwt.t;
}

val repair_empty_fork :
  fork_repair_deps ->
  target_epoch:int64 ->
  target_root:string ->
  required:int ->
  current_root_quorum:bool ->
  bool Lwt.t

val run :
  ?stale_retries:int ->
  config ->
  deps ->
  unit Lwt.t