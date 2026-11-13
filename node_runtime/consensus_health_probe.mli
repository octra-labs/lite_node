(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_driver = Octra_consensus.C_driver

type majority = {
  root : string;
  count : int;
  weight : Z.t;
}

type root_quorum =
  | No_root
  | Missing_quorum of majority
  | Matching_quorum of majority
  | Mismatching_quorum of majority

type in_sync_plan =
  | Peer_root_mismatch of majority
  | Accept_genesis
  | Missing_peer_root_quorum of int
  | Accept_current_root

type retry_plan =
  | Retry_probe of int
  | Stop_probe

type ahead_no_quorum_plan =
  | Wait_current_head
  | Probe_current_head

type ahead_current_root_plan =
  | Current_root_matches
  | Current_root_mismatch of majority
  | Wait_current_root_quorum

type ahead_quorum_plan =
  | Stay_active of {
      wake_ready : bool;
    }
  | Delay_quarantine of {
      wake_ready : bool;
    }
  | Quarantine_ahead

type lag_mode =
  | Soft_lag
  | Quarantine_lag

type lag_plan =
  | Snapshot_fallback of int
  | Lagging_soft of int
  | Lagging_quarantine of int

val peer_root_majority :
  weights:C_driver.root_weights ->
  C_driver.epoch_root_response_record list ->
  majority option

val peer_root_quorum :
  weights:C_driver.root_weights ->
  required:int ->
  local_root:string ->
  C_driver.epoch_root_response_record list ->
  root_quorum

val root_quorum_count :
  root_quorum ->
  int

val root_quorum_has_quorum :
  root_quorum ->
  bool

val peer_roots : weights:C_driver.root_weights ->
  C_driver.epoch_root_response_record list -> majority list
val own_root_proved : weights:C_driver.root_weights -> local_root:string ->
  C_driver.epoch_root_response_record list -> bool
val target_ceiling : head:int64 -> head_time:float option -> now:float ->
  allowance:int -> int64 option
val peer_target : weights:C_driver.root_weights -> ceiling:int64 ->
  C_driver.epoch_root_response_record list -> int64 option
val peer_ahead : weights:C_driver.root_weights -> head:int64 -> ceiling:int64 ->
  C_driver.epoch_root_response_record list -> int64 option

val in_sync_plan :
  required:int ->
  has_committed_root:bool ->
  genesis_attested:bool ->
  root_quorum ->
  in_sync_plan

val moving_head_plan :
  stale_retries:int ->
  retry_plan

val stale_sample_plan :
  live_head_attested:bool ->
  stale_retries:int ->
  retry_plan

val within_ahead_grace : head:int -> ahead_by:int -> grace_epochs:int ->
  drift_tolerance:int -> bool

val ahead_no_quorum_plan :
  head:int ->
  ahead_by:int ->
  grace_epochs:int ->
  drift_tolerance:int ->
  quarantine_active:bool ->
  ahead_no_quorum_plan

val ahead_current_root_plan :
  root_quorum ->
  ahead_current_root_plan

val ahead_quorum_plan :
  head:int ->
  ahead_by:int ->
  streak:int ->
  grace_epochs:int ->
  drift_tolerance:int ->
  streak_threshold:int ->
  quarantine_active:bool ->
  ahead_quorum_plan

val classify_lag_mode :
  lag:int ->
  soft_lag:int ->
  lag_mode

val snapshot_plan :
  lag:int ->
  lag_plan

val catchup_lag_plan :
  lag:int ->
  soft_lag:int ->
  lag_plan