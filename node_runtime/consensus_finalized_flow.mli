(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type phase =
  | Catchup
  | Quarantine
  | State_attestation

type notice =
  | No_notice
  | Finality_gap_required
  | Observer_queued

type catchup_plan = {
  reason : string;
  drop_stashed : bool;
  queue_before_catchup : bool;
  notice : notice;
}

type no_driver_plan = {
  reason : string;
  notice : notice;
}

type plan =
  | Drop_duplicate
  | Run_catchup of catchup_plan
  | Quarantine_no_driver of no_driver_plan
  | Apply_now
  | Stash of phase

type stashed_plan =
  | Direct_apply_stashed of {
      clear_reason : string;
    }
  | Wait_stashed
  | Replay_stashed

type proposer_info = {
  epoch : int;
  creator_addr : string;
  commit_round : int;
}

val phase_label :
  phase ->
  string

val proposer_info :
  epoch:int ->
  creator_addr:string ->
  commit_round:int ->
  proposer_info option

val expected_root :
  string ->
  string option

val plan :
  head:int ->
  header_epoch:int ->
  observer:bool ->
  driver_available:bool ->
  catchup_active:bool ->
  quarantine_active:bool ->
  state_attested:bool ->
  plan

val stashed_plan :
  phase:phase ->
  header_epoch:int ->
  current_epoch:int ->
  prev_root_matches:bool ->
  state_attested:bool ->
  catchup_active:bool ->
  stashed_plan