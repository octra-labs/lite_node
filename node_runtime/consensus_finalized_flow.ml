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

let zero_root =
  String.make 32 '\x00'

let phase_label = function
  | Catchup -> "catchup"
  | Quarantine -> "quarantine"
  | State_attestation -> "state_attestation"

let proposer_info ~epoch ~creator_addr ~commit_round =
  if String.length creator_addr > 3 then
    Some { epoch; creator_addr; commit_round }
  else
    None

let expected_root root =
  if String.length root = 32 && root <> zero_root then Some root else None

let gate_phase ~catchup_active ~quarantine_active ~state_attested =
  if catchup_active then Some Catchup
  else if quarantine_active then Some Quarantine
  else if not state_attested then Some State_attestation
  else None

let run_catchup ~reason ~drop_stashed ~queue_before_catchup ~notice =
  Run_catchup {
    reason;
    drop_stashed;
    queue_before_catchup;
    notice;
  }

let no_driver ~reason ~notice =
  Quarantine_no_driver { reason; notice }

let plan ~head ~header_epoch ~observer ~driver_available ~catchup_active
    ~quarantine_active ~state_attested =
  match Octra_consensus.Finality_log.decide ~head header_epoch with
  | Octra_consensus.Finality_log.Already_applied ->
    Drop_duplicate
  | Octra_consensus.Finality_log.Need_catchup ->
    if not driver_available then
      no_driver
        ~reason:(Printf.sprintf "finality_gap_no_driver height = %d head = %d" header_epoch head)
        ~notice:Finality_gap_required
    else if observer then
      run_catchup
        ~reason:"observer_finality_gap"
        ~drop_stashed:true
        ~queue_before_catchup:false
        ~notice:Finality_gap_required
    else
      run_catchup
        ~reason:"finality_gap"
        ~drop_stashed:false
        ~queue_before_catchup:false
        ~notice:Finality_gap_required
  | Octra_consensus.Finality_log.Apply_now ->
    if observer then
      if driver_available then
        run_catchup
          ~reason:"observer_finality"
          ~drop_stashed:true
          ~queue_before_catchup:true
          ~notice:Observer_queued
      else
        no_driver
          ~reason:(Printf.sprintf "observer_finality_no_driver height = %d head = %d" header_epoch head)
          ~notice:No_notice
    else
      match gate_phase ~catchup_active ~quarantine_active ~state_attested with
      | Some phase -> Stash phase
      | None -> Apply_now

let stashed_plan ~phase ~header_epoch ~current_epoch ~prev_root_matches
    ~catchup_active =
  if catchup_active then
    Wait_stashed
  else if header_epoch = current_epoch && prev_root_matches then
    Direct_apply_stashed {
      clear_reason = "direct_finalized_apply:" ^ phase_label phase;
    }
  else
    Replay_stashed