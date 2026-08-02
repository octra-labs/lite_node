(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  state_attested : bool ref;
  state_attested_head : int option ref;
  state_attested_root : string option ref;
  quarantine_active : bool ref;
  quarantine_reason : string ref;
  quarantine_since_epoch : int ref;
  prev_root_mismatch_streak : int ref;
  state_root_mismatch_streak : int ref;
  ahead_of_target_streak : int ref;
}

type clear_result =
  | Cleared
  | Inactive
  | Refused of string

let create () = {
  state_attested = ref false;
  state_attested_head = ref None;
  state_attested_root = ref None;
  quarantine_active = ref false;
  quarantine_reason = ref "";
  quarantine_since_epoch = ref (-1);
  prev_root_mismatch_streak = ref 0;
  state_root_mismatch_streak = ref 0;
  ahead_of_target_streak = ref 0;
}

let state_attested t =
  !(t.state_attested)

let quarantine_active t =
  !(t.quarantine_active)

let quarantine_active_ref t =
  t.quarantine_active

let quarantine_reason t =
  !(t.quarantine_reason)

let quarantine_since_epoch t =
  !(t.quarantine_since_epoch)

let prev_root_streak t =
  !(t.prev_root_mismatch_streak)

let set_prev_root_streak t streak =
  t.prev_root_mismatch_streak := streak

let state_root_streak t =
  !(t.state_root_mismatch_streak)

let set_state_root_streak t streak =
  t.state_root_mismatch_streak := streak

let ahead_streak t =
  !(t.ahead_of_target_streak)

let incr_ahead_streak t =
  incr t.ahead_of_target_streak

let clear_state_attested t =
  t.state_attested := false;
  t.state_attested_head := None;
  t.state_attested_root := None

let set_state_attested t ~head ~root =
  t.state_attested := true;
  t.state_attested_head := Some head;
  t.state_attested_root := Some root

let attested_head t head =
  state_attested t
  && (match !(t.state_attested_head) with
      | Some attested -> attested = head
      | None -> false)

let starts_with prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.sub value 0 prefix_len = prefix

let root_attestation_recovers reason =
  List.exists
    (fun prefix -> starts_with prefix reason)
    [
      "peer_root_mismatch_at_head ";
      "ahead_of_target_by_";
      "lag_";
      "prev_state_root_mismatch_streak = ";
      "state_root_mismatch_streak = ";
    ]

let catchup_recovers reason =
  root_attestation_recovers reason
  || starts_with "lag_" reason
  || starts_with "catchup_" reason

let recoverable_reason reason =
  catchup_recovers reason
  || starts_with "bundle_wait_timeout_epoch_" reason

let root_evidence evidence =
  List.mem
    evidence
    [
      "genesis_in_sync";
      "in_sync";
      "ahead_wait_current_head";
      "ahead_current_root_quorum";
      "soft_lag_current_root";
    ]

let clear_allowed ~reason ~evidence =
  String.equal reason evidence
  || String.equal evidence "fork_empty_rollback"
  || (catchup_recovers reason
      && starts_with "catchup_complete:" evidence
      && not (starts_with "catchup_complete:already_in_sync:" evidence))
  || (root_attestation_recovers reason && root_evidence evidence)
  || (root_attestation_recovers reason
      && starts_with "direct_finalized_apply:" evidence)

let enter_quarantine t ~epoch ~reason =
  let entered = not (quarantine_active t) in
  if entered then begin
    t.quarantine_active := true;
    clear_state_attested t;
    t.quarantine_since_epoch := epoch;
    t.quarantine_reason := reason
  end else if
    recoverable_reason (quarantine_reason t)
    && not (recoverable_reason reason)
  then begin
    t.quarantine_reason := reason;
    clear_state_attested t
  end;
  entered

let reset_quarantine t =
  t.quarantine_active := false;
  t.quarantine_reason := "";
  t.quarantine_since_epoch := -1;
  t.prev_root_mismatch_streak := 0;
  t.state_root_mismatch_streak := 0;
  t.ahead_of_target_streak := 0

let clear_quarantine t ~evidence =
  if not (quarantine_active t) then Inactive
  else
    let reason = quarantine_reason t in
    if clear_allowed ~reason ~evidence then begin
      reset_quarantine t;
      Cleared
    end else
      Refused reason