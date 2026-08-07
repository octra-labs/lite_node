(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_driver = Octra_consensus.C_driver

type majority = {
  root : string;
  count : int;
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

let required_attesters ~active_f ~validator_count ~configured =
  let max_peer_attesters = max 0 (validator_count - 1) in
  let base = max (active_f + 1) configured in
  if max_peer_attesters = 0 then 0 else min max_peer_attesters base

let peer_root_majority responses =
  let counts = Hashtbl.create 8 in
  List.iter
    (fun (r : C_driver.epoch_root_response_record) ->
      match r.state_root with
      | Some root ->
        let cur = try Hashtbl.find counts root with Not_found -> 0 in
        Hashtbl.replace counts root (cur + 1)
      | None -> ())
    responses;
  Hashtbl.fold
    (fun root count best ->
      match best with
      | None -> Some { root; count }
      | Some best when count > best.count -> Some { root; count }
      | Some _ -> best)
    counts
    None

let root_with_quorum ~required majority =
  match majority with
  | Some m when m.count >= required -> Some m
  | _ -> None

let compare_root ~required ~local_root majority =
  match majority with
  | None -> No_root
  | Some m when m.count < required -> Missing_quorum m
  | Some m when m.root = local_root -> Matching_quorum m
  | Some m -> Mismatching_quorum m

let peer_root_quorum ~required ~local_root responses =
  responses
  |> peer_root_majority
  |> compare_root ~required ~local_root

let root_quorum_count = function
  | No_root -> 0
  | Missing_quorum m
  | Matching_quorum m
  | Mismatching_quorum m -> m.count

let root_quorum_has_quorum = function
  | Matching_quorum _
  | Mismatching_quorum _ -> true
  | No_root
  | Missing_quorum _ -> false

let peer_root_with_quorum ~required responses =
  responses
  |> peer_root_majority
  |> root_with_quorum ~required

let in_sync_plan ~genesis_attested root_quorum =
  match root_quorum with
  | Mismatching_quorum m -> Peer_root_mismatch m
  | _ when genesis_attested -> Accept_genesis
  | No_root -> Missing_peer_root_quorum 0
  | Missing_quorum m -> Missing_peer_root_quorum m.count
  | Matching_quorum _ -> Accept_current_root

let moving_head_plan ~stale_retries =
  if stale_retries > 0 then Retry_probe (stale_retries - 1)
  else Stop_probe

let stale_sample_plan ~live_head_attested ~stale_retries =
  if (not live_head_attested) && stale_retries > 0 then
    Retry_probe (stale_retries - 1)
  else
    Stop_probe

let within_ahead_grace ~head ~ahead_by ~grace_epochs ~drift_tolerance =
  head < grace_epochs || ahead_by <= drift_tolerance

let ahead_no_quorum_plan ~head ~ahead_by ~grace_epochs ~drift_tolerance
    ~quarantine_active =
  if quarantine_active then
    Probe_current_head
  else if within_ahead_grace ~head ~ahead_by ~grace_epochs ~drift_tolerance then
    Wait_current_head
  else
    Probe_current_head

let ahead_current_root_plan = function
  | Matching_quorum _ -> Current_root_matches
  | Mismatching_quorum m -> Current_root_mismatch m
  | No_root
  | Missing_quorum _ -> Wait_current_root_quorum

let ahead_quorum_plan ~head ~ahead_by ~streak ~grace_epochs
    ~drift_tolerance ~streak_threshold ~quarantine_active =
  let wake_ready = not quarantine_active in
  if within_ahead_grace ~head ~ahead_by ~grace_epochs ~drift_tolerance then
    Stay_active { wake_ready }
  else if streak < streak_threshold then
    Delay_quarantine { wake_ready }
  else
    Quarantine_ahead

let classify_lag_mode ~lag ~soft_lag =
  if lag <= max 0 soft_lag then Soft_lag else Quarantine_lag

let snapshot_plan ~lag =
  Snapshot_fallback lag

let catchup_lag_plan ~lag ~soft_lag =
  match classify_lag_mode ~lag ~soft_lag with
  | Soft_lag -> Lagging_soft lag
  | Quarantine_lag -> Lagging_quarantine lag