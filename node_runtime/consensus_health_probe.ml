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

module Addresses = Map.Make (String)

let eligible (weights : C_driver.root_weights) responses =
  C_driver.epoch_root_records ~epoch_id:weights.epoch_id responses
  |> List.filter_map (fun (r : C_driver.epoch_root_response_record) ->
    match weights.weight r.C_driver.responder_addr with
    | Some weight when Z.sign weight > 0 -> Some (r, weight)
    | None | Some _ -> None)

let root_order a b =
  let by_weight = Z.compare b.weight a.weight in
  if by_weight <> 0 then by_weight else
  let by_count = Int.compare b.count a.count in
  if by_count <> 0 then by_count else String.compare a.root b.root

let peer_roots ~weights responses =
  eligible weights responses
  |> List.fold_left (fun roots ((r : C_driver.epoch_root_response_record), weight) ->
    match r.C_driver.state_root with
    | Some root when String.length root = 32 ->
      Addresses.update root (function
        | None -> Some {root; count = 1; weight}
        | Some prior -> Some {root; count = prior.count + 1;
            weight = Z.add prior.weight weight}) roots
    | None | Some _ -> roots) Addresses.empty
  |> Addresses.bindings
  |> List.map snd
  |> List.sort root_order

let peer_root_majority ~weights responses =
  List.nth_opt (peer_roots ~weights responses) 0

let root_agrees (weights : C_driver.root_weights) root =
  weights.agrees ~count:root.count ~weight:root.weight

let own_root_proved ~weights ~local_root responses =
  peer_roots ~weights responses
  |> List.exists (fun root -> root.root = local_root && root_agrees weights root)

let peer_root_quorum ~(weights : C_driver.root_weights) ~required ~local_root responses =
  let roots = peer_roots ~weights responses in
  let own = List.find_opt (fun root -> root.root = local_root) roots in
  let foreign = List.filter (fun root ->
    root.root <> local_root && root_agrees weights root) roots in
  match foreign, own with
  | [other], own when
      (match own with None -> true | Some own ->
        not (root_agrees weights own) && Z.gt other.weight own.weight) ->
    Mismatching_quorum other
  | [], Some own when weights.attested ~required ~count:own.count ~weight:own.weight ->
    Matching_quorum own
  | _ -> match roots with [] -> No_root | first :: _ -> Missing_quorum first

let root_quorum_count = function
  | No_root -> 0
  | Missing_quorum m
  | Matching_quorum m
  | Mismatching_quorum m -> m.count

let root_quorum_has_quorum = function
  | Matching_quorum _ -> true
  | Mismatching_quorum _
  | No_root
  | Missing_quorum _ -> false

let target_ceiling ~head ~head_time ~now ~allowance =
  match head_time with
  | Some time when head >= 0L && allowance >= 0
      && Float.is_finite time && Float.is_finite now
      && time >= 0. && now >= time ->
    let steps = ceil ((now -. time) /. Octra_consensus.Epoch_time.interval_seconds) in
    if not (Float.is_finite steps) || steps >= Int64.to_float Int64.max_int then None
    else
      let total = Z.(add (of_int64 head)
        (add (of_int64 (Int64.of_float steps)) (of_int allowance))) in
      if Z.fits_int64 total then Some (Z.to_int64 total) else None
  | None | Some _ -> None

let peer_heads ~weights ~ceiling responses =
  eligible weights responses |> List.filter (fun ((r : C_driver.epoch_root_response_record), _) ->
    r.C_driver.responder_head_epoch >= -1L
    && r.responder_head_epoch <= ceiling)

let head_order replies =
  replies |> List.map (fun ((r : C_driver.epoch_root_response_record), _) -> r.responder_head_epoch)
  |> List.sort_uniq (fun a b -> Int64.compare b a)

let head_support head replies =
  List.fold_left (fun (count, weight) ((r : C_driver.epoch_root_response_record), value) ->
    if r.C_driver.responder_head_epoch >= head then count + 1, Z.add weight value
    else count, weight) (0, Z.zero) replies

let peer_target ~weights ~ceiling responses =
  let replies = peer_heads ~weights ~ceiling responses in
  head_order replies
  |> List.find_opt (fun head ->
    let count, weight = head_support head replies in
    weights.C_driver.agrees ~count ~weight)

let peer_ahead ~(weights : C_driver.root_weights) ~head ~ceiling responses =
  let replies = peer_heads ~weights ~ceiling responses in
  let current = snd (head_support head replies) in
  let below = Z.sub (snd (head_support (-1L) replies)) current in
  if not (weights.ahead below) || weights.ahead current then None
  else head_order replies
    |> List.find_opt (fun target -> target < head
      && weights.ahead (snd (head_support target replies)))

let in_sync_plan ~required ~has_committed_root ~genesis_attested root_quorum =
  match root_quorum with
  | Mismatching_quorum m -> Peer_root_mismatch m
  | No_root | Missing_quorum _ when genesis_attested -> Accept_genesis
  | No_root when required = 0 && has_committed_root -> Accept_current_root
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