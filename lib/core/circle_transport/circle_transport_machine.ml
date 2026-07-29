(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let claim_admissible
    ~current_epoch
    ~transport_policy
    ~(intent : Circles.outbox_intent)
    ~status
    ~delivery_key_deliverable
    ~existing_claims
    ~active_claims
    (claim : Circles.relay_claim) =
  let latest_claim = Circle_transport_claim_set.latest_claim existing_claims in
  let relay_claim = Circle_transport_claim_set.find_claim_by_relay existing_claims claim.relay_id in
  let relay_active_claim = Circle_transport_claim_set.find_claim_by_relay active_claims claim.relay_id in
  let active_claim_count = List.length active_claims in
  if not (Circle_transport_policy.relay_allowed transport_policy claim.relay_id) then
    Error ("circle_transport_policy_violation", "relay is not allowed by circle transport policy")
  else if not delivery_key_deliverable then
    Error ("circle_delivery_key_inactive", "outbox delivery key is not deliverable for the claim window")
  else if Int64.compare intent.expiry_epoch (Int64.of_int current_epoch) <= 0 then
    Error ("circle_outbox_expired", "outbox intent already expired")
  else if not (Circle_transport_claim.claim_window_valid ~current_epoch ~intent_expiry_epoch:intent.expiry_epoch claim) then
    Error ("circle_relay_claim_window_invalid", "relay claim window is invalid for this intent")
  else
    match status with
    | Circles.Fulfilled ->
      Error ("circle_outbox_already_fulfilled", "outbox intent already fulfilled")
    | Circles.Cancelled ->
      Error ("circle_outbox_cancelled", "outbox intent is cancelled")
    | Circles.Expired ->
      Error ("circle_outbox_expired", "outbox intent already expired")
    | Circles.Open
    | Circles.Claimed ->
      if Option.is_some relay_active_claim then
        Error ("circle_relay_claim_duplicate", "relay already has an active claim for this intent")
      else if Circle_transport_topology.single_relay transport_policy then
        if active_claim_count > 0 then
          Error ("circle_relay_claim_strategy_rejected", "outbox already has an active relay claim")
        else if
          Circle_transport_lease.replacement_allowed
            transport_policy
            ~current_epoch
            ~existing_claim:latest_claim
            ~incoming_relay_id:claim.relay_id
        then
          Ok ()
        else
          Error ("circle_relay_claim_strategy_rejected", "outbox relay claim strategy rejected this replacement")
      else if active_claim_count >= Circle_transport_topology.active_claim_limit transport_policy then
        Error ("circle_relay_claim_capacity_exceeded", "outbox active relay claim capacity is exhausted")
      else
        match relay_claim with
        | Some relay_claim when Circle_transport_claim.expired_at current_epoch relay_claim ->
          begin
            match transport_policy.claim_strategy with
            | Circle_transport_policy.No_replacement ->
              Error ("circle_relay_claim_strategy_rejected", "relay claim strategy rejected this relay re-claim")
            | Any_replacement
            | Same_relay_only ->
              Ok ()
          end
        | Some _ ->
          Error ("circle_relay_claim_duplicate", "relay already has a stored claim for this intent")
        | None ->
          Ok ()

let ingress_admissible
    ~current_epoch
    ~transport_policy
    ~(intent : Circles.outbox_intent)
    ~status
    ~active_claims
    (payload : Circles.ingress_commit_payload) =
  if Int64.compare intent.expiry_epoch (Int64.of_int current_epoch) <= 0 then
    Error ("circle_outbox_expired", "outbox intent already expired")
  else
    match status with
    | Circles.Open ->
      Error ("circle_relay_claim_required", "outbox intent requires a live relay claim")
    | Circles.Claimed ->
      begin
        if active_claims = [] then
          Error ("circle_relay_claim_inactive", "relay claim is missing or expired")
        else if not (Circle_transport_topology.claim_ready transport_policy ~active_claims) then
          Error ("circle_relay_quorum_pending", "outbox relay quorum is not yet satisfied")
        else
          match Circle_transport_claim_set.find_claim_by_relay active_claims payload.relay_id with
          | None ->
            Error ("circle_relay_claim_mismatch", "ingress relay does not match an active relay claim")
          | Some claim ->
            if not (Circle_transport_quorum.relay_allowed transport_policy active_claims payload.relay_id) then
              Error ("circle_relay_primary_required", "ingress relay is not the selected relay for the current claim-set")
            else if Circle_transport_claim.ingress_matches_claim claim payload then
              Ok ()
            else
              Error ("circle_relay_claim_mismatch", "ingress relay does not match the active relay claim")
      end
    | Circles.Fulfilled ->
      Error ("circle_outbox_already_fulfilled", "outbox intent already fulfilled")
    | Circles.Cancelled ->
      Error ("circle_outbox_cancelled", "outbox intent is cancelled")
    | Circles.Expired ->
      Error ("circle_outbox_expired", "outbox intent already expired")

let relay_cancel_admissible
    ~current_epoch
    ~(intent : Circles.outbox_intent)
    ~status
    ~delivery_key_state
    ~existing_claims
    ~active_claims
    (cancel : Circles.relay_cancel) =
  let existing_claim =
    Circle_transport_claim_set.find_claim_by_relay existing_claims cancel.relay_id in
  let active_claim =
    Circle_transport_claim_set.find_claim_by_relay active_claims cancel.relay_id in
  match status with
  | Circles.Fulfilled ->
    Error ("circle_outbox_already_fulfilled", "outbox intent already fulfilled")
  | Circles.Cancelled ->
    Error ("circle_outbox_cancelled", "outbox intent is cancelled")
  | Circles.Expired ->
    Error ("circle_outbox_expired", "outbox intent already expired")
  | Circles.Open ->
    Error ("circle_relay_claim_required", "outbox intent has no relay claim to cancel")
  | Circles.Claimed ->
    begin
      match active_claim, existing_claim with
      | Some _, _ ->
        begin
          match cancel.reason with
          | Circles.Relay_cancelled ->
            Ok ()
          | reason when Circle_key_resolution.matches_state delivery_key_state reason ->
            Ok ()
          | _ ->
            Error ("circle_relay_cancel_reason_invalid", "active relay cancel reason is not allowed")
        end
      | None, Some claim ->
        if Int64.compare intent.expiry_epoch (Int64.of_int current_epoch) <= 0 then
          begin
            match cancel.reason with
            | Circles.Intent_expired -> Ok ()
            | _ -> Error ("circle_relay_cancel_reason_invalid", "intent expiry requires intent_expired reason")
          end
        else if Circle_transport_claim.expired_at current_epoch claim then
          begin
            match cancel.reason with
            | Circles.Claim_expired -> Ok ()
            | reason when Circle_key_resolution.matches_state delivery_key_state reason -> Ok ()
            | _ -> Error ("circle_relay_cancel_reason_invalid", "expired relay cancel reason is not allowed")
          end
        else
          Error ("circle_relay_claim_inactive", "relay claim is no longer active for cancellation")
      | None, None ->
        Error ("circle_relay_claim_mismatch", "cancel relay does not match a claimed relay")
    end