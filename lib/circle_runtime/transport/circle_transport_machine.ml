(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


let claim_admissible ~current_epoch ~(intent : Octra_core.Circles.outbox_intent) ~status ~existing_claim claim =
  if Int64.compare intent.expiry_epoch (Int64.of_int current_epoch) <= 0 then
    Error ("circle_outbox_expired", "outbox intent already expired")
  else if not (Circle_transport_claim.claim_window_valid ~current_epoch ~intent_expiry_epoch:intent.expiry_epoch claim) then
    Error ("circle_relay_claim_window_invalid", "relay claim window is invalid for this intent")
  else
    match status with
    | Octra_core.Circles.Open ->
      Ok ()
    | Claimed ->
      begin
        match existing_claim with
        | None ->
          Error ("circle_relay_claim_invalid_state", "claimed outbox is missing claim metadata")
        | Some existing_claim ->
          if Circle_transport_claim.replacement_allowed ~current_epoch (Some existing_claim) then
            Ok ()
          else
            Error ("circle_relay_claim_active", "outbox intent already has a live relay claim")
      end
    | Fulfilled ->
      Error ("circle_outbox_already_fulfilled", "outbox intent already fulfilled")
    | Cancelled ->
      Error ("circle_outbox_cancelled", "outbox intent is cancelled")
    | Expired ->
      Error ("circle_outbox_expired", "outbox intent already expired")

let ingress_admissible ~current_epoch ~(intent : Octra_core.Circles.outbox_intent) ~status ~active_claim payload =
  if Int64.compare intent.expiry_epoch (Int64.of_int current_epoch) <= 0 then
    Error ("circle_outbox_expired", "outbox intent already expired")
  else
    match status with
    | Octra_core.Circles.Open ->
      Error ("circle_relay_claim_required", "outbox intent requires a live relay claim")
    | Claimed ->
      begin
        match active_claim with
        | None ->
          Error ("circle_relay_claim_inactive", "relay claim is missing or expired")
        | Some claim ->
          if Circle_transport_claim.ingress_matches_claim claim payload then
            Ok ()
          else
            Error ("circle_relay_claim_mismatch", "ingress relay does not match the active relay claim")
      end
    | Fulfilled ->
      Error ("circle_outbox_already_fulfilled", "outbox intent already fulfilled")
    | Cancelled ->
      Error ("circle_outbox_cancelled", "outbox intent is cancelled")
    | Expired ->
      Error ("circle_outbox_expired", "outbox intent already expired")