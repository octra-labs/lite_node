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


let live_at current_epoch (claim : Circles.relay_claim) =
  let current_epoch = Int64.of_int current_epoch in
  Int64.compare current_epoch claim.claim_epoch >= 0
  && Int64.compare current_epoch claim.claim_expiry_epoch < 0

let expired_at current_epoch (claim : Circles.relay_claim) =
  Int64.compare (Int64.of_int current_epoch) claim.claim_expiry_epoch >= 0

let replacement_allowed ~current_epoch = function
  | None -> true
  | Some claim -> expired_at current_epoch claim

let ingress_matches_claim (claim : Circles.relay_claim) (payload : Circles.ingress_commit_payload) =
  claim.intent_id = payload.intent_id
  && claim.relay_id = payload.relay_id

let claim_window_valid ~current_epoch ~intent_expiry_epoch (claim : Circles.relay_claim) =
  Int64.compare claim.claim_epoch (Int64.of_int current_epoch) >= 0
  && Int64.compare claim.claim_expiry_epoch claim.claim_epoch > 0
  && Int64.compare claim.claim_expiry_epoch intent_expiry_epoch <= 0