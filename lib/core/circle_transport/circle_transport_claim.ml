(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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