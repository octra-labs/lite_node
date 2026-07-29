(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type evaluation = {
  effective_status : Circles.outbox_status;
  effective_resolution : Circles.outbox_resolution option;
}

let status_of_reason = function
  | Circles.Fulfilled_delivery -> Circles.Fulfilled
  | Relay_cancelled
  | Owner_cancelled
  | Claim_set_exhausted
  | Delivery_key_inactive
  | Delivery_key_expired
  | Delivery_key_revoked
  | Delivery_key_erased -> Circles.Cancelled
  | Intent_expired
  | Claim_expired -> Circles.Expired

let make
    ?actor_id
    ?related_key_id
    ~intent_id
    ~resolved_epoch
    reason =
  {
    Circles.intent_id;
    status = status_of_reason reason;
    reason;
    resolved_epoch;
    actor_id;
    related_key_id;
  }