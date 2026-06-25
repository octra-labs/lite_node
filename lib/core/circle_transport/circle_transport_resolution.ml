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