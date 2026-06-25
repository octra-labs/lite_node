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


let expired_claim_status (policy : Circle_transport_policy.t) =
  match policy.lease_class with
  | Circle_transport_policy.Reopen_on_expiry -> Circles.Open
  | Terminal_on_expiry -> Circles.Expired
  | Cancel_on_expiry -> Circles.Cancelled

let replacement_allowed
    (policy : Circle_transport_policy.t)
    ~current_epoch
    ~existing_claim
    ~incoming_relay_id =
  match existing_claim with
  | None -> true
  | Some claim ->
    if not (Circle_transport_claim.expired_at current_epoch claim) then
      false
    else
      match policy.claim_strategy with
      | Circle_transport_policy.Any_replacement -> true
      | Same_relay_only -> claim.relay_id = incoming_relay_id
      | No_replacement -> false