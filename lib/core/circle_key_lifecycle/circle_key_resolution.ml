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


let outbox_reason_of_state = function
  | Circle_key_state.Expired -> Some Circles.Delivery_key_expired
  | Revoked -> Some Delivery_key_revoked
  | Erased -> Some Delivery_key_erased
  | Scheduled
  | Live -> None

let matches_state state reason =
  match outbox_reason_of_state state with
  | Some expected -> expected = reason || reason = Circles.Delivery_key_inactive
  | None -> false