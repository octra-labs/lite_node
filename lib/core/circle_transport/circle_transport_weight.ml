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


let relay_weight (policy : Circle_transport_policy.t) relay_id =
  match List.assoc_opt relay_id policy.relay_weights with
  | Some weight when weight > 0 -> weight
  | _ -> 1

let claims_weight policy claims =
  List.fold_left
    (fun acc (claim : Circles.relay_claim) -> acc + relay_weight policy claim.relay_id)
    0
    claims