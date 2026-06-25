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


let primary_claim transport_policy claims =
  Circle_transport_rank.primary_claim transport_policy claims

let eligible_claims transport_policy claims =
  match (transport_policy : Circle_transport_policy.t).ingress_strategy with
  | Circle_transport_policy.Any_ready_relay ->
    Circle_transport_rank.ranked_claims transport_policy claims
  | Primary_oldest
  | Primary_newest
  | Primary_heaviest
  | Primary_heaviest_oldest
  | Primary_heaviest_newest ->
    begin
      match primary_claim transport_policy claims with
      | Some claim -> [claim]
      | None -> []
    end

let eligible_relays transport_policy claims =
  eligible_claims transport_policy claims
  |> List.map (fun (claim : Circles.relay_claim) -> claim.relay_id)

let relay_allowed transport_policy claims relay_id =
  let ingress_strategy =
    (transport_policy : Circle_transport_policy.t).ingress_strategy in
  match ingress_strategy with
  | Circle_transport_policy.Any_ready_relay ->
    List.exists (fun (claim : Circles.relay_claim) -> String.equal claim.relay_id relay_id) claims
  | Primary_oldest
  | Primary_newest
  | Primary_heaviest
  | Primary_heaviest_oldest
  | Primary_heaviest_newest ->
    begin
      match primary_claim transport_policy claims with
      | Some claim -> String.equal claim.relay_id relay_id
      | None -> false
    end