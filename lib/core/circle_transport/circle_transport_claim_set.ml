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


let compare_claims (left : Circles.relay_claim) (right : Circles.relay_claim) =
  match Int64.compare right.claim_epoch left.claim_epoch with
  | 0 ->
    begin
      match Int64.compare right.claim_expiry_epoch left.claim_expiry_epoch with
      | 0 -> String.compare left.relay_id right.relay_id
      | diff -> diff
    end
  | diff ->
    diff

let sort claims =
  List.sort compare_claims claims

let latest_claim claims =
  match sort claims with
  | claim :: _ -> Some claim
  | [] -> None

let find_claim_by_relay claims relay_id =
  List.find_opt (fun (claim : Circles.relay_claim) -> String.equal claim.relay_id relay_id) claims

let claim_live ~current_epoch ?(cancelled_relays = []) (claim : Circles.relay_claim) =
  Circle_transport_claim.live_at current_epoch claim
  && not (List.mem claim.relay_id cancelled_relays)

let live_claims ~current_epoch ?(cancelled_relays = []) claims =
  claims
  |> sort
  |> List.filter (claim_live ~current_epoch ~cancelled_relays)

let active_claim_count ~current_epoch ?(cancelled_relays = []) claims =
  List.length (live_claims ~current_epoch ~cancelled_relays claims)

let active_relay_ids ~current_epoch ?(cancelled_relays = []) claims =
  live_claims ~current_epoch ~cancelled_relays claims
  |> List.map (fun (claim : Circles.relay_claim) -> claim.relay_id)

let primary_active_claim ~current_epoch ?(cancelled_relays = []) claims =
  match live_claims ~current_epoch ~cancelled_relays claims with
  | claim :: _ -> Some claim
  | [] -> None

let without_relay claims relay_id =
  List.filter (fun (claim : Circles.relay_claim) -> not (String.equal claim.relay_id relay_id)) claims