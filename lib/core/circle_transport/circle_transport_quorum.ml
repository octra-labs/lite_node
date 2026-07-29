(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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