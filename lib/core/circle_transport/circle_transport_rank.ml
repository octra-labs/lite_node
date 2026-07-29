(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let compare_newest (left : Circles.relay_claim) (right : Circles.relay_claim) =
  Circle_transport_claim_set.compare_claims left right

let compare_oldest (left : Circles.relay_claim) (right : Circles.relay_claim) =
  Circle_transport_claim_set.compare_claims right left

let compare_weighted ~prefer_newest policy (left : Circles.relay_claim) (right : Circles.relay_claim) =
  let left_weight = Circle_transport_weight.relay_weight policy left.relay_id in
  let right_weight = Circle_transport_weight.relay_weight policy right.relay_id in
  match Int.compare right_weight left_weight with
  | 0 ->
    if prefer_newest then
      compare_newest left right
    else
      compare_oldest left right
  | diff ->
    diff

let ranked_claims (policy : Circle_transport_policy.t) claims =
  match policy.ingress_strategy with
  | Circle_transport_policy.Any_ready_relay ->
    Circle_transport_claim_set.sort claims
  | Primary_oldest ->
    List.sort compare_oldest claims
  | Primary_newest ->
    List.sort compare_newest claims
  | Primary_heaviest ->
    List.sort (compare_weighted ~prefer_newest:true policy) claims
  | Primary_heaviest_oldest ->
    List.sort (compare_weighted ~prefer_newest:false policy) claims
  | Primary_heaviest_newest ->
    List.sort (compare_weighted ~prefer_newest:true policy) claims

let primary_claim policy claims =
  match ranked_claims policy claims with
  | claim :: _ -> Some claim
  | [] -> None