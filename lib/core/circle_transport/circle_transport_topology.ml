(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let active_claim_limit (policy : Circle_transport_policy.t) =
  Circle_transport_policy.active_claim_limit policy

let quorum_threshold (policy : Circle_transport_policy.t) =
  Circle_transport_policy.quorum_threshold_value policy

let quorum_weight_threshold (policy : Circle_transport_policy.t) =
  Circle_transport_policy.quorum_weight_threshold_value policy

let active_claim_weight (policy : Circle_transport_policy.t) claims =
  Circle_transport_weight.claims_weight policy claims

let claim_ready (policy : Circle_transport_policy.t) ~active_claims =
  match policy.claim_topology, policy.quorum_mode with
  | Circle_transport_policy.Quorum_relays, Weighted_quorum ->
    active_claim_weight policy active_claims >= quorum_weight_threshold policy
  | _ ->
    List.length active_claims >= quorum_threshold policy

let single_relay (policy : Circle_transport_policy.t) =
  match policy.claim_topology with
  | Circle_transport_policy.Single_relay -> true
  | Parallel_relays
  | Quorum_relays ->
    false

let multi_relay (policy : Circle_transport_policy.t) =
  not (single_relay policy)