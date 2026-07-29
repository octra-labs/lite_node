(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let relay_weight (policy : Circle_transport_policy.t) relay_id =
  match List.assoc_opt relay_id policy.relay_weights with
  | Some weight when weight > 0 -> weight
  | _ -> 1

let claims_weight policy claims =
  List.fold_left
    (fun acc (claim : Circles.relay_claim) -> acc + relay_weight policy claim.relay_id)
    0
    claims