(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Flow = Octra_consensus.Resource_attestation_flow
module Resource = Octra_consensus.Resource_attestations

type seen = {
  seen_epoch : int64;
  node : string;
  kind : string;
  weight : int64;
}

let seen gossip =
  let attestation = gossip.Flow.attestation in
  {
    seen_epoch = gossip.Flow.seen_epoch;
    node = Text.addr_short attestation.Resource.node_id;
    kind = Resource.attestation_kind_to_string attestation.kind;
    weight = attestation.Resource.weight;
  }

let log_seen gossip =
  let event = seen gossip in
  Log.info "consensus"
    "resource_attestation seen_epoch = %Ld node = %s kind = %s weight = %Ld"
    event.seen_epoch
    event.node
    event.kind
    event.weight;
  Lwt.return_unit