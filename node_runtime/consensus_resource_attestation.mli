(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type seen = {
  seen_epoch : int64;
  node : string;
  kind : string;
  weight : int64;
}

val seen :
  Octra_consensus.Resource_attestation_flow.gossip ->
  seen

val log_seen :
  Octra_consensus.Resource_attestation_flow.gossip ->
  unit Lwt.t