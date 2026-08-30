(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type domain =
  | Legacy_epoch
  | Tx_gossip
  | Consensus
  | Resource_compute
  | Unknown of int

let consensus_types = [
  Octra_net.P2p_frame.msg_cons_propose;
  Octra_net.P2p_frame.msg_cons_vote;
  Octra_net.P2p_frame.msg_cons_finalize;
  Octra_net.P2p_frame.msg_cons_round_sync;
  Octra_net.P2p_frame.msg_query_epoch_root;
  Octra_net.P2p_frame.msg_epoch_root_response;
  Octra_net.P2p_frame.msg_query_bundle;
  Octra_net.P2p_frame.msg_bundle_response;
  Octra_net.P2p_frame.msg_query_catchup_range;
  Octra_net.P2p_frame.msg_catchup_range_response;
  Octra_net.P2p_frame.msg_query_catchup_range_v2;
  Octra_net.P2p_frame.msg_catchup_range_response_v2;
  Octra_net.P2p_frame.msg_resource_attestation;
  Octra_net.P2p_frame.msg_vote_evidence;
]

let legacy_epoch_broadcast = Octra_net.P2p_frame.msg_epoch_broadcast

let is_consensus t =
  List.exists (( = ) t) consensus_types

let classify = function
  | t when t = legacy_epoch_broadcast -> Legacy_epoch
  | t when t = Octra_net.P2p_frame.msg_tx_gossip -> Tx_gossip
  | t when is_consensus t -> Consensus
  | t when t = Octra_net.P2p_frame.msg_resource_compute -> Resource_compute
  | t -> Unknown t