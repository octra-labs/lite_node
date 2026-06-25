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


type domain =
  | Legacy_epoch
  | Tx_gossip
  | Consensus
  | Unknown of int

let consensus_types = [
  Octra_net.P2p_frame.msg_cons_propose;
  Octra_net.P2p_frame.msg_cons_vote;
  Octra_net.P2p_frame.msg_cons_finalize;
  Octra_net.P2p_frame.msg_query_epoch_root;
  Octra_net.P2p_frame.msg_epoch_root_response;
  Octra_net.P2p_frame.msg_query_bundle;
  Octra_net.P2p_frame.msg_bundle_response;
  Octra_net.P2p_frame.msg_query_catchup_range;
  Octra_net.P2p_frame.msg_catchup_range_response;
  Octra_net.P2p_frame.msg_query_catchup_range_v2;
  Octra_net.P2p_frame.msg_catchup_range_response_v2;
]

let legacy_epoch_broadcast = 0x40

let is_consensus t =
  List.exists (( = ) t) consensus_types

let classify = function
  | t when t = legacy_epoch_broadcast -> Legacy_epoch
  | t when t = Octra_net.P2p_frame.msg_tx_gossip -> Tx_gossip
  | t when is_consensus t -> Consensus
  | t -> Unknown t