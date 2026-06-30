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