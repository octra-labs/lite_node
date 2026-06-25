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


let relay_claim_subject = Octra_core.Circle_transport_subject.relay_claim_subject

let ingress_packet_subject = Octra_core.Circle_transport_subject.ingress_packet_subject

let ingress_payload_subject = Octra_core.Circle_transport_subject.ingress_payload_subject