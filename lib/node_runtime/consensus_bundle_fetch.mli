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


module Bundle = Consensus_bundle_validation

type accepted = {
  responder_addr : string;
  bundle : Bundle.accepted;
}

type finalized =
  | Finalized_missing
  | Finalized_validate_bug
  | Finalized_accepted of accepted

type proposal =
  | Proposal_fallback
  | Proposal_accepted of accepted

val finalized :
  response:Octra_consensus.C_driver.bundle_response_record option ->
  bundle:Bundle.accepted option ->
  finalized

val proposal :
  response:Octra_consensus.C_driver.bundle_response_record option ->
  bundle:Bundle.accepted option ->
  proposal