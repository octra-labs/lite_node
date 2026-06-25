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
module C_driver = Octra_consensus.C_driver

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

let accepted (response : C_driver.bundle_response_record) bundle =
  {
    responder_addr = response.responder_addr;
    bundle;
  }

let finalized ~response ~bundle =
  match response, bundle with
  | None, _ -> Finalized_missing
  | Some response, Some bundle -> Finalized_accepted (accepted response bundle)
  | Some _, None -> Finalized_validate_bug

let proposal ~response ~bundle =
  match response, bundle with
  | Some response, Some bundle -> Proposal_accepted (accepted response bundle)
  | _ -> Proposal_fallback