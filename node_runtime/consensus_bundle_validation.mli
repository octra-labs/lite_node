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


module Transaction = Octra_core.Transaction

type accepted = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
}

val finalized :
  header:Octra_consensus.C_types.epoch_header ->
  Octra_consensus.C_driver.bundle_response_record ->
  (accepted, string) result

val proposal :
  header:Octra_consensus.C_types.epoch_header ->
  expected_hashes:string list ->
  Octra_consensus.C_driver.bundle_response_record ->
  (accepted, string) result