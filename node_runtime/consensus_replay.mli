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


type plan = {
  header : Octra_consensus.C_types.epoch_header;
  commit_round : int;
  finalize : Octra_consensus.C_types.finalize;
  txs : Octra_core.Transaction.t list;
  tx_hashes : string list;
  epoch : int;
  proposer_info : Octra_core.Epochlog.proposer_info option;
  expected_root : string option;
}

val parse_header :
  default_chain_id:string ->
  Yojson.Safe.t ->
  Octra_consensus.C_types.epoch_header * int

val parse_bundle :
  Yojson.Safe.t ->
  Octra_core.Transaction.t list

val build_plan :
  header:Octra_consensus.C_types.epoch_header ->
  commit_round:int ->
  txs:Octra_core.Transaction.t list ->
  plan

val load_plan :
  default_chain_id:string ->
  header_path:string ->
  bundle_path:string option ->
  plan