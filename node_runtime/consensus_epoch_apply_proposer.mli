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


module Epochlog = Octra_core.Epochlog

type source =
  | Env
  | Override
  | Pending
  | Finalized_header
  | Epochlog_disk
  | Round_robin_fallback

type missing =
  | Missing_consensus_proposer
  | Missing_fallback_validator

type selected = {
  source : source;
  proposer : Epochlog.proposer_info;
}

type request = {
  epoch_id : int;
  consensus_mode : bool;
  active_validators : string list;
  env_fee_recipient : string option;
  override_proposer : Epochlog.proposer_info option;
  pending_proposer : Epochlog.proposer_info option;
  finalized_header_proposer : Epochlog.proposer_info option;
  disk_epoch : Epochlog.epoch_header option;
}

val source_label : source -> string
val valid_proposer : Epochlog.proposer_info -> bool
val proposer_from_addr : ?commit_round:int -> string -> Epochlog.proposer_info option
val proposer_from_disk_epoch : Epochlog.epoch_header -> Epochlog.proposer_info option
val choose : request -> (selected, missing) result