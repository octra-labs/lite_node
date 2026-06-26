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


module C_types = Octra_consensus.C_types
module Finality_log = Octra_consensus.Finality_log
module Head_manifest = Octra_core.Head_manifest

type codec = {
  root_to_raw32 : string -> string;
  raw_to_hex : string -> string;
}

type empty_replay_plan =
  | Not_empty_replayable
  | Proposal_id_mismatch of {
      logged : string;
      computed : string;
    }
  | Arm_empty_replay of {
      finalize : C_types.finalize;
      proposer : Consensus_finalized_flow.proposer_info option;
      expected_root : string option;
      proposal_id : string;
      root_short : string;
    }

type deps = {
  chain_id : string;
  head : unit -> int;
  last_finality : unit -> Finality_log.entry option;
  cached_head : unit -> Head_manifest.t option;
  codec : codec;
  store_finalized : epoch:int -> C_types.finalize -> unit;
  store_proposer : Consensus_finalized_flow.proposer_info -> unit;
  store_expected_root : epoch:int -> root:string -> unit;
  store_empty_bundle : proposal_id:string -> unit;
  reset_proposal_state : unit -> unit;
  set_consensus_finalized : bool -> unit;
  normalize_next_epoch_for_head : source:string -> unit;
  mark_quarantine : string -> unit;
}

val plan_empty_replay :
  chain_id:string ->
  codec:codec ->
  head:Head_manifest.t option ->
  Finality_log.entry ->
  empty_replay_plan

val handle_startup : deps -> unit