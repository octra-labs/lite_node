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
module C_driver = Octra_consensus.C_driver
module Bundle_fetch = Consensus_bundle_fetch

type deps = {
  write_finality : C_types.finalize -> unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle : proposal_id:string -> bool;
  cached_bundle_len : proposal_id:string -> int;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  store_empty_bundle : C_types.epoch_header -> unit;
  query_bundle :
    epoch_id:int64 ->
    proposal_id:string ->
    validate:(C_driver.bundle_response_record -> bool) ->
    C_driver.bundle_response_record option Lwt.t;
  store_accepted_bundle :
    proposal_id:string ->
    Bundle_fetch.accepted ->
    unit;
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  post_finalize : epoch_id:int64 -> proposed_root:string -> unit Lwt.t;
}

val run : deps -> C_types.finalize -> unit Lwt.t