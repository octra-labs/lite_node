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
module C_hash = Octra_consensus.C_hash
module C_driver = Octra_consensus.C_driver
module Bundle_fetch = Consensus_bundle_fetch
module Bundle_validation = Consensus_bundle_validation
module Log = Octra_log

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

type node_deps = {
  write_finality : C_types.finalize -> unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle : proposal_id:string -> bool;
  cached_bundle_len : proposal_id:string -> int;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  store_empty_bundle : C_types.epoch_header -> unit;
  driver : unit -> C_driver.t option;
  set_proposal : Octra_core.Transaction.t list -> string list -> unit;
  store_proposal_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Octra_core.Transaction.t list ->
    receipts_json:string list ->
    unit;
  queue_missing_bundle : target_epoch:int64 -> reason:string -> unit;
  post_finalize : epoch_id:int64 -> proposed_root:string -> unit Lwt.t;
}

let node_deps input =
  {
    write_finality = input.write_finality;
    chaos_after_finality_log = input.chaos_after_finality_log;
    cached_bundle = input.cached_bundle;
    cached_bundle_len = input.cached_bundle_len;
    header_has_empty_bundle = input.header_has_empty_bundle;
    store_empty_bundle = input.store_empty_bundle;
    query_bundle = (fun ~epoch_id ~proposal_id ~validate ->
      match input.driver () with
      | None -> Lwt.return_none
      | Some driver ->
        C_driver.query_bundle
          driver
          ~epoch_id
          ~proposal_id
          ~timeout_seconds:5.0
          ~validate);
    store_accepted_bundle = (fun ~proposal_id accepted ->
      let bundle = accepted.Bundle_fetch.bundle in
      input.set_proposal bundle.txs bundle.tx_hashes;
      input.store_proposal_bundle
        ~proposal_id
        ~tx_hashes:bundle.tx_hashes
        ~txs:bundle.txs
        ~receipts_json:bundle.receipts_json);
    queue_missing_bundle = input.queue_missing_bundle;
    post_finalize = input.post_finalize;
  }

let bundle_deps (deps : deps) header proposal_id =
  Bundle_fetch.{
    cached_bundle = (fun () ->
      deps.cached_bundle ~proposal_id);
    header_has_empty_bundle = (fun () ->
      deps.header_has_empty_bundle header);
    store_empty_bundle = (fun () ->
      deps.store_empty_bundle header);
    validate_bundle = (fun response ->
      match Bundle_validation.finalized ~header response with
      | Ok bundle -> Some bundle
      | Error _ -> None);
    query_bundle = (fun ~validate ->
      deps.query_bundle
        ~epoch_id:header.C_types.epoch_id
        ~proposal_id
        ~validate);
    store_accepted_bundle = (fun accepted ->
      deps.store_accepted_bundle ~proposal_id accepted);
    queue_missing_bundle = (fun ~epoch_id ->
      deps.queue_missing_bundle
        ~target_epoch:epoch_id
        ~reason:"finalized_bundle_missing");
  }

let run (deps : deps) finalize =
  let open Lwt.Syntax in
  let header = finalize.C_types.header in
  let round = finalize.C_types.commit_round in
  let proposal_id = C_hash.proposal_id header in
  Log.info "consensus"
    "BFT finalized epoch = %Ld round = %d txs = %d"
    header.C_types.epoch_id
    round
    (deps.cached_bundle_len ~proposal_id);
  deps.write_finality finalize;
  deps.chaos_after_finality_log ();
  let* bundle_state =
    Bundle_fetch.ensure_finalized
      (bundle_deps deps header proposal_id)
      ~epoch_id:header.C_types.epoch_id
  in
  match bundle_state with
  | Bundle_fetch.Finalized_deferred ->
    Lwt.return_unit
  | Bundle_fetch.Finalized_ready ->
    deps.post_finalize
      ~epoch_id:header.C_types.epoch_id
      ~proposed_root:header.C_types.proposed_state_root