(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module C_hash = Octra_consensus.C_hash
module C_driver = Octra_consensus.C_driver
module Bundle_fetch = Consensus_bundle_fetch
module Bundle_validation = Consensus_bundle_validation
module Log = Octra_log

type deps = {
  check_finality : C_types.finalize -> unit;
  write_finality : C_types.finalize -> unit;
  persist_finality_certificate : C_types.finalize -> unit;
  persist_finality_bundle :
    C_types.finalize ->
    Consensus_finality_journal.bundle ->
    unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle : proposal_id:string -> bool;
  cached_bundle_data :
    proposal_id:string ->
    (string list * Octra_core.Transaction.t list * string list) option;
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
  sleep : float -> unit Lwt.t;
  post_finalize : epoch_id:int64 -> proposed_root:string -> unit Lwt.t;
}

type node_deps = {
  check_finality : C_types.finalize -> unit;
  write_finality : C_types.finalize -> unit;
  persist_finality_certificate : C_types.finalize -> unit;
  persist_finality_bundle :
    C_types.finalize ->
    Consensus_finality_journal.bundle ->
    unit;
  chaos_after_finality_log : unit -> unit;
  cached_bundle : proposal_id:string -> bool;
  cached_bundle_data :
    proposal_id:string ->
    (string list * Octra_core.Transaction.t list * string list) option;
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
  sleep : float -> unit Lwt.t;
  post_finalize : epoch_id:int64 -> proposed_root:string -> unit Lwt.t;
}

let node_deps input =
  {
    check_finality = input.check_finality;
    write_finality = input.write_finality;
    persist_finality_certificate = input.persist_finality_certificate;
    persist_finality_bundle = input.persist_finality_bundle;
    chaos_after_finality_log = input.chaos_after_finality_log;
    cached_bundle = input.cached_bundle;
    cached_bundle_data = input.cached_bundle_data;
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
    sleep = input.sleep;
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
  }

let journal_bundle tx_hashes txs receipts_json =
  Consensus_finality_journal.{
    tx_hashes;
    txs;
    receipts_json;
  }

let persist_available_bundle (deps : deps) finalize proposal_id =
  match deps.cached_bundle_data ~proposal_id with
  | Some (tx_hashes, txs, receipts_json) ->
    deps.persist_finality_bundle
      finalize
      (journal_bundle tx_hashes txs receipts_json);
    true
  | None ->
    false

let rec await_bundle (deps : deps) header proposal_id delay =
  let open Lwt.Syntax in
  let* state =
    Bundle_fetch.ensure_finalized
      (bundle_deps deps header proposal_id)
      ~epoch_id:header.C_types.epoch_id
  in
  match state with
  | Bundle_fetch.Finalized_ready ->
    Lwt.return_unit
  | Bundle_fetch.Finalized_deferred ->
    let* () = deps.sleep delay in
    await_bundle deps header proposal_id (min 5.0 (delay *. 2.0))

let run (deps : deps) finalize =
  let open Lwt.Syntax in
  let header = finalize.C_types.header in
  let round = finalize.C_types.commit_round in
  let proposal_id = C_hash.proposal_id header in
  deps.check_finality finalize;
  deps.persist_finality_certificate finalize;
  let bundle_persisted =
    persist_available_bundle deps finalize proposal_id
  in
  deps.write_finality finalize;
  deps.chaos_after_finality_log ();
  Log.info "consensus"
    "event = finalized_certificate epoch = %Ld round = %d txs = %d"
    header.C_types.epoch_id
    round
    (deps.cached_bundle_len ~proposal_id);
  let* () = await_bundle deps header proposal_id 1.0 in
  if
    not bundle_persisted
    && not (persist_available_bundle deps finalize proposal_id)
  then
    failwith "finalized bundle ready without cached data";
  deps.post_finalize
    ~epoch_id:header.C_types.epoch_id
    ~proposed_root:header.C_types.proposed_state_root