(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Bundle = Consensus_bundle_validation
module C_driver = Octra_consensus.C_driver
module Log = Octra_log

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

type proposal_bundle = {
  txs : Octra_core.Transaction.t list;
  receipts_json : string list;
}

type finalized_state =
  | Finalized_ready
  | Finalized_deferred

type finalized_deps = {
  cached_bundle : unit -> bool;
  header_has_empty_bundle : unit -> bool;
  store_empty_bundle : unit -> unit;
  validate_bundle : C_driver.bundle_response_record -> Bundle.accepted option;
  query_bundle :
    validate:(C_driver.bundle_response_record -> bool) ->
    C_driver.bundle_response_record option Lwt.t;
  store_accepted_bundle : accepted -> unit;
  queue_missing_bundle : epoch_id:int64 -> unit;
}

type proposal_deps = {
  cached_proposal_bundle : unit -> proposal_bundle option;
  local_preverify_bundle : unit -> proposal_bundle Lwt.t;
  driver_available : unit -> bool;
  validate_bundle : C_driver.bundle_response_record -> Bundle.accepted option;
  query_bundle :
    validate:(C_driver.bundle_response_record -> bool) ->
    C_driver.bundle_response_record option Lwt.t;
  store_proposal_bundle : accepted -> unit;
}

let accepted (response : C_driver.bundle_response_record) bundle =
  {
    responder_addr = response.responder_addr;
    bundle;
  }

let short14 s =
  String.sub s 0 (min 14 (String.length s))

let finalized ~response ~bundle =
  match response, bundle with
  | None, _ -> Finalized_missing
  | Some response, Some bundle -> Finalized_accepted (accepted response bundle)
  | Some _, None -> Finalized_validate_bug

let proposal ~response ~bundle =
  match response, bundle with
  | Some response, Some bundle -> Proposal_accepted (accepted response bundle)
  | _ -> Proposal_fallback

let proposal_bundle_of_accepted bundle =
  {
    txs = bundle.Bundle.txs;
    receipts_json = bundle.receipts_json;
  }

let fetch_finalized (deps : finalized_deps) ~epoch_id =
  let open Lwt.Syntax in
  Log.warn "consensus"
    "missed canonical bundle for finalized epoch = %Ld action = query_peers"
    epoch_id;
  let bundle_holder = ref None in
  let validate response =
    match deps.validate_bundle response with
    | None -> false
    | Some bundle ->
      bundle_holder := Some bundle;
      true
  in
  let* response = deps.query_bundle ~validate in
  match finalized ~response ~bundle:!bundle_holder with
  | Finalized_missing ->
    Log.error "consensus"
      "bundle fetch failed epoch = %Ld reason = no_valid_response"
      epoch_id;
    Lwt.return_unit
  | Finalized_validate_bug ->
    Log.error "consensus"
      "bundle fetch failed epoch = %Ld reason = validate_holder_unset"
      epoch_id;
    Lwt.return_unit
  | Finalized_accepted accepted ->
    deps.store_accepted_bundle accepted;
    Log.info "consensus"
      "bundle fetch success epoch = %Ld n = %d peer = %s"
      epoch_id
      (List.length accepted.bundle.Bundle.txs)
      (short14 accepted.responder_addr);
    Lwt.return_unit

let ensure_finalized (deps : finalized_deps) ~epoch_id =
  let open Lwt.Syntax in
  let* () =
    if deps.cached_bundle () then Lwt.return_unit
    else if deps.header_has_empty_bundle () then begin
      deps.store_empty_bundle ();
      Lwt.return_unit
    end else
      fetch_finalized deps ~epoch_id
  in
  if deps.cached_bundle () then
    Lwt.return Finalized_ready
  else begin
    deps.queue_missing_bundle ~epoch_id;
    Log.warn "consensus"
      "finalized epoch = %Ld deferred reason = canonical_bundle_unavailable"
      epoch_id;
    Lwt.return Finalized_deferred
  end

let local_proposal_fallback (deps : proposal_deps) ~reason ~local_tx_count
    ~expected_hash_count =
  Log.warn "consensus"
    "verify_proposal canonical_bundle = fallback reason = %s have = %d need = %d"
    reason local_tx_count expected_hash_count;
  deps.local_preverify_bundle ()

let fetch_proposal (deps : proposal_deps) ~local_tx_count ~expected_hash_count =
  let open Lwt.Syntax in
  let bundle_holder = ref None in
  let validate response =
    match deps.validate_bundle response with
    | None -> false
    | Some bundle ->
      bundle_holder := Some bundle;
      true
  in
  let* response = deps.query_bundle ~validate in
  match proposal ~response ~bundle:!bundle_holder with
  | Proposal_accepted accepted ->
    deps.store_proposal_bundle accepted;
    Log.info "consensus"
      "verify_proposal canonical_bundle = fetched n = %d peer = %s"
      (List.length accepted.bundle.Bundle.txs)
      (short14 accepted.responder_addr);
    Lwt.return (proposal_bundle_of_accepted accepted.bundle)
  | Proposal_fallback ->
    local_proposal_fallback deps
      ~reason:"fetch_failed"
      ~local_tx_count
      ~expected_hash_count

let ensure_proposal (deps : proposal_deps) ~epoch_id:_ ~proposal_id_short ~expected_hash_count
    ~local_tx_count ~missing_count ~hashes_empty =
  match deps.cached_proposal_bundle () with
  | Some bundle ->
    Log.info "consensus"
      "verify_proposal canonical_bundle = cached pid = %s n = %d"
      proposal_id_short
      (List.length bundle.txs);
    Lwt.return bundle
  | None when hashes_empty ->
    Lwt.return { txs = []; receipts_json = [] }
  | None when missing_count = 0 ->
    local_proposal_fallback deps
      ~reason:"local_complete"
      ~local_tx_count
      ~expected_hash_count
  | None ->
    Log.info "consensus"
      "verify_proposal canonical_bundle = fetch_missing missing = %d total = %d"
      missing_count expected_hash_count;
    if deps.driver_available () then
      fetch_proposal deps ~local_tx_count ~expected_hash_count
    else
      local_proposal_fallback deps
        ~reason:"no_driver"
        ~local_tx_count
        ~expected_hash_count