(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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
  validate_bundle :
    Octra_consensus.C_driver.bundle_response_record ->
    Bundle.accepted option;
  query_bundle :
    validate:(Octra_consensus.C_driver.bundle_response_record -> bool) ->
    Octra_consensus.C_driver.bundle_response_record option Lwt.t;
  store_accepted_bundle : accepted -> unit;
}

type proposal_deps = {
  cached_proposal_bundle : unit -> proposal_bundle option;
  local_preverify_bundle : unit -> proposal_bundle Lwt.t;
  driver_available : unit -> bool;
  validate_bundle :
    Octra_consensus.C_driver.bundle_response_record ->
    Bundle.accepted option;
  query_bundle :
    validate:(Octra_consensus.C_driver.bundle_response_record -> bool) ->
    Octra_consensus.C_driver.bundle_response_record option Lwt.t;
  store_proposal_bundle : accepted -> unit;
}

val finalized :
  response:Octra_consensus.C_driver.bundle_response_record option ->
  bundle:Bundle.accepted option ->
  finalized

val proposal :
  response:Octra_consensus.C_driver.bundle_response_record option ->
  bundle:Bundle.accepted option ->
  proposal

val ensure_finalized :
  finalized_deps ->
  epoch_id:int64 ->
  finalized_state Lwt.t

val ensure_proposal :
  proposal_deps ->
  epoch_id:int64 ->
  proposal_id_short:string ->
  expected_hash_count:int ->
  local_tx_count:int ->
  missing_count:int ->
  hashes_empty:bool ->
  proposal_bundle Lwt.t