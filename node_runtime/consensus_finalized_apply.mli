(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_types = Octra_consensus.C_types
module C_driver = Octra_consensus.C_driver
module Bundle_fetch = Consensus_bundle_fetch

type deps = {
  check_finality : C_types.finalize -> unit;
  write_finality : C_types.finalize -> unit;
  persist_finality_certificate :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    unit;
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
  bundle_wait_timeout_seconds : float;
  bundle_wait_expired : epoch_id:int64 -> unit;
  bundle_wait_recovered : epoch_id:int64 -> unit;
  post_finalize : epoch_id:int64 -> proposed_root:string -> unit Lwt.t;
}

type node_deps = {
  check_finality : C_types.finalize -> unit;
  write_finality : C_types.finalize -> unit;
  persist_finality_certificate :
    validator_set:C_types.validator_set ->
    C_types.finalize ->
    unit;
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
  bundle_wait_timeout_seconds : float;
  bundle_wait_expired : epoch_id:int64 -> unit;
  bundle_wait_recovered : epoch_id:int64 -> unit;
  post_finalize : epoch_id:int64 -> proposed_root:string -> unit Lwt.t;
}

val node_deps :
  node_deps ->
  deps

val run :
  deps ->
  validator_set:C_types.validator_set ->
  C_types.finalize ->
  unit Lwt.t