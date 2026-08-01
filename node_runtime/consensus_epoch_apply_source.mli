(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types

type cached_bundle = string list * Transaction.t list * string list

type effect =
  | Store_empty_bundle of C_types.epoch_header

type selected = {
  txs : Transaction.t list;
  receipts_json : string list;
  preverify_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
  effects : effect list;
}

type fatal =
  | Override_preverify_failed of string
  | Missing_finalized_header
  | Receipt_root_mismatch of { proposal_id : string }
  | Missing_canonical_bundle of { proposal_id : string }
  | Outcome_invalid of string

type deps = {
  check_override_receipts :
    epoch_id:int ->
    receipts:string list ->
    Transaction.t list ->
    (unit, string) result;
  find_finalized : int -> C_types.finalize option;
  cached_bundle : string -> cached_bundle option;
  receipt_root_matches : C_types.epoch_header -> string list -> bool;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  staging_txs : unit -> Transaction.t list;
}

type node_deps = {
  check_override_receipts :
    epoch_id:int ->
    receipts:string list ->
    Transaction.t list ->
    (unit, string) result;
  find_finalized : int -> C_types.finalize option;
  cached_bundle : string -> cached_bundle option;
  receipt_root_matches : C_types.epoch_header -> string list -> bool;
  header_has_empty_bundle : C_types.epoch_header -> bool;
  staging_txs : unit -> Transaction.t list;
  store_empty_bundle : C_types.epoch_header -> unit;
  fatal : string -> unit;
  exit : unit -> Transaction.t list * string list;
}

type request = {
  epoch_id : int;
  override_ordered_txs : Transaction.t list option;
  override_receipts_json : string list option;
  consensus_mode : bool;
}

val proposal_id_short : string -> string

val fatal_lines : epoch_id:int -> fatal -> string list

val choose : deps -> request -> (selected, fatal) result

val run :
  deps ->
  request ->
  apply_effect:(effect -> unit) ->
  fatal:(string -> unit) ->
  exit:(unit -> Transaction.t list * string list) ->
  selected
val run_node :
  node_deps ->
  request ->
  selected