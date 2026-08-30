(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Transaction = Octra_core.Transaction

type encoded = string list * string list * string list

type decoded = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
  rejections : Octra_core.Tx_outcome.rejection list;
}

type frozen = {
  header : Octra_consensus.C_types.epoch_header;
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
}

type stats = {
  stores : int;
  hits : int;
  misses : int;
  evictions : int;
  cache_size : int;
  fifo_size : int;
}

type cached =
  | Missing
  | Decode_error of string
  | Cached of decoded

type t

type node_runtime = {
  cached_bundle : string -> (string list * Transaction.t list * string list) option;
  store_bundle :
    proposal_id:string ->
    tx_hashes:string list ->
    txs:Transaction.t list ->
    receipts_json:string list ->
    unit;
  receipt_root_matches :
    Octra_consensus.C_types.epoch_header ->
    string list ->
    bool;
  header_has_empty_bundle :
    Octra_consensus.C_types.epoch_header ->
    bool;
  store_empty_bundle :
    Octra_consensus.C_types.epoch_header ->
    unit;
  store_empty_proposal : proposal_id:string -> unit;
  lookup_raw : string -> encoded option;
}

val create : cap:int -> t

val create_with_limits :
  cap:int ->
  shared_cap:int ->
  shared_limit:int ->
  t

val share :
  t ->
  Transaction.t list ->
  string list

val find_shared :
  t ->
  string ->
  Transaction.t option

val stats : t -> stats

val store :
  t ->
  pid:string ->
  tx_hashes:string list ->
  txs:Transaction.t list ->
  receipts_json:string list ->
  stats option

val peek_raw : t -> string -> encoded option

val lookup_raw : t -> string -> encoded option

val cached : t -> string -> cached

val store_with_log :
  t ->
  pid:string ->
  tx_hashes:string list ->
  txs:Transaction.t list ->
  receipts_json:string list ->
  unit

val cached_with_log :
  t ->
  string ->
  decoded option

val receipt_root_matches :
  Octra_consensus.C_types.epoch_header ->
  string list ->
  bool

val header_has_empty_bundle :
  Octra_consensus.C_types.epoch_header ->
  bool

val store_empty_header_with_log :
  t ->
  Octra_consensus.C_types.epoch_header ->
  unit

val node_runtime :
  t ->
  node_runtime

val decode : encoded -> (decoded, string) result

val parse_txs : encoded -> (Transaction.t list, string) result

val freeze_key : epoch_id:int64 -> round:int -> string

val freeze : t -> string -> frozen -> unit

val find_frozen : t -> string -> frozen option

val prune_frozen : t -> finalized_epoch:int64 -> unit

type preverify_purpose =
  | Build_proposal
  | Validate_proposal

val preverify_item_key :
  purpose:preverify_purpose ->
  state_root:string ->
  tx_hash:string ->
  string

val run_preverify_once :
  t ->
  purpose:preverify_purpose ->
  state_root:string ->
  tx_hashes:string list ->
  txs:Transaction.t list ->
  (string ->
   Transaction.t list ->
   Octra_core.Preverify_worker.batch Lwt.t) ->
  Octra_core.Preverify_worker.batch Lwt.t