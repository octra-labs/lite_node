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


module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types

type cached_bundle = string list * Transaction.t list * string list

type effect =
  | Remove_processed of string list
  | Store_empty_bundle of C_types.epoch_header

type selected = {
  txs : Transaction.t list;
  receipts_json : string list;
  effects : effect list;
}

type fatal =
  | Override_preverify_failed of string
  | Missing_finalized_header
  | Receipt_root_mismatch of { proposal_id : string }
  | Missing_canonical_bundle of { proposal_id : string }

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
  remove_processed : string list -> unit;
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
  Transaction.t list * string list
val run_node :
  node_deps ->
  request ->
  Transaction.t list * string list