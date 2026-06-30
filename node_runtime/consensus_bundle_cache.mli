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

type encoded = string list * string list * string list

type decoded = {
  tx_hashes : string list;
  txs : Transaction.t list;
  receipts_json : string list;
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

val create : cap:int -> t

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

val decode : encoded -> (decoded, string) result

val parse_txs : encoded -> (Transaction.t list, string) result

val freeze_key : epoch_id:int64 -> round:int -> string

val freeze : t -> string -> frozen -> unit

val find_frozen : t -> string -> frozen option

val prune_frozen : t -> finalized_epoch:int64 -> unit