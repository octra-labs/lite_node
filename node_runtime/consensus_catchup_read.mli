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


type deps = {
  get_epoch_json : int -> string option;
  get_tx_by_txid : int64 -> (string * string) option;
  read_receipts : int -> string list;
  root_to_raw32 : string -> string;
}

val range :
  ?max_chunk:int ->
  ?max_bytes:int ->
  deps ->
  from_epoch:int64 ->
  max_epochs:int ->
  [ `Ok of Octra_consensus.C_codec.catchup_epoch_record list * int64 option
  | `NotFound
  | `Internal of string ]