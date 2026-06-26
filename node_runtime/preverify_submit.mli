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


type result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type cache_entry = {
  cache_key : string;
  cache_ts : float;
  cache_pending : bool;
}

type cache_prune_plan = {
  prune_expired : string list;
  prune_overflow : string list;
}

type hard_cap_plan = {
  hard_cap_drop : string list;
  hard_cap_requested_drop : int;
}

type io = {
  tx_hash : string;
  sender : string;
  ptd : Octra_core.Crypto.PrivateTransferV4.t;
  get_pvac_pubkey : string -> string option Lwt.t;
  sender_enc : string -> string;
  start_task : (unit -> result Lwt.t) -> result Lwt.t;
  insert_task : string -> result Lwt.t -> unit;
  verify_ranges :
    pubkey_blob:string ->
    sender_enc:string ->
    Octra_core.Crypto.PrivateTransferV4.t ->
    ((bool * bool), string) Stdlib.result Lwt.t;
  now : unit -> float;
}

type launcher = {
  get_pvac_pubkey : string -> string option Lwt.t;
  sender_enc : string -> string;
  start_task : (unit -> result Lwt.t) -> result Lwt.t;
  insert_task : string -> result Lwt.t -> unit;
  verify_ranges :
    pubkey_blob:string ->
    sender_enc:string ->
    Octra_core.Crypto.PrivateTransferV4.t ->
    ((bool * bool), string) Stdlib.result Lwt.t;
  now : unit -> float;
}

val result_of_ranges :
  sender_enc_snapshot:string ->
  ((bool * bool), string) Stdlib.result ->
  result

val cache_prune_plan :
  now:float ->
  ttl:float ->
  max_entries:int ->
  pending_ceiling:float ->
  cache_entry list ->
  cache_prune_plan

val hard_cap_plan :
  max_entries:int ->
  cache_entry list ->
  hard_cap_plan

val launch :
  io ->
  unit

val launch_for_tx :
  launcher ->
  tx_hash:string ->
  Octra_core.Transaction.t ->
  unit