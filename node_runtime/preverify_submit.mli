(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type task_result =
  | Checked of result
  | Unavailable of Tx_view.preverify_unavailable

type admit =
  | Unmanaged
  | Started
  | Existing
  | Busy

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
  start_task : string -> (unit -> task_result Lwt.t) -> admit;
  verify_ranges :
    pubkey_blob:string ->
    sender_enc:string ->
    Octra_core.Crypto.PrivateTransferV4.t ->
    ((bool * bool), Tx_view.preverify_error) Stdlib.result Lwt.t;
  now : unit -> float;
}

type launcher = {
  get_pvac_pubkey : string -> string option Lwt.t;
  sender_enc : string -> string;
  start_task : string -> (unit -> task_result Lwt.t) -> admit;
  verify_ranges :
    pubkey_blob:string ->
    sender_enc:string ->
    Octra_core.Crypto.PrivateTransferV4.t ->
    ((bool * bool), Tx_view.preverify_error) Stdlib.result Lwt.t;
  now : unit -> float;
}

val result_of_ranges :
  sender_enc_snapshot:string ->
  ((bool * bool), Tx_view.preverify_error) Stdlib.result ->
  task_result

val cache_prune_plan :
  now:float ->
  ttl:float ->
  max_entries:int ->
  cache_entry list ->
  cache_prune_plan

val hard_cap_plan :
  max_entries:int ->
  cache_entry list ->
  hard_cap_plan

val launch :
  io ->
  admit

val launch_for_tx :
  launcher ->
  tx_hash:string ->
  Octra_core.Transaction.t ->
  admit