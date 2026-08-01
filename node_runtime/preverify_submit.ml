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

let short16 value =
  String.sub value 0 (min 16 (String.length value))

let oldest_non_pending entries =
  entries
  |> List.filter (fun entry -> not entry.cache_pending)
  |> List.sort (fun a b -> compare a.cache_ts b.cache_ts)

let rec take_keys n entries =
  match n, entries with
  | _, _ when n <= 0 -> []
  | _, [] -> []
  | n, entry :: rest -> entry.cache_key :: take_keys (n - 1) rest

let cache_prune_plan ~now ~ttl ~max_entries entries =
  let expired =
    List.filter
      (fun entry ->
        let age = now -. entry.cache_ts in
        not entry.cache_pending && age > ttl)
      entries
  in
  let expired_keys = List.map (fun entry -> entry.cache_key) expired in
  let remaining =
    List.filter
      (fun entry -> not (List.mem entry.cache_key expired_keys))
      entries
  in
  let completed =
    List.filter (fun entry -> not entry.cache_pending) remaining
  in
  let overflow_needed = List.length completed - max_entries in
  let overflow =
    if overflow_needed > 0 then
      take_keys overflow_needed (oldest_non_pending completed)
    else []
  in
  { prune_expired = expired_keys; prune_overflow = overflow }

let hard_cap_plan ~max_entries entries =
  let cap_threshold = max_entries + max_entries / 5 in
  if List.length entries >= cap_threshold then
    let requested_drop = List.length entries - max_entries + 1 in
    {
      hard_cap_drop = take_keys requested_drop (oldest_non_pending entries);
      hard_cap_requested_drop = requested_drop;
    }
  else
    { hard_cap_drop = []; hard_cap_requested_drop = 0 }

let result_of_ranges ~sender_enc_snapshot = function
  | Error (Tx_view.Preverify_invalid _) ->
    Checked { delta_ok = false; balance_ok = false; sender_enc_snapshot }
  | Error (Tx_view.Preverify_unavailable reason) ->
    Unavailable reason
  | Ok (delta_ok, balance_ok) ->
    Checked { delta_ok; balance_ok; sender_enc_snapshot }

let launch (io : io) =
  io.start_task io.tx_hash (fun () ->
    let open Lwt.Syntax in
    let t0 = io.now () in
    Log.info "pre_verify" "start tx = %s from = %s" (short16 io.tx_hash) io.sender;
    let* pk_opt = io.get_pvac_pubkey io.sender in
    match pk_opt with
    | Some pubkey_blob ->
      Log.info "pre_verify" "pubkey loaded bytes = %d elapsed = %.1fs"
        (String.length pubkey_blob)
        (io.now () -. t0);
      let sender_enc = io.sender_enc io.sender in
      let* ranges =
        io.verify_ranges ~pubkey_blob ~sender_enc io.ptd in
      begin
        match ranges with
        | Error (Tx_view.Preverify_invalid e) ->
          Log.warn "pre_verify" "bad pvac pubkey addr = %s reason = %s" io.sender e
        | Error (Tx_view.Preverify_unavailable reason) ->
          Log.warn "pre_verify" "unavailable addr = %s reason = %s"
            io.sender
            (Tx_view.preverify_unavailable_message reason)
        | Ok (delta_ok, balance_ok) ->
          Log.info "pre_verify" "done delta_ok = %b balance_ok = %b elapsed = %.1fs"
            delta_ok
            balance_ok
            (io.now () -. t0)
      end;
      Lwt.return (result_of_ranges ~sender_enc_snapshot:sender_enc ranges)
    | None ->
      Log.warn "pre_verify" "no pvac pubkey addr = %s" io.sender;
      Lwt.return
        (result_of_ranges
           ~sender_enc_snapshot:(io.sender_enc io.sender)
           (Error (Tx_view.Preverify_invalid "pvac pubkey missing"))))

let launch_for_tx (launcher : launcher) ~tx_hash tx =
  match Tx_view.preverify_stealth_payload tx with
  | None ->
    Unmanaged
  | Some ptd ->
    launch {
      tx_hash;
      sender = tx.Octra_core.Transaction.from;
      ptd;
      get_pvac_pubkey = launcher.get_pvac_pubkey;
      sender_enc = launcher.sender_enc;
      start_task = launcher.start_task;
      verify_ranges = launcher.verify_ranges;
      now = launcher.now;
    }