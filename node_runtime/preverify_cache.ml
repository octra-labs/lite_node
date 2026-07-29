(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result = Preverify_submit.result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type state =
  | Missing
  | Pending
  | Ready
  | Failed of string

type gate =
  | Ready_gate
  | Failed_gate of string
  | Timeout_gate of string
  | Defer_gate of {
    next_count : int;
    status : string;
  }

let cache : (string, result Lwt.t) Hashtbl.t = Hashtbl.create 100

let cache_ts : (string, float) Hashtbl.t = Hashtbl.create 100

let pending = ref 0

let float_env name ~fallback ~min_value ~max_value =
  match Sys.getenv_opt name with
  | None -> fallback
  | Some raw ->
    try
      let value = float_of_string raw in
      match classify_float value with
      | FP_nan | FP_infinite -> fallback
      | FP_normal | FP_subnormal | FP_zero ->
        if value < min_value || value > max_value then fallback else value
    with _ -> fallback

let int_env name ~fallback ~min_value ~max_value =
  match Sys.getenv_opt name with
  | None -> fallback
  | Some raw ->
    try
      let value = int_of_string raw in
      if value < min_value || value > max_value then fallback else value
    with _ -> fallback

let cache_ttl () =
  float_env "OCTRA_PREVERIFY_CACHE_TTL"
    ~fallback:45.
    ~min_value:1.
    ~max_value:3_600.

let pending_ceiling () =
  float_env "OCTRA_PREVERIFY_PENDING_CEILING"
    ~fallback:90.
    ~min_value:1.
    ~max_value:7_200.

let configured_max_entries () =
  int_env "OCTRA_PREVERIFY_CACHE_MAX"
    ~fallback:64
    ~min_value:1
    ~max_value:4_096

let pending_max () =
  int_env "OCTRA_PREVERIFY_PENDING_MAX"
    ~fallback:2
    ~min_value:1
    ~max_value:16

let now () =
  Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9

let pending_count () =
  !pending

let has_capacity () =
  !pending < pending_max ()

let gc_on_finish () =
  match Sys.getenv_opt "OCTRA_PREVERIFY_GC_ON_FINISH" with
  | Some "0" | Some "false" ->
    false
  | _ ->
    true

let start_task f =
  incr pending;
  Lwt.finalize f (fun () ->
    pending := max 0 (!pending - 1);
    if gc_on_finish () then Gc.major ();
    Lwt.return_unit)

let find hash =
  Hashtbl.find_opt cache hash

let remove hash =
  Hashtbl.remove cache hash;
  Hashtbl.remove cache_ts hash

let entry_is_pending hash =
  match find hash with
  | Some task ->
    Lwt.state task = Lwt.Sleep
  | None ->
    false

let entries () =
  Hashtbl.fold
    (fun key ts acc ->
      Preverify_submit.{
        cache_key = key;
        cache_ts = ts;
        cache_pending = entry_is_pending key;
      } :: acc)
    cache_ts
    []

let drop keys =
  List.iter remove keys

let prune ?(ttl = -1.0) ?(max_entries = -1) () =
  let ttl = if ttl < 0.0 then cache_ttl () else ttl in
  let max_entries =
    if max_entries < 0 then configured_max_entries () else max_entries
  in
  let pending_ceiling = pending_ceiling () in
  let now = now () in
  let plan =
    Preverify_submit.cache_prune_plan
      ~now
      ~ttl
      ~max_entries
      ~pending_ceiling
      (entries ())
  in
  drop plan.Preverify_submit.prune_expired;
  drop plan.prune_overflow;
  let n_expired = List.length plan.prune_expired in
  if n_expired > 0 then
    Log.info
      "pre_verify"
      "prune expired = %d ttl = %.0fs size = %d"
      n_expired
      ttl
      (Hashtbl.length cache)

let insert_with_cap hash result_promise =
  let now = now () in
  let max_entries = configured_max_entries () in
  let plan =
    Preverify_submit.hard_cap_plan
      ~max_entries
      (entries ())
  in
  if plan.Preverify_submit.hard_cap_requested_drop > 0 then begin
    drop plan.hard_cap_drop;
    Log.warn
      "pre_verify"
      "hard cap triggered dropped = %d size = %d"
      plan.hard_cap_requested_drop
      (Hashtbl.length cache_ts)
  end;
  Hashtbl.replace cache hash result_promise;
  Hashtbl.replace cache_ts hash now

let state hash =
  match find hash with
  | Some task ->
    begin
      match Lwt.state task with
      | Lwt.Return _ ->
        Ready
      | Lwt.Fail exn ->
        Failed (Printexc.to_string exn)
      | Lwt.Sleep ->
        Pending
    end
  | None ->
    Missing

let gate ~state ~defer_count ~max_defer =
  match state with
  | Ready ->
    Ready_gate
  | Failed reason ->
    Failed_gate reason
  | Missing | Pending ->
    let next_count = defer_count + 1 in
    if next_count > max_defer then
      let reason =
        match state with
        | Missing -> "task not in cache"
        | Pending -> "still running"
        | Ready | Failed _ -> "unexpected cache state"
      in
      Timeout_gate reason
    else
      let status =
        match state with
        | Missing -> "missing"
        | Pending -> "pending"
        | Ready | Failed _ -> "unexpected"
      in
      Defer_gate { next_count; status }

let ready_result hash ~sender_enc_snapshot =
  match find hash with
  | Some task ->
    begin
      match Lwt.state task with
      | Lwt.Return result
        when String.equal result.sender_enc_snapshot sender_enc_snapshot ->
        Some result
      | _ ->
        None
    end
  | None ->
    None

let retain keep =
  Hashtbl.filter_map_inplace
    (fun key value -> if keep key then Some value else None)
    cache;
  Hashtbl.filter_map_inplace
    (fun key ts -> if Hashtbl.mem cache key then Some ts else None)
    cache_ts