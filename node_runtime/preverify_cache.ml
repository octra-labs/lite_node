(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type result = Preverify_submit.result = {
  delta_ok : bool;
  balance_ok : bool;
  sender_enc_snapshot : string;
}

type task_result = Preverify_submit.task_result =
  | Checked of result
  | Unavailable of Tx_view.preverify_unavailable

type state =
  | Missing
  | Pending
  | Ready
  | Unavailable_state of Tx_view.preverify_unavailable
  | Failed of string

type gate =
  | Ready_gate
  | Failed_gate of string
  | Timeout_gate of string
  | Unavailable_gate of {
    next_count : int;
    reason : Tx_view.preverify_unavailable;
  }
  | Defer_gate of {
    next_count : int;
    status : string;
  }

let cache : (string, task_result Lwt.t) Hashtbl.t = Hashtbl.create 100

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

let configured_max_entries () =
  int_env "OCTRA_PREVERIFY_CACHE_MAX"
    ~fallback:64
    ~min_value:1
    ~max_value:4_096

let pending_max () =
  Octra_core.Resource_lanes.preverify_speculative_queue_limit

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

let find hash =
  Hashtbl.find_opt cache hash

let remove hash =
  begin
    match find hash with
    | Some task when Lwt.is_sleeping task -> Lwt.cancel task
    | Some _
    | None -> ()
  end;
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
  let now = now () in
  let plan =
    Preverify_submit.cache_prune_plan
      ~now
      ~ttl
      ~max_entries
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

let start_task hash f =
  if Hashtbl.mem cache hash then
    Preverify_submit.Existing
  else if not (has_capacity ()) then
    Preverify_submit.Busy
  else begin
    incr pending;
    let task =
      Lwt.finalize
        (fun () ->
          try f ()
          with exn -> Lwt.fail exn)
        (fun () ->
          pending := max 0 (!pending - 1);
          if gc_on_finish () then Gc.major ();
          Lwt.return_unit)
    in
    insert_with_cap hash task;
    Preverify_submit.Started
  end

let state hash =
  match find hash with
  | Some task ->
    begin
      match Lwt.state task with
      | Lwt.Return (Checked _) ->
        Ready
      | Lwt.Return (Unavailable reason) ->
        Unavailable_state reason
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
  | Missing | Pending | Unavailable_state _ ->
    let next_count = defer_count + 1 in
    if next_count > max_defer then
      let reason =
        match state with
        | Missing -> "task not in cache"
        | Pending -> "still running"
        | Unavailable_state reason ->
          Tx_view.preverify_unavailable_message reason
        | Ready | Failed _ -> "unexpected cache state"
      in
      Timeout_gate reason
    else
      match state with
      | Unavailable_state reason -> Unavailable_gate { next_count; reason }
      | Missing -> Defer_gate { next_count; status = "missing" }
      | Pending -> Defer_gate { next_count; status = "pending" }
      | Ready | Failed _ -> Defer_gate { next_count; status = "unexpected" }

let ready_result hash ~sender_enc_snapshot =
  match find hash with
  | Some task ->
    begin
      match Lwt.state task with
      | Lwt.Return (Checked result)
        when String.equal result.sender_enc_snapshot sender_enc_snapshot ->
        Some result
      | _ ->
        None
    end
  | None ->
    None

let retain keep =
  Hashtbl.fold
    (fun key _ dropped -> if keep key then dropped else key :: dropped)
    cache
    []
  |> drop