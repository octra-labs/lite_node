(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type route = Head | Range
type outcome = Good | Neutral | Busy | Failed
type row = { source : string; route : route; faults : int; retry : float; id : int }
type t = {
  now : unit -> float;
  jitter : unit -> float;
  mutable rows : row list;
  mutable serial : int;
}

let create ?(now = fun () -> Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9) ?jitter () =
  let jitter = match jitter with
    | Some jitter -> jitter
    | None ->
      let rng = Random.State.make_self_init () in
      fun () -> 0.5 +. Random.State.float rng 0.5 in
  { now; jitter; rows = []; serial = 0 }

let same source route row = row.source = source && row.route = route

let plan t route sources =
  t.rows <- List.filter (fun row -> List.mem row.source sources) t.rows;
  let retry source = match List.find_opt (same source route) t.rows with
    | None -> 0.
    | Some row -> row.retry in
  let ready, paused = List.partition (fun source -> retry source <= t.now ()) sources in
  let paused = List.stable_sort (fun a b -> Float.compare (retry a) (retry b)) paused in
  ready, (match paused with [] -> [] | first :: _ -> [first])

let start t route source =
  if t.serial = max_int then invalid_arg "source request id exhausted";
  t.serial <- t.serial + 1;
  let row = match List.find_opt (same source route) t.rows with
    | Some row -> { row with id = t.serial }
    | None -> { source; route; faults = 0; retry = 0.; id = t.serial } in
  t.rows <- row :: List.filter (fun row -> not (same source route row)) t.rows;
  row

let finish t row outcome =
  match List.find_opt (same row.source row.route) t.rows with
  | Some current when current.id = row.id ->
    let row = match outcome with
      | Good -> { row with faults = 0; retry = 0. }
      | Neutral -> row
      | Busy -> { row with retry = t.now () +. 3. }
      | Failed ->
        let faults = min 6 (row.faults + 1) in
        let delay = min 30. (float_of_int (1 lsl (faults - 1))) in
        let jitter = t.jitter () in
        let jitter = if Float.is_finite jitter then max 0.5 (min 1. jitter) else 1. in
        { row with faults; retry = t.now () +. delay *. jitter } in
    t.rows <- { row with id = 0 }
      :: List.filter (fun item -> not (same row.source row.route item)) t.rows
  | _ -> ()

let contains text word =
  let rec loop i =
    i + String.length word <= String.length text
    && (String.sub text i (String.length word) = word || loop (i + 1)) in
  loop 0

let error reason =
  if List.exists (contains reason)
      ["state_sync_range_busy"; "state_sync_busy"; "committed state changed during read"]
  then Busy
  else if List.exists (contains reason)
      ["invalid JSON"; "not_found"; "HTTP 404"; "range part"; "range first part"]
  then Neutral
  else Failed