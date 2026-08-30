(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type lease = {
  snapshot_id : string;
  last_seen : float;
}

let marker_name = "download.lease"
let renew_interval = 60.
let active_window = 900.
let max_snapshot_age = 86_400.
let retained_limit = 2

let marker snapshot = Filename.concat snapshot marker_name

let regular_file path =
  try (Unix.lstat path).Unix.st_kind = Unix.S_REG
  with Unix.Unix_error _ -> false

let renew ~now snapshot =
  let path = marker snapshot in
  try
    let should_write =
      if not (Sys.file_exists path) then true
      else if not (regular_file path) then false
      else now -. (Unix.stat path).Unix.st_mtime >= renew_interval
    in
    if not should_write then
      if Sys.file_exists path && not (regular_file path) then
        Error "state sync lease marker is not a regular file"
      else
        Ok ()
    else begin
      let descriptor =
        Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT] 0o600
      in
      Unix.close descriptor;
      Unix.utimes path now now;
      Ok ()
    end
  with exn ->
    Error (Printexc.to_string exn)

let read ~now ~published_at ~snapshot_id snapshot =
  let path = marker snapshot in
  if not (regular_file path) then None
  else
    try
      let last_seen = (Unix.stat path).Unix.st_mtime in
      let fresh = last_seen <= now +. renew_interval
        && now -. last_seen <= active_window
      in
      let bounded = published_at <= now +. renew_interval
        && now -. published_at <= max_snapshot_age
      in
      if fresh && bounded then Some { snapshot_id; last_seen }
      else None
    with Unix.Unix_error _ -> None

let retained leases =
  leases
  |> List.sort (fun left right ->
       let order = Float.compare right.last_seen left.last_seen in
       if order <> 0 then order
       else String.compare left.snapshot_id right.snapshot_id)
  |> List.filteri (fun index _ -> index < retained_limit)
  |> List.map (fun value -> value.snapshot_id)