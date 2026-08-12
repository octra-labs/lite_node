(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type marker = {
  epoch_id : int;
  phase : string;
  ts : float;
}

let marker_path data_dir =
  Filename.concat data_dir "epoch_commit_in_progress.json"

let write_marker data_dir epoch_id phase =
  let path = marker_path data_dir in
  let staged = path ^ ".staged" in
  let j = `Assoc [
    "epoch_id", `Int epoch_id;
    "phase", `String phase;
    "ts", `Float (Unix.gettimeofday ());
  ] in
  let oc = open_out_bin staged in
  Yojson.Safe.to_channel oc j;
  flush oc;
  (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ());
  close_out oc;
  Unix.rename staged path

let clear_marker data_dir =
  let path = marker_path data_dir in
  if Sys.file_exists path then (try Sys.remove path with _ -> ())

let read_marker data_dir =
  let path = marker_path data_dir in
  if not (Sys.file_exists path) then None
  else
    try
      let j = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      Some {
        epoch_id = j |> member "epoch_id" |> to_int;
        phase = j |> member "phase" |> to_string;
        ts = j |> member "ts" |> to_number;
      }
    with _ -> None