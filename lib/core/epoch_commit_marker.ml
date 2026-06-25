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


type marker = {
  epoch_id : int;
  phase : string;
  ts : float;
}

let marker_path data_dir =
  Filename.concat data_dir "epoch_commit_in_progress.json"

let write_marker data_dir epoch_id phase =
  let path = marker_path data_dir in
  let tmp = path ^ ".tmp" in
  let j = `Assoc [
    "epoch_id", `Int epoch_id;
    "phase", `String phase;
    "ts", `Float (Unix.gettimeofday ());
  ] in
  let oc = open_out_bin tmp in
  Yojson.Safe.to_channel oc j;
  flush oc;
  (try Unix.fsync (Unix.descr_of_out_channel oc) with _ -> ());
  close_out oc;
  Unix.rename tmp path

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