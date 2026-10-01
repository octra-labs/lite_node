(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type marker = {
  epoch_id : int;
  phase : string;
  ts : float;
}

let marker_path data_dir =
  Filename.concat data_dir "epoch_commit_in_progress.json"

let recovery_path data_dir =
  Filename.concat data_dir "recovery_required"

let sync_directory data_dir =
  let fd = Unix.openfile data_dir [Unix.O_RDONLY] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd)
    (fun () -> Unix.fsync fd)

let write_file data_dir path bytes =
  let staged = path ^ ".staged" in
  let channel = open_out_bin staged in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
    output_string channel bytes;
    flush channel;
    Unix.fsync (Unix.descr_of_out_channel channel);
    close_out channel);
  Unix.rename staged path;
  sync_directory data_dir

let clear_file data_dir path =
  (try Unix.unlink path with
   | Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  sync_directory data_dir

let file_exists path =
  match Unix.lstat path with
  | { Unix.st_kind = Unix.S_REG; _ } -> true
  | _ -> failwith "storage marker is not a regular file"
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> false

let valid_phase = function
  | "stage_batch_begin" | "wal_written" | "begin" | "chaindata_begin"
  | "chaindata_committed" | "irmin_begin" | "irmin_committed" -> true
  | _ -> false

let validate marker =
  if marker.epoch_id < 0 || not (valid_phase marker.phase)
     || not (Float.is_finite marker.ts) || marker.ts < 0. then
    failwith "invalid epoch commit marker";
  marker

let write_marker data_dir epoch_id phase =
  let marker = validate { epoch_id; phase; ts = Unix.gettimeofday () } in
  let j = `Assoc [
    "epoch_id", `Int marker.epoch_id;
    "phase", `String marker.phase;
    "ts", `Float marker.ts;
  ] in
  write_file data_dir (marker_path data_dir) (Yojson.Safe.to_string j)

let clear_marker data_dir =
  clear_file data_dir (marker_path data_dir)

let read_marker data_dir =
  let path = marker_path data_dir in
  if not (file_exists path) then None
  else
    let j = Yojson.Safe.from_file path in
    let open Yojson.Safe.Util in
    Some (validate {
      epoch_id = j |> member "epoch_id" |> to_int;
      phase = j |> member "phase" |> to_string;
      ts = j |> member "ts" |> to_number;
    })

let recovery_required data_dir =
  file_exists (recovery_path data_dir)

let require_recovery data_dir =
  write_file data_dir (recovery_path data_dir) "required\n"

let clear_recovery data_dir =
  clear_file data_dir (recovery_path data_dir)