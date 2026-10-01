(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type error = { path : string; reason : string }

let exit_code = 78

let recover ~data_dir action =
  try Ok (action ()) with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) ->
    Error {path = data_dir; reason = "store ownership is held by another process"}
  | Octra_core.Startup_recovery.Refused reason -> Error {path = data_dir; reason}
  | Octra_core.Recovery_index.Refused reason -> Error {path = data_dir; reason}
  | Octra_core.Trim_index.Refused reason -> Error {path = data_dir; reason}
  | Octra_core.Aux_index.Refused reason -> Error {path = data_dir; reason}
  | Octra_core.Wal.Read_error (path, reason) -> Error {path; reason}
  | Octra_core.Commit_journal.Read_error (path, offset, reason) ->
    Error {path; reason = Printf.sprintf "offset = %Ld detail = %s" offset reason}

let check data_dir =
  try
    ignore (Octra_core.Wal.read_pending data_dir);
    ignore (Octra_core.Wal.read_pending_commits data_dir);
    Octra_core.Commit_journal.check data_dir;
    match Octra_core.Head_manifest.load_result data_dir with
    | Missing | Present _ -> Ok ()
    | Corrupt reason -> Error {path = Octra_core.Head_manifest.path data_dir; reason}
  with
  | Octra_core.Wal.Read_error (path, reason) -> Error {path; reason}
  | Octra_core.Commit_journal.Read_error (path, offset, reason) ->
    Error {path; reason = Printf.sprintf "offset = %Ld detail = %s" offset reason}