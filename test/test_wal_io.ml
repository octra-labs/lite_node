(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Wal = Octra_core.Wal

let expect label ok = if not ok then failwith label

let rejects action =
  match action () with
  | _ -> false
  | exception _ -> true

let put path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes; flush channel)

let entry epoch_id = Wal.{
  epoch_id;
  pre_state_root = "before";
  post_state_root = "after";
  parent_commit = "parent";
  start_txid = 1L;
  tx_count = 1;
  finalized_by = "tester";
  finalized_at = 1.;
  irmin_last_epoch_before = epoch_id - 1;
}

let test_roundtrip () =
  Test_workspace.with_dir "wal_roundtrip" (fun dir ->
    expect "missing WAL directory" (Wal.read_pending dir = []);
    Wal.write dir (entry 2);
    Wal.write dir (entry 1);
    expect "entry bytes changed" (Wal.read_pending dir = [entry 1; entry 2]);
    Wal.delete dir 1;
    Wal.delete dir 1;
    expect "wrong entry removed" (Wal.read_pending dir = [entry 2]))

let test_bad_json () =
  Test_workspace.with_dir "wal_bad_json" (fun dir ->
    Wal.write dir (entry 1);
    List.iter (fun bytes ->
      put (Wal.entry_path dir 1) bytes;
      expect "damaged WAL treated as absent"
        (rejects (fun () -> Wal.read_pending dir));
      expect "damaged WAL erased" (Sys.file_exists (Wal.entry_path dir 1)))
      [""; "{"; "{}"])

let test_wrong_epoch () =
  Test_workspace.with_dir "wal_wrong_epoch" (fun dir ->
    Wal.write dir (entry 1);
    put (Wal.entry_path dir 1) (Wal.to_json (entry 2));
    expect "filename and epoch differ"
      (rejects (fun () -> Wal.read_pending dir)))

let test_file_kind () =
  Test_workspace.with_dir "wal_file_kind" (fun dir ->
    Wal.ensure_dir dir;
    let path = Wal.entry_path dir 1 in
    Unix.mkdir path 0o700;
    expect "directory treated as absent" (rejects (fun () -> Wal.read_pending dir));
    Unix.rmdir path;
    let target = Filename.concat dir "record" in
    put target (Wal.to_json (entry 1));
    Unix.symlink target path;
    expect "linked WAL accepted" (rejects (fun () -> Wal.read_pending dir));
    Unix.unlink path)

let test_dir_link () =
  Test_workspace.with_dir "wal_dir_link" (fun dir ->
    Unix.symlink (Filename.concat dir "missing") (Wal.wal_dir dir);
    expect "broken WAL directory treated as absent"
      (rejects (fun () -> Wal.read_pending dir));
    expect "broken pending directory treated as absent"
      (rejects (fun () -> Wal.read_pending_commits dir));
    Unix.unlink (Wal.wal_dir dir))

let test_delete_error () =
  Test_workspace.with_dir "wal_delete_error" (fun dir ->
    Wal.ensure_dir dir;
    let path = Wal.entry_path dir 1 in
    Unix.mkdir path 0o700;
    expect "WAL delete failure hidden" (rejects (fun () -> Wal.delete dir 1));
    let pending = Wal.pending_commit_path dir 1 0 in
    Unix.mkdir pending 0o700;
    expect "pending delete failure hidden"
      (rejects (fun () -> Wal.delete_pending_commit dir 1 0));
    expect "epoch delete failure hidden"
      (rejects (fun () -> Wal.delete_pending_commits_for_epoch dir 1)))

let test_pending_name () =
  Test_workspace.with_dir "wal_pending_name" (fun dir ->
    Wal.ensure_dir dir;
    put (Wal.pending_commit_path dir 1 0)
      {|{"epoch_id":2,"round":0,"proposal_id":"p","proposed_state_root":"r","txid_hi":"1","ts":1,"validator_addr":"v"}|};
    expect "pending filename and epoch differ"
      (rejects (fun () -> Wal.read_pending_commits dir)))

let test_error_path () =
  Test_workspace.with_dir "wal_error_path" (fun dir ->
    Wal.ensure_dir dir;
    let path = Wal.entry_path dir 41 in
    put path "{";
    let file, reason = try ignore (Wal.read_pending dir); "", "" with
      | Wal.Read_error (file, reason) -> file, reason in
    expect "WAL error omits path" (file = path && reason <> "");
    expect "WAL evidence removed" (Sys.file_exists path);
    let pending = Wal.pending_commit_path dir 41 0 in
    put pending "{";
    let file, reason = try ignore (Wal.read_pending_commits dir); "", "" with
      | Wal.Read_error (file, reason) -> file, reason in
    expect "pending error omits path" (file = pending && reason <> "");
    expect "pending evidence removed" (Sys.file_exists pending))

let () =
  let cases = [
    "roundtrip", test_roundtrip;
    "bad_json", test_bad_json;
    "wrong_epoch", test_wrong_epoch;
    "file_kind", test_file_kind;
    "dir_link", test_dir_link;
    "delete_error", test_delete_error;
    "pending_name", test_pending_name;
    "error_path", test_error_path;
  ] in
  let failures = List.filter_map (fun (name, action) ->
    match action () with
    | () -> Printf.printf "event = test name = %s status = passed\n%!" name; None
    | exception exn ->
      Printf.eprintf "event = test name = %s status = failed reason = %s\n%!"
        name (Printexc.to_string exn);
      Some name) cases in
  if failures <> [] then exit 1