(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Head = Octra_core.Head_manifest
module Journal = Octra_core.Commit_journal
module Wal = Octra_core.Wal

let expect label value = if not value then failwith label

let fail_sync () = raise (Unix.Unix_error (Unix.EIO, "fsync", "test"))

let rejects action =
  match action () with
  | () -> false
  | exception Unix.Unix_error (Unix.EIO, "fsync", "test") -> true

let read path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let head epoch_id = Head.{
  schema_version = schema_version;
  generation = epoch_id;
  epoch_id;
  state_root = String.make 64 'a';
  ledger_state_root = None;
  irmin_commit = None;
  txid_hi = Int64.of_int epoch_id;
  txlog_seg = Some 0;
  txlog_off = Some 32;
  epochlog_off = Some 0;
  commit_id = "epoch-" ^ string_of_int epoch_id;
  ts = 1.;
  quorum_cert_hash = None;
  epoch_index_hash = None;
  epoch_index_root = None;
}

let pending = Wal.{
  epoch_id = 2;
  round = 0;
  proposal_id = "proposal";
  proposed_state_root = "root";
  txid_hi = 2L;
  ts = 1.;
  validator_addr = "validator";
  proposal_b64 = None;
  vote_b64 = None;
  tx_hashes = [];
  txs_json = [];
  receipts_json = [];
}

let test_head_file () =
  Test_workspace.with_dir "head_file_sync" (fun dir ->
    Head.atomic_write dir (head 1);
    let before = read (Head.path dir) in
    expect "HEAD reports success after failed file sync"
      (rejects (fun () -> Head.atomic_write ~sync:(fun _ -> fail_sync ()) dir (head 2)));
    expect "HEAD published before file sync" (read (Head.path dir) = before))

let test_head_dir () =
  Test_workspace.with_dir "head_dir_sync" (fun dir ->
    let sync fd =
      if (Unix.fstat fd).Unix.st_kind = Unix.S_DIR then fail_sync ()
      else Unix.fsync fd in
    expect "HEAD reports success after failed directory sync"
      (rejects (fun () -> Head.atomic_write ~sync dir (head 2)));
    Head.atomic_write dir (head 2);
    expect "HEAD retry bytes differ" (Head.load dir = Some (head 2)))

let record = Journal.Commit { commit_id = "epoch-2"; generation = 2; ts = 1. }

let test_journal_file () =
  Test_workspace.with_dir "journal_file_sync" (fun dir ->
    expect "journal reports success after failed file sync"
      (rejects (fun () -> Journal.append ~sync:(fun _ -> fail_sync ()) dir record)))

let test_journal_dir () =
  Test_workspace.with_dir "journal_dir_sync" (fun dir ->
    let sync fd =
      if (Unix.fstat fd).Unix.st_kind = Unix.S_DIR then fail_sync ()
      else Unix.fsync fd in
    expect "new journal reports success without directory sync"
      (rejects (fun () -> Journal.append ~sync dir record)))

let test_wal_remove_retry () =
  Test_workspace.with_dir "wal_remove_sync" (fun dir ->
    Wal.write_pending_commit dir pending;
    let path = Wal.pending_commit_path dir 2 0 in
    expect "initial remove did not reach failed sync"
      (rejects (fun () -> Wal.delete_file ~sync:(fun _ -> fail_sync ()) path));
    expect "unlink did not occur before failed sync" (not (Sys.file_exists path));
    expect "remove retry skipped directory sync"
      (rejects (fun () -> Wal.delete_file ~sync:(fun _ -> fail_sync ()) path));
    Wal.delete_file path)

let test_wal_dir_retry () =
  Test_workspace.with_dir "wal_dir_sync" (fun dir ->
    expect "initial mkdir did not reach failed sync"
      (rejects (fun () -> Wal.ensure_dir ~sync:(fun _ -> fail_sync ()) dir));
    expect "mkdir did not occur before failed sync" (Sys.is_directory (Wal.wal_dir dir));
    expect "mkdir retry skipped parent sync"
      (rejects (fun () -> Wal.ensure_dir ~sync:(fun _ -> fail_sync ()) dir)))

let test_pending_retry () =
  Test_workspace.with_dir "pending_sync" (fun dir ->
    Wal.ensure_dir dir;
    let path = Wal.pending_commit_path dir 2 0 in
    let sync fd =
      if Sys.file_exists path && (Unix.fstat fd).Unix.st_kind = Unix.S_DIR
      then fail_sync () else Unix.fsync fd in
    expect "initial pending write did not reach failed sync"
      (rejects (fun () -> Wal.write_pending_commit ~sync dir pending));
    expect "rename did not occur before failed sync" (Sys.file_exists path);
    let synced = ref [] in
    Wal.write_pending_commit ~sync:(fun fd ->
      synced := (Unix.fstat fd).Unix.st_kind :: !synced;
      Unix.fsync fd) dir pending;
    expect "pending retry skipped file sync" (List.mem Unix.S_REG !synced);
    expect "pending retry skipped directory sync" (List.mem Unix.S_DIR !synced);
    expect "pending retry changed content" (Wal.read_pending_commits dir = [pending]))

let test_head_crash () =
  List.iter (fun (kind, after) ->
    Test_workspace.with_dir "head_sync_crash" (fun dir ->
      Head.atomic_write dir (head 1);
      let pid = Unix.fork () in
      if pid = 0 then begin
        let sync fd =
          if (Unix.fstat fd).Unix.st_kind = kind then begin
            if after then Unix.fsync fd;
            Unix._exit 91
          end else Unix.fsync fd in
        Head.atomic_write ~sync dir (head 2);
        Unix._exit 92
      end;
      expect "child missed HEAD sync cut" (Test_workspace.wait pid = Unix.WEXITED 91);
      let expected = if kind = Unix.S_REG then head 1 else head 2 in
      expect "HEAD process cut produced partial record" (Head.load dir = Some expected);
      Head.atomic_write dir (head 2);
      expect "HEAD retry after process cut" (Head.load dir = Some (head 2))))
    [Unix.S_REG, false; Unix.S_REG, true; Unix.S_DIR, false; Unix.S_DIR, true]

let test_pending_crash () =
  List.iter (fun cut -> List.iter (fun after ->
    Test_workspace.with_dir "pending_sync_crash" (fun dir ->
      let pid = Unix.fork () in
      if pid = 0 then begin
        let calls = ref 0 in
        let sync fd =
          incr calls;
          if !calls = cut then begin
            if after then Unix.fsync fd;
            Unix._exit 91
          end else Unix.fsync fd in
        Wal.write_pending_commit ~sync dir pending;
        Unix._exit 92
      end;
      expect "child missed pending sync cut" (Test_workspace.wait pid = Unix.WEXITED 91);
      let expected = if cut < 3 then [] else [pending] in
      expect "pending process cut produced partial record" (Wal.read_pending_commits dir = expected);
      Wal.write_pending_commit dir pending;
      expect "pending retry after process cut" (Wal.read_pending_commits dir = [pending])))
    [false; true]) [1; 2; 3]

let test_sync_order () =
  Test_workspace.with_dir "sync_order" (fun dir ->
    let events = ref [] in
    let sync fd =
      events := (Unix.fstat fd).Unix.st_kind :: !events;
      Unix.fsync fd in
    Head.atomic_write ~sync dir (head 1);
    expect "HEAD sync order" (List.rev !events = [Unix.S_REG; Unix.S_DIR]);
    events := [];
    Journal.append ~sync dir record;
    expect "journal sync order" (List.rev !events = [Unix.S_REG; Unix.S_DIR]);
    events := [];
    Wal.write_pending_commit ~sync dir pending;
    expect "pending sync order" (List.rev !events = [Unix.S_DIR; Unix.S_REG; Unix.S_DIR]);
    events := [];
    Wal.write_pending_commit ~sync dir pending;
    expect "pending retry sync order" (List.rev !events = [Unix.S_DIR; Unix.S_REG; Unix.S_DIR]))

let () =
  let cases = [
    "head_file", test_head_file;
    "head_dir", test_head_dir;
    "journal_file", test_journal_file;
    "journal_dir", test_journal_dir;
    "wal_remove_retry", test_wal_remove_retry;
    "wal_dir_retry", test_wal_dir_retry;
    "pending_retry", test_pending_retry;
    "head_crash", test_head_crash;
    "pending_crash", test_pending_crash;
    "sync_order", test_sync_order;
  ] in
  let failures = List.filter_map (fun (name, run) ->
    match run () with
    | () -> Printf.printf "event = test name = %s status = passed\n%!" name; None
    | exception error ->
      Printf.eprintf "event = test name = %s status = failed reason = %s\n%!"
        name (Printexc.to_string error);
      Some name) cases in
  if failures <> [] then exit 1