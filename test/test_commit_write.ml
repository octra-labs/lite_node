(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module J = Octra_core.Commit_journal

let expect reason valid = if not valid then failwith reason
let refused action = try action (); false with _ -> true
let handles () = Array.length (Sys.readdir (if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd"))
let record id = J.Prepare {commit_id = id; prev_generation = 7;
  epoch_id = 8; planned_txid_hi = 9L; planned_state_root = "root"; ts = 0.}
let text record = Yojson.Safe.to_string (J.record_to_json record) ^ "\n"
let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let short_write dir interrupted =
  let row = record "short" in
  let calls = ref 0 in
  let write fd line offset count =
    incr calls;
    if interrupted && !calls mod 2 = 1 then raise (Unix.Unix_error (Unix.EINTR, "write", "test"));
    Unix.write_substring fd line offset (min 7 count) in
  J.append ~write dir row;
  expect "short write was not exercised" (!calls > 2);
  expect "short writes changed record bytes" (read (J.path dir) = text row);
  expect "short writes changed decoded record" (J.read_all dir = [row])

let partial dir =
  let first = record "first" and second = record "second" in
  J.append dir first;
  let calls = ref 0 in
  let write fd line offset count =
    incr calls;
    if !calls = 2 then raise (Unix.Unix_error (Unix.EIO, "write", "test"));
    Unix.write_substring fd line offset (min 7 count) in
  let before = handles () in
  expect "partial write error was ignored" (refused (fun () -> J.append ~write dir second));
  expect "partial write leaked descriptor" (handles () = before);
  let expected = text first ^ String.sub (text second) 0 7 in
  expect "partial write changed prior frame" (read (J.path dir) = expected);
  expect "partial frame was accepted" (refused (fun () -> ignore (J.read_all dir)));
  expect "retry extended partial frame" (refused (fun () -> J.append dir second));
  expect "refused retry changed bytes" (read (J.path dir) = expected)

let no_progress dir value =
  let first = record "first" in
  J.append dir first;
  let before = handles () in
  expect "invalid write count accepted"
    (refused (fun () -> J.append ~write:(fun _ _ _ _ -> value) dir (record "next")));
  expect "invalid write count leaked descriptor" (handles () = before);
  expect "invalid write count changed bytes" (read (J.path dir) = text first)

let sync_error dir target after =
  let row = record "sync" in
  let calls = ref 0 in
  let sync fd =
    incr calls;
    if !calls = target then begin
      if after then Unix.fsync fd;
      raise (Unix.Unix_error (Unix.EIO, "fsync", "test"))
    end;
    Unix.fsync fd in
  let before = handles () in
  expect "failed sync reported success" (refused (fun () -> J.append ~sync dir row));
  expect "failed sync leaked descriptor" (handles () = before);
  expect "failed sync changed frame bytes" (read (J.path dir) = text row);
  expect "failed sync skipped selected call" (!calls = target)

let sync_order dir =
  let kinds = ref [] in
  J.append ~sync:(fun fd -> kinds := !kinds @ [(Unix.fstat fd).Unix.st_kind]; Unix.fsync fd)
    dir (record "sync");
  expect "file and parent sync order differs" (!kinds = [Unix.S_REG; Unix.S_DIR])

let invalid_record dir =
  let before = handles () in
  let row = J.Commit {commit_id = "invalid"; generation = 8; ts = nan} in
  expect "invalid record written" (refused (fun () -> J.append dir row));
  expect "invalid record created journal" (not (Sys.file_exists (J.path dir)));
  expect "invalid record leaked descriptor" (handles () = before)

let run root =
  let cases = ["short", (fun dir -> short_write dir false);
    "interrupted", (fun dir -> short_write dir true); "partial", partial;
    "zero", (fun dir -> no_progress dir 0); "negative", (fun dir -> no_progress dir (-1));
    "excess", (fun dir -> no_progress dir max_int);
    "file_before", (fun dir -> sync_error dir 1 false);
    "file_after", (fun dir -> sync_error dir 1 true);
    "dir_before", (fun dir -> sync_error dir 2 false);
    "dir_after", (fun dir -> sync_error dir 2 true);
    "sync_order", sync_order; "invalid_record", invalid_record] in
  let failed = ref false in
  List.iter (fun (name, test) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try test dir; Printf.printf "event = passed case = %s\n%!" name
    with exn -> failed := true;
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string exn)) cases;
  if !failed then exit 1

let () = Test_workspace.with_dir "commit_write" run