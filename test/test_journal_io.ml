(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Txlog = Octra_core.Txlog
module Epochlog = Octra_core.Epochlog

let expect reason value = if not value then failwith reason

let handles () =
  Array.length (Sys.readdir (if Sys.file_exists "/proc/self/fd" then
    "/proc/self/fd" else "/dev/fd"))

let identity fd =
  let stat = Unix.fstat fd in
  stat.Unix.st_dev, stat.Unix.st_ino

let path_identity path =
  let stat = Unix.stat path in
  stat.Unix.st_dev, stat.Unix.st_ino

let fail_sync () = raise (Unix.Unix_error (Unix.EIO, "fsync", "test"))

let refuses action =
  match action () with
  | () -> false
  | exception Unix.Unix_error (Unix.EIO, "fsync", "test") -> true

let test_rotate_sync dir =
  let log = Txlog.open_log dir in
  Fun.protect ~finally:(fun () -> Txlog.close log) (fun () ->
    let segment, offset, length = Txlog.append log ~epoch_id:7 ~payload:"kept" in
    let before = identity log.current_fd in
    expect "rotation acknowledged failed sync"
      (refuses (fun () -> Txlog.rotate ~sync:(fun _ -> fail_sync ()) log));
    expect "rotation changed segment before sync" (log.current_seg = segment);
    expect "rotation changed descriptor before sync" (identity log.current_fd = before);
    expect "rotation created file before sync" (not (Sys.file_exists (Txlog.seg_path dir 1)));
    expect "rotation changed previous data"
      (Txlog.read_record log ~seg_id:segment ~offset ~len:length = (7, "kept"));
    let calls = ref [] in
    Txlog.rotate ~sync:(fun fd -> calls := identity fd :: !calls; Unix.fsync fd) log;
    expect "rotation did not sync previous file" (!calls = [before]);
    expect "rotation did not advance" (log.current_seg = segment + 1);
    ignore (Txlog.append log ~epoch_id:8 ~payload:"next"))

let test_rotate_open dir =
  let log = Txlog.open_log dir in
  Fun.protect ~finally:(fun () -> try Txlog.close log with Unix.Unix_error _ -> ())
    (fun () ->
      let segment, offset, length = Txlog.append log ~epoch_id:7 ~payload:"kept" in
      let before = identity log.current_fd in
      Unix.mkdir (Txlog.seg_path dir 1) 0o700;
      let refused = match Txlog.rotate log with
        | () -> false
        | exception Unix.Unix_error (Unix.EISDIR, _, _) -> true in
      expect "rotation did not reject directory" refused;
      expect "failed open closed previous descriptor" (identity log.current_fd = before);
      expect "failed open changed segment" (log.current_seg = segment);
      expect "failed open changed bytes"
        (Txlog.read_record log ~seg_id:segment ~offset ~len:length = (7, "kept"));
      ignore (Txlog.append log ~epoch_id:8 ~payload:"retry"))

let test_sync dir kind =
  let data = Filename.concat dir "data" in
  Unix.mkdir data 0o700;
  let close, sync, file =
    if kind = "txlog" then
      let log = Txlog.open_log data in
      (fun () -> Txlog.close log),
      (fun call -> Txlog.fsync ~sync:call log), Txlog.seg_path data 0
    else
      let path = Filename.concat data "epochs.dat" in
      let log = Epochlog.open_log path in
      (fun () -> Epochlog.close log),
      (fun call -> Epochlog.fsync ~sync:call log), path in
  Fun.protect ~finally:close (fun () ->
    let expected = List.map path_identity [file; data; dir] in
    for cut = 1 to 3 do
      let before = handles () in
      let count = ref 0 in
      expect "journal acknowledged failed sync"
        (refuses (fun () -> sync (fun fd ->
          incr count;
          if !count = cut then fail_sync () else Unix.fsync fd)));
      expect "failed sync leaked a descriptor" (handles () = before);
      let calls = ref [] in
      sync (fun fd -> calls := identity fd :: !calls; Unix.fsync fd);
      expect "journal retry skipped sync" (List.rev !calls = expected)
    done)

let test_readonly dir kind =
  let close, sync =
    if kind = "txlog" then begin
      Txlog.close (Txlog.open_log dir);
      let log = Txlog.open_log ~readonly:true dir in
      (fun () -> Txlog.close log), (fun call -> Txlog.fsync ~sync:call log)
    end else begin
      let path = Filename.concat dir "epochs.dat" in
      Epochlog.close (Epochlog.open_log path);
      let log = Epochlog.open_log ~readonly:true path in
      (fun () -> Epochlog.close log), (fun call -> Epochlog.fsync ~sync:call log)
    end in
  Fun.protect ~finally:close (fun () -> sync (fun _ -> failwith "read-only sync"))

let test_open_handles dir kind readonly =
  let path = Filename.concat dir (if kind = "txlog" then "seg000000.dat" else "epochs.dat") in
  let output = open_out_bin path in
  output_string output "abcdefgh";
  close_out output;
  let before = handles () in
  for _ = 1 to 4 do
    let refused = match
      if kind = "txlog" then Txlog.close (Txlog.open_log ~readonly dir)
      else Epochlog.close (Epochlog.open_log ~readonly path)
    with
    | () -> false
    | exception Failure reason when reason = kind ^ ": truncated header" -> true in
    expect "journal accepted truncated header" refused
  done;
  expect "failed journal open leaked descriptors" (handles () = before);
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
    expect "failed journal open changed bytes"
      (in_channel_length input = 8 && really_input_string input 8 = "abcdefgh"))

let test_rotate_kill dir after =
  let pid = Unix.fork () in
  if pid = 0 then begin
    let log = Txlog.open_log dir in
    ignore (Txlog.append log ~epoch_id:7 ~payload:"kept");
    Txlog.rotate ~sync:(fun fd ->
      if after then Unix.fsync fd;
      Unix.kill (Unix.getpid ()) Sys.sigkill;
      Unix._exit 92) log;
    Unix._exit 93
  end;
  expect "child missed rotation sync cut"
    (snd (Unix.waitpid [] pid) = Unix.WSIGNALED Sys.sigkill);
  expect "rotation published segment before sync returned"
    (not (Sys.file_exists (Txlog.seg_path dir 1)));
  let log = Txlog.open_log dir in
  Fun.protect ~finally:(fun () -> Txlog.close log) (fun () ->
    expect "process kill changed old frame"
      (Txlog.read_record log ~seg_id:0 ~offset:Txlog.header_size ~len:12 = (7, "kept"));
    Txlog.rotate log;
    ignore (Txlog.append log ~epoch_id:8 ~payload:"retry");
    Txlog.fsync log)

let test_read_append dir =
  let log = Txlog.open_log dir in
  Fun.protect ~finally:(fun () -> Txlog.close log) (fun () ->
    let first = Txlog.append log ~epoch_id:191661 ~payload:(String.make 32 'a') in
    let old = Txlog.append log ~epoch_id:191662 ~payload:(String.make 59610 'b') in
    let read (seg_id, offset, len) = Txlog.read_record log ~seg_id ~offset ~len in
    ignore (read first);
    let next = Txlog.append log ~epoch_id:192986 ~payload:(String.make 3277 'c') in
    expect "read cursor changed a prior frame"
      (read old = (191662, String.make 59610 'b'));
    expect "append returned a different location"
      (read next = (192986, String.make 3277 'c'));
    expect "append logical EOF differs"
      (log.current_offset = (Unix.fstat log.current_fd).Unix.st_size))

let test_rotate_readonly dir =
  Txlog.close (Txlog.open_log dir);
  let log = Txlog.open_log ~readonly:true dir in
  Fun.protect ~finally:(fun () -> Txlog.close log) (fun () ->
    let refused = match Txlog.rotate log with
      | () -> false
      | exception Failure reason when reason = "txlog: rotate on read-only log" -> true
      | exception _ -> false in
    expect "read-only rotation accepted" refused;
    expect "read-only rotation wrote a segment"
      (not (Sys.file_exists (Txlog.seg_path dir 1))))

let run root =
  let failures = ref 0 in
  List.iter (fun (name, test) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    match test dir with
    | () -> Printf.printf "event = test name = %s status = passed\n%!" name
    | exception error ->
      incr failures;
      Printf.printf "event = test name = %s status = failed reason = %s\n%!"
        name (Printexc.to_string error))
    ["rotate_sync", test_rotate_sync; "rotate_open", test_rotate_open;
     "txlog_sync", (fun dir -> test_sync dir "txlog");
     "epochlog_sync", (fun dir -> test_sync dir "epochlog");
     "txlog_readonly", (fun dir -> test_readonly dir "txlog");
     "epochlog_readonly", (fun dir -> test_readonly dir "epochlog");
     "txlog_open", (fun dir -> test_open_handles dir "txlog" false);
     "epochlog_open", (fun dir -> test_open_handles dir "epochlog" false);
     "txlog_open_readonly", (fun dir -> test_open_handles dir "txlog" true);
     "epochlog_open_readonly", (fun dir -> test_open_handles dir "epochlog" true);
     "rotate_kill_before", (fun dir -> test_rotate_kill dir false);
     "rotate_kill_after", (fun dir -> test_rotate_kill dir true);
     "read_append", test_read_append;
     "rotate_readonly", test_rotate_readonly];
  if !failures <> 0 then exit 1

let () = Test_workspace.with_dir "journal_io" run