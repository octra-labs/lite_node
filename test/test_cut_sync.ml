(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Txlog
module Epoch = Octra_core.Epochlog

let expect message ok = if not ok then failwith message
let identity fd = let s = Unix.fstat fd in s.Unix.st_dev, s.Unix.st_ino
let path_id path = let s = Unix.stat path in s.Unix.st_dev, s.Unix.st_ino
let fail_io call = raise (Unix.Unix_error (Unix.EIO, call, "cut-test"))

let fails_io action =
  match action () with
  | () -> false
  | exception Unix.Unix_error (Unix.EIO, _, "cut-test") -> true

let blocked action =
  match action () with
  | () -> false
  | exception Failure text -> String.ends_with ~suffix:"cut is incomplete" text

let prepare_tx dir =
  let log = Tx.open_log dir in
  ignore (Tx.append log ~epoch_id:0 ~payload:"kept");
  let target = Tx.current_position log in
  ignore (Tx.append log ~epoch_id:1 ~payload:"rest");
  for _ = 1 to 3 do
    Tx.rotate log;
    ignore (Tx.append log ~epoch_id:1 ~payload:"rest")
  done;
  Tx.fsync log;
  log, target

let verify_tx log (segment, offset) =
  expect "transaction cut position differs" (Tx.current_position log = (segment, offset));
  expect "transaction cut changed retained frame"
    (Tx.read_record log ~seg_id:0 ~offset:Tx.header_size ~len:12 = (0, "kept"));
  for segment = 1 to 3 do
    expect "transaction cut kept future segment"
      (not (Sys.file_exists (Tx.seg_path log.Tx.dir segment)))
  done;
  ignore (Tx.append log ~epoch_id:1 ~payload:"retry");
  Tx.fsync log

let test_tx_sync dir stop =
  let log, ((seg_id, offset) as target) = prepare_tx dir in
  Fun.protect ~finally:(fun () -> Tx.close log) (fun () ->
    let count = ref 0 in
    expect "transaction cut acknowledged failed sync"
      (fails_io (fun () -> Tx.truncate_to log ~seg_id ~offset ~sync:(fun fd ->
        incr count;
        if !count = stop then fail_io "fsync" else Unix.fsync fd)));
    expect "transaction append accepted incomplete cut"
      (blocked (fun () -> ignore (Tx.append log ~epoch_id:2 ~payload:"forbidden")));
    expect "transaction rotation accepted incomplete cut"
      (blocked (fun () -> Tx.rotate log));
    expect "transaction cut target changed after failure"
      (blocked (fun () -> Tx.truncate_to log ~seg_id ~offset:Tx.header_size));
    let calls = ref [] in
    Tx.truncate_to log ~seg_id ~offset ~sync:(fun fd ->
      calls := identity fd :: !calls; Unix.fsync fd);
    expect "transaction cut retry skipped durability"
      (List.rev !calls = List.map path_id [Tx.seg_path dir 0; dir; Filename.dirname dir]);
    verify_tx log target)

let test_tx_remove dir =
  let log, ((seg_id, offset) as target) = prepare_tx dir in
  Fun.protect ~finally:(fun () -> Tx.close log) (fun () ->
    let removed = ref [] in
    expect "transaction cut acknowledged failed removal"
      (fails_io (fun () -> Tx.truncate_to log ~seg_id ~offset ~remove:(fun path ->
        if List.length !removed = 1 then fail_io "unlink";
        Unix.unlink path;
        removed := path :: !removed)));
    expect "transaction cut did not remove highest segment first"
      (!removed = [Tx.seg_path dir 3]);
    expect "transaction cut created a segment gap"
      (Sys.file_exists (Tx.seg_path dir 1) && Sys.file_exists (Tx.seg_path dir 2));
    expect "transaction append accepted failed removal"
      (blocked (fun () -> ignore (Tx.append log ~epoch_id:2 ~payload:"forbidden")));
    Tx.truncate_to log ~seg_id ~offset;
    verify_tx log target)

let prepare_epoch dir =
  let path = Filename.concat dir "epochs.dat" in
  let log = Epoch.open_log path in
  Epoch.append log {Epoch.empty_epoch_header with id = 0};
  let target = Epoch.current_offset log in
  Epoch.append log {Epoch.empty_epoch_header with id = 1};
  Epoch.fsync log;
  log, target

let verify_epoch log offset =
  expect "epoch cut position differs" (Epoch.current_offset log = offset);
  expect "epoch cut kept future header" (Option.map (fun h -> h.Epoch.id) (Epoch.last log) = Some 0);
  expect "epoch cut changed retained header" (Epoch.get log 0 = Some Epoch.empty_epoch_header);
  Epoch.append log {Epoch.empty_epoch_header with id = 1};
  Epoch.fsync log

let test_epoch_sync dir stop =
  let log, offset = prepare_epoch dir in
  Fun.protect ~finally:(fun () -> Epoch.close log) (fun () ->
    let count = ref 0 in
    expect "epoch cut acknowledged failed sync"
      (fails_io (fun () -> Epoch.truncate_to log ~offset ~sync:(fun fd ->
        incr count;
        if !count = stop then fail_io "fsync" else Unix.fsync fd)));
    expect "epoch append accepted incomplete cut"
      (blocked (fun () -> Epoch.append log {Epoch.empty_epoch_header with id = 2}));
    expect "epoch cut target changed after failure"
      (blocked (fun () -> Epoch.truncate_to log ~offset:Epoch.header_size));
    let calls = ref [] in
    Epoch.truncate_to log ~offset ~sync:(fun fd ->
      calls := identity fd :: !calls; Unix.fsync fd);
    expect "epoch cut retry skipped durability"
      (List.rev !calls = List.map path_id [Filename.concat dir "epochs.dat"; dir; Filename.dirname dir]);
    verify_epoch log offset)

let rec wait pid =
  try snd (Unix.waitpid [] pid) with Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let kill_at stop after =
  let count = ref 0 in
  fun fd ->
    incr count;
    if !count <> stop then Unix.fsync fd
    else begin
      if after then Unix.fsync fd;
      Unix.kill (Unix.getpid ()) Sys.sigkill;
      Unix._exit 92
    end

let test_tx_kill dir stop after =
  let log, ((seg_id, offset) as target) = prepare_tx dir in
  Tx.close log;
  flush_all ();
  let pid = Unix.fork () in
  if pid = 0 then begin
    let log = Tx.open_log dir in
    Tx.truncate_to log ~seg_id ~offset ~sync:(kill_at stop after);
    Unix._exit 93
  end;
  expect "transaction cut missed process stop" (wait pid = Unix.WSIGNALED Sys.sigkill);
  let log = Tx.open_log dir in
  Fun.protect ~finally:(fun () -> Tx.close log) (fun () ->
    Tx.truncate_to log ~seg_id ~offset;
    verify_tx log target)

let test_epoch_kill dir stop after =
  let log, offset = prepare_epoch dir in
  Epoch.close log;
  flush_all ();
  let pid = Unix.fork () in
  if pid = 0 then begin
    let log = Epoch.open_log (Filename.concat dir "epochs.dat") in
    Epoch.truncate_to log ~offset ~sync:(kill_at stop after);
    Unix._exit 93
  end;
  expect "epoch cut missed process stop" (wait pid = Unix.WSIGNALED Sys.sigkill);
  let log = Epoch.open_log (Filename.concat dir "epochs.dat") in
  Fun.protect ~finally:(fun () -> Epoch.close log) (fun () ->
    Epoch.truncate_to log ~offset;
    verify_epoch log offset)

let run root =
  let cases = ["tx_remove", test_tx_remove] @
    List.concat_map (fun stop ->
      let suffix = string_of_int stop in
      ["tx_sync" ^ suffix, (fun dir -> test_tx_sync dir stop);
       "epoch_sync" ^ suffix, (fun dir -> test_epoch_sync dir stop)] @
      List.concat_map (fun after ->
        let suffix = suffix ^ (if after then "after" else "before") in
        ["tx_kill" ^ suffix, (fun dir -> test_tx_kill dir stop after);
         "epoch_kill" ^ suffix, (fun dir -> test_epoch_kill dir stop after)]) [false; true])
      [1; 2; 3] in
  let failed = List.fold_left (fun failed (name, action) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try action dir;
      Printf.printf "case = %s status = pass\n%!" name;
      failed
    with exn ->
      Printf.eprintf "case = %s status = fail reason = %s\n%!" name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "cut_sync" run