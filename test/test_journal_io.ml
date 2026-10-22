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

let parallel jobs =
  let work = List.map (fun run ->
    let result = ref None in
    let thread = Thread.create (fun () ->
      try run () with error -> result := Some error) () in
    thread, result) jobs in
  List.iter (fun (thread, _) -> Thread.join thread) work;
  List.iter (fun (_, result) -> Option.iter raise !result) work

let test_epoch_read dir =
  let log = Epochlog.open_log (Filename.concat dir "epochs.dat") in
  Fun.protect ~finally:(fun () -> Epochlog.close log) (fun () ->
    let header id = { Epochlog.empty_epoch_header with id;
      state_root = String.make 65536 (if id = 1 then 'a' else 'b') } in
    List.iter (fun id -> Epochlog.append log (header id)) [1; 2];
    ignore (Unix.lseek log.fd 5 Unix.SEEK_SET);
    let check id =
      expect "epoch read changed record" (Epochlog.get log id = Some (header id));
      expect "epoch byte read changed record"
        (Epochlog.read_entry log ~max_bytes:100000 id
         = Ok (Some (Epochlog.epoch_to_json (header id))));
      expect "epoch position is missing" (Option.is_some (Epochlog.offset_after log id)) in
    check 1;
    ignore (Epochlog.current_offset log);
    ignore (Epochlog.read_all log);
    expect "epoch scan failed" (Result.is_ok (Epochlog.read_all_strict log));
    expect "epoch read moved file cursor" (Unix.lseek log.fd 0 Unix.SEEK_CUR = 5);
    parallel [
      (fun () -> for _ = 1 to 2000 do check 1 done);
      (fun () -> for _ = 1 to 2000 do check 2 done);
      (fun () -> for id = 3 to 66 do
        Epochlog.append log { Epochlog.empty_epoch_header with id }
      done)];
    expect "epoch append lost a record" (List.length (Epochlog.read_all log) = 66);
    expect "parallel reads moved append cursor"
      (Unix.lseek log.fd 0 Unix.SEEK_CUR = (Unix.fstat log.fd).Unix.st_size))

let test_tx_read dir =
  let log = Txlog.open_log dir in
  Fun.protect ~finally:(fun () -> Txlog.close log) (fun () ->
    let payload id = String.make 65536 (if id = 1 then 'a' else 'b') in
    let records = List.map (fun id ->
      id, Txlog.append log ~epoch_id:id ~payload:(payload id)) [1; 2] in
    ignore (Unix.lseek log.current_fd 5 Unix.SEEK_SET);
    let check (id, (seg_id, offset, len)) =
      expect "transaction read changed record"
        (Txlog.read_record log ~seg_id ~offset ~len = (id, payload id)) in
    List.iter check records;
    expect "transaction read moved file cursor"
      (Unix.lseek log.current_fd 0 Unix.SEEK_CUR = 5);
    parallel (List.map (fun record () ->
      for _ = 1 to 2000 do check record done) records))

let test_epoch_damage dir =
  let path = Filename.concat dir "epochs.dat" in
  let log = Epochlog.open_log path in
  Fun.protect ~finally:(fun () -> Epochlog.close log) (fun () ->
    Epochlog.append log { Epochlog.empty_epoch_header with id = 1 };
    let offset = Hashtbl.find log.offsets 1 in
    let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_CLOEXEC] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      ignore (Unix.lseek fd (offset + 8) Unix.SEEK_SET);
      ignore (Unix.write fd (Bytes.of_string "x") 0 1));
    let errors = ref [] in
    let on_error error = errors := error :: !errors in
    expect "damaged epoch was accepted" (Epochlog.get ~on_error log 1 = None);
    expect "checksum reason missing" (!errors = [Epochlog.Log_checksum_mismatch offset]);
    expect "damaged epoch bytes were accepted"
      (Epochlog.read_entry log ~max_bytes:100000 1 = Error `Invalid);
    expect "damaged epoch position accepted" (Epochlog.offset_after log 1 = None))

let test_epoch_identity dir =
  let log = Epochlog.open_log (Filename.concat dir "epochs.dat") in
  Fun.protect ~finally:(fun () -> Epochlog.close log) (fun () ->
    List.iter (fun id -> Epochlog.append log
      { Epochlog.empty_epoch_header with id }) [1; 2];
    let errors = ref [] in
    let on_error error = errors := error :: !errors in
    expect "missing epoch was accepted" (Epochlog.get ~on_error log 3 = None);
    expect "missing offset reason" (!errors = [Epochlog.Log_epoch_missing 3]);
    errors := [];
    Hashtbl.replace log.offsets 1 (Hashtbl.find log.offsets 2);
    expect "different epoch accepted" (Epochlog.get ~on_error log 1 = None);
    expect "different epoch reason"
      (!errors = [Epochlog.Log_epoch_identity (Hashtbl.find log.offsets 2, 1, 2)]);
    expect "different epoch bytes accepted"
      (Epochlog.read_entry log ~max_bytes:100000 1 = Error `Invalid);
    expect "different epoch position accepted" (Epochlog.offset_after log 1 = None);
    expect "other epoch was changed" (Option.is_some (Epochlog.get log 2)))

let with_store dir run =
  let module Store = Octra_core.Store_chaindata in
  let store = Store.open_chaindata dir in
  Fun.protect ~finally:(fun () -> Store.close store) (fun () ->
    Store.begin_batch store;
    List.iter (fun id -> Store.set_epoch store
      { Epochlog.empty_epoch_header with id }) [1; 2];
    Store.commit_batch store;
    run store)

let test_store_read dir =
  let module Store = Octra_core.Store_chaindata in
  with_store dir (fun store ->
    let read id () =
      for _ = 1 to 2000 do
        expect "shared store read lost epoch"
          (match Store.get_bound_epoch_header store id with
           | Ok header -> header.Epochlog.id = id
           | Error _ -> false)
      done in
    parallel [read 1; read 2; read 1];
    expect "missing store epoch was accepted"
      (Store.get_bound_epoch_header store 3 = Error "epoch not found"))

let test_store_index dir =
  let module Store = Octra_core.Store_chaindata in
  with_store dir (fun store ->
    Store.begin_batch store;
    Octra_core.Chaindata_index.buffer_epoch (Store.index store) 1 "{";
    Store.commit_batch store;
    expect "invalid index was treated as missing"
      (Store.get_bound_epoch_header store 1 = Error "epoch index header invalid");
    expect "invalid index changed another epoch"
      (Result.is_ok (Store.get_bound_epoch_header store 2)))

let test_store_io dir =
  let module Store = Octra_core.Store_chaindata in
  with_store dir (fun store ->
    let fd = store.Store.epochlog.Epochlog.fd in
    let saved = Unix.dup fd in
    Unix.close fd;
    Fun.protect ~finally:(fun () -> Unix.dup2 saved fd; Unix.close saved) (fun () ->
      expect "epoch IO failure escaped the reader"
        (Store.get_bound_epoch_header store 1 = Error "epoch log read failed")))

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
     "rotate_readonly", test_rotate_readonly;
     "epoch_read", test_epoch_read; "tx_read", test_tx_read;
     "epoch_damage", test_epoch_damage; "epoch_identity", test_epoch_identity;
     "store_read", test_store_read; "store_index", test_store_index;
     "store_io", test_store_io];
  if !failures <> 0 then exit 1

let () = Test_workspace.with_dir "journal_io" run