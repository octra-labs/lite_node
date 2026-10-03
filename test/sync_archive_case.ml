(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

module Archive = Octra_bootstrap.Sync_archive
module Head = Octra_core.Head_manifest
module Publish = Octra_node_runtime.Sync_publish
module Sync = Octra_bootstrap.State_sync

let expect reason value =
  if not value then failwith reason

let get = function
  | Ok value -> value
  | Error reason -> failwith reason

let write path value =
  let output = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output value)

let with_root action =
  Test_workspace.with_dir "sync_archive" (fun root ->
    let key = "OCTRA_STATE_SYNC_SNAPSHOT_DIR" in
    let prior = Sys.getenv_opt key in
    Fun.protect
      ~finally:(fun () -> Unix.putenv key (Option.value ~default:"" prior))
      (fun () -> Unix.putenv key root; action root))

let expect_busy = function
  | Error "state sync archive is in use" -> ()
  | _ -> failwith "archive ownership did not exclude a second writer"

let test_ownership () = with_root (fun root ->
  let owner = Archive.run root (fun owner ->
    expect_busy (Archive.run root (fun _ -> ()));
    let target = Archive.path owner "snapshot" in
    expect "owned target refused" (Archive.owns owner target);
    expect "foreign target accepted"
      (not (Archive.owns owner (Filename.concat (Filename.dirname root) "snapshot")));
    List.iter (fun id ->
      let refused = try ignore (Archive.path owner id); false with Invalid_argument _ -> true in
      expect "invalid archive path accepted" refused)
      [""; "."; ".."; "../other"; "a/b"; "a\\b"; "a.next"];
    owner) |> get in
  let closed = try ignore (Archive.path owner "snapshot"); false with Invalid_argument _ -> true in
  expect "closed owner remains usable" closed;
  expect "archive failure was hidden"
    (Result.is_error (Archive.run root (fun _ -> failwith "write failed")));
  ignore (Archive.run root (fun _ -> ()) |> get))

let head epoch = Head.{
    schema_version; generation = epoch; epoch_id = epoch;
    state_root = String.make 64 'a'; ledger_state_root = None;
    irmin_commit = None; txid_hi = 0L; txlog_seg = Some 0;
    txlog_off = Some 16; epochlog_off = Some 16; commit_id = "snapshot";
    ts = float_of_int epoch; quorum_cert_hash = None;
    epoch_index_hash = None; epoch_index_root = None;
  }

let snapshot root id epoch =
  let path = Filename.concat root id in
  Unix.mkdir path 0o750;
  write (Filename.concat path "HEAD.json") (Head.to_json (head epoch));
  write (Sync.snapshot_certificate_path path) "retention checks publication time";
  path

let test_retention () = with_root (fun root ->
  let current = snapshot root "current" 10 in
  let leased = snapshot root "leased" 11 in
  let latest = snapshot root "latest" 13 in
  let old = snapshot root "old" 12 in
  get (Octra_node_runtime.Sync_lease.renew ~now:(Unix.gettimeofday ()) leased);
  let stage = Filename.concat root "unfinished.next" in
  Unix.mkdir stage 0o750;
  ignore (Archive.run root (fun owner -> Archive.mark_stage owner "unfinished") |> get);
  write (Filename.concat stage "ledger.dat") "incomplete";
  let invalid = Filename.concat root "invalid.next.next" in
  Unix.mkdir invalid 0o750;
  let observed = ref false in
  let errors = Publish.retain root ~retain:1 ~current:(fun () ->
    observed := true;
    expect_busy (Archive.run root (fun _ -> ()));
    "current") in
  expect "retention errors" (errors = []);
  expect "published id was not read under ownership" !observed;
  List.iter (fun path -> expect "retained snapshot missing" (Sys.file_exists path))
    [current; leased; latest; invalid];
  List.iter (fun path -> expect "retired archive remains" (not (Sys.file_exists path)))
    [old; stage];
  expect "retention is not repeatable"
    (Publish.retain root ~retain:1 ~current:(fun () -> "current") = []))

let test_link () = with_root (fun root ->
  Test_workspace.with_dir "sync_link" (fun outside ->
    let value = Filename.concat outside "value" in
    write value "preserve";
    let stage = Filename.concat root "linked.next" in
    Unix.mkdir stage 0o750;
    ignore (Archive.run root (fun owner -> Archive.mark_stage owner "linked") |> get);
    Unix.symlink outside (Filename.concat stage "linked");
    expect "linked stage deletion failed"
      (Publish.retain root ~retain:2 ~current:(fun () -> "current") = []);
    expect "linked target was removed" (Sys.file_exists value);
    let removed = try ignore (Unix.lstat stage); false with
      Unix.Unix_error (Unix.ENOENT, _, _) -> true in
    expect "stage link remains" removed;
    let linked_root = Filename.concat root "linked" in
    Unix.symlink outside linked_root;
    expect "linked archive root accepted"
      (Archive.run linked_root (fun _ -> ()) = Error "state sync archive is not a directory")))

let test_cancel () = with_root (fun root ->
  let started = Atomic.make false in
  let allowed = Atomic.make false in
  let completed = Atomic.make false in
  let stage = Filename.concat root "writing.next" in
  Unix.mkdir stage 0o750;
  let clock = Mtime_clock.counter () in
  let physical = ref Lwt.return_unit in
  let work = Archive.run_lwt root (fun owner ->
    Archive.mark_stage owner "writing";
    let task = Lwt_preemptive.detach (fun () ->
      Atomic.set started true;
      while not (Atomic.get allowed)
        && Mtime.Span.to_float_ns (Mtime_clock.count clock) < 3e9 do
        Thread.delay 0.001
      done;
      write (Filename.concat stage "ledger.dat") "complete";
      Atomic.set completed true) () in
    physical := task;
    task >|= fun () -> Ok ()) in
  let rec wait n =
    if Atomic.get started then Lwt.return_unit
    else if n = 0 then Lwt.fail_with "writer did not start"
    else Lwt_unix.sleep 0.001 >>= fun () -> wait (n - 1)
  in
  Lwt_main.run (Lwt.finalize
    (fun () ->
      wait 2000 >>= fun () ->
      Lwt.cancel work;
      expect "physical write was cancelled" (not (Atomic.get completed));
      expect_busy (Archive.run root (fun _ -> ()));
      Lwt.cancel work;
      Lwt.pause () >>= fun () ->
      expect_busy (Archive.run root (fun _ -> ()));
      expect "retention entered active archive"
        (Publish.retain root ~retain:2 ~current:(fun () -> "current") <> []);
      expect "active stage removed" (Sys.file_exists stage);
      Atomic.set allowed true;
      Lwt.catch
        (fun () -> work >>= fun _ -> Lwt.fail_with "cancellation was lost")
        (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn))
    (fun () ->
      Atomic.set allowed true;
      Lwt.catch (fun () -> Lwt.protected !physical) (fun _ -> Lwt.return_unit)
      >>= fun () ->
      Lwt.catch (fun () -> work >|= fun _ -> ()) (fun _ -> Lwt.return_unit)));
  expect "ownership released before write completed" (Atomic.get completed);
  expect "completed stage cleanup failed"
    (Publish.retain root ~retain:2 ~current:(fun () -> "current") = []);
  expect "completed stage remains" (not (Sys.file_exists stage)))

let test_capture () = with_root (fun root ->
  let module Store = Octra_core.Store_irmin in
  let module Capture = Octra_bootstrap.Sync_capture in
  let module Image = Octra_core.Ledger_image in
  let run = Lwt_main.run in
  let data_dir = Filename.concat root "origin" in
  let store = run (Store.open_store data_dir) in
  Fun.protect ~finally:(fun () -> run (Store.close store)) (fun () ->
    let tree = run (Store.begin_bulk store) in
    let tree = run (Store.bulk_add tree ["programs"; "state"] "preserved") in
    run (Store.commit_bulk store tree "archive input");
    let commit = Option.get (run (Store.get_commit_hash store)) in
    let ledger = Option.get (run (Store.get_head_hash store)) in
    let head = Head.{ (head 42) with state_root = ledger;
      ledger_state_root = Some ledger; irmin_commit = Some commit } in
    let source = Capture.{ data_dir; store; head; roots = [] } in
    let target = Filename.concat root "captured" in
    ignore (Archive.run root (fun _ ->
      expect_busy (run (Capture.build source ~target))) |> get);
    expect "contending capture wrote a stage" (not (Sys.file_exists (target ^ ".next")));
    let report = run (Capture.build source ~target) |> get in
    expect "capture root changed" (report.ledger_root = ledger);
    expect "capture commit changed" (run (Store.get_commit_hash store) = Some commit);
    expect "capture stage remains" (not (Sys.file_exists (target ^ ".next")));
    expect "capture ready marker missing" (Sys.file_exists (Sync.snapshot_ready_path target));
    expect "ownership marker entered published snapshot"
      (not (Sys.file_exists (Filename.concat target ".writer")));
    let restored = Filename.concat root "restored" in
    ignore (run (Image.restore ~source:(Filename.concat target "ledger.dat")
      ~target:restored ~expected_root:ledger) |> get);
    let copy = run (Store.open_store restored) in
    Fun.protect ~finally:(fun () -> run (Store.close copy)) (fun () ->
      expect "restored state differs"
        (run (Store.read copy ["programs"; "state"]) = Some "preserved"));
    expect "existing capture was overwritten"
      (Result.is_error (run (Capture.build source ~target)));
    let bad = Capture.{ source with head = Head.{ head with ledger_state_root = Some (String.make 64 '0') } } in
    let rejected = Filename.concat root "rejected" in
    expect "capture with wrong root accepted"
      (Result.is_error (run (Capture.build bad ~target:rejected)));
    expect "wrong root became visible" (not (Sys.file_exists rejected));
    expect "failed capture stage remains" (not (Sys.file_exists (rejected ^ ".next")));
    ignore (Archive.run root (fun _ -> ()) |> get)))

let test_unmarked () = with_root (fun root ->
  let stage = Filename.concat root "legacy.next" in
  Unix.mkdir stage 0o750;
  let data = Filename.concat stage "ledger.dat" in
  write data "preserve";
  let preserved () =
    expect "unmarked stage deletion was attempted"
      (Publish.retain root ~retain:2 ~current:(fun () -> "current")
       = ["legacy.next", "state sync stage ownership is unknown"]);
    let cleared = Lwt_main.run (Archive.run_lwt root (fun owner ->
      Publish.clear_stage owner (Filename.concat root "legacy")
      >|= fun () -> Ok ())) in
    expect "publisher removed unmarked stage" (Result.is_error cleared);
    expect "unmarked stage changed"
      (In_channel.with_open_bin data In_channel.input_all = "preserve")
  in
  preserved ();
  let marker = Filename.concat stage ".writer" in
  List.iter (fun value -> write marker value; preserved ())
    [""; "partial"; "octra-sync-archive-1\nextra"];
  Unix.unlink marker;
  Unix.symlink data marker;
  preserved ();
  Unix.unlink marker;
  ignore (Archive.run root (fun owner -> Archive.mark_stage owner "legacy") |> get);
  ignore (Lwt_main.run (Archive.run_lwt root (fun owner ->
    Publish.clear_stage owner (Filename.concat root "legacy")
    >|= fun () -> Ok ())) |> get);
  expect "marked stage was not removed" (not (Sys.file_exists stage)))

let prepare_stage owner id =
  let target = Archive.path owner id in
  let stage = target ^ ".next" in
  Unix.mkdir stage 0o750;
  Archive.mark_stage owner id;
  write (Filename.concat stage "ledger.dat") "verified image";
  let nested = Filename.concat stage "pvac" in
  Unix.mkdir nested 0o750;
  write (Filename.concat nested "key") "verified key";
  target, stage

let check_image target =
  expect "published ledger differs"
    (In_channel.with_open_bin (Filename.concat target "ledger.dat")
      In_channel.input_all = "verified image");
  expect "published key differs"
    (In_channel.with_open_bin (Filename.concat target "pvac/key")
      In_channel.input_all = "verified key")

let publish_syncs owner =
  let target, stage = prepare_stage owner "complete" in
  let syncs = ref 0 in
  Archive.publish_stage ~sync:(fun fd -> incr syncs; Unix.fsync fd) owner "complete";
  expect "published stage remains" (not (Sys.file_exists stage));
  expect "published ownership marker remains"
    (not (Sys.file_exists (Filename.concat target ".writer")));
  check_image target;
  !syncs

let publish_errors owner syncs =
  List.iter (fun after ->
    for stop = 1 to syncs do
      let id = Printf.sprintf "sync-%b-%d" after stop in
      let target, stage = prepare_stage owner id in
      let count = ref 0 in
      let sync fd =
        incr count;
        if !count = stop then begin
          if after then Unix.fsync fd;
          raise (Unix.Unix_error (Unix.EIO, "fsync", id))
        end else Unix.fsync fd in
      let refused = try Archive.publish_stage ~sync owner id; false with
        Unix.Unix_error (Unix.EIO, "fsync", _) -> true in
      expect "sync failure was hidden" refused;
      if Sys.file_exists stage then begin
        expect "failed publication lost stage ownership" (Archive.marked_stage owner id);
        expect "partial publication became visible" (not (Sys.file_exists target));
        Archive.publish_stage owner id
      end else Archive.finish_publish owner id;
      check_image target;
      expect "publication retry left ownership marker"
        (not (Sys.file_exists (Filename.concat target ".writer")))
    done) [false; true]

let publish_crash owner syncs =
  List.iter (fun after ->
    for stop = 1 to syncs do
      let id = Printf.sprintf "crash-%b-%d" after stop in
      let target, stage = prepare_stage owner id in
      flush_all ();
      let pid = Unix.fork () in
      if pid = 0 then begin
        let count = ref 0 in
        let sync fd =
          incr count;
          if !count = stop then begin
            if after then Unix.fsync fd;
            Unix.kill (Unix.getpid ()) Sys.sigkill
          end else Unix.fsync fd in
        Archive.publish_stage ~sync owner id;
        exit 2
      end;
      let _, status = Unix.waitpid [] pid in
      expect "publication did not reach crash point" (status = Unix.WSIGNALED Sys.sigkill);
      if Sys.file_exists stage then begin
        expect "interrupted stage lost ownership" (Archive.marked_stage owner id);
        expect "interrupted stage became visible" (not (Sys.file_exists target));
        Archive.publish_stage owner id
      end else Archive.finish_publish owner id;
      check_image target;
      expect "crash retry left ownership marker"
        (not (Sys.file_exists (Filename.concat target ".writer")));
      expect "interrupted capture published a certificate"
        (not (Sys.file_exists (Sync.snapshot_certificate_path target)))
    done) [false; true]

let publish_order owner =
  let target, stage = prepare_stage owner "ordered" in
  let identity path = let st = Unix.stat path in st.Unix.st_dev, st.st_ino in
  let required = [".writer"; "ledger.dat"; "pvac/key"; "pvac"; ""]
    |> List.map (fun name -> identity (Filename.concat stage name)) in
  let synced = ref [] in
  Archive.publish_stage ~sync:(fun fd ->
    if Sys.file_exists target then
      expect "directory became visible before file sync"
        (List.for_all (fun id -> List.mem id !synced) required);
    Unix.fsync fd;
    let st = Unix.fstat fd in
    synced := (st.Unix.st_dev, st.st_ino) :: !synced) owner "ordered"

let publish_existing owner =
  let target, stage = prepare_stage owner "occupied" in
  Unix.mkdir target 0o750;
  let refused = try Archive.publish_stage owner "occupied"; false with
    Invalid_argument reason when reason = "state sync target already exists" -> true in
  expect "existing target was overwritten" refused;
  expect "target conflict lost stage ownership" (Archive.marked_stage owner "occupied");
  expect "target conflict removed data" (Sys.file_exists (Filename.concat stage "ledger.dat"));
  expect "target conflict changed destination" (Sys.readdir target = [||])

let finish_syncs owner =
  let target, _ = prepare_stage owner "finish" in
  Archive.publish_stage owner "finish";
  let identity path = let stat = Unix.stat path in stat.Unix.st_dev, stat.st_ino in
  let required = List.map identity
    [Filename.concat target "ledger.dat"; Filename.concat target "pvac/key";
     Filename.concat target "pvac"; target; Filename.dirname target] in
  let synced = ref [] in
  Archive.finish_publish ~sync:(fun fd ->
    Unix.fsync fd;
    let stat = Unix.fstat fd in
    synced := (stat.Unix.st_dev, stat.st_ino) :: !synced) owner "finish";
  expect "publication retry omitted sync"
    (List.for_all (fun entry -> List.mem entry !synced) required);
  List.iter (fun after ->
    for stop = 1 to List.length !synced do
      let count = ref 0 in
      let sync fd =
        incr count;
        if !count = stop then begin
          if after then Unix.fsync fd;
          raise (Unix.Unix_error (Unix.EIO, "fsync", "retry"))
        end else Unix.fsync fd in
      let refused = try Archive.finish_publish ~sync owner "finish"; false with
        Unix.Unix_error (Unix.EIO, "fsync", "retry") -> true in
      expect "publication retry hid sync failure" refused;
      check_image target;
      Archive.finish_publish owner "finish"
    done) [false; true]

let publish_links owner =
  let target, stage = prepare_stage owner "linked-entry" in
  let complete = Archive.path owner "complete" in
  Unix.symlink (Filename.concat complete "ledger.dat") (Filename.concat stage "linked");
  let refused = try Archive.publish_stage owner "linked-entry"; false with
    Invalid_argument _ -> true in
  expect "linked entry was published" (refused && not (Sys.file_exists target));
  expect "linked entry lost ownership" (Archive.marked_stage owner "linked-entry");
  check_image complete

let test_publish () = with_root (fun root ->
  ignore (Archive.run root (fun owner ->
    let syncs = publish_syncs owner in
    finish_syncs owner;
    publish_errors owner syncs;
    expect "publication omitted file or directory sync" (syncs >= 7);
    publish_crash owner syncs;
    publish_order owner;
    publish_existing owner;
    publish_links owner) |> get))

let run () =
  test_ownership ();
  test_retention ();
  test_link ();
  test_publish ();
  test_cancel ();
  test_capture ();
  test_unmarked ();
  print_endline "status = pass test = sync_archive"