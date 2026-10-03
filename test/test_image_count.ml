(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module I = Image_codec
module W = Test_workspace

let expect name value = if not value then failwith name

let test_count () =
  W.with_dir "image-count" (fun dir ->
    let path = Filename.concat dir "records.dat" in
    let channel = open_out_bin path in
    let sink : I.sink = {
      channel; buffer = Buffer.create 128; records = 16_777_215L;
      bytes = 0L; prior = None; pvac_hashes = [];
    } in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      Lwt_main.run (I.emit sink (I.Value (["a"], "one")));
      Lwt_main.run (I.emit sink (I.Value (["b"], "two")));
      I.put_u32 sink.buffer 0;
      Lwt_main.run (I.drain sink));
    expect "writer record count was capped" (sink.records = 16_777_217L);
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
      let reader : I.reader = {
        channel; size = LargeFile.in_channel_length channel; format = I.Full64;
        records = 16_777_215L; prior = None;
      } in
      expect "first record differs" (I.read_record reader = Some (I.Value (["a"], "one")));
      expect "second record differs" (I.read_record reader = Some (I.Value (["b"], "two")));
      expect "reader record count was capped" (reader.records = 16_777_217L);
      expect "image end differs" (I.read_record reader = None);
      I.exact_end reader);
    List.iter (fun format ->
      let channel = open_in_bin path in
      Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
        let reader : I.reader = {
          channel; size = LargeFile.in_channel_length channel; format;
          records = 16_777_216L; prior = None;
        } in
        match I.read_record reader with
        | _ -> failwith "old record policy changed"
        | exception Failure reason ->
            expect "old record refusal differs" (reason = "ledger image record count exceeds limit")))
      [I.Prior; I.Path64]);
  print_endline "status = pass test = image_count"

let test_emit_yield () =
  W.with_dir "image-loop" (fun dir ->
    let channel = open_out_bin (Filename.concat dir "records.dat") in
    let sink : I.sink = {
      channel; buffer = Buffer.create 128; records = 0L;
      bytes = 0L; prior = None; pvac_hashes = [];
    } in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      let open Lwt.Syntax in
      let observed = ref 0L in
      let control =
        let* () = Lwt.pause () in
        observed := sink.records;
        Lwt.return_unit in
      let rec emit = function
        | [] -> Lwt.return_unit
        | key :: rest ->
            let* () = I.emit sink (I.Value ([key], "one")) in
            emit rest in
      let keys = List.init 1024 string_of_int |> List.sort String.compare in
      Lwt_main.run (Lwt.both control (emit keys)) |> ignore;
      expect "image record loop did not yield" (!observed > 0L && !observed < 1024L);
      expect "image yield lost records" (sink.records = 1024L);
      expect "image yield depended on buffer drain" (sink.bytes < Int64.of_int I.drain_bytes)));
  print_endline "status = pass test = image_emit_yield"

let test_list_yield () =
  let open Lwt.Syntax in
  let keys = List.init 1024 string_of_int in
  let tree = Lwt_main.run (Lwt_list.fold_left_s (fun tree key ->
    I.Store.Tree.add tree [key] "one") (I.Store.Tree.empty ()) keys) in
  let completed = ref false in
  let observed = ref false in
  let control =
    let* () = Lwt.pause () in
    observed := not !completed;
    Lwt.return_unit in
  let job =
    let* entries = I.sorted_entries tree in
    completed := true;
    Lwt.return entries in
  let entries, () = Lwt_main.run (Lwt.both job control) in
  expect "image directory list did not yield" !observed;
  expect "image directory order changed"
    (List.map fst entries = List.sort String.compare keys);
  print_endline "status = pass test = image_list_yield"

let test_restore_sync () =
  let module S = Octra_core.Store_irmin in
  let run = Lwt_main.run in
  let ok = function Ok value -> value | Error reason -> failwith reason in
  W.with_dir "image-sync" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "origin")) in
    let source = Filename.concat dir "ledger.dat" in
    let root = Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let tree = run (S.begin_bulk store) in
      let tree = run (S.bulk_add tree ["value"] "kept") in
      run (S.commit_bulk store tree "image sync");
      let commit = Option.get (run (S.get_commit_hash store)) in
      (run (I.write store ~commit ~path:source) |> ok).root) in
    let probe = Filename.concat dir "probe" in
    let synced = ref [] in
    let inode stat = stat.Unix.st_dev, stat.Unix.st_ino in
    let parent = Unix.stat dir in
    let sync_probe descriptor =
      let current = Unix.fstat descriptor in
      let is_parent = inode current = inode parent in
      expect "restore files synced after publication"
        (is_parent = Sys.file_exists probe);
      synced := inode current :: !synced;
      Unix.fsync descriptor in
    ignore (run (I.restore_run ~sync:sync_probe ~free:(fun _ -> Int64.max_int)
      ~source ~target:probe ~expected_root:root) |> ok);
    let rec paths path =
      let stat = Unix.lstat path in
      inode stat :: (if stat.st_kind = Unix.S_DIR then
        Sys.readdir path |> Array.to_list
        |> List.concat_map (fun name -> paths (Filename.concat path name))
      else []) in
    expect "restored files were not synced"
      (List.sort compare !synced = List.sort compare (inode parent :: paths probe));
    let steps = List.length !synced in
    List.iter (fun after ->
      for stop = 1 to steps do
        let target = Filename.concat dir (Printf.sprintf "failure-%b-%d" after stop) in
        let count = ref 0 in
        let sync descriptor =
          incr count;
          if !count = stop then begin
            if after then Unix.fsync descriptor;
            raise (Unix.Unix_error (Unix.EIO, "fsync", "image"))
          end else Unix.fsync descriptor in
        expect "restore file sync failure accepted"
          (Result.is_error (run (I.restore_run ~sync ~free:(fun _ -> Int64.max_int)
            ~source ~target ~expected_root:root)));
        expect "restore missed sync failure point" (!count = stop);
        expect "restore published before syncing files"
          (Sys.file_exists target = (stop = steps));
        let restored = run (I.restore_run ~sync:Unix.fsync ~free:(fun _ -> Int64.max_int)
          ~source ~target ~expected_root:root) |> ok in
        expect "restore sync retry lost root" (restored.root = root)
      done) [false; true];
    let target = Filename.concat dir "restored" in
    let calls = ref 0 in
    let sync descriptor =
      let current = Unix.fstat descriptor in
      if inode parent <> inode current then Unix.fsync descriptor
      else begin
        incr calls;
        expect "restore sync preceded publication" (Sys.file_exists target);
        expect "restore sync preceded rename" (not (Sys.file_exists (target ^ ".next")));
        raise (Unix.Unix_error (Unix.EIO, "fsync", "image"))
      end in
    let finish descriptor =
      if inode parent = inode (Unix.fstat descriptor) then incr calls;
      Unix.fsync descriptor in
    let restore sync = run (I.restore_run ~sync ~free:(fun _ -> Int64.max_int)
      ~source ~target ~expected_root:root) in
    expect "restore sync failure accepted" (Result.is_error (restore sync));
    expect "restore sync was not attempted" (!calls = 1);
    let commit, actual = run (I.verify_existing target root) |> ok in
    expect "restore sync failure changed root" (actual = root);
    let stage = target ^ ".next" in
    Unix.mkdir stage 0o750;
    let held = Filename.concat stage "held" in
    Out_channel.with_open_bin held (fun output -> output_string output "kept");
    expect "restore retry ignored directory sync" (Result.is_error (restore sync));
    expect "restore retry did not sync" (!calls = 2);
    expect "restore sync failure removed unrelated stage" (Sys.file_exists held);
    let restored = restore finish |> ok in
    expect "restore retry changed commit" (restored.commit = commit);
    expect "restore retry changed root" (restored.root = root);
    expect "restore retry did not finish sync" (!calls = 3);
    expect "restore sync retry removed unrelated stage" (Sys.file_exists held);
    let killed = Filename.concat dir "killed" in
    let process = Unix.create_process Sys.executable_name
      [|Sys.executable_name; "--restore-kill"; source; killed; root|]
      Unix.stdin Unix.stdout Unix.stderr in
    let rec wait () =
      try snd (Unix.waitpid [] process) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
    expect "restore did not stop after rename"
      (wait () = Unix.WSIGNALED Sys.sigkill);
    expect "restore kill did not publish directory" (Sys.file_exists killed);
    expect "restore kill retained stage" (not (Sys.file_exists (killed ^ ".next")));
    let written = run (I.restore_run
      ~sync:finish
      ~free:(fun _ -> Int64.max_int) ~source ~target:killed ~expected_root:root) |> ok in
    expect "restore kill retry changed root" (written.root = root);
    expect "restore kill retry did not sync" (!calls = 4));
  print_endline "status = pass test = image_restore_sync"

let () =
  match Array.to_list Sys.argv with
  | [_] ->
      test_restore_sync ();
      test_count ();
      test_emit_yield ();
      test_list_yield ()
  | [_; "--restore-kill"; source; target; expected_root] ->
      let parent = Unix.stat (Filename.dirname target) in
      let sync descriptor =
        let current = Unix.fstat descriptor in
        if parent.st_dev = current.st_dev && parent.st_ino = current.st_ino then
          Unix.kill (Unix.getpid ()) Sys.sigkill
        else Unix.fsync descriptor in
      ignore (Lwt_main.run (I.restore_run ~sync ~free:(fun _ -> Int64.max_int)
        ~source ~target ~expected_root));
      failwith "restore kill was not reached"
  | _ -> failwith "image count arguments are invalid"