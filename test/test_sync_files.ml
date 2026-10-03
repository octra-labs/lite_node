(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Journal = Octra_bootstrap.State_sync_journal
module Manifest = Octra_bootstrap.State_sync_manifest

let expect message condition =
  if not condition then failwith message

let ok = function Ok value -> value | Error reason -> failwith reason

let body = "verified snapshot part"

let file =
  let size = String.length body in
  let sha256 = Digestif.SHA256.(digest_string body |> to_hex) in
  let chunk = Manifest.{index = 0; offset = 0L; size; sha256} in
  Manifest.{path = "state.bin"; size = Int64.of_int size;
    sha256; chunks = [chunk]}

let prepare dir =
  let destination, partial, _ = Journal.prepare_file ~stage:dir file |> ok in
  Journal.write_chunk ~partial (List.hd file.chunks) body |> ok;
  file, destination, partial

let inode stat = stat.Unix.st_dev, stat.Unix.st_ino

let test_retry after =
  Test_workspace.with_dir "sync-files-retry" (fun dir ->
    let file, destination, partial = prepare dir in
    let parent = inode (Unix.stat dir) in
    let calls = ref 0 in
    let sync descriptor =
      if inode (Unix.fstat descriptor) <> parent then Unix.fsync descriptor
      else begin
        incr calls;
        expect "directory sync preceded rename"
          (Sys.file_exists destination && not (Sys.file_exists partial));
        if after then Unix.fsync descriptor;
        raise (Unix.Unix_error (Unix.EIO, "fsync", "test"))
      end in
    let finish sync = Journal.finalize_file ~sync ~destination ~partial file in
    expect "file sync failure accepted" (Result.is_error (finish sync));
    expect "file sync failure point not reached" (!calls = 1);
    expect "file was not renamed" (Sys.file_exists destination);
    expect "file retry ignored directory sync" (Result.is_error (finish sync));
    expect "file retry did not sync" (!calls = 2);
    let current = inode (Unix.stat destination) in
    let done_calls = ref 0 in
    let done_sync descriptor =
      expect "file retry synced another directory" (inode (Unix.fstat descriptor) = parent);
      incr done_calls;
      Unix.fsync descriptor in
    finish done_sync |> ok;
    finish done_sync |> ok;
    expect "file retry did not finish sync" (!done_calls = 2);
    expect "file retry replaced content" (inode (Unix.stat destination) = current);
    expect "file retry changed content" (Journal.hash_file destination = Ok file.sha256));
  Printf.printf "status = pass test = sync_files_retry after = %b\n%!" after

let test_invalid () =
  List.iter (fun completed ->
    Test_workspace.with_dir "sync-files-invalid" (fun dir ->
      let file, destination, partial = prepare dir in
      let source = if completed then begin
        Journal.finalize_file ~destination ~partial file |> ok;
        destination
      end else partial in
      Out_channel.with_open_bin source (fun output -> output_string output "changed");
      let calls = ref 0 in
      expect "invalid file accepted"
        (Result.is_error (Journal.finalize_file
          ~sync:(fun _ -> incr calls) ~destination ~partial file));
      expect "invalid file synced" (!calls = 0);
      expect "invalid file publication changed" (Sys.file_exists destination = completed);
      expect "invalid file changed" (In_channel.with_open_bin source In_channel.input_all = "changed")))
    [false; true];
  print_endline "status = pass test = sync_files_invalid"

let test_restart () =
  Test_workspace.with_dir "sync-files-restart" (fun dir ->
    let file, destination, partial = prepare dir in
    let child = Unix.create_process Sys.executable_name
      [|Sys.executable_name; "--finalize-kill"; dir|]
      Unix.stdin Unix.stdout Unix.stderr in
    let rec wait () =
      try snd (Unix.waitpid [] child) with
      | Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
    expect "file finalization did not stop" (wait () = Unix.WSIGNALED Sys.sigkill);
    expect "file was not published before crash"
      (Sys.file_exists destination && not (Sys.file_exists partial));
    let calls = ref 0 in
    Journal.finalize_file
      ~sync:(fun descriptor -> incr calls; Unix.fsync descriptor)
      ~destination ~partial file |> ok;
    expect "file restart did not sync" (!calls = 1);
    expect "file restart changed content" (Journal.hash_file destination = Ok file.sha256));
  print_endline "status = pass test = sync_files_restart"

let () =
  match Array.to_list Sys.argv with
  | [_] ->
      test_restart ();
      test_retry false;
      test_retry true;
      test_invalid ()
  | [_; "--finalize-kill"; dir] ->
      let destination = Filename.concat dir file.path in
      let partial = Journal.part_path destination in
      Journal.finalize_file ~sync:(fun _ -> Unix.kill (Unix.getpid ()) Sys.sigkill)
        ~destination ~partial file |> ok;
      failwith "file finalization did not reach directory sync"
  | _ -> failwith "sync file arguments are invalid"