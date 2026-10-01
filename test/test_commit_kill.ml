(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module J = Octra_core.Commit_journal

type step = Before_write | Partial_write | Written | File_before | File_after | Dir_before | Dir_after | Complete

let name = function
  | Before_write -> "before_write" | Partial_write -> "partial_write" | Written -> "written"
  | File_before -> "file_before" | File_after -> "file_after"
  | Dir_before -> "dir_before" | Dir_after -> "dir_after" | Complete -> "complete"

let expect reason valid = if not valid then failwith reason
let row id = J.Commit {commit_id = id; generation = 8; ts = 0.}
let text row = Yojson.Safe.to_string (J.record_to_json row) ^ "\n"
let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))
let kill () = Unix.kill (Unix.getpid ()) Sys.sigkill; Unix._exit 99

let append dir step record =
  let write fd line offset count =
    if step = Before_write then kill ();
    let written = Unix.write_substring fd line offset (if step = Partial_write then min count 7 else count) in
    if step = Partial_write || (step = Written && offset + written = String.length line) then kill ();
    written in
  let sync fd =
    let kind = (Unix.fstat fd).Unix.st_kind in
    if (kind = Unix.S_REG && step = File_before) || (kind = Unix.S_DIR && step = Dir_before) then kill ();
    Unix.fsync fd;
    if (kind = Unix.S_REG && step = File_after) || (kind = Unix.S_DIR && step = Dir_after) then kill () in
  J.append ~write ~sync dir record

let run_case root step =
  let dir = Filename.concat root (name step) in
  Unix.mkdir dir 0o700;
  let first = row "first" and second = row "second" in
  J.append dir first;
  let ack = Filename.concat dir "ack" in
  flush_all ();
  let status = match Unix.fork () with
    | 0 ->
      (try
        append dir step second;
        let channel = open_out_bin ack in
        output_string channel "returned";
        close_out channel;
        Unix._exit 0
      with _ -> Unix._exit 2)
    | pid -> Recovery_case.wait pid in
  expect "wrong process termination"
    (status = if step = Complete then Unix.WEXITED 0 else Unix.WSIGNALED Sys.sigkill);
  expect "append returned before selected interruption" (Sys.file_exists ack = (step = Complete));
  let expected = match step with
    | Before_write -> text first
    | Partial_write -> text first ^ String.sub (text second) 0 7
    | _ -> text first ^ text second in
  expect "process death changed prior record bytes" (read (J.path dir) = expected);
  (match step with
  | Partial_write ->
    let refused = try ignore (J.read_all dir); false with
      | J.Read_error (_, offset, "unfinished record") -> offset = Int64.of_int (String.length (text first))
      | _ -> false in
    expect "process death exposed a partial record" refused;
    expect "append extended interrupted record" (try J.append dir second; false with _ -> true);
    expect "refused append changed evidence" (read (J.path dir) = expected)
  | Before_write -> expect "empty append changed old records" (J.read_all dir = [first])
  | _ -> expect "complete visible record changed" (J.read_all dir = [first; second]));
  Printf.printf "event = passed case = %s\n%!" (name step)

let run root =
  List.iter (run_case root)
    [Before_write; Partial_write; Written; File_before; File_after; Dir_before; Dir_after; Complete]

let () = Test_workspace.with_dir "commit_kill" run