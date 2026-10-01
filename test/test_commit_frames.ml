(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module J = Octra_core.Commit_journal

let expect reason valid = if not valid then failwith reason
let row reason = J.Abort {commit_id = "frame"; reason; ts = 0.}
let text row = Yojson.Safe.to_string (J.record_to_json row) ^ "\n"
let write path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel bytes)

let run root =
  let run name test =
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    test dir;
    Printf.printf "event = passed case = %s\n%!" name in
  run "chunks" (fun dir ->
    let rows = List.map (fun count -> row (String.make count 'a')) [1; 65535; 65536; 65537; 131071] in
    List.iter (J.append dir) rows;
    expect "chunking changed decoded records" (J.read_all dir = rows));
  let capacity = J.max_record_bytes - (String.length (text (row "")) - 1) in
  run "size_limit" (fun dir ->
    let largest = row (String.make capacity 'a') in
    J.append dir largest;
    expect "maximum record rejected" (J.read_all dir = [largest]));
  run "oversize_write" (fun dir ->
    let oversized = row (String.make (capacity + 1) 'a') in
    expect "oversized append accepted" (try J.append dir oversized; false with _ -> true);
    expect "oversized append created file" (not (Sys.file_exists (J.path dir))));
  run "oversize_read" (fun dir ->
    let first = text (row "first") in
    let last = text (row (String.make (capacity + 1) 'a')) in
    write (J.path dir) (first ^ last);
    let refused = try ignore (J.read_all dir); false with
      | J.Read_error (path, offset, "record exceeds size limit") ->
        path = J.path dir && offset = Int64.of_int (String.length first)
      | _ -> false in
    expect "oversized record position differs" refused);
  run "missing_delimiter" (fun dir ->
    let first = text (row "first") in
    write (J.path dir) (first ^ "unfinished");
    let refused = try ignore (J.read_all dir); false with
      | J.Read_error (_, offset, "unfinished record") -> offset = Int64.of_int (String.length first)
      | _ -> false in
    expect "unfinished record position differs" refused);
  run "file_link" (fun dir ->
    let target = Filename.concat dir "retained" in
    write target (text (row "retained"));
    Unix.symlink target (J.path dir);
    expect "read followed journal link" (try ignore (J.read_all dir); false with _ -> true);
    expect "append followed journal link" (try J.append dir (row "new"); false with _ -> true));
  run "file_directory" (fun dir ->
    Unix.mkdir (J.path dir) 0o700;
    expect "read accepted directory" (try ignore (J.read_all dir); false with _ -> true);
    expect "append accepted directory" (try J.append dir (row "new"); false with _ -> true));
  run "file_fifo" (fun dir ->
    Unix.mkfifo (J.path dir) 0o600;
    expect "read accepted FIFO" (try ignore (J.read_all dir); false with _ -> true);
    expect "append accepted FIFO" (try J.append dir (row "new"); false with _ -> true))

let () = Test_workspace.with_dir "commit_frames" run