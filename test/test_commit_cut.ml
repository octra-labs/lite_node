(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module J = Octra_core.Commit_journal

let expect reason valid = if not valid then failwith reason
let handles () = Array.length (Sys.readdir (if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd"))
let text row = Yojson.Safe.to_string (J.record_to_json row) ^ "\n"
let write path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel bytes)
let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let first = J.Prepare {commit_id = "first"; prev_generation = 7;
  epoch_id = 8; planned_txid_hi = 9L; planned_state_root = "root"; ts = 0.}

let run root =
  let rows = ["prepare", first;
    "commit", J.Commit {commit_id = "first"; generation = 8; ts = 0.};
    "abort", J.Abort {commit_id = "first"; reason = "test\ninterruption"; ts = 0.}] in
  List.iter (fun (name, row) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    let prefix = text first and line = text row in
    let path = J.path dir in
    let start = handles () in
    for count = 0 to String.length line do
      let bytes = prefix ^ String.sub line 0 count in
      write path bytes;
      if count = 0 || count = String.length line then begin
        let expected = if count = 0 then [first] else [first; row] in
        expect "complete frames changed" (J.read_all dir = expected)
      end else begin
        let refused = try ignore (J.read_all dir); false with
          | J.Read_error (file, offset, "unfinished record") ->
            file = path && offset = Int64.of_int (String.length prefix)
          | _ -> false in
        expect (Printf.sprintf "partial record accepted at byte %d" count) refused;
        expect "append extended partial record" (try J.append dir row; false with _ -> true)
      end;
      expect "frame inspection changed retained bytes" (read path = bytes)
    done;
    expect "frame inspection leaked descriptors" (handles () = start);
    Printf.printf "event = passed case = %s positions = %d\n%!" name (String.length line + 1)) rows

let () = Test_workspace.with_dir "commit_cut" run