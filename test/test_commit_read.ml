(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module J = Octra_core.Commit_journal

let expect reason valid = if not valid then failwith reason

let write path contents =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let record = J.Prepare {commit_id = "first"; prev_generation = 7;
  epoch_id = 8; planned_txid_hi = 9L; planned_state_root = "root"; ts = 0.}

let json = J.record_to_json record
let text = Yojson.Safe.to_string json

let handles () =
  Array.length (Sys.readdir (if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd"))

let rejected dir contents =
  let path = J.path dir in
  write path contents;
  let refused = try ignore (J.read_all dir); false with _ -> true in
  expect "invalid commit journal accepted" refused;
  expect "refusal changed journal bytes" (read path = contents)

let leak dir =
  write (J.path dir) "{broken\n";
  let before = handles () in
  for _ = 1 to 8 do
    expect "malformed journal accepted" (try ignore (J.read_all dir); false with _ -> true)
  done;
  let after = handles () in
  Printf.printf "event = read_handles before = %d after = %d\n%!" before after;
  expect "malformed journal leaked a descriptor" (before = after)

let duplicate = match json with
  | `Assoc rows -> Yojson.Safe.to_string (`Assoc (("commit_id", `String "second") :: rows))
  | _ -> assert false

let nonfinite = {|{"type":"PREPARE","commit_id":"first","prev_generation":7,"epoch_id":8,"planned_txid_hi":"9","planned_state_root":"root","ts":1e999}|}

let run root =
  let cases = [
    "valid", (fun dir -> J.append dir record; expect "valid record changed" (J.read_all dir = [record]));
    "missing", (fun dir -> expect "missing journal fabricated records" (J.read_all dir = []));
    "unknown", (fun dir -> rejected dir (text ^ "\n{\"type\":\"OTHER\"}\n"));
    "duplicate", (fun dir -> rejected dir (duplicate ^ "\n"));
    "blank", (fun dir -> rejected dir (text ^ "\n\n"));
    "unfinished", (fun dir -> rejected dir text);
    "nonfinite", (fun dir -> rejected dir (nonfinite ^ "\n"));
    "parse_handles", leak;
    "append_unfinished", (fun dir ->
      let original = String.sub text 0 (String.length text - 3) in
      write (J.path dir) original;
      let refused = try J.append dir record; false with _ -> true in
      expect "append extended an unfinished record" refused;
      expect "refused append changed bytes" (read (J.path dir) = original));
  ] in
  let failed = ref false in
  List.iter (fun (name, test) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try test dir; Printf.printf "event = passed case = %s\n%!" name
    with exn -> failed := true;
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string exn)) cases;
  if !failed then exit 1

let () = Test_workspace.with_dir "commit_read" run