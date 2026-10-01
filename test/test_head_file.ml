(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module H = Octra_core.Head_manifest

let expect reason valid = if not valid then failwith reason
let handles () = Array.length (Sys.readdir (if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd"))
let write path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () -> output_string channel bytes)
let valid = {|{"schema_version":3,"generation":1,"epoch_id":1,"state_root":"root","ledger_state_root":"ledger","irmin_commit":"commit","txid_hi":"9","txlog_seg":0,"txlog_off":42,"epochlog_off":63,"commit_id":"first","ts":0.0,"quorum_cert_hash":null,"epoch_index_hash":"index","epoch_index_root":"epochs"}|}

let corrupt dir = match H.load_result dir with H.Corrupt _ -> true | _ -> false

let run root =
  let cases = [
    "missing", (fun dir -> expect "missing HEAD refused" (H.load_result dir = H.Missing));
    "valid", (fun dir -> write (H.path dir) valid;
      expect "valid HEAD refused" (H.load_result dir = H.Present (H.of_json valid)));
    "dangling_link", (fun dir -> Unix.symlink (Filename.concat dir "missing") (H.path dir);
      expect "dangling HEAD link treated as missing" (corrupt dir));
    "file_link", (fun dir ->
      let target = Filename.concat dir "retained" in
      write target valid;
      Unix.symlink target (H.path dir);
      expect "HEAD reader followed a link" (corrupt dir));
    "parent_file", (fun dir ->
      let file = Filename.concat dir "parent" in
      write file "retained";
      expect "non-directory HEAD parent treated as missing" (corrupt file));
    "directory", (fun dir ->
      Unix.mkdir (H.path dir) 0o700;
      let before = handles () in
      for _ = 1 to 8 do expect "HEAD directory accepted" (corrupt dir) done;
      let after = handles () in
      Printf.printf "event = head_handles before = %d after = %d\n%!" before after;
      expect "HEAD read error leaked descriptors" (before = after));
    "malformed", (fun dir ->
      write (H.path dir) "{";
      expect "malformed HEAD accepted" (corrupt dir);
      expect "malformed HEAD load returned an empty pointer"
        (try ignore (H.load dir); false with _ -> true))
  ] in
  let failed = ref false in
  List.iter (fun (name, test) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try test dir; Printf.printf "event = passed case = %s\n%!" name
    with error -> failed := true;
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string error)) cases;
  if !failed then exit 1

let () = Test_workspace.with_dir "head_file" run