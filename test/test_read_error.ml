(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module J = Octra_core.Commit_journal

let expect reason valid = if not valid then failwith reason
let handles () = Array.length (Sys.readdir (if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd"))
let write path text =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let contents path =
  match Unix.lstat path with
  | stat when stat.Unix.st_kind = Unix.S_REG ->
    let channel = open_in_bin path in
    Fun.protect ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Some (really_input_string channel (in_channel_length channel)))
  | _ -> None
  | exception Unix.Unix_error (Unix.ENOTDIR, _, _) -> None

let refused dir offset =
  let bytes = contents (J.path dir) in
  let before = handles () in
  List.iter (fun read ->
    let valid = try read dir; false with
      | J.Read_error (path, position, reason) ->
        path = J.path dir && position = offset && reason <> ""
      | _ -> false in
    expect "journal refusal lacks typed path or offset" valid;
    expect "journal refusal leaked a descriptor" (handles () = before);
    expect "journal refusal changed file bytes" (contents (J.path dir) = bytes))
    [(fun dir -> ignore (J.read_all dir)); J.check]

let run root =
  let first = "{\"type\":\"COMMIT\",\"commit_id\":\"first\",\"generation\":1,\"ts\":0.0}\n" in
  let cases = [
    "valid", (fun dir -> write (J.path dir) first; J.check dir;
      expect "journal fold lost records" (J.fold dir ~init:0 ~f:(fun count _ -> count + 1) = 1));
    "empty", (fun dir -> write (J.path dir) ""; J.check dir);
    "missing", (fun dir -> J.check dir);
    "malformed", (fun dir -> write (J.path dir) (first ^ "{\n"); refused dir (Int64.of_int (String.length first)));
    "unknown", (fun dir -> write (J.path dir) "{\"type\":\"OTHER\"}\n"; refused dir 0L);
    "directory", (fun dir -> Unix.mkdir (J.path dir) 0o700; refused dir 0L);
    "fifo", (fun dir -> Unix.mkfifo (J.path dir) 0o600; refused dir 0L);
    "link", (fun dir -> Unix.symlink (Filename.concat dir "missing") (J.path dir); refused dir 0L);
    "parent_file", (fun dir ->
      let file = Filename.concat dir "parent" in
      write file "retained";
      refused file 0L;
      expect "journal refusal changed parent" (contents file = Some "retained"))
  ] in
  let failed = ref false in
  List.iter (fun (name, test) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try test dir; Printf.printf "event = passed case = %s\n%!" name
    with error -> failed := true;
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string error)) cases;
  if !failed then exit 1

let () = Test_workspace.with_dir "read_error" run