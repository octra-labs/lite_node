(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case

let run_child mode dir =
  try
    (match mode with
    | "irmin" ->
      let store = Lwt_main.run (Case.SI.open_store (Filename.concat dir "irmin_store")) in
      Lwt_main.run (Case.SI.close store)
    | "chain" ->
      let chain = Case.SC.open_chaindata (Filename.concat dir "chaindata") in
      Case.SC.close chain
    | "read" ->
      let store = Lwt_main.run (Case.SI.open_store ~readonly:true (Filename.concat dir "irmin_store")) in
      let chain = Case.SC.open_chaindata ~readonly:true (Filename.concat dir "chaindata") in
      Case.SC.close chain;
      Lwt_main.run (Case.SI.close store)
    | "close_error" ->
      let path = Filename.concat dir "chaindata" in
      let chain = Case.SC.open_chaindata path in
      Unix.close chain.Case.SC.txlog.Octra_core.Txlog.current_fd;
      let failed = match Case.SC.close chain with
        | () -> false
        | exception error ->
          Printf.printf "event = close_error reason = %S\n%!" (Printexc.to_string error);
          true in
      Case.expect "close failure was not reported" failed;
      (match Case.SC.open_chaindata path with
      | other -> Case.SC.close other; failwith "close failure released writer ownership"
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ())
    | "close_twice" ->
      let path = Filename.concat dir "chaindata" in
      let first = Case.SC.open_chaindata path in
      Case.SC.close first;
      let second = Case.SC.open_chaindata path in
      Fun.protect ~finally:(fun () -> Case.SC.close second) (fun () ->
        Case.SC.close first;
        Case.SC.fsync second)
    | _ -> failwith "unknown child mode");
    Printf.printf "event = child mode = %s status = opened\n%!" mode;
    exit 0
  with exn ->
    Printf.eprintf "event = child mode = %s status = refused reason = %s\n%!"
      mode (Printexc.to_string exn);
    exit 2

let execute ?(stderr = Unix.stderr) binary args =
  flush_all ();
  Case.wait (Unix.create_process binary (Array.of_list (binary :: args))
    Unix.stdin Unix.stdout stderr)

let status dir =
  Case.with_stores dir (fun chain store ->
    Printf.printf "event = state path = %s head = %d irmin = %s chain = %s wal = %d\n%!"
      dir (Option.get (Case.HM.load dir)).Case.HM.epoch_id
      (Option.value ~default:"missing" (Lwt_main.run (Case.SI.get_meta store "last_epoch")))
      (match Case.SC.last_epoch_id chain with
        | Ok (Some epoch) -> string_of_int epoch | Ok None -> "empty" | Error reason -> reason)
      (List.length (Case.Wal.read_pending dir)))

let test_open root mode allowed =
  let dir = Filename.concat root ("open_" ^ mode) in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  let outcome = Case.with_stores dir (fun _ _ ->
    execute Sys.executable_name ["child"; mode; dir]) in
  Printf.printf "event = open_test mode = %s exit = %s\n%!" mode
    (match outcome with Unix.WEXITED n -> string_of_int n | _ -> "signal");
  Case.expect "writer ownership result differs"
    (outcome = Unix.WEXITED (if allowed then 0 else 2))

let test_cut root tool held =
  let dir = Filename.concat root (if held then "cut_held" else "cut_closed") in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  Case.advance dir head;
  let before = Case.evidence dir in
  status dir;
  let outcome = if held then Case.with_stores dir (fun _ _ -> execute tool ["--offline"; dir])
    else execute tool ["--offline"; dir] in
  let after = Case.evidence dir in
  status dir;
  Printf.printf "event = cut_test held = %b exit = %s preserved = %b\n%!" held
    (match outcome with Unix.WEXITED n -> string_of_int n | _ -> "signal")
    (before = after);
  Case.expect "cut changed an Irmin-committed epoch" (before = after);
  Case.expect "cut accepted an Irmin-committed epoch" (outcome <> Unix.WEXITED 0)

let test_cut_owner root tool =
  let dir = Filename.concat root "cut_owner" in
  Unix.mkdir dir 0o700;
  let irmin = Filename.concat dir "irmin_store" in
  Unix.mkdir irmin 0o700;
  let chain = Case.SC.open_chaindata (Filename.concat dir "chaindata") in
  Fun.protect ~finally:(fun () -> Case.SC.close chain) (fun () ->
    let path = Filename.concat dir "cut_error" in
    let output = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in
    Case.expect "cut accepted held chain ownership"
      (Fun.protect ~finally:(fun () -> Unix.close output)
        (fun () -> execute ~stderr:output tool ["--offline"; dir]) = Unix.WEXITED 2);
    let reason = Case.read path in
    Case.expect "cut inspected Irmin before chain ownership"
      (List.exists (fun error -> reason = Printf.sprintf
        "event = rollback status = refused reason = %S\n"
        (Printexc.to_string (Unix.Unix_error (error, "flock", ""))))
        [Unix.EAGAIN; Unix.EWOULDBLOCK]);
    Case.expect "cut opened Irmin before chain ownership" (Sys.readdir irmin = [||]))

let test_closed root tool =
  let dir = Filename.concat root "cut_valid" in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  Case.advance dir head;
  Case.with_stores dir (fun _ store ->
    let prior = Option.get (Lwt_main.run (Case.SI.Store.Branch.find store.Case.SI.repo "epoch_0")) in
    Lwt_main.run (Case.SI.Store.Head.set store.store prior));
  Case.Marker.write_marker dir 1 "wal_written";
  let before = Case.evidence dir in
  Case.expect "cut without offline confirmation accepted" (execute tool [dir] = Unix.WEXITED 2);
  Case.expect "cut without offline confirmation changed files" (Case.evidence dir = before);
  Case.expect "valid offline cut refused" (execute tool ["--offline"; dir] = Unix.WEXITED 0);
  Case.expect "cut changed HEAD" (Case.HM.load dir = Some head);
  Case.expect "cut retained WAL" (Case.Wal.read_pending dir = []);
  Case.expect "completed cut repeat refused" (execute tool ["--offline"; dir] = Unix.WEXITED 0);
  Case.with_stores dir (fun chain store ->
    Case.expect "cut changed Irmin commit"
      (Lwt_main.run (Case.SI.get_commit_hash store) = head.Case.HM.irmin_commit);
    Case.expect "cut changed Irmin epoch"
      (Lwt_main.run (Case.SI.get_meta store "last_epoch") = Some "0");
    Case.expect "cut position differs"
      (Case.SC.txlog_position chain = (Option.get head.txlog_seg, Option.get head.txlog_off));
    Case.expect "cut epoch position differs"
      (Case.SC.epochlog_offset chain = Option.get head.epochlog_off));
  Case.expect "normal startup after cut refused" (Case.recover dir = Unix.WEXITED 0)

let test_same root =
  let dir = Filename.concat root "same_process" in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  Case.with_stores dir (fun _ _ ->
    match Case.SC.open_chaindata (Filename.concat dir "chaindata") with
    | chain -> Case.SC.close chain; failwith "same process opened second writer"
    | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), "flock", _) -> ())

let test_alias root =
  let dir = Filename.concat root "alias_owner" in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  let alias = Filename.concat root "alias_link" in
  Unix.symlink dir alias;
  Case.with_stores dir (fun _ _ ->
    Case.expect "directory alias opened second writer"
      (execute Sys.executable_name ["child"; "chain"; alias] = Unix.WEXITED 2))

let test_open_error root name file =
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  let path = Filename.concat dir file in
  let saved = Case.read path in
  let write value =
    let channel = open_out_bin path in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel value) in
  write "invalid";
  let refused = match Case.SC.open_chaindata (Filename.concat dir "chaindata") with
    | chain -> Case.SC.close chain; false
    | exception _ -> true in
  Case.expect "invalid journal accepted" refused;
  write saved;
  Case.expect "failed open retained writer ownership"
    (execute Sys.executable_name ["child"; "chain"; dir] = Unix.WEXITED 0)

let test_index_error root =
  let dir = Filename.concat root "index_error" in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  let path = Filename.concat dir "chaindata/index" in
  let saved = Filename.concat dir "index_saved" in
  Unix.rename path saved;
  let channel = open_out_bin path in
  close_out channel;
  let refused = match Case.SC.open_chaindata (Filename.concat dir "chaindata") with
    | chain -> Case.SC.close chain; false
    | exception _ -> true in
  Case.expect "invalid index accepted" refused;
  Unix.unlink path;
  Unix.rename saved path;
  Case.expect "failed index open retained writer ownership"
    (execute Sys.executable_name ["child"; "chain"; dir] = Unix.WEXITED 0)

let test_close root mode =
  let dir = Filename.concat root mode in
  Unix.mkdir dir 0o700;
  ignore (Case.prepare dir);
  Case.expect "close ownership test failed"
    (execute Sys.executable_name ["child"; mode; dir] = Unix.WEXITED 0)

let run root tool =
  let cases = [
    "irmin_owner", (fun () -> test_open root "irmin" false);
    "chain_owner", (fun () -> test_open root "chain" false);
    "read_owner", (fun () -> test_open root "read" true);
    "same_process", (fun () -> test_same root);
    "alias_owner", (fun () -> test_alias root);
    "tx_open_error", (fun () -> test_open_error root "tx_error" "chaindata/txlog/seg000000.dat");
    "epoch_open_error", (fun () -> test_open_error root "epoch_error" "chaindata/epochlog/epochs.dat");
    "index_open_error", (fun () -> test_index_error root);
    "close_error", (fun () -> test_close root "close_error");
    "close_twice", (fun () -> test_close root "close_twice");
    "cut_closed", (fun () -> test_cut root tool false);
    "cut_held", (fun () -> test_cut root tool true);
    "cut_owner", (fun () -> test_cut_owner root tool);
    "cut_valid", (fun () -> test_closed root tool)] in
  let failed = List.filter_map (fun (name, action) ->
    match action () with
    | () -> Printf.printf "event = passed case = %s\n%!" name; None
    | exception exn -> Printf.eprintf "event = failed case = %s reason = %s\n%!"
        name (Printexc.to_string exn); Some name) cases in
  if failed <> [] then exit 1

let () =
  match Array.to_list Sys.argv with
  | [_; "child"; mode; dir] -> run_child mode dir
  | [_; tool] -> Test_workspace.with_dir "store_owner" (fun root -> run root tool)
  | [_; root; tool] -> run root tool
  | _ -> failwith "repair command path is required"