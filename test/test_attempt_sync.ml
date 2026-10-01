(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Recovery = Octra_core.Startup_recovery
module Journal = Octra_core.Commit_journal

type step = Chain | Index | Irmin | Head | Journal_file | Journal_dir

let name = function
  | Chain -> "chain" | Index -> "index" | Irmin -> "irmin" | Head -> "head"
  | Journal_file -> "journal_file" | Journal_dir -> "journal_dir"

let aborts dir = Journal.read_all dir |> List.filter (function
  | Journal.Abort _ -> true | _ -> false) |> List.length

let stage dir head =
  Case.advance dir head;
  Case.change_prepare dir (function
    | Journal.Prepare row -> Journal.Prepare {row with commit_id = "cut-attempt"}
    | _ -> failwith "unexpected test record");
  Case.with_stores dir (fun _ store ->
    let prior = Option.get (Lwt_main.run
      (Case.SI.Store.Branch.find store.Case.SI.repo "epoch_0")) in
    Lwt_main.run (Case.SI.Store.Head.set store.store prior));
  Case.Marker.write_marker dir 1 "wal_written"

let effects step killed after =
  let fail () =
    if killed then begin Unix.kill (Unix.getpid ()) Sys.sigkill; Unix._exit 99 end
    else raise (Unix.Unix_error (Unix.EIO, "fsync", "test")) in
  let real = Recovery.effects in
  match step with
  | Chain -> {real with sync_chain = (fun _ -> fail ())}
  | Index -> {real with sync_index = (fun _ -> fail ())}
  | Irmin -> {real with sync_irmin = (fun _ -> fail ())}
  | Head -> {real with sync_head = (fun _ -> fail ())}
  | Journal_file | Journal_dir ->
    let count = ref 0 in
    let target = if step = Journal_file then 1 else 2 in
    let sync fd =
      incr count;
      if !count = target then begin if after then Unix.fsync fd; fail () end
      else Unix.fsync fd in
    {real with append_journal = Journal.append ~sync}

let interrupt dir step killed after =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try
      Case.with_stores dir (fun chaindata store ->
        ignore (Lwt_main.run (Recovery.recover_using (effects step killed after)
          ~data_dir:dir ~chaindata ~store)));
      exit 0
    with exn -> Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string exn); exit 2)
  | pid -> Case.wait pid

let retry dir =
  let calls = ref 0 in
  let sync fd = incr calls; Unix.fsync fd in
  let effects = {Recovery.effects with append_journal = Journal.append ~sync} in
  Case.with_stores dir (fun chaindata store ->
    ignore (Lwt_main.run (Recovery.recover_using effects ~data_dir:dir ~chaindata ~store)));
  Case.expect "retry did not synchronize retirement file and directory" (!calls = 2)

let run_case root step killed after =
  let label = name step ^ (if killed then "_kill" else "_error") ^
    (if after then "_after" else "_before") in
  let dir = Filename.concat root label in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  stage dir head;
  let result = interrupt dir step killed after in
  Case.expect "interruption did not stop retirement"
    (result = if killed then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 2);
  Case.expect "interruption changed HEAD" (Case.HM.load dir = Some head);
  Case.expect "interruption removed WAL" (List.length (Case.Wal.read_pending dir) = 1);
  Case.expect "interruption removed recovery guard" (Case.Marker.recovery_required dir);
  let expected = match step with Journal_file | Journal_dir -> 1 | _ -> 0 in
  Case.expect "attempt retired before state synchronization" (aborts dir = expected);
  retry dir;
  Case.expect "successful cut changed HEAD" (Case.HM.load dir = Some head);
  Case.expect "successful cut retained WAL" (Case.Wal.read_pending dir = []);
  Case.expect "retry skipped visible retirement" (aborts dir = expected + 1);
  retry dir;
  Case.advance dir head;
  Case.expect "successor recovery refused" (Case.recover dir = Unix.WEXITED 0);
  let head = Option.get (Case.HM.load dir) in
  Case.expect "successor did not advance" (head.epoch_id = 1);
  Case.expect "successor did not select the new attempt" (head.commit_id = "phase-forward");
  Printf.printf "event = passed case = %s\n%!" label

let run root =
  let failed = ref false in
  List.iter (fun step -> List.iter (fun killed ->
    let positions = match step with Journal_file | Journal_dir -> [false; true] | _ -> [false] in
    List.iter (fun after ->
      try run_case root step killed after with exn ->
        failed := true;
        Printf.eprintf "event = failed case = %s kill = %b after = %b reason = %s\n%!"
          (name step) killed after (Printexc.to_string exn)) positions) [false; true])
    [Chain; Index; Irmin; Head; Journal_file; Journal_dir];
  if !failed then exit 1

let () = Test_workspace.with_dir "attempt_sync" run