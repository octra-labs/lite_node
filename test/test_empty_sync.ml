(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Recovery = Octra_core.Startup_recovery
module Journal = Octra_core.Commit_journal
module Attempt = Octra_core.Commit_attempt

type step = Chain | Index | Irmin | Journal_file | Journal_dir

let name = function
  | Chain -> "chain" | Index -> "index" | Irmin -> "irmin"
  | Journal_file -> "journal_file" | Journal_dir -> "journal_dir"

let prepare id = Journal.Prepare {commit_id = id; prev_generation = -1;
  epoch_id = 0; planned_txid_hi = -1L; planned_state_root = Case.hash 'a'; ts = 1.}

let aborts dir = Journal.read_all dir |> List.filter (function
  | Journal.Abort _ -> true | _ -> false) |> List.length

let effects step killed after =
  let fail () =
    if killed then begin Unix.kill (Unix.getpid ()) Sys.sigkill; Unix._exit 99 end
    else raise (Unix.Unix_error (Unix.EIO, "fsync", "test")) in
  let real = Recovery.effects in
  match step with
  | Chain -> {real with sync_chain = (fun _ -> fail ())}
  | Index -> {real with sync_index = (fun _ -> fail ())}
  | Irmin -> {real with sync_irmin = (fun _ -> fail ())}
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

let retry dir expected =
  let calls = ref [] in
  let record step = calls := step :: !calls in
  let real = Recovery.effects in
  let sync fd = record "journal_sync"; Unix.fsync fd in
  let effects = Recovery.{
    sync_chain = (fun chain -> record "chain"; real.sync_chain chain);
    sync_index = (fun index -> record "index"; real.sync_index index);
    sync_irmin = (fun store -> record "irmin"; real.sync_irmin store);
    write_head = (fun _ _ -> failwith "initial recovery published HEAD");
    sync_head = (fun _ -> failwith "initial recovery synchronized absent HEAD");
    append_journal = Journal.append ~sync} in
  Case.with_stores dir (fun chaindata store ->
    ignore (Lwt_main.run (Recovery.recover_using effects ~data_dir:dir ~chaindata ~store)));
  let journal = List.init (2 * expected) (fun _ -> "journal_sync") in
  Case.expect "initial recovery synchronization order differs"
    (List.rev !calls = ["chain"; "index"; "irmin"] @ journal);
  Case.expect "initial recovery removed the boot guard" (Case.Marker.recovery_required dir);
  Case.expect "initial recovery created HEAD" (Case.HM.load_result dir = Case.HM.Missing);
  match Attempt.read ~epoch:0 ~generation:(-1) (Journal.read_all dir) with
  | Ok rows -> Case.expect "initial recovery left an active attempt" (Attempt.active rows = Ok None)
  | Error reason -> failwith reason

let run_case root step killed after =
  let label = name step ^ (if killed then "_kill" else "_error") ^
    (if after then "_after" else "_before") in
  let dir = Filename.concat root label in
  Unix.mkdir dir 0o700;
  Case.with_stores dir (fun _ _ -> ());
  Journal.append dir (prepare "first");
  let result = interrupt dir step killed after in
  Case.expect "interruption did not stop initial retirement"
    (result = if killed then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 2);
  Case.expect "interruption published HEAD" (Case.HM.load_result dir = Case.HM.Missing);
  Case.expect "interruption removed the boot guard" (Case.Marker.recovery_required dir);
  let expected = match step with Journal_file | Journal_dir -> 1 | _ -> 0 in
  Case.expect "initial attempt retired before state synchronization" (aborts dir = expected);
  retry dir 1;
  Case.expect "retry skipped a visible initial retirement" (aborts dir = expected + 1);
  retry dir 1;
  Journal.append dir (prepare "second");
  retry dir 2;
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
    [Chain; Index; Irmin; Journal_file; Journal_dir];
  if !failed then exit 1

let () = Test_workspace.with_dir "empty_sync" run