(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Recovery = Octra_core.Startup_recovery

type step = Chain | Index | Irmin | Head_file | Head_dir | Head_sync

let name = function
  | Chain -> "chain" | Index -> "index" | Irmin -> "irmin"
  | Head_file -> "head_file" | Head_dir -> "head_dir" | Head_sync -> "head_sync"

let injected killed () =
  if killed then Unix.kill (Unix.getpid ()) Sys.sigkill
  else raise (Unix.Unix_error (Unix.EIO, "fsync", "test"))

let effects step killed =
  let real = Recovery.effects in
  let fail () = injected killed () in
  let sync count target fd =
    incr count;
    if !count = target then fail () else Unix.fsync fd in
  match step with
  | Chain -> {real with sync_chain = (fun _ -> fail ())}
  | Index -> {real with sync_index = (fun _ -> fail ())}
  | Irmin -> {real with sync_irmin = (fun _ -> fail ())}
  | Head_file | Head_dir ->
    let count = ref 0 in
    let target = if step = Head_file then 1 else 2 in
    {real with write_head = (fun dir head -> Case.HM.atomic_write ~sync:(sync count target) dir head)}
  | Head_sync -> {real with sync_head = (fun _ -> fail ())}

let interrupt dir step killed =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try
      Case.with_stores dir (fun chaindata store ->
        ignore (Lwt_main.run (Recovery.recover_using (effects step killed)
          ~data_dir:dir ~chaindata ~store)));
      exit 0
    with exn -> Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string exn); exit 2)
  | pid -> Case.wait pid

let validate dir =
  Case.with_stores dir (fun chaindata store ->
    let head = Option.get (Case.HM.load dir) in
    Case.expect "recovery published wrong epoch" (head.epoch_id = 1);
    Case.expect "recovery published wrong root"
      (Some (Case.HM.ledger_state_root head) = Lwt_main.run (Case.SI.get_head_hash store));
    Case.expect "recovery published wrong commit"
      (head.irmin_commit = Lwt_main.run (Case.SI.get_commit_hash store));
    Case.expect "recovery changed journal high-water" (head.txid_hi = 1L);
    Case.expect "recovery changed transaction"
      (Case.SC.get_tx_by_hash chaindata (Case.hash 'b') <> None);
    head)

let run root step killed =
  let label = name step ^ (if killed then "_kill" else "_error") in
  let dir = Filename.concat root label in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  Case.advance dir head;
  let outcome = interrupt dir step killed in
  Case.expect "injected interruption was not reached"
    (outcome = (if killed then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 2));
  Case.expect "interrupted recovery removed WAL" (List.length (Case.Wal.read_pending dir) = 1);
  Case.expect "interrupted recovery cleared guard"
    (Octra_core.Epoch_commit_marker.recovery_required dir);
  let disk_head = Option.get (Case.HM.load dir) in
  Case.expect "interruption exposed mixed HEAD" (disk_head.epoch_id = 0 || disk_head.epoch_id = 1);
  Case.expect "recovery retry refused" (Case.recover dir = Unix.WEXITED 0);
  let recovered = validate dir in
  Case.expect "successful recovery retained WAL" (Case.Wal.read_pending dir = []);
  Case.expect "second recovery refused" (Case.recover dir = Unix.WEXITED 0);
  Case.expect "second recovery changed committed state" (validate dir = recovered);
  Printf.printf "event = passed case = %s\n%!" label

let run_all root =
  let failed = List.fold_left (fun failed step ->
    List.fold_left (fun failed killed ->
      try run root step killed; failed with exn ->
        Printf.eprintf "event = failed case = %s kill = %b reason = %s\n%!"
          (name step) killed (Printexc.to_string exn);
        true) failed [false; true]) false [Chain; Index; Irmin; Head_file; Head_dir; Head_sync] in
  if failed then exit 1

let run_trim root =
  List.iter (fun step -> List.iter (fun killed ->
    let label = name step ^ (if killed then "_kill" else "_error") in
    let dir = Filename.concat root label in
    Unix.mkdir dir 0o700;
    let head, _ = Case.prepare dir in
    let segment = Filename.concat dir "chaindata/txlog/seg000000.dat" in
    let prefix = Case.read segment in
    Case.with_stores dir (fun chain _ ->
      for _ = 0 to 3 do
        ignore (Octra_core.Txlog.append chain.Case.SC.txlog ~epoch_id:1
          ~payload:(Case.hash 'b' ^ "{}"));
        Octra_core.Txlog.rotate chain.txlog
      done;
      Case.SC.fsync chain);
    Case.Marker.write_marker dir 1 "stage_batch_begin";
    let outcome = interrupt dir step killed in
    Case.expect "trim interruption was not reached"
      (outcome = (if killed then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 2));
    Case.expect "trim changed committed HEAD" (Case.HM.load dir = Some head);
    Case.expect "trim changed committed bytes" (Case.read segment = prefix);
    Case.expect "trim interruption erased commit marker" (Case.Marker.read_marker dir <> None);
    Case.expect "trim interruption cleared recovery guard" (Case.Marker.recovery_required dir);
    for _ = 1 to 2 do
      Case.expect "trim retry refused" (Case.recover dir = Unix.WEXITED 0);
      Case.expect "trim retry changed HEAD" (Case.HM.load dir = Some head);
      Case.expect "trim retry changed bytes" (Case.read segment = prefix);
      Case.with_stores dir (fun chain store ->
        Case.expect "trim retry changed Irmin"
          (Case.SI.get_commit_hash store |> Lwt_main.run = head.irmin_commit);
        Case.expect "trim retry retained suffix"
          (Case.SC.txlog_position chain = (Option.get head.txlog_seg, Option.get head.txlog_off)))
    done;
    Printf.printf "event = passed case = trim_%s\n%!" label)
    [false; true]) [Chain; Index; Irmin; Head_sync]

let () =
  Test_workspace.with_dir "phase_sync" run_all;
  Test_workspace.with_dir "trim_sync" run_trim