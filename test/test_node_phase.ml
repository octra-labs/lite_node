(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Boot = Octra_node_runtime.Startup_node_boot_shell
module Start = Octra_node_runtime.Wal_start

let boot dir =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try
      Case.expect "early WAL check refused" (Start.check dir = Ok ());
      Case.with_stores dir (fun chaindata store ->
        let exit_fatal () = failwith "boot refused" in
        Boot.run_store {data_dir = dir; store; exit_fatal};
        let ledger = Octra_core.Ledger.create store in
        let outcome = Start.recover ~data_dir:dir (fun () -> Boot.run_node {
          data_dir = dir; store; ledger; chaindata; total_tx_count = ref 0;
          observer_mode = true; wallet = {address = "octFROM"; pub = ""};
          consensus_mode = true; voting_consensus_mode = false;
          consensus_port_configured = (fun () -> true);
          validators = (fun () -> []); int_value = (fun _ value -> value);
          env = (fun _ -> None); exit_fatal;
        }) in
        let epoch = match outcome with
          | Ok epoch -> epoch
          | Error error ->
            Case.expect "recovery error path differs" (error.path = dir);
            Case.expect "recovery error reason missing" (error.reason <> "");
            exit Start.exit_code in
        Case.expect "boot next epoch differs" (epoch = 2));
      exit 0
    with exn -> Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string exn); exit 2)
  | pid -> Case.wait pid

let verify dir =
  Case.with_stores dir (fun chaindata store ->
    let head = Option.get (Case.HM.load dir) in
    Case.expect "boot HEAD epoch differs" (head.epoch_id = 1);
    Case.expect "boot HEAD root differs"
      (Some (Case.HM.ledger_state_root head) = Lwt_main.run (Case.SI.get_head_hash store));
    Case.expect "boot HEAD commit differs"
      (head.irmin_commit = Lwt_main.run (Case.SI.get_commit_hash store));
    Case.expect "boot lost committed transaction"
      (Case.SC.get_tx_by_hash chaindata (Case.hash 'b') <> None);
    Case.expect "boot retained WAL" (Case.Wal.read_pending dir = []);
    Case.expect "boot retained guard" (not (Case.Marker.recovery_required dir));
    head)

let run root =
  Case.expect "unrelated exception was classified as WAL failure"
    (try ignore (Start.recover ~data_dir:root (fun () -> failwith "control")); false
     with Failure reason -> reason = "control" | _ -> false);
  let cases = [
    "forward", (fun _ -> ()), true;
    "wal_pre", (fun dir -> Case.change_wal dir (fun entry ->
      {entry with Case.Wal.pre_state_root = Case.hash '6'})), false;
    "wal_range", (fun dir -> Case.change_wal dir (fun entry ->
      {entry with Case.Wal.start_txid = 42L})), false;
    "prepare_root", (fun dir -> Case.change_prepare dir (function
      | Octra_core.Commit_journal.Prepare entry ->
        Octra_core.Commit_journal.Prepare {entry with planned_state_root = Case.hash '6'}
      | _ -> assert false)), false;
    "marker_epoch", (fun dir -> Case.Marker.write_marker dir 99 "irmin_committed"), false] in
  let failed = List.fold_left (fun failed (name, alter, valid) ->
    try
      let dir = Filename.concat root name in
      Unix.mkdir dir 0o700;
      let head, _ = Case.prepare dir in
      Case.advance dir head;
      alter dir;
      let before = Case.evidence dir in
      let outcome = boot dir in
      if valid then begin
        Case.expect "valid boot refused" (outcome = Unix.WEXITED 0);
        let first = verify dir in
        Case.expect "boot retry refused" (boot dir = Unix.WEXITED 0);
        Case.expect "boot retry changed HEAD" (verify dir = first)
      end else begin
        Case.expect "invalid boot changed evidence" (Case.evidence dir = before);
        Case.expect "invalid boot exit code differs" (outcome = Unix.WEXITED 78)
      end;
      Printf.printf "event = passed case = %s\n%!" name;
      failed
    with exn ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!"
        name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "node_phase" run