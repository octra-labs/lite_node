(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Case = Recovery_case
module Commit = Octra_node_runtime.Consensus_epoch_commit
module Ledger = Octra_core.Ledger
module Journal = Octra_core.Commit_journal

external sync_count : unit -> int = "octra_sync_count"

let phase target name =
  Unix.putenv "OCTRA_TEST_SYNC_COUNT" "0";
  Unix.putenv "OCTRA_TEST_SYNC_TARGET" target;
  Unix.putenv "OCTRA_TEST_SYNC_PHASE" name

let child action =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try action (); exit 0 with exn ->
      Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string exn);
      exit 2)
  | pid -> Case.wait pid

let commit dir head = child (fun () ->
  Case.with_stores dir (fun chaindata store ->
    let ledger = Ledger.create store in
    let pre_state_root = Option.get (Lwt_main.run (Case.SI.get_head_hash store)) in
    let parent_commit = Option.get (Lwt_main.run (Case.SI.get_commit_hash store)) in
    let start_txid = Case.SC.next_txid chaindata in
    Case.SC.begin_batch chaindata;
    (match Ledger.begin_journal ledger with
    | Ok () -> () | Error reason -> failwith reason);
    Lwt_main.run (Case.SI.begin_epoch_batch store);
    Lwt_main.run (Case.SI.set_meta store "last_epoch" "1");
    Lwt_main.run (Case.SI.set_meta store "current_epoch" "2");
    let post_state_root = Option.get (Lwt_main.run (Case.SI.get_batch_tree_hash store)) in
    let epoch_index_hash, epoch_index_root = Case.Eic.next_root
      ~prev:(Option.get head.Case.HM.epoch_index_root) ~epoch_id:1 [] in
    let post_consensus_root = Case.Eic.folded_state_root
      ~ledger_state_root:post_state_root ~epoch_index_root in
    let plan = Octra_core.Epoch_exec.{
      base_reward = Z.zero; fees_burned = Z.zero; fees_rewarded = Z.zero;
      total_reward = Z.zero; proposer_total = Z.zero; each_validator = Z.zero;
      remainder = Z.zero; new_emission_remaining = Z.zero;
      new_total_supply = Z.zero; new_supply_retired = Z.zero;
      supply_tracking_active = false;
    } in
    let deps = Commit.{
      data_dir = dir; store; ledger; chaindata;
      trace = (fun _ -> ()); log = (fun _ -> ());
      fatal = (fun reason -> failwith reason);
      exit = (fun () -> failwith "unexpected commit exit");
    } in
    let effects = Commit.live_commit_effects deps in
    let failure_effects = Commit.live_failure_effects deps
      ~rollback:(fun () -> failwith "rollback after Irmin commit") in
    Lwt_main.run (Commit.run_commit ~effects ~failure_effects {
      epoch_id = 1; pre_state_root; post_state_root; post_consensus_root;
      prev_state_root = head.state_root; parent_commit; start_txid; tx_count = 0;
      finalized_by = "octFROM"; finalized_at = 1.;
      proposer = {creator_addr = "octFROM"; commit_round = 0};
      confirmed_fees = Z.zero; plan; reward_recipients = [];
      reward_source = {reward_proposer_addr = "octFROM";
        reward_proposer_public_key = None;
        reward_members = [{reward_address = "octFROM"; reward_public_key = None;
          reward_weight = Z.one}]};
      epoch_receipts_json = []; commit_id = "commit-sync"; prev_generation = 0;
      planned_txid_hi = head.txid_hi; epoch_index_hash; epoch_index_root;
      progress = Commit.commit_progress ();
    });
    Case.expect "HEAD published without requested synchronization" (sync_count () > 0)))

let control dir target =
  Case.with_stores dir (fun _ store ->
    phase target "control";
    Case.SI.sync_branches store.Case.SI.store_path;
    Case.expect "trace missed explicit synchronization" (sync_count () = 1);
    phase target "error";
    let refused = try Case.SI.sync_branches store.store_path; false with
      | Unix.Unix_error (Unix.EIO, _, _) -> true in
    Case.expect "injection missed explicit synchronization" (refused && sync_count () = 1);
    phase target "")

let run_case root target mode =
  let dir = Filename.concat root (target ^ "_" ^ mode) in
  Unix.mkdir dir 0o700;
  let head, _ = Case.prepare dir in
  control dir target;
  phase target mode;
  let stopped = commit dir head in
  phase target "";
  if mode = "commit" then
    Case.expect "complete commit failed" (stopped = Unix.WEXITED 0)
  else begin
    let killed = mode = "kill_before" || mode = "kill_after" in
    Case.expect "sync interruption did not stop commit"
      (stopped = if killed then Unix.WSIGNALED Sys.sigkill else Unix.WEXITED 2);
    Case.expect "sync interruption changed HEAD" (Case.HM.load_result dir = Case.HM.Present head);
    Case.expect "sync interruption removed WAL" (List.length (Case.Wal.read_pending dir) = 1);
    Case.expect "sync interruption removed commit marker" (Case.Marker.read_marker dir <> None);
    let journal = Journal.read_all dir in
    Case.expect "sync interruption completed the attempt"
      (match journal with [Journal.Prepare row] -> row.commit_id = "commit-sync" | _ -> false)
  end;
  Case.expect "successor recovery failed" (Case.recover dir = Unix.WEXITED 0);
  let current = match Case.HM.load_result dir with
    | Case.HM.Present value -> value | _ -> failwith "recovered HEAD is missing" in
  Case.expect "wrong recovered successor" (current.epoch_id = 1 && current.commit_id = "commit-sync");
  Case.expect "successor changed transaction range" (current.txid_hi = head.txid_hi);
  Case.expect "recovery retained completed WAL" (Case.Wal.read_pending dir = []);
  Case.expect "recovery cleared boot guard" (Case.Marker.recovery_required dir);
  Case.expect "repeated successor recovery failed" (Case.recover dir = Unix.WEXITED 0);
  Case.expect "repeated recovery changed HEAD" (Case.HM.load_result dir = Case.HM.Present current);
  Printf.printf "event = branch_cycle target = %s mode = %s status = pass\n%!" target mode

let run root =
  let failed = ref false in
  List.iter (fun target -> List.iter (fun mode ->
    try run_case root target mode with exn ->
      phase target "";
      failed := true;
      Printf.eprintf "event = branch_cycle target = %s mode = %s status = fail reason = %s\n%!"
        target mode (Printexc.to_string exn))
    ["commit"; "error"; "error_after"; "kill_before"; "kill_after"])
    ["branches"; "directory"];
  if !failed then exit 1

let () = Test_workspace.with_dir "branch_sync" run