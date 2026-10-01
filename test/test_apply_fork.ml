(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Recovery_case
module Start = Octra_node_runtime.Wal_start
module Boot = Octra_node_runtime.Startup_node_boot_shell
module Fork = Octra_core.Fork_head_repair
module J = Octra_core.Commit_journal

let child action =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try action (); exit 0 with exn ->
      Printf.eprintf "event = child_error reason = %s\n%!" (Printexc.to_string exn); exit 2)
  | pid -> C.wait pid

let boot dir = child (fun () ->
  C.expect "early check failed" (Start.check dir = Ok ());
  C.with_stores dir (fun chaindata store ->
    let exit_fatal () = exit 78 in
    match Start.recover ~data_dir:dir (fun () ->
      Boot.run_store {data_dir = dir; store; exit_fatal};
      Boot.run_node {
        data_dir = dir; store; chaindata; ledger = Octra_core.Ledger.create store;
        total_tx_count = ref 0; observer_mode = true;
        wallet = {address = "octFROM"; pub = ""};
        consensus_mode = true; voting_consensus_mode = false;
        consensus_port_configured = (fun () -> true);
        validators = (fun () -> []); int_value = (fun _ value -> value);
        env = (fun _ -> None); exit_fatal;
      }) with
    | Ok epoch -> C.expect "next epoch differs" (epoch = 1)
    | Error error ->
      Printf.eprintf "event = boot_refused reason = %s\n%!" error.reason; exit Start.exit_code))

let save chain = C.SC.save_tx chain ~hash:(C.hash 'b') ~epoch_id:1
  ~from_addr:"octFROM" ~to_addr:"octTO" ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
  ~op_type:"standard" ~encrypted_data:"" ~message:""

let apply dir stop =
  let head, _ = C.prepare dir in
  let outcome = child (fun () -> C.with_stores dir (fun chain store ->
    C.SC.begin_batch chain;
    Lwt_main.run (C.SI.begin_epoch_batch store);
    Lwt_main.run (C.SI.set_meta store "last_epoch" "1");
    save chain;
    if stop = "sync" then C.SC.fsync chain;
    if stop = "prepare" then J.append dir (J.Prepare {
      commit_id = "before-wal"; prev_generation = head.generation; epoch_id = 1;
      planned_txid_hi = Int64.succ head.txid_hi; planned_state_root = C.hash '9'; ts = 0.});
    if List.mem stop ["kill"; "sync"; "prepare"] then begin
      Unix.kill (Unix.getpid ()) Sys.sigkill; Unix._exit 99
    end;
    if stop = "error" then failwith "apply interruption";
    C.SC.abort_batch chain;
    C.expect "abort did not restore transaction counter"
      (C.SC.next_txid chain = Int64.succ head.txid_hi))) in
  C.expect "wrong apply termination" (outcome = match stop with
    | "kill" | "sync" | "prepare" -> Unix.WSIGNALED Sys.sigkill
    | "error" -> Unix.WEXITED 2 | _ -> Unix.WEXITED 0);
  C.expect "pre-WAL interruption prevents startup" (boot dir = Unix.WEXITED 0);
  C.expect "second startup failed" (boot dir = Unix.WEXITED 0);
  C.with_stores dir (fun chain _ ->
    C.expect "committed HEAD changed" (C.HM.load dir = Some head);
    C.expect "committed transaction missing" (C.SC.get_tx_by_hash chain (C.hash 'a') <> None);
    C.expect "uncommitted transaction visible" (C.SC.get_tx_by_hash chain (C.hash 'b') = None);
    C.expect "pre-WAL apply left journal bytes"
      (C.SC.txlog_position chain = (Option.get head.txlog_seg, Option.get head.txlog_off)))

let prepare_fork dir =
  let head, _ = C.prepare dir in
  C.with_stores dir (fun chain store ->
    Lwt_main.run (C.SI.tag_epoch store 0);
    Lwt_main.run (C.SI.begin_epoch_batch store);
    Lwt_main.run (C.SI.set_meta store "last_epoch" "1");
    Lwt_main.run (C.SI.set_meta store "current_epoch" "2");
    Lwt_main.run (C.SI.commit_epoch_batch store "empty epoch");
    let ledger = Option.get (Lwt_main.run (C.SI.get_head_hash store)) in
    let epoch_hash, root = C.Eic.next_root ~prev:(Option.get head.epoch_index_root) ~epoch_id:1 [] in
    let state_root = C.Eic.folded_state_root ~ledger_state_root:ledger ~epoch_index_root:root in
    C.SC.begin_batch chain;
    C.SC.set_epoch chain {C.EL.empty_epoch_header with id = 1; state_root;
      prev_state_root = head.state_root; parent_commit = Option.get head.irmin_commit;
      start_txid = Int64.succ head.txid_hi; tx_count = 0};
    C.SC.set_epoch_index_commitment chain ~epoch_id:1 ~epoch_hash ~root;
    C.SC.commit_batch chain;
    C.SC.fsync chain;
    let next = {head with C.HM.epoch_id = 1; generation = 1; state_root;
      ledger_state_root = Some ledger; irmin_commit = Lwt_main.run (C.SI.get_commit_hash store);
      epochlog_off = Some (C.SC.epochlog_offset chain); commit_id = "empty-one";
      epoch_index_hash = Some epoch_hash; epoch_index_root = Some root} in
    C.HM.atomic_write dir next;
    let plan = Lwt_main.run (Fork.plan ~data_dir:dir ~store ~chaindata:chain
      ~target:0 ~root:head.state_root) |> Result.get_ok in
    J.append dir (J.Prepare {commit_id = next.commit_id; prev_generation = 0;
      epoch_id = 1; planned_txid_hi = head.txid_hi; planned_state_root = state_root; ts = 0.});
    J.append dir (J.Commit {commit_id = next.commit_id; generation = 1; ts = 0.});
    head, next, plan)

let fork dir mode =
  let head, next, plan = prepare_fork dir in
  let before = C.evidence dir in
  C.with_stores dir (fun chain store ->
    if mode = "stage" then begin
      let result = Lwt_main.run (Fork.stage_empty ~data_dir:dir ~store ~chaindata:chain
        ~target:0 ~root:head.state_root) in
      C.expect "committed fork was staged" (match result with Fork.Snapshot_required _ -> true | _ -> false);
      C.expect "refused stage wrote repair log" (Fork.read_plan dir = Ok None)
    end else begin
      Octra_core.Fork_repair_log.write dir plan;
      if mode = "store" then
        C.expect "stored plan rewound committed HEAD"
          (Result.is_error (Lwt_main.run (Fork.resume_store ~data_dir:dir ~store)))
      else begin
        C.HM.atomic_write dir plan.head;
        C.expect "stored plan cut committed journal"
          (Result.is_error (Fork.resume_chain ~data_dir:dir ~chaindata:chain));
        C.HM.atomic_write dir next
      end
    end);
  C.expect "refused repair changed store evidence" (C.evidence dir = before)

let failed_write dir =
  let head, _ = C.prepare dir in
  C.with_stores dir (fun chain _ ->
    C.SC.begin_batch chain;
    save chain;
    let descriptor = (C.SC.txlog chain).current_fd in
    let saved = Unix.dup descriptor in
    let refused = Fun.protect ~finally:(fun () ->
      Unix.dup2 saved descriptor; Unix.close saved) (fun () ->
      Unix.close descriptor;
      try C.SC.set_epoch chain C.EL.empty_epoch_header; false
      with Unix.Unix_error (Unix.EBADF, _, _) -> true) in
    C.expect "write error was ignored" refused;
    C.SC.abort_batch chain;
    C.expect "abort reset a failed journal write"
      (try C.SC.begin_batch chain; false with Invalid_argument _ -> true);
    ignore (C.SC.rollback_to_head chain ~head
      ~inflight_start_txid:(Int64.succ head.txid_hi) ~inflight_tx_count:1);
    C.SC.begin_batch chain;
    C.SC.abort_batch chain);
  C.expect "write recovery prevents startup" (boot dir = Unix.WEXITED 0)

let run root =
  let cases = List.map (fun mode -> "apply_" ^ mode, fun dir -> apply dir mode)
    ["kill"; "sync"; "prepare"; "error"; "abort"]
    @ List.map (fun mode -> "fork_" ^ mode, fun dir -> fork dir mode) ["stage"; "store"; "chain"]
    @ ["write_error", failed_write] in
  let failed = List.fold_left (fun failed (name, action) ->
    let dir = Filename.concat root name in
    Unix.mkdir dir 0o700;
    try action dir; Printf.printf "event = passed case = %s\n%!" name; failed
    with exn -> Printf.eprintf "event = failed case = %s reason = %s\n%!"
      name (Printexc.to_string exn); true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "apply_fork" run