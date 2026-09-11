(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module S = Octra_core.Store_irmin
module J = Octra_node_runtime.Consensus_finality_journal
module R = Octra_core.Fork_head_repair
module B = Octra_node_runtime.Fork_repair_boot
module F = Octra_consensus.Finality_log

let expect name value = if not value then failwith name

let ok = function
  | Ok value -> value
  | Error error -> failwith error

let gc_keep_case () =
  let proof_window = Int64.to_int J.history_limit in
  let read value name =
    if name = "OCTRA_GC_KEEP_EPOCHS" then value else None
  in
  expect "GC keep default differs"
    (S.gc_keep_epochs_of (read None) = Ok 8192);
  expect "GC keep proof window rejected"
    (S.gc_keep_epochs_of (read (Some (string_of_int proof_window)))
     = Ok proof_window);
  expect "GC keep below proof window accepted"
    (Result.is_error
       (S.gc_keep_epochs_of
          (read (Some (string_of_int (proof_window - 1))))));
  expect "GC keep above maximum accepted"
    (Result.is_error (S.gc_keep_epochs_of (read (Some "65537"))));
  expect "GC keep text accepted"
    (Result.is_error (S.gc_keep_epochs_of (read (Some "many"))))

let repair_source_case () =
  let source = Octra_core.Head_manifest.{
    schema_version;
    generation = 12;
    epoch_id = 12;
    state_root = "state";
    ledger_state_root = Some "ledger";
    irmin_commit = Some "first";
    txid_hi = 13L;
    txlog_seg = Some 1;
    txlog_off = Some 2;
    epochlog_off = Some 3;
    commit_id = "first";
    ts = 4.;
    quorum_cert_hash = Some "qc";
    epoch_index_hash = Some "index";
    epoch_index_root = Some "root";
  } in
  let rewritten = {
    source with
    irmin_commit = Some "second";
    commit_id = "second";
  } in
  expect "equivalent repair source rejected" (R.same_source source rewritten);
  expect "different repair root accepted"
    (not (R.same_source source { rewritten with state_root = "other" }))

let boot_head epoch root txid =
  Octra_core.Head_manifest.{
    schema_version;
    generation = epoch;
    epoch_id = epoch;
    state_root = root;
    ledger_state_root = Some root;
    irmin_commit = Some root;
    txid_hi = txid;
    txlog_seg = Some 0;
    txlog_off = Some 0;
    epochlog_off = Some 0;
    commit_id = root;
    ts = 0.;
    quorum_cert_hash = None;
    epoch_index_hash = Some root;
    epoch_index_root = Some root;
  }

let final_entry epoch root txid =
  F.{
    height = epoch;
    round = 0;
    proposal_id = root;
    tx_list_hash = root;
    state_root = root;
    creator_addr = root;
    txid_hi = txid;
    qc_hash = None;
    ts = 0.;
  }

let repair_boot_case () =
  let source = boot_head 12 "source" 13L in
  let target = boot_head 11 "target" 12L in
  let current = boot_head 14 "current" 15L in
  let plan = Octra_core.Fork_repair_log.{
    source;
    target_root = target.state_root;
    next_txid = 13L;
    head = target;
  } in
  let cleared = ref false in
  let rewound = ref false in
  let dropped = ref false in
  let deps head = B.{
    read_plan = (fun () -> Ok (Some plan));
    head = (fun () -> Some head);
    finality_at = (function
      | 11 -> Some (final_entry 11 "target" 12L)
      | 14 -> Some (final_entry 14 "current" 15L)
      | _ -> None);
    journal_committed = (fun () -> true);
    committed_for = (fun entry ->
      if entry.F.height = 14 then Ok () else Error "wrong current entry");
    rewind_journal = (fun _ -> rewound := true; Ok ());
    drop_after = (fun _ -> dropped := true; 3);
    clear = (fun () -> cleared := true);
  } in
  let advanced = B.run (deps current) in
  expect "advanced repair did not finish"
    (advanced = Ok (B.Resumed { target = 11; head = 14; dropped = 0 }));
  expect "advanced repair rewound journal" (not !rewound);
  expect "advanced repair dropped finality" (not !dropped);
  expect "advanced repair log survived" !cleared;
  cleared := false;
  let exact = B.run (deps target) in
  expect "target repair did not finish"
    (exact = Ok (B.Resumed { target = 11; head = 11; dropped = 3 }));
  expect "target repair did not rewind journal" !rewound;
  expect "target repair did not drop finality" !dropped;
  expect "target repair log survived" !cleared;
  cleared := false;
  let mismatched = B.run (deps { current with state_root = "other" }) in
  expect "advanced repair accepted mismatched HEAD" (Result.is_error mismatched);
  expect "mismatched repair cleared log" (not !cleared)

let rec remove path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stat when stat.Unix.st_kind = Unix.S_DIR ->
    Sys.readdir path |> Array.iter (fun name -> remove (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path

let head store =
  let* commit = S.Store.Head.find store.S.store in
  match commit with
  | None -> failwith "head missing"
  | Some commit -> Lwt.return commit

let value store expected =
  let* actual = S.read store ["value"] in
  expect "restored contents differ" (actual = Some expected);
  Lwt.return_unit

let epoch store number contents =
  let* () = S.write store ["value"] contents in
  S.tag_epoch store number

let no_split store =
  let* branches = S.Store.Branch.list store.S.repo in
  expect "split branch survived rollback"
    (List.for_all (fun branch -> S.split_id branch = None) branches);
  expect "split epoch survived rollback" (store.S.split_epoch = None);
  Lwt.return_unit

let run path =
  let* store = S.open_store ~fresh:true path in
  let* () = Lwt.finalize (fun () ->
    let* () = epoch store 1 "restored" in
    let* () = epoch store 2 "discarded" in
    let* split = S.collect_pack_at store ~keep:1 2 in
    expect "initial split missing" (split = S.Gc_split 2);
    let* () = epoch store 3 "tip" in
    let* plan = S.collect_plan store ~keep:1 3 in
    expect "plan not reserved" (match plan with `Measure _ -> true | _ -> false);
    let* saved = head store in
    let* refused = S.rollback_to_epoch store 1 in
    expect "rollback entered collection plan" (Result.is_error refused);
    let* current = head store in
    expect "refused rollback changed head" (S.Store.Commit.hash current = S.Store.Commit.hash saved);
    let* () = S.clear_gc_plan store in
    let* result = S.rollback_to_epoch store 1 in
    ignore (ok result);
    let* () = no_split store in
    let* () = value store "restored" in
    let* repeated = S.rollback_to_epoch store 1 in
    ignore (ok repeated);
    no_split store
  ) (fun () -> S.close store) in
  let* store = S.open_store path in
  let* () = Lwt.finalize (fun () ->
    let* () = no_split store in
    let* () = value store "restored" in
    let* _ = S.drop_epoch_tags_after store 1 in
    let* () = S.write store ["kept"] "old-tree-value" in
    let* () = epoch store 2 "new-branch" in
    let* split = S.collect_pack_at store ~keep:1 2 in
    expect "replacement split missing" (split = S.Gc_split 2);
    let* () = epoch store 3 "new-tip" in
    let* plan = S.collect_plan store ~keep:1 3 in
    let floor, commit = match plan with
      | `Measure value -> value
      | `Done _ -> failwith "second plan missing"
    in
    let* tip = head store in
    let branch = Printf.sprintf "pack_split_%d" floor in
    let* () = S.Store.Branch.set store.S.repo branch tip in
    let* rejected = S.start_pack_gc store ~keep:1 ~floor ~commit 3 in
    expect "same epoch replacement entered GC" (rejected = S.Gc_missing floor);
    let* () = S.Store.Branch.set store.S.repo branch commit in
    let* () = S.clear_gc_plan store in
    let* started = S.collect_pack_at ~free:Int64.max_int store ~keep:1 3 in
    expect "collection did not start" (match started with S.Gc_started _ -> true | _ -> false);
    if not (S.Store.Gc.is_finished store.S.repo) then begin
      let* refused = S.rollback_to_epoch store 2 in
      expect "rollback entered active GC" (Result.is_error refused);
      Lwt.return_unit
    end else Lwt.return_unit
  ) (fun () ->
    let* result = S.wait_pack_gc store in
    ignore (ok result);
    S.close store) in
  let* store = S.open_store path in
  Lwt.finalize (fun () ->
    let* () = value store "new-tip" in
    let* retained = S.read store ["kept"] in
    expect "GC removed restored tree value" (retained = Some "old-tree-value");
    let* old = S.read_at_epoch store 2 ["value"] in
    expect "GC removed retained epoch" (old = Some "new-branch");
    Lwt.return_unit
  ) (fun () -> S.close store)

let old_anchor path =
  let* store = S.open_store ~fresh:true path in
  let* () = Lwt.finalize (fun () ->
    let* () = epoch store 1 "old-head" in
    let* restored = head store in
    let* () = epoch store 2 "other-branch" in
    let* split = S.collect_pack_at store ~keep:1 2 in
    expect "old split missing" (split = S.Gc_split 2);
    S.Store.Head.set store.S.store restored
  ) (fun () -> S.close store) in
  let* store = S.open_store path in
  Lwt.finalize (fun () ->
    let* () = no_split store in
    value store "old-head"
  ) (fun () -> S.close store)

let () =
  gc_keep_case ();
  repair_source_case ();
  repair_boot_case ();
  let data = Filename.concat (Sys.getcwd ()) "runtime_data" in
  if not (Sys.file_exists data) then Unix.mkdir data 0o700;
  let path = Filename.concat data (Printf.sprintf "gc-head-%d" (Unix.getpid ())) in
  expect "test directory exists" (not (Sys.file_exists path));
  let old_path = path ^ "-old" in
  expect "old test directory exists" (not (Sys.file_exists old_path));
  Fun.protect ~finally:(fun () -> remove path; remove old_path) (fun () ->
    Lwt_main.run (let* () = run path in old_anchor old_path));
  Printf.printf "event = gc_head status = pass reopen = 3\n"