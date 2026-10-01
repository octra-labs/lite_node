(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Recovery_case

let corrupt path =
  let fd = Unix.openfile path [Unix.O_RDWR] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    ignore (Unix.lseek fd 24 Unix.SEEK_SET);
    expect "short corruption write" (Unix.write_substring fd "x" 0 1 = 1);
    Unix.fsync fd)

let remove_hash dir =
  with_stores dir (fun chain _ ->
    let index = chain.SC.index in
    ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      Lmdb.Map.remove index.tx_loc ~txn (hash 'a'))))

let append_suffix dir =
  with_stores dir (fun chain _ ->
    ignore (Octra_core.Txlog.append chain.SC.txlog ~epoch_id:1 ~payload:(hash 'b' ^ "{}")))

let index_write dir action =
  with_stores dir (fun chain _ ->
    ignore (Lmdb.Txn.go Lmdb.Rw chain.SC.index.env (action chain.index)))

let suffix_cases = [
  "torn_header", (fun dir _ _ ->
    let file = Filename.concat dir "chaindata/txlog/seg000001.dat" in
    let channel = open_out_bin file in
    Fun.protect ~finally:(fun () -> close_out_noerr channel)
      (fun () -> output_string channel "OTX")), true;
  "torn_suffix", (fun dir _ _ ->
    let file = Filename.concat dir "chaindata/txlog/seg000000.dat" in
    Unix.truncate file ((Unix.stat file).st_size - 3)), true;
  "staging_suffix", (fun dir _ _ -> Marker.write_marker dir 1 "stage_batch_begin"), true;
  "prepared_suffix", (fun dir head _ ->
    Octra_core.Commit_journal.append dir (Octra_core.Commit_journal.Prepare {
      commit_id = "next"; prev_generation = head.HM.generation; epoch_id = 1;
      planned_txid_hi = 1L; planned_state_root = hash '9'; ts = 0.})), true;
  "suffix_commit_missing", (fun dir head _ -> HM.atomic_write dir {head with HM.irmin_commit = None}), false;
  "suffix_prefix_changed", (fun dir _ _ -> corrupt (Filename.concat dir "chaindata/txlog/seg000000.dat")), false;
  "suffix_hash_missing", (fun dir _ _ -> remove_hash dir), false;
  "suffix_wal_marker", (fun dir _ _ -> Marker.write_marker dir 1 "wal_written"), false;
  "suffix_next_txid", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.meta ~txn "next_txid" "2")), false;
  "suffix_epoch_meta", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.epoch_meta ~txn 1l "{}")), false;
  "suffix_addr", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.add index.Octra_core.Chaindata_index.addr_tx ~txn "octFROM" 1L)), false;
  "suffix_addr_wrong", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.add index.Octra_core.Chaindata_index.addr_tx ~txn "octUNRELATED" 0L)), false;
  "suffix_addr_missing", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.remove index.Octra_core.Chaindata_index.addr_tx ~txn ~value:0L "octFROM")), false;
  "suffix_recent_wrong", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.addr_recent ~txn "octUNRELATED"
      (Octra_core.Chaindata_index.encode_addr_recent [0, 0L]))), false;
  "suffix_recent", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.addr_recent ~txn "octFROM"
      (Octra_core.Chaindata_index.encode_addr_recent [1, 1L]))), false;
  "suffix_eic_future", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.meta ~txn "eic_epoch_root:1" (hash '9'))), false;
  "suffix_eic_latest", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.meta ~txn "eic_latest_root" (hash '9'))), false;
  "suffix_txid", (fun dir _ _ -> index_write dir (fun index txn ->
    let bytes = Lmdb.Map.get index.Octra_core.Chaindata_index.txid_loc ~txn 0L in
    Lmdb.Map.set index.txid_loc ~txn 1L bytes)), false;
  "suffix_receipt", (fun dir _ _ -> index_write dir (fun index txn ->
    Lmdb.Map.set index.Octra_core.Chaindata_index.receipts ~txn (hash 'b') {|{"epoch":1}|})), false;
  "suffix_later_prepare", (fun dir _ _ ->
    Octra_core.Commit_journal.append dir (Octra_core.Commit_journal.Prepare {
      commit_id = "later"; prev_generation = 1; epoch_id = 2;
      planned_txid_hi = 2L; planned_state_root = hash '9'; ts = 0.})), false
]

let run root =
  let cases = [
    "clean", (fun _ _ _ -> ()), true;
    "boot_same_meta", (fun dir _ _ -> with_stores dir (fun _ store ->
      Lwt_main.run (SI.set_meta store "current_epoch" "1"))), true;
    "wal_done", (fun dir _ entry -> Wal.write dir entry), true;
    "tag_done", (fun dir _ _ -> Marker.write_marker dir 0 "irmin_committed"), true;
    "hash_missing", (fun dir _ _ -> remove_hash dir), true;
    "eic_missing", (fun dir head _ ->
      HM.atomic_write dir {head with HM.epoch_index_hash = None; epoch_index_root = None}), true;
    "index_fields_missing", (fun dir head _ ->
      remove_hash dir;
      HM.atomic_write dir {head with HM.epoch_index_hash = None; epoch_index_root = None}), true;
    "wal_bad_frame", (fun dir _ entry ->
      Wal.write dir entry;
      corrupt (Filename.concat dir "chaindata/txlog/seg000000.dat")), false;
    "wal_bad_state", (fun dir head entry ->
      Wal.write dir entry;
      HM.atomic_write dir {head with HM.state_root = hash '4'}), false;
    "wal_bad_txid", (fun dir _ entry ->
      Wal.write dir {entry with Wal.start_txid = 42L}), false;
    "wal_bad_pre", (fun dir _ entry ->
      Wal.write dir {entry with Wal.pre_state_root = hash '4'}), false;
    "wal_bad_parent", (fun dir _ entry ->
      Wal.write dir {entry with Wal.parent_commit = hash '4'}), false;
    "tag_future", (fun dir _ _ -> Marker.write_marker dir 99 "irmin_committed"), false;
    "tag_bad_head", (fun dir _ _ ->
      Marker.write_marker dir 99 "irmin_committed";
      let channel = open_out_bin (Filename.concat dir "HEAD.json") in
      Fun.protect ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel "{")), false;
    "root_changed", (fun dir _ _ -> with_stores dir (fun _ store ->
      Lwt_main.run (SI.set_meta store "changed" "yes"))), false;
    "tag_conflict", (fun dir _ _ -> with_stores dir (fun _ store ->
      let original = Option.get (Lwt_main.run (SI.Store.Head.find store.SI.store)) in
      Lwt_main.run (SI.set_meta store "changed" "yes");
      Lwt_main.run (SI.tag_epoch store 0);
      Lwt_main.run (SI.Store.Head.set store.store original))), false;
    "suffix_without_wal", (fun dir _ _ -> append_suffix dir), true;
    "head_lag", (fun dir _ _ -> with_stores dir (fun _ store ->
      Lwt_main.run (SI.set_meta store "last_epoch" "1"))), false]
    @ List.map (fun (name, alter, succeeds) -> name,
        (fun dir head entry -> append_suffix dir; alter dir head entry), succeeds) suffix_cases in
  let failed = List.fold_left (fun failed (name, alter, succeeds) ->
    try
      let dir = Filename.concat root name in
      Unix.mkdir dir 0o700;
      let head, entry = prepare dir in
      alter dir head entry;
      if name = "torn_header" then with_stores dir (fun chain _ ->
        expect "incomplete segment permitted append"
          (match Octra_core.Txlog.append chain.SC.txlog ~epoch_id:1 ~payload:"forbidden" with
           | _ -> false | exception Failure _ -> true);
        expect "incomplete segment permitted rotation"
          (match Octra_core.Txlog.rotate chain.txlog with
           | () -> false | exception Failure _ -> true));
      let before = evidence dir in
      let outcome = recover dir in
      let after = evidence dir in
      let old_head, old_records, old_tags, old_commit = before in
      let new_head, new_records, new_tags, new_commit = after in
      Printf.printf "event = result case = %s exit = %s head_same = %b records_same = %b tags_same = %b commit_same = %b\n%!"
        name (match outcome with Unix.WEXITED n -> string_of_int n | _ -> "signal")
        (old_head = new_head) (old_records = new_records) (old_tags = new_tags)
        (old_commit = new_commit);
      if succeeds then begin
        expect "valid recovery refused" (outcome = Unix.WEXITED 0);
        expect "recovery changed HEAD value" (HM.load dir = Some head);
        with_stores dir (fun chain _ ->
          expect "committed transaction is missing" (SC.get_tx_by_hash chain (hash 'a') <> None);
          expect "recovery retained uncommitted suffix"
            (SC.txlog_position chain = (Option.get head.txlog_seg, Option.get head.txlog_off)));
        expect "repeat recovery refused" (recover dir = Unix.WEXITED 0);
        let _, _, _, repeated = evidence dir in
        expect "repeat recovery changed Irmin" (repeated = old_commit)
      end
      else begin
        expect "invalid recovery changed evidence" (before = after);
        expect "invalid recovery accepted" (outcome = Unix.WEXITED 1 || outcome = Unix.WEXITED 2)
      end;
      failed
    with exn ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "wal_phase" run