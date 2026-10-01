(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module Index = Octra_core.Chaindata_index
module Txlog = Octra_core.Txlog
module HM = Octra_core.Head_manifest
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment

let expect label ok =
  if not ok then failwith label

let hash value = Printf.sprintf "%064x" value

let with_store action =
  Test_workspace.with_dir "tx_loc_repair" (fun dir ->
    HM.cached := None;
    let store = SC.open_chaindata (Filename.concat dir "chaindata") in
    Fun.protect ~finally:(fun () -> SC.close store; HM.cached := None) (fun () -> action store))

let append store value =
  Txlog.append (SC.txlog store) ~epoch_id:1 ~payload:(hash value ^ "{}")

let seal store =
  let index = SC.index store in
  let items = match Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
    Txlog.fold_strict (SC.txlog store) ~init:(0L, []) ~f:(fun (txid, items) record ->
      Lmdb.Map.set index.txid_loc ~txn txid
        (Index.encode_txid_loc ~seg_id:record.segment ~offset:record.offset ~len:record.length);
      Ok (Int64.succ txid, Eic.item ~txid ~hash:(String.sub record.payload 0 64) :: items))) with
    | Some (Ok (_, items)) -> items
    | _ -> failwith "cannot seal test history" in
  let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:1 items in
  let ledger_root = hash 900 in
  let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:root in
  SC.begin_batch store;
  SC.set_epoch store { EL.empty_epoch_header with id = 1; start_txid = 0L;
    tx_count = List.length items; state_root };
  SC.set_epoch_index_commitment store ~epoch_id:1 ~epoch_hash ~root;
  SC.commit_batch store;
  let segment, offset = SC.txlog_position store in
  HM.set_cached HM.{ schema_version; generation = 1; epoch_id = 1; state_root;
    ledger_state_root = Some ledger_root; irmin_commit = None;
    txid_hi = Int64.of_int (List.length items - 1);
    txlog_seg = Some segment; txlog_off = Some offset;
    epochlog_off = Some (SC.epochlog_offset store); commit_id = "tx-loc-repair";
    ts = 0.; quorum_cert_hash = None; epoch_index_hash = Some epoch_hash;
    epoch_index_root = Some root }

let rewrite store segment offset bytes =
  let path = Txlog.seg_path (SC.txlog store).dir segment in
  let fd = Unix.openfile path [Unix.O_RDWR] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    expect "short test write" (Unix.write fd bytes 0 (Bytes.length bytes) = Bytes.length bytes);
    Unix.fsync fd)

let metadata = ["index_schema_version", "checked_schema";
  "repaired_upto_epoch", "0"; "history_note", "preserved";
  "eic_epoch_root:0", hash 42]

let seed_metadata store =
  List.iter (fun (key, value) -> Index.set_meta_direct (SC.index store) key value) metadata

let check_metadata store =
  List.iter (fun (key, value) ->
    expect ("metadata changed: " ^ key)
      (Index.get_meta (SC.index store) key = Some value)) metadata

let refuse store =
  match SC.verify_and_repair_tx_loc_only store ~max_epoch:1 with
  | stats ->
    expect "corrupt journal accepted" (stats.errors <> []);
    expect "failed repair reported writes" (stats.repaired = 0)
  | exception Failure _ -> ()

let expect_no_write store =
  expect "failed repair committed prefix"
    (Index.get_tx_loc_raw (SC.index store) (hash 1) = None);
  check_metadata store

let test_success () =
  with_store (fun store ->
    let first = append store 1 in
    let second = append store 2 in
    seal store;
    seed_metadata store;
    let stats = SC.verify_and_repair_tx_loc_only store ~max_epoch:1 in
    expect "missing locations not repaired" (stats.errors = [] && stats.repaired = 2);
    List.iter (fun (value, (segment, offset, length)) ->
      expect "wrong repaired location"
        (Index.get_tx_loc_raw (SC.index store) (hash value)
          = Some (segment, offset, length, 1))) [1, first; 2, second];
    let again = SC.verify_and_repair_tx_loc_only store ~max_epoch:1 in
    expect "repair not idempotent" (again.errors = [] && again.repaired = 0);
    check_metadata store)

let test_damage damage =
  with_store (fun store ->
    ignore (append store 1);
    let location = append store 2 in
    seal store;
    seed_metadata store;
    damage store location;
    refuse store;
    expect_no_write store)

let test_checksum () =
  test_damage (fun store (segment, offset, length) ->
    let bytes = Txlog.read_at (SC.txlog store) ~seg_id:segment
      ~offset:(offset + length) 1 in
    Bytes.set_uint8 bytes 0 (Bytes.get_uint8 bytes 0 lxor 1);
    rewrite store segment (offset + length) bytes)

let test_short_prefix () =
  test_damage (fun store (_, offset, _) ->
    Unix.ftruncate (SC.txlog store).current_fd (offset + 2))

let test_short_record () =
  test_damage (fun store (_, offset, length) ->
    Unix.ftruncate (SC.txlog store).current_fd (offset + length))

let test_segment_header () =
  test_damage (fun store (segment, _, _) ->
    let bytes = Bytes.create 4 in
    Txlog.write_u32_le bytes 0 19;
    rewrite store segment 6 bytes)

let test_missing_segment () =
  test_damage (fun store _ ->
    Txlog.rotate (SC.txlog store);
    Txlog.rotate (SC.txlog store);
    Unix.unlink (Txlog.seg_path (SC.txlog store).dir 1))

let test_missing_first () =
  test_damage (fun store _ ->
    Txlog.rotate (SC.txlog store);
    Unix.unlink (Txlog.seg_path (SC.txlog store).dir 0))

let test_location_mismatch () =
  test_damage (fun store (segment, offset, length) ->
    Index.set_tx_loc_only (SC.index store) (hash 2) ~seg_id:segment
      ~offset:(offset + 1) ~len:length ~epoch_id:1)

let test_location_bytes () =
  test_damage (fun store _ ->
    let index = SC.index store in
    ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      Lmdb.Map.set index.tx_loc ~txn (hash 2) "broken")))

let test_duplicate () =
  with_store (fun store ->
    ignore (append store 1);
    ignore (append store 1);
    seal store;
    seed_metadata store;
    refuse store;
    expect_no_write store)

let test_payload () =
  with_store (fun store ->
    ignore (append store 1);
    ignore (Txlog.append (SC.txlog store) ~epoch_id:1 ~payload:(hash 2 ^ "broken"));
    seal store;
    seed_metadata store;
    refuse store;
    expect_no_write store)

let test_large_abort () =
  with_store (fun store ->
    for value = 1 to 50_001 do ignore (append store value) done;
    let segment, offset, _ = append store 50_002 in
    seal store;
    seed_metadata store;
    let bytes = Bytes.create 4 in
    Txlog.write_u32_le bytes 0 (Txlog.max_record_len + 1);
    rewrite store segment offset bytes;
    refuse store;
    expect_no_write store)

let () =
  let cases = ["success", test_success; "checksum", test_checksum;
    "short_prefix", test_short_prefix; "short_record", test_short_record;
    "segment_header", test_segment_header; "missing_segment", test_missing_segment;
    "missing_first", test_missing_first; "location_mismatch", test_location_mismatch;
    "location_bytes", test_location_bytes; "duplicate", test_duplicate;
    "payload", test_payload; "large_abort", test_large_abort] in
  let failures = List.filter_map (fun (name, action) ->
    match action () with
    | () -> Printf.printf "event = test name = %s status = passed\n%!" name; None
    | exception exn ->
      Printf.eprintf "event = test name = %s status = failed reason = %s\n%!"
        name (Printexc.to_string exn);
      Some name) cases in
  if failures <> [] then exit 1