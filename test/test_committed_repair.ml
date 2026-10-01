(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module SI = Octra_core.Store_irmin
module HM = Octra_core.Head_manifest
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment
module Index = Octra_core.Chaindata_index
module Txlog = Octra_core.Txlog

let expect label ok = if not ok then failwith label
let hash c = String.make 64 c

let with_store action =
  Test_workspace.with_dir "committed_repair" (fun dir ->
    HM.cached := None;
    let chain = SC.open_chaindata (Filename.concat dir "chaindata") in
    let store = Lwt_main.run (SI.open_store (Filename.concat dir "irmin_store")) in
    Fun.protect ~finally:(fun () ->
      SC.close chain;
      Lwt_main.run (SI.close store);
      HM.cached := None) (fun () -> action dir chain store))

let commit dir chain store orphan =
  Lwt_main.run (SI.begin_epoch_batch store);
  Lwt_main.run (SI.set_meta store "last_epoch" "0");
  Lwt_main.run (SI.set_meta store "current_epoch" "1");
  Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
  let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
  let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
  let orphan_loc = Txlog.append (SC.txlog chain) ~epoch_id:0 ~payload:(orphan ^ "{}") in
  SC.begin_batch chain;
  let txid = SC.next_txid chain in
  SC.save_tx chain ~hash:(hash 'a') ~epoch_id:0 ~from_addr:"octA" ~to_addr:"octB"
    ~tx_json:{|{"from":"octA","to_":"octB"}|}
    ~op_type:"standard" ~encrypted_data:"" ~message:"";
  let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
    [Eic.item ~txid ~hash:(hash 'a')] in
  let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:root in
  SC.set_epoch chain { EL.empty_epoch_header with id = 0; start_txid = txid;
    tx_count = 1; state_root };
  SC.set_epoch_index_commitment chain ~epoch_id:0 ~epoch_hash ~root;
  SC.commit_batch chain;
  SC.fsync chain;
  let segment, offset = SC.txlog_position chain in
  let head = HM.{ schema_version; generation = 0; epoch_id = 0; state_root;
    ledger_state_root = Some ledger_root; irmin_commit; txid_hi = txid;
    txlog_seg = Some segment; txlog_off = Some offset;
    epochlog_off = Some (SC.epochlog_offset chain); commit_id = "committed-repair";
    ts = 0.; quorum_cert_hash = None; epoch_index_hash = Some epoch_hash;
    epoch_index_root = Some root } in
  HM.atomic_write dir head;
  HM.set_cached head;
  txid, orphan_loc

let remove_hash chain value =
  let index = SC.index chain in
  ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
    Lmdb.Map.remove index.tx_loc ~txn value))

let test_duplicate_restart () =
  with_store (fun dir chain store ->
    ignore (commit dir chain store (hash 'a'));
    let expected = Index.get_tx_loc_raw (SC.index chain) (hash 'a') in
    let result = Lwt_main.run (Octra_core.Startup_recovery.recover
      ~data_dir:dir ~chaindata:chain ~store) in
    expect "duplicate interrupted attempt refused" result.eic_ok;
    expect "committed location changed" (Index.get_tx_loc_raw (SC.index chain) (hash 'a') = expected))

let test_orphan_not_indexed () =
  with_store (fun dir chain store ->
    ignore (commit dir chain store (hash 'b'));
    remove_hash chain (hash 'a');
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:0 in
    expect "committed repair refused" (stats.errors = []);
    expect "orphan indexed as confirmed" (SC.get_tx_by_hash chain (hash 'b') = None);
    expect "committed transaction missing" (SC.get_tx_by_hash chain (hash 'a') <> None);
    expect "wrong repair count" (stats.repaired = 1))

let test_wrong_txid () =
  with_store (fun dir chain store ->
    let txid, (segment, offset, length) = commit dir chain store (hash 'b') in
    let index = SC.index chain in
    remove_hash chain (hash 'a');
    ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      Lmdb.Map.set index.txid_loc ~txn txid
        (Index.encode_txid_loc ~seg_id:segment ~offset ~len:length)));
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:0 in
    expect "incorrect txid reference accepted" (stats.errors <> []);
    expect "failed repair changed tx_loc" (Index.get_tx_loc_raw index (hash 'a') = None);
    expect "failed repair exposed orphan" (SC.get_tx_by_hash chain (hash 'b') = None))

let test_orphan_suffix () =
  with_store (fun dir chain store ->
    ignore (commit dir chain store (hash 'a'));
    ignore (Txlog.append (SC.txlog chain) ~epoch_id:0 ~payload:(hash 'b' ^ "{}"));
    let position = SC.txlog_position chain in
    remove_hash chain (hash 'a');
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:0 in
    expect "suffix changed committed selection" (stats.errors = [] && stats.repaired = 1);
    expect "uncommitted suffix became visible" (SC.get_tx_by_hash chain (hash 'b') = None);
    expect "inspection truncated journal" (SC.txlog_position chain = position))

let test_refuse change =
  with_store (fun dir chain store ->
    let txid, location = commit dir chain store (hash 'b') in
    remove_hash chain (hash 'a');
    change chain txid location;
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:0 in
    expect "unproven history accepted" (stats.errors <> [] && stats.repaired = 0);
    expect "refusal committed writes" (Index.get_tx_loc_raw (SC.index chain) (hash 'a') = None))

let set_txid chain txid bytes =
  let index = SC.index chain in
  ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
    match bytes with
    | None -> Lmdb.Map.remove index.txid_loc ~txn txid
    | Some bytes -> Lmdb.Map.set index.txid_loc ~txn txid bytes))

let change_head change =
  HM.set_cached (change (Option.get (HM.get_cached ())))

let test_extra_hash () =
  test_refuse (fun chain _ (segment, offset, length) ->
    Index.set_tx_loc_only (SC.index chain) (hash 'b')
      ~seg_id:segment ~offset ~len:length ~epoch_id:0)

let test_empty_epoch () =
  with_store (fun dir chain store ->
    ignore (commit dir chain store (hash 'b'));
    let head = Option.get (HM.get_cached ()) in
    let epoch_hash, root = Eic.next_root ~prev:(Option.get head.epoch_index_root) ~epoch_id:1 [] in
    let ledger_root = HM.ledger_state_root head in
    let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:root in
    SC.begin_batch chain;
    SC.set_epoch chain { EL.empty_epoch_header with id = 1; start_txid = 1L; state_root };
    SC.set_epoch_index_commitment chain ~epoch_id:1 ~epoch_hash ~root;
    SC.commit_batch chain;
    HM.set_cached { head with epoch_id = 1; state_root;
      epochlog_off = Some (SC.epochlog_offset chain);
      epoch_index_hash = Some epoch_hash; epoch_index_root = Some root };
    remove_hash chain (hash 'a');
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:1 in
    expect "empty epoch broke committed walk" (stats.errors = [] && stats.repaired = 1);
    expect "earlier orphan indexed" (SC.get_tx_by_hash chain (hash 'b') = None))

let test_range_errors () =
  let validate epochs hi = Octra_core.Committed_history.validate_ranges
    ~cap:1 ~txid_hi:hi ~epoch:(-1) ~next_txid:0L epochs in
  let first = { EL.empty_epoch_header with id = 0; start_txid = 0L; tx_count = 1 } in
  let second = { first with id = 1; start_txid = 1L } in
  expect "valid ranges refused" (validate [first; second] 1L = Ok ());
  List.iter (fun epochs ->
    expect "invalid ranges accepted" (Result.is_error (validate epochs 1L)))
    [[second]; [first; { second with start_txid = 0L }];
     [first; { second with tx_count = -1 }]; [second; first]]

let test_head_eic_recovery () =
  with_store (fun dir chain store ->
    ignore (commit dir chain store (hash 'b'));
    let head = Option.get (HM.get_cached ()) in
    HM.atomic_write dir { head with epoch_index_hash = None; epoch_index_root = None };
    remove_hash chain (hash 'a');
    let result = Lwt_main.run (Octra_core.Startup_recovery.recover ~data_dir:dir ~chaindata:chain ~store) in
    expect "folded HEAD did not prove EIC" result.eic_ok;
    expect "folded HEAD admitted orphan" (SC.get_tx_by_hash chain (hash 'b') = None))

let test_epoch_order () =
  with_store (fun dir chain store ->
    ignore (EL.append chain.SC.epochlog { EL.empty_epoch_header with id = 1; start_txid = 1L });
    ignore (commit dir chain store (hash 'b'));
    remove_hash chain (hash 'a');
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:0 in
    expect "epoch filtering hid invalid order" (stats.errors <> []);
    expect "epoch order refusal wrote index" (Index.get_tx_loc_raw (SC.index chain) (hash 'a') = None))

let history dir chain store first_eic count =
  let root = ref Eic.genesis_root in
  for epoch = 0 to count - 1 do
    Lwt_main.run (SI.begin_epoch_batch store);
    Lwt_main.run (SI.set_meta store "last_epoch" (string_of_int epoch));
    Lwt_main.run (SI.set_meta store "current_epoch" (string_of_int (epoch + 1)));
    Lwt_main.run (SI.commit_epoch_batch store "test history epoch");
    let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
    let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
    let tx_hash = hash (Char.chr (Char.code 'a' + epoch)) in
    SC.begin_batch chain;
    let txid = SC.next_txid chain in
    SC.save_tx chain ~hash:tx_hash ~epoch_id:epoch ~from_addr:"octA" ~to_addr:"octB"
      ~tx_json:{|{"from":"octA","to_":"octB"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, next_root = Eic.next_root ~prev:!root ~epoch_id:epoch
      [Eic.item ~txid ~hash:tx_hash] in
    let linked = epoch >= first_eic in
    let state_root = if linked then Eic.folded_state_root
      ~ledger_state_root:ledger_root ~epoch_index_root:next_root else ledger_root in
    SC.set_epoch chain { EL.empty_epoch_header with id = epoch; start_txid = txid;
      tx_count = 1; state_root };
    if linked then begin
      SC.set_epoch_index_commitment chain ~epoch_id:epoch ~epoch_hash ~root:next_root;
      root := next_root
    end;
    SC.commit_batch chain;
    SC.fsync chain;
    let segment, offset = SC.txlog_position chain in
    let head = HM.{ schema_version; generation = epoch; epoch_id = epoch; state_root;
      ledger_state_root = (if linked then Some ledger_root else None);
      irmin_commit; txid_hi = txid; txlog_seg = Some segment; txlog_off = Some offset;
      epochlog_off = Some (SC.epochlog_offset chain); commit_id = "history-test";
      ts = 0.; quorum_cert_hash = None;
      epoch_index_hash = (if linked then Some epoch_hash else None);
      epoch_index_root = (if linked then Some next_root else None) } in
    HM.atomic_write dir head;
    HM.set_cached head
  done

let test_legacy_history first_eic =
  with_store (fun dir chain store ->
    history dir chain store first_eic 5;
    let before = HM.load dir in
    remove_hash chain (hash 'a');
    let result = Lwt_main.run (Octra_core.Startup_recovery.recover
      ~data_dir:dir ~chaindata:chain ~store) in
    expect "legacy history blocked recovery" result.eic_ok;
    expect "legacy location not repaired" (result.index_repaired = 1);
    expect "legacy recovery changed HEAD" (HM.load dir = before);
    let again = Lwt_main.run (Octra_core.Startup_recovery.recover
      ~data_dir:dir ~chaindata:chain ~store) in
    expect "legacy restart refused" again.eic_ok)

let remove_meta chain key =
  let index = SC.index chain in
  ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn -> Lmdb.Map.remove index.meta ~txn key))

let test_eic_damage change =
  with_store (fun dir chain store ->
    history dir chain store 3 6;
    remove_hash chain (hash 'a');
    change chain;
    let stats = SC.verify_and_repair_tx_loc_only chain ~max_epoch:5 in
    expect "broken EIC suffix accepted" (stats.errors <> [] && stats.repaired = 0);
    expect "failed EIC repair wrote legacy prefix" (Index.get_tx_loc_raw (SC.index chain) (hash 'a') = None))

let () =
  let cases = ["duplicate_restart", test_duplicate_restart;
    "orphan_not_indexed", test_orphan_not_indexed; "wrong_txid", test_wrong_txid;
    "orphan_suffix", test_orphan_suffix; "extra_hash", test_extra_hash;
    "empty_epoch", test_empty_epoch; "range_errors", test_range_errors;
    "head_eic_recovery", test_head_eic_recovery;
    "epoch_order", test_epoch_order;
    "legacy_prefix", (fun () -> test_legacy_history 3);
    "legacy_only", (fun () -> test_legacy_history 5);
    "eic_gap", (fun () -> test_eic_damage (fun chain ->
      remove_meta chain "eic_epoch_hash:4"; remove_meta chain "eic_epoch_root:4"));
    "eic_partial", (fun () -> test_eic_damage (fun chain -> remove_meta chain "eic_epoch_root:3"));
    "eic_wrong_start", (fun () -> test_eic_damage (fun chain ->
      Index.set_meta_direct (SC.index chain) "eic_epoch_root:3" (hash 'f')));
    "eic_reset", (fun () -> test_eic_damage (fun chain ->
      let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:4
        [Eic.item ~txid:4L ~hash:(hash 'e')] in
      SC.set_epoch_index_commitment_direct chain ~epoch_id:4 ~epoch_hash ~root));
    "eic_first_missing", (fun () -> test_eic_damage (fun chain ->
      remove_meta chain "eic_epoch_hash:3"; remove_meta chain "eic_epoch_root:3"));
    "eic_all_missing", (fun () -> test_eic_damage (fun chain ->
      for epoch = 3 to 5 do
        remove_meta chain (Printf.sprintf "eic_epoch_hash:%d" epoch);
        remove_meta chain (Printf.sprintf "eic_epoch_root:%d" epoch)
      done));
    "missing_txid", (fun () -> test_refuse (fun chain txid _ -> set_txid chain txid None));
    "malformed_txid", (fun () -> test_refuse (fun chain txid _ -> set_txid chain txid (Some "broken")));
    "missing_head", (fun () -> test_refuse (fun _ _ _ -> HM.cached := None));
    "wrong_head_root", (fun () -> test_refuse (fun _ _ _ ->
      change_head (fun h -> { h with epoch_index_root = Some (hash 'f') })));
    "wrong_state_root", (fun () -> test_refuse (fun _ _ _ ->
      change_head (fun h -> { h with state_root = hash 'f' })));
    "short_head_extent", (fun () -> test_refuse (fun _ _ (_, offset, _) ->
      change_head (fun h -> { h with txlog_off = Some offset })))] in
  let failed = List.filter_map (fun (name, action) ->
    try action (); Printf.printf "event = test name = %s status = passed\n%!" name; None
    with exn -> Printf.eprintf "event = test name = %s status = failed reason = %s\n%!"
      name (Printexc.to_string exn); Some name) cases in
  if failed <> [] then exit 1