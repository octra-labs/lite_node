(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module SI = Octra_core.Store_irmin
module HM = Octra_core.Head_manifest
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment

let assert_msg ok msg =
  if not ok then failwith msg

let hash c =
  String.make 64 c

let work_dir () =
  Test_workspace.unique_dir "octra_crash_eic"

let cleanup dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))

let epoch_header ~epoch_id ~state_root ~prev_state_root ~parent_commit ~start_txid ~tx_count =
  {
    EL.empty_epoch_header with
    id = epoch_id;
    state_root;
    prev_state_root;
    parent_commit;
    start_txid;
    tx_count;
    finalized_by = "tester";
    finalized_at = Unix.gettimeofday ();
  }

let save_tx store ~epoch_id ~tx_hash ~from_addr ~to_addr =
  SC.save_tx store
    ~hash:tx_hash
    ~epoch_id
    ~from_addr
    ~to_addr
    ~tx_json:(Printf.sprintf {|{"from":"%s","to_":"%s"}|} from_addr to_addr)
    ~op_type:"standard"
    ~encrypted_data:""
    ~message:""

let commit_epoch0 data_dir chaindata store =
  let epoch_id = 0 in
  Lwt_main.run (SI.begin_epoch_batch store);
  Lwt_main.run (SI.set_meta store "last_epoch" "0");
  Lwt_main.run (SI.set_meta store "current_epoch" "1");
  Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
  let state_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
  let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
  SC.begin_batch chaindata;
  let start_txid = SC.next_txid chaindata in
  let tx_hash = hash 'a' in
  save_tx chaindata ~epoch_id ~tx_hash ~from_addr:"octA" ~to_addr:"octB";
  let epoch_hash, root = Eic.next_root
    ~prev:Eic.genesis_root
    ~epoch_id
    [Eic.item ~txid:start_txid ~hash:tx_hash]
  in
  SC.set_epoch chaindata (epoch_header
    ~epoch_id
    ~state_root
    ~prev_state_root:(hash '8')
    ~parent_commit:(hash '7')
    ~start_txid
    ~tx_count:1);
  SC.set_epoch_index_commitment chaindata ~epoch_id ~epoch_hash ~root;
  SC.commit_batch chaindata;
  SC.fsync chaindata;
  let txlog_seg, txlog_off = SC.txlog_position chaindata in
  let epochlog_off = SC.epochlog_offset chaindata in
  HM.atomic_write data_dir {
    schema_version = HM.schema_version;
    generation = epoch_id;
    epoch_id;
    state_root;
    ledger_state_root = None;
    irmin_commit;
    txid_hi = start_txid;
    txlog_seg = Some txlog_seg;
    txlog_off = Some txlog_off;
    epochlog_off = Some epochlog_off;
    commit_id = "epoch0";
    ts = Unix.gettimeofday ();
    quorum_cert_hash = None;
    epoch_index_hash = Some epoch_hash;
    epoch_index_root = Some root;
  };
  root, state_root

let leave_chaindata_residue data_dir chaindata ~prev_eic_root ~pre_state_root =
  let epoch_id = 1 in
  let parent_commit = Option.get (Option.get (HM.load data_dir)).HM.irmin_commit in
  SC.begin_batch chaindata;
  let start_txid = SC.next_txid chaindata in
  let tx_hash = hash 'b' in
  save_tx chaindata ~epoch_id ~tx_hash ~from_addr:"octC" ~to_addr:"octD";
  let epoch_hash, root = Eic.next_root
    ~prev:prev_eic_root
    ~epoch_id
    [Eic.item ~txid:start_txid ~hash:tx_hash]
  in
  SC.set_epoch chaindata (epoch_header
    ~epoch_id
    ~state_root:(hash '6')
    ~prev_state_root:pre_state_root
    ~parent_commit
    ~start_txid
    ~tx_count:1);
  SC.set_epoch_index_commitment chaindata ~epoch_id ~epoch_hash ~root;
  SC.commit_batch chaindata;
  SC.fsync chaindata;
  Octra_core.Wal.write data_dir {
    epoch_id;
    pre_state_root;
    post_state_root = hash '6';
    parent_commit;
    start_txid;
    tx_count = 1;
    finalized_by = "tester";
    finalized_at = Unix.gettimeofday ();
    irmin_last_epoch_before = 0;
  };
  start_txid

let leave_irmin_committed_head_lag data_dir chaindata store ~prev_eic_root =
  let epoch_id = 1 in
  let head = Option.get (HM.load data_dir) in
  let parent_commit = Option.get head.HM.irmin_commit in
  SC.begin_batch chaindata;
  let start_txid = SC.next_txid chaindata in
  let tx_hash = hash 'c' in
  save_tx chaindata ~epoch_id ~tx_hash ~from_addr:"octE" ~to_addr:"octF";
  let epoch_hash, root = Eic.next_root
    ~prev:prev_eic_root
    ~epoch_id
    [Eic.item ~txid:start_txid ~hash:tx_hash]
  in
  Lwt_main.run (SI.begin_epoch_batch store);
  Lwt_main.run (SI.set_meta store "last_epoch" "1");
  Lwt_main.run (SI.set_meta store "current_epoch" "2");
  Lwt_main.run (SI.commit_epoch_batch store "test epoch one");
  let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
  let folded = Eic.folded_state_root
    ~ledger_state_root:ledger_root
    ~epoch_index_root:root
  in
  SC.set_epoch chaindata (epoch_header
    ~epoch_id
    ~state_root:folded
    ~prev_state_root:head.state_root
    ~parent_commit
    ~start_txid
    ~tx_count:1);
  SC.set_epoch_index_commitment chaindata ~epoch_id ~epoch_hash ~root;
  SC.commit_batch chaindata;
  SC.fsync chaindata;
  Octra_core.Commit_journal.append data_dir (Octra_core.Commit_journal.Prepare {
    commit_id = "crash-eic"; prev_generation = head.generation; epoch_id;
    planned_txid_hi = start_txid; planned_state_root = folded; ts = 0.});
  Octra_core.Wal.write data_dir {
    epoch_id;
    pre_state_root = HM.ledger_state_root head;
    post_state_root = ledger_root;
    parent_commit;
    start_txid;
    tx_count = 1;
    finalized_by = "tester";
    finalized_at = Unix.gettimeofday ();
    irmin_last_epoch_before = 0;
  };
  Octra_core.Epoch_commit_marker.write_marker data_dir epoch_id "irmin_committed";
  epoch_hash, root, ledger_root, folded

let test_cut_eic () =
  let data_dir = work_dir () in
  let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
  let store = Lwt_main.run (SI.open_store (Filename.concat data_dir "irmin_store")) in
  Fun.protect
    ~finally:(fun () ->
      SC.close chaindata;
      Lwt_main.run (SI.close store);
      cleanup data_dir)
    (fun () ->
      let eic0, root0 = commit_epoch0 data_dir chaindata store in
      let residue_txid = leave_chaindata_residue data_dir chaindata
        ~prev_eic_root:eic0
        ~pre_state_root:root0
      in
      assert_msg (SC.get_last_epoch chaindata |> Option.map (fun h -> h.EL.id) = Some 1)
        "residue epoch must exist before recovery";
      assert_msg (SC.get_tx_by_txid chaindata residue_txid <> None)
        "residue txid must exist before recovery";
      let result = Lwt_main.run (Octra_core.Startup_recovery.recover
        ~data_dir
        ~chaindata
        ~store)
      in
      assert_msg result.boundary_ok "boundary must converge";
      assert_msg result.eic_checked "EIC must be checked";
      assert_msg result.eic_ok "EIC must pass";
      assert_msg (SC.get_last_epoch chaindata |> Option.map (fun h -> h.EL.id) = Some 0)
        "residue epoch must be rolled back";
      assert_msg (SC.get_tx_by_txid chaindata residue_txid = None)
        "residue txid must be removed";
      assert_msg (Octra_core.Wal.read_pending data_dir = [])
        "WAL must be cleared")

let test_publish_eic () =
  let data_dir = work_dir () in
  let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
  let store = Lwt_main.run (SI.open_store (Filename.concat data_dir "irmin_store")) in
  Fun.protect
    ~finally:(fun () ->
      SC.close chaindata;
      Lwt_main.run (SI.close store);
      cleanup data_dir)
    (fun () ->
      let eic0, _root0 = commit_epoch0 data_dir chaindata store in
      let epoch_hash, root, ledger_root, folded =
        leave_irmin_committed_head_lag data_dir chaindata store ~prev_eic_root:eic0
      in
      let result = Lwt_main.run (Octra_core.Startup_recovery.recover
        ~data_dir
        ~chaindata
        ~store)
      in
      assert_msg result.boundary_ok "boundary must converge after HEAD rebuild";
      assert_msg result.eic_checked "rebuilt HEAD must be EIC checked";
      assert_msg result.eic_ok "rebuilt HEAD EIC must pass";
      match HM.load_result data_dir with
      | HM.Present h ->
          assert_msg (h.HM.epoch_id = 1) "HEAD must advance to Irmin epoch";
          assert_msg (h.HM.ledger_state_root = Some ledger_root) "HEAD must keep ledger root";
          assert_msg (h.HM.state_root = folded) "HEAD must keep folded root";
          assert_msg (h.HM.epoch_index_hash = Some epoch_hash) "HEAD must keep epoch index hash";
          assert_msg (h.HM.epoch_index_root = Some root) "HEAD must keep epoch index root"
      | _ -> failwith "HEAD must load after rebuild")

let test_missing_eic () =
  let data_dir = work_dir () in
  let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
  let store = Lwt_main.run (SI.open_store (Filename.concat data_dir "irmin_store")) in
  Fun.protect
    ~finally:(fun () ->
      SC.close chaindata;
      Lwt_main.run (SI.close store);
      cleanup data_dir)
    (fun () ->
      let eic0, _root0 = commit_epoch0 data_dir chaindata store in
      let epoch0_hash =
        match SC.get_epoch_index_commitment chaindata 0 with
        | Some h, _ -> h
        | _ -> failwith "epoch0 eic hash missing"
      in
      SC.set_epoch_index_commitment_direct chaindata
        ~epoch_id:0
        ~epoch_hash:epoch0_hash
        ~root:eic0;
      let epoch_hash, root, ledger_root, folded =
        leave_irmin_committed_head_lag data_dir chaindata store ~prev_eic_root:eic0
      in
      Octra_core.Epoch_commit_marker.clear_marker data_dir;
      let txlog_seg, txlog_off = SC.txlog_position chaindata in
      let epochlog_off = SC.epochlog_offset chaindata in
      HM.atomic_write data_dir {
        schema_version = HM.schema_version;
        generation = 1;
        epoch_id = 1;
        state_root = folded;
        ledger_state_root = Some ledger_root;
        irmin_commit = None;
        txid_hi = 1L;
        txlog_seg = Some txlog_seg;
        txlog_off = Some txlog_off;
        epochlog_off = Some epochlog_off;
        commit_id = "bad-head-no-eic";
        ts = Unix.gettimeofday ();
        quorum_cert_hash = None;
        epoch_index_hash = None;
        epoch_index_root = None;
      };
      let result = Lwt_main.run (Octra_core.Startup_recovery.recover
        ~data_dir
        ~chaindata
        ~store)
      in
      assert_msg result.eic_checked "in-place repair must lead to EIC check";
      assert_msg result.eic_ok "in-place repaired EIC must pass";
      match HM.load_result data_dir with
      | HM.Present h ->
          assert_msg (h.HM.state_root = folded) "in-place repair must keep folded root";
          assert_msg (h.HM.epoch_index_hash = Some epoch_hash) "in-place repair must set epoch hash";
          assert_msg (h.HM.epoch_index_root = Some root) "in-place repair must set epoch root"
      | _ -> failwith "HEAD must load after in-place repair")

let test_early_marker () =
  List.iter (fun phase ->
    let data_dir = work_dir () in
    let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
    let store = Lwt_main.run (SI.open_store (Filename.concat data_dir "irmin_store")) in
    Fun.protect
      ~finally:(fun () ->
        SC.close chaindata;
        Lwt_main.run (SI.close store);
        cleanup data_dir)
      (fun () ->
        ignore (commit_epoch0 data_dir chaindata store);
        Octra_core.Epoch_commit_marker.write_marker data_dir 1 phase;
        let result = Lwt_main.run (Octra_core.Startup_recovery.recover
          ~data_dir
          ~chaindata
          ~store)
        in
        assert_msg result.boundary_ok ("boundary must stay clean after " ^ phase);
        assert_msg result.eic_checked ("EIC must be checked after " ^ phase);
        assert_msg result.eic_ok ("EIC must pass after " ^ phase);
        assert_msg (Octra_core.Epoch_commit_marker.read_marker data_dir = None)
          ("marker must be cleared after " ^ phase);
        assert_msg (SC.get_last_epoch chaindata |> Option.map (fun h -> h.EL.id) = Some 0)
          ("chaindata head must stay at epoch0 after " ^ phase)))
    ["stage_batch_begin"; "wal_written"]

let () =
  Random.init 13;
  test_cut_eic ();
  Gc.full_major ();
  test_publish_eic ();
  Gc.full_major ();
  test_missing_eic ();
  Gc.full_major ();
  test_early_marker ();
  Gc.full_major ();
  print_endline "status = pass test = crash_atomic_eic"