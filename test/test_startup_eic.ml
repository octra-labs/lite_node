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
  Test_workspace.unique_dir "octra_startup_eic"

let cleanup dir =
  ignore (Sys.command (Printf.sprintf "rm -rf %s" (Filename.quote dir)))

let epoch_header ~epoch_id ~start_txid ~state_root =
  {
    EL.empty_epoch_header with
    id = epoch_id;
    state_root;
    prev_state_root = hash '8';
    parent_commit = hash '7';
    start_txid;
    tx_count = 1;
    finalized_by = "tester";
    finalized_at = Unix.gettimeofday ();
  }

let prepare_node data_dir =
  let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
  let store = Lwt_main.run (SI.open_store (Filename.concat data_dir "irmin_store")) in
  Lwt_main.run (SI.begin_epoch_batch store);
  Lwt_main.run (SI.set_meta store "last_epoch" "0");
  Lwt_main.run (SI.set_meta store "current_epoch" "1");
  Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
  let ledger_state_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
  let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
  SC.begin_batch chaindata;
  let epoch_id = 0 in
  let start_txid = SC.next_txid chaindata in
  let tx_hash = hash 'a' in
  SC.save_tx chaindata
    ~hash:tx_hash
    ~epoch_id
    ~from_addr:"octFROM"
    ~to_addr:"octTO"
    ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
    ~op_type:"standard"
    ~encrypted_data:""
    ~message:"";
  let epoch_hash, root = Eic.next_root
    ~prev:Eic.genesis_root
    ~epoch_id
    [Eic.item ~txid:start_txid ~hash:tx_hash]
  in
  let state_root = Eic.folded_state_root ~ledger_state_root ~epoch_index_root:root in
  SC.set_epoch chaindata (epoch_header ~epoch_id ~start_txid ~state_root);
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
    ledger_state_root = Some ledger_state_root;
    irmin_commit;
    txid_hi = start_txid;
    txlog_seg = Some txlog_seg;
    txlog_off = Some txlog_off;
    epochlog_off = Some epochlog_off;
    commit_id = "startup-eic";
    ts = Unix.gettimeofday ();
    quorum_cert_hash = None;
    epoch_index_hash = Some epoch_hash;
    epoch_index_root = Some root;
  };
  chaindata, store

let test_recovery_checks_eic () =
  let data_dir = work_dir () in
  let chaindata, store = prepare_node data_dir in
  Fun.protect
    ~finally:(fun () ->
      SC.close chaindata;
      Lwt_main.run (SI.close store);
      cleanup data_dir)
    (fun () ->
      let result = Lwt_main.run (Octra_core.Startup_recovery.recover
        ~data_dir
        ~chaindata
        ~store)
      in
      assert_msg result.boundary_ok "boundary must converge";
      assert_msg result.eic_checked "EIC must be checked";
      assert_msg result.eic_ok "EIC must pass")

let () =
  Random.init 11;
  test_recovery_checks_eic ();
  print_endline "status = pass test = startup_eic"