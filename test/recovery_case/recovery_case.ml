(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module SI = Octra_core.Store_irmin
module HM = Octra_core.Head_manifest
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment
module Wal = Octra_core.Wal
module Marker = Octra_core.Epoch_commit_marker
module Recovery = Octra_core.Startup_recovery

let expect label ok = if not ok then failwith label
let hash c = String.make 64 c

let with_stores dir action =
  let chain = SC.open_chaindata (Filename.concat dir "chaindata") in
  Fun.protect ~finally:(fun () -> SC.close chain) (fun () ->
    let store = Lwt_main.run (SI.open_store (Filename.concat dir "irmin_store")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (SI.close store))
      (fun () -> action chain store))

let prepare dir =
  with_stores dir (fun chain store ->
    Lwt_main.run (SI.set_meta store "last_epoch" "-1");
    let pre_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
    let parent = Option.get (Lwt_main.run (SI.get_commit_hash store)) in
    Lwt_main.run (SI.begin_epoch_batch store);
    Lwt_main.run (SI.set_meta store "last_epoch" "0");
    Lwt_main.run (SI.set_meta store "current_epoch" "1");
    Lwt_main.run (SI.set_meta store "total_supply" "0");
    Lwt_main.run (SI.set_account store "octFROM" Octra_core.Ledger_types.empty_account);
    Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
    let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
    let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
    SC.begin_batch chain;
    let start_txid = SC.next_txid chain in
    let tx_hash = hash 'a' in
    SC.save_tx chain ~hash:tx_hash ~epoch_id:0 ~from_addr:"octFROM" ~to_addr:"octTO"
      ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
      [Eic.item ~txid:start_txid ~hash:tx_hash] in
    let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root
      ~epoch_index_root:root in
    SC.set_epoch chain {EL.empty_epoch_header with id = 0; state_root; parent_commit = parent;
      start_txid; tx_count = 1};
    SC.set_epoch_index_commitment chain ~epoch_id:0 ~epoch_hash ~root;
    SC.commit_batch chain;
    SC.fsync chain;
    let txlog_seg, txlog_off = SC.txlog_position chain in
    let head = HM.{schema_version; generation = 0; epoch_id = 0; state_root;
      ledger_state_root = Some ledger_root; irmin_commit; txid_hi = start_txid;
      txlog_seg = Some txlog_seg; txlog_off = Some txlog_off;
      epochlog_off = Some (SC.epochlog_offset chain); commit_id = "wal-phase";
      ts = 0.; quorum_cert_hash = None; epoch_index_hash = Some epoch_hash;
      epoch_index_root = Some root} in
    HM.atomic_write dir head;
    let entry = Wal.{epoch_id = 0; pre_state_root = pre_root;
      post_state_root = ledger_root; parent_commit = parent; start_txid; tx_count = 1;
      finalized_by = "test"; finalized_at = 0.; irmin_last_epoch_before = -1;
      irmin_parent = Some parent} in
    head, entry)

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let rec files dir =
  if not (Sys.file_exists dir) then []
  else Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
    let path = Filename.concat dir name in
    if Sys.is_directory path then files path
    else if name = "lock.mdb" then []
    else [path, read path])

let evidence dir =
  let records = List.concat_map (fun name -> files (Filename.concat dir name))
    ["chaindata"; "wal"] @ List.filter_map (fun name ->
      let path = Filename.concat dir name in
      if Sys.file_exists path then Some (path, read path) else None)
      ["commit_journal.log"; "epoch_commit_in_progress.json"] in
  let head = read (Filename.concat dir "HEAD.json") in
  let tags, commit = with_stores dir (fun _ store ->
    let tags = Lwt_main.run (SI.list_epoch_tags store) in
    let records = List.map (fun epoch -> epoch, Lwt_main.run (SI.epoch_binding store epoch)) tags in
    records, Lwt_main.run (SI.get_commit_hash store)) in
  head, records, tags, commit

let rec wait pid =
  try snd (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let recover dir =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try
      with_stores dir (fun chain store ->
        ignore (Lwt_main.run (Recovery.recover ~data_dir:dir ~chaindata:chain ~store)));
      exit 0
    with exn ->
      Printf.eprintf "event = refused reason = %s\n%!" (Printexc.to_string exn);
      exit 2)
  | pid -> wait pid

let advance dir head =
  with_stores dir (fun chain store ->
    Lwt_main.run (SI.tag_epoch store 0);
    Lwt_main.run (SI.begin_epoch_batch store);
    Lwt_main.run (SI.set_meta store "last_epoch" "1");
    Lwt_main.run (SI.set_meta store "current_epoch" "2");
    Lwt_main.run (SI.commit_epoch_batch store "test epoch one");
    let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
    SC.begin_batch chain;
    let start_txid = SC.next_txid chain in
    let tx_hash = hash 'b' in
    SC.save_tx chain ~hash:tx_hash ~epoch_id:1 ~from_addr:"octFROM" ~to_addr:"octTO"
      ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, root = Eic.next_root ~prev:(Option.get head.HM.epoch_index_root)
      ~epoch_id:1 [Eic.item ~txid:start_txid ~hash:tx_hash] in
    let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root
      ~epoch_index_root:root in
    SC.set_epoch chain {EL.empty_epoch_header with id = 1; state_root;
      prev_state_root = head.state_root; parent_commit = Option.get head.irmin_commit;
      start_txid; tx_count = 1};
    SC.set_epoch_index_commitment chain ~epoch_id:1 ~epoch_hash ~root;
    SC.commit_batch chain;
    SC.fsync chain;
    Octra_core.Commit_journal.append dir (Octra_core.Commit_journal.Prepare {
      commit_id = "phase-forward"; prev_generation = head.generation; epoch_id = 1;
      planned_txid_hi = start_txid; planned_state_root = state_root; ts = 0.});
    Wal.write dir Wal.{epoch_id = 1; pre_state_root = HM.ledger_state_root head;
      post_state_root = ledger_root; parent_commit = Option.get head.irmin_commit;
      start_txid; tx_count = 1; finalized_by = "test"; finalized_at = 0.;
      irmin_last_epoch_before = 0; irmin_parent = head.irmin_commit};
    Marker.write_marker dir 1 "irmin_committed")

let change_wal dir change =
  Wal.write dir (change (List.hd (Wal.read_pending dir)))

let change_prepare dir change =
  let journal = Octra_core.Commit_journal.read_all dir in
  match journal with
  | [Octra_core.Commit_journal.Prepare entry] ->
    let channel = open_out_bin (Octra_core.Commit_journal.path dir) in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel (Yojson.Safe.to_string
        (Octra_core.Commit_journal.record_to_json
          (change (Octra_core.Commit_journal.Prepare entry))) ^ "\n"))
  | _ -> failwith "unexpected test journal"