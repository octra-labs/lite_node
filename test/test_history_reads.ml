(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module Index = Octra_core.Chaindata_index
module Txlog = Octra_core.Txlog
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment
module Head = Octra_core.Head_manifest
module RPC = Octra_node_runtime.History_read_rpc
module Startup = Octra_node_runtime.Startup_history_shell

exception Refused

let expect name condition = if not condition then failwith name
let hash text = Digestif.SHA256.(digest_string text |> to_hex)

let rec inventory dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
    let path = Filename.concat dir name in
    if Sys.is_directory path then inventory path
    else if name = "lock.mdb" then []
    else [path, Digest.to_hex (Digest.file path)])

let with_store epoch action =
  Test_workspace.with_dir "history_reads" (fun dir ->
    Head.cached := None;
    let store = SC.open_chaindata (Filename.concat dir "chaindata") in
    Fun.protect ~finally:(fun () -> SC.close store; Head.cached := None) (fun () ->
      let payload = {|{"from":"sender","to_":"receiver","amount":"1","nonce":1}|} in
      let orphan = hash "orphan" and first = hash "first" and second = hash "second" in
      ignore (Txlog.append (SC.txlog store) ~epoch_id:epoch ~payload:(orphan ^ payload));
      SC.begin_batch store;
      List.iter (fun value -> SC.save_tx store ~hash:value ~epoch_id:epoch
        ~from_addr:"sender" ~to_addr:"receiver" ~tx_json:payload
        ~op_type:"standard" ~encrypted_data:"" ~message:"") [first; second];
      let items = [Eic.item ~txid:0L ~hash:first; Eic.item ~txid:1L ~hash:second] in
      let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:epoch items in
      let ledger_root = hash "ledger" in
      let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:root in
      SC.set_epoch store {EL.empty_epoch_header with id = epoch; start_txid = 0L;
        tx_count = 2; state_root};
      SC.set_epoch_index_commitment store ~epoch_id:epoch ~epoch_hash ~root;
      SC.fsync store;
      SC.commit_batch store;
      let segment, offset = SC.txlog_position store in
      Head.set_cached Head.{schema_version; generation = 1; epoch_id = epoch; state_root;
        ledger_state_root = Some ledger_root; irmin_commit = None; txid_hi = 1L;
        txlog_seg = Some segment; txlog_off = Some offset;
        epochlog_off = Some (SC.epochlog_offset store); commit_id = "history-read";
        ts = 0.; quorum_cert_hash = None; epoch_index_hash = Some epoch_hash;
        epoch_index_root = Some root};
      action dir store second))

let epoch_read store epoch =
  Lwt_main.run (RPC.transactions_by_epoch store
    ~params:(`List [`Int epoch; `Int 10; `Int 0]) ~current_epoch_id:(epoch + 1))

let test_epoch_missing () =
  with_store 101 (fun dir store _ ->
    Index.remove_txid_loc_direct (SC.index store) 1L;
    let before = inventory dir in
    let result = epoch_read store 101 in
    expect "epoch RPC changed persistent data" (inventory dir = before);
    expect "epoch RPC guessed a transaction location"
      (Index.get_txid_loc_raw (SC.index store) 1L = None);
    expect "epoch RPC accepted missing index" (match result with
      | Error error -> error.Octra_core.Rpc.code = 116 | Ok _ -> false))

let test_epoch_valid () =
  with_store 102 (fun dir store _ ->
    let before = inventory dir in
    expect "valid epoch RPC refused" (Result.is_ok (epoch_read store 102));
    expect "valid epoch RPC changed persistent data" (inventory dir = before))

let test_epoch_meta rewrite epoch =
  with_store epoch (fun dir store _ ->
    let index = SC.index store in
    expect "test metadata write failed" (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      if rewrite then begin
        let original = Lmdb.Map.get index.epoch_meta ~txn (Int32.of_int epoch) in
        let fields = match Yojson.Safe.from_string original with
          | `Assoc fields -> fields | _ -> failwith "epoch test data is invalid" in
        Lmdb.Map.set index.epoch_meta ~txn (Int32.of_int epoch)
          (Yojson.Safe.to_string (`Assoc (("tx_count", `Int 0) :: List.remove_assoc "tx_count" fields)))
      end else Lmdb.Map.remove index.epoch_meta ~txn (Int32.of_int epoch)) = Some ());
    let before = inventory dir in
    let result = epoch_read store epoch in
    expect "metadata RPC changed persistent data" (inventory dir = before);
    expect "metadata RPC falsely reported an empty epoch" (match result with
      | Error error -> error.Octra_core.Rpc.code = 116 | Ok _ -> false))

let test_hash_missing () =
  with_store 103 (fun dir store second ->
    let index = SC.index store in
    expect "test deletion failed" (Lmdb.Txn.go Lmdb.Rw index.env (fun txn ->
      Lmdb.Map.remove index.tx_loc ~txn second) = Some ());
    let before = inventory dir in
    let result = Lwt_main.run (RPC.transaction ~find_drop:(fun _ -> None) store
      ~params:(`List [`String second])) in
    expect "hash RPC changed persistent data" (inventory dir = before);
    expect "hash RPC wrote a secondary index" (Index.get_tx_loc_raw index second = None);
    expect "hash RPC lost a readable committed transaction" (Result.is_ok result))

let startup store ~marker =
  Startup.run_startup_checks {
    int_value = (fun _ _ -> 1);
    first_epoch = (fun () -> 0);
    last_epoch = (fun () -> Option.map (fun h -> h.EL.id) (SC.get_last_epoch store));
    status_at = SC.get_epoch_index_status store;
    marker_path = "marker";
    marker_exists = (fun _ -> marker);
    irmin_stealth_counter = (fun () -> 0L);
    chaindata_next_txid = (fun () -> SC.next_txid store);
    exit_fatal = (fun () -> raise Refused);
  }

let test_startup_missing marker epoch =
  with_store epoch (fun dir store _ ->
    Index.remove_txid_loc_direct (SC.index store) 1L;
    let before = inventory dir in
    let refused = try startup store ~marker; false with Refused -> true in
    expect "startup changed incomplete data" (inventory dir = before);
    expect "startup accepted missing index" refused)

let test_startup_valid () =
  with_store 106 (fun dir store _ ->
    let before = inventory dir in
    startup store ~marker:false;
    expect "startup changed valid data" (inventory dir = before))

let () =
  let failures = List.filter_map (fun (name, run) ->
    try run (); Printf.printf "event = history_read name = %s status = passed\n%!" name; None
    with error -> Printf.printf "event = history_read name = %s status = failed reason = %s\n%!"
      name (Printexc.to_string error); Some name)
    ["epoch_missing", test_epoch_missing; "epoch_valid", test_epoch_valid;
     "hash_missing", test_hash_missing;
     "startup_missing", (fun () -> test_startup_missing false 104);
     "startup_marker", (fun () -> test_startup_missing true 105);
     "startup_valid", test_startup_valid;
     "metadata_missing", (fun () -> test_epoch_meta false 107);
     "metadata_changed", (fun () -> test_epoch_meta true 108)] in
  if failures <> [] then exit 1