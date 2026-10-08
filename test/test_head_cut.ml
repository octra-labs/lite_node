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
    Lwt_main.run (SI.begin_epoch_batch store);
    Lwt_main.run (SI.set_meta store "last_epoch" "0");
    Lwt_main.run (SI.set_meta store "current_epoch" "1");
    Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
    let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
    let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
    SC.begin_batch chain;
    let start_txid = SC.next_txid chain in
    let tx_hash = hash 'a' in
    SC.save_tx chain ~hash:tx_hash ~epoch_id:0
      ~from_addr:"octFROM" ~to_addr:"octTO"
      ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
      [Eic.item ~txid:start_txid ~hash:tx_hash] in
    let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root
      ~epoch_index_root:root in
    SC.set_epoch chain {EL.empty_epoch_header with id = 0; state_root;
      start_txid; tx_count = 1};
    SC.set_epoch_index_commitment chain ~epoch_id:0 ~epoch_hash ~root;
    SC.commit_batch chain;
    SC.fsync chain;
    let txlog_seg, txlog_off = SC.txlog_position chain in
    let head = HM.{schema_version; generation = 0; epoch_id = 0; state_root;
      ledger_state_root = Some ledger_root; irmin_commit;
      txid_hi = start_txid; txlog_seg = Some txlog_seg;
      txlog_off = Some txlog_off; epochlog_off = Some (SC.epochlog_offset chain);
      commit_id = "head-cut"; ts = 0.; quorum_cert_hash = None;
      epoch_index_hash = Some epoch_hash; epoch_index_root = Some root} in
    HM.atomic_write dir head;
    SC.begin_batch chain;
    let suffix_start = SC.next_txid chain in
    let suffix_hash = hash 'b' in
    SC.save_tx chain ~hash:suffix_hash ~epoch_id:1
      ~from_addr:"octFROM" ~to_addr:"octTO"
      ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, root = Eic.next_root ~prev:root ~epoch_id:1
      [Eic.item ~txid:suffix_start ~hash:suffix_hash] in
    SC.set_epoch chain {EL.empty_epoch_header with id = 1; state_root = hash '8';
      start_txid = suffix_start; tx_count = 1};
    SC.set_epoch_index_commitment chain ~epoch_id:1 ~epoch_hash ~root;
    SC.commit_batch chain;
    SC.fsync chain;
    let entry = Wal.{epoch_id = 1; pre_state_root = ledger_root;
      post_state_root = hash '8'; parent_commit = Option.get irmin_commit; start_txid = suffix_start;
      tx_count = 1; finalized_by = "test"; finalized_at = 0.;
      irmin_last_epoch_before = 0; irmin_parent = None} in
    expect "unexpected recovery action"
      (Wal.decide_action ~entry ~chaindata_last_epoch:1 ~irmin_last_epoch:0
        = Wal.ForwardReplayIrmin);
    Wal.write dir entry;
    Marker.write_marker dir 1 "wal_written";
    head)

let read path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

let rec files dir =
  Sys.readdir dir |> Array.to_list |> List.sort String.compare
  |> List.concat_map (fun name ->
    let path = Filename.concat dir name in
    if Sys.is_directory path then files path
    else if name = "lock.mdb" then []
    else [path, read path])

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

let test root name change alter succeeds =
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  let head = prepare dir in
  if succeeds then HM.atomic_write dir (change head)
  else begin
    let channel = open_out_bin (HM.path dir) in
    Fun.protect ~finally:(fun () -> close_out_noerr channel) (fun () ->
      output_string channel (HM.to_json (change head));
      flush channel;
      Unix.fsync (Unix.descr_of_out_channel channel))
  end;
  alter dir;
  let paths = ["chaindata"; "wal"] in
  let before = List.concat_map (fun path -> files (Filename.concat dir path)) paths in
  let outcome = recover dir in
  let after = List.concat_map (fun path -> files (Filename.concat dir path)) paths in
  let changed = List.filter_map (fun (path, bytes) ->
    if List.assoc_opt path after = Some bytes then None else Some path) before in
  Printf.printf "event = result case = %s outcome = %s changed = %d\n%!"
    name (match outcome with Unix.WEXITED n -> string_of_int n | _ -> "signal")
    (List.length changed);
  List.iter (fun path -> Printf.printf "event = changed path = %s\n%!" path) changed;
  if succeeds then begin
    expect "valid cut refused" (outcome = Unix.WEXITED 0);
    expect "completed cut retained WAL" (Wal.read_pending dir = []);
    with_stores dir (fun chain _ ->
      expect "cut next transaction differs" (SC.next_txid chain = Int64.succ head.HM.txid_hi);
      expect "cut transaction extent differs"
        (SC.txlog_position chain = (Option.get head.txlog_seg, Option.get head.txlog_off));
      expect "cut epoch extent differs" (SC.epochlog_offset chain = Option.get head.epochlog_off))
  end
  else begin
    expect "invalid cut accepted" (outcome = Unix.WEXITED 1 || outcome = Unix.WEXITED 2);
    expect "invalid cut modified evidence" (before = after)
  end

let shorten path =
  Unix.truncate path ((Unix.stat path).Unix.st_size - 1)

let corrupt path offset =
  let fd = Unix.openfile path [Unix.O_RDWR] 0 in
  Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
    ignore (Unix.lseek fd offset Unix.SEEK_SET);
    expect "short corruption write" (Unix.write_substring fd "x" 0 1 = 1);
    Unix.fsync fd)

let run root =
  let idle _ = () in
  let txpath dir = Filename.concat dir "chaindata/txlog/seg000000.dat" in
  let eppath dir = Filename.concat dir "chaindata/epochlog/epochs.dat" in
  let cases = ["valid", Fun.id, idle, true;
    "tx_inside", (fun h -> {h with HM.txlog_off = Option.map (fun n -> n - 8) h.HM.txlog_off}), idle, false;
    "epoch_inside", (fun h -> {h with HM.epochlog_off = Option.map (fun n -> n - 8) h.HM.epochlog_off}), idle, false;
    "tx_prefix", (fun h -> {h with HM.txlog_off = Some 16}), idle, false;
    "tx_missing", (fun h -> {h with HM.txlog_off = None}), idle, false;
    "state_root", (fun h -> {h with HM.state_root = hash '5'}), idle, false;
    "index_root", (fun h -> {h with HM.epoch_index_root = Some (hash '5')}), idle, false;
    "tx_corrupt", Fun.id, (fun dir -> corrupt (txpath dir) 24), false;
    "epoch_corrupt", Fun.id, (fun dir -> corrupt (eppath dir) 24), false;
    "torn_tx", Fun.id, (fun dir -> shorten (txpath dir)), true;
    "torn_epoch", Fun.id, (fun dir -> shorten (eppath dir)), true;
    "torn_both", Fun.id, (fun dir -> shorten (txpath dir); shorten (eppath dir)), true;
    "wal_start", Fun.id, (fun dir ->
      let entry = List.hd (Wal.read_pending dir) in
      Wal.write dir {entry with Wal.start_txid = 0L}), false;
    "wal_pre_root", Fun.id, (fun dir ->
      let entry = List.hd (Wal.read_pending dir) in
      Wal.write dir {entry with Wal.pre_state_root = hash '4'}), false] in
  let failed = List.fold_left (fun failed (name, change, alter, succeeds) ->
    try test root name change alter succeeds; failed with exn ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!" name (Printexc.to_string exn);
      true) false cases in
  if failed then exit 1

let () = Test_workspace.with_dir "head_cut" run