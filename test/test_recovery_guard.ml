(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module SC = Octra_core.Store_chaindata
module SI = Octra_core.Store_irmin
module HM = Octra_core.Head_manifest
module EL = Octra_core.Epochlog
module Eic = Octra_core.Epoch_index_commitment
module Marker = Octra_core.Epoch_commit_marker
module Recovery = Octra_core.Startup_recovery
module Index = Octra_core.Chaindata_index

let expect label ok =
  if not ok then failwith label

let rejects action =
  match action () with
  | _ -> false
  | exception _ -> true

let write path bytes =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel bytes; flush channel)

let hash c = String.make 64 c

let with_stores dir action =
  let chaindata = SC.open_chaindata (Filename.concat dir "chaindata") in
  Fun.protect ~finally:(fun () -> SC.close chaindata) (fun () ->
    let store = Lwt_main.run (SI.open_store (Filename.concat dir "irmin_store")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (SI.close store))
      (fun () -> action chaindata store))

let prepare dir =
  with_stores dir (fun chaindata store ->
    Lwt_main.run (SI.begin_epoch_batch store);
    Lwt_main.run (SI.set_meta store "last_epoch" "0");
    Lwt_main.run (SI.set_meta store "current_epoch" "1");
    Lwt_main.run (SI.set_meta store "total_supply" "0");
    Lwt_main.run (SI.set_account store "observer" Octra_core.Ledger_types.empty_account);
    Lwt_main.run (SI.commit_epoch_batch store "test epoch zero");
    let ledger_root = Option.get (Lwt_main.run (SI.get_head_hash store)) in
    let irmin_commit = Lwt_main.run (SI.get_commit_hash store) in
    SC.begin_batch chaindata;
    let start_txid = SC.next_txid chaindata in
    let tx_hash = hash 'a' in
    SC.save_tx chaindata ~hash:tx_hash ~epoch_id:0
      ~from_addr:"octFROM" ~to_addr:"octTO"
      ~tx_json:{|{"from":"octFROM","to_":"octTO"}|}
      ~op_type:"standard" ~encrypted_data:"" ~message:"";
    let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:0
      [Eic.item ~txid:start_txid ~hash:tx_hash] in
    let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root
      ~epoch_index_root:root in
    SC.set_epoch chaindata {
      EL.empty_epoch_header with
      id = 0;
      state_root;
      start_txid;
      tx_count = 1;
    };
    SC.set_epoch_index_commitment chaindata ~epoch_id:0 ~epoch_hash ~root;
    SC.commit_batch chaindata;
    SC.fsync chaindata;
    let txlog_seg, txlog_off = SC.txlog_position chaindata in
    let head = HM.{
      schema_version;
      generation = 0;
      epoch_id = 0;
      state_root;
      ledger_state_root = Some ledger_root;
      irmin_commit;
      txid_hi = start_txid;
      txlog_seg = Some txlog_seg;
      txlog_off = Some txlog_off;
      epochlog_off = Some (SC.epochlog_offset chaindata);
      commit_id = "recovery-guard";
      ts = 0.;
      quorum_cert_hash = None;
      epoch_index_hash = Some epoch_hash;
      epoch_index_root = Some root;
    } in
    HM.atomic_write dir head;
    head)

let run dir =
  let result = with_stores dir (fun chaindata store ->
    Lwt_main.run (Recovery.recover ~data_dir:dir ~chaindata ~store)) in
  Marker.clear_recovery dir;
  result

let child_run dir =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try ignore (run dir); exit 0 with exn ->
      Printf.eprintf "event = recovery_refused reason = %s\n%!"
        (Printexc.to_string exn);
      exit 2)
  | pid -> Test_workspace.wait pid

let failed = function
  | Unix.WEXITED 1 | Unix.WEXITED 2 -> true
  | _ -> false

let guard_path dir = Filename.concat dir "recovery_required"

let test_marker_roundtrip () =
  Test_workspace.with_dir "marker_roundtrip" (fun dir ->
    expect "missing marker" (Marker.read_marker dir = None);
    Marker.write_marker dir 7 "wal_written";
    expect "marker roundtrip"
      (match Marker.read_marker dir with
       | Some m -> m.epoch_id = 7 && m.phase = "wal_written"
       | None -> false);
    Marker.clear_marker dir;
    Marker.clear_marker dir;
    expect "marker removed" (Marker.read_marker dir = None))

let test_marker_invalid () =
  Test_workspace.with_dir "marker_invalid" (fun dir ->
    List.iter (fun bytes ->
      write (Marker.marker_path dir) bytes;
      expect "invalid marker accepted as absent"
        (rejects (fun () -> Marker.read_marker dir)))
      ["{"; "{}";
       {|{"epoch_id":-1,"phase":"begin","ts":0}|};
       {|{"epoch_id":1,"phase":"unexpected","ts":0}|};
       {|{"epoch_id":1,"phase":"begin","ts":NaN}|}])

let test_marker_io () =
  Test_workspace.with_dir "marker_io" (fun dir ->
    Unix.mkdir (Marker.marker_path dir) 0o700;
    expect "marker removal error swallowed"
      (rejects (fun () -> Marker.clear_marker dir));
    expect "marker read error swallowed"
      (rejects (fun () -> Marker.read_marker dir));
    expect "marker write error swallowed"
      (rejects (fun () -> Marker.write_marker dir 0 "begin")))

let test_phase_retry phase =
  Test_workspace.with_dir "phase_retry" (fun dir ->
    let head = prepare dir in
    Marker.write_marker dir 0 phase;
    HM.atomic_write dir { head with epoch_index_hash = Some (hash 'f') };
    for _attempt = 1 to 2 do
      expect "damaged HEAD accepted" (failed (child_run dir));
      expect "commit marker erased before verification"
        (Option.map (fun m -> m.Marker.phase) (Marker.read_marker dir) = Some phase);
      expect "recovery guard missing after failure" (Sys.file_exists (guard_path dir))
    done;
    HM.atomic_write dir head;
    let repaired = run dir in
    expect "retry skipped repair" (not repaired.full_repair_skipped);
    expect "repair not verified" (repaired.boundary_ok && repaired.eic_ok);
    expect "commit marker retained after success" (Marker.read_marker dir = None);
    expect "guard retained after success" (not (Sys.file_exists (guard_path dir)));
    expect "clean restart skipped proof" (not (run dir).full_repair_skipped))

let test_unmarked_retry () =
  Test_workspace.with_dir "unmarked_retry" (fun dir ->
    let head = prepare dir in
    ignore (run dir);
    HM.atomic_write dir { head with epoch_index_hash = Some (hash 'f') };
    expect "damaged HEAD accepted" (failed (child_run dir));
    expect "unmarked failure lost guard" (Sys.file_exists (guard_path dir));
    HM.atomic_write dir head;
    expect "guard did not force verification" (not (run dir).full_repair_skipped);
    expect "clean restart skipped proof" (not (run dir).full_repair_skipped))

let test_schema_failure () =
  Test_workspace.with_dir "schema_failure" (fun dir ->
    let head = prepare dir in
    with_stores dir (fun chaindata _ ->
      Index.set_meta_direct (SC.index chaindata) "index_schema_version" "unchecked");
    Marker.write_marker dir 0 "begin";
    HM.atomic_write dir { head with epoch_index_hash = Some (hash 'f') };
    expect "damaged HEAD accepted" (failed (child_run dir));
    with_stores dir (fun chaindata _ ->
      expect "failed recovery certified schema"
        (Index.get_meta (SC.index chaindata) "index_schema_version" = Some "unchecked")))

let test_writer_watermark () =
  Test_workspace.with_dir "writer_watermark" (fun dir ->
    ignore (prepare dir);
    expect "writer metadata bypassed first verification" (not (run dir).full_repair_skipped);
    expect "verified restart skipped proof" (not (run dir).full_repair_skipped))

let test_epoch_suffix () =
  Test_workspace.with_dir "epoch_suffix" (fun dir ->
    ignore (prepare dir);
    let path = Filename.concat dir "chaindata/epochlog/epochs.dat" in
    let fd = Unix.openfile path [Unix.O_WRONLY; Unix.O_APPEND] 0 in
    Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
      expect "short epoch test write" (Unix.write_substring fd "x" 0 1 = 1);
      Unix.fsync fd);
    Marker.write_marker dir 0 "begin";
    for _attempt = 1 to 2 do
      expect "truncated epoch journal accepted" (failed (child_run dir));
      expect "epoch failure erased marker" (Marker.read_marker dir <> None);
      expect "epoch failure erased guard" (Sys.file_exists (guard_path dir))
    done)

let test_bad_marker_guard () =
  Test_workspace.with_dir "bad_marker_guard" (fun dir ->
    ignore (prepare dir);
    ignore (run dir);
    write (Marker.marker_path dir) "{";
    expect "invalid marker accepted" (failed (child_run dir));
    expect "marker parse failure lost guard" (Sys.file_exists (guard_path dir));
    Unix.unlink (Marker.marker_path dir);
    expect "deleted marker bypassed verification" (not (run dir).full_repair_skipped))

let test_bad_wal_guard () =
  Test_workspace.with_dir "bad_wal_guard" (fun dir ->
    ignore (prepare dir);
    ignore (run dir);
    Octra_core.Wal.ensure_dir dir;
    let path = Octra_core.Wal.entry_path dir 1 in
    write path "{";
    for _ = 1 to 2 do
      expect "damaged WAL accepted" (failed (child_run dir));
      expect "WAL parse failure lost guard" (Sys.file_exists (guard_path dir));
      expect "WAL evidence erased" (Sys.file_exists path)
    done)

let test_late_startup_failure () =
  Test_workspace.with_dir "late_startup_failure" (fun dir ->
    ignore (prepare dir);
    with_stores dir (fun chaindata store ->
      ignore (Lwt_main.run (Recovery.recover ~data_dir:dir ~chaindata ~store)));
    expect "storage check released startup guard"
      (Marker.recovery_required dir);
    expect "unfinished startup skipped verification" (not (run dir).full_repair_skipped))

let test_boot_guard () =
  Test_workspace.with_dir "boot_guard" (fun dir ->
    ignore (prepare dir);
    let boot stop = with_stores dir (fun chaindata store ->
      let module Boot = Octra_node_runtime.Startup_node_boot_shell in
      let ledger = Octra_core.Ledger.create store in
      Boot.run_node Boot.{
        data_dir = dir; store; ledger; chaindata;
        total_tx_count = ref 0; observer_mode = true;
        wallet = {address = "observer"; pub = ""};
        consensus_mode = true; voting_consensus_mode = false;
        consensus_port_configured = (fun () -> true);
        validators = (fun () -> []);
        int_value = (fun _ value -> if stop then failwith "planned history failure" else value);
        env = (fun _ -> None);
        exit_fatal = (fun () -> failwith "unexpected startup failure");
      }) in
    for _ = 1 to 2 do
      expect "history failure not reached"
        (match boot true with
         | _ -> false
         | exception Failure reason -> reason = "planned history failure");
      expect "failed boot released guard" (Marker.recovery_required dir)
    done;
    expect "verified boot epoch differs" (boot false = 1);
    expect "verified boot retained guard" (not (Marker.recovery_required dir));
    expect "verified boot skipped proof" (not (run dir).full_repair_skipped))

let () =
  let phases = ["stage_batch_begin"; "wal_written"; "begin"; "chaindata_begin";
    "chaindata_committed"; "irmin_begin"; "irmin_committed"] in
  let cases = [
    "marker_roundtrip", test_marker_roundtrip;
    "marker_invalid", test_marker_invalid;
    "marker_io", test_marker_io;
    "unmarked_retry", test_unmarked_retry;
    "schema_failure", test_schema_failure;
    "writer_watermark", test_writer_watermark;
    "epoch_suffix", test_epoch_suffix;
    "bad_marker_guard", test_bad_marker_guard;
    "bad_wal_guard", test_bad_wal_guard;
    "late_startup_failure", test_late_startup_failure;
    "boot_guard", test_boot_guard;
  ] @ List.map (fun phase -> phase, (fun () -> test_phase_retry phase)) phases in
  let failures = List.filter_map (fun (name, action) ->
    match action () with
    | () -> Printf.printf "event = test name = %s status = passed\n%!" name; None
    | exception exn ->
      Printf.eprintf "event = test name = %s status = failed reason = %s\n%!"
        name (Printexc.to_string exn);
      Some name) cases in
  if failures <> [] then exit 1