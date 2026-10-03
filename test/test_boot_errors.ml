(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Recovery_case
module Start = Octra_node_runtime.Wal_start
module Boot = Octra_node_runtime.Startup_node_boot_shell

let boot ?(single = false) ?(overrides = []) dir =
  flush_all ();
  match Unix.fork () with
  | 0 ->
    (try
      C.expect "early check failed" (Start.check dir = Ok ());
      C.with_stores dir (fun chaindata store ->
        let exit_fatal () = exit 2 in
        match Start.recover ~data_dir:dir (fun () ->
          Boot.run_store {data_dir = dir; store; exit_fatal};
          Boot.run_node {
            data_dir = dir; store; chaindata; ledger = Octra_core.Ledger.create store;
            total_tx_count = ref 0; observer_mode = true;
            wallet = {address = "octFROM"; pub = ""};
            consensus_mode = not single; voting_consensus_mode = false;
            consensus_port_configured = (fun () -> true);
            validators = (fun () -> []); int_value = (fun _ value -> value);
            env = (fun name -> if List.mem name overrides then Some "1" else None);
            exit_fatal;
          }) with
        | Ok _ -> ()
        | Error error ->
          C.expect "refusal reason missing" (error.reason <> "");
          Printf.eprintf "event = refused reason = %s\n%!" error.reason;
          exit Start.exit_code);
      exit 0
    with exn -> Printf.eprintf "event = exception reason = %s\n%!" (Printexc.to_string exn); exit 2)
  | pid -> C.wait pid

let damage dir kind =
  if kind = "fork" then begin
    let channel = open_out_bin (Octra_core.Fork_repair_log.path dir) in
    output_string channel "{}";
    close_out channel
  end else
    C.with_stores dir (fun chain _ ->
      let index = C.SC.index chain in
      ignore (Lmdb.Txn.go Lmdb.Rw index.env (fun txn -> match kind with
        | "hash" -> Lmdb.Map.set index.tx_loc ~txn (C.hash 'a') "bad"
        | "reference" -> Lmdb.Map.set index.txid_loc ~txn 0L "bad"
        | "receipt" -> Lmdb.Map.set index.receipts ~txn (C.hash 'a') "{}"
        | "aux" -> Lmdb.Map.set index.meta ~txn Octra_core.Aux_index.pending_key "{}"
        | _ -> ())); Octra_core.Chaindata_index.sync index)

let run root =
  let failed = List.fold_left (fun failed kind ->
    let dir = Filename.concat root kind in
    Unix.mkdir dir 0o700;
    try
      ignore (C.prepare dir);
      damage dir kind;
      let before = C.evidence dir in
      let first = boot dir and second = boot dir in
      let expected = Unix.WEXITED (if kind = "valid" then 0 else 78) in
      C.expect "storage refusal has retryable exit code" (first = expected && second = expected);
      if kind <> "valid" then C.expect "refusal changed evidence" (C.evidence dir = before);
      Printf.printf "event = passed case = %s\n%!" kind;
      failed
    with exn ->
      Printf.eprintf "event = failed case = %s reason = %s\n%!" kind (Printexc.to_string exn);
      true) false ["hash"; "reference"; "receipt"; "aux"; "fork"; "valid"] in
  if failed then exit 1

let override_guard root =
  let module Marker = Octra_core.Epoch_commit_marker in
  List.iteri (fun index overrides ->
    List.iter (fun (name, mark) ->
      let dir = Filename.concat root (Printf.sprintf "%s_%d" name index) in
      Unix.mkdir dir 0o700;
      ignore (C.prepare dir);
      mark dir;
      let before = C.evidence dir in
      for _ = 1 to 2 do
        C.expect "single mode bypassed recovery barrier"
          (boot ~single:true ~overrides dir = Unix.WEXITED 78);
        C.expect "refused override changed storage" (C.evidence dir = before);
        C.expect "refused override removed recovery barrier"
          (Sys.file_exists (Marker.recovery_path dir)
           || Sys.file_exists (Marker.marker_path dir))
      done;
      Printf.printf "event = passed case = %s option = %d\n%!" name index)
      ["required", Marker.require_recovery;
       "phase", (fun dir -> Marker.write_marker dir 1 "wal_written");
       "directory", (fun dir -> Unix.mkdir (Marker.recovery_path dir) 0o700);
       "symlink", (fun dir -> Unix.symlink "HEAD.json" (Marker.recovery_path dir));
       "damaged", (fun dir ->
         let channel = open_out_bin (Marker.marker_path dir) in
         Fun.protect ~finally:(fun () -> close_out channel)
           (fun () -> output_string channel "{"))])
    [["OCTRA_SKIP_RECOVERY"];
     ["OCTRA_SKIP_RECONCILE"];
     ["OCTRA_SKIP_RECOVERY"; "OCTRA_SKIP_RECONCILE"]]

let override_policy () =
  List.iter (fun consensus_mode ->
    List.iter (fun recovery_required ->
      List.iter (fun skip_recovery ->
        List.iter (fun skip_indexes ->
          let actual = Boot.recovery_override_error ~consensus_mode
            ~recovery_required ~skip_recovery ~skip_reconcile:skip_indexes in
          let refused = (consensus_mode || recovery_required)
            && (skip_recovery || skip_indexes) in
          C.expect "recovery override policy differs"
            (Option.is_some actual = refused)) [false; true]) [false; true])
      [false; true]) [false; true]

let () = Test_workspace.with_dir "boot_errors" (fun root ->
  override_policy ();
  run root;
  override_guard root)