(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Recovery_case
module Start = Octra_node_runtime.Wal_start
module Boot = Octra_node_runtime.Startup_node_boot_shell

let boot dir =
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
            consensus_mode = true; voting_consensus_mode = false;
            consensus_port_configured = (fun () -> true);
            validators = (fun () -> []); int_value = (fun _ value -> value);
            env = (fun _ -> None); exit_fatal;
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

let () = Test_workspace.with_dir "boot_errors" run