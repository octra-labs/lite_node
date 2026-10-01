(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Drop = Octra_core.Tx_drop
module Transaction = Octra_core.Transaction

let fail name =
  failwith ("test_tx_drop: " ^ name)

let expect name condition =
  if not condition then fail name

let row ?(from_addr = "octFrom") ?(to_addr = "octTo") index =
  Drop.{
    hash = Digestif.SHA256.(digest_string (string_of_int index) |> to_hex);
    from_addr;
    to_addr;
    nonce = index;
    ou = Z.of_int (1_000 + index);
    op_type = Transaction.ProgramExec;
    reason = "expired";
    detail = "TTL exceeded";
    dropped_at = float_of_int index;
  }

let data_dir name =
  let path =
    Test_workspace.path
      (name ^ "_" ^ string_of_int (Unix.getpid ()))
  in
  if not (Sys.file_exists path) then Unix.mkdir path 0o700;
  path

let test_reopen () =
  let path = data_dir "tx_drop_reopen" in
  let expected = row 1 in
  let db = Drop.open_db path in
  begin
    match Drop.save_many db [expected] with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  Drop.close db;
  let reopened = Drop.open_db path in
  begin
    match Drop.find reopened expected.hash with
    | None -> fail "reopen lookup missing"
    | Some actual ->
      expect "reopen hash" (String.equal actual.hash expected.hash);
      expect "reopen nonce" (actual.nonce = expected.nonce);
      expect "reopen fee" (Z.equal actual.ou expected.ou);
      expect "reopen operation" (actual.op_type = expected.op_type);
      expect "reopen reason" (String.equal actual.reason expected.reason);
      expect "reopen detail" (String.equal actual.detail expected.detail)
  end;
  Drop.close reopened

let test_limit () =
  let path = data_dir "tx_drop_limit" in
  let first = row 1 in
  let second = row 2 in
  let third = row 3 in
  let db = Drop.open_db ~max_rows:2 path in
  begin
    match Drop.save_many db [first; second; third] with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  expect "oldest row removed" (Option.is_none (Drop.find db first.hash));
  expect "second row retained" (Option.is_some (Drop.find db second.hash));
  expect "third row retained" (Option.is_some (Drop.find db third.hash));
  Drop.close db

let test_write_after_lookup () =
  let path = data_dir "tx_drop_write_after_lookup" in
  let first = row 11 in
  let second = row 12 in
  let db = Drop.open_db path in
  begin
    match Drop.save_many db [first] with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  expect "first row found" (Option.is_some (Drop.find db first.hash));
  begin
    match Drop.save_many db [second] with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  expect "second row found" (Option.is_some (Drop.find db second.hash));
  Drop.close db

let test_address_history () =
  let path = data_dir "tx_drop_address" in
  let first = row ~from_addr:"octAlice" ~to_addr:"octBob" 21 in
  let second = row ~from_addr:"octCarol" ~to_addr:"octAlice" 22 in
  let self = row ~from_addr:"octAlice" ~to_addr:"octAlice" 23 in
  let unrelated = row ~from_addr:"octCarol" ~to_addr:"octBob" 24 in
  let db = Drop.open_db path in
  begin
    match Drop.save_many db [first; second; self; unrelated] with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  let hashes rows = List.map (fun (item : Drop.row) -> item.hash) rows in
  expect "address query order"
    (hashes (Drop.by_addr db "octAlice" ~limit:10 ~offset:0)
     = [self.hash; second.hash; first.hash]);
  expect "address query page"
    (hashes (Drop.by_addr db "octAlice" ~limit:1 ~offset:1)
     = [second.hash]);
  Drop.close db;
  let reopened = Drop.open_db path in
  expect "address query survives reopen"
    (hashes (Drop.by_addr reopened "octAlice" ~limit:10 ~offset:0)
     = [self.hash; second.hash; first.hash]);
  Drop.close reopened

let save db rows =
  match Drop.save_many db rows with
  | Ok () -> ()
  | Error reason -> fail reason

let test_replace () =
  let path = data_dir "tx_drop_replace" in
  let db = Drop.open_db ~max_rows:2 path in
  let first = row ~from_addr:"octOld" ~to_addr:"octOld" 31 in
  let second = row 32 in
  let changed = {first with from_addr = "octNew"; to_addr = "octNew"; dropped_at = 34.} in
  save db [first; second];
  save db [changed; row 33];
  expect "old address index removed" (Drop.by_addr db "octOld" ~limit:10 ~offset:0 = []);
  expect "replacement retains one address reference"
    (Drop.by_addr db "octNew" ~limit:10 ~offset:0 = [changed]);
  expect "trim removes replaced ordering" (Drop.find db second.hash = None);
  expect "replacement row retained" (Drop.find db first.hash = Some changed);
  Drop.close db

let test_map_full () =
  let path = data_dir "tx_drop_full" in
  let db = Drop.open_db ~max_rows:10_000 path in
  let original = row 41 in
  save db [original];
  let changed = {original with reason = "replaced"} in
  let large = List.init 1_200 (fun index ->
    {(row (index + 100)) with detail = String.make 60_000 'x'}) in
  begin match Drop.save_many db (changed :: large) with
  | Error reason ->
    expect "map exhaustion was reached" (reason = Printexc.to_string Lmdb.Map_full)
  | Ok () -> fail "full map accepted oversized batch"
  end;
  expect "full map restores original row" (Drop.find db original.hash = Some original);
  expect "full map restores address index"
    (Drop.by_addr db original.from_addr ~limit:10_000 ~offset:0 = [original]);
  Drop.close db;
  let db = Drop.open_db path in
  expect "aborted batch stays absent after reopen"
    (Drop.by_addr db original.from_addr ~limit:10_000 ~offset:0 = [original]);
  save db [row 42];
  Drop.close db

let test_legacy_files () =
  let path = data_dir "tx_drop_legacy" in
  let legacy = Filename.concat path "drop_db" in
  Unix.mkdir legacy 0o700;
  let bytes = "retained old diagnostic bytes" in
  List.iter (fun name ->
    let channel = open_out_bin (Filename.concat legacy name) in
    output_string channel bytes;
    close_out channel) ["events.sqlite"; "events.sqlite-wal"; "events.sqlite-shm"];
  let db = Drop.open_db path in
  save db [row 51];
  Drop.close db;
  List.iter (fun name ->
    let channel = open_in_bin (Filename.concat legacy name) in
    let actual = really_input_string channel (in_channel_length channel) in
    close_in channel;
    expect "old diagnostics left unchanged" (actual = bytes))
    ["events.sqlite"; "events.sqlite-wal"; "events.sqlite-shm"];
  expect "separate local LMDB created"
    (Sys.file_exists (Filename.concat path "local_drops/data.mdb"));
  expect "no chain store created"
    (not (Sys.file_exists (Filename.concat path "chaindata")))

let test_bad_batch () =
  let path = data_dir "tx_drop_bad" in
  let db = Drop.open_db path in
  let first = row 61 in
  let bad = {(row 62) with dropped_at = Float.nan} in
  expect "invalid record aborts batch" (Result.is_error (Drop.save_many db [first; bad]));
  expect "batch prefix not visible" (Drop.find db first.hash = None);
  save db [first];
  Drop.close db

let test_empty_recipient () =
  let path = data_dir "tx_drop_empty" in
  let db = Drop.open_db path in
  let program = {(row ~to_addr:"" 65) with op_type = Transaction.ProgramDeploy} in
  save db [program];
  expect "program drop retains empty recipient" (Drop.find db program.hash = Some program);
  expect "program drop sender indexed"
    (Drop.by_addr db program.from_addr ~limit:10 ~offset:0 = [program]);
  expect "empty address query is empty" (Drop.by_addr db "" ~limit:10 ~offset:0 = []);
  Drop.close db

let test_write_kill () =
  let path = data_dir "tx_drop_kill" in
  let first = row 71 in
  let db = Drop.open_db path in
  save db [first];
  Drop.close db;
  let child = Unix.fork () in
  if child = 0 then begin
    let env = Lmdb.Env.create Lmdb.Rw ~max_maps:3
      ~map_size:(64 * 1024 * 1024) ~flags:Lmdb.Env.Flags.no_tls
      (Filename.concat path "local_drops") in
    let rows = Lmdb.Map.open_existing Lmdb.Map.Nodup
      ~key:Lmdb.Conv.string ~value:Lmdb.Conv.string ~name:"rows" env in
    ignore (Lmdb.Txn.go Lmdb.Rw env (fun txn ->
      let changed = {first with from_addr = "octChanged"; dropped_at = 72.} in
      Lmdb.Map.set rows ~txn first.hash (Octra_core.Drop_record.encode changed);
      Unix.kill (Unix.getpid ()) Sys.sigkill));
    Unix._exit 99
  end;
  let _, status = Unix.waitpid [] child in
  expect "child killed before index publication" (status = Unix.WSIGNALED Sys.sigkill);
  let db = Drop.open_db path in
  expect "killed transaction row rolled back" (Drop.find db first.hash = Some first);
  expect "killed transaction index retained"
    (Drop.by_addr db first.from_addr ~limit:10 ~offset:0 = [first]);
  expect "killed transaction new address absent"
    (Drop.by_addr db "octChanged" ~limit:10 ~offset:0 = []);
  save db [row 72];
  Drop.close db

let () =
  let path = data_dir "tx_drop_files" in
  let db = Drop.open_db path in
  Drop.close db;
  expect "local drops must not open SQLite"
    (not (Sys.file_exists (Filename.concat path "drop_db/events.sqlite")));
  test_reopen ();
  test_limit ();
  test_write_after_lookup ();
  test_address_history ();
  test_replace ();
  test_map_full ();
  test_legacy_files ();
  test_bad_batch ();
  test_empty_recipient ();
  test_write_kill ();
  print_endline "status = pass test = tx_drop"