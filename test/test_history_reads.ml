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

let with_store ?(ops = ["standard"; "standard"]) ?(sender = fun _ -> "sender")
    ?(fields = []) ?(orphan = true) epoch action =
  Test_workspace.with_dir "history_reads" (fun dir ->
    Head.cached := None;
    let store = SC.open_chaindata (Filename.concat dir "chaindata") in
    Fun.protect ~finally:(fun () -> SC.close store; Head.cached := None) (fun () ->
      let payload = {|{"from":"sender","to_":"receiver","amount":"1","nonce":1}|} in
      let first = hash "first" and second = hash "second" in
      if orphan then
        ignore (Txlog.append (SC.txlog store) ~epoch_id:epoch ~payload:(hash "orphan" ^ payload));
      SC.begin_batch store;
      let hashes = List.mapi (fun i op ->
        let value = if i = 0 then first else if i = 1 then second else hash (string_of_int i) in
        let json = Yojson.Safe.from_string payload in
        let base = match json with `Assoc fields -> fields | _ -> assert false in
        let fields = fields @ ("from", `String (sender i)) :: List.remove_assoc "from" base in
        let tx_json = Yojson.Safe.to_string (`Assoc (("op_type", `String op) :: fields)) in
        SC.save_tx store ~hash:value ~epoch_id:epoch
          ~from_addr:(sender i) ~to_addr:"receiver" ~tx_json
          ~op_type:op ~encrypted_data:"" ~message:"";
        value) ops in
      let items = List.mapi (fun i hash -> Eic.item ~txid:(Int64.of_int i) ~hash) hashes in
      let epoch_hash, root = Eic.next_root ~prev:Eic.genesis_root ~epoch_id:epoch items in
      let ledger_root = hash "ledger" in
      let state_root = Eic.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:root in
      SC.set_epoch store {EL.empty_epoch_header with id = epoch; start_txid = 0L;
        tx_count = List.length hashes; state_root};
      SC.set_epoch_index_commitment store ~epoch_id:epoch ~epoch_hash ~root;
      SC.fsync store;
      SC.commit_batch store;
      let segment, offset = SC.txlog_position store in
      Head.set_cached Head.{schema_version; generation = 1; epoch_id = epoch; state_root;
        ledger_state_root = Some ledger_root; irmin_commit = None;
        txid_hi = Int64.of_int (List.length hashes - 1);
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

let test_private_address () =
  with_store ~ops:["standard"; "stealth"; "claim"; "standard"] 109 (fun dir store _ ->
    let before = inventory dir in
    List.iter (fun addr ->
      let read limit offset =
        Lwt_main.run (RPC.transactions_by_address ~drops_by_addr:(fun _ ~limit:_ ~offset:_ -> [])
          store ~params:(`List [`String addr; `Int limit; `Int offset]) ~addr)
        |> function Ok json -> json | Error error -> failwith error.Octra_core.Rpc.message in
      let open Yojson.Safe.Util in
      let page = read 10 0 in
      expect "private rows leaked through address count" (page |> member "total" = `Int 2);
      let rows = page |> member "transactions" |> to_list in
      expect "private rows leaked through address page" (List.length rows = 2);
      List.iter (fun row -> expect "private hash leaked through address page"
        (row |> member "op_type" = `String "standard")) rows;
      expect "private rows changed page offset"
        (read 1 1 |> member "transactions" |> to_list |> List.length = 1);
      expect "private rows changed final page"
        (read 1 2 |> member "transactions" = `List []);
      let account = Octra_core.Ledger.{balance = Z.of_int 10; nonce = 31;
        public_key = None; encrypted_balance = None; decrypt_allowance = Z.zero} in
      let response = Lwt_main.run (Octra_node_runtime.Account_read_rpc.account store
        ~params:(`List [`String addr; `Int 10]) ~profile_enabled:false
        ~started_at:0. ~addr ~account)
        |> function Ok json -> json | Error error -> failwith error.Octra_core.Rpc.message in
      expect "private rows leaked through account count"
        (response |> member "tx_count" = `Int 2);
      expect "private rows leaked through account history"
        (response |> member "recent_txs" |> to_list |> List.length = 2);
      expect "account nonce was changed by the display rule"
        (response |> member "nonce" = `Int 31)) ["sender"; "receiver"];
    expect "unrelated address gained history"
      ((Lwt_main.run (SC.public_addr_rows store "unrelated" ~limit:10 ~offset:0)
        |> Result.get_ok).total = 0);
    expect "address read changed durable indexes" (inventory dir = before);
    expect "private rows lost from verification index"
      (Index.addr_tx_count (SC.index store) "sender" = 4);
    expect "private rows broke epoch completeness"
      (Option.fold ~none:false ~some:SC.epoch_index_status_ok
        (SC.get_epoch_index_status store 109)))

let test_address_work () =
  let ops = List.init 384 (fun i -> if i mod 3 = 0 then "stealth" else "standard") in
  with_store ~ops 110 (fun dir store _ ->
    let before = inventory dir in
    let finished = ref false and ticks = ref 0 in
    let rec pulse () =
      if !finished then Lwt.return_unit
      else Lwt.bind (Lwt.pause ()) (fun () -> incr ticks; pulse ()) in
    let read offset = SC.public_addr_rows store "sender" ~limit:101 ~offset in
    let work = Lwt.bind (read 0) (fun result -> finished := true; Lwt.return result) in
    let first, () = Lwt_main.run (Lwt.both work (pulse ())) in
    let first = Result.get_ok first in
    expect "address read stopped the event loop" (!ticks > 1);
    expect "address count differs" (first.total = 256);
    expect "address page limit differs" (List.length first.rows = 101);
    let index = Hashtbl.find store.SC.public_cache "sender" in
    let next = Lwt_main.run (read 101) |> Result.get_ok in
    expect "address paging rebuilt the index"
      (Hashtbl.find store.SC.public_cache "sender" == index);
    expect "address next page differs" (next.total = 256 && List.length next.rows = 101);
    let hashes rows = List.map (fun row -> Yojson.Safe.Util.member "hash" row) rows in
    expect "address pages overlap"
      (not (List.exists (fun hash -> List.mem hash (hashes next.rows)) (hashes first.rows)));
    let last = Lwt_main.run (read 202) |> Result.get_ok in
    expect "address final page differs" (last.total = 256 && List.length last.rows = 54);
    let pending = List.init 8 (fun _ -> read 0) in
    let excess = Lwt_main.run (read 0) in
    expect "address queue admitted excess work" (excess = Error "address history busy");
    expect "address queue refused admitted work"
      (Lwt_main.run (Lwt.all pending) |> List.for_all Result.is_ok);
    expect "address read changed files" (inventory dir = before);
    expect "address reader retained requests" (store.SC.public_waiters = 0))

let account_read store =
  let account = Octra_core.Ledger.{balance = Z.of_int 123; nonce = 31;
    public_key = Some "public-key"; encrypted_balance = Some "cipher";
    decrypt_allowance = Z.zero} in
  Lwt_main.run (Octra_node_runtime.Account_read_rpc.account store
    ~params:(`List [`String "sender"; `Int 10]) ~profile_enabled:false
    ~started_at:0. ~addr:"sender" ~account)

let test_token_work () =
  with_store ~ops:(List.init 384 (fun _ -> "standard")) 111 (fun dir store _ ->
    let before = inventory dir in
    let finished = ref false and ticks = ref 0 in
    let rec pulse () =
      if !finished then Lwt.return_unit
      else Lwt.bind (Lwt.pause ()) (fun () -> incr ticks; pulse ()) in
    let read () = SC.token_txs_by_addr_page store "sender" ~limit:10 ~offset:0 in
    let work = Lwt.bind (read ()) (fun result -> finished := true; Lwt.return result) in
    let page, () = Lwt_main.run (Lwt.both work (pulse ())) in
    let page = Result.get_ok page in
    expect "token read stopped the event loop" (!ticks > 1);
    expect "standard transfer became token transfer" (page.total = 0 && page.rows = []);
    expect "token page is incomplete" (not page.incomplete && page.missing = 0);
    Index.remove_txid_loc_direct (SC.index store) 1L;
    Hashtbl.clear store.SC.token_rows_page_cache;
    let page = Lwt_main.run (read ()) |> Result.get_ok in
    expect "missing token record was hidden" (page.incomplete && page.missing = 1);
    expect "token read changed transaction log"
      (List.filter (fun (path, _) -> Filename.check_suffix path ".log") before
       = List.filter (fun (path, _) -> Filename.check_suffix path ".log") (inventory dir));
    store.SC.public_waiters <- 8;
    expect "token queue ignored occupied readers"
      (Lwt_main.run (read ()) = Error "token history busy");
    store.SC.public_waiters <- 0)

let test_token_page () =
  with_store ~ops:(List.init 125 (fun _ -> "call"))
    ~fields:["encrypted_data", `String "transfer";
      "message", `String "[\"receiver\",\"17\"]"] 112 (fun _ store _ ->
    let read addr offset = Lwt_main.run
      (SC.token_txs_by_addr_page store addr ~limit:100 ~offset) |> Result.get_ok in
    let first = read "sender" 0 and last = read "sender" 100 in
    expect "token pages changed counts"
      (first.total = 125 && first.outgoing = 125 && first.incoming = 0
       && first.has_more && List.length first.rows = 100
       && not last.has_more && List.length last.rows = 25);
    let incoming = read "receiver" 0 in
    expect "token incoming attribution changed"
      (incoming.incoming = 125 && incoming.outgoing = 0);
    let hashes (page : SC.token_rows_page) = List.map (Yojson.Safe.Util.member "hash") page.rows in
    expect "token pages overlap"
      (not (List.exists (fun hash -> List.mem hash (hashes last)) (hashes first)));
    let cached = read "sender" 0 in
    expect "token cache changed page" (first = cached))

let check_account_error store =
  let json = account_read store |> function
    | Ok json -> json
    | Error _ -> failwith "history failure removed account fields" in
  let open Yojson.Safe.Util in
  expect "history failure changed nonce" (json |> member "nonce" = `Int 31);
  expect "history failure changed balance" (json |> member "balance_raw" = `String "123");
  expect "history failure changed key state" (json |> member "has_public_key" = `Bool true);
  expect "history failure changed cipher state" (json |> member "has_encrypted_balance" = `Bool true);
  expect "history failure guessed count" (json |> member "tx_count" = `Null);
  expect "history failure exposed rows" (json |> member "recent_txs" = `List []);
  expect "history failure omitted status" (json |> member "history_status" = `String "unavailable");
  expect "history failure omitted reason" (json |> member "history_error" <> `Null)

let test_account_missing () =
  with_store 111 (fun dir store _ ->
    Index.remove_txid_loc_direct (SC.index store) 1L;
    let before = inventory dir in
    check_account_error store;
    let history = Lwt_main.run (RPC.transactions_by_address
      ~drops_by_addr:(fun _ ~limit:_ ~offset:_ -> []) store
      ~params:(`List [`String "sender"; `Int 10; `Int 0]) ~addr:"sender") in
    expect "missing address history was presented as complete" (Result.is_error history);
    expect "account read changed files" (inventory dir = before))

let test_account_busy () =
  with_store 112 (fun _ store _ ->
    store.SC.public_waiters <- 8;
    Fun.protect ~finally:(fun () -> store.SC.public_waiters <- 0)
      (fun () -> check_account_error store))

let test_address_slice () =
  let ops = List.init 1025 (fun i -> if i mod 3 = 0 then "claim" else "standard") in
  with_store ~ops 113 (fun dir store _ ->
    let before = inventory dir in
    let read () = Lwt_main.run (SC.public_addr_rows store "sender" ~limit:10 ~offset:0) in
    expect "cold address scan exceeded slice" (read () = Error "address history pending");
    let progress = (Hashtbl.find store.SC.public_cache "sender").SC.raw_count in
    expect "cold address scan did not retain progress" (progress > 0 && progress <= 1024);
    let rec finish attempts =
      if attempts = 0 then failwith "address slices did not finish";
      match read () with
      | Ok result -> result
      | Error "address history pending" -> finish (attempts - 1)
      | Error reason -> failwith reason in
    let result = finish 10 in
    expect "address slices changed public count" (result.total = 683);
    expect "address slices changed page size" (List.length result.rows = 10);
    expect "address slices exposed private rows"
      (List.for_all (fun row -> Yojson.Safe.Util.member "op_type" row = `String "standard") result.rows);
    expect "address slices changed files" (inventory dir = before))

let test_address_parallel () =
  with_store ~ops:(List.init 128 (fun _ -> "standard")) 114 (fun _ store _ ->
    let slow = SC.public_addr_rows store "sender" ~limit:10 ~offset:0 in
    let fast = SC.public_addr_rows store "unrelated" ~limit:10 ~offset:0 in
    let slow_state = Lwt_main.run (Lwt.bind fast (fun result ->
      expect "unrelated address failed" (Result.is_ok result);
      Lwt.return (Lwt.state slow))) in
    expect "unrelated address waited for cold scan"
      (match slow_state with Lwt.Sleep -> true | _ -> false);
    ignore (Lwt_main.run slow))

let test_address_eviction () =
  let sender i = "address-" ^ string_of_int i in
  with_store ~sender ~ops:(List.init 65 (fun _ -> "standard")) 115 (fun _ store _ ->
    let read i = Lwt_main.run (SC.public_addr_rows store (sender i) ~limit:1 ~offset:0)
      |> Result.get_ok |> ignore in
    for i = 0 to 63 do read i done;
    read 0;
    let hot = Hashtbl.find store.SC.public_cache (sender 0) in
    read 64;
    expect "address cache was cleared" (Hashtbl.length store.SC.public_cache = 64);
    expect "hot address was evicted" (Hashtbl.find store.SC.public_cache (sender 0) == hot);
    expect "least used address was retained" (not (Hashtbl.mem store.SC.public_cache (sender 1))))

let test_address_large () =
  let count = 100_001 in
  let ops = List.init count (fun i -> if i mod 3 = 0 then "claim" else "standard") in
  with_store ~ops 118 (fun _ store _ ->
    let started = Unix.gettimeofday () in
    let rec read previous requests =
      if requests > 110 then failwith "large address did not finish";
      let result = Lwt_main.run (SC.public_addr_rows store "sender" ~limit:10 ~offset:0) in
      let progress = (Hashtbl.find store.SC.public_cache "sender").SC.raw_count in
      expect "large address exceeded one slice" (progress - previous <= 1024);
      match result with
      | Error "address history pending" ->
        expect "large address lost progress" (progress > previous);
        read progress (requests + 1)
      | Error reason -> failwith reason
      | Ok page ->
        expect "large address count differs" (page.total = 66_667);
        expect "large address page differs" (List.length page.rows = 10);
        expect "large address leaked private rows" (List.for_all (fun row ->
          Yojson.Safe.Util.member "op_type" row = `String "standard") page.rows);
        Printf.printf "event = address_large rows = %d requests = %d seconds = %.3f\n%!"
          count (requests + 1) (Unix.gettimeofday () -. started) in
    read 0 0)

let test_account_pending () =
  with_store ~ops:(List.init 1025 (fun i -> if i mod 3 = 0 then "stealth" else "standard"))
    116 (fun _ store _ ->
      check_account_error store;
      let json = account_read store |> Result.get_ok in
      let open Yojson.Safe.Util in
      expect "account retry lost nonce" (json |> member "nonce" = `Int 31);
      expect "account retry lost public count" (json |> member "tx_count" = `Int 683);
      expect "account retry lost history" (json |> member "recent_txs" |> to_list |> List.length = 10);
      expect "account retry kept history error" (json |> member "history_error" = `Null))

let test_page_resume () =
  let ops = List.init 2050 (fun i -> if i = 0 || i = 2049 then "standard" else "claim") in
  with_store ~ops 117 (fun _ store _ ->
    let rec warm attempts =
      if attempts = 0 then failwith "address count did not finish";
      match Lwt_main.run (SC.public_addr_rows store "sender" ~limit:0 ~offset:0) with
      | Ok result -> expect "private gap changed total" (result.total = 2)
      | Error "address history pending" -> warm (attempts - 1)
      | Error reason -> failwith reason in
    warm 5;
    let read () = Lwt_main.run (SC.public_addr_rows store "sender" ~limit:2 ~offset:0) in
    expect "private gap exceeded page slice" (read () = Error "address history pending");
    expect "private gap exceeded second page slice" (read () = Error "address history pending");
    let page = read () |> Result.get_ok in
    let hashes = List.map (Yojson.Safe.Util.member "hash") page.rows in
    expect "page resume lost rows" (hashes = [`String (hash "2049"); `String (hash "first")]);
    expect "page repeat changed rows" ((read () |> Result.get_ok).rows = page.rows);
    let older = Lwt_main.run (SC.public_addr_rows store "sender" ~limit:1 ~offset:1)
      |> Result.get_ok in
    expect "page switch reused another query" (List.map (Yojson.Safe.Util.member "hash") older.rows
      = [`String (hash "first")]))

let test_address_change () =
  with_store ~orphan:false 0 (fun _ store _ ->
    let read () = Lwt_main.run (SC.public_addr_rows store "sender" ~limit:10 ~offset:0)
      |> Result.get_ok in
    let original = read () in
    let head = Option.get (Head.get_cached ()) in
    SC.begin_batch store;
    List.iteri (fun i op ->
      let tx_json = Printf.sprintf
        {|{"from":"sender","to_":"receiver","amount":"1","nonce":%d,"op_type":"%s"}|}
        (i + 2) op in
      SC.save_tx store ~hash:(hash op) ~epoch_id:1 ~from_addr:"sender" ~to_addr:"receiver"
        ~tx_json ~op_type:op ~encrypted_data:"" ~message:"") ["stealth"; "claim"; "standard"];
    SC.commit_batch store;
    Head.cached := None;
    let appended = read () in
    expect "append exposed private count" (appended.total = original.total + 1);
    expect "append lost public row" (List.length appended.rows = 3);
    let pending = SC.public_addr_rows store "receiver" ~limit:10 ~offset:0 in
    ignore (SC.rollback_to_head store ~head ~inflight_start_txid:2L ~inflight_tx_count:3);
    Head.set_cached head;
    expect "old generation read survived rollback"
      (Lwt_main.run pending = Error "address history changed");
    expect "rollback kept old history" ((read ()).rows = original.rows);
    expect "rollback retained readers" (store.SC.public_waiters = 0))

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
  let tests = if Array.mem "--large" Sys.argv then ["address_large", test_address_large]
    else
    ["private_address", test_private_address; "address_work", test_address_work;
     "token_work", test_token_work;
     "token_page", test_token_page;
     "account_missing", test_account_missing; "account_busy", test_account_busy;
     "address_slice", test_address_slice; "address_parallel", test_address_parallel;
     "address_eviction", test_address_eviction;
     "account_pending", test_account_pending; "page_resume", test_page_resume;
     "address_change", test_address_change;
     "epoch_missing", test_epoch_missing; "epoch_valid", test_epoch_valid;
     "hash_missing", test_hash_missing;
     "startup_missing", (fun () -> test_startup_missing false 104);
     "startup_marker", (fun () -> test_startup_missing true 105);
     "startup_valid", test_startup_valid;
     "metadata_missing", (fun () -> test_epoch_meta false 107);
     "metadata_changed", (fun () -> test_epoch_meta true 108)] in
  let failures = List.filter_map (fun (name, run) ->
    try run (); Printf.printf "event = history_read name = %s status = passed\n%!" name; None
    with error -> Printf.printf "event = history_read name = %s status = failed reason = %s\n%!"
      name (Printexc.to_string error); Some name)
    tests in
  if failures <> [] then exit 1