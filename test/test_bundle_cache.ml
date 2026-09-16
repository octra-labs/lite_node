(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_node_runtime.Consensus_bundle_cache
module Transaction = Octra_core.Transaction
module C_types = Octra_consensus.C_types
module C_engine = Octra_consensus.C_engine
module C_hash = Octra_consensus.C_hash

let fail msg =
  failwith ("test_bundle_cache: " ^ msg)

let expect label cond =
  if not cond then fail label

let tx ?message ?(op_type = Transaction.Standard) nonce =
  Transaction.{
    from = "oct_sender";
    to_ = "oct_receiver";
    amount = Z.of_int 1;
    nonce;
    ou = Z.of_int 1_000;
    timestamp = 1.0;
    signature = "sig";
    public_key = Some "pub";
    message;
    op_type;
    encrypted_data = None;
  }

let header epoch_id =
  C_types.{
    proto_version = C_types.proto_version_current;
    chain_id = "octra-test";
    epoch_id;
    prev_state_root = String.make 32 '\xaa';
    tx_list_hash = String.make 32 '\xbb';
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = String.make 32 '\xcc';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb";
    txid_hi = 0L;
    ts = 0.0;
  }

let empty_header epoch_id =
  C_types.{
    (header epoch_id) with
    tx_list_hash = C_engine.tx_list_hash_for_header [];
    receipt_root = C_hash.receipt_root [];
  }

let raw_of_tx tx =
  ([Transaction.hash tx], [Yojson.Safe.to_string (Transaction.to_yojson tx)], [])

let test_store_cached_and_stats () =
  let cache = C.create ~cap:4 in
  let tx = tx 1 in
  expect "store no summary" (C.store cache ~pid:"p1" ~tx_hashes:[Transaction.hash tx] ~txs:[tx] ~receipts_json:[] = None);
  begin
  match C.cached cache "p1" with
  | C.Cached bundle ->
    expect "cached hash" (bundle.C.tx_hashes = [Transaction.hash tx]);
    expect "cached txs" (bundle.C.txs = [tx]);
    expect "cached receipts" (bundle.C.receipts_json = [])
  | C.Missing -> fail "cached missing"
  | C.Decode_error e -> fail ("cached decode error " ^ e)
  end;
  let stats = C.stats cache in
  expect "stats stores" (stats.C.stores = 1);
  expect "stats hits" (stats.C.hits = 1);
  expect "stats misses" (stats.C.misses = 0)

let test_peek_does_not_count () =
  let cache = C.create ~cap:4 in
  let tx = tx 1 in
  let _ = C.store cache ~pid:"p1" ~tx_hashes:[Transaction.hash tx] ~txs:[tx] ~receipts_json:[] in
  let before = C.stats cache in
  expect "peek found" (Option.is_some (C.peek_raw cache "p1"));
  let after = C.stats cache in
  expect "peek hits unchanged" (before.C.hits = after.C.hits);
  expect "peek misses unchanged" (before.C.misses = after.C.misses)

let test_lookup_counts_and_evicts () =
  let cache = C.create ~cap:1 in
  let tx1 = tx 1 in
  let tx2 = tx 2 in
  let _ = C.store cache ~pid:"p1" ~tx_hashes:[Transaction.hash tx1] ~txs:[tx1] ~receipts_json:[] in
  let _ = C.store cache ~pid:"p2" ~tx_hashes:[Transaction.hash tx2] ~txs:[tx2] ~receipts_json:[] in
  expect "p1 evicted" (C.lookup_raw cache "p1" = None);
  expect "p2 present" (Option.is_some (C.lookup_raw cache "p2"));
  let stats = C.stats cache in
  expect "evictions" (stats.C.evictions = 1);
  expect "cache size" (stats.C.cache_size = 1);
  expect "hits" (stats.C.hits = 1);
  expect "misses" (stats.C.misses = 1)

let test_shared_count () =
  let cache =
    C.create_with_limits
      ~cap:4
      ~check_limit:1_000_000
      ~shared_cap:2
      ~shared_limit:(32 * 1024 * 1024)
  in
  let first = tx 1 in
  let second = tx 2 in
  let third = tx 3 in
  let first_hash = Transaction.hash first in
  let second_hash = Transaction.hash second in
  let third_hash = Transaction.hash third in
  expect "shared first pair"
    (C.share cache [first; second] = [first_hash; second_hash]);
  expect "shared first found" (C.find_shared cache first_hash = Some first);
  expect "shared repeat announced" (C.share cache [first] = [first_hash]);
  expect "shared third announced" (C.share cache [third] = [third_hash]);
  expect "shared oldest evicted" (C.find_shared cache first_hash = None);
  expect "shared second retained" (C.find_shared cache second_hash = Some second);
  expect "shared third retained" (C.find_shared cache third_hash = Some third)

let test_shared_txs_byte_limit () =
  let item = tx 4 in
  let bytes =
    Yojson.Safe.to_string (Transaction.to_yojson item)
    |> String.length
  in
  let exact = C.create_with_limits ~cap:4 ~check_limit:1_000_000 ~shared_cap:2 ~shared_limit:bytes in
  let short =
    C.create_with_limits ~cap:4 ~check_limit:1_000_000 ~shared_cap:2 ~shared_limit:(bytes - 1)
  in
  let hash = Transaction.hash item in
  expect "shared exact retained" (C.share exact [item] = [hash]);
  expect "shared oversized refused" (C.share short [item] = []);
  expect "shared oversized missing" (C.find_shared short hash = None)

let test_decode_and_parse_txs () =
  let tx = tx ~message:"payload" 1 in
  let raw = raw_of_tx tx in
  begin
    match C.parse_txs raw with
    | Ok [parsed] -> expect "parse tx" (parsed = tx)
    | Ok _ -> fail "parse count"
    | Error e -> fail ("parse error " ^ e)
  end;
  begin
    match C.decode raw with
    | Ok bundle -> expect "decode tx" (bundle.C.txs = [tx])
    | Error e -> fail ("decode error " ^ e)
  end;
  match C.decode (["bad"], [Yojson.Safe.to_string (Transaction.to_yojson tx)], []) with
  | Ok _ -> fail "bad hash accepted"
  | Error e -> expect "bad hash reason" (e = "bundle tx hash mismatch")

let test_oversized_bundle_rejected () =
  let item = tx ~message:(String.make 257 'm') 1 in
  let raw = raw_of_tx item in
  expect "oversized parse rejected" (Result.is_error (C.parse_txs raw));
  expect "oversized decode rejected" (Result.is_error (C.decode raw))

let test_summary_tick () =
  let cache = C.create ~cap:128 in
  let rec loop last = function
    | 51 -> last
    | n ->
      let tx = tx n in
      let next =
        C.store cache
          ~pid:("p" ^ string_of_int n)
          ~tx_hashes:[Transaction.hash tx]
          ~txs:[tx]
          ~receipts_json:[]
      in
      loop next (n + 1)
  in
  match loop None 1 with
  | Some stats ->
    expect "summary stores" (stats.C.stores = 50);
    expect "summary size" (stats.C.cache_size = 50)
  | None -> fail "summary missing"

let test_log_empty () =
  let cache = C.create ~cap:4 in
  let tx = tx 7 in
  C.store_with_log cache
    ~pid:"p7"
    ~tx_hashes:[Transaction.hash tx]
    ~txs:[tx]
    ~receipts_json:[];
  begin
    match C.cached_with_log cache "p7" with
    | Some bundle ->
      expect "logged cached tx" (bundle.C.txs = [tx]);
      expect "logged cached receipts" (bundle.C.receipts_json = [])
    | None -> fail "logged cached missing"
  end;
  let bad_pid = String.make 32 '\x07' in
  C.store_with_log cache
    ~pid:bad_pid
    ~tx_hashes:["bad"]
    ~txs:[tx]
    ~receipts_json:[];
  expect "bad cached helper" (C.cached_with_log cache bad_pid = None);
  let empty = empty_header 9L in
  let non_empty = header 10L in
  expect "receipt root matches" (C.receipt_root_matches empty []);
  expect "receipt root mismatch" (not (C.receipt_root_matches empty ["{}"]));
  expect "empty header" (C.header_has_empty_bundle empty);
  expect "non-empty header" (not (C.header_has_empty_bundle non_empty));
  C.store_empty_header_with_log cache empty;
  match C.cached_with_log cache (C_hash.proposal_id empty) with
  | Some bundle ->
    expect "empty stored hashes" (bundle.C.tx_hashes = []);
    expect "empty stored txs" (bundle.C.txs = []);
    expect "empty stored receipts" (bundle.C.receipts_json = [])
  | None -> fail "empty bundle missing"

let test_node_runtime () =
  let cache = C.create ~cap:4 in
  let runtime = C.node_runtime cache in
  let tx = tx 11 in
  runtime.C.store_bundle
    ~proposal_id:"p11"
    ~tx_hashes:[Transaction.hash tx]
    ~txs:[tx]
    ~receipts_json:[];
  begin
    match runtime.cached_bundle "p11" with
    | Some (_tx_hashes, txs, _receipts_json) ->
      expect "runtime cached tx" (txs = [tx]);
      expect "runtime raw" (Option.is_some (runtime.lookup_raw "p11"))
    | None -> fail "runtime cached missing"
  end;
  let empty = empty_header 12L in
  expect "runtime empty header" (runtime.header_has_empty_bundle empty);
  expect "runtime receipt root" (runtime.receipt_root_matches empty []);
  runtime.store_empty_bundle empty;
  expect "runtime empty cached"
    (Option.is_some (runtime.cached_bundle (C_hash.proposal_id empty)));
  runtime.store_empty_proposal ~proposal_id:"empty-pid";
  expect "runtime empty proposal"
    (Option.is_some (runtime.cached_bundle "empty-pid"))

let test_frozen_prune () =
  let cache = C.create ~cap:4 in
  let old_header = header 1L in
  let new_header = header 3L in
  C.freeze cache "old" C.{ header = old_header; tx_hashes = []; txs = []; receipts_json = [] };
  C.freeze cache "new" C.{ header = new_header; tx_hashes = []; txs = []; receipts_json = [] };
  expect "old found" (Option.is_some (C.find_frozen cache "old"));
  expect "new found" (Option.is_some (C.find_frozen cache "new"));
  C.prune_frozen cache ~finalized_epoch:1L;
  expect "old pruned" (C.find_frozen cache "old" = None);
  expect "new kept" (Option.is_some (C.find_frozen cache "new"));
  expect "freeze key" (C.freeze_key ~epoch_id:3L ~round:2 = "3:2")

let ready_batch items =
  Octra_core.Preverify_worker.{
    ready = List.map (fun tx -> { tx; receipt = None }) items;
    skipped = [];
  }

let run_preverify_for cache ~purpose ~state_root txs verify =
  C.run_preverify_once
    cache
    ~purpose
    ~state_root
    ~tx_hashes:(List.map Transaction.hash txs)
    ~txs
    (fun _ txs -> verify txs)

let run_preverify cache ~state_root txs verify =
  run_preverify_for cache ~purpose:C.Build_proposal ~state_root txs verify

let test_check_reuse () =
  let cache = C.create ~cap:4 in
  let first = tx 21 in
  let second = tx 22 in
  let calls = Hashtbl.create 2 in
  let verify txs =
    List.iter
      (fun tx ->
        let hash = Transaction.hash tx in
        let count = Option.value ~default:0 (Hashtbl.find_opt calls hash) in
        Hashtbl.replace calls hash (count + 1))
      txs;
    Lwt.return (ready_batch txs)
  in
  let run state_root txs =
    run_preverify cache ~state_root txs verify
    |> Lwt_main.run
  in
  let initial = run "state-a" [first] in
  let grown = run "state-a" [first; second] in
  expect "initial ready" (Octra_core.Preverify_worker.txs initial = [first]);
  expect "grown ready order"
    (Octra_core.Preverify_worker.txs grown = [first; second]);
  expect "first item reused"
    (Hashtbl.find calls (Transaction.hash first) = 1);
  expect "second item verified once"
    (Hashtbl.find calls (Transaction.hash second) = 1);
  let _ = run "state-b" [first; second] in
  expect "new state verifies first"
    (Hashtbl.find calls (Transaction.hash first) = 2);
  expect "new state verifies second"
    (Hashtbl.find calls (Transaction.hash second) = 2)

let test_check_cancel () =
  let cache = C.create ~cap:4 in
  let item = tx 23 in
  let calls = ref 0 in
  let pending, resolve = Lwt.wait () in
  let verify _ =
    incr calls;
    pending
  in
  let first = run_preverify cache ~state_root:"state-a" [item] verify in
  Lwt.cancel first;
  let second = run_preverify cache ~state_root:"state-a" [item] verify in
  Lwt.wakeup_later resolve (ready_batch [item]);
  let batch = Lwt_main.run second in
  expect "cancelled waiter preserved job" (!calls = 1);
  expect "second waiter completed"
    (Octra_core.Preverify_worker.txs batch = [item])

let test_check_retry () =
  let cache = C.create ~cap:4 in
  let item = tx 24 in
  let calls = ref 0 in
  let verify txs =
    incr calls;
    if !calls = 1 then Lwt.fail_with "expected failure"
    else Lwt.return (ready_batch txs)
  in
  begin
    try
      run_preverify cache ~state_root:"state-a" [item] verify
      |> Lwt_main.run
      |> ignore;
      fail "preverify failure accepted"
    with Failure reason ->
      expect "preverify failure reason" (reason = "expected failure")
  end;
  let batch =
    run_preverify cache ~state_root:"state-a" [item] verify
    |> Lwt_main.run
  in
  expect "failed job retried" (!calls = 2);
  expect "retry completed"
    (Octra_core.Preverify_worker.txs batch = [item])

let test_check_defer () =
  let cache = C.create ~cap:4 in
  let item = tx 28 in
  let calls = ref 0 in
  let verify txs =
    incr calls;
    if !calls = 1 then
      Lwt.return
        Octra_core.Preverify_worker.{
          ready = [];
          skipped = [{ tx = item; reason = "pending"; kind = Deferred }];
        }
    else
      Lwt.return (ready_batch txs)
  in
  let first =
    run_preverify cache ~state_root:"state-a" [item] verify
    |> Lwt_main.run
  in
  expect "deferred item omitted"
    (Octra_core.Preverify_worker.txs first = []);
  let second =
    run_preverify cache ~state_root:"state-a" [item] verify
    |> Lwt_main.run
  in
  expect "deferred item retried" (!calls = 2);
  expect "retried item ready"
    (Octra_core.Preverify_worker.txs second = [item])

let test_check_roles () =
  let cache = C.create ~cap:4 in
  let item = tx 29 in
  let build_calls = ref 0 in
  let validate_calls = ref 0 in
  let build_verify txs =
    incr build_calls;
    Lwt.return (ready_batch txs)
  in
  let validate_verify txs =
    incr validate_calls;
    Lwt.return (ready_batch txs)
  in
  run_preverify_for cache
    ~purpose:C.Build_proposal
    ~state_root:"state-a"
    [item]
    build_verify
  |> Lwt_main.run
  |> ignore;
  run_preverify_for cache
    ~purpose:C.Validate_proposal
    ~state_root:"state-a"
    [item]
    validate_verify
  |> Lwt_main.run
  |> ignore;
  expect "builder job ran once" (!build_calls = 1);
  expect "validator used its own role" (!validate_calls = 1)

let test_circle_isolation () =
  let cache = C.create ~cap:4 in
  let circle = tx ~op_type:Transaction.CircleCall 25 in
  let light = tx 26 in
  let calls = Hashtbl.create 2 in
  let verify txs =
    List.iter
      (fun tx ->
        let hash = Transaction.hash tx in
        Hashtbl.replace calls hash true)
      txs;
    Lwt.return (ready_batch txs)
  in
  let batch =
    run_preverify cache ~state_root:"state-a" [circle; light] verify
    |> Lwt_main.run
  in
  expect "circle isolated"
    (Octra_core.Preverify_worker.txs batch = [circle]);
  expect "light verification skipped"
    (not (Hashtbl.mem calls (Transaction.hash light)));
  match batch.Octra_core.Preverify_worker.skipped with
  | [item] ->
    expect "circle isolation reason"
      (item.reason = "circle_receipt_snapshot_isolation")
  | _ -> fail "circle isolation skip missing"

let test_check_hash () =
  let cache = C.create ~cap:4 in
  let item = tx 27 in
  let task =
    C.run_preverify_once
      cache
      ~purpose:C.Build_proposal
      ~state_root:"state-a"
      ~tx_hashes:["wrong"]
      ~txs:[item]
      (fun _ txs -> Lwt.return (ready_batch txs))
  in
  try
    Lwt_main.run task |> ignore;
    fail "preverify hash mismatch accepted"
  with Failure reason ->
    expect "preverify hash mismatch reason"
      (reason = "consensus preverify hash mismatch")

let small_cache ~check_limit =
  C.create_with_limits ~cap:4096 ~check_limit
    ~shared_cap:2 ~shared_limit:4096

let test_text_sharing () =
  let cache = C.create ~cap:2 in
  let item = tx ~message:(String.make 131_072 'x') 7 in
  let save cache pid value =
    ignore (C.store cache ~pid ~tx_hashes:[Transaction.hash value]
      ~txs:[value] ~receipts_json:[String.make 131_072 'r'])
  in
  let observe () =
    save cache "first" item;
    save cache "second" item;
    let first = Option.get (C.peek_raw cache "first") in
    let second = Option.get (C.peek_raw cache "second") in
    expect "shared bundle bytes unchanged" (first = second);
    let _, first_txs, first_receipts = first in
    let _, second_txs, second_receipts = second in
    expect "transaction text shared" (List.hd first_txs == List.hd second_txs);
    expect "receipt text shared" (List.hd first_receipts == List.hd second_receipts);
    let other = C.create ~cap:2 in
    save other "first" item;
    let _, other_txs, _ = Option.get (C.peek_raw other "first") in
    expect "text owner isolated" (List.hd first_txs != List.hd other_txs);
    let weak = Weak.create 1 in
    Weak.set weak 0 (Some (List.hd first_txs));
    weak
  in
  let weak = observe () in
  save cache "third" (tx 8);
  save cache "fourth" (tx 9);
  Gc.full_major ();
  expect "unreferenced text released" (Weak.get weak 0 = None);
  expect "text eviction policy retained" ((C.stats cache).evictions = 2);
  expect "current text retained" (Option.is_some (C.peek_raw cache "fourth"))

let test_cache_bytes () =
  let cache = C.create ~cap:2 in
  let put pid size =
    C.store cache ~pid ~tx_hashes:[] ~txs:[]
      ~receipts_json:[String.make size 'a'] |> ignore
  in
  put "first" 1024;
  let first = (C.stats cache).cache_bytes in
  put "first" 1024;
  expect "replacement accounting" ((C.stats cache).cache_bytes = first);
  put "second" 16384;
  expect "both bundles retained" ((C.stats cache).cache_size = 2);
  expect "bytes accounted" ((C.stats cache).cache_bytes > first + 16384);
  put "third" 1024;
  expect "count eviction retained" (C.peek_raw cache "first" = None);
  expect "large bundle available" (Option.is_some (C.peek_raw cache "second"));
  expect "fifo tracks live entries"
    ((C.stats cache).fifo_size = (C.stats cache).cache_size)

let test_check_bytes () =
  let cache = small_cache ~check_limit:8192 in
  let calls = ref 0 in
  let verify items = incr calls; Lwt.return (ready_batch items) in
  let run item =
    run_preverify cache ~state_root:"root" [item] verify |> Lwt_main.run
  in
  for index = 1 to 100 do
    let item = tx ~message:(String.make 1024 'b') index in
    let result = run item in
    expect "result preserved" (Octra_core.Preverify_worker.txs result = [item]);
    expect "check byte budget" ((C.stats cache).preverify_bytes <= 8192)
  done;
  let last = tx ~message:(String.make 1024 'b') 100 in
  ignore (run last);
  expect "latest check reused" (!calls = 100);
  ignore (run (tx ~message:(String.make 1024 'b') 1));
  expect "evicted check recomputed" (!calls = 101);
  let item = tx ~message:(String.make 16384 'c') 101 in
  let result = run item in
  expect "large check returned" (Octra_core.Preverify_worker.txs result = [item]);
  expect "large check not retained" ((C.stats cache).preverify_bytes <= 8192)

let test_check_root () =
  let cache = small_cache ~check_limit:8192 in
  let old = Weak.create 1 in
  let add_old () =
    let item = tx ~message:(String.make 1024 'd') 102 in
    Weak.set old 0 (Some item);
    run_preverify cache ~state_root:"old" [item]
      (fun items -> Lwt.return (ready_batch items)) |> Lwt_main.run |> ignore
  in
  add_old ();
  Gc.full_major ();
  expect "current result owned" (Weak.check old 0);
  run_preverify cache ~state_root:"new" []
    (fun items -> Lwt.return (ready_batch items)) |> Lwt_main.run |> ignore;
  Gc.full_major ();
  expect "old result released" (not (Weak.check old 0));
  expect "old bytes released" ((C.stats cache).preverify_bytes = 0);
  expect "old queue released" ((C.stats cache).preverify_queue = 0)

let test_check_late () =
  let cache = small_cache ~check_limit:8192 in
  let item = tx 103 in
  let second = tx 104 in
  let job, resolve = Lwt.wait () in
  let old = run_preverify cache ~state_root:"old" [item; second]
    (function
      | [entry] when entry = item -> job
      | items -> Lwt.return (ready_batch items)) in
  run_preverify cache ~state_root:"new" [item]
    (fun items -> Lwt.return (ready_batch items)) |> Lwt_main.run |> ignore;
  let before = C.stats cache in
  Lwt.wakeup_later resolve (ready_batch [item]);
  let result = Lwt_main.run old in
  expect "old waiter completed" (Octra_core.Preverify_worker.txs result = [item; second]);
  expect "late result did not reenter" ((C.stats cache).preverify_size = 1);
  expect "late result accounting" ((C.stats cache).preverify_bytes = before.preverify_bytes)

let test_check_pending () =
  let cache = small_cache ~check_limit:1024 in
  let item = tx 105 in
  let job, resolve = Lwt.task () in
  let calls = ref 0 in
  let verify _ = incr calls; job in
  let first = run_preverify cache ~state_root:"root" [item] verify in
  run_preverify cache ~state_root:"root" [tx ~message:(String.make 2048 'e') 106]
    (fun items -> Lwt.return (ready_batch items)) |> Lwt_main.run |> ignore;
  Lwt.cancel first;
  expect "shared task not cancelled" (Lwt.is_sleeping job);
  let second = run_preverify cache ~state_root:"root" [item] verify in
  expect "pending not duplicated" (!calls = 1);
  Lwt.wakeup_later resolve (ready_batch [item]);
  let result = Lwt_main.run second in
  expect "pending returned" (Octra_core.Preverify_worker.txs result = [item]);
  expect "pending completion budget" ((C.stats cache).preverify_bytes <= 1024)

let test_check_aba () =
  List.iter (fun outcome ->
    let cache = small_cache ~check_limit:8192 in
    let item = tx 107 in
    let job, resolve = Lwt.task () in
    let old = run_preverify cache ~state_root:"a" [item] (fun _ -> job) in
    let observed = Lwt.catch
      (fun () -> Lwt.map (fun _ -> ()) old)
      (fun _ -> Lwt.return_unit) in
    run_preverify cache ~state_root:"b" []
      (fun items -> Lwt.return (ready_batch items)) |> Lwt_main.run |> ignore;
    let calls = ref 0 in
    let verify items = incr calls; Lwt.return (ready_batch items) in
    run_preverify cache ~state_root:"a" [item] verify |> Lwt_main.run |> ignore;
    let before = C.stats cache in
    begin match outcome with
    | `Failure -> Lwt.wakeup_later_exn resolve (Failure "expected failure")
    | `Deferred ->
      Lwt.wakeup_later resolve Octra_core.Preverify_worker.{
        ready = [];
        skipped = [{ tx = item; reason = "pending"; kind = Deferred }];
      }
    | `Ready -> Lwt.wakeup_later resolve (ready_batch [item])
    end;
    Lwt_main.run observed;
    expect "aba retained count" ((C.stats cache).preverify_size = before.preverify_size);
    expect "aba retained bytes" ((C.stats cache).preverify_bytes = before.preverify_bytes);
    run_preverify cache ~state_root:"a" [item] verify |> Lwt_main.run |> ignore;
    expect "aba retained result" (!calls = 1)) [`Failure; `Deferred; `Ready]

let () =
  test_text_sharing ();
  test_cache_bytes ();
  test_check_bytes ();
  test_check_root ();
  test_check_late ();
  test_check_pending ();
  test_check_aba ();
  test_store_cached_and_stats ();
  test_peek_does_not_count ();
  test_lookup_counts_and_evicts ();
  test_shared_count ();
  test_shared_txs_byte_limit ();
  test_decode_and_parse_txs ();
  test_oversized_bundle_rejected ();
  test_summary_tick ();
  test_log_empty ();
  test_node_runtime ();
  test_frozen_prune ();
  test_check_reuse ();
  test_check_cancel ();
  test_check_retry ();
  test_check_defer ();
  test_check_roles ();
  test_circle_isolation ();
  test_check_hash ();
  print_endline "status = pass test = bundle_cache"