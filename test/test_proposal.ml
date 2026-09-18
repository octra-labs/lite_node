(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_node_runtime.Consensus_proposal
module Transaction = Octra_core.Transaction
module C_hash = Octra_consensus.C_hash
module C_driver = Octra_consensus.C_driver
module C_protocol = Octra_consensus.C_protocol
module C_types = Octra_consensus.C_types
module Cache = Octra_node_runtime.Consensus_bundle_cache
module F = Octra_node_runtime.Consensus_bundle_fetch
module W = Octra_core.Preverify_worker

let fail msg =
  failwith ("test_node_runtime_consensus_proposal: " ^ msg)

let verdict_accepts = function
  | C_driver.Proposal_accept -> true
  | C_driver.Proposal_wait
  | C_driver.Proposal_reject -> false

let verdict_rejects = function
  | C_driver.Proposal_reject -> true
  | C_driver.Proposal_accept
  | C_driver.Proposal_wait -> false

let verdict_waits = function
  | C_driver.Proposal_wait -> true
  | C_driver.Proposal_accept
  | C_driver.Proposal_reject -> false

let tx ?message ?(ou = 1_000) nonce =
  Transaction.{
    from = "oct_sender";
    to_ = "oct_receiver";
    amount = Z.zero;
    nonce;
    ou = Z.of_int ou;
    timestamp = 1.0;
    signature = "sig";
    public_key = Some "pub";
    message;
    op_type = Standard;
    encrypted_data = None;
  }

let expect label cond =
  if not cond then fail label

let with_protocol_activation value run =
  let prior = Sys.getenv_opt C_protocol.activation_env in
  Unix.putenv C_protocol.activation_env value;
  Fun.protect
    run
    ~finally:(fun () ->
      Unix.putenv
        C_protocol.activation_env
        (Option.value ~default:"" prior))

let raw ch =
  String.make 32 ch

let header ?(receipts_json = []) () =
  C_types.{
    proto_version = C_types.proto_version_current;
    chain_id = "octra-test";
    epoch_id = 10L;
    prev_state_root = raw 'p';
    tx_list_hash = raw 't';
    receipt_root = C_hash.receipt_root receipts_json;
    proposed_state_root = raw 's';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "oct_creator";
    txid_hi = 0L;
    ts = 1.0;
  }

let verify_deps ?(address_ok = true) ?(signature_ok = true) () =
  C.{
    public_key_for_tx = (fun tx -> tx.Transaction.public_key);
    verify_address_pubkey = (fun ~addr:_ ~pubkey:_ -> address_ok);
    verify_tx_signature = (fun _ ~pubkey:_ -> signature_ok);
  }

let generous_limits =
  C.limits
    ~max_txs:100
    ~max_bytes:1_000_000
    ~max_ou:(Z.of_int 1_000_000)

let test_totals () =
  let txs = [tx 1; tx ~message:"payload" ~ou:2_000 2] in
  let totals = C.totals txs in
  expect "count" (totals.C.count = 2);
  expect "bytes" (totals.C.bytes = C.wire_size (List.nth txs 0) + C.wire_size (List.nth txs 1));
  expect "ou" (Z.equal totals.C.ou (Z.of_int 2_000))

let test_within_limits () =
  let txs = [tx 1; tx ~message:(String.make 2_000 'x') 2] in
  let totals = C.totals txs in
  let ok =
    C.limits
      ~max_txs:2
      ~max_bytes:totals.C.bytes
      ~max_ou:totals.C.ou
  in
  expect "within exact limits" (C.within_limits ~limits:ok txs);
  expect "count rejects"
    (not (C.within_limits ~limits:(C.limits ~max_txs:1 ~max_bytes:totals.C.bytes ~max_ou:totals.C.ou) txs));
  expect "bytes rejects"
    (not (C.within_limits ~limits:(C.limits ~max_txs:2 ~max_bytes:(totals.C.bytes - 1) ~max_ou:totals.C.ou) txs));
  expect "ou rejects"
    (not (C.within_limits ~limits:(C.limits ~max_txs:2 ~max_bytes:totals.C.bytes ~max_ou:(Z.pred totals.C.ou)) txs))

let test_cap_count () =
  let txs = [tx 1; tx 2; tx 3] in
  let capped = C.cap ~limits:(C.limits ~max_txs:2 ~max_bytes:10_000 ~max_ou:(Z.of_int 10_000)) txs in
  expect "cap count len" (List.length capped.C.txs = 2);
  expect "cap count skipped" (capped.C.skipped = 1);
  expect "cap count totals" (capped.C.totals.C.count = 2)

let test_cap_bytes () =
  let first = tx 1 in
  let second = tx ~message:(String.make 2_000 'x') 2 in
  let limit =
    C.limits
      ~max_txs:10
      ~max_bytes:(C.wire_size first)
      ~max_ou:(Z.of_int 10_000)
  in
  let capped = C.cap ~limits:limit [first; second] in
  expect "cap bytes len" (List.length capped.C.txs = 1);
  expect "cap bytes skipped" (capped.C.skipped = 1);
  expect "cap bytes total" (capped.C.totals.C.bytes = C.wire_size first)

let test_cap_ou () =
  let first = tx ~ou:1_000 1 in
  let second = tx ~ou:2_000 2 in
  let capped =
    C.cap
      ~limits:(C.limits ~max_txs:10 ~max_bytes:10_000 ~max_ou:(Z.of_int 1_000))
      [first; second]
  in
  expect "cap ou len" (List.length capped.C.txs = 1);
  expect "cap ou skipped" (capped.C.skipped = 1);
  expect "cap ou total" (Z.equal capped.C.totals.C.ou (Z.of_int 1_000))

let test_select_staged_hashes_once () =
  let staged = [tx 1; tx 2; tx 3; tx 4] in
  let calls = ref 0 in
  let hash_tx item =
    incr calls;
    string_of_int item.Transaction.nonce
  in
  let selected =
    C.select_staged
      ~hash_tx
      ~hashes:["3"; "1"; "missing"; "1"]
      staged
  in
  expect "staging hashes once" (!calls = List.length staged);
  expect "staging proposal order"
    (List.map (fun item -> item.Transaction.nonce) selected = [3; 1; 1])

let fake_batch ?(skipped = []) items =
  let ready =
    List.map (fun item -> { W.tx = item; receipt = None }) items
  in
  Lwt.return ({ W.ready; skipped } : W.batch)

let test_local_preverify_ready () =
  let txs = [tx 1; tx 2] in
  let shaped =
    Lwt_main.run
      (C.local_preverify_bundle
         ~run_many:fake_batch
         ~tx_hashes:(List.map Transaction.hash txs)
         txs)
  in
  expect "local ready txs" (shaped.C.ready_txs = txs);
  expect "local no receipts" (shaped.receipts_json = []);
  expect "local skipped count" (shaped.skipped_count = 0)

let test_local_preverify_disabled () =
  let worker_called = ref false in
  let disabled =
    { (tx 1) with Transaction.op_type = Transaction.PrivateOp }
  in
  let shaped =
    Lwt_main.run
      (C.local_preverify_bundle
         ~run_many:(fun _ ->
           worker_called := true;
           fake_batch [])
         ~tx_hashes:[Transaction.hash disabled]
         [disabled])
  in
  expect "disabled preverify worker bypassed" (not !worker_called);
  expect "disabled preverify no txs" (shaped.C.ready_txs = []);
  expect "disabled preverify skipped" (shaped.skipped_count = 1)

let local_shape ?(receipts_json = []) ready_txs =
  C.{
    batch = {
      W.ready =
        List.map (fun item -> { W.tx = item; receipt = None }) ready_txs;
      skipped = [];
    };
    ready_txs;
    receipts_json;
    skipped_count = 0;
    skipped_sample = "";
  }

let received ?(receipts_json = []) txs =
  F.{ txs; receipts_json; rejections = [] }

let expect_local_error label expected value =
  match value with
  | Error reason -> expect label (reason = expected)
  | Ok _ -> fail (label ^ " accepted")

let test_check_local_bundle () =
  let txs = [tx 1; tx 2] in
  let hashes = List.map Transaction.hash txs in
  begin
    match C.check_local_bundle ~expected_hashes:hashes (received txs) (local_shape txs) with
    | Ok _ -> ()
    | Error reason -> fail ("local bundle rejected " ^ reason)
  end;
  expect_local_error
    "received hash mismatch"
    "received_tx_hash_mismatch"
    (C.check_local_bundle
       ~expected_hashes:hashes
       (received [List.hd txs])
       (local_shape txs));
  expect_local_error
    "local tx mismatch"
    "local_preverify_tx_mismatch"
    (C.check_local_bundle
       ~expected_hashes:hashes
       (received txs)
       (local_shape [List.hd txs]));
  expect_local_error
    "local receipt mismatch"
    "local_preverify_receipt_mismatch"
    (C.check_local_bundle
       ~expected_hashes:hashes
       (received ~receipts_json:["{}"] txs)
       (local_shape ~receipts_json:["[]"] txs))

let test_validator_preverify_wait () =
  let cache = Cache.create ~cap:4 in
  let item = tx 30 in
  let hashes = [Transaction.hash item] in
  let build_calls = ref 0 in
  let validate_calls = ref 0 in
  let validation, resolve_validation = Lwt.wait () in
  let build_batch =
    Cache.run_preverify_once
      cache
      ~purpose:Cache.Build_proposal
      ~state_root:(raw 'p')
      ~tx_hashes:hashes
      ~txs:[item]
      (fun _ txs ->
         incr build_calls;
         fake_batch txs)
    |> Lwt_main.run
  in
  expect "builder prepared transaction"
    (W.txs build_batch = [item]);
  let validate_many txs =
    Cache.run_preverify_once
      cache
      ~purpose:Cache.Validate_proposal
      ~state_root:(raw 'p')
      ~tx_hashes:hashes
      ~txs
      (fun _ _ ->
         incr validate_calls;
         validation)
  in
  let local =
    C.local_preverify_bundle
      ~run_many:validate_many
      ~tx_hashes:hashes
      [item]
  in
  expect "validator remains attached to running verification"
    (Lwt.is_sleeping local);
  expect "roles do not share the builder job"
    (!build_calls = 1 && !validate_calls = 1);
  Lwt.wakeup resolve_validation
    W.{ ready = [{ tx = item; receipt = None }]; skipped = [] };
  let local = Lwt_main.run local in
  match C.check_local_bundle
          ~expected_hashes:hashes
          (received [item])
          local with
  | Ok _ -> ()
  | Error reason -> fail ("validator rejected prepared proposal " ^ reason)

let cached_check cache item verify =
  Cache.run_preverify_once
    cache
    ~purpose:Cache.Validate_proposal
    ~state_root:(raw 'p')
    ~tx_hashes:[Transaction.hash item]
    ~txs:[item]
    (fun _ _ -> verify ())

let test_cache_retry_order () =
  let cache = Cache.create ~cap:2 in
  let first = tx 31 in
  let calls = ref 0 in
  let verify () =
    incr calls;
    if !calls = 1 then
      fake_batch
        ~skipped:[{ W.tx = first; reason = "pending"; kind = W.Deferred }]
        []
    else fake_batch [first]
  in
  let run item verify = Lwt_main.run (cached_check cache item verify) in
  let _ = run first verify in
  let _ = run (tx 32) (fun () -> fake_batch [tx 32]) in
  let _ = run first verify in
  let _ = run (tx 33) (fun () -> fake_batch [tx 33]) in
  let batch = run first verify in
  expect "retry kept its insertion order" (!calls = 2);
  expect "retry kept its result" (W.txs batch = [first])

let test_cache_retry_space () =
  List.iter
    (fun cap ->
      let cache = Cache.create ~cap in
      let item = tx 34 in
      List.iter
        (fun index ->
          let result =
            Lwt.catch
              (fun () ->
                let open Lwt.Syntax in
                let* _ = cached_check cache item (fun () ->
                  match index mod 3 with
                  | 0 -> failwith "retry"
                  | 1 -> Lwt.fail_with "retry"
                  | _ ->
                    fake_batch
                      ~skipped:[{ W.tx = item; reason = "pending"; kind = W.Deferred }]
                      [])
                in
                Lwt.return_unit)
              (function
                | Failure reason when reason = "retry" -> Lwt.return_unit
                | exn -> Lwt.fail exn)
          in
          Lwt_main.run result;
          let stats = Cache.stats cache in
          expect "retry removed its result" (stats.preverify_size = 0);
          expect "retry queue respects capacity" (stats.preverify_queue <= max 0 cap))
        (List.init 100 Fun.id))
    [-1; 0; 1; 2; 4]

let test_cache_cancel_job () =
  let cache = Cache.create ~cap:2 in
  let item = tx 35 in
  let job, wake = Lwt.task () in
  let calls = ref 0 in
  let verify () = incr calls; job in
  let first = cached_check cache item verify in
  let second = cached_check cache item verify in
  Lwt.cancel first;
  expect "cancelled caller detached" (Lwt.state first = Lwt.Fail Lwt.Canceled);
  expect "shared job still pending" (Lwt.is_sleeping job);
  Lwt.wakeup wake W.{ ready = [{ tx = item; receipt = None }]; skipped = [] };
  let result = Lwt_main.run second in
  expect "shared job ran once" (!calls = 1);
  expect "remaining caller completed" (W.txs result = [item])

let test_cache_late_failure () =
  let cache = Cache.create ~cap:1 in
  let item = tx 36 in
  let old, wake = Lwt.task () in
  let first = cached_check cache item (fun () -> old) in
  let _ = Lwt_main.run (cached_check cache (tx 37) (fun () -> fake_batch [tx 37])) in
  let calls = ref 0 in
  let verify () = incr calls; fake_batch [item] in
  let _ = Lwt_main.run (cached_check cache item verify) in
  Lwt.wakeup_exn wake (Failure "retry");
  begin
    match Lwt.state first with
    | Lwt.Fail (Failure reason) when reason = "retry" -> ()
    | _ -> fail "old caller lost its failure"
  end;
  let result = Lwt_main.run (cached_check cache item verify) in
  expect "old failure kept newer result" (!calls = 1);
  expect "newer result complete" (W.txs result = [item])

let test_cache_result_parity () =
  let items = List.init 64 (fun index -> tx (40 + index mod 7)) in
  let run cap =
    let cache = Cache.create ~cap in
    List.map
      (fun item ->
        let batch = Lwt_main.run (cached_check cache item (fun () -> fake_batch [item])) in
        let stats = Cache.stats cache in
        expect "result capacity" (stats.preverify_size <= max 0 cap);
        expect "queue capacity" (stats.preverify_queue <= max 0 cap);
        batch)
      items
  in
  let expected = run 0 in
  List.iter
    (fun cap -> expect "cache removal preserves result" (run cap = expected))
    [1; 2; 7; 64]

let test_reject_forged_heavy_receipt () =
  let base = tx 1 in
  let heavy = { base with Transaction.op_type = Transaction.DecryptOp } in
  let hash = Transaction.hash heavy in
  let receipt =
    match Octra_core.Preverify_receipt.for_tx
      ~input_hash:(W.input_hash heavy)
      ~output_hash:(W.output_hash heavy "ok")
      ~ok:true
      ~reason:""
      heavy with
    | Ok value -> Octra_core.Preverify_receipt.canonical value
    | Error reason -> fail ("forged receipt sample " ^ reason)
  in
  expect_local_error
    "forged heavy receipt"
    "local_preverify_tx_mismatch"
    (C.check_local_bundle
       ~expected_hashes:[hash]
       (received ~receipts_json:[receipt] [heavy])
       C.{
         batch = { W.ready = []; skipped = [] };
         ready_txs = [];
         receipts_json = [];
         skipped_count = 1;
         skipped_sample = "proof_failed";
       })

let test_preverify_cap_skip_sample () =
  let txs = [tx 1; tx 2; tx 3] in
  let skipped = [{ W.tx = List.nth txs 2; reason = "heavy"; kind = W.Invalid }] in
  let shaped =
    Lwt_main.run
      (C.build_preverify
         ~run_many:(fun txs -> fake_batch ~skipped txs)
         ~limits:(C.limits ~max_txs:2 ~max_bytes:1_000_000 ~max_ou:(Z.of_int 1_000_000))
         txs)
  in
  expect "build cap txs" (List.length shaped.C.txs = 2);
  expect "build cap skipped" (shaped.capped.C.skipped = 1);
  expect "build hashes" (shaped.tx_hashes = List.map Transaction.hash shaped.txs);
  expect "build skipped count" (shaped.skipped_count = 1);
  expect "build skipped sample"
    (String.ends_with ~suffix:":heavy" shaped.skipped_sample)

let test_layera_validator_addrs () =
  expect "layera fallback"
    (C.layera_validator_addrs ~env:None ~fallback:"oct_fallback" = ["oct_fallback"]);
  expect "layera env parsed sorted"
    (C.layera_validator_addrs
       ~env:(Some "octB:1,bad,octA:2")
       ~fallback:"oct_fallback" = ["octA"; "octB"]);
  expect "layera invalid env stays empty"
    (C.layera_validator_addrs ~env:(Some "bad") ~fallback:"oct_fallback" = [])

let test_layera_diag_context () =
  let ctx =
    C.layera_diag_context
      ~validator_addrs:["octA"; "octB"]
      ~hash_validators:(fun payload -> "hash:" ^ payload)
      ~meta:(fun key -> "meta:" ^ key)
  in
  expect "layera ctx addrs" (ctx.C.validator_addrs = ["octA"; "octB"]);
  expect "layera ctx sha" (ctx.validators_sha = "hash:octA,octB");
  expect "layera ctx current" (ctx.meta_current = "meta:current_epoch");
  expect "layera ctx last" (ctx.meta_last = "meta:last_epoch");
  expect "layera ctx supply" (ctx.meta_supply = "meta:total_supply");
  expect "layera ctx emission" (ctx.meta_emission = "meta:emission_remaining")

let test_layera_env_diag_context () =
  let ctx =
    C.layera_env_diag_context
      ~env:(Some "octB:pub,octA:pub")
      ~fallback:"octFallback"
      ~hash_validators:(fun payload -> "hash:" ^ payload)
      ~meta:(fun key -> "meta:" ^ key)
  in
  expect "layera env addrs" (ctx.C.validator_addrs = ["octA"; "octB"]);
  expect "layera env sha" (ctx.validators_sha = "hash:octA,octB")

let test_validator_pubkeys_fallback () =
  let calls = ref 0 in
  let pubkeys =
    C.validator_pubkeys
      ~driver:None
      ~fallback:(fun () ->
        incr calls;
        ["octA", "pubA"; "octB", "pubB"])
  in
  expect "validator fallback called" (!calls = 1);
  expect "validator fallback pubkeys" (pubkeys = ["octA", "pubA"; "octB", "pubB"])

let test_preview_exec_env () =
  let env =
    C.epoch_exec_env
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~epoch_ts:120.0
      ~proposer:"octP"
      ~validator_pubkeys:["octA", "pubA"; "octB", "pubB"]
      ~prev_state_root:(raw 'r')
      ~ready_state_root_at:(fun epoch ->
        Lwt.return_some (Printf.sprintf "root-%d" epoch))
      ~ready_max_lag:7
  in
  expect "env chain" (env.Octra_core.Epoch_exec.chain_id = "octra-test");
  expect "env epoch" (env.epoch_id = 12);
  expect "env proposer" (env.proposer_addr = "octP");
  expect "env validator addrs" (env.validator_addrs = ["octA"; "octB"]);
  expect "env validator pubkeys" (env.validator_pubkeys = ["octA", "pubA"; "octB", "pubB"]);
  expect "env prev root" (env.prev_state_root = raw 'r');
  expect "env ts" (env.epoch_ts = 120.0);
  expect "env lag" (env.ready_max_lag = 7);
  match env.ready_state_root_at with
  | None -> fail "env readiness callback"
  | Some fn ->
    expect "env readiness callback"
      (Lwt_main.run (fn 9) = Some "root-9")

let test_admission_plan () =
  let plan ?(state_attested = true) ?(quarantine_active = false)
      ?(quarantine_reason = "") ~epoch_id ~current_epoch () =
    C.admission_plan
      ~epoch_id:(Int64.of_int epoch_id)
      ~current_epoch
      ~state_attested
      ~quarantine_active
      ~quarantine_reason
  in
  (match plan ~state_attested:false ~epoch_id:10 ~current_epoch:10 () with
   | C.Defer_state_not_attested -> ()
   | _ -> fail "state not attested plan");
  (match plan
           ~quarantine_active:true
           ~quarantine_reason:"local root mismatch"
           ~epoch_id:10
           ~current_epoch:10
           () with
   | C.Defer_quarantine { reason } ->
     expect "quarantine reason" (reason = "local root mismatch")
   | _ -> fail "quarantine plan");
  (match plan ~epoch_id:9 ~current_epoch:10 () with
   | C.Realign_stale_height { target_epoch } ->
     expect "realign target" (target_epoch = 10L)
   | _ -> fail "realign stale plan");
  (match plan ~epoch_id:11 ~current_epoch:10 () with
   | C.Defer_apply_gap -> ()
   | _ -> fail "apply gap plan");
  (match plan ~epoch_id:10 ~current_epoch:10 () with
   | C.Proceed -> ()
   | _ -> fail "proceed plan")

let test_admission_proceed () =
  let started = ref [] in
  let result =
    Lwt_main.run
      (C.handle_proposal_admission
         ~start_height:(fun height ->
           started := height :: !started;
           Lwt.return_unit)
         ~epoch_id:10L
         ~current_epoch:10
         ~state_attested:true
         ~quarantine_active:false
         ~quarantine_reason:"")
  in
  expect "admission shell proceed" (result = C.Proposal_admit);
  expect "admission shell proceed no start" (!started = [])

let test_admission_realign () =
  let started = ref [] in
  let result =
    Lwt_main.run
      (C.handle_proposal_admission
         ~start_height:(fun height ->
           started := height :: !started;
           Lwt.return_unit)
         ~epoch_id:9L
         ~current_epoch:10
         ~state_attested:true
         ~quarantine_active:false
         ~quarantine_reason:"")
  in
  expect "admission shell realign defer" (result = C.Proposal_defer);
  expect "admission shell realign start" (!started = [10L])

let test_admission_quarantine () =
  let started = ref [] in
  let result =
    Lwt_main.run
      (C.handle_proposal_admission
         ~start_height:(fun height ->
           started := height :: !started;
           Lwt.return_unit)
         ~epoch_id:10L
         ~current_epoch:10
         ~state_attested:true
         ~quarantine_active:true
         ~quarantine_reason:"q")
  in
  expect "admission shell quarantine defer" (result = C.Proposal_defer);
  expect "admission shell quarantine no start" (!started = [])

type proposal_probe = {
  start_heights : int64 list ref;
  preview_requests : C.build_preview_request list ref;
  set_proposals : (Transaction.t list * string list) list ref;
  stored_bundles : (string * string list * Transaction.t list * string list) list ref;
  frozen_writes :
    (string * Octra_node_runtime.Consensus_bundle_cache.frozen) list ref;
}

let exec_result ~confirmed ~rejected ~post_state_root =
  Stdlib.Ok Octra_core.Epoch_exec.{
    post_state_root;
    artifacts = {
      confirmed = List.map (fun item -> item, 0) confirmed;
      rejected =
        List.map
          (fun item ->
            { tx = item; error_type = "bad"; reason = "rejected" })
          rejected;
      confirmed_fees = Z.zero;
      tx_count = List.length confirmed + List.length rejected;
    };
  }

let proposal_head state_root =
  Octra_core.Head_manifest.{
    schema_version = 3;
    generation = 1;
    epoch_id = 11;
    state_root;
    ledger_state_root = None;
    irmin_commit = None;
    txid_hi = 6L;
    txlog_seg = None;
    txlog_off = None;
    epochlog_off = None;
    commit_id = "commit";
    ts = 89.0;
    quorum_cert_hash = None;
    epoch_index_hash = None;
    epoch_index_root = None;
  }

let make_proposal_deps ?(state_attested = true) ?(quarantine_active = false)
    ?(current = fun () -> true)
    ?(current_epoch = 12) ?(round = 3) ?frozen ?(staging = [])
    ?(now = 99.0) ?(previous_epoch_ts = Some 89.0)
    ?(ledger_root = raw 'p') ?(cached_head = None)
    ?(admits = fun _ -> true)
    ?(build_preverify_once = fun ~state_root:_ ~tx_hashes:_ txs ->
      fake_batch txs)
    ?(preview_result = fun request ->
        exec_result
          ~confirmed:request.C.txs
          ~rejected:[]
          ~post_state_root:(raw 'l'))
    () =
  let start_heights = ref [] in
  let preview_requests = ref [] in
  let set_proposals = ref [] in
  let stored_bundles = ref [] in
  let frozen_writes = ref [] in
  let probe = {
    start_heights;
    preview_requests;
    set_proposals;
    stored_bundles;
    frozen_writes;
  } in
  let deps = C.{
    current;
    start_height = (fun height ->
      start_heights := height :: !start_heights;
      Lwt.return_unit);
    current_epoch = (fun () -> current_epoch);
    state_attested = (fun () -> state_attested);
    quarantine_active = (fun () -> quarantine_active);
    quarantine_reason = (fun () -> "test_quarantine");
    read_prev_ledger_root = (fun () -> Lwt.return_some ledger_root);
    cached_head = (fun () -> cached_head);
    current_round = (fun () -> round);
    parent_commit = (fun ~epoch_id:_ -> Ok None);
    frozen_bundle = (fun _ -> frozen);
    store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
      stored_bundles :=
        (proposal_id, tx_hashes, txs, receipts_json) :: !stored_bundles);
    staging_txs = (fun () -> staging);
    admits_tx = admits;
    build_preverify_once;
    staging_total = (fun () -> List.length staging);
    proposer = (fun () -> "oct_creator");
    validator_pubkeys = (fun _ -> ["oct_validator", "pub"]);
    preview = (fun request ->
      preview_requests := request :: !preview_requests;
      Lwt.return (preview_result request));
    prev_eic_root = (fun () -> Octra_core.Epoch_index_commitment.genesis_root);
    next_txid = (fun () -> 7L);
    set_proposal = (fun txs hashes ->
      set_proposals := (txs, hashes) :: !set_proposals);
    head_txid_hi = (fun () -> Some 6L);
    freeze = (fun key frozen ->
      frozen_writes := (key, frozen) :: !frozen_writes);
    now = (fun () -> now);
    previous_epoch_ts = (fun _ -> previous_epoch_ts);
  } in
  deps, probe

let run_make_proposal deps =
  Lwt_main.run
    (C.make_proposal
       deps
       ~chain_id:"octra-test"
       ~root_to_raw32:(fun root -> root)
       ~limits:generous_limits
       ~epoch_id:12L)

let test_build_head_progress () =
  List.iter (fun phase ->
    let current = ref true in
    let release, finish = Lwt.wait () in
    let entered = ref false in
    let pause run =
      entered := true;
      let open Lwt.Syntax in
      let* () = Lwt.protected release in
      run ()
    in
    let deps, probe = make_proposal_deps ~current:(fun () -> !current) ~staging:[tx 1] () in
    let deps = match phase with
      | `Root -> { deps with read_prev_ledger_root = (fun () -> pause deps.read_prev_ledger_root) }
      | `Checks -> { deps with build_preverify_once = (fun ~state_root ~tx_hashes txs ->
          pause (fun () -> deps.build_preverify_once ~state_root ~tx_hashes txs)) }
      | `Preview -> { deps with preview = (fun request -> pause (fun () -> deps.preview request)) }
    in
    let result = C.make_proposal deps ~chain_id:"octra-test"
      ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L
    in
    expect "build waiting" (!entered && Lwt.is_sleeping result);
    current := false;
    Lwt.wakeup_later finish ();
    expect "superseded build returns no plan" (Lwt_main.run result = None);
    expect "superseded build did not publish"
      (!(probe.set_proposals) = [] && !(probe.stored_bundles) = [] && !(probe.frozen_writes) = []))
    [`Root; `Checks; `Preview]

let test_preverify_single_flight () =
  let item = tx 1 in
  let calls = ref [] in
  let build_preverify_once ~state_root ~tx_hashes txs =
    calls := (state_root, tx_hashes) :: !calls;
    fake_batch txs
  in
  let deps, _ =
    make_proposal_deps
      ~staging:[item]
      ~ledger_root:(raw 'l')
      ~cached_head:(Some (proposal_head (raw 'p')))
      ~build_preverify_once
      ()
  in
  ignore (run_make_proposal deps);
  match !calls with
  | [(state_root, tx_hashes)] ->
    expect "proposal preverify state key" (state_root = raw 'l');
    expect "proposal preverify tx key"
      (tx_hashes = [Transaction.hash item])
  | _ ->
    failwith "proposal preverify did not use single-flight path"

let test_make_unattested_defer () =
  let deps, probe =
    make_proposal_deps ~state_attested:false ~staging:[tx 1] ()
  in
  expect "make proposal defer" (run_make_proposal deps = None);
  expect "make proposal defer no preview" (!(probe.preview_requests) = []);
  expect "make proposal defer no store" (!(probe.stored_bundles) = [])

let test_make_epoch_time_defer () =
  let preverify_calls = ref 0 in
  let deps, probe =
    make_proposal_deps
      ~now:94.0
      ~previous_epoch_ts:(Some 89.0)
      ~staging:[tx 1]
      ~build_preverify_once:(fun ~state_root:_ ~tx_hashes:_ txs ->
        incr preverify_calls;
        fake_batch txs)
      ()
  in
  expect "make proposal time defer" (run_make_proposal deps = None);
  expect "make proposal time no preverify" (!preverify_calls = 0);
  expect "make proposal time no preview" (!(probe.preview_requests) = []);
  expect "make proposal time no store" (!(probe.stored_bundles) = [])

let test_make_reuse_frozen_bundle () =
  let final_txs = [tx 1] in
  let final_hashes = List.map Transaction.hash final_txs in
  let envelope =
    C.build_proposal_envelope
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~prev_state_root:(raw 'p')
      ~final_hashes
      ~final_txs
      ~receipts_json:["receipt"]
      ~proposed_state_root:(raw 's')
      ~creator_addr:"oct_creator"
      ~next_txid:7L
      ~head_txid_hi:(Some 6L)
      ~parent_commit:None
      ~ts:99.0
  in
  let deps, probe =
    make_proposal_deps
      ~frozen:envelope.C.frozen_bundle
      ~staging:[tx 2]
      ()
  in
  (match run_make_proposal deps with
   | Some plan ->
     expect "make frozen header" (plan.header = envelope.header);
     expect "make frozen hashes" (plan.tx_hashes = final_hashes);
     expect "make frozen parent" (plan.parent_commit = None)
   | None ->
     fail "make frozen result");
  expect "make frozen no preview" (!(probe.preview_requests) = []);
  expect "make frozen stored"
    (!(probe.stored_bundles) =
     [envelope.proposal_id, final_hashes, final_txs, ["receipt"]]);
  expect "make frozen no freeze write" (!(probe.frozen_writes) = [])

let test_make_preview_rejections () =
  let confirmed = tx 1 in
  let rejected = tx 2 in
  let confirmed_hash = Transaction.hash confirmed in
  let preview_count = ref 0 in
  let deps, probe =
    make_proposal_deps
      ~staging:[confirmed; rejected]
      ~preview_result:(fun request ->
        incr preview_count;
        match !preview_count with
        | 1 ->
          expect "make first preview txs"
            (request.C.txs = [confirmed; rejected]);
          exec_result
            ~confirmed:[confirmed]
            ~rejected:[rejected]
            ~post_state_root:(raw 'l')
        | 2 ->
          expect "make second preview txs" (request.C.txs = [confirmed]);
          exec_result
            ~confirmed:[confirmed]
            ~rejected:[]
            ~post_state_root:(raw 'q')
        | _ ->
          fail "make unexpected preview retry")
      ()
  in
  (match run_make_proposal deps with
   | Some plan ->
     let _, preview_eic_root =
       Octra_core.Epoch_index_commitment.next_root_from_hashes
         ~prev:Octra_core.Epoch_index_commitment.genesis_root
         ~epoch_id:12
         ~start_txid:7L
         [confirmed_hash]
     in
     let expected_root =
       Octra_core.Epoch_index_commitment.folded_state_root
         ~ledger_state_root:(raw 'q')
         ~epoch_index_root:preview_eic_root
     in
     expect "make final hashes" (plan.tx_hashes = [confirmed_hash]);
     expect "make header hash"
       (plan.header.C_types.tx_list_hash = C.tx_list_hash [confirmed_hash]);
     expect "make second preview root"
       (plan.header.proposed_state_root = expected_root);
     expect "make txid hi" (plan.header.txid_hi = 7L);
     expect "make parent" (plan.parent_commit = None)
   | None ->
     fail "make proposal result");
  expect "make preview called" (List.length !(probe.preview_requests) = 2);
  expect "make set proposal" (!(probe.set_proposals) = [([confirmed], [confirmed_hash])]);
  (match !(probe.stored_bundles) with
   | [proposal_id, hashes, txs, receipts] ->
     expect "make stored pid" (String.length proposal_id > 0);
     expect "make stored hashes" (hashes = [confirmed_hash]);
     expect "make stored txs" (txs = [confirmed]);
     begin
       match Octra_core.Tx_outcome.decode ~confirmed:txs receipts with
       | Error error -> fail error
       | Ok partition ->
         expect "make stored rejection count"
           (List.length partition.rejections = 1);
         expect "make stored rejection tx"
           ((List.hd partition.rejections).tx = rejected)
     end
   | _ ->
     fail "make stored bundle");
  expect "make freeze" (List.length !(probe.frozen_writes) = 1)

let test_make_rejection_only () =
  let rejected = tx 1 in
  let preview_count = ref 0 in
  let deps, probe =
    make_proposal_deps
      ~staging:[rejected]
      ~preview_result:(fun request ->
        incr preview_count;
        match !preview_count with
        | 1 ->
          expect "rejection only first preview" (request.C.txs = [rejected]);
          exec_result
            ~confirmed:[]
            ~rejected:[rejected]
            ~post_state_root:(raw 'l')
        | 2 ->
          expect "rejection only stable preview" (request.C.txs = []);
          exec_result
            ~confirmed:[]
            ~rejected:[]
            ~post_state_root:(raw 'q')
        | _ -> fail "rejection only preview count")
      ()
  in
  (match run_make_proposal deps with
   | None -> fail "rejection only proposal missing"
   | Some plan ->
     expect "rejection only hashes" (plan.tx_hashes = []);
     expect "rejection only receipt root"
       (plan.header.C_types.receipt_root <> C_hash.receipt_root []));
  match !(probe.stored_bundles) with
  | [_, hashes, txs, receipts] ->
    expect "rejection only stored hashes" (hashes = []);
    expect "rejection only stored txs" (txs = []);
    begin
      match Octra_core.Tx_outcome.decode ~confirmed:[] receipts with
      | Error error -> fail error
      | Ok partition ->
        expect "rejection only count" (List.length partition.rejections = 1);
        expect "rejection only tx"
          ((List.hd partition.rejections).tx = rejected)
    end
  | _ -> fail "rejection only stored bundle"

let test_make_keep_partition () =
  let failed = tx 1 in
  let dependent = tx 2 in
  let independent = { (tx 1) with Transaction.from = "oct_other" } in
  let staged = Transaction.consensus_order [failed; dependent; independent] in
  let preview txs =
    let confirmed, rejected =
      List.partition
        (fun item ->
          item <> failed && (item <> dependent || List.mem failed txs))
        txs
    in
    exec_result ~confirmed ~rejected ~post_state_root:(raw 'l')
  in
  let deps, probe =
    make_proposal_deps
      ~staging:staged
      ~preview_result:(fun request -> preview request.C.txs)
      ()
  in
  expect "partition proposal present" (Option.is_some (run_make_proposal deps));
  match !(probe.stored_bundles) with
  | [_, _, confirmed, receipts] ->
    let outcomes =
      match Octra_core.Tx_outcome.decode ~confirmed receipts with
      | Ok value -> value
      | Error reason -> fail reason
    in
    let candidates =
      match Octra_core.Tx_outcome.merge ~confirmed ~rejections:outcomes.rejections with
      | Ok value -> value
      | Error reason -> fail reason
    in
    begin
      match C.verify_preview_partition
        ~candidates ~confirmed ~rejections:outcomes.rejections (preview candidates) with
      | Ok () -> ()
      | Error reason -> fail ("built partition rejected: " ^ reason)
    end;
    expect "dependent transaction deferred" (not (List.mem dependent candidates));
    expect "independent transaction retained" (confirmed = [independent]);
    expect "original rejection retained"
      (List.map (fun (item : Octra_core.Tx_outcome.rejection) -> item.tx)
         outcomes.rejections = [failed])
  | _ -> fail "partition bundle missing"

let test_make_dependency_chain () =
  List.iter
    (fun length ->
      let chain = List.init length (fun index -> tx (index + 1)) in
      let independent = { (tx 1) with Transaction.from = "oct_other" } in
      let staged = Transaction.consensus_order (independent :: chain) in
      let preview txs =
        let confirmed, rejected =
          List.partition
            (fun item ->
              item = independent
              || List.exists
                (fun prior ->
                  prior.Transaction.from = item.Transaction.from
                  && prior.nonce + 1 = item.nonce)
                txs)
            txs
        in
        exec_result ~confirmed ~rejected ~post_state_root:(raw 'l')
      in
      let deps, probe =
        make_proposal_deps ~staging:staged
          ~preview_result:(fun request -> preview request.C.txs) ()
      in
      expect "chain proposal present" (Option.is_some (run_make_proposal deps));
      expect "chain preview limit"
        (List.length !(probe.preview_requests) <= (2 * List.length staged) + 1);
      match !(probe.stored_bundles) with
      | [_, _, confirmed, receipts] ->
        let get = function Ok value -> value | Error reason -> fail reason in
        let outcomes = get (Octra_core.Tx_outcome.decode ~confirmed receipts) in
        let candidates =
          get (Octra_core.Tx_outcome.merge ~confirmed ~rejections:outcomes.rejections)
        in
        get (C.verify_preview_partition ~candidates ~confirmed
          ~rejections:outcomes.rejections (preview candidates));
        get (C.verify_preview_partition ~candidates:confirmed ~confirmed
          ~rejections:[] (preview confirmed));
        expect "chain independent retained" (confirmed = [independent]);
        expect "chain rejected positions"
          (List.map (fun (item : Octra_core.Tx_outcome.rejection) -> item.tx)
             outcomes.rejections
           = List.filter (fun item -> item.Transaction.nonce mod 2 = 1) chain);
        expect "chain staging unchanged" (deps.C.staging_txs () = staged)
      | _ -> fail "chain bundle missing")
    [1; 2; 3; 4; 8; 16]

let test_make_preview_error_defer () =
  let deps, probe =
    make_proposal_deps
      ~staging:[tx 1]
      ~preview_result:(fun _ -> Stdlib.Error "preview_failed")
      ()
  in
  expect "make preview error defer" (run_make_proposal deps = None);
  expect "make preview error called"
    (List.length !(probe.preview_requests) = 1);
  expect "make preview error no proposal" (!(probe.set_proposals) = []);
  expect "make preview error no store" (!(probe.stored_bundles) = []);
  expect "make preview error no freeze" (!(probe.frozen_writes) = [])

let test_make_partition_error_defer () =
  let deps, probe =
    make_proposal_deps
      ~staging:[tx 1]
      ~preview_result:(fun _ ->
        exec_result ~confirmed:[] ~rejected:[] ~post_state_root:(raw 'l'))
      ()
  in
  expect "make preview partition defer" (run_make_proposal deps = None);
  expect "make preview partition called"
    (List.length !(probe.preview_requests) = 1);
  expect "make preview partition no proposal" (!(probe.set_proposals) = []);
  expect "make preview partition no store" (!(probe.stored_bundles) = [])

let verify ?(deps = verify_deps ()) ?(limits = generous_limits)
    ?(receipts_json = []) ?(expected_tx_count = 1) txs =
  C.verify_bundle
    deps
    ~limits
    ~header:(header ~receipts_json ())
    ~expected_tx_count
    txs
    receipts_json

let test_verify_bundle_success () =
  match verify [tx 1] with
  | Ok verified ->
    expect "verified tx count" (List.length verified.C.txs = 1);
    expect "verified receipts" (verified.receipts_json = [])
  | Error _ -> fail "verify bundle success"

let test_verify_bundle_missing_txs () =
  match verify ~expected_tx_count:2 [tx 1] with
  | Error (C.Missing_txs { have; need }) ->
    expect "missing have" (have = 1);
    expect "missing need" (need = 2)
  | _ -> fail "missing tx reject"

let test_bundle_zero_count_tx () =
  match verify ~expected_tx_count:0 [tx 1] with
  | Error (C.Missing_txs { have; need }) ->
    expect "zero count have" (have = 1);
    expect "zero count need" (need = 0)
  | _ -> fail "zero count accepted tx"

let test_bundle_disabled_op () =
  let disabled =
    { (tx 1) with Transaction.op_type = Transaction.PrivateOp }
  in
  match verify [disabled] with
  | Error (C.Disabled_operation { hash; op_type }) ->
    expect "disabled hash short" (String.length hash <= 12);
    expect
      "disabled op type"
      (op_type = Transaction.op_type_to_string Transaction.PrivateOp)
  | _ -> fail "disabled operation reject"

let test_bundle_underpriced_tx () =
  let call =
    {
      (tx ~ou:1 2) with
      Transaction.op_type = Transaction.ContractCall;
    }
  in
  let program =
    {
      (tx 1) with
      Transaction.op_type = Transaction.ProgramDeploy;
      encrypted_data = Some (String.make 4_096 'x');
      ou = Z.of_int 200_000;
    }
  in
  let previous = Sys.getenv_opt "OCTRA_BFT_RELEASE_PROFILE" in
  Unix.putenv "OCTRA_BFT_RELEASE_PROFILE" "devnet_full_v1";
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv
        "OCTRA_BFT_RELEASE_PROFILE"
        (Option.value previous ~default:""))
    (fun () ->
      begin
        match verify [call] with
        | Error (C.Underpriced_transaction {
            hash;
            op_type;
            provided;
            required;
          }) ->
          expect "underpriced call hash short" (String.length hash <= 12);
          expect "underpriced call op type" (op_type = "call");
          expect "underpriced call provided" (Z.equal provided Z.one);
          expect "underpriced call required" (Z.equal required (Z.of_int 1_000))
        | _ -> fail "underpriced contract call accepted"
      end;
      match verify [program] with
      | Error (C.Underpriced_transaction {
          hash;
          op_type;
          provided;
          required;
        }) ->
        expect "underpriced hash short" (String.length hash <= 12);
        expect "underpriced op type" (op_type = "program_deploy");
        expect "underpriced provided" (Z.equal provided (Z.of_int 200_000));
        expect "underpriced required" (Z.gt required provided)
      | _ -> fail "underpriced Program deploy accepted")

let test_bundle_receipt_root_error () =
  match
    C.verify_bundle
      (verify_deps ())
      ~limits:generous_limits
      ~header:(header ~receipts_json:["other"] ())
      ~expected_tx_count:1
      [tx 1]
      []
  with
  | Error C.Receipt_root_mismatch -> ()
  | _ -> fail "receipt root mismatch reject"

let test_bundle_receipt_decode_error () =
  match verify ~receipts_json:["not-a-receipt"] [tx 1] with
  | Error (C.Receipt_decode_failed _) -> ()
  | _ -> fail "receipt decode reject"

let test_bundle_preverify_failure () =
  let t = tx 1 in
  match verify ~expected_tx_count:2 [t; t] with
  | Error (C.Receipt_decode_failed e) ->
    expect "duplicate reason" (e = "outcome_hash_duplicate")
  | _ -> fail "preverify reject"

let test_verify_bundle_limit_failed () =
  let limits =
    C.limits
      ~max_txs:1
      ~max_bytes:1_000_000
      ~max_ou:(Z.of_int 1_000_000)
  in
  match verify ~limits ~expected_tx_count:2 [tx 1; tx 2] with
  | Error (C.Bundle_limit { totals; limits }) ->
    expect "limit totals" (totals.C.count = 2);
    expect "limit max" (limits.C.max_txs = 1)
  | _ -> fail "limit reject"

let test_verify_bundle_bad_signature () =
  match verify ~deps:(verify_deps ~signature_ok:false ()) [tx 1] with
  | Error (C.Invalid_tx_signature { hash; from_addr }) ->
    expect "hash short" (String.length hash <= 12);
    expect "from short" (String.length from_addr <= 14)
  | _ -> fail "signature reject"

let identity_root s =
  s

let eic_root ~epoch_id ~tx_hashes ~start_txid ~prev_eic_root =
  let _, root =
    Octra_core.Epoch_index_commitment.next_root_from_hashes
      ~prev:prev_eic_root
      ~epoch_id
      ~start_txid
      tx_hashes
  in
  root

let folded_root ~ledger_state_root ~epoch_index_root =
  Octra_core.Epoch_index_commitment.folded_state_root
    ~ledger_state_root
    ~epoch_index_root

let head ?ledger_state_root ?epoch_index_root () =
  let ledger_state_root_opt = ledger_state_root in
  let epoch_index_root_opt = epoch_index_root in
  Octra_core.Head_manifest.{
    schema_version = 3;
    generation = 1;
    epoch_id = 10;
    state_root = raw 's';
    ledger_state_root = ledger_state_root_opt;
    irmin_commit = None;
    txid_hi = 9L;
    txlog_seg = None;
    txlog_off = None;
    epochlog_off = None;
    commit_id = "commit";
    ts = 1.0;
    quorum_cert_hash = None;
    epoch_index_hash = None;
    epoch_index_root = epoch_index_root_opt;
  }

let test_prev_eic_root_from_head () =
  let genesis = Octra_core.Epoch_index_commitment.genesis_root in
  expect "prev eic no head" (C.prev_eic_root_from_head None = genesis);
  expect "prev eic missing ledger"
    (C.prev_eic_root_from_head
       (Some (head ~epoch_index_root:"eic-root" ())) = genesis);
  expect "prev eic missing eic"
    (C.prev_eic_root_from_head
       (Some (head ~ledger_state_root:"ledger-root" ())) = genesis);
  expect "prev eic from head"
    (C.prev_eic_root_from_head
       (Some (head ~ledger_state_root:"ledger-root" ~epoch_index_root:"eic-root" ()))
     = "eic-root")

let test_preview_root_match () =
  let tx_hashes = [String.make 64 'a'] in
  let preview_eic_root =
    eic_root
      ~epoch_id:10
      ~tx_hashes
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
  in
  let expected =
    folded_root
      ~ledger_state_root:(raw 'l')
      ~epoch_index_root:preview_eic_root
  in
  match
    C.preview_decision
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~tx_hashes
      ~tx_count:1
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~local_ledger_root:(raw 'p')
      ~proposed_state_root:expected
      ~preview:(C.Preview_ok { post_state_root = raw 'l' })
  with
  | C.Preview_accept { computed_root; preview_eic_root = got_eic } ->
    expect "preview computed root" (computed_root = expected);
    expect "preview eic root" (got_eic = preview_eic_root)
  | _ -> fail "preview accept"

let test_preview_empty_local_root () =
  let preview_eic_root =
    eic_root
      ~epoch_id:10
      ~tx_hashes:[]
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
  in
  let expected =
    folded_root
      ~ledger_state_root:(raw 'p')
      ~epoch_index_root:preview_eic_root
  in
  match
    C.preview_decision
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~tx_hashes:[]
      ~tx_count:0
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~local_ledger_root:(raw 'p')
      ~proposed_state_root:expected
      ~preview:(C.Preview_ok { post_state_root = "" })
  with
  | C.Preview_accept _ -> ()
  | _ -> fail "preview empty accept"

let test_preview_nonempty_no_root () =
  match
    C.preview_decision
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~tx_hashes:[String.make 64 'a']
      ~tx_count:1
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~local_ledger_root:(raw 'p')
      ~proposed_state_root:(raw 's')
      ~preview:(C.Preview_ok { post_state_root = "" })
  with
  | C.Preview_root_mismatch { computed_root = None; _ } -> ()
  | _ -> fail "preview missing nonempty root reject"

let test_preview_decision_error () =
  match
    C.preview_decision
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~tx_hashes:[]
      ~tx_count:0
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~local_ledger_root:(raw 'p')
      ~proposed_state_root:(raw 's')
      ~preview:(C.Preview_error "boom")
  with
  | C.Preview_error_reject e ->
    expect "preview error" (e = "boom")
  | _ -> fail "preview error reject"

let test_preview_status_of_result () =
  let result =
    Stdlib.Ok Octra_core.Epoch_exec.{
      post_state_root = raw 'r';
      artifacts = {
        confirmed = [];
        rejected = [];
        confirmed_fees = Z.zero;
        tx_count = 0;
      };
    }
  in
  (match C.preview_status_of_result result with
   | C.Preview_ok { post_state_root } ->
     expect "preview status root" (post_state_root = raw 'r')
   | C.Preview_error _ -> fail "preview status ok");
  (match C.preview_status_of_result (Stdlib.Error "boom") with
   | C.Preview_error e -> expect "preview status error" (e = "boom")
   | C.Preview_ok _ -> fail "preview status error")

let test_proposal_roots () =
  let roots =
    C.proposal_roots
      ~root_to_raw32:identity_root
      ~ledger_head:(Some (raw 'l'))
      ~cached_head:(Some (head ~ledger_state_root:"ledger" ~epoch_index_root:"eic" ()))
  in
  expect "proposal ledger root" (roots.C.prev_ledger_root = raw 'l');
  expect "proposal state root" (roots.prev_state_root = raw 's');
  let roots =
    C.proposal_roots
      ~root_to_raw32:identity_root
      ~ledger_head:(Some (raw 'l'))
      ~cached_head:(Some { (head ()) with Octra_core.Head_manifest.state_root = "" })
  in
  expect "proposal empty state falls back to ledger"
    (roots.prev_state_root = raw 'l');
  let roots =
    C.proposal_roots
      ~root_to_raw32:identity_root
      ~ledger_head:None
      ~cached_head:None
  in
  expect "proposal no roots ledger" (roots.prev_ledger_root = String.make 32 '\x00');
  expect "proposal no roots state" (roots.prev_state_root = String.make 32 '\x00')

let test_prev_root_decision_match () =
  match
    C.prev_root_decision
      ~epoch_id:10L
      ~target_root:(raw 'r')
      ~current_root:(raw 'r')
      ~max_wait_tries:60
      ~tries_left:12
      ~current_streak:3
      ~quarantine_threshold:4
  with
  | C.Prev_root_match -> ()
  | _ -> fail "prev root match"

let test_prev_root_mismatch_grace () =
  match
    C.prev_root_decision
      ~epoch_id:10L
      ~target_root:(raw 't')
      ~current_root:(raw 'c')
      ~max_wait_tries:60
      ~tries_left:55
      ~current_streak:1
      ~quarantine_threshold:4
  with
  | C.Prev_root_mismatch { waited_steps; streak_after; quarantine_reason } ->
    expect "waited steps" (waited_steps = 5);
    expect "streak after" (streak_after = 2);
    expect "no quarantine" (quarantine_reason = None)
  | _ -> fail "prev root mismatch before quarantine"

let test_prev_root_quarantine () =
  match
    C.prev_root_decision
      ~epoch_id:10L
      ~target_root:(raw 't')
      ~current_root:(raw 'c')
      ~max_wait_tries:60
      ~tries_left:0
      ~current_streak:3
      ~quarantine_threshold:4
  with
  | C.Prev_root_mismatch { waited_steps; streak_after; quarantine_reason } ->
    expect "waited all steps" (waited_steps = 60);
    expect "streak quarantine" (streak_after = 4);
    expect "quarantine reason"
      (quarantine_reason = Some "prev_state_root_mismatch_streak = 4 epoch = 10")
  | _ -> fail "prev root mismatch quarantine"

let prev_root_wait ?(replay_count = ref 0) ?(sleep_count = ref 0) roots =
  let pending = ref roots in
  C.{
    read_root = (fun () ->
      match !pending with
      | [] -> Lwt.return (raw 'z')
      | root :: rest ->
        pending := rest;
        Lwt.return root);
    replay_stashed = (fun () ->
      incr replay_count;
      Lwt.return_unit);
    sleep = (fun _ ->
      incr sleep_count;
      Lwt.return_unit);
  }

let test_prev_root_wait_immediate () =
  let replay_count = ref 0 in
  let sleep_count = ref 0 in
  let sample =
    Lwt_main.run
      (C.wait_for_prev_root
         (prev_root_wait ~replay_count ~sleep_count [raw 't'])
         ~target_root:(raw 't')
         ~max_tries:4
         ~delay_seconds:0.01)
  in
  expect "initial immediate" (sample.C.initial_root = raw 't');
  expect "current immediate" (sample.current_root = raw 't');
  expect "tries immediate" (sample.tries_left = 4);
  expect "replay skipped" (!replay_count = 0);
  expect "sleep skipped" (!sleep_count = 0)

let test_prev_root_wait_retry () =
  let replay_count = ref 0 in
  let sleep_count = ref 0 in
  let sample =
    Lwt_main.run
      (C.wait_for_prev_root
         (prev_root_wait ~replay_count ~sleep_count [raw 'a'; raw 't'])
         ~target_root:(raw 't')
         ~max_tries:4
         ~delay_seconds:0.01)
  in
  expect "initial retry" (sample.C.initial_root = raw 'a');
  expect "current retry" (sample.current_root = raw 't');
  expect "tries retry" (sample.tries_left = 4);
  expect "replay retry" (!replay_count = 1);
  expect "sleep retry" (!sleep_count = 0)

let test_prev_root_wait_exhausted () =
  let replay_count = ref 0 in
  let sleep_count = ref 0 in
  let sample =
    Lwt_main.run
      (C.wait_for_prev_root
         (prev_root_wait ~replay_count ~sleep_count [raw 'a'; raw 'a'; raw 'b'; raw 'c'])
         ~target_root:(raw 't')
         ~max_tries:2
         ~delay_seconds:0.01)
  in
  expect "initial exhausted" (sample.C.initial_root = raw 'a');
  expect "current exhausted" (sample.current_root = raw 'c');
  expect "tries exhausted" (sample.tries_left = 0);
  expect "replay exhausted" (!replay_count = 1);
  expect "sleep exhausted" (!sleep_count = 2)

let test_preview_plan_reject_retry () =
  let confirmed = [tx 1] in
  let rejected = [tx 2] in
  let final_hashes = List.map Transaction.hash confirmed in
  let plan =
    C.build_preview_plan
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~prev_ledger_root:(raw 'p')
      ~fallback_ledger_root:(raw 'c')
      ~input_txs:(confirmed @ rejected)
      ~preview:(C.Build_preview_ok {
        post_state_root = raw 'l';
        confirmed;
        rejected;
      })
  in
  expect "preview final txs" (plan.C.final_txs = confirmed);
  expect "preview final hashes" (plan.C.final_hashes = final_hashes);
  expect "preview rejected hashes"
    (plan.C.rejected_hashes = List.map Transaction.hash rejected);
  expect "preview proposed root held" (plan.C.proposed_state_root = raw 'c');
  expect "preview consensus root held" (plan.C.preview_consensus_root = None);
  expect "preview retry marker"
    (plan.C.preview_error = Some "preview_rejected_transactions")

let test_preview_plan_empty_root () =
  let preview_eic_root =
    eic_root
      ~epoch_id:10
      ~tx_hashes:[]
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
  in
  let expected =
    folded_root
      ~ledger_state_root:(raw 'p')
      ~epoch_index_root:preview_eic_root
  in
  let plan =
    C.build_preview_plan
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~prev_ledger_root:(raw 'p')
      ~fallback_ledger_root:(raw 'c')
      ~input_txs:[]
      ~preview:(C.Build_preview_ok {
        post_state_root = "";
        confirmed = [];
        rejected = [];
      })
  in
  expect "empty final txs" (plan.C.final_txs = []);
  expect "empty preview root" (plan.C.proposed_state_root = expected)

let test_preview_plan_error_defer () =
  let input_txs = [tx 1; tx 2] in
  let plan =
    C.build_preview_plan
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~prev_ledger_root:(raw 'p')
      ~fallback_ledger_root:(raw 'c')
      ~input_txs
      ~preview:(C.Build_preview_error "boom")
  in
  expect "error final txs" (plan.C.final_txs = []);
  expect "error final hashes" (plan.C.final_hashes = []);
  expect "error proposed root" (plan.C.proposed_state_root = raw 'c');
  expect "error marker" (plan.C.preview_error = Some "boom")

let test_preview_output_reject_retry () =
  let confirmed = tx 1 in
  let rejected = tx 2 in
  let preview_result =
    Stdlib.Ok Octra_core.Epoch_exec.{
      post_state_root = raw 'l';
      artifacts = {
        confirmed = [confirmed, 0];
        rejected = [{
          tx = rejected;
          error_type = "bad";
          reason = "rejected";
        }];
        confirmed_fees = Z.zero;
        tx_count = 2;
      };
    }
  in
  let rejections =
    match
      Octra_core.Tx_outcome.build
        ~candidates:[confirmed; rejected]
        [rejected, "bad", "rejected"]
    with
    | Ok value -> value
    | Error error -> fail error
  in
  let output =
    C.build_preview_output
      ~root_to_raw32:identity_root
      ~epoch_id:10L
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
      ~prev_ledger_root:(raw 'p')
      ~fallback_ledger_root:(raw 'c')
      ~input_txs:[confirmed; rejected]
      ~batch:(Lwt_main.run (fake_batch [confirmed; rejected]))
      ~rejections
      ~preview_result
  in
  expect "output identifies confirmed"
    (output.C.plan.final_txs = [confirmed]);
  expect "output rejected count" (output.rejected_count = 1);
  expect "output receipts" (List.length output.receipts_json = 1);
  expect "output retry marker"
    (output.plan.preview_error = Some "preview_rejected_transactions")

let diag_context validator_addrs =
  C.layera_diag_context
    ~validator_addrs
    ~hash_validators:(fun payload -> "hash:" ^ payload)
    ~meta:(fun key -> "meta:" ^ key)

let proposal_for_txs txs =
  let tx_hashes = List.map Transaction.hash txs in
  let txid_hi =
    match txs with
    | [] -> 6L
    | _ -> Int64.add 7L (Int64.of_int (List.length txs - 1))
  in
  let epoch_id = 12L in
  let preview_eic_root =
    eic_root
      ~epoch_id:(Int64.to_int epoch_id)
      ~tx_hashes
      ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root
  in
  let proposed_state_root =
    folded_root
      ~ledger_state_root:(raw 'l')
      ~epoch_index_root:preview_eic_root
  in
  let header = C_types.{
    proto_version = C_types.proto_version_current;
    chain_id = "octra-test";
    epoch_id;
    prev_state_root = raw 'p';
    tx_list_hash = C.tx_list_hash tx_hashes;
    receipt_root = C_hash.receipt_root [];
    proposed_state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "oct_creator";
    txid_hi;
    ts = 99.0;
  } in
  C_types.{
    chain_id = "octra-test";
    epoch_id;
    round = 0;
    valid_round = None;
    header;
    tx_hashes;
    parent_commit = None;
    proposer = "oct_creator";
    signature = "sig";
  }

let verify_proposal_deps ?(quarantine = false) ?(root = raw 'p')
    ?(current = fun () -> true)
    ?(prev_streak = 0) ?(state_streak = 0)
    ?(driver_available = false) ?(staging = [])
    ?(cached_bundle = fun ~proposal_id:_ -> None)
    ?(now = 99.0)
    ?(previous_epoch_ts = Some 89.0)
    ?(share_txs = fun _ -> ())
    ?(ledger_root = raw 'p')
    ?(validate_preverify_once = fun ~state_root:_ ~tx_hashes:_ txs ->
      fake_batch txs)
    ?(preview_result = fun request ->
      exec_result
        ~confirmed:request.C.txs
        ~rejected:[]
        ~post_state_root:(raw 'l'))
    () =
  let quarantines = ref [] in
  let prev_streak_ref = ref prev_streak in
  let state_streak_ref = ref state_streak in
  let sleeps = ref [] in
  let stores = ref [] in
  let set_proposals = ref [] in
  let previews = ref [] in
  let deps = C.{
    current;
    now = (fun () -> now);
    previous_epoch_ts = (fun _ -> previous_epoch_ts);
    quarantine_active = (fun () -> quarantine);
    quarantine_reason = (fun () -> "test_quarantine");
    mark_quarantine = (fun reason ->
      quarantines := reason :: !quarantines);
    prev_root_streak = (fun () -> !prev_streak_ref);
    set_prev_root_streak = (fun n ->
      prev_streak_ref := n);
    state_root_streak = (fun () -> !state_streak_ref);
    set_state_root_streak = (fun n ->
      state_streak_ref := n);
    limits = generous_limits;
    layera_diag_live = (fun () -> false);
    layera_env_diag_context = (fun () -> diag_context ["oct_creator"]);
    layera_diag_context = (fun ~validator_addrs -> diag_context validator_addrs);
    wait_prev_root = {
      read_root = (fun () -> Lwt.return root);
      replay_stashed = (fun () -> Lwt.return_unit);
      sleep = (fun seconds ->
        sleeps := seconds :: !sleeps;
        Lwt.return_unit);
    };
    max_prev_root_wait_tries = 0;
    prev_root_wait_delay_seconds = 0.01;
    quarantine_mismatch_threshold = 4;
    staging_txs = (fun () -> staging);
    cached_bundle;
    validate_preverify_once;
    driver_available = (fun () -> driver_available);
    validate_bundle = (fun ~header:_ ~expected_hashes:_ _ -> None);
    query_bundle = (fun ~epoch_id:_ ~proposal_id:_ ~validate:_ ->
      Lwt.return_none);
    store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
      stores := (proposal_id, tx_hashes, txs, receipts_json) :: !stores);
    public_key_for_tx = (fun tx -> tx.Transaction.public_key);
    verify_address_pubkey = (fun ~addr:_ ~pubkey:_ -> true);
    verify_tx_signature = (fun _ ~pubkey:_ -> true);
    validator_pubkeys = (fun _ -> ["oct_validator", "pub"]);
    read_local_ledger_root = (fun () -> Lwt.return ledger_root);
    preview = (fun request ->
      previews := request :: !previews;
      Lwt.return (preview_result request));
    prev_eic_root = (fun () -> Octra_core.Epoch_index_commitment.genesis_root);
    next_txid = (fun () -> 7L);
    head_txid_hi = (fun () -> Some 6L);
    root_to_raw32 = identity_root;
    set_proposal = (fun txs hashes ->
      set_proposals := (txs, hashes) :: !set_proposals);
    share_txs;
    verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
  } in
  deps, quarantines, prev_streak_ref, state_streak_ref, sleeps, stores,
  set_proposals, previews

let test_verify_txid_hi_mismatch () =
  let item = tx 1 in
  let proposal = proposal_for_txs [item] in
  let proposal =
    {
      proposal with
      header = { proposal.header with txid_hi = 8L };
    }
  in
  let deps, _, _, _, _, stores, set_proposals, previews =
    verify_proposal_deps ~staging:[item] ()
  in
  expect "verify txid mismatch reject"
    (verdict_rejects
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "verify txid mismatch no bundle" (!stores = []);
  expect "verify txid mismatch no proposal" (!set_proposals = []);
  expect "verify txid mismatch no preview" (!previews = [])

let test_verify_prev_root_quarantine () =
  let item = tx 1 in
  let proposal = proposal_for_txs [item] in
  let deps, quarantines, prev_streak_ref, _state_streak_ref, sleeps, stores,
      set_proposals, previews =
    verify_proposal_deps
      ~root:(raw 'x')
      ~prev_streak:3
      ~staging:[item]
      ()
  in
  expect "verify prev mismatch reject"
    (verdict_rejects
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "verify prev mismatch streak" (!prev_streak_ref = 4);
  expect "verify prev mismatch quarantine"
    (!quarantines = ["prev_state_root_mismatch_streak = 4 epoch = 12"]);
  expect "verify prev mismatch no sleep" (!sleeps = []);
  expect "verify prev mismatch no store" (!stores = []);
  expect "verify prev mismatch no set" (!set_proposals = []);
  expect "verify prev mismatch no preview" (!previews = [])

let test_verify_missing_prev_time () =
  let proposal = proposal_for_txs [tx 1] in
  let deps, _, _, _, _, _, _, _ =
    verify_proposal_deps ~previous_epoch_ts:None ()
  in
  expect "missing previous time did not wait"
    (verdict_waits
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)))

let test_prev_time_retry () =
  let item = tx 1 in
  let proposal = proposal_for_txs [item] in
  let previous = ref None in
  let deps, quarantines, _, _, _, stores, set_proposals, previews =
    verify_proposal_deps ~staging:[item] ()
  in
  let deps = C.{ deps with previous_epoch_ts = (fun _ -> !previous) } in
  let verify () = Lwt_main.run (C.verify_proposal deps ~chain_id:"octra-test" proposal) in
  expect "missing time waits" (verdict_waits (verify ()));
  expect "missing time no store" (!stores = []);
  expect "missing time no proposal" (!set_proposals = []);
  expect "missing time no preview" (!previews = []);
  previous := Some nan;
  expect "invalid time rejects" (verdict_rejects (verify ()));
  previous := Some 89.;
  expect "available time accepts" (verdict_accepts (verify ()));
  expect "time read no quarantine" (!quarantines = [])

let test_verify_missing_bundle_wait () =
  let proposal = proposal_for_txs [tx 1] in
  let deps, quarantines, _, _, _, stores, set_proposals, previews =
    verify_proposal_deps ~driver_available:true ()
  in
  expect "missing bundle did not wait"
    (verdict_waits
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "missing bundle quarantined" (!quarantines = []);
  expect "missing bundle stored" (!stores = []);
  expect "missing bundle set proposal" (!set_proposals = []);
  expect "missing bundle previewed" (!previews = [])

let test_verify_local_preview () =
  let item = tx 1 in
  let tx_hash = Transaction.hash item in
  let shared = ref [] in
  let proposal = proposal_for_txs [item] in
  let deps, quarantines, prev_streak_ref, state_streak_ref, _sleeps, stores,
      set_proposals, previews =
    verify_proposal_deps
      ~prev_streak:2
      ~state_streak:2
      ~staging:[item]
      ~share_txs:(fun txs -> shared := txs :: !shared)
      ~preview_result:(fun request ->
        expect "verify preview request txs" (request.C.txs = [item]);
        exec_result
          ~confirmed:[item]
          ~rejected:[]
          ~post_state_root:(raw 'l'))
      ()
  in
  expect "verify proposal accept"
    (verdict_accepts
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "verify accept quarantines" (!quarantines = []);
  expect "verify accept prev streak reset" (!prev_streak_ref = 0);
  expect "verify accept state streak reset" (!state_streak_ref = 0);
  expect "verify accept preview count" (List.length !previews = 1);
  expect "verify accept proposal set" (!set_proposals = [([item], [tx_hash])]);
  expect "verify accept shared" (!shared = [[item]]);
  expect "verify accept stores twice" (List.length !stores = 2)

let test_verify_head_progress () =
  List.iter (fun phase ->
    let item = tx 1 in
    let current = ref true in
    let shared = ref [] in
    let release, finish = Lwt.wait () in
    let entered = ref false in
    let pause run =
      entered := true;
      let open Lwt.Syntax in
      let* () = Lwt.protected release in
      run ()
    in
    let deps, quarantines, prev_streak, state_streak, _, _, proposals, _ =
      verify_proposal_deps ~current:(fun () -> !current) ~staging:[item]
        ~prev_streak:3 ~state_streak:3 ~share_txs:(fun txs -> shared := txs :: !shared)
        ~preview_result:(fun request ->
          exec_result ~confirmed:request.C.txs ~rejected:[] ~post_state_root:(raw 'x')) ()
    in
    let deps = match phase with
      | `Root -> { deps with wait_prev_root = {
          deps.wait_prev_root with read_root = (fun () -> pause (fun () -> Lwt.return (raw 'x'))) } }
      | `Ledger -> { deps with read_local_ledger_root = (fun () -> pause deps.read_local_ledger_root) }
      | `Checks -> { deps with validate_preverify_once = (fun ~state_root ~tx_hashes txs ->
          pause (fun () -> deps.validate_preverify_once ~state_root ~tx_hashes txs)) }
      | `Preview -> { deps with preview = (fun request -> pause (fun () -> deps.preview request)) }
    in
    let result = C.verify_proposal deps ~chain_id:"octra-test" (proposal_for_txs [item]) in
    expect "verification waiting" (!entered && Lwt.is_sleeping result);
    current := false;
    Lwt.wakeup_later finish ();
    expect "superseded verification waits" (verdict_waits (Lwt_main.run result));
    expect "head progress did not quarantine" (!quarantines = []);
    expect "head progress kept counters" (!prev_streak = 3 && !state_streak = 3);
    expect "head progress did not publish" (!proposals = [] && !shared = []))
    [`Root; `Ledger; `Checks; `Preview]

let test_verify_staging_lookup () =
  let module S = Octra_core.Tx_staging in
  let module D = Octra_node_runtime.Consensus_driver_wiring in
  S.clear ();
  Fun.protect ~finally:S.clear (fun () ->
    let pending = tx 3 in
    let ready = Transaction.{ (tx 1) with from = "oct_other" } in
    List.iter (fun item ->
      match S.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000_000, 0)) item with
      | Ok _ -> ()
      | Error reason -> fail reason) [pending; ready];
    let adapters = D.node_standard_adapters D.{
      getenv = (fun _ -> None);
      get_meta = (fun _ -> None);
      wallet_addr = "oct_creator";
      wallet_pub = "pub";
      find_account = (fun _ -> Some Octra_core.Ledger.empty_account);
      cached_head = (fun () -> None);
      read_prev_ledger_root = (fun () -> Lwt.return_none);
      next_txid = (fun () -> 7L);
      proposal_state = Octra_node_runtime.Consensus_proposal_state.create ();
      catchup_active = ref false;
      staging_epoch_capacity = Z.of_int 10_000;
      write_pending = (fun _ -> ());
      validator_pubkeys_for_epoch = (fun ~wallet_addr:_ ~wallet_pub:_ ~epoch:_ -> []);
    } in
    expect "proposer excludes nonce gap" (adapters.staging_epoch_txs () = [ready]);
    let items = [ready; pending] in
    let proposal = proposal_for_txs items in
    let queries = ref 0 in
    let deps, _, _, _, _, _, proposals, previews =
      verify_proposal_deps ~driver_available:true
        ~validate_preverify_once:(fun ~state_root:_ ~tx_hashes:_ txs ->
          expect "local proposal order" (txs = items);
          fake_batch txs) ()
    in
    let deps = C.{ deps with
      staging_txs = adapters.staging_txs;
      query_bundle = (fun ~epoch_id:_ ~proposal_id:_ ~validate:_ ->
        incr queries;
        Lwt.return_none);
    } in
    expect "ready-only lookup needs bundle"
      (verdict_waits (Lwt_main.run (C.verify_proposal
        { deps with staging_txs = adapters.staging_epoch_txs }
        ~chain_id:"octra-test" proposal)));
    expect "missing lookup queries once" (!queries = 1 && !previews = []);
    queries := 0;
    expect "all staging supplies bundle"
      (verdict_accepts (Lwt_main.run
        (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
    expect "local lookup avoids query" (!queries = 0);
    expect "local lookup preserves order"
      (!proposals = [items, List.map Transaction.hash items]);
    expect "local lookup still previews" (List.length !previews = 1);
    expect "local lookup still verifies"
      (verdict_rejects (Lwt_main.run (C.verify_proposal
        { deps with verify_tx_signature = (fun _ ~pubkey:_ -> false) }
        ~chain_id:"octra-test" proposal)));
    expect "invalid signature avoids preview" (List.length !previews = 1);
    expect "validator leaves staging intact" (adapters.staging_total () = 2);
    expect "proposer remains ready-only" (adapters.staging_epoch_txs () = [ready]))

let test_verify_ledger_preverify () =
  let item = tx 1 in
  let calls = ref [] in
  let validate_preverify_once ~state_root ~tx_hashes txs =
    calls := (state_root, tx_hashes) :: !calls;
    fake_batch txs
  in
  let deps, _, _, _, _, _, _, _ =
    verify_proposal_deps
      ~root:(raw 'p')
      ~ledger_root:(raw 'l')
      ~staging:[item]
      ~validate_preverify_once
      ()
  in
  expect "verify ledger preverify accepted"
    (verdict_accepts
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test"
             (proposal_for_txs [item]))));
  expect "verify ledger preverify calls" (List.length !calls = 2);
  List.iter
    (fun (state_root, tx_hashes) ->
      expect "verify ledger preverify state" (state_root = raw 'l');
      expect "verify ledger preverify hashes"
        (tx_hashes = [Transaction.hash item]))
    !calls

let proposal_outcome ~accepted ~rejected ~reason =
  let rejections =
    match
      Octra_core.Tx_outcome.build
        ~candidates:[accepted; rejected]
        [rejected, "program_exec_failed", reason]
    with
    | Ok value -> value
    | Error error -> fail error
  in
  let receipts = Octra_core.Tx_outcome.encode [] rejections in
  let base = proposal_for_txs [accepted] in
  let header = {
    base.C_types.header with
    receipt_root = C_hash.receipt_root receipts;
  } in
  {
    base with
    C_types.header;
  },
  F.{
    txs = [accepted];
    receipts_json = receipts;
    rejections;
  }

let outcome_preview ~accepted ~rejected ~reason request =
  match request.C.txs with
  | [first; second] when first = accepted && second = rejected ->
    Stdlib.Ok Octra_core.Epoch_exec.{
      post_state_root = raw 'x';
      artifacts = {
        confirmed = [accepted, 0];
        rejected = [{
          tx = rejected;
          error_type = "program_exec_failed";
          reason;
        }];
        confirmed_fees = Z.zero;
        tx_count = 2;
      };
    }
  | [first] when first = accepted ->
    exec_result
      ~confirmed:[accepted]
      ~rejected:[]
      ~post_state_root:(raw 'l')
  | _ -> Stdlib.Error "unexpected_preview_input"

let test_verify_reproduced_rejection () =
  let accepted = tx 1 in
  let rejected = tx 2 in
  let proposal, bundle =
    proposal_outcome
      ~accepted
      ~rejected
      ~reason:"method not found"
  in
  let deps, _, _, _, _, _, set_proposals, previews =
    verify_proposal_deps
      ~cached_bundle:(fun ~proposal_id:_ -> Some bundle)
      ~preview_result:
        (outcome_preview
           ~accepted
           ~rejected
           ~reason:"method not found")
      ()
  in
  expect "rejection outcome accepted"
    (verdict_accepts
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "rejection outcome previews" (List.length !previews = 2);
  expect "rejection outcome proposal"
    (!set_proposals = [([accepted], [Transaction.hash accepted])])

let test_verify_forged_rejection () =
  let accepted = tx 1 in
  let rejected = tx 2 in
  let shared = ref [] in
  let proposal, bundle =
    proposal_outcome
      ~accepted
      ~rejected
      ~reason:"forged reason"
  in
  let deps, _, _, _, _, _, set_proposals, previews =
    verify_proposal_deps
      ~cached_bundle:(fun ~proposal_id:_ -> Some bundle)
      ~share_txs:(fun txs -> shared := txs :: !shared)
      ~preview_result:
        (outcome_preview
           ~accepted
           ~rejected
           ~reason:"method not found")
      ()
  in
  expect "forged rejection accepted"
    (verdict_rejects
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "forged rejection previews" (List.length !previews = 1);
  expect "forged rejection proposal" (!set_proposals = []);
  expect "forged rejection shared" (!shared = [])

let test_verify_old_valid_round () =
  let item = tx 1 in
  let proposal =
    {
      (proposal_for_txs [item]) with
      round = 1;
      valid_round = Some 0;
    }
  in
  let deps, _, _, _, _, _, _, previews =
    verify_proposal_deps ~now:1_000.0 ~staging:[item] ()
  in
  expect "old valid value rejected"
    (verdict_accepts
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "old valid value preview" (List.length !previews = 1)

let test_future_reproposal_prior () =
  let item = tx 1 in
  let source = proposal_for_txs [item] in
  let header =
    {
      source.header with
      chain_id = "octra-devnet-9871-cluster";
      ts = 1_700_000_100.0;
    }
  in
  let proposal =
    {
      source with
      chain_id = header.chain_id;
      round = 1;
      valid_round = Some 0;
      header;
    }
  in
  let deps, _, _, _, _, _, _, previews =
    verify_proposal_deps
      ~now:1_700_000_010.0
      ~previous_epoch_ts:(Some 1_700_000_000.0)
      ~staging:[item]
      ()
  in
  expect "future pre-activation reproposal rejected"
    (verdict_accepts
       (Lwt_main.run
          (C.verify_proposal
             deps
             ~chain_id:"octra-devnet-9871-cluster"
             proposal)));
  expect "future pre-activation reproposal not previewed"
    (List.length !previews = 1)

let test_future_reproposal_active () =
  let item = tx 1 in
  let activation =
    match
      Octra_consensus.C_epoch_time_policy.activation_for_chain
        "octra-devnet-9871-cluster"
    with
    | Some value -> value
    | None -> failwith "devnet epoch time activation missing"
  in
  let epoch_id = Int64.of_int activation.activation_epoch in
  let source = proposal_for_txs [item] in
  let header =
    {
      source.header with
      chain_id = "octra-devnet-9871-cluster";
      epoch_id;
      ts = 1_700_000_100.0;
    }
  in
  let proposal =
    {
      source with
      chain_id = header.chain_id;
      epoch_id;
      round = 1;
      valid_round = Some 0;
      header;
    }
  in
  let deps, _, _, _, _, _, _, previews =
    verify_proposal_deps
      ~now:1_700_000_010.0
      ~previous_epoch_ts:(Some 1_700_000_000.0)
      ~staging:[item]
      ()
  in
  expect "future activated reproposal accepted"
    (verdict_rejects
       (Lwt_main.run
          (C.verify_proposal
             deps
             ~chain_id:"octra-devnet-9871-cluster"
             proposal)));
  expect "future activated reproposal previewed" (!previews = [])

let test_verify_invalid_valid_round () =
  let item = tx 1 in
  let proposal =
    {
      (proposal_for_txs [item]) with
      round = 1;
      valid_round = Some 1;
    }
  in
  let deps, _, _, _, _, _, _, previews =
    verify_proposal_deps ~now:1_000.0 ~staging:[item] ()
  in
  expect "invalid valid round accepted"
    (verdict_rejects
       (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)));
  expect "invalid valid round previewed" (!previews = [])

let test_tx_hash_admission_match () =
  let tx_hashes = [String.make 64 'a'; C.raw32_to_hex (raw 'b')] in
  match
    C.tx_hash_admission
      ~expected_tx_list_hash:(C.tx_list_hash tx_hashes)
      ~tx_hashes
  with
  | C.Tx_hash_ok got ->
    expect "hash admission payload" (got = tx_hashes)
  | C.Tx_hash_mismatch -> fail "tx hash admission accept"

let test_tx_hash_admission_mismatch () =
  match
    C.tx_hash_admission
      ~expected_tx_list_hash:(raw 'x')
      ~tx_hashes:[String.make 64 'a']
  with
  | C.Tx_hash_mismatch -> ()
  | C.Tx_hash_ok _ -> fail "tx hash admission reject"

let test_build_header_non_empty () =
  let final_hashes = [String.make 64 'a'; String.make 64 'b'] in
  let receipts_json = ["receipt"] in
  let h =
    C.build_header
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~prev_state_root:(raw 'p')
      ~final_hashes
      ~receipts_json
      ~proposed_state_root:(raw 's')
      ~creator_addr:"oct_creator"
      ~next_txid:10L
      ~head_txid_hi:(Some 7L)
      ~parent_commit_hash:Octra_net.Hash_domain.nil_hash
      ~ts:99.0
  in
  expect "header chain" (h.C_types.chain_id = "octra-test");
  expect "header tx hash" (h.tx_list_hash = C.tx_list_hash final_hashes);
  expect "header receipt root" (h.receipt_root = C_hash.receipt_root receipts_json);
  expect "header txid hi" (h.txid_hi = 11L)

let test_empty_header_head_txid () =
  let h =
    C.build_header
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~prev_state_root:(raw 'p')
      ~final_hashes:[]
      ~receipts_json:[]
      ~proposed_state_root:(raw 's')
      ~creator_addr:"oct_creator"
      ~next_txid:10L
      ~head_txid_hi:(Some 7L)
      ~parent_commit_hash:Octra_net.Hash_domain.nil_hash
      ~ts:99.0
  in
  expect "empty header txid hi" (h.C_types.txid_hi = 7L)

let test_empty_header_next_txid () =
  let h =
    C.build_header
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~prev_state_root:(raw 'p')
      ~final_hashes:[]
      ~receipts_json:[]
      ~proposed_state_root:(raw 's')
      ~creator_addr:"oct_creator"
      ~next_txid:10L
      ~head_txid_hi:None
      ~parent_commit_hash:Octra_net.Hash_domain.nil_hash
      ~ts:99.0
  in
  expect "empty fallback txid hi" (h.C_types.txid_hi = 9L)

let test_build_proposal_envelope () =
  let final_txs = [tx 1; tx 2] in
  let final_hashes = List.map Transaction.hash final_txs in
  let receipts_json = ["r1"; "r2"] in
  let envelope =
    C.build_proposal_envelope
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~prev_state_root:(raw 'p')
      ~final_hashes
      ~final_txs
      ~receipts_json
      ~proposed_state_root:(raw 's')
      ~creator_addr:"oct_creator"
      ~next_txid:10L
      ~head_txid_hi:(Some 7L)
      ~parent_commit:None
      ~ts:99.0
  in
  expect "envelope pid"
    (envelope.C.proposal_id = C_hash.proposal_id envelope.header);
  expect "envelope txid" (envelope.txid_hi = envelope.header.C_types.txid_hi);
  expect "envelope hashes" (envelope.tx_hashes = final_hashes);
  expect "envelope txs" (envelope.txs = final_txs);
  expect "envelope receipts" (envelope.receipts_json = receipts_json);
  expect "envelope frozen header" (envelope.frozen_bundle.header = envelope.header);
  expect "envelope frozen hashes" (envelope.frozen_bundle.tx_hashes = final_hashes);
  expect "envelope frozen txs" (envelope.frozen_bundle.txs = final_txs);
  expect "envelope frozen receipts"
    (envelope.frozen_bundle.receipts_json = receipts_json)

let test_envelope_activation () =
  with_protocol_activation "13" (fun () ->
    let build epoch_id =
      C.build_proposal_envelope
        ~chain_id:"octra-test"
        ~epoch_id
        ~prev_state_root:(raw 'p')
        ~final_hashes:[]
        ~final_txs:[]
        ~receipts_json:[]
        ~proposed_state_root:(raw 's')
        ~creator_addr:"oct_creator"
        ~next_txid:10L
        ~head_txid_hi:(Some 7L)
        ~parent_commit:None
        ~ts:99.0
    in
    let legacy = build 12L in
    let current = build 13L in
    expect
      "legacy envelope before activation"
      (legacy.C.header.proto_version
       = C_types.proto_version_parent_legacy);
    expect
      "legacy envelope has nil parent"
      (Octra_net.Hash_domain.is_nil legacy.header.parent_commit_hash);
    expect
      "current envelope at activation"
      (current.C.header.proto_version = C_types.proto_version_current))

let test_frozen_proposal () =
  let final_txs = [tx 1] in
  let final_hashes = List.map Transaction.hash final_txs in
  let receipts_json = [] in
  let envelope =
    C.build_proposal_envelope
      ~chain_id:"octra-test"
      ~epoch_id:12L
      ~prev_state_root:(raw 'p')
      ~final_hashes
      ~final_txs
      ~receipts_json
      ~proposed_state_root:(raw 's')
      ~creator_addr:"oct_creator"
      ~next_txid:10L
      ~head_txid_hi:(Some 7L)
      ~parent_commit:None
      ~ts:99.0
  in
  let frozen = C.frozen_proposal envelope.C.frozen_bundle in
  expect "frozen header" (frozen.C.header = envelope.header);
  expect "frozen hashes" (frozen.tx_hashes = final_hashes);
  expect "frozen txs" (frozen.txs = final_txs);
  expect "frozen receipts" (frozen.receipts_json = receipts_json);
  expect "frozen pid" (frozen.proposal_id = envelope.proposal_id);
  expect "frozen short" (String.length frozen.proposal_id_short = 16)

let sha_hex s =
  Digestif.SHA256.to_hex (Digestif.SHA256.of_raw_string s)

let test_precommit_sync_missing () =
  match
    C.precommit_sync_plan
      ~proposal_id:(raw 'p')
      ~current_tx_hashes:[]
      ~cached_bundle:None
  with
  | C.Precommit_sync_missing { pid_short } ->
    expect "missing pid short" (String.length pid_short = 16)
  | _ -> fail "precommit missing"

let test_precommit_current_no_parse () =
  match
    C.precommit_sync_plan
      ~proposal_id:(raw 'p')
      ~current_tx_hashes:[]
      ~cached_bundle:(Some ([], [], []))
  with
  | C.Precommit_sync_current -> ()
  | _ -> fail "precommit current"

let test_precommit_sync_decoded () =
  match
    C.precommit_sync_plan
      ~proposal_id:(raw 'p')
      ~current_tx_hashes:[String.make 64 'a']
      ~cached_bundle:(Some ([], [], []))
  with
  | C.Precommit_sync_decoded got ->
    expect "decoded pid short" (String.length got.pid_short = 16);
    expect "decoded hashes" (got.tx_hashes = []);
    expect "decoded txs" (got.txs = []);
    expect "decoded receipts" (got.receipts_json = [])
  | _ -> fail "precommit decoded"

let test_precommit_decode_failure () =
  match
    C.precommit_sync_plan
      ~proposal_id:(raw 'p')
      ~current_tx_hashes:[]
      ~cached_bundle:(Some ([String.make 64 'a'], ["not json"], []))
  with
  | C.Precommit_sync_decode_failed { pid_short; error } ->
    expect "decode failed pid short" (String.length pid_short = 16);
    expect "decode failed error" (String.length error > 0)
  | _ -> fail "precommit decode failed"

let before_precommit_deps ?validator_set ?(current = []) ?cached
    ?(now = 42.0) () =
  let synced = ref [] in
  let unsynced = ref 0 in
  let pending = ref [] in
  let validator_set =
    match validator_set with
    | Some value -> value
    | None ->
      C_types.make_validator_set [
        C_types.{ address = "oct_validator"; pubkey = String.make 32 '\x01' };
      ]
  in
  let deps = C.{
    chain_id = "octra-test";
    validator_set = (fun () -> validator_set);
    current_tx_hashes = (fun () -> current);
    cached_bundle = (fun _ -> cached);
    sync_bundle = (fun ~tx_hashes ~txs ~receipts_json ->
      synced := (tx_hashes, txs, receipts_json) :: !synced);
    mark_unsynced = (fun () ->
      incr unsynced);
    write_pending = (fun entry ->
      pending := entry :: !pending);
    now = (fun () -> now);
  } in
  deps, synced, unsynced, pending

let precommit_wires () =
  let header =
    C_types.{
      proto_version = C_types.proto_version_current;
      chain_id = "octra-test";
      epoch_id = 12L;
      prev_state_root = raw 'r';
      tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [];
      receipt_root = C_hash.receipt_root [];
      proposed_state_root = raw 's';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = "oct_validator";
      txid_hi = 99L;
      ts = 42.0;
    }
  in
  let proposal_id = C_hash.proposal_id header in
  let proposal =
    C_types.{
      chain_id = "octra-test";
      epoch_id = 12L;
      round = 3;
      valid_round = None;
      header;
      tx_hashes = [];
      parent_commit = None;
      proposer = "oct_validator";
      signature = String.make 64 '\x00';
    }
  in
  let vote =
    C_types.{
      chain_id = "octra-test";
      epoch_id = 12L;
      round = 3;
      vote_type = Precommit;
      proposal_id;
      validator = "oct_validator";
      signature = String.make 64 '\x00';
    }
  in
  proposal_id,
  Octra_consensus.C_codec.encode_propose proposal,
  Octra_consensus.C_codec.encode_vote vote

let reproposal_precommit_wires () =
  let validator_set =
    C_types.make_validator_set [
      C_types.{ address = "oct_creator"; pubkey = String.make 32 '\x01' };
      C_types.{ address = "oct_validator"; pubkey = String.make 32 '\x02' };
    ]
  in
  let header =
    C_types.{
      proto_version = C_types.proto_version_current;
      chain_id = "octra-test";
      epoch_id = 12L;
      prev_state_root = raw 'r';
      tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header [];
      receipt_root = C_hash.receipt_root [];
      proposed_state_root = raw 's';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = "oct_creator";
      txid_hi = 99L;
      ts = 42.0;
    }
  in
  let proposal_id = C_hash.proposal_id header in
  let proposal =
    C_types.{
      chain_id = "octra-test";
      epoch_id = 12L;
      round = 1;
      valid_round = Some 0;
      header;
      tx_hashes = [];
      parent_commit = None;
      proposer = "oct_validator";
      signature = String.make 64 '\x00';
    }
  in
  let vote =
    C_types.{
      chain_id = "octra-test";
      epoch_id = 12L;
      round = 1;
      vote_type = Precommit;
      proposal_id;
      validator = "oct_validator";
      signature = String.make 64 '\x00';
    }
  in
  validator_set,
  proposal_id,
  Octra_consensus.C_codec.encode_propose proposal,
  Octra_consensus.C_codec.encode_vote vote

let test_pending_commit_hashes () =
  let proposal_id = raw 'p' in
  let proposed_state_root = raw 's' in
  let entry =
    C.pending_commit
      ~epoch_id:12L
      ~round:3
      ~proposal_id
      ~proposed_state_root
      ~txid_hi:99L
      ~ts:42.0
      ~validator_addr:"oct_validator"
      ~proposal_wire:"proposal"
      ~vote_wire:"vote"
      ~tx_hashes:["hash"]
      ~txs_json:["tx"]
      ~receipts_json:["receipt"]
  in
  expect "pending epoch" (entry.Octra_core.Wal.epoch_id = 12);
  expect "pending round" (entry.round = 3);
  expect "pending proposal hash" (entry.proposal_id = sha_hex proposal_id);
  expect "pending root hash" (entry.proposed_state_root = sha_hex proposed_state_root);
  expect "pending txid" (entry.txid_hi = 99L);
  expect "pending ts" (entry.ts = 42.0);
  expect "pending validator" (entry.validator_addr = "oct_validator");
  expect "pending proposal" (entry.proposal_b64 = Some (Base64.encode_exn "proposal"));
  expect "pending vote" (entry.vote_b64 = Some (Base64.encode_exn "vote"));
  expect "pending hashes" (entry.tx_hashes = ["hash"]);
  expect "pending txs" (entry.txs_json = ["tx"]);
  expect "pending receipts" (entry.receipts_json = ["receipt"])

let test_before_precommit_current () =
  let proposal_id, proposal_wire, vote_wire = precommit_wires () in
  let deps, synced, unsynced, pending =
    before_precommit_deps
      ~cached:([], [], [])
      ()
  in
  let allowed =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:3
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire
  in
  expect "before current allowed" allowed;
  expect "before current no sync" (!synced = []);
  expect "before current no unsync" (!unsynced = 0);
  expect "before current pending" (List.length !pending = 1)

let test_before_precommit_reproposal () =
  let validator_set, proposal_id, proposal_wire, vote_wire =
    reproposal_precommit_wires ()
  in
  let deps, synced, unsynced, pending =
    before_precommit_deps
      ~validator_set
      ~cached:([], [], [])
      ()
  in
  let allowed =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:1
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire
  in
  expect "before reproposal allowed" allowed;
  expect "before reproposal no sync" (!synced = []);
  expect "before reproposal no unsync" (!unsynced = 0);
  expect "before reproposal pending" (List.length !pending = 1)

let test_before_precommit_active_set () =
  let validator_set, proposal_id, proposal_wire, vote_wire =
    reproposal_precommit_wires ()
  in
  let stale =
    C_types.make_validator_set [
      C_types.{ address = "oct_other"; pubkey = String.make 32 '\x03' };
    ]
  in
  let active = ref stale in
  let deps, _, unsynced, pending =
    before_precommit_deps
      ~cached:([], [], [])
      ()
  in
  let deps =
    {
      deps with
      validator_set = (fun () -> !active);
    }
  in
  let run () =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:1
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire
  in
  expect "stale validator set refused" (not (run ()));
  active := validator_set;
  expect "active validator set accepted" (run ());
  expect "dynamic validator set unsynced once" (!unsynced = 1);
  expect "dynamic validator set pending" (List.length !pending = 1)

let test_before_precommit_decoded () =
  let proposal_id, proposal_wire, vote_wire = precommit_wires () in
  let deps, synced, unsynced, pending =
    before_precommit_deps
      ~current:[String.make 64 'a']
      ~cached:([], [], [])
      ()
  in
  let allowed =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:3
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire
  in
  expect "before decoded allowed" allowed;
  expect "before decoded synced" (!synced = [[], [], []]);
  expect "before decoded no unsync" (!unsynced = 0);
  expect "before decoded pending" (List.length !pending = 1)

let test_before_precommit_missing () =
  let proposal_id, proposal_wire, vote_wire = precommit_wires () in
  let deps, synced, unsynced, pending = before_precommit_deps () in
  let allowed =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:3
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire
  in
  expect "before missing refused" (not allowed);
  expect "before missing no sync" (!synced = []);
  expect "before missing unsync" (!unsynced = 1);
  expect "before missing pending" (!pending = [])

let test_precommit_wrong_validator () =
  let proposal_id, proposal_wire, vote_wire = precommit_wires () in
  let vote =
    Octra_consensus.C_codec.decode_vote vote_wire
  in
  let wrong_vote_wire =
    Octra_consensus.C_codec.encode_vote
      { vote with validator = "oct_other" }
  in
  let deps, _, unsynced, pending =
    before_precommit_deps
      ~cached:([], [], [])
      ()
  in
  let allowed =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:3
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire:wrong_vote_wire
  in
  expect "wrong validator refused" (not allowed);
  expect "wrong validator unsynced" (!unsynced = 1);
  expect "wrong validator no pending" (!pending = [])

let test_precommit_write_failure () =
  let proposal_id, proposal_wire, vote_wire = precommit_wires () in
  let deps, _, unsynced, pending =
    before_precommit_deps
      ~cached:([], [], [])
      ()
  in
  let deps =
    {
      deps with
      write_pending = (fun _ -> failwith "write failed");
    }
  in
  let allowed =
    C.handle_before_precommit
      deps
      ~epoch_id:12L
      ~round:3
      ~proposal_id
      ~proposed_state_root:(raw 's')
      ~txid_hi:99L
      ~validator_addr:"oct_validator"
      ~proposal_wire
      ~vote_wire
  in
  expect "write failure refused" (not allowed);
  expect "write failure unsynced" (!unsynced = 1);
  expect "write failure no pending" (!pending = [])

let () =
  test_totals ();
  test_within_limits ();
  test_cap_count ();
  test_cap_bytes ();
  test_cap_ou ();
  test_select_staged_hashes_once ();
  test_local_preverify_ready ();
  test_local_preverify_disabled ();
  test_check_local_bundle ();
  test_validator_preverify_wait ();
  test_cache_retry_order ();
  test_cache_retry_space ();
  test_cache_cancel_job ();
  test_cache_late_failure ();
  test_cache_result_parity ();
  test_reject_forged_heavy_receipt ();
  test_preverify_cap_skip_sample ();
  test_layera_validator_addrs ();
  test_layera_diag_context ();
  test_layera_env_diag_context ();
  test_validator_pubkeys_fallback ();
  test_preview_exec_env ();
  test_admission_plan ();
  test_admission_proceed ();
  test_admission_realign ();
  test_admission_quarantine ();
  test_make_unattested_defer ();
  test_make_epoch_time_defer ();
  test_preverify_single_flight ();
  test_build_head_progress ();
  test_make_reuse_frozen_bundle ();
  test_make_preview_rejections ();
  test_make_rejection_only ();
  test_make_keep_partition ();
  test_make_dependency_chain ();
  test_make_preview_error_defer ();
  test_make_partition_error_defer ();
  test_verify_bundle_success ();
  test_verify_bundle_missing_txs ();
  test_bundle_zero_count_tx ();
  test_bundle_disabled_op ();
  test_bundle_underpriced_tx ();
  test_bundle_receipt_root_error ();
  test_bundle_receipt_decode_error ();
  test_bundle_preverify_failure ();
  test_verify_bundle_limit_failed ();
  test_verify_bundle_bad_signature ();
  test_prev_eic_root_from_head ();
  test_preview_root_match ();
  test_preview_empty_local_root ();
  test_preview_nonempty_no_root ();
  test_preview_decision_error ();
  test_preview_status_of_result ();
  test_proposal_roots ();
  test_prev_root_decision_match ();
  test_prev_root_mismatch_grace ();
  test_prev_root_quarantine ();
  test_prev_root_wait_immediate ();
  test_prev_root_wait_retry ();
  test_prev_root_wait_exhausted ();
  test_preview_plan_reject_retry ();
  test_preview_plan_empty_root ();
  test_preview_plan_error_defer ();
  test_preview_output_reject_retry ();
  test_verify_txid_hi_mismatch ();
  test_verify_prev_root_quarantine ();
  test_verify_missing_prev_time ();
  test_prev_time_retry ();
  test_verify_missing_bundle_wait ();
  test_verify_local_preview ();
  test_verify_head_progress ();
  test_verify_staging_lookup ();
  test_verify_ledger_preverify ();
  test_verify_reproduced_rejection ();
  test_verify_forged_rejection ();
  test_verify_old_valid_round ();
  test_future_reproposal_prior ();
  test_future_reproposal_active ();
  test_verify_invalid_valid_round ();
  test_tx_hash_admission_match ();
  test_tx_hash_admission_mismatch ();
  test_build_header_non_empty ();
  test_empty_header_head_txid ();
  test_empty_header_next_txid ();
  test_build_proposal_envelope ();
  test_envelope_activation ();
  test_frozen_proposal ();
  test_precommit_sync_missing ();
  test_precommit_current_no_parse ();
  test_precommit_sync_decoded ();
  test_precommit_decode_failure ();
  test_pending_commit_hashes ();
  test_before_precommit_current ();
  test_before_precommit_reproposal ();
  test_before_precommit_active_set ();
  test_before_precommit_decoded ();
  test_before_precommit_missing ();
  test_precommit_wrong_validator ();
  test_precommit_write_failure ();
  print_endline "status = pass test = node_runtime_consensus_proposal"