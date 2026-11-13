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
  failwith ("test_proposal: " ^ msg)

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
    verify_tx_signature = (fun ~signing:_ _ ~pubkey:_ -> signature_ok);
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
  expect "layera local address"
    (C.layera_validator_addrs ~env:None ~fallback:"oct_local" = ["oct_local"]);
  expect "layera env parsed sorted"
    (C.layera_validator_addrs
       ~env:(Some "octB:1,bad,octA:2")
       ~fallback:"oct_local" = ["octA"; "octB"]);
  expect "layera invalid env stays empty"
    (C.layera_validator_addrs ~env:(Some "bad") ~fallback:"oct_local" = [])

let test_private_slot_preview () =
  let high = {(tx ~ou:200_000 1) with Transaction.from = "octZ";
    op_type = Transaction.EncryptOp} in
  let low = {(tx ~ou:100_000 1) with Transaction.from = "octA";
    op_type = Transaction.EncryptOp} in
  let suffix = {(tx 2) with Transaction.from = "octA"} in
  let public = {(tx 1) with Transaction.from = "octPublic"} in
  let inputs = [high; low; suffix; public] in
  let run ready =
    Lwt_main.run (C.build_preverify ~private_slots:{fhe = 1; stealth = 1}
      ~limits:(C.limits ~max_txs:10 ~max_bytes:1_000_000 ~max_ou:(Z.of_int 10_000_000))
      ~run_many:(fun _ -> fake_batch ready) inputs) in
  let selected = run inputs in
  expect "preview receives price winner only" (selected.txs = [high; public]);
  let selected = run [low; suffix; public] in
  expect "failed proof releases proposal slot" (selected.txs = [low; suffix; public]);
  expect "private selection hashes bind exact preview"
    (selected.tx_hashes = List.map Transaction.hash selected.txs)

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
      ~fallback:"octLocal"
      ~hash_validators:(fun payload -> "hash:" ^ payload)
      ~meta:(fun key -> "meta:" ^ key)
  in
  expect "layera env addrs" (ctx.C.validator_addrs = ["octA"; "octB"]);
  expect "layera env sha" (ctx.validators_sha = "hash:octA,octB")

let test_validator_pubkeys_local () =
  let calls = ref 0 in
  let pubkeys =
    C.validator_pubkeys
      ~driver:None
      ~fallback:(fun () ->
        incr calls;
        ["octA", "pubA"; "octB", "pubB"])
  in
  expect "validator local called" (!calls = 1);
  expect "validator local pubkeys" (pubkeys = ["octA", "pubA"; "octB", "pubB"])

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

let test_check_plan () =
  let plan ?(state_attested = true) ?(quarantine_active = false)
      ?(quarantine_reason = "") ~epoch_id ~current_epoch () =
    C.check_plan
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
   | _ -> fail "realign old epoch plan");
  (match plan ~epoch_id:11 ~current_epoch:10 () with
   | C.Defer_apply_gap -> ()
   | _ -> fail "apply gap plan");
  (match plan ~epoch_id:10 ~current_epoch:10 () with
   | C.Proceed -> ()
   | _ -> fail "proceed plan")

let test_check_proceed () =
  let started = ref [] in
  let result =
    Lwt_main.run
      (C.handle_proposal_check
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

let test_check_realign () =
  let started = ref [] in
  let result =
    Lwt_main.run
      (C.handle_proposal_check
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

let test_check_quarantine () =
  let started = ref [] in
  let result =
    Lwt_main.run
      (C.handle_proposal_check
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
  holds : string list ref;
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
            { tx = item; error_type = "bad"; reason = "rejected"; charged = false })
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
    holds = ref [];
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
    parent_txs = (fun _ -> None);
    frozen_bundle = (fun _ -> frozen);
    store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
      stored_bundles :=
        (proposal_id, tx_hashes, txs, receipts_json) :: !stored_bundles);
    staging_txs = (fun ?(circles = true) () ->
      if circles then staging else Octra_node_runtime.Circle_refill.without staging);
    evict_preview = (fun ?epoch:_ _ -> failwith "unexpected preview eviction");
    hold_preview = (fun ~epoch:_ tx -> probe.holds := Transaction.hash tx :: !(probe.holds));
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

let test_cell_retry () =
  let module Core = Octra_core in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module Cell = Octra_node_runtime.Consensus_circle_cell_preverify in
  let run = Lwt_main.run in
  Test_workspace.with_dir "cell-proposal" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "store")) in
    let key = "OCTRA_PVAC_VERIFY_WORKER" in
    let prior = Sys.getenv_opt key in
    Unix.putenv key (Filename.concat dir "absent-worker");
    Fun.protect ~finally:(fun () ->
      Unix.putenv key (Option.value prior ~default:""); run (S.close store)) (fun () ->
      let ledger = L.create store in
      let owner = "oct" ^ String.make 44 '1' in
      let circle = "oct" ^ String.make 44 '2' in
      expect "cell account missing" (L.add_account ledger owner (Z.of_int 1_000_000) = Ok ());
      run (L.flush_dirty_lwt ledger);
      run (S.set_pvac_pubkey store owner "cell-test-key");
      run (S.deploy_circle store Core.Circles.{circle_id = circle; runtime = Octb;
        version = 1L; owner; code_hash = zero_hash_hex; stable_root = zero_hash_hex;
        assets_root = zero_hash_hex; privacy_class = Public; browser_mode = Native_sealed;
        resource_mode = Sealed_read; policy_hash = None; members_root = None;
        export_policy = None; limits = default_limits});
      let policy = Hashtbl.create 2 in
      Hashtbl.add policy Core.Circle_hfhe_policy.require_live_key_policy_key "false";
      Hashtbl.add policy Core.Circle_hfhe_policy.encrypt_proof_key "none";
      ignore (run (S.save_circle_stable_storage store circle policy));
      let root = run (L.hash ledger) in
      let root_to_raw32 value =
        match Octra_net.Oce1.hash32_of_hex (Core.Epoch_index_commitment.root_hex value) with
        | Ok value -> value
        | Error reason -> fail reason in
      let cache_root = root_to_raw32 root in
      let runtime = Cell.{store; ledger;
        rules = Core.Rule_graph.create ~chain_id:"octra-test" ~root_at:(fun _ -> Core.Rule_graph.Missing);
        env = (fun ~pre_state_root -> Core.Epoch_exec.{chain_id = "octra-test";
          epoch_id = 12; proposer_addr = owner; validator_addrs = []; validator_pubkeys = [];
          prev_state_root = pre_state_root; epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1})} in
      let message = Yojson.Safe.to_string (`Assoc [
        "state_ref", `String (String.make 64 'a'); "key_id", `String "cell-key";
        "plaintext_hash", `String (String.make 64 'b');
        "ciphertext_commitment", `String (Base64.encode_exn (String.make 32 'c'));
        "amount_commitment", `String (Base64.encode_exn (String.make 32 'd'))]) in
      let ordinary = {(tx 1) with Transaction.from = "oct_other"} in
      List.iter (fun op_type ->
        let cell = {(tx ~message 1) with Transaction.op_type; from = owner; to_ = circle;
          encrypted_data = Some (Base64.encode_exn "cell-cipher")} in
        let plan = run (Core.Circle_cell_transition.prepare ~store ~ledger ~current_epoch:12 cell)
          |> function Ok plan -> plan | Error reason -> fail reason in
        let check verify kind =
          let cache = Cache.create ~cap:4 in
          let calls = ref 0 and online = ref false in
          let build_preverify_once ~state_root ~tx_hashes inputs =
            Cache.run_preverify_once ~epoch:12 cache ~purpose:Cache.Build_proposal
              ~state_root ~tx_hashes ~txs:inputs (fun _ inputs ->
                W.run_many ~registered:false ~keyed:false ~strict:false ~ledger
                  ~field_policy:Core.Private_ledger.Unique_fields
                  ~circle_cell_preverify:(fun ~pre_state_hash ~pre_state_root item ->
                    incr calls;
                    if !online then Lwt.return_ok plan.transition_hash
                    else verify ~pre_state_hash ~pre_state_root item) inputs) in
          let deps, probe = make_proposal_deps ~staging:[cell; ordinary]
            ~ledger_root:root ~build_preverify_once () in
          let selected = run (C.build_preverify ~limits:generous_limits
            ~run_many:(fun inputs -> build_preverify_once ~state_root:cache_root
              ~tx_hashes:(List.map Transaction.hash inputs) inputs) [cell; ordinary]) in
          expect "cell failure blocked ordinary transaction" (selected.txs = [ordinary]);
          expect "cell failure classification differs"
            (match selected.batch.skipped with
             | [skip] -> skip.tx = cell && skip.kind = kind
               && W.checked_cacheable (W.Checked_skip skip) = (kind = W.Invalid)
             | _ -> false);
          expect "cell failure prevented proposal" (Option.is_some (run (C.make_proposal deps
            ~chain_id:"octra-test" ~root_to_raw32 ~limits:generous_limits ~epoch_id:12L)));
          expect "cell failure reached ordinary preview"
            (List.map (fun request -> request.C.txs) !(probe.preview_requests) = [[ordinary]]);
          expect "cell failure cache reuse differs" (!calls = if kind = W.Invalid then 1 else 2);
          if kind = W.Unavailable then begin
            online := true;
            let recovered = run (build_preverify_once ~state_root:cache_root
              ~tx_hashes:(List.map Transaction.hash [cell; ordinary]) [cell; ordinary]) in
            expect "cell retry did not recover" (W.txs recovered = [cell] && !calls = 3);
            let again = run (build_preverify_once ~state_root:cache_root
              ~tx_hashes:(List.map Transaction.hash [cell; ordinary]) [cell; ordinary]) in
            expect "cell receipt cache changed" (again = recovered && !calls = 3)
          end in
        expect "cell did not reach proof worker"
          (run (Cell.run runtime ~pre_state_hash:(W.state_hash root) ~pre_state_root:root cell)
           = Error (`Unavailable "proof worker unavailable: worker_missing"));
        check (Cell.run runtime) W.Unavailable;
        List.iter (fun error ->
          check (fun ~pre_state_hash:_ ~pre_state_root:_ _ -> Lwt.fail error) W.Unavailable)
          [Core.Private_ledger.Worker_retry "second verification busy";
           Core.Exec_resource.Unavailable Host; Failure "cell read failed"];
        check (fun ~pre_state_hash:_ ~pre_state_root:_ _ ->
          Lwt.return_error (`Invalid "proof rejected")) W.Invalid;
        List.iter (fun error ->
          let propagated action =
            let task = try action () with actual -> Lwt.fail actual in
            run (Lwt.try_bind (fun () -> task) (fun _ -> Lwt.return_false)
              (fun actual -> Lwt.return (actual = error))) in
          let verify ~pre_state_hash:_ ~pre_state_root:_ _ = Lwt.fail error in
          let refused = propagated (fun () ->
            W.run ~registered:false ~keyed:false ~strict:false ~ledger
              ~field_policy:Core.Private_ledger.Unique_fields
              ~pre_state_hash:(W.state_hash root) ~pre_state_root:root
              ~circle_cell_preverify:verify cell) in
          expect "active cell failure was swallowed" refused;
          let broken = {runtime with Cell.env = (fun ~pre_state_root:_ -> raise error)} in
          expect "cell callback swallowed active failure"
            (propagated (fun () -> Cell.run broken ~pre_state_hash:(W.state_hash root)
              ~pre_state_root:root cell)))
          [Out_of_memory; Stack_overflow; Lwt.Canceled;
           Core.Exec_resource.Unavailable Memory; Core.Exec_resource.Unavailable Stack];
        expect "cell preverify changed live root" (run (L.hash ledger) = root))
        [Transaction.CircleBalanceCellPut; Transaction.CircleRegisterCellPut]))

let test_circle_budget () =
  let module Lane = Octra_core.Resource_lanes in
  let module Commit = Octra_core.Preverify_commit in
  let budget = Lane.default_budget Lane.Circle_compute in
  let circle ou = {(tx 1) with Transaction.op_type = CircleCall; ou} in
  let ordinary = {(tx 1) with Transaction.from = "oct_other"} in
  let build_preverify_once ~state_root:_ ~tx_hashes:_ inputs =
    match inputs with
    | item :: _ when item.Transaction.op_type = CircleCall -> fake_batch [item]
    | _ -> fake_batch inputs in
  let preview_result request =
    let checked = List.fold_left (fun used item ->
      Result.bind used (fun used -> Commit.check_budget (Commit.create []) used item))
      (Ok []) request.C.txs in
    Result.bind checked (fun _ ->
      exec_result ~confirmed:request.txs ~rejected:[] ~post_state_root:(raw 'l')) in
  let exact = circle budget.max_ou in
  let control, control_probe = make_proposal_deps ~staging:[exact]
    ~build_preverify_once ~preview_result () in
  expect "circle budget control builds" (Option.is_some (run_make_proposal control));
  expect "circle budget control reaches preview"
    (List.map (fun request -> request.C.txs) !(control_probe.preview_requests) = [[exact]]);
  expect "circle budget control enters proposal"
    (!(control_probe.set_proposals) = [[exact], [Transaction.hash exact]]);
  let deps, probe = make_proposal_deps ~staging:[circle (Z.succ budget.max_ou); ordinary]
    ~build_preverify_once ~preview_result () in
  List.iter (fun _ ->
    expect "circle excess budget cannot block proposal" (Option.is_some (run_make_proposal deps))) [0; 1];
  expect "circle excess omitted before preview"
    (List.for_all (fun request -> request.C.txs = [ordinary]) !(probe.preview_requests))

let test_compute_budget () =
  let module Bill = Octra_core.Compute_bill in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (Octra_core.Rule_graph.vm_bytes_activation_for_chain chain_id)).activation_epoch in
  let public_key = Base64.encode_exn (String.make 32 'p') in
  let message = Bill.encode_request ~fuel:100_000 ["selection payload"] in
  let item = {(tx 1) with Transaction.op_type = ComputeSettle;
    from = Octra_core.Crypto.Address.address_from_pubkey public_key;
    to_ = Octra_core.Compute_funding.escrow;
    signature = Base64.encode_exn (String.make 64 '\000');
    public_key = Some public_key; message = Some message; ou = Bill.fee message} in
  let limits = C.limits ~max_txs:100 ~max_bytes:1_000_000 ~max_ou:(Z.of_int 10_000_000) in
  let run height =
    let deps, probe = make_proposal_deps ~current_epoch:height ~staging:[item] () in
    let plan = Lwt_main.run (C.make_proposal deps ~chain_id ~root_to_raw32:Fun.id
      ~limits ~epoch_id:(Int64.of_int height)) in
    expect "compute payment proposal missing" (Option.is_some plan);
    probe in
  let before = run (epoch - 1) in
  expect "compute payment selected before activation"
    (List.for_all (fun request -> request.C.txs = []) !(before.preview_requests));
  let probe = run epoch in
  expect "compute payment reached preview without rule"
    (List.for_all (fun request -> request.C.txs = []) !(probe.preview_requests));
  expect "compute payment reached proposal without rule"
    (List.for_all (fun (txs, hashes) -> txs = [] && hashes = []) !(probe.set_proposals))

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
  expect "make frozen no set" (!(probe.set_proposals) = []);
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

let test_paid_proposal () =
  List.iter (fun prepared ->
  let paid = tx 1 in
  let unpaid = { (tx 1) with from = "oct_other" } in
  let calls = ref 0 in
  let preview request =
    incr calls;
    expect "paid request disappeared" (List.mem paid request.C.txs);
    let rejected = List.map (fun item -> Octra_core.Epoch_exec.{
      tx = item; error_type = "compute_fund_failed"; reason = "input refused";
      charged = item = paid}) request.txs in
    Ok Octra_core.Epoch_exec.{post_state_root = raw 'q'; artifacts = {
      confirmed = []; rejected; confirmed_fees = paid.ou; tx_count = 0}} in
  let deps, probe = make_proposal_deps ~staging:[paid; unpaid]
    ~preview_result:preview () in
  let deps = {deps with C.evict_preview = (fun ?epoch:_ item ->
    expect "paid transaction evicted" (item = unpaid))} in
  let prepare request =
    if !calls >= 2 then Lwt.fail (Octra_core.Exec_resource.Exhausted
      (Transaction.hash paid, Octra_core.Exec_resource.Time))
    else Lwt.return_ok C.{execution = Result.get_ok (preview request);
      preverify = request.preverify} in
  let result = Lwt_main.run (C.make_proposal ?prepare:(if prepared then Some prepare else None)
    ~clock:(fun () -> 0.) ~budget:15. deps ~chain_id:"octra-test"
    ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "paid proposal lost to repeated preparation" (Option.is_some result);
  expect "paid proposal retry count" (!calls = 2);
  match !(probe.stored_bundles) with
  | [_, [], [], receipts] ->
    let module O = Octra_core.Tx_outcome in
    let outcomes = Result.get_ok (O.decode ~confirmed:[] receipts) in
    let inputs = Result.get_ok (O.merge ~confirmed:[] ~rejections:outcomes.rejections) in
    expect "paid receipt missing"
      (List.exists (fun (item : O.rejection) -> item.tx = paid && item.charged) outcomes.rejections);
    expect "paid execution missing" (O.executed ~confirmed:[] outcomes.rejections = [paid]);
    expect "paid verification failed" (Result.is_ok (C.verify_preview_partition
      ~inputs ~confirmed:[] ~rejections:outcomes.rejections (preview { C.epoch_id = 12L;
        epoch_ts = 1.; proposal_id = "paid"; expected_prev_root = raw 'p';
        prev_state_root = raw 'p'; parent_commit = None; proposer = "oct_creator";
        validator_pubkeys = []; preverify = Octra_core.Preverify_commit.create []; txs = inputs })));
    let forged = List.map (fun (item : O.rejection) -> {item with charged = false}) outcomes.rejections in
    expect "unpaid marker accepted" (not (O.equal forged outcomes.rejections))
  | _ -> fail "paid bundle changed") [false; true]

let test_preview_moved () =
  let module E = Octra_core.Epoch_exec in
  let target = tx 1 in
  let other = {(tx 1) with from = "oct_other"} in
  let row ?(kind = "compute_fund_failed") ?(charged = true) reason item =
    E.{tx = item; error_type = kind; reason; charged} in
  let pass confirmed rejected = E.{post_state_root = raw 'q'; artifacts = {
    confirmed = List.map (fun item -> item, 0) confirmed; rejected;
    confirmed_fees = Z.zero; tx_count = List.length confirmed}} in
  let hash = Transaction.hash in
  let both = List.sort String.compare [hash target; hash other] in
  let duplicate = Error ("preview_outcome_invalid:" ^ Result.get_error
    (Octra_core.Tx_outcome.build ~inputs:[target; target] [])) in
  let paid = pass [] [row "input refused" target] in
  let twice = pass [] [row "input refused" target; row "input refused" target] in
  let unpaid reason = pass [target] [row ~charged:false reason other] in
  let cases = [
    "same", paid, paid, Ok [];
    "class", pass [target] [], paid, Ok [hash target];
    "text", paid, pass [] [row "input absent" target], Ok [hash target];
    "pair", pass [] [row ~kind:"a:b" "c" target],
      pass [] [row ~kind:"a" "b:c" target], Ok [hash target];
    "exception", pass [] [row ~kind:"wasm_exception" "one" target],
      pass [] [row ~kind:"wasm_exception" "two" target], Ok [];
    "free", unpaid "one", unpaid "two", Ok [];
    "left", pass [target] [], pass [] [row ~charged:false "one" target], Ok [hash target];
    "right", pass [] [row ~charged:false "one" target], pass [target] [], Ok [hash target];
    "order", pass [target; other] [],
      pass [] [row "input refused" target; row "input refused" other], Ok both;
    "prior", twice, paid, duplicate;
    "result", paid, twice, duplicate] in
  let faults = List.filter_map (fun (name, prior, result, expected) ->
    let actual = C.preview_moved ~prior ~result in
    if actual = expected then None
    else Some (Printf.sprintf "case = %s %s" name (match actual with
      | Ok moved -> Printf.sprintf "count = %d" (List.length moved)
      | Error error -> "error = " ^ error))) cases in
  if faults <> [] then fail ("preview moved: " ^ String.concat "; " faults)

let test_paid_change () =
  let module E = Octra_core.Epoch_exec in
  let module O = Octra_core.Tx_outcome in
  let module Work = Octra_core.Exec_resource in
  let target = tx 1 in
  let free = {(tx 1) with from = "oct_free"} in
  let later = {(tx 2) with from = "oct_free"} in
  let extra = {(tx 1) with from = "oct_extra"} in
  let preview mode request =
    let txs = request.C.txs in
    let joint = List.mem free txs in
    let near = List.mem extra txs in
    let cut = mode = `Cut && near && not joint
      && Work.defer (Transaction.hash extra) Work.Host in
    let refuse reason item = E.{tx = item; error_type = "compute_fund_failed";
      reason; charged = item = target} in
    let rejected = List.filter_map (fun item ->
      if item = target then
        match mode, joint with
        | (`Joint | `Reserve), false | `Alone, true -> Some (refuse "input refused" item)
        | `Text, _ -> Some (refuse (if joint then "input refused" else "input absent") item)
        | _ -> None
      else if item = extra || (mode = `Cut && not near) then None
      else Some (refuse "sender refused" item)) txs in
    let confirmed = List.filter (fun item -> not (cut && item = extra)
      && not (List.exists (fun (row : E.tx_reject) -> row.tx = item) rejected)) txs
      |> Transaction.consensus_order in
    Ok E.{post_state_root = raw 'q'; artifacts = {
      confirmed = List.map (fun item -> item, 0) confirmed; rejected;
      confirmed_fees = Z.zero; tx_count = List.length confirmed}} in
  let run (mode, name, staging) prepared =
    let preview = preview mode in
    let deps, probe = make_proposal_deps ~staging ~preview_result:preview () in
    let deps = {deps with C.evict_preview = (fun ?epoch:_ item ->
      expect "paid change evicted moved transaction" (item = free))} in
    let prepare request = Lwt.return_ok C.{execution = Result.get_ok (preview request);
      preverify = request.preverify} in
    let result = Lwt_main.run (C.make_proposal ?prepare:(if prepared then Some prepare else None)
      ~clock:(fun () -> 0.) ~budget:15. deps ~chain_id:"octra-test"
      ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
    let label = Printf.sprintf "mode = %s prepared = %b" name prepared in
    let held hash = List.mem hash !(probe.holds) in
    match result, !(probe.stored_bundles) with
    | Some _, [_, _, txs, receipts] ->
      let outcomes = Result.get_ok (O.decode ~confirmed:txs receipts) in
      let inputs = Result.get_ok (O.merge ~confirmed:txs ~rejections:outcomes.rejections) in
      let verdict = C.verify_preview_partition ~inputs ~confirmed:txs
        ~rejections:outcomes.rejections (preview { C.epoch_id = 12L;
          epoch_ts = 1.; proposal_id = "paid"; expected_prev_root = raw 'p';
          prev_state_root = raw 'p'; parent_commit = None; proposer = "oct_creator";
          validator_pubkeys = []; preverify = Octra_core.Preverify_commit.create []; txs = inputs }) in
      let rows = List.map (fun (row : O.rejection) -> row.tx, row.charged) outcomes.rejections in
      let shape = match mode with
        | `Cut -> txs = [free; target] && rows = [] && held (Transaction.hash extra)
        | `Reserve -> txs = [] && rows = [target, true] && held (Transaction.hash free)
        | _ -> txs = [] && rows = [free, false] in
      (match verdict with
       | Error error -> [label ^ " verdict = " ^ error]
       | Ok () when held (Transaction.hash target) -> [label ^ " verdict = target_held"]
       | Ok () when not shape -> [label ^ " verdict = content_differs"]
       | Ok () -> [])
    | _ -> [label ^ " verdict = proposal_missing"] in
  let pair = [free; target] in
  let cases = [`Joint, "joint", pair; `Alone, "alone", pair; `Text, "text", pair;
    `Cut, "cut", [extra; free; target]] in
  let faults = List.concat_map (fun case -> List.concat_map (run case) [false; true]) cases
    @ run (`Reserve, "reserve", [free; later; target]) true in
  if faults <> [] then fail ("paid change: " ^ String.concat "; " faults)

let test_refill_saved () =
  let module R = Octra_core.Exec_resource in
  let run mode =
    let first = tx 1 in
    let refused = {(tx 1) with from = "oct_refused"} in
    let extra = {(tx 1) with from = "oct_extra"} in
    let calls = ref 0 in
    let moved = ref false in
    let live = ref true in
    let after field before changed () =
      if !moved && mode = field then changed else before in
    let base, probe = make_proposal_deps ~staging:[first; refused; extra] () in
    let conflict = C.build_proposal_envelope ~chain_id:"octra-test" ~epoch_id:12L
      ~prev_state_root:(raw 'p') ~final_hashes:[] ~final_txs:[] ~receipts_json:[]
      ~proposed_state_root:(raw 'z') ~creator_addr:"oct_creator"
      ~next_txid:7L ~head_txid_hi:(Some 6L) ~parent_commit:None ~ts:99. in
    let deps = {base with C.current = (fun () -> !live);
      current_epoch = after `Epoch 12 13;
      current_round = after `Round 3 4;
      state_attested = after `Attested true false;
      quarantine_active = after `Quarantine false true;
      cached_head = after `Head None (Some (proposal_head (raw 'h')));
      prev_eic_root = after `Eic (base.prev_eic_root ()) (raw 'e');
      next_txid = after `Next 7L 8L;
      head_txid_hi = after `High (Some 6L) (Some 7L);
      proposer = after `Proposer "oct_creator" "oct_changed";
      validator_pubkeys = (fun _ -> after `Members
        ["oct_validator", "pub"] ["oct_other", "pub"] ());
      parent_commit = (fun ~epoch_id:_ -> after `Parent (Ok None) (Error "parent changed") ());
      previous_epoch_ts = (fun _ -> after `Previous (Some 89.) (Some 90.) ());
      frozen_bundle = (fun _ -> after `Frozen None (Some conflict.frozen_bundle) ());
      read_prev_ledger_root = (fun () ->
        let open Lwt.Syntax in
        let* () = if !moved && mode = `ReadCancel then
            let+ () = Lwt.pause () in live := false
          else Lwt.return_unit in
        if !moved && mode = `ReadError then Lwt.fail (Failure "root read failed")
        else if !moved && mode = `NoLedger then Lwt.return_none
        else Lwt.return (after `Root (Some (raw 'p')) (Some (raw 'r')) ()));
      evict_preview = (fun ?epoch:_ item -> expect "refill evicted completed work" (item = refused))} in
    let prepare (request : C.build_preview_request) =
      incr calls;
      let result confirmed rejected root =
        Lwt.return_ok C.{execution = Result.get_ok
          (exec_result ~confirmed ~rejected ~post_state_root:root);
          preverify = request.C.preverify} in
      match !calls with
      | 1 ->
        expect "refill initial selection" (request.txs = [first; refused]);
        result [first] [refused] (raw 'l')
      | 2 ->
        expect "refill stable selection" (request.txs = [first]);
        result [first] [] (raw 'q')
      | 3 ->
        expect "refill extension selection" (request.txs = [first; extra]);
        expect "refill published before selection"
          (!(probe.stored_bundles) = [] && !(probe.frozen_writes) = []);
        moved := true;
        (match mode with
        | `Success -> result request.txs [] (raw 'w')
        | `Error -> Lwt.return_error "extension refused"
        | `Cancelled -> Lwt.fail Lwt.Canceled
        | `Exception -> Lwt.fail (Failure "extension failed")
        | `Host -> Lwt.fail (R.Unavailable Host)
        | `Memory -> Lwt.fail (R.Unavailable Memory)
        | `Stack -> Lwt.fail (R.Unavailable Stack)
        | `Context -> live := false; Lwt.fail (R.Unavailable Time)
        | _ -> Lwt.fail (R.Exhausted (Transaction.hash first, R.Time)))
      | _ -> fail "refill repeated completed work" in
    let result = Lwt_main.run (Lwt.try_bind (fun () ->
      C.make_proposal ~prepare ~clock:(fun () ->
        if !moved && mode = `AcceptClosed then 6. else after `Deadline 0. 20. ()) ~budget:15. deps
        ~chain_id:"octra-test" ~root_to_raw32:Fun.id
        ~limits:{generous_limits with C.max_txs = 2} ~epoch_id:12L)
      (fun value -> Lwt.return_ok value) (fun error -> Lwt.return_error error)) in
    expect "refill count differs" (!calls = 3);
    expect "refill held completed transaction" (!(probe.holds) = []);
    match mode, result with
    | (`Time | `AcceptClosed | `Success), Ok (Some plan) ->
      let items, root = if mode = `Success then [first; extra], raw 'w' else [first], raw 'q' in
      let hashes = List.map Transaction.hash items in
      let _, eic = Octra_core.Epoch_index_commitment.next_root_from_hashes
        ~prev:(base.prev_eic_root ()) ~epoch_id:12 ~start_txid:7L hashes in
      let expected = Octra_core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:root ~epoch_index_root:eic in
      expect "refill retained wrong root" (plan.header.proposed_state_root = expected);
      expect "refill retained wrong transactions" (plan.tx_hashes = hashes);
      expect "refill publication count"
        (List.length !(probe.stored_bundles) = 1 && List.length !(probe.frozen_writes) = 1
         && !(probe.set_proposals) = [items, hashes]);
      let module O = Octra_core.Tx_outcome in
      let outcomes = if mode = `Success then [] else Result.get_ok
        (O.build ~inputs:[first; refused] [refused, "bad", "rejected"]) in
      let receipts = O.encode [] outcomes in
      let expected = C.build_proposal_envelope ~chain_id:"octra-test" ~epoch_id:12L
        ~prev_state_root:(raw 'p') ~final_hashes:hashes ~final_txs:items ~receipts_json:receipts
        ~proposed_state_root:expected ~creator_addr:"oct_creator" ~next_txid:7L
        ~head_txid_hi:(Some 6L) ~parent_commit:None ~ts:99. in
      expect "refill changed checked envelope" (plan.header = expected.header);
      expect "refill changed receipt bytes"
        (!(probe.stored_bundles) = [expected.proposal_id, hashes, items, receipts]);
      let key = Cache.freeze_key ~epoch_id:12L ~round:3 in
      expect "refill changed freeze identity" (!(probe.frozen_writes) = [key, expected.frozen_bundle])
    | (`Time | `AcceptClosed), _ -> fail "refill discarded checked proposal"
    | `Success, _ -> fail "refill extension missing"
    | `Cancelled, Error Lwt.Canceled ->
      expect "refill cancellation published" (!(probe.stored_bundles) = [] && !(probe.frozen_writes) = [])
    | `Exception, Error (Failure reason) when reason = "extension failed" ->
      expect "refill failure published" (!(probe.stored_bundles) = [] && !(probe.frozen_writes) = [])
    | `ReadError, Error (Failure reason) when reason = "root read failed" ->
      expect "refill unreadable root published" (!(probe.stored_bundles) = [] && !(probe.frozen_writes) = [])
    | (`Error | `Host | `Memory | `Stack | `Context | `Deadline | `Epoch | `Round
      | `Attested | `Quarantine | `Head | `Eic | `Next | `High | `Proposer
      | `Members | `Parent | `Previous | `Root | `ReadCancel | `NoLedger | `Frozen), Ok None ->
      expect "refill changed context published"
        (!(probe.set_proposals) = [] && !(probe.stored_bundles) = [] && !(probe.frozen_writes) = [])
    | _ -> fail "refill result differs" in
  List.iter run [`Time; `AcceptClosed; `Success; `Error; `Cancelled; `Exception; `Host; `Memory;
    `Stack; `Context; `Deadline; `Epoch; `Round; `Attested; `Quarantine; `Head;
    `Eic; `Next; `High; `Proposer; `Members; `Parent; `Previous; `Root; `ReadCancel;
    `ReadError; `NoLedger; `Frozen]

let test_publish_failure () =
  List.iter (fun phase ->
    let base, probe = make_proposal_deps ~staging:[tx 1] () in
    let calls = ref [] in
    let publish step action =
      calls := step :: !calls;
      if phase = step then raise (Octra_core.Exec_resource.Unavailable Time);
      action () in
    let deps = {base with C.set_proposal = (fun txs hashes ->
        publish `Set (fun () -> base.set_proposal txs hashes));
      store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
        publish `Store (fun () -> base.store_bundle ~proposal_id ~tx_hashes ~txs ~receipts_json));
      freeze = (fun key value -> publish `Freeze (fun () -> base.freeze key value))} in
    let propagated = try ignore (run_make_proposal deps); false with
      | Octra_core.Exec_resource.Unavailable Time -> true in
    expect "publication failure hidden" propagated;
    let expected = match phase with
      | `Set -> [`Set] | `Store -> [`Store; `Set] | `Freeze -> [`Freeze; `Store; `Set] in
    expect "publication failure retried" (!calls = expected);
    expect "publication failure wrote freeze" (!(probe.frozen_writes) = [])) [`Set; `Store; `Freeze]

let test_private_apply_slot () =
  let item key ou =
    let public_key = Base64.encode_exn (String.make 32 key) in
    {(tx ~ou 1) with Transaction.op_type = EncryptOp;
      from = Octra_core.Crypto.Address.address_from_pubkey public_key;
      signature = Base64.encode_exn (String.make 64 '\000');
      public_key = Some public_key} in
  let high = item 'a' 200_000 and low = item 'b' 100_000 in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (Octra_core.Rule_graph.tx_envelope_activation_for_chain chain_id)).activation_epoch in
  let deps, probe = make_proposal_deps ~current_epoch:epoch ~staging:[high; low]
    ~preview_result:(fun request ->
      let rejected, confirmed = List.partition (fun tx -> tx = high) request.C.txs in
      let result = Result.get_ok (exec_result ~confirmed ~rejected ~post_state_root:(raw 'q')) in
      let reason = if List.mem low request.C.txs then "fhe_epoch_cap" else "rejected" in
      Ok {result with artifacts = {result.artifacts with rejected =
        List.map (fun r -> {r with Octra_core.Epoch_exec.reason}) result.artifacts.rejected}}) () in
  let plan = Lwt_main.run (C.make_proposal ~private_slots:{fhe = 1; stealth = 1}
    deps ~chain_id ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:(Int64.of_int epoch)) in
  expect "apply rejection consumed private slot"
    (match plan with Some plan -> plan.tx_hashes = [Transaction.hash low] | None -> false);
  match !(probe.stored_bundles) with
  | [_, _, txs, receipts] ->
    (match Octra_core.Tx_outcome.decode ~confirmed:txs receipts with
    | Ok partition ->
      expect "apply rejection evidence lost"
        (List.map (fun r -> r.Octra_core.Tx_outcome.tx) partition.rejections = [high]);
      expect "refill rejection differs from replay"
        ((List.hd partition.rejections).reason = "fhe_epoch_cap")
    | Error reason -> fail reason)
  | _ -> fail "private proposal missing"

let test_private_refill_cap () =
  let item key ou op_type =
    let public_key = Base64.encode_exn (String.make 32 key) in
    {(tx ~ou 1) with Transaction.op_type;
      from = Octra_core.Crypto.Address.address_from_pubkey public_key;
      signature = Base64.encode_exn (String.make 64 '\000');
      public_key = Some public_key} in
  let first = item 'a' 300_000 EncryptOp in
  let second = item 'b' 200_000 EncryptOp in
  let public = item 'c' 100_000 Standard in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (Octra_core.Rule_graph.tx_envelope_activation_for_chain chain_id)).activation_epoch in
  let deps, probe = make_proposal_deps ~current_epoch:epoch ~staging:[first; second; public]
    ~preview_result:(fun request ->
      let rejected, confirmed = List.partition (fun tx -> tx = public) request.C.txs in
      exec_result ~confirmed ~rejected ~post_state_root:(raw 'q')) () in
  let limits = {generous_limits with C.max_txs = 2} in
  let plan = Lwt_main.run (C.make_proposal ~private_slots:{fhe = 1; stealth = 1}
    deps ~chain_id ~root_to_raw32:Fun.id ~limits ~epoch_id:(Int64.of_int epoch)) in
  expect "refill cap lost original proposal" (Option.is_some plan);
  match !(probe.stored_bundles) with
  | [_, _, txs, receipts] ->
    let partition = Result.get_ok (Octra_core.Tx_outcome.decode ~confirmed:txs receipts) in
    expect "refill cap lost rejected input"
      (List.map (fun r -> r.Octra_core.Tx_outcome.tx) partition.rejections = [public])
  | _ -> fail "refill cap proposal missing"

let test_private_refill_steps () =
  let item key ou op_type =
    let public_key = Base64.encode_exn (String.make 32 key) in
    {(tx ~ou 1) with Transaction.op_type;
      from = Octra_core.Crypto.Address.address_from_pubkey public_key;
      signature = Base64.encode_exn (String.make 64 '\000');
      public_key = Some public_key} in
  let private_txs = List.init 4 (fun i ->
    item (Char.chr (97 + i)) (500_000 - i * 100_000) EncryptOp) in
  let public = item 'p' 100_000 Standard in
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = (Option.get (Octra_core.Rule_graph.tx_envelope_activation_for_chain chain_id)).activation_epoch in
  let calls = ref 0 in
  let deps, probe = make_proposal_deps ~current_epoch:epoch ~staging:(private_txs @ [public])
    ~preview_result:(fun request ->
      incr calls;
      let private_txs = List.filter (fun item -> item <> public) request.C.txs in
      let accepted = match private_txs with first :: _ :: _ -> Some first | _ -> None in
      let confirmed, rejected = List.partition
        (fun item -> item = public || Some item = accepted) request.C.txs in
      exec_result ~confirmed ~rejected ~post_state_root:(raw 'q')) () in
  let plan = Lwt_main.run (C.make_proposal ~private_slots:{fhe = 1; stealth = 1}
    deps ~chain_id ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:(Int64.of_int epoch)) in
  expect "refill exhausted preview budget"
    (match plan with Some plan -> plan.tx_hashes = [Transaction.hash public] | None -> false);
  expect "refill transition count differs" (!calls = 13);
  match !(probe.stored_bundles) with
  | [_, _, txs, receipts] ->
    let partition = Result.get_ok (Octra_core.Tx_outcome.decode ~confirmed:txs receipts) in
    expect "refill evidence differs"
      (List.map (fun r -> r.Octra_core.Tx_outcome.tx) partition.rejections = [List.nth private_txs 3])
  | _ -> fail "refill proposal missing"

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
    let inputs =
      match Octra_core.Tx_outcome.merge ~confirmed ~rejections:outcomes.rejections with
      | Ok value -> value
      | Error reason -> fail reason
    in
    begin
      match C.verify_preview_partition
        ~inputs ~confirmed ~rejections:outcomes.rejections (preview inputs) with
      | Ok () -> ()
      | Error reason -> fail ("built partition rejected: " ^ reason)
    end;
    expect "dependent transaction deferred" (not (List.mem dependent inputs));
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
        let inputs =
          get (Octra_core.Tx_outcome.merge ~confirmed ~rejections:outcomes.rejections)
        in
        get (C.verify_preview_partition ~inputs ~confirmed
          ~rejections:outcomes.rejections (preview inputs));
        get (C.verify_preview_partition ~inputs:confirmed ~confirmed
          ~rejections:[] (preview confirmed));
        expect "chain independent retained" (confirmed = [independent]);
        expect "chain rejected positions"
          (List.map (fun (item : Octra_core.Tx_outcome.rejection) -> item.tx)
             outcomes.rejections
           = List.filter (fun item -> item.Transaction.nonce mod 2 = 1) chain);
        expect "chain staging unchanged" (deps.C.staging_txs () = staged)
      | _ -> fail "chain bundle missing")
    [1; 2; 3; 4; 8; 16]

let test_preview_resources () =
  let module Work = Octra_core.Exec_resource in
  let module Wiring = Octra_node_runtime.Consensus_driver_wiring in
  let first = tx 1 in
  let second = { (tx 1) with Transaction.from = "oct_other" } in
  List.iter (fun cause ->
    List.iter (fun failed ->
      let inputs = Transaction.consensus_order [first; second] in
      let deps, probe = make_proposal_deps ~staging:inputs () in
      let previews = ref 0 in
      let deps = {deps with C.preview = (fun request ->
        incr previews;
        Wiring.preview_with_optional_catch ~catch_exn:true ~warn:ignore (fun () ->
          match List.find_opt (fun tx -> List.mem tx failed) request.C.txs with
          | Some tx -> Work.run ~hash:(Transaction.hash tx) (fun () -> Lwt.fail cause)
          | None -> deps.preview request))} in
      expect "resource failure stops proposal" (Option.is_some (run_make_proposal deps));
      expect "resource retries differ" (!previews = List.length failed + 1);
      expect "resource failure changes staging" (deps.staging_txs () = inputs);
      match !(probe.stored_bundles) with
      | [_, _, confirmed, receipts] ->
        expect "resource failure changes retained input"
          (confirmed = List.filter (fun tx -> not (List.mem tx failed)) inputs);
        let outcome = Octra_core.Tx_outcome.decode ~confirmed receipts |> Result.get_ok in
        expect "resource failure becomes consensus rejection" (outcome.rejections = []);
        expect "resource failure publishes extra proposal"
          (List.length !(probe.frozen_writes) = 1)
      | _ -> fail "resource proposal missing")
      [[first]; [second]; [first; second]]) [Out_of_memory; Stack_overflow];
  let deps, probe = make_proposal_deps ~staging:[first] () in
  let deps = {deps with C.preview = (fun _ -> Lwt.fail (Work.Exhausted ("unknown", Work.Memory)))} in
  expect "unknown resource identity accepted" (run_make_proposal deps = None);
  expect "unknown resource identity published" (!(probe.stored_bundles) = []);
  let raised = try
    ignore (Lwt_main.run (Work.run ~hash:"tx" (fun () -> Lwt.fail Exit))); false
    with Exit -> true in
  expect "unrelated failure reinterpreted" raised

let test_preview_time () =
  let module Work = Octra_core.Exec_resource in
  let first = tx 1 in
  let slow = {(tx 1) with Transaction.from = "oct_slow"} in
  let last = {(tx 1) with Transaction.from = "oct_last"} in
  let inputs = Transaction.consensus_order [first; slow; last] in
  let deps, probe = make_proposal_deps ~staging:inputs () in
  let removed = ref [] in
  let prior = deps.preview in
  let deps = {deps with C.preview = (fun request ->
    if List.mem slow request.C.txs && not (Array.exists (String.equal "selection") Sys.argv)
    then Work.run ~hash:(Transaction.hash slow) (fun () -> Lwt.fail (Work.Unavailable Work.Time))
    else prior request);
    evict_preview = (fun ?epoch:_ item -> removed := item :: !removed)} in
  expect "time limit stopped proposal" (Option.is_some (run_make_proposal deps));
  expect "local time refusal removed transaction" (!removed = []);
  match !(probe.stored_bundles) with
  | [_, _, confirmed, receipts] ->
    expect "time limit removed healthy transaction"
      (confirmed = List.filter ((<>) slow) inputs);
    let outcome = Octra_core.Tx_outcome.decode ~confirmed receipts |> Result.get_ok in
    expect "time limit produced consensus rejection" (outcome.rejections = [])
  | _ -> fail "time limited proposal missing"

let test_preview_duration () =
  let module Work = Octra_core.Exec_resource in
  let item = tx 1 in
  List.iter (fun duration -> List.iter (fun worker ->
    let now = ref 0. in
    let deps, probe = make_proposal_deps ~staging:[item] () in
    let action () = now := !now +. duration; () in
    let deps = {deps with C.preview = (fun request ->
      if request.C.txs = [] then deps.preview request else
      Work.run ~hash:(Transaction.hash item) (fun () ->
        let open Lwt.Syntax in
        let* () = if worker then
            Work.detach ~launch:(fun work input -> Lwt.return (work input)) action ()
          else Lwt.return (Work.work action) in
        deps.preview request));
      evict_preview = (fun ?epoch:_ _ -> failwith "elapsed time evicted transaction")} in
    let plan = Lwt_main.run (C.make_proposal ~budget:40. ~clock:(fun () -> !now) deps
      ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
    expect "elapsed time prevented proposal" (Option.is_some plan);
    expect "elapsed time excluded transaction" ((Option.get plan).tx_hashes = [Transaction.hash item]);
    expect "elapsed time changed staging" (deps.staging_txs () = [item]);
    expect "elapsed time retried completed work" (List.length !(probe.preview_requests) = 1))
    [false; true]) [1.; 8.5]

let test_build_budget () =
  let module Work = Octra_core.Exec_resource in
  let inputs = List.init 4 (fun i -> {(tx 1) with Transaction.from = "oct" ^ string_of_int i}) in
  let now = ref 0. in
  let attempts = ref 0 in
  let deps, probe = make_proposal_deps ~staging:inputs () in
  let deps = {deps with C.preview = (fun request ->
    incr attempts;
    match request.C.txs with
    | [] -> deps.preview request
    | item :: _ -> Work.run ~hash:(Transaction.hash item) (fun () ->
        now := !now +. 4.;
        Work.work (fun () -> raise (Work.Unavailable Work.Host))))} in
  let result = Lwt_main.run (C.make_proposal ~budget:10. ~clock:(fun () -> !now) deps
    ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "proposal deadline restarted" (result = None && !attempts = 2);
  expect "expired proposal published" (!(probe.stored_bundles) = [] && !(probe.frozen_writes) = []);
  expect "deadline changed staging" (deps.staging_txs () = inputs);
  let pending, _ = Lwt.task () in
  let cancelled = ref false in
  Lwt.on_cancel pending (fun () -> cancelled := true);
  let deps, probe = make_proposal_deps ~staging:inputs
    ~build_preverify_once:(fun ~state_root:_ ~tx_hashes:_ _ -> pending) () in
  let result = Lwt_main.run (C.make_proposal ~budget:0.05 deps
    ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "precheck ignored deadline" (result = None && !cancelled);
  expect "precheck timeout published" (!(probe.set_proposals) = []);
  List.iter (fun budget ->
    expect "invalid proposal duration accepted" (try
      ignore (C.make_proposal ~budget deps ~chain_id:"octra-test"
        ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L); false
    with Invalid_argument _ -> true)) [0.; -1.; nan; infinity]

let test_host_budget () =
  let module Work = Octra_core.Exec_resource in
  let slow = tx 1 in
  let healthy = {slow with Transaction.from = "oct_healthy"} in
  let deps, probe = make_proposal_deps ~staging:[slow; healthy] () in
  let cancelled = ref false in
  let deps = {deps with C.preview = (fun request ->
    if not (List.mem slow request.C.txs) then deps.preview request else
    Work.run ~hash:(Transaction.hash slow) (fun () -> Work.interrupt (fun () ->
      let pending, _ = Lwt.task () in
      Lwt.on_cancel pending (fun () -> cancelled := true);
      pending)))} in
  let result = Lwt_main.run (C.make_proposal ~budget:0.2 deps
    ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "host deadline omitted healthy work"
    (Option.map (fun (plan : Octra_consensus.C_driver.proposal_plan) -> plan.tx_hashes) result = Some [Transaction.hash healthy]);
  expect "host deadline did not cancel" !cancelled;
  expect "host deadline did not hold" (List.mem (Transaction.hash slow) !(probe.holds));
  expect "host deadline changed staging" (deps.staging_txs () = [slow; healthy]);
  match !(probe.stored_bundles) with
  | [_, _, confirmed, receipts] ->
    let outcome = Octra_core.Tx_outcome.decode ~confirmed receipts |> Result.get_ok in
    expect "host time produced rejection" (outcome.rejections = [])
  | _ -> fail "host deadline result absent"

let test_proposal_prefix () =
  let module Work = Octra_core.Exec_resource in
  let inputs = List.init 4 (fun i -> {(tx 1) with Transaction.from = "oct" ^ string_of_int i})
    |> Transaction.consensus_order in
  let now = ref 0. in
  let deps, probe = make_proposal_deps ~staging:inputs () in
  let deps = {deps with C.preview = (fun request ->
    let rec take = function
      | tx :: rest when Work.next () -> now := !now +. 2.; tx :: take rest
      | _ -> [] in
    let confirmed = take request.C.txs in
    Lwt.return (exec_result ~confirmed ~rejected:[] ~post_state_root:(raw 'l')))} in
  let result = Lwt_main.run (C.make_proposal ~budget:15. ~clock:(fun () -> !now) deps
    ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "completed prefix omitted" (Option.map (fun (plan : Octra_consensus.C_driver.proposal_plan) ->
    plan.tx_hashes) result = Some (List.map Transaction.hash (List.filteri (fun i _ -> i < 3) inputs)));
  expect "prefix replayed work" (!now = 6.);
  expect "prefix held unstarted work" (!(probe.holds) = []);
  expect "prefix changed staging" (deps.staging_txs () = inputs)

let test_preview_selection () =
  let module Work = Octra_core.Exec_resource in
  let inputs = List.init 4 (fun i -> {(tx 1) with Transaction.from = "oct" ^ string_of_int i}) in
  let slow = List.filteri (fun i _ -> i < 2) inputs in
  let healthy = List.filteri (fun i _ -> i >= 2) inputs in
  let deps, probe = make_proposal_deps ~staging:inputs () in
  let calls = ref 0 in
  let deps = {deps with C.preview = (fun request ->
    incr calls;
    let confirmed = List.filter (fun item ->
      if not (Work.next ~hash:(Transaction.hash item) ()) then false
      else if List.mem item slow then begin
        expect "selection defer missing" (Work.defer (Transaction.hash item) Work.Time);
        false
      end else true) request.C.txs in
    Lwt.return (exec_result ~confirmed ~rejected:[] ~post_state_root:(raw 'l')))} in
  let result = run_make_proposal deps in
  expect "selection lost healthy senders" (Option.map (fun (plan : C_driver.proposal_plan) ->
    plan.tx_hashes) result = Some (List.map Transaction.hash healthy));
  expect "selection replayed completed work" (!calls = 1);
  expect "selection held another sender"
    (List.sort String.compare !(probe.holds) = List.sort String.compare (List.map Transaction.hash slow));
  expect "selection changed staging" (deps.staging_txs () = inputs)

let test_preview_replay () =
  let module Work = Octra_core.Exec_resource in
  let inputs = List.init 4 (fun i -> {(tx 1) with Transaction.from = "oct" ^ string_of_int i}) in
  let refused = List.hd inputs in
  let healthy = List.nth inputs 1 in
  let now = ref 0. in
  let calls = ref 0 in
  let deps, probe = make_proposal_deps ~staging:inputs () in
  let deps = {deps with C.preview = (fun request ->
    incr calls;
    let selected = List.filter (fun item ->
      if Work.next ~hash:(Transaction.hash item) () then begin
        now := !now +. (if !calls = 1 then 3. else 0.5);
        true
      end else false) request.C.txs in
    Lwt.return (exec_result ~confirmed:(List.filter ((<>) refused) selected)
      ~rejected:(List.filter ((=) refused) selected) ~post_state_root:(raw 'l')))} in
  let result = Lwt_main.run (C.make_proposal ~budget:15. ~clock:(fun () -> !now) deps
    ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "replay omitted completed work" (Option.map (fun (plan : C_driver.proposal_plan) ->
    plan.tx_hashes) result = Some [Transaction.hash healthy]);
  expect "replay admitted unseen work" (!calls = 2 && !now = 6.5);
  expect "replay held unstarted work" (!(probe.holds) = []);
  expect "replay changed staging" (deps.staging_txs () = inputs)

let test_preview_expiry () =
  let module Work = Octra_core.Exec_resource in
  let item = tx 1 in
  List.iter (fun elapsed ->
    let now = ref 0. and current = ref true in
    let deps, probe = make_proposal_deps ~staging:[item] () in
    let deps = {deps with C.current = (fun () -> !current);
      preview = (fun request ->
        Lwt.catch (fun () -> Work.run ~hash:(Transaction.hash item) (fun () ->
          if elapsed then now := 20. else current := false;
          Work.work (fun () -> ());
          deps.preview request)) (function
          | Work.Exhausted (hash, resource) ->
            expect "expiry defer absent" (Work.defer hash resource);
            deps.preview {request with C.txs = []}
          | error -> Lwt.fail error))} in
    let result = Lwt_main.run (C.make_proposal ~budget:15. ~clock:(fun () -> !now) deps
      ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
    expect "expired preview published" (result = None && !(probe.stored_bundles) = []);
    expect "expired preview held transaction" (!(probe.holds) = [])) [false; true];
  List.iter (fun error_type ->
    let refused = {item with Transaction.from = "oct_refused"} in
    let deps, probe = make_proposal_deps ~staging:[item; refused] () in
    let calls = ref 0 in
    let prepare request =
      incr calls;
      let confirmed = List.filter ((<>) refused) request.C.txs in
      let rejected = List.filter ((=) refused) request.C.txs in
      let execution = Result.get_ok (exec_result ~confirmed ~rejected ~post_state_root:(raw 'l')) in
      let artifacts = {execution.Octra_core.Epoch_exec.artifacts with rejected =
        List.map (fun (row : Octra_core.Epoch_exec.tx_reject) -> {row with error_type})
          execution.artifacts.rejected} in
      Lwt.return_ok C.{execution = {execution with artifacts}; preverify = request.preverify} in
    let result = Lwt_main.run (C.make_proposal ~prepare ~budget:15. ~clock:(fun () -> 0.) deps
      ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
    expect "fresh prepare failure blocked other sender" (Option.map
      (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result = Some [Transaction.hash item]);
    expect "fresh prepare failure lost hold"
      (!(probe.holds) = [Transaction.hash refused] && !calls = 2))
    ["vm_transition_incomplete"; "vm_transition_exception"];
  let refused = {item with Transaction.from = "oct_refused"} in
  let now = ref 0. and calls = ref 0 in
  let deps, probe = make_proposal_deps ~staging:[item; refused] () in
  let deps = {deps with C.preview = (fun request ->
    incr calls;
    match !calls with
    | 1 ->
      now := 8.;
      Lwt.return (exec_result ~confirmed:[item] ~rejected:[refused] ~post_state_root:(raw 'l'))
    | 2 ->
      Work.run ~hash:(Transaction.hash item) (fun () ->
        Work.work (fun () -> now := !now +. 1.5);
        deps.preview request)
    | _ ->
      Lwt.return (exec_result ~confirmed:request.C.txs ~rejected:[] ~post_state_root:(raw 'l')))} in
  let result = Lwt_main.run (C.make_proposal ~budget:15. ~clock:(fun () -> !now) deps
    ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
  expect "late repeat held completed transaction" (!(probe.holds) = []);
  expect "repeat window continued selection" (!calls = 3);
  expect "late repeat published incomplete result" (Option.map
    (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result = Some []);
  let deps, probe = make_proposal_deps ~staging:[item] () in
  let deps = {deps with C.hold_preview = (fun ~epoch:_ _ -> raise (Work.Unavailable Work.Host));
    preview = (fun _ ->
      expect "hold error defer absent" (Work.defer (Transaction.hash item) Work.Time);
      Lwt.return (exec_result ~confirmed:[] ~rejected:[] ~post_state_root:(raw 'l')))} in
  let result = try run_make_proposal deps with
    Work.Unavailable Work.Host -> fail "hold failure escaped proposal" in
  expect "hold failure published proposal" (result = None && !(probe.stored_bundles) = [])

let test_program_window () =
  let module Core = Octra_core in
  let module Work = Core.Exec_resource in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module X = Core.Epoch_exec in
  let module P = Circle_proposal in
  Test_workspace.with_dir "program_window" (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
      let people = List.init 4 (fun i -> P.identity (i + 1))
        |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
      let ledger = L.create store in
      List.iter (fun who -> ignore (L.add_account ledger who.P.address (Z.of_int 1_000_000)
        |> Result.get_ok)) people;
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
      Lwt_main.run (S.set_meta store "emission_remaining" "0");
      let root = Lwt_main.run (L.hash ledger) in
      let inputs = List.mapi (fun i who -> P.signed who ~ou:1000
        ~op_type:(if i < 2 then Transaction.ProgramExec
          else if i = 2 then Standard else ValidatorReady)
        ~to_:(List.hd people).address ~nonce:1 ~message:"[]" ~method_:None) people in
      let slow = List.hd inputs in
      let kept = List.filteri (fun i _ -> i >= 2) inputs in
      expect "program window input order" (Transaction.consensus_order inputs = inputs);
      let now = ref 0. in
      let visited = ref [] in
      let env = X.{chain_id = "octra-test"; epoch_id = 12; proposer_addr = slow.from;
        validator_addrs = []; validator_pubkeys = []; prev_state_root = root;
        epoch_ts = 99.; ready_state_root_at = None; ready_max_lag = -1} in
      let execute txs = Core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
        ~proof_mode:Core.Rule_graph.Prior ~fold:X.prior_fold ~epoch_id:12
        ~proposal_id:"program-window" (fun backend ->
          X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
            ~process_tx:(fun ~backend ~env:_ tx -> Work.run ~hash:(Transaction.hash tx) (fun () ->
              let open Lwt.Syntax in
              visited := tx :: !visited;
              expect "program window debit" (L.debit backend.X.ledger tx.from (Z.of_int 10) tx.nonce = Ok ());
              let* () = S.set_meta backend.store tx.from "visited" in
              Work.work (fun () ->
                now := !now +. (if tx.op_type = ProgramExec then 6. else 0.01));
              Lwt.return_ok (X.Confirmed Z.zero)))) in
      let captured = ref None in
      let deps, probe = make_proposal_deps ~staging:inputs ~ledger_root:root () in
      let deps = {deps with C.preview = (fun request ->
        Lwt.map (fun result -> captured := Some result; result) (execute request.C.txs))} in
      let result = Lwt_main.run (C.make_proposal ~budget:15. ~clock:(fun () -> !now) deps
        ~chain_id:"octra-test" ~root_to_raw32:P.root_raw ~limits:generous_limits ~epoch_id:12L) in
      expect "program cut omitted other senders" (Option.map
        (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result
        = Some (List.map Transaction.hash kept));
      expect "program cut executed second call" (List.rev !visited = slow :: kept);
      expect "program cut held unstarted work" (!(probe.holds) = [Transaction.hash slow]);
      expect "program cut changed staging" (deps.staging_txs () = inputs);
      expect "program cut changed ledger" (Lwt_main.run (L.hash ledger) = root);
      let selected = Option.get !captured |> Result.get_ok in
      expect "program cut produced refusal" (selected.X.artifacts.rejected = []);
      let replay = Lwt_main.run (execute kept) |> Result.get_ok in
      expect "program cut replay differs" (selected = replay)))

let test_repeat_window () =
  let module Core = Octra_core in
  let module Work = Core.Exec_resource in
  let module Pool = Core.Tx_staging in
  let module Refill = Octra_node_runtime.Circle_refill in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module X = Core.Epoch_exec in
  let module P = Circle_proposal in
  List.iter (fun (prepared, descendants, operation_cut) ->
  Pool.clear ();
  Test_workspace.with_dir "repeat_window" (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (S.close store)) (fun () ->
      let people = List.init 3 (fun i -> P.identity (i + 1))
        |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
      let ledger = L.create store in
      List.iter (fun who -> ignore (L.add_account ledger who.P.address (Z.of_int 1_000_000)
        |> Result.get_ok)) people;
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
      Lwt_main.run (S.set_meta store "emission_remaining" "0");
      let root = Lwt_main.run (L.hash ledger) in
      let item i nonce op_type = P.signed (List.nth people i) ~ou:1000 ~op_type
        ~to_:(List.hd people).address ~nonce ~message:"[]" ~method_:None in
      let nonce = if descendants then 2 else 1 in
      let healthy = item 0 1 Transaction.Standard
      and refused = item 1 nonce Transaction.Standard
      and heavy = item 2 nonce Transaction.ProgramExec in
      let inputs = if descendants then
          [healthy; item 1 1 Standard; refused; item 1 3 Standard;
            item 2 1 Standard; heavy; item 2 3 Standard]
        else [healthy; refused; heavy] in
      let retained = if descendants then
          List.filter (fun item -> item.Transaction.nonce = 1) inputs
        else [healthy] in
      let rejected = List.filter (fun item -> item.Transaction.from = refused.from
        && item.nonce >= refused.nonce) inputs in
      let shortened = Refill.through ~rejected inputs in
      let without_heavy = Refill.before ~excluded:[heavy] shortened in
      let requests = if operation_cut then [inputs; shortened; without_heavy; retained]
        else if descendants && prepared then [inputs; shortened; retained]
        else [inputs; retained] in
      let executions = if operation_cut then
          [inputs; Refill.through ~rejected:[heavy] shortened; without_heavy; retained]
        else requests in
      expect "repeat window input order" (Transaction.consensus_order inputs = inputs);
      expect "repeat window input cutoffs"
        (Refill.before ~excluded:(heavy :: rejected) inputs = retained);
      List.iter (fun item -> ignore (Pool.add_smart
        ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) item |> Result.get_ok)) inputs;
      expect "repeat window staging inputs" (Pool.all () = inputs);
      expect "repeat window pool cutoffs"
        (Refill.before ~excluded:(List.rev (heavy :: rejected)) (Pool.all ()) = retained);
      expect "repeat window cutoff idempotence"
        (Refill.before ~excluded:(heavy :: heavy :: rejected) retained = retained);
      let entry = Hashtbl.find Pool.hash_index (Transaction.hash heavy) in
      if not operation_cut then entry.preview <- Some (Pool.hold_after 10L None);
      let allowance = if operation_cut then 3.75 else 7.5 in
      expect "repeat window operation allowance" (Pool.preview_limit (Transaction.hash heavy) 3.75 = allowance);
      expect "repeat window prior hold active"
        (Pool.Preview.send (Pool.Preview.Ready (Some 11L, Transaction.hash heavy)) = Ok ());
      let now = ref 0. and calls = ref 0 and visited = ref [] and captured = ref None in
      let received = ref [] in
      let evicted = ref [] in
      let env = X.{chain_id = "octra-test"; epoch_id = 12; proposer_addr = healthy.from;
        validator_addrs = []; validator_pubkeys = []; prev_state_root = root;
        epoch_ts = 99.; ready_state_root_at = None; ready_max_lag = -1} in
      let execute txs = Core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
        ~proof_mode:Core.Rule_graph.Prior ~fold:X.prior_fold ~epoch_id:12
        ~proposal_id:"repeat-window" (fun backend ->
          X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
            ~process_tx:(fun ~backend ~env:_ item -> Work.run ~hash:(Transaction.hash item) (fun () ->
              let open Lwt.Syntax in
              visited := (!calls, item) :: !visited;
              let debit = L.debit backend.X.ledger item.from (Z.of_int 10) item.nonce in
              if item.from = refused.from && item.nonce > refused.nonce then
                expect "repeat window descendant nonce accepted" (Result.is_error debit)
              else expect "repeat window debit" (debit = Ok ());
              let* () = S.set_meta backend.store item.from (string_of_int item.nonce) in
              if item = heavy then begin
                let first, second = if operation_cut then
                    (if !calls = 1 then 0.5, 0.5 else 2., 2.)
                  else if not descendants then 2., 4.
                  else if prepared then 1.15, 1.15 else 1.9, 1.9 in
                Work.work (fun () -> now := !now +. first);
                Work.work (fun () -> now := !now +. second)
              end else Work.work (fun () -> now := !now +. 0.1);
              if List.mem item rejected then Lwt.return_error ("bad", "refused")
              else Lwt.return_ok (X.Confirmed Z.zero)))) in
      let deps, probe = make_proposal_deps ~staging:(Pool.all ()) ~ledger_root:root () in
      let preview request =
        incr calls;
        received := request.C.txs :: !received;
        Lwt.map (fun result -> captured := Some result; result) (execute request.C.txs) in
      let deps = {deps with C.preview;
        staging_txs = (fun ?(circles = true) () ->
          if circles then Pool.all () else Refill.without (Pool.all ()));
        proposer = (fun () -> healthy.from);
        validator_pubkeys = (fun _ -> []);
        evict_preview = (fun ?epoch item ->
          expect "repeat window eviction identity"
            (operation_cut && prepared && epoch = Some 12L && item = refused && !evicted = []);
          let _, permitted = Pool.Preview.step ~limit:1 Pool.Preview.empty
            (Pool.Preview.Refuse {head = 11L; epoch = 12L; hash = Transaction.hash item}) in
          expect "repeat window eviction epoch refused" (permitted = Ok ());
          expect "repeat window eviction missing pool entry" (Option.is_some (Pool.drop_preview item));
          evicted := [item]);
        hold_preview = (fun ~epoch item ->
          deps.hold_preview ~epoch item;
          Pool.Preview.send (Pool.Preview.Hold {head = 11L; epoch; hash = Transaction.hash item})
            |> Result.get_ok)} in
      let prepare = if not prepared then None else Some (fun (request : C.build_preview_request) ->
        Lwt.map (Result.map (fun execution -> C.{execution; preverify = request.C.preverify}))
          (preview request)) in
      let result = Lwt_main.run (C.make_proposal ?prepare ~budget:15. ~clock:(fun () -> !now) deps
        ~chain_id:"octra-test" ~root_to_raw32:P.root_raw ~limits:generous_limits ~epoch_id:12L) in
      expect "repeat window lost other sender" (Option.map
        (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result
          = Some (List.map Transaction.hash retained));
      expect "repeat window preview inputs" (List.rev !received = requests);
      expect "repeat window execution order" (List.rev !visited =
        List.concat (List.mapi (fun index txs -> List.map (fun item -> index + 1, item) txs) executions));
      let elapsed = List.mapi (fun index txs -> List.fold_left (fun total item ->
        total +. if item <> heavy then 0.1 else if operation_cut then
          (if index = 0 then 1. else 4.)
        else if not descendants then 6. else if prepared then 2.3 else 3.8) 0. txs) executions
        |> List.fold_left (+.) 0. in
      expect "repeat window retry count"
        (!calls = List.length requests && abs_float (!now -. elapsed) < 0.000001);
      expect "repeat window hold differs" (List.mem (Transaction.hash heavy) !(probe.holds) = operation_cut);
      expect "repeat window grew skipped allowance"
        (Pool.preview_limit (Transaction.hash heavy) 3.75 = 7.5);
      expect "repeat window ready differs"
        (Result.is_ok (Pool.Preview.send (Pool.Preview.Ready (Some 11L, Transaction.hash heavy)))
          = not operation_cut);
      expect "repeat window changed base ledger" (Lwt_main.run (L.hash ledger) = root);
      List.iter (fun who ->
        expect "repeat window changed base nonce" ((L.find ledger who.P.address).L.nonce = 0);
        expect "repeat window changed base store"
          (Lwt_main.run (S.get_meta store who.P.address) = None)) people;
      expect "repeat window eviction differs" (!evicted = if operation_cut then [refused] else []);
      let pending = if operation_cut then List.filter ((<>) refused) inputs else inputs in
      expect "repeat window changed staging" (Pool.all () = pending);
      expect "repeat window evicted retained sender"
        (List.for_all (fun item -> List.mem item (Pool.all ())) retained);
      expect "repeat window lost nonce descendants"
        (List.for_all (fun item -> List.mem item (Pool.all ()))
          (List.filter (fun item -> item.Transaction.nonce > nonce) inputs));
      let module Outcome = Core.Tx_outcome in
      let receipts = match !(probe.stored_bundles) with
        | [_, _, _, receipts] -> receipts
        | _ -> fail "repeat window publication count" in
      let outcomes = Outcome.decode ~confirmed:retained receipts |> Result.get_ok in
      let expected = if operation_cut then
          Outcome.build ~inputs:(retained @ [refused]) [refused, "bad", "refused"] |> Result.get_ok
        else [] in
      expect "repeat window refusal receipt differs" (Outcome.equal outcomes.rejections expected);
      let selected = Option.get !captured |> Result.get_ok in
      let replay = Lwt_main.run (execute retained) |> Result.get_ok in
      expect "repeat window replay differs" (selected = replay);
      expect "repeat window retained rejection rows" (selected.X.artifacts.rejected = []);
      let hashes = List.map Transaction.hash retained in
      let _, eic = Core.Epoch_index_commitment.next_root_from_hashes
        ~prev:(deps.prev_eic_root ()) ~epoch_id:12 ~start_txid:7L hashes in
      let expected = Core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:replay.X.post_state_root ~epoch_index_root:eic |> P.root_raw in
      expect "repeat window published root differs"
        ((Option.get result).header.proposed_state_root = expected))))
    [false, false, false; true, false, false; false, true, false; true, true, false;
      true, true, true]

let test_completion_window () =
  let module Core = Octra_core in
  let module Work = Core.Exec_resource in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module X = Core.Epoch_exec in
  let module P = Circle_proposal in
  List.iter (fun (prepared, asynchronous, early) ->
  Core.Tx_staging.clear ();
  Test_workspace.with_dir "completion_window" (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Core.Tx_staging.clear (); Lwt_main.run (S.close store)) (fun () ->
      let people = List.init 4 (fun i -> P.identity (i + 1))
        |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
      let ledger = L.create store in
      List.iter (fun who -> ignore (L.add_account ledger who.P.address (Z.of_int 1_000_000)
        |> Result.get_ok)) people;
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
      Lwt_main.run (S.set_meta store "emission_remaining" "0");
      let root = Lwt_main.run (L.hash ledger) in
      let inputs = List.mapi (fun i who -> P.signed who ~ou:1000
        ~op_type:(if i = 0 then (if early then Transaction.ProgramDeploy else ProgramExec)
          else if early && i = 2 then ProgramExec else Standard)
        ~to_:(List.hd people).address ~nonce:1 ~message:"[]" ~method_:None) people in
      let slow = List.hd inputs and healthy = List.nth inputs 1 and edge = List.nth inputs 2 in
      expect "completion window input order" (Transaction.consensus_order inputs = inputs);
      if early then ignore (Core.Tx_staging.add_smart
        ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) edge |> Result.get_ok);
      let now = ref 0. and visited = ref [] in
      let env = X.{chain_id = "octra-test"; epoch_id = 12; proposer_addr = slow.from;
        validator_addrs = []; validator_pubkeys = []; prev_state_root = root;
        epoch_ts = 99.; ready_state_root_at = None; ready_max_lag = -1} in
      let execute ?foreign txs = Core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
        ~proof_mode:Core.Rule_graph.Prior ~fold:X.prior_fold ~epoch_id:12
        ~proposal_id:"completion-window" (fun backend ->
          X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
            ~process_tx:(fun ~backend ~env:_ tx -> Work.run ~hash:(Transaction.hash tx) (fun () ->
              let open Lwt.Syntax in
              visited := tx :: !visited;
              expect "completion window debit" (L.debit backend.X.ledger tx.from (Z.of_int 10) tx.nonce = Ok ());
              let* () = S.set_meta backend.store tx.from "visited" in
              Option.iter (fun hash -> raise (Work.Window_expired hash)) foreign;
              let cost () = now := !now +.
                (if tx = slow then 6. else if tx = healthy then (if early then 1.8 else 2.9)
                  else if early then 0.3 else 0.2) in
              let* () = if asynchronous then Work.interrupt (fun () -> cost (); Lwt.return_unit)
                else Lwt.return (Work.work cost) in
              Lwt.return_ok (X.Confirmed Z.zero)))) in
      let captured = ref None in
      let deps, probe = make_proposal_deps ~staging:inputs ~ledger_root:root () in
      let preview request =
        Lwt.map (fun result -> captured := Some result; result) (execute request.C.txs) in
      let deps = {deps with C.preview} in
      let prepare = if not prepared then None else Some (fun (request : C.build_preview_request) ->
        Lwt.map (Result.map (fun execution -> C.{execution; preverify = request.C.preverify}))
          (preview request)) in
      let result = Lwt_main.run (C.make_proposal ?prepare ~budget:15. ~clock:(fun () -> !now) deps
        ~chain_id:"octra-test" ~root_to_raw32:P.root_raw ~limits:generous_limits ~epoch_id:12L) in
      let kept = if early then [healthy; List.nth inputs 3] else [healthy] in
      expect "completion window lost completed work" (Option.map
        (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result = Some (List.map Transaction.hash kept));
      expect "completion window hold absent"
        (List.sort String.compare !(probe.holds) = List.sort String.compare
          [Transaction.hash slow]);
      expect "completion window executed unseen work" (List.rev !visited =
        if early then inputs else [slow; healthy; edge]);
      expect "completion window changed staging" (deps.staging_txs () = inputs);
      expect "completion window changed ledger" (Lwt_main.run (L.hash ledger) = root);
      let selected = Option.get !captured |> Result.get_ok in
      expect "completion window produced refusal" (selected.X.artifacts.rejected = []);
      let replay = Lwt_main.run (execute kept) |> Result.get_ok in
      expect "completion window replay differs" (selected = replay);
      let omitted = ref [] in
      let foreign = Transaction.hash edge in
      let refused = Lwt_main.run (Lwt.catch (fun () ->
        Lwt.map (fun _ -> false) (Work.preview ~accept:(fun () -> true)
          ~defer:(fun _ _ -> fail "window used resource hold")
          ~omit:(fun hash -> omitted := hash :: !omitted)
          (fun () -> execute ~foreign [healthy]))) (function
            | Work.Window_expired hash when hash = foreign -> Lwt.return_true
            | error -> Lwt.fail error)) in
      expect "completion window accepted foreign identity" (refused && !omitted = []);
      expect "completion window foreign changed ledger" (Lwt_main.run (L.hash ledger) = root))))
    [false, false, true; true, true, true;
      false, false, false; false, true, false; true, false, false; true, true, false]

let test_refill_core () =
  let module Core = Octra_core in
  let module Work = Core.Exec_resource in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module X = Core.Epoch_exec in
  let module P = Circle_proposal in
  let module Shell = Octra_node_runtime.Consensus_proposal_preview_shell in
  Test_workspace.with_dir "refill_core" (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (S.close store)) (fun () ->
      let people = List.init 3 (fun i -> P.identity (i + 1))
        |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
      let ledger = L.create store in
      List.iter (fun who -> ignore (L.add_account ledger who.P.address (Z.of_int 1_000_000)
        |> Result.get_ok)) people;
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
      Lwt_main.run (S.set_meta store "emission_remaining" "0");
      let root = Lwt_main.run (L.hash ledger) in
      let inputs = List.map (fun who -> P.signed who ~ou:1000 ~op_type:Standard
        ~to_:(List.hd people).address ~nonce:1 ~message:"[]" ~method_:None) people in
      let first = List.hd inputs and refused = List.nth inputs 1 in
      expect "core refill input order" (Transaction.consensus_order inputs = inputs);
      let now = ref 0. and calls = ref 0 in
      let saved = ref None in
      let visited = ref [] in
      let run ~epoch_id ~proposal_id ~expected_prev_root ~preverify:_ ~parent_commit:_
          ~reward:_ ~env ~txs =
        Core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
          ~proof_mode:Core.Rule_graph.Prior ~fold:X.prior_fold ~epoch_id ~proposal_id
          ?expected_prev_root (fun backend ->
            X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
              ~process_tx:(fun ~backend ~env:_ item -> Work.run ~hash:(Transaction.hash item) (fun () ->
                let open Lwt.Syntax in
                visited := (!calls, item) :: !visited;
                expect "core refill debit" (L.debit backend.X.ledger item.from (Z.of_int 10) item.nonce = Ok ());
                let* () = S.set_meta backend.store item.from "visited" in
                Work.work (fun () -> now := !now +. (if !calls = 3 then 4. else 0.01));
                if item = refused then Lwt.return_error ("bad", "refused")
                else Lwt.return_ok (X.Confirmed Z.zero)))) in
      let prepare ~epoch_id ~proposal_id ~expected_prev_root ~preverify ~parent_commit ~reward ~env ~txs =
        incr calls;
        let open Lwt.Syntax in
        let+ value = run ~epoch_id ~proposal_id ~expected_prev_root ~preverify ~parent_commit ~reward ~env ~txs in
        if !calls = 2 then saved := Some value;
        Result.map (fun execution -> C.{execution; preverify}) value in
      let shell : Shell.deps = {chain_id = "octra-test"; program_trust = Octra_vm.Program_trust.empty;
        backend = {run; prepare}; ready_state_root_at = (fun _ -> Lwt.return_none);
        ready_max_lag = -1; warn = ignore} in
      let prepare = Shell.prepare_at ~mode:(fun ~epoch:_ -> Ok Core.Rule_graph.Active)
        shell 12L |> Result.get_ok |> Option.get in
      let deps, probe = make_proposal_deps ~staging:inputs ~ledger_root:root () in
      let deps = {deps with C.proposer = (fun () -> first.from);
        validator_pubkeys = (fun _ -> []);
        evict_preview = (fun ?epoch:_ item -> expect "core refill evicted completed work" (item = refused))} in
      let result = Lwt_main.run (C.make_proposal ~prepare ~budget:15. ~clock:(fun () -> !now)
        deps ~chain_id:"octra-test" ~root_to_raw32:P.root_raw
        ~limits:{generous_limits with C.max_txs = 2} ~epoch_id:12L) in
      expect "core refill discarded checked proposal" (Option.is_some result);
      expect "core refill retry count" (!calls = 3);
      expect "core refill execution order"
        (List.rev !visited = [1, first; 1, refused; 2, first; 3, first]);
      expect "core refill held completed work" (!(probe.holds) = []);
      expect "core refill changed base ledger" (Lwt_main.run (L.hash ledger) = root);
      let original = Option.get !saved |> Result.get_ok in
      let hashes = [Transaction.hash first] in
      let _, eic = Core.Epoch_index_commitment.next_root_from_hashes
        ~prev:(deps.prev_eic_root ()) ~epoch_id:12 ~start_txid:7L hashes in
      let expected = Core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:original.X.post_state_root ~epoch_index_root:eic |> P.root_raw in
      let plan = Option.get result in
      expect "core refill root differs" (plan.header.proposed_state_root = expected && plan.tx_hashes = hashes);
      expect "core refill publication count"
        (List.length !(probe.stored_bundles) = 1 && List.length !(probe.frozen_writes) = 1)))

let test_window_renew () =
  let module Core = Octra_core in
  let module Pool = Core.Tx_staging in
  let module Work = Core.Exec_resource in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module X = Core.Epoch_exec in
  let module P = Circle_proposal in
  let prior = Pool.{head = 9L; count = 2; after = 11L} in
  let renewed = Pool.hold_after ~mode:Pool.Renew 11L (Some prior) in
  expect "renewal grew count" (renewed = Pool.{head = 11L; count = 2; after = 13L});
  expect "renewal repeated within head" (Pool.hold_after ~mode:Pool.Renew 11L (Some renewed) = renewed);
  expect "growth stopped increasing count"
    (Pool.hold_after 11L (Some prior) = Pool.{head = 11L; count = 3; after = 15L});
  List.iter (fun prepared ->
  List.iter (fun (op_type, start, seconds, renew) ->
    Pool.clear ();
    Test_workspace.with_dir "window_renew" (fun dir ->
      let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
      Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (S.close store)) (fun () ->
        let people = List.init 2 (fun i -> P.identity (i + 1))
          |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
        let ledger = L.create store in
        List.iter (fun who -> L.add_account ledger who.P.address (Z.of_int 1_000_000)
          |> Result.get_ok |> ignore) people;
        Lwt_main.run (L.flush_dirty_lwt ledger);
        Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
        Lwt_main.run (S.set_meta store "emission_remaining" "0");
        let root = Lwt_main.run (L.hash ledger) in
        let item who kind = P.signed who ~ou:1000 ~op_type:kind
          ~to_:(List.hd people).address ~nonce:1 ~message:"[]" ~method_:None in
        let ordinary = item (List.hd people) Transaction.Standard in
        let slow = item (List.nth people 1) op_type in
        let inputs = [ordinary; slow] in
        expect "renewal input order" (Transaction.consensus_order inputs = inputs);
        List.iter (fun tx -> Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000, 0)) tx
          |> Result.get_ok |> ignore) inputs;
        let entry = Hashtbl.find Pool.hash_index (Transaction.hash slow) in
        entry.preview <- Some prior;
        expect "renewal accepted wrong epoch" (Result.is_error (Pool.Preview.send
          (Pool.Preview.Renew {head = 11L; epoch = 13L; hash = Transaction.hash slow})));
        expect "renewal invented missing hold" (Result.is_error (Pool.Preview.send
          (Pool.Preview.Renew {head = 11L; epoch = 12L; hash = Transaction.hash ordinary})));
        let now = ref 0. and captured = ref None in
        let env = X.{chain_id = "octra-test"; epoch_id = 12; proposer_addr = ordinary.from;
          validator_addrs = []; validator_pubkeys = []; prev_state_root = root;
          epoch_ts = 99.; ready_state_root_at = None; ready_max_lag = -1} in
        let execute txs = Core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
          ~proof_mode:Core.Rule_graph.Prior ~fold:X.prior_fold ~epoch_id:12
          ~proposal_id:"window-renew" (fun backend ->
            X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
              ~process_tx:(fun ~backend ~env:_ tx ->
                if tx = slow then now := start;
                Work.run ~hash:(Transaction.hash tx) (fun () ->
                  let open Lwt.Syntax in
                  expect "renewal debit failed" (L.debit backend.X.ledger tx.from (Z.of_int 10) tx.nonce = Ok ());
                  let* () = S.set_meta backend.store tx.from "visited" in
                  Work.work (fun () -> now := !now +. (if tx = slow then seconds else 0.1));
                  Lwt.return_ok (X.Confirmed Z.zero)))) in
        let deps, probe = make_proposal_deps ~staging:inputs ~ledger_root:root
          ~cached_head:(Some (proposal_head root)) () in
        let preview request = Lwt.map (fun result -> captured := Some result; result)
          (execute request.C.txs) in
        let deps = {deps with C.preview} in
        let prepare = if not prepared then None else Some (fun (request : C.build_preview_request) ->
          Lwt.map (Result.map (fun execution -> C.{execution; preverify = request.preverify})) (preview request)) in
        let result = Lwt_main.run (C.make_proposal ?prepare ~clock:(fun () -> !now) ~budget:15. deps
          ~chain_id:"octra-test" ~root_to_raw32:P.root_raw ~limits:generous_limits ~epoch_id:12L) in
        expect "renewal lost ordinary sender" (Option.map
          (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result = Some [Transaction.hash ordinary]);
        expect "completion grew allowance" (Pool.preview_limit (Transaction.hash slow) 3.75 = 15.);
        expect "completion used growing hold" (!(probe.holds) = []);
        expect "completion cooldown differs" (entry.preview = Some (if renew then renewed else prior));
        expect "completion readiness differs"
          (Result.is_ok (Pool.Preview.send (Pool.Preview.Ready (Some 11L, Transaction.hash slow))) = not renew);
        let selected = Option.get !captured |> Result.get_ok in
        let replay = Lwt_main.run (execute [ordinary]) |> Result.get_ok in
        expect "completion rollback differs" (selected = replay && selected.artifacts.rejected = []);
        expect "completion changed ledger" (Lwt_main.run (L.hash ledger) = root);
        expect "completion retained write" (Lwt_main.run (S.get_meta store slow.from) = None);
        let _, eic = Core.Epoch_index_commitment.next_root_from_hashes
          ~prev:(deps.prev_eic_root ()) ~epoch_id:12 ~start_txid:7L [Transaction.hash ordinary] in
        let expected = Core.Epoch_index_commitment.folded_state_root
          ~ledger_state_root:replay.post_state_root ~epoch_index_root:eic |> P.root_raw in
        expect "completion published another root" ((Option.get result).header.proposed_state_root = expected))))
    [Transaction.ProgramExec, 0.1, 8.25, true;
      Transaction.ProgramExec, 4.5, 3.75, false;
      Transaction.Standard, 5.5, 3.6, false]) [false; true]

let test_sender_reserve () =
  let module Core = Octra_core in
  let module Pool = Core.Tx_staging in
  let module Work = Core.Exec_resource in
  let module S = Core.Store_irmin in
  let module L = Core.Ledger in
  let module X = Core.Epoch_exec in
  let module P = Circle_proposal in
  Pool.clear ();
  Test_workspace.with_dir "sender_reserve" (fun dir ->
    let store = Lwt_main.run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (S.close store)) (fun () ->
      let people = List.init 6 (fun i -> P.identity (i + 1))
        |> List.sort (fun a b -> String.compare a.P.address b.P.address) in
      let ledger = L.create store in
      List.iter (fun who -> L.add_account ledger who.P.address (Z.of_int 1_000_000)
        |> Result.get_ok |> ignore) people;
      Lwt_main.run (L.flush_dirty_lwt ledger);
      Lwt_main.run (S.set_meta store "total_supply" (Z.to_string (L.get_total_supply ledger)));
      Lwt_main.run (S.set_meta store "emission_remaining" "0");
      let root = Lwt_main.run (L.hash ledger) in
      let inputs = List.map (fun who -> P.signed who ~ou:1000 ~op_type:Transaction.Standard
        ~to_:(List.hd people).address ~nonce:1 ~message:"" ~method_:None) people in
      List.iter (fun tx -> Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 1_000_000, 0)) tx
        |> Result.get_ok |> ignore) inputs;
      let retained = List.filteri (fun i _ -> i >= 4) inputs in
      let env = X.{chain_id = "octra-test"; epoch_id = 12; proposer_addr = (List.hd inputs).from;
        validator_addrs = []; validator_pubkeys = []; prev_state_root = root;
        epoch_ts = 99.; ready_state_root_at = None; ready_max_lag = -1} in
      let execute txs = Core.State_preview.with_preview ~base_store:store ~base_ledger:ledger
        ~proof_mode:Core.Rule_graph.Prior ~fold:X.prior_fold ~epoch_id:12
        ~proposal_id:"sender-reserve" (fun backend ->
          X.run_core ~reward:None ~preverify:None ~backend ~env ~txs
            ~process_tx:(fun ~backend ~env:_ tx ->
              expect "sender reserve debit failed" (L.debit backend.X.ledger tx.from (Z.of_int 10) tx.nonce = Ok ());
              Lwt.return_ok (X.Confirmed Z.zero))) in
      let received = ref [] and captured = ref None in
      let deps, probe = make_proposal_deps ~staging:inputs ~ledger_root:root () in
      let prepare (request : C.build_preview_request) =
        received := request.txs :: !received;
        if List.length !received <= 4 then
          Lwt.fail (Work.Exhausted (Transaction.hash (List.hd request.txs), Work.Host))
        else Lwt.map (Result.map (fun execution ->
          captured := Some execution; C.{execution; preverify = request.preverify})) (execute request.txs) in
      let result = Lwt_main.run (C.make_proposal ~prepare ~clock:(fun () -> 0.) ~budget:15. deps
        ~chain_id:"octra-test" ~root_to_raw32:P.root_raw ~limits:generous_limits ~epoch_id:12L) in
      expect "sender reserve did not use final attempt"
        (List.rev !received = List.init 5 (fun cut -> List.filteri (fun i _ -> i >= cut) inputs));
      let held = List.filteri (fun i _ -> i < 4) inputs
        |> List.map Transaction.hash |> List.sort String.compare in
      expect "sender reserve held retained sender"
        (List.sort String.compare !(probe.holds) = held);
      expect "sender reserve lost retained sender" (Option.map
        (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) result = Some (List.map Transaction.hash retained));
      let replay = Lwt_main.run (execute retained) |> Result.get_ok in
      expect "sender reserve execution differs" (!captured = Some replay);
      expect "sender reserve changed ledger" (Lwt_main.run (L.hash ledger) = root);
      expect "sender reserve changed pool" (Pool.all () = inputs);
      let _, eic = Core.Epoch_index_commitment.next_root_from_hashes
        ~prev:(deps.prev_eic_root ()) ~epoch_id:12 ~start_txid:7L (List.map Transaction.hash retained) in
      let expected = Core.Epoch_index_commitment.folded_state_root
        ~ledger_state_root:replay.post_state_root ~epoch_index_root:eic |> P.root_raw in
      expect "sender reserve published another root" ((Option.get result).header.proposed_state_root = expected)))

let test_budget_growth () =
  let module Work = Octra_core.Exec_resource in
  let module Pool = Octra_core.Tx_staging in
  let who = Circle_proposal.identity 1 in
  List.iter (fun op_type ->
  let item = Circle_proposal.signed who ~ou:1000 ~op_type
    ~to_:(Circle_proposal.identity 2).address ~nonce:1 ~message:"" ~method_:None in
  Pool.clear ();
  Fun.protect ~finally:Pool.clear (fun () ->
    ignore (Pool.add_smart ~lookup:(fun _ -> Some (Z.of_int 100_000, 0)) item |> Result.get_ok);
    let attempt ?(spent = 0.) seconds =
      let now = ref 0. in
      let deps, _ = make_proposal_deps ~staging:[item] () in
      let deps = {deps with C.preview = (fun request ->
        if request.C.txs = [] then deps.preview request else
        let () = now := !now +. spent in
        Work.run ~hash:(Transaction.hash item) (fun () ->
          Work.work (fun () -> now := !now +. seconds;
            deps.preview request)))} in
      Lwt_main.run (C.make_proposal ~budget:15. ~clock:(fun () -> !now) deps
        ~chain_id:"octra-test" ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:12L) in
    expect "first slow execution accepted" (Option.fold ~none:true
      ~some:(fun (plan : C_driver.proposal_plan) -> plan.tx_hashes = []) (attempt 6.));
    let entry = Hashtbl.find Pool.hash_index (Transaction.hash item) in
    entry.preview <- Some (Pool.hold_after 11L None);
    expect "held execution budget did not grow" (Option.map
      (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) (attempt 6.)
      = Some [Transaction.hash item]);
    expect "execution used completion reserve" (Option.fold ~none:true
      ~some:(fun (plan : C_driver.proposal_plan) -> plan.tx_hashes = []) (attempt ~spent:8. 1.2));
    expect "lane completion reserve differs" (Option.map
      (fun (plan : C_driver.proposal_plan) -> plan.tx_hashes) (attempt ~spent:7.5 0.8)
      = Some (if op_type = Transaction.Standard then [Transaction.hash item] else []));
    expect "growing budget bypassed deadline" (attempt 11. = None)))
    Transaction.[Standard; ProgramExec]

let test_round_window () =
  let module Gate = Octra_consensus.C_proposal_work_gate in
  let gate = Gate.create () in
  Gate.start gate ~delay:0. ~duration:0.08;
  let deps, probe = make_proposal_deps ~staging:[tx 1] () in
  let result = Lwt_main.run (let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.05 in
    Gate.run gate ~relevant:(fun () -> true) (fun () ->
      C.make_proposal deps ~chain_id:"octra-test" ~root_to_raw32:Fun.id
        ~limits:generous_limits ~epoch_id:12L)) in
  expect "round wait restarted budget" (result = None && !(probe.preview_requests) = [])

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

let test_circle_receipt () =
  let module Lane = Octra_core.Resource_lanes in
  let module Receipt = Octra_core.Preverify_receipt in
  let env = "OCTRA_BFT_RELEASE_PROFILE" in
  let prior = Sys.getenv_opt env in
  Unix.putenv env "devnet_full_v1";
  Fun.protect ~finally:(fun () -> Unix.putenv env (Option.value ~default:"" prior)) (fun () ->
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 '\003') |> Result.get_ok in
    let public = Mirage_crypto_ec.Ed25519.pub_of_priv key
      |> Mirage_crypto_ec.Ed25519.pub_to_octets |> Base64.encode_exn in
    let secret = Mirage_crypto_ec.Ed25519.priv_to_octets key |> Base64.encode_exn in
    let address = Octra_core.Crypto.Address.address_from_pubkey public in
    let budget = Lane.default_budget Lane.Circle_compute in
    let empty = {(tx 1) with Transaction.op_type = CircleCall; from = address; to_ = address;
      ou = budget.max_ou; public_key = Some public; encrypted_data = Some "run"; message = Some ""}
      |> fun item -> Transaction.sign_with_privkey item secret in
    let count = budget.max_bytes - Lane.tx_bytes empty in
    let circle = Receipt.{snapshot_hash = String.make 64 'a'; circle_id = address;
      code_hash = String.make 64 'b'; stable_root = String.make 64 'c';
      public_reads_hash = String.make 64 'd'; context_hash = String.make 64 'e'; transcript = []} in
    let limits = C.limits ~max_txs:1 ~max_bytes:(budget.max_bytes + 1) ~max_ou:budget.max_ou in
    let deps = C.{public_key_for_tx = (fun _ -> Some public);
      verify_address_pubkey = (fun ~addr ~pubkey -> Octra_core.Crypto.Address.verify_address_pubkey addr pubkey);
      verify_tx_signature = (fun ~signing item ~pubkey -> Octra_core.Tx_signature.verify signing item pubkey)} in
    List.iter (fun (size, ou, reason) ->
      let item = {empty with Transaction.message = Some (String.make size 'a'); ou}
        |> fun item -> Transaction.sign_with_privkey item secret in
      let receipt = Receipt.make_circle ~tx_hash:(Transaction.hash item)
        ~input_hash:(W.circle_input_hash item circle) ~output_hash:(W.circle_output_hash item circle "ok")
        ~circle ~ok:true ~reason:"" ~cost:(Lane.cost item) |> Result.get_ok in
      expect "circle receipt passes structural and cost verification"
        (Octra_core.Preverify_commit.check_receipt item receipt = Ok ());
      let receipts_json = [Yojson.Safe.to_string (Receipt.to_yojson receipt)] in
      let result = verify ~deps ~limits ~receipts_json [item] in
      match reason, result with
      | None, Ok verified -> expect "circle exact budget retained" (verified.C.txs = [item])
      | Some reason, Error (C.Preverify_gate_failed actual) ->
        expect "circle validator rejects resource excess" (actual = "lane_budget:circle_compute:" ^ reason)
      | _ -> fail "circle receipt budget verdict")
      [count, budget.max_ou, None;
       count + 1, budget.max_ou, Some "bytes";
       count, Z.succ budget.max_ou, Some "ou"])

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
      ~certified_root:None
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
      ~certified_root:(Some (raw 't'))
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
      ~certified_root:(Some (raw 't'))
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
          charged = false;
        }];
        confirmed_fees = Z.zero;
        tx_count = 2;
      };
    }
  in
  let rejections =
    match
      Octra_core.Tx_outcome.build
        ~inputs:[confirmed; rejected]
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
    verify_tx_signature = (fun ~signing:_ _ ~pubkey:_ -> true);
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
  expect "verify unproven mismatch streak" (!prev_streak_ref = 3);
  expect "verify unproven mismatch quarantine" (!quarantines = []);
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

let test_unproven_roots () =
  let proposal = proposal_for_txs [tx 1] in
  List.iter (fun root ->
    let deps, quarantines, prev, state, _, _, _, _ =
      verify_proposal_deps ~root ~prev_streak:3 ~state_streak:3 ~staging:[tx 1]
        ~preview_result:(fun request ->
          exec_result ~confirmed:request.C.txs ~rejected:[] ~post_state_root:(raw 'x')) () in
    for _ = 1 to 6 do
      expect "unproven root rejected"
        (verdict_rejects (Lwt_main.run
          (C.verify_proposal deps ~chain_id:"octra-test" proposal)))
    done;
    expect "unproven roots did not quarantine" (!quarantines = []);
    expect "unproven roots preserved counters" (!prev = 3 && !state = 3))
    [raw 'p'; raw 'x'];
  List.iter (fun certified_root ->
    match C.prev_root_decision ~certified_root ~epoch_id:12L
      ~target_root:(raw 'p') ~current_root:(raw 'x') ~max_wait_tries:0
      ~tries_left:0 ~current_streak:9 ~quarantine_threshold:4 with
    | C.Prev_root_mismatch {streak_after = 9; quarantine_reason = None; _} -> ()
    | _ -> fail "unrelated certificate caused quarantine") [None; Some (raw 'z')]

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

let test_prepare_reuse () =
  List.iter (fun cached ->
    let item = tx 1 in
    let proposal = proposal_for_txs [item] in
    let calls = ref 0 in
    let signatures = ref 0 in
    let prepare request =
      incr calls;
      Lwt.return_ok C.{preverify = request.preverify;
        execution = Result.get_ok (exec_result ~confirmed:request.txs
          ~rejected:[] ~post_state_root:(raw 'l'))} in
    let deps, _, _, _, _, _, proposals, previews = verify_proposal_deps
      ~staging:[item] ~cached_bundle:(fun ~proposal_id:_ ->
        if cached then Some (received [item]) else None) () in
    let deps = {deps with verify_tx_signature = (fun ~signing:_ _ ~pubkey:_ ->
      incr signatures; true)} in
    for round = 0 to 1 do
      expect "prepared proposal accepted" (verdict_accepts (Lwt_main.run
        (C.verify_proposal ~prepare deps ~chain_id:"octra-test" {proposal with round})));
      expect "prepared execution repeated" (!calls = round + 1);
      expect "prepared signature omitted" (!signatures = round + 1)
    done;
    expect "prepared preview repeated" (!previews = []);
    expect "prepared proposals published" (List.length !proposals = 2);
    let bad_signature = {deps with verify_tx_signature =
      (fun ~signing:_ _ ~pubkey:_ -> false)} in
    expect "prepared signature refusal" (verdict_rejects (Lwt_main.run
      (C.verify_proposal ~prepare bad_signature ~chain_id:"octra-test" proposal)));
    let wrong_root = {proposal with header = {proposal.header with proposed_state_root = raw 'x'}} in
    expect "prepared root refusal" (verdict_rejects (Lwt_main.run
      (C.verify_proposal ~prepare deps ~chain_id:"octra-test" wrong_root)));
    expect "prepared refusals not published" (List.length !proposals = 2)) [false; true]

let test_prepare_cancel () =
  List.iter (fun cached -> List.iter (fun cancel ->
    let item = tx 1 in
    let current = ref true in
    let release, finish = Lwt.wait () in
    let calls = ref 0 in
    let prepare request =
      incr calls;
      let open Lwt.Syntax in
      let* () = Lwt.protected release in
      Lwt.return_ok C.{preverify = request.preverify;
        execution = Result.get_ok (exec_result ~confirmed:request.txs
          ~rejected:[] ~post_state_root:(raw 'l'))} in
    let deps, quarantines, _, _, _, _, proposals, _ = verify_proposal_deps
      ~current:(fun () -> !current) ~staging:[item]
      ~cached_bundle:(fun ~proposal_id:_ -> if cached then Some (received [item]) else None) () in
    let verify () = C.verify_proposal ~prepare deps ~chain_id:"octra-test" (proposal_for_txs [item]) in
    let pending = verify () in
    expect "prepare pending" (Lwt.is_sleeping pending && !calls = 1);
    if cancel then begin
      Lwt.cancel pending;
      expect "prepare cancellation lost" (match Lwt.state pending with
        | Lwt.Fail Lwt.Canceled -> true | _ -> false);
      Lwt.wakeup_later finish ();
      Lwt_main.run (Lwt.pause ())
    end else begin
      current := false;
      Lwt.wakeup_later finish ();
      expect "prepare after head change waits" (verdict_waits (Lwt_main.run pending))
    end;
    expect "prepare after head change no effects" (!proposals = [] && !quarantines = []);
    current := true;
    expect "prepare new context accepts" (verdict_accepts (Lwt_main.run (verify ())));
    expect "prepare new context executed" (!calls = 2)) [false; true]) [false; true]

let test_verify_host_cancel () =
  let module Work = Octra_core.Exec_resource in
  List.iter (fun changed ->
    let item = tx 1 in
    let current = ref true in
    let cancelled = ref false in
    let deps, quarantines, _, _, _, _, published, _ = verify_proposal_deps
      ~current:(fun () -> !current) ~staging:[item] () in
    let prepare request = Work.interrupt (fun () ->
      let pending = Lwt_unix.sleep 0.05 in
      Lwt.on_cancel pending (fun () -> cancelled := true);
      Lwt.map (fun () -> Ok C.{preverify = request.C.preverify;
        execution = Result.get_ok (exec_result ~confirmed:request.txs
          ~rejected:[] ~post_state_root:(raw 'l'))}) pending) in
    let timer = Lwt.bind (Lwt_unix.sleep 0.01) (fun () ->
      if changed then current := false;
      Lwt.return_unit) in
    let result = Lwt_main.run (C.verify_proposal ~prepare deps ~chain_id:"octra-test"
      (proposal_for_txs [item])) in
    Lwt.cancel timer;
    expect "validator host cancellation differs" (!cancelled = changed);
    expect "validator host quarantined" (!quarantines = []);
    if changed then begin
      expect "changed context was rejected" (verdict_waits result);
      expect "changed host result published" (!published = [])
    end else expect "validator used local time limit" (verdict_accepts result)) [false; true]

let test_prepare_closed () =
  let item = tx 1 in
  let current = ref true in
  let calls = ref 0 in
  let deps, _, _, _, _, _, proposals, _ = verify_proposal_deps
    ~current:(fun () -> !current) ~staging:[item]
    ~validate_preverify_once:(fun ~state_root:_ ~tx_hashes:_ items ->
      current := false; fake_batch items) () in
  let prepare _ = incr calls; Lwt.return_error "unexpected execution" in
  expect "closed context waits" (verdict_waits (Lwt_main.run
    (C.verify_proposal ~prepare deps ~chain_id:"octra-test" (proposal_for_txs [item]))));
  expect "closed context executed" (!calls = 0 && !proposals = [])

let test_verify_worker_retry () =
  List.iter (fun partial ->
    let first = tx 1 in
    let second = tx 2 in
    let inputs = if partial then [first; second] else [first] in
    let online = ref false in
    let proposal = proposal_for_txs inputs in
    let deps, quarantines, prev, state, _, stores, proposals, previews =
      verify_proposal_deps ~prev_streak:2 ~state_streak:2
        ~cached_bundle:(fun ~proposal_id:_ -> Some (received inputs))
        ~validate_preverify_once:(fun ~state_root:_ ~tx_hashes:_ inputs ->
          if !online then fake_batch inputs
          else if partial then
            Lwt.return W.{(defer "worker offline" [second]) with
              ready = [{tx = first; receipt = None}]}
          else Lwt.return (W.defer "worker offline" inputs)) ()
    in
    let verify () =
      Lwt_main.run (C.verify_proposal deps ~chain_id:"octra-test" proposal)
    in
    List.iter (fun _ ->
      expect "unavailable verification waits" (verdict_waits (verify ()))) [0; 1];
    expect "worker failure preserved counters" (!prev = 2 && !state = 2);
    expect "worker failure no quarantine" (!quarantines = []);
    expect "worker failure no effects"
      (!stores = [] && !proposals = [] && !previews = []);
    online := true;
    expect "same proposal recovers" (verdict_accepts (verify ()));
    expect "recovery previewed once" (List.length !previews = 1))
    [false; true]

let test_verify_worker_refusal () =
  let first = tx 1 in
  let second = tx 2 in
  let inputs = [first; second] in
  let pending = W.defer "worker offline" [first] in
  let skip kind = W.{tx = second; reason = "refused"; kind} in
  let batches = [
    W.{pending with skipped = pending.skipped @ [skip Invalid]};
    W.{pending with skipped = pending.skipped @ [skip Deferred]};
    pending;
    W.defer "worker offline" [first; first];
    W.defer "worker offline" [first; tx 3];
    W.{ready = [{tx = first; receipt = None}; {tx = second; receipt = None}];
      skipped = [skip Invalid]};
  ] in
  List.iter (fun batch ->
    let deps, quarantines, _, _, _, stores, proposals, previews =
      verify_proposal_deps
        ~cached_bundle:(fun ~proposal_id:_ -> Some (received inputs))
        ~validate_preverify_once:(fun ~state_root:_ ~tx_hashes:_ _ ->
          Lwt.return batch) ()
    in
    expect "invalid or incomplete verification rejects"
      (verdict_rejects (Lwt_main.run
        (C.verify_proposal deps ~chain_id:"octra-test" (proposal_for_txs inputs))));
    expect "refused verification no effects"
      (!quarantines = [] && !stores = [] && !proposals = [] && !previews = []))
    batches

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
      save_drops = ignore;
      chain_id = "proposal-test";
      duty_state = (fun _ -> Ok Octra_core.Set_fold.empty);
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
        { deps with verify_tx_signature = (fun ~signing:_ _ ~pubkey:_ -> false) }
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
        ~inputs:[accepted; rejected]
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
          charged = false;
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

let test_prepare_inputs () =
  let accepted = tx 1 in
  let rejected = tx 2 in
  let proposal, bundle = proposal_outcome ~accepted ~rejected ~reason:"method not found" in
  let calls = ref [] in
  let prepare request =
    calls := request.C.txs :: !calls;
    Lwt.return (Result.map (fun execution -> C.{execution; preverify = request.preverify})
      (outcome_preview ~accepted ~rejected ~reason:"method not found" request)) in
  let deps, _, _, _, _, _, _, _ = verify_proposal_deps
    ~cached_bundle:(fun ~proposal_id:_ -> Some bundle) () in
  expect "different execution inputs accepted" (verdict_accepts (Lwt_main.run
    (C.verify_proposal ~prepare deps ~chain_id:"octra-test" proposal)));
  expect "different execution inputs reused" (!calls = [[accepted; rejected]; [accepted]]);
  let failed request =
    if List.length request.C.txs = 2 then Lwt.return_error "worker unavailable"
    else prepare request in
  expect "failed execution not replaced" (verdict_waits (Lwt_main.run
    (C.verify_proposal ~prepare:failed deps ~chain_id:"octra-test" proposal)))

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

let test_verify_paid () =
  let key = "OCTRA_BFT_RELEASE_PROFILE" in
  let prior = Sys.getenv_opt key in
  let check () =
  let chain_id = "octra-devnet-9871-cluster" in
  let height = (Option.get (Octra_core.Rule_graph.vm_bytes_activation_for_chain chain_id)).activation_epoch in
  let epoch_id = Int64.of_int height in
  let public_key = Base64.encode_exn (String.make 32 'p') in
  let paid = {(tx ~message:"{}" ~ou:10000 1) with op_type = Transaction.ComputeFund;
    from = Octra_core.Crypto.Address.address_from_pubkey public_key;
    to_ = Octra_core.Compute_funding.escrow; public_key = Some public_key;
    signature = Base64.encode_exn (String.make 64 '\000')} in
  let outcomes = Octra_core.Tx_outcome.build ~charged:[paid] ~inputs:[paid]
    [paid, "compute_fund_failed", "input refused"] |> Result.get_ok in
  let receipts = Octra_core.Tx_outcome.encode [] outcomes in
  let base = proposal_for_txs [] in
  let root = folded_root ~ledger_state_root:(raw 'q')
    ~epoch_index_root:(eic_root ~epoch_id:height ~tx_hashes:[] ~start_txid:7L
      ~prev_eic_root:Octra_core.Epoch_index_commitment.genesis_root) in
  let proposal = {base with C_types.chain_id; epoch_id;
    header = {base.header with chain_id; epoch_id; receipt_root = C_hash.receipt_root receipts;
      proposed_state_root = root}} in
  let calls = ref 0 in
  let prepare request =
    incr calls;
    expect "validator omitted paid execution" (request.C.txs = [paid]);
    Lwt.return_ok C.{preverify = Octra_core.Preverify_commit.create [];
      execution = Octra_core.Epoch_exec.{post_state_root = raw 'q'; artifacts = {
        confirmed = []; rejected = [{tx = paid; error_type = "compute_fund_failed";
          reason = "input refused"; charged = true}]; confirmed_fees = paid.ou; tx_count = 1}}} in
  let bundle = F.{txs = []; receipts_json = receipts; rejections = outcomes} in
  let deps, _, _, _, _, _, _, _ = verify_proposal_deps
    ~cached_bundle:(fun ~proposal_id:_ -> Some bundle)
    ~preview_result:(fun _ -> fail "validator skipped prepared execution") () in
  expect "ordered validator refused paid outcome"
    (verdict_accepts (Lwt_main.run (C.verify_proposal ~prepare deps ~chain_id proposal)));
  expect "ordered paid execution repeated" (!calls = 1) in
  Unix.putenv key "devnet_full_v1";
  Fun.protect check ~finally:(fun () -> Unix.putenv key (Option.value prior ~default:""))

let test_receipt_order () =
  let key = "OCTRA_BFT_RELEASE_PROFILE" in
  let prior = Sys.getenv_opt key in
  Unix.putenv key "devnet_full_v1";
  Fun.protect ~finally:(fun () -> Unix.putenv key (Option.value prior ~default:"")) (fun () ->
    let chain_id = "octra-devnet-9871-cluster" in
    let activation = (Option.get
      (Octra_core.Rule_graph.vm_bytes_activation_for_chain chain_id)).activation_epoch in
    let module Receipt = Octra_core.Preverify_receipt in
    let module Gate = Octra_core.Preverify_commit in
    let items = List.init 2 (fun index ->
      let who = Circle_proposal.identity (index + 10) in
      let op_type, method_ = if index = 0 then Transaction.CircleCall, Some "run"
        else Transaction.EncryptOp, None in
      Circle_proposal.signed who ~op_type ~to_:who.address
        ~nonce:1 ~message:"[]" ~method_) |> Transaction.consensus_order in
    let make_receipt item =
      if item.Transaction.op_type <> Transaction.CircleCall then
        let state = Receipt.{pre_state_hash = String.make 64 'a';
          source_cipher_hash = String.make 64 'b'; pvac_key_hash = String.make 64 'c';
          transition_hash = None} in
        Receipt.for_tx_bound ~input_hash:(W.bound_input_hash item state)
          ~output_hash:(W.bound_output_hash item state "ok") ~state ~ok:true ~reason:"" item
        |> Result.get_ok
      else
      let circle = Receipt.{snapshot_hash = String.make 64 'a'; circle_id = item.Transaction.to_;
        code_hash = String.make 64 'b'; stable_root = String.make 64 'c';
        public_reads_hash = String.make 64 'd'; context_hash = String.make 64 'e'; transcript = []} in
      Receipt.make_circle ~tx_hash:(Transaction.hash item)
        ~input_hash:(W.circle_input_hash item circle)
        ~output_hash:(W.circle_output_hash item circle "ok")
        ~circle ~ok:true ~reason:"" ~cost:(Octra_core.Resource_lanes.cost item)
      |> Result.get_ok in
    let batch items = W.{ready = List.map (fun tx ->
      {tx; receipt = if tx.Transaction.op_type = Transaction.ComputeFund then None
        else Some (make_receipt tx)}) items; skipped = []} in
    let prepare request =
      let rejected, confirmed = List.partition (fun item ->
        item.Transaction.op_type = Transaction.ComputeFund) request.C.txs in
      let confirmed = Transaction.consensus_order confirmed in
      let execution = exec_result ~confirmed ~rejected ~post_state_root:(raw 'l')
        |> Result.get_ok in
      let artifacts = execution.Octra_core.Epoch_exec.artifacts in
      let execution = {execution with artifacts = {artifacts with
        rejected = List.map (fun row -> {row with Octra_core.Epoch_exec.charged = true}) artifacts.rejected;
        confirmed_fees = List.fold_left (fun sum item -> Z.add sum item.Transaction.ou) Z.zero rejected}} in
      Lwt.return_ok C.{execution; preverify = Gate.create (List.map make_receipt confirmed)} in
    List.iter (fun height -> List.iter (fun (count, with_paid) ->
      let confirmed = List.filteri (fun index _ -> index < count) items in
      let paid = if with_paid then [Circle_proposal.signed (Circle_proposal.identity 12)
        ~op_type:Transaction.ComputeFund ~to_:Octra_core.Compute_funding.escrow
        ~nonce:1 ~message:"{}" ~method_:None] else [] in
      let staging = List.rev confirmed @ paid in
      if count = 2 then expect "receipt order case has equal input and execution"
        (List.rev confirmed <> confirmed);
      let deps, probe = make_proposal_deps ~current_epoch:height ~staging
        ~build_preverify_once:(fun ~state_root:_ ~tx_hashes:_ txs -> Lwt.return (batch txs)) () in
      let plan = Lwt_main.run (C.make_proposal ~prepare deps ~chain_id
        ~root_to_raw32:Fun.id ~limits:generous_limits ~epoch_id:(Int64.of_int height))
        |> Option.get in
      expect "ordered proposal changed transaction order"
        (plan.tx_hashes = List.map Transaction.hash confirmed);
      let bundle = match !(probe.stored_bundles) with
        | [_, _, txs, receipts_json] -> F.{txs; receipts_json; rejections = []}
        | _ -> fail "ordered proposal bundle absent" in
      let proposal = C_types.{chain_id; epoch_id = Int64.of_int height; round = 0;
        valid_round = None; header = plan.header; tx_hashes = plan.tx_hashes;
        parent_commit = plan.parent_commit; proposer = plan.header.creator_addr; signature = "sig"} in
      let deps, _, _, _, _, _, _, _ = verify_proposal_deps ~staging
        ~cached_bundle:(fun ~proposal_id:_ -> Some bundle)
        ~validate_preverify_once:(fun ~state_root:_ ~tx_hashes:_ txs -> Lwt.return (batch txs)) () in
      expect "validator refused execution-ordered receipts"
        (verdict_accepts (Lwt_main.run (C.verify_proposal ~prepare deps ~chain_id proposal)));
      if count = 2 && not with_paid then
        List.iter (fun changed ->
          let checks = ref 0 in
          let calls = ref 0 in
          let checked ~state_root:_ ~tx_hashes:_ inputs =
            incr checks;
            let result = batch inputs in
            let ready = List.map (fun (entry : W.ready) ->
              if not changed || !checks <> 2 then entry else
              let receipt = Option.map (fun (receipt : Receipt.t) ->
                {receipt with reason = "second verification"}) entry.receipt in
              {entry with receipt}) result.ready in
            Lwt.return W.{result with ready} in
          let deps = {deps with cached_bundle = (fun ~proposal_id:_ -> None);
            validate_preverify_once = checked} in
          let prepare request = incr calls; prepare request in
          expect "receipt input change refused" (verdict_accepts (Lwt_main.run
            (C.verify_proposal ~prepare deps ~chain_id proposal)));
          expect "receipt input change reused" (!calls = if changed then 2 else 1)) [false; true];
      if count = 2 && not with_paid then begin
        let calls = ref 0 in
        let queries = ref 0 in
        let prior = ref None in
        let prepare request =
          incr calls;
          let key = request.C.txs, request.preverify.receipts in
          if !calls = 1 then begin
            prior := Some key;
            Lwt.return_error "worker unavailable"
          end else begin
            expect "retry execution inputs changed" (!prior = Some key);
            prepare request
          end in
        let response = Octra_consensus.C_driver.{responder_addr = "oct_peer";
          tx_hashes = proposal.tx_hashes;
          txs_json = List.map (fun item -> Transaction.to_yojson item |> Yojson.Safe.to_string) bundle.txs;
          receipts_json = bundle.receipts_json} in
        let deps = {deps with cached_bundle = (fun ~proposal_id:_ -> None);
          driver_available = (fun () -> true);
          validate_bundle = (fun ~header:_ ~expected_hashes:_ _ ->
            Some Octra_node_runtime.Consensus_bundle_validation.{tx_hashes = List.map Transaction.hash bundle.txs;
              txs = bundle.txs; receipts_json = bundle.receipts_json; rejections = []});
          query_bundle = (fun ~epoch_id:_ ~proposal_id:_ ~validate ->
            incr queries;
            expect "retry bundle refused" (validate response);
            Lwt.return_some response)} in
        expect "same execution did not recover" (verdict_accepts (Lwt_main.run
          (C.verify_proposal ~prepare deps ~chain_id proposal)));
        expect "failed execution was cached" (!calls = 2 && !queries = 1)
      end;
      let artifacts = Octra_core.Tx_outcome.split_admit bundle.receipts_json |> Result.get_ok in
      let expected = List.map (fun item -> Receipt.canonical (make_receipt item)) confirmed in
      expect "proposal receipts differ from execution order" (artifacts.preverify = expected);
      expect "paid refusal disappeared from receipt order case"
        (List.map (fun row -> row.Octra_core.Tx_outcome.tx, row.charged) artifacts.rejections
         = List.map (fun item -> item, true) paid);
      expect "paid refusal disappeared from replay"
        (Octra_core.Tx_outcome.executed ~confirmed artifacts.rejections
         = Transaction.consensus_order (confirmed @ paid)))
      ([1, false; 2, false] @ if height = activation then [2, true] else []))
      [1_663_000; activation - 1; activation])

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

let test_time_recovery () =
  let item = tx 1 in
  let source = proposal_for_txs [item] in
  let parent = 1_700_086_400.0 in
  List.iter (fun valid_round ->
    let proposal = {
      source with
      round = 1;
      valid_round;
      header = { source.header with ts = parent +. 10.0 };
    } in
    List.iter (fun (now, accepted) ->
      let deps, _, _, _, _, _, _, previews = verify_proposal_deps
        ~now ~previous_epoch_ts:(Some parent) ~staging:[item] () in
      let verdict = Lwt_main.run
        (C.verify_proposal deps ~chain_id:proposal.chain_id proposal) in
      expect "temporal recovery admission differs"
        (if accepted then verdict_accepts verdict else verdict_rejects verdict);
      expect "rejected time entered execution"
        (List.length !previews = if accepted then 1 else 0))
      [parent -. 86_400.0, false; parent +. 4.0, false;
       parent +. 5.0, true; parent +. 10.0, true]) [None; Some 0]

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

let test_tx_hash_check_match () =
  let tx_hashes = [String.make 64 'a'; C.raw32_to_hex (raw 'b')] in
  match
    C.tx_hash_check
      ~expected_tx_list_hash:(C.tx_list_hash tx_hashes)
      ~tx_hashes
  with
  | C.Tx_hash_ok got ->
    expect "hash admission payload" (got = tx_hashes)
  | C.Tx_hash_mismatch -> fail "tx hash admission accept"

let test_tx_hash_check_mismatch () =
  match
    C.tx_hash_check
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
  expect "empty default txid hi" (h.C_types.txid_hi = 9L)

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
  let superseded_set =
    C_types.make_validator_set [
      C_types.{ address = "oct_other"; pubkey = String.make 32 '\x03' };
    ]
  in
  let active = ref superseded_set in
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
  expect "superseded validator set refused" (not (run ()));
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

let test_circle_path ?(focus = false) () =
  Circle_proposal.run ~focus ~limits:generous_limits
    ~make_deps:(fun ~epoch ~staging ~root ->
      fst (make_proposal_deps ~current_epoch:epoch ~staging ~ledger_root:root ()))
    ~verify_deps:(fun ~staging ~root ~ledger_root ->
      let deps, _, _, _, _, _, _, _ =
        verify_proposal_deps ~staging ~root ~ledger_root () in
      deps) ()

let test_circle_host ?(cut_only = false) () =
  Circle_host.run ~cut_only
    ~make_deps:(fun ~epoch ~staging ~root ->
      fst (make_proposal_deps ~current_epoch:epoch ~staging ~ledger_root:root ()))
    ~verify_deps:(fun ~staging ~root ~ledger_root ->
      let deps, _, _, _, _, _, _, _ =
        verify_proposal_deps ~staging ~root ~ledger_root () in
      deps) ()

let () =
  Circle_host.worker ();
  if Array.length Sys.argv = 3 && Sys.argv.(1) = "--hfhe-checks" then begin
    List.iter (fun accepted -> Test_workspace.with_dir "proof_routes" (fun output ->
      Circle_host.proof_path ~retry:true ~range:true ~cold:true ~accepted
        ~input:Sys.argv.(2) ~output
        ~make_deps:(fun ~epoch ~staging ~root ->
          fst (make_proposal_deps ~current_epoch:epoch ~staging ~ledger_root:root ()))
        ~verify_deps:(fun ~staging ~root ~ledger_root ->
          let deps, _, _, _, _, _, _, _ = verify_proposal_deps ~staging ~root ~ledger_root () in deps) ()))
      [false; true];
    print_endline "event = proof_routes status = passed";
    exit 0
  end;
  if Array.length Sys.argv = 4 && List.mem Sys.argv.(1)
      ["--hfhe-window"; "--hfhe-retry"; "--hfhe-chain"; "--hfhe-late"; "--hfhe-cold"; "--hfhe-large"] then begin
    Printexc.record_backtrace true;
    Circle_host.proof_path ~retry:(Sys.argv.(1) <> "--hfhe-window")
      ~range:(Sys.argv.(1) = "--hfhe-large")
      ~accepted:(Sys.argv.(1) <> "--hfhe-late")
      ~cold:(Sys.argv.(1) = "--hfhe-cold")
      ~calls:(if List.mem Sys.argv.(1) ["--hfhe-chain"; "--hfhe-cold"] then 2 else 1)
      ~input:Sys.argv.(2) ~output:Sys.argv.(3)
      ~make_deps:(fun ~epoch ~staging ~root ->
        fst (make_proposal_deps ~current_epoch:epoch ~staging ~ledger_root:root ()))
      ~verify_deps:(fun ~staging ~root ~ledger_root ->
        let deps, _, _, _, _, _, _, _ = verify_proposal_deps ~staging ~root ~ledger_root () in deps) ();
    print_endline "event = hfhe_window status = passed";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--active-cut" then begin
    test_circle_host ~cut_only:true ();
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--preview-selection" then begin
    test_window_renew ();
    test_sender_reserve ();
    test_repeat_window ();
    test_completion_window ();
    test_refill_core ();
    test_refill_saved ();
    test_publish_failure ();
    test_proposal_prefix ();
    test_preview_selection ();
    test_preview_replay ();
    test_program_window ();
    test_budget_growth ();
    print_endline "event = preview_selection status = passed";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--host-budget" then begin
    test_window_renew ();
    test_sender_reserve ();
    test_repeat_window ();
    test_completion_window ();
    test_refill_core ();
    test_refill_saved ();
    test_publish_failure ();
    test_paid_proposal ();
    test_preview_moved ();
    test_paid_change ();
    test_preview_expiry ();
    test_program_window ();
    test_budget_growth ();
    test_circle_host ();
    test_circle_path ~focus:true ();
    test_round_window ();
    test_proposal_prefix ();
    test_preview_selection ();
    test_preview_replay ();
    test_build_budget ();
    test_host_budget ();
    test_preview_duration ();
    test_prepare_cancel ();
    test_prepare_reuse ();
    test_verify_host_cancel ();
    print_endline "event = host_budget status = passed";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--prepare" then begin
    test_prepare_reuse ();
    test_prepare_cancel ();
    test_prepare_closed ();
    test_prepare_inputs ();
    test_verify_paid ();
    test_receipt_order ();
    print_endline "event = proposal_prepare status = passed";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--preview-time" then begin
    test_preview_duration ();
    test_preview_time ();
    test_preview_resources ();
    test_build_head_progress ();
    print_endline "event = preview_time status = passed";
    exit 0
  end;
  if Array.exists (fun value -> value = "--time" || value = "selection") Sys.argv then begin
    test_preview_time ();
    print_endline "status = pass test = proposal_time";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--cell" then begin
    test_cell_retry ();
    print_endline "status = pass test = cell_proposal";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--order" then begin
    test_receipt_order ();
    print_endline "status = pass test = receipt_order";
    exit 0
  end;
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--compute" then begin
    test_compute_budget ();
    test_paid_proposal ();
    test_preview_moved ();
    test_paid_change ();
    test_verify_paid ();
    print_endline "status = pass test = compute_proposal";
    exit 0
  end;
  if Array.length Sys.argv = 6 && Sys.argv.(1) = "circle-store" then
    Circle_store.child ();
  test_window_renew ();
  test_sender_reserve ();
  test_completion_window ();
  test_repeat_window ();
  test_circle_host ();
  test_circle_path ();
  Circle_store.run ();
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
  test_private_slot_preview ();
  test_layera_validator_addrs ();
  test_layera_diag_context ();
  test_layera_env_diag_context ();
  test_validator_pubkeys_local ();
  test_preview_exec_env ();
  test_check_plan ();
  test_check_proceed ();
  test_check_realign ();
  test_check_quarantine ();
  test_make_unattested_defer ();
  test_make_epoch_time_defer ();
  test_preverify_single_flight ();
  test_build_head_progress ();
  test_make_reuse_frozen_bundle ();
  test_circle_budget ();
  test_compute_budget ();
  test_paid_proposal ();
  test_preview_moved ();
  test_paid_change ();
  test_make_preview_rejections ();
  test_refill_saved ();
  test_publish_failure ();
  test_refill_core ();
  test_private_apply_slot ();
  test_private_refill_cap ();
  test_private_refill_steps ();
  test_make_rejection_only ();
  test_make_keep_partition ();
  test_make_dependency_chain ();
  test_make_preview_error_defer ();
  test_preview_resources ();
  test_preview_time ();
  test_preview_duration ();
  test_round_window ();
  test_proposal_prefix ();
  test_preview_selection ();
  test_preview_replay ();
  test_preview_expiry ();
  test_program_window ();
  test_budget_growth ();
  test_build_budget ();
  test_host_budget ();
  test_verify_host_cancel ();
  test_make_partition_error_defer ();
  test_verify_bundle_success ();
  test_circle_receipt ();
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
  test_unproven_roots ();
  test_verify_missing_prev_time ();
  test_prev_time_retry ();
  test_verify_missing_bundle_wait ();
  test_verify_local_preview ();
  test_prepare_reuse ();
  test_prepare_cancel ();
  test_prepare_closed ();
  test_prepare_inputs ();
  test_verify_worker_retry ();
  test_verify_worker_refusal ();
  test_verify_head_progress ();
  test_verify_staging_lookup ();
  test_verify_ledger_preverify ();
  test_verify_reproduced_rejection ();
  test_verify_paid ();
  test_receipt_order ();
  test_verify_forged_rejection ();
  test_verify_old_valid_round ();
  test_future_reproposal_prior ();
  test_future_reproposal_active ();
  test_time_recovery ();
  test_verify_invalid_valid_round ();
  test_tx_hash_check_match ();
  test_tx_hash_check_mismatch ();
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
  test_cell_retry ();
  test_before_precommit_reproposal ();
  test_before_precommit_active_set ();
  test_before_precommit_decoded ();
  test_before_precommit_missing ();
  test_precommit_wrong_validator ();
  test_precommit_write_failure ();
  print_endline "status = pass test = proposal"