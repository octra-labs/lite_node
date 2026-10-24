(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module P = C.Private_ledger
module T = C.Transaction
module S = C.Store_irmin
module B = Octra_node_runtime.Consensus_epoch_private_balance_shell

let check reason value = if not value then failwith reason
let run = Lwt_main.run
let get = function Ok value -> value | Error _ -> failwith "unexpected private failure"
let owner = "oct" ^ String.make 44 '1'
let write path bytes =
  let out = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out out) (fun () -> output_string out bytes)

let tx op = T.{from = owner; to_ = owner; amount = Z.one; nonce = 1;
  ou = Z.of_int 100; timestamp = 1.; signature = ""; public_key = None;
  message = None; encrypted_data = None; op_type = op}

let private_artifact ledger registered op =
  run (P.preverify_private_artifact ~registered ~keyed:false ~math:false
    ~field_policy:P.Unique_fields ~strict:true ledger (tx op)) |> get

let private_bind ledger registered op artifact =
  run (P.bind_private_artifact ~registered ~keyed:false ~math:false
    ~field_policy:P.Unique_fields ~strict:true ledger (tx op) artifact)

let test_files store ledger =
  let path = S.pvac_legacy_path store owner in
  C.Ledger.set_pvac_kat ledger owner "wrong";
  List.iter (fun op ->
    write path "one";
    let old = private_artifact ledger false op in
    let fresh = private_artifact ledger true op in
    check "old artifact crossed registered rule"
      (private_bind ledger true op old = P.Private_source_changed);
    write path "two";
    check "historical file no longer affects source"
      (private_bind ledger false op old = P.Private_source_changed);
    check "registered result depends on local file" (match private_bind ledger true op fresh with
      | P.Private_artifact_invalid _ -> true | _ -> false))
    [T.EncryptOp; T.DecryptOp; T.StealthOp; T.ClaimOp];
  let operation = {(tx T.KeySwitch) with encrypted_data = Some
    "{\"new_pubkey\":\"\",\"aes_kat\":\"wrong\"}"} in
  let artifact registered = run (P.preverify_key_switch_artifact ~registered ~keyed:false
    ~field_policy:P.Unique_fields ~strict:true ledger operation) |> get in
  let bind registered proof = run (P.bind_key_switch_artifact ~registered ~keyed:false
    ~field_policy:P.Unique_fields ~strict:true ledger operation proof) in
  let old = artifact false in
  let fresh = artifact true in
  check "key artifact crossed registered rule" (bind true old = P.Key_switch_source_changed);
  write path "three";
  check "key snapshot ignored old file change" (bind false old = P.Key_switch_source_changed);
  check "registered key snapshot depends on old file"
    (match bind true fresh with P.Key_switch_artifact_invalid _ -> true | _ -> false);
  run (S.set_pvac_pubkey store owner "registered");
  Unix.unlink (S.pvac_blob_path store (S.pvac_hash "registered"));
  check "missing registered file became transaction error"
    (try ignore (private_artifact ledger true T.EncryptOp); false
     with C.Exec_resource.Unavailable Host -> true)

let test_kat ledger =
  let run_op registered run_op =
    let planned = ref false and rejected = ref false in
    let deps = B.live_ledger_balance_op_deps B.{
      registered; ledger; tx = tx T.EncryptOp; gate = (fun () -> None);
      plan = (fun () -> planned := true; Lwt.return_error P.{tag = "test"; reason = "test"; user_reason = "test"});
      log_failure = ignore; trace_cipher = (fun _ _ _ _ -> ()); mark_debit = ignore;
      incr_fhe = ignore; reject_gate = (fun _ -> failwith "unexpected balance gate");
      reject_failure = (fun error -> rejected := error.P.tag <> "test"; Lwt.return_unit);
      short_addr = Fun.id; confirm = (fun () -> failwith "unexpected confirmation");
    } in
    run (run_op deps);
    check "Single KAT rule differs" (!planned = registered && !rejected = not registered) in
  List.iter (fun registered -> run_op registered B.run_encrypt; run_op registered B.run_decrypt) [false; true]

let test_key_actor store =
  let module A = Octra_node_runtime.Pvac_status_actor in
  let module M = C.Pvac_migration in
  let addr = "oct" ^ String.make 44 '3' in
  write (S.pvac_legacy_path store addr) "local-key";
  let pending, wake = Lwt.wait () in
  let reads = ref [] in
  let actor = A.create A.{
    load_pubkey = (fun ~registered ~addr ->
      reads := registered :: !reads;
      let open Lwt.Syntax in
      let* () = if registered then Lwt.return_unit else pending in
      S.get_pvac_pubkey ~registered store addr);
    hash_pubkey = S.pvac_hash;
    classify_cipher = (fun _ -> M.Empty);
    classify_key = (function None -> M.Missing | Some "local-key" -> M.Historical | Some _ -> M.Current);
  } in
  Fun.protect ~finally:(fun () -> run (A.shutdown actor)) (fun () ->
    let query registered = A.query actor ~registered ~addr ~key_hash:None ~cipher:"0" in
    let before = query false and after = query true in
    Lwt.wakeup wake ();
    let first, second = run (Lwt.both before after) in
    check "actor lost captured key mode" ((get first).M.key_class = M.Historical && (get second).key_class = M.Missing);
    check "actor reused cross-mode key" (!reads = [true; false]);
    check "actor warm cache crossed mode" ((get (run (query false))).key_class = M.Historical);
    check "actor reread cached mode" (!reads = [true; false]));
  let rpc registered = run (Octra_node_runtime.Account_read_rpc.pvac_pubkey ~registered store ~addr) in
  check "RPC key lookup ignored mode" (rpc false <> rpc true);
  run (S.set_pvac_pubkey store addr "committed-key");
  Unix.unlink (S.pvac_blob_path store (S.pvac_hash "committed-key"));
  check "RPC missing blob became absence"
    (try ignore (rpc true); false with C.Exec_resource.Unavailable Host -> true)

let test_cell_fault () =
  Test_workspace.with_dir "cell-read-fault" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "store")) in
    let ledger = C.Ledger.create store in
    run (S.close store);
    let module Cell = Octra_node_runtime.Consensus_circle_cell_preverify in
    let root = String.make 64 'a' in
    let runtime = Cell.{store; ledger;
      rules = C.Rule_graph.create ~chain_id:"test-cell" ~root_at:(fun _ -> C.Rule_graph.Missing);
      env = (fun ~pre_state_root -> C.Epoch_exec.{chain_id = "test-cell"; epoch_id = 1;
        proposer_addr = owner; validator_addrs = []; validator_pubkeys = [];
        prev_state_root = pre_state_root; epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1})} in
    check "local cell read fault became invalid receipt"
      (match run (Cell.run runtime ~pre_state_hash:(C.Preverify_worker.state_hash root)
          ~pre_state_root:root (tx T.CircleBalanceCellPut)) with
       | Error (`Unavailable _) -> true | _ -> false);
    List.iter (fun op ->
      let called = ref false in
      let cell = {(tx op) with encrypted_data = Some (Base64.encode_exn "cipher")} in
      let result = run (C.Preverify_worker.run ~registered:false ~keyed:false
        ~field_policy:P.Unique_fields ~strict:false ~ledger
        ~pre_state_hash:(C.Preverify_worker.state_hash root) ~pre_state_root:root
        ~circle_cell_preverify:(fun ~pre_state_hash:_ ~pre_state_root:_ _ ->
          called := true; Lwt.return_ok root) cell) in
      check "cell source read fault did not defer"
        (match result with C.Preverify_worker.Defer _ -> not !called | _ -> false))
      [T.CircleBalanceCellPut; T.CircleRegisterCellPut])

let test_single_failure () =
  let module N = Octra_node_runtime in
  let module L = C.Ledger in
  let module E = C.Epoch_exec in
  let module G = C.Rule_graph in
  let module Cache = N.Preverify_cache in
  let module Live = N.Consensus_epoch_apply_sender_live in
  let module Shared = N.Consensus_epoch_apply_shared in
  let chain = "octra-devnet-9871-cluster" in
  let other = "oct" ^ String.make 44 '3' in
  let stealth = {(tx T.StealthOp) with to_ = "stealth"; amount = Z.zero} in
  let hash = T.hash stealth in
  Test_workspace.with_dir "single-task-failure" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "store")) in
    let chaindata = C.Store_chaindata.open_chaindata (Filename.concat dir "chain") in
    Fun.protect ~finally:(fun () -> Cache.remove hash; S.abort_epoch_batch store;
      run (S.close store); C.Store_chaindata.abort_batch chaindata;
      C.Store_chaindata.close chaindata) (fun () ->
      let ledger = L.create store in
      List.iter (fun addr -> ignore (get (L.add_account ledger addr (Z.of_int 1_000_000))))
        [owner; other];
      L.set_pvac_kat ledger owner (C.Pvac_registry.expected_kat ());
      run (L.flush_dirty_lwt ledger);
      run (S.begin_epoch_batch store);
      C.Store_chaindata.begin_batch chaindata;
      List.iter (fun epoch ->
        let registered = G.program_inputs_at ~chain_id:chain ~epoch = G.Active in
        check "Single activation changed" (registered = (epoch >= 1_740_000));
        let env = E.{chain_id = chain; epoch_id = epoch; proposer_addr = owner;
          validator_addrs = []; validator_pubkeys = []; prev_state_root = String.make 64 'a';
          epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = -1} in
        List.iter (fun error ->
          Cache.remove hash;
          let pending, wake = Lwt.task () in
          ignore (Cache.start_task ~registered ~keyed:false hash (fun () -> pending));
          Lwt.wakeup_exn wake error;
          let before = L.find ledger owner in
          let next = L.find ledger other in
          let ordinary = {(tx T.Standard) with from = other; to_ = other;
            nonce = next.nonce + 1} in
          let deferred = ref [] and confirmed = ref [] and rejected = ref [] in
          let processed = ref [] and fatal = ref [] and exits = ref 0 in
          let fees = ref Z.zero in
          let live = Live.{ledger; store; chaindata; program_trust = Octra_vm.Program_trust.empty;
            object_cost = false; cipher_mode = G.Prior; wallet_addr = owner;
            pre_state_hash = env.prev_state_root; fold = E.prior_fold;
            standard_env = (fun () -> env); current_epoch = (fun () -> epoch);
            max_fhe_per_epoch = 4; max_stealth_per_epoch = 4; max_stealth_defer = 2;
            stealth_inline_verify_allowed = false; fhe_in_epoch_counter = ref 0;
            stealth_in_epoch_counter = ref 0; stealth_defer_count = Hashtbl.create 1;
            deferred_stealth_txs = deferred; confirmed_fees = fees; short_addr = Fun.id;
            log_rejected = (fun _ tag _ -> rejected := tag :: !rejected);
            confirm_tx = (fun tx -> confirmed := tx :: !confirmed);
            notify_new_account = ignore; notify_confirmed = (fun _ _ -> ());
            notify_rejected = (fun _ _ -> ());
            legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected migration");
            private_field_policy = P.Unique_fields;
            private_result_policy = (fun _ -> C.Private_result_policy.Recoverable)} in
          let runtime = Shared.{consensus_mode = false; current_epoch = live.current_epoch;
            log_shared = (fun ~tx_count:_ -> failwith "unexpected shared route"); short = Fun.id;
            fatal = (fun reason -> fatal := reason :: !fatal); exit = (fun () -> incr exits);
            backend = (fun () -> E.make_live_backend ~fold:E.prior_fold store ledger);
            env = (fun () -> env); max_fhe_per_epoch = 4; max_stealth_per_epoch = 4;
            process_sender = (fun txs ->
              processed := (List.hd txs).T.from :: !processed;
              Live.run live txs);
            confirm_tx = ignore; reject_tx = (fun _ _ _ -> ());
            notify_confirmed = (fun _ _ -> ()); notify_rejected = (fun _ _ -> ());
            program_trust = Octra_vm.Program_trust.empty; circle_mode = G.Prior;
            wasm_compute_mode = G.Prior; object_cost = false; owner_migration_mode = G.Prior;
            cipher_mode = G.Prior; proof_mode = G.Prior; math = false;
            private_field_policy = P.Unique_fields; private_result_policy = live.private_result_policy;
            legacy_replay = live.legacy_replay; add_rejected_fee = ignore;
            advance_validator_set = (fun () -> Lwt.return_unit)} in
          run (Shared.run_runtime runtime [ordinary; stealth]);
          check "failed cache reached Single exit" (!fatal = [] && !exits = 0);
          check "failed cache blocked next sender" (List.rev !processed = [owner; other]
            && !confirmed = [ordinary]);
          check "failed cache did not defer" (!deferred = [stealth] && !rejected = []);
          check "failed cache changed private state" (L.find ledger owner = before && !fees = Z.zero);
          check "next sender did not apply" ((L.find ledger other).nonce = ordinary.nonce);
          check "failed cache left journal open" (not (L.journal_active ledger)))
          [Failure "pvac blob unavailable"; C.Exec_resource.Unavailable Host;
           P.Worker_retry "worker busy"; Out_of_memory; Stack_overflow; Lwt.Canceled])
        [1_737_332; 1_737_333; 1_739_999; 1_740_000]))

let test_defer_cleanup () =
  let module Cleanup = Octra_node_runtime.Consensus_epoch_cleanup in
  C.Tx_staging.clear ();
  let counts = Hashtbl.create 3 in
  List.iter (fun name -> Hashtbl.add counts name 2) ["expired"; "removed"; "finished"];
  let live = tx T.Standard in
  ignore (get (C.Tx_staging.add_smart ~lookup:(fun _ -> Some (Z.of_int 100_000_000, 0)) live));
  let hash = T.hash live in
  Hashtbl.add counts hash 1;
  let refs = Cleanup.refs ~last_epoch_time:(ref 0.)
    ~tree:(ref (C.Tree.create ~epoch_id:1 ~parent_commit:""))
    ~deferred_stealth_txs:(ref []) ~stealth_in_epoch_counter:(ref 0)
    ~stealth_defer_count:counts ~fhe_in_epoch_counter:(ref 0) ~swarm_opt:(ref None)
    ~save_drops:ignore ~confirmed_nonce:(fun _ -> None) in
  let context = Cleanup.{now = 1.; epoch_id = 1; parent_commit = "";
    post_consensus_root = ""; confirmed_count = 0; producer = owner;
    processed_hashes = []; txs_serialized = []} in
  run (Cleanup.run_node refs context);
  check "cleanup dropped live defer counter" (Hashtbl.length counts = 1 && Hashtbl.find counts hash = 1);
  C.Tx_staging.clear ();
  run (Cleanup.run_node refs context);
  check "defer counters survived removal from staging" (Hashtbl.length counts = 0)

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--local" then begin
    test_cell_fault ();
    test_single_failure ();
    print_endline "status = pass test = local_preverify";
    exit 0
  end;
  check "ready I/O changed" (C.Exec_resource.await (Lwt.return 7) = 7);
  check "failed I/O changed" (try ignore (C.Exec_resource.await (Lwt.fail Exit)); false with Exit -> true);
  let unresolved, _ = Lwt.wait () in
  check "exhausted I/O became transaction refusal"
    (try ignore (C.Exec_resource.await ~steps:0 unresolved); false with C.Exec_resource.Unavailable Host -> true);
  let engine = Lwt_engine.get () in
  let ticks = ref 0 in
  let clock = object inherit Lwt_engine.select method! iter _ = incr ticks end in
  Lwt_engine.set ~transfer:false ~destroy:false clock;
  let pending, _ = Lwt.wait () in
  Fun.protect ~finally:(fun () -> Lwt_engine.set ~transfer:false engine) (fun () ->
    check "local I/O deadline became program error"
      (try ignore (Octra_vm.Contract.run_s pending); false
       with C.Exec_resource.Unavailable Host -> true));
  check "I/O test did not exhaust pump" (!ticks = 100001);
  Test_workspace.with_dir "private-keys" (fun dir ->
    let store = run (S.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = C.Ledger.create store in
      ignore (C.Ledger.add_account ledger owner (Z.of_int 1000));
      test_files store ledger; test_kat ledger; test_key_actor store));
  test_cell_fault ();
  test_single_failure ();
  test_defer_cleanup ();
  print_endline "status = pass test = private_keys"