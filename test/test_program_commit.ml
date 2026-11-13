(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case

module D = C.Store_chaindata
module Head = C.Head_manifest
module Index = C.Epoch_index_commitment
module Commit = Consensus_epoch_commit
module Shared = Consensus_epoch_apply_shared
module Start = Consensus_epoch_apply_start_shell

let source = {|
program PaidCommit {
  state { counter: int }
  constructor() { self.counter = 0 }
  payable fn refuse(to: address) {
    self.counter += 1
    require(transfer(to, 1), "payment refused")
    require(false, "caller refused")
  }
}
|}

let key, public, owner = identity 180
let _, payee_public, payee = identity 181
let _, leader_public, leader = identity 182
let package = compile source
let address = V.Contract.addr_from_code package.envelope owner 1
let path dir name = Filename.concat dir name

let with_stores dir action =
  let store = run (S.open_store (path dir "irmin_store")) in
  Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
    let chaindata = D.open_chaindata (path dir "chaindata") in
    Fun.protect ~finally:(fun () -> D.close chaindata)
      (fun () -> action store chaindata (C.Ledger.create store)))

let read path = In_channel.with_open_bin path (fun channel ->
  Yojson.Safe.from_string (In_channel.input_all channel))

let clock dir = Yojson.Safe.Util.to_float (read (path dir "clock.json"))

let state ?root store ledger =
  let addresses = [owner; payee; leader; address] @ List.init 4 (fun index ->
    let _, _, value = identity (210 + index) in value) in
  let root = match root with Some root -> root | None -> run (C.Ledger.hash ledger) in
  `Assoc ["root", `String root;
    "accounts", `List (List.map (fun addr ->
      let account = Option.get (C.Ledger.find_opt ledger addr) in
      `Assoc ["address", `String addr; "balance", `String (Z.to_string account.balance);
        "nonce", `Int account.nonce]) addresses);
    "storage", `Assoc (List.map (fun (key, value) -> key, `String value) (fields store address))]

let prepare dir =
  let now = Unix.gettimeofday () in
  save (path dir "clock.json") (`Float now);
  with_stores dir (fun store chaindata ledger ->
    List.iter (fun (addr, pubkey) ->
      C.Ledger.add_account_with_pubkey ledger addr initial pubkey |> unwrap)
      [owner, public; payee, payee_public; leader, leader_public];
    run (C.Ledger.flush_dirty_lwt ledger);
    run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
    run (S.set_meta store "total_supply" (Z.to_string (C.Ledger.get_total_supply ledger)));
    run (S.set_meta store "emission_remaining" "0");
    let env = environment now leader in
    Source_proposal.initialize ~store ~ledger ~env;
    let tx = transaction ~timestamp:now ~public ~sender:owner ~target:address ~nonce:1 ~fee:400_000
      ~operation:T.ProgramDeploy ~payload:(Base64.encode_exn package.package) ~params:(`List []) |> sign key in
    C.Tx_staging.clear ();
    (match ingress ledger tx with Ok _ -> ()
      | Error error -> failwith ("commit setup RPC refused: " ^ error.C.Rpc.message));
    let request, execution, _ = Source_proposal.check_many ~progress:false ~store ~ledger ~env ~expected:[tx] [tx] in
    need "commit setup execution differs" (Source_proposal.apply ~store ~ledger request = execution);
    C.Tx_staging.clear ();
    let ledger_root = run (C.Ledger.hash ledger) in
    let epoch_hash, epoch_root = Index.next_root ~prev:Index.genesis_root ~epoch_id:epoch [] in
    let state_root = Index.folded_state_root ~ledger_state_root:ledger_root ~epoch_index_root:epoch_root in
    let parent_commit = Source_proposal.parent epoch state_root (now +. 10.) in
    let validator_set_hash = Octra_consensus.C_config.validator_set_hash parent_commit.validator_set
      |> Octra_bootstrap.State_sync_checkpoint.raw_to_hex in
    let floor = C.History_floor.create ~chain_id:chain ~epoch ~state_root ~ledger_state_root:ledger_root
      ~txid_hi:0L ~config_hash:(String.make 64 'a') ~validator_set_hash
      ~epoch_index_hash:epoch_hash ~epoch_index_root:epoch_root ~parent_commit |> unwrap in
    D.seed_history_floor chaindata floor |> unwrap;
    run (S.tag_epoch store epoch);
    let segment, offset = D.txlog_position chaindata in
    let head = Commit.head_manifest ~generation:epoch ~state_root ~ledger_root
      ~irmin_commit:(run (S.get_commit_hash store)) ~txid_hi:0L ~txlog_seg:segment ~txlog_off:offset
      ~epochlog_off:(D.epochlog_offset chaindata) ~commit_id:"paid-start" ~ts:now
      ~epoch_index_hash:epoch_hash ~epoch_index_root:epoch_root in
    Head.atomic_write dir head;
    save (path dir "before.json") (state store ledger))

let refused_tx dir =
  let tx = transaction ~timestamp:(clock dir) ~public ~sender:owner ~target:address ~nonce:2 ~fee:1000
    ~operation:T.ProgramExec ~payload:"refuse" ~params:(`List [`String payee]) in
  sign ~at:(epoch + 1) key {tx with T.amount = Z.of_int 10}

let begin_apply ~store ~chaindata ~ledger ~head ~env receipts =
  let source = Consensus_epoch_apply_source.{rules = rules ();
    find_finalized = (fun _ -> failwith "unexpected finalized read");
    cached_bundle = (fun _ -> failwith "unexpected bundle read");
    receipt_root_matches = (fun _ _ -> failwith "unexpected bundle check");
    header_has_empty_bundle = (fun _ -> failwith "unexpected empty check");
    staging_txs = (fun () -> failwith "unexpected staging read");
    store_empty_bundle = (fun _ -> failwith "unexpected bundle write");
    fatal = failwith; exit = (fun () -> failwith "paid source exited")} in
  let preflight = Consensus_epoch_apply_preflight.{
    begin_store_batch = (fun () -> C.Ledger.begin_journal ledger |> unwrap;
      S.begin_epoch_batch store);
    begin_chaindata_batch = (fun () -> D.begin_batch chaindata);
    ledger_hash = (fun () -> C.Ledger.hash ledger);
    cached_head = (fun () -> Some head);
    expected_prev_root = (fun _ -> Some head.Head.state_root);
    check_execution = (fun epoch head -> Consensus_profile.check_execution_parent
      ~chain_id:chain ~epoch ~store ~head Sys.getenv_opt);
    fatal = failwith; require_sync = (fun _ -> failwith "paid start needs sync");
    exit = (fun () -> failwith "paid preflight exited")} in
  let epoch_env = Consensus_epoch_apply_env.{chain_id = chain;
    current_epoch = (fun () -> env.E.epoch_id); epoch_ts = (fun _ -> Some env.epoch_ts);
    driver = (fun () -> None); validator_fallback = (fun _ -> env.validator_pubkeys);
    ready_state_root_at = (fun _ -> Lwt.return_none); ready_max_lag = 0} in
  run (Start.run Start.{source; preflight; epoch_env; now = (fun () -> env.epoch_ts);
    staging_size = (fun () -> 0); clear_spent_nonces = (fun () -> ());
    log_processing = (fun ~epoch_id:_ ~tx_count:_ ~deferred:_ ~elapsed:_ -> ())}
    Start.{epoch_id = env.epoch_id; override_ordered_txs = Some [];
      override_receipts_json = Some receipts; consensus_mode = true; elapsed = 0.})

let apply dir phase =
  with_stores dir (fun store chaindata ledger ->
    let now = clock dir in
    let head = Option.get (Head.load dir) in
    let tx = refused_tx dir in
    let env = {(environment (now +. 10.) leader) with E.epoch_id = epoch + 1} in
    C.Tx_staging.clear ();
    need "paid commit RPC refused" (Result.is_ok (ingress ~at:env.epoch_id ledger tx));
    let request, expected, _ = Source_proposal.check_many ~progress:false ~store ~ledger ~env
      ~expected:[] ~paid:[tx, "execution reverted [caller refused]"] [tx] in
    C.Tx_staging.clear ();
    let parent_commit = request.Consensus_proposal.parent_commit in
    let reward = Consensus_reward_attribution.resolve_for_epoch ~chain_id:chain
      ~epoch_id:request.epoch_id ~proposer_addr:request.proposer
      ~validator_pubkeys:request.validator_pubkeys parent_commit |> unwrap in
    let env = Consensus_proposal.epoch_exec_env ~chain_id:chain ~epoch_id:request.epoch_id
      ~epoch_ts:request.epoch_ts ~proposer:request.proposer ~validator_pubkeys:request.validator_pubkeys
      ~prev_state_root:request.prev_state_root ~ready_state_root_at:(fun _ -> Lwt.return_none) ~ready_max_lag:0 in
    let fold = Set_rule.bind (rules ()) ~chain_id:chain ~parent:parent_commit ~epoch:env.epoch_id |> unwrap in
    let policy = fold env.epoch_id |> unwrap in
    let backend = E.make_live_backend ~fold ~proof_mode:policy.standard_mode ~math:policy.math
      ~execution_profile:(fun ~chain_id ~epoch ->
        Consensus_profile.execution_profile ~chain_id ~epoch Sys.getenv_opt) store ledger in
    let rejections = C.Tx_outcome.build ~inputs:[tx] ~charged:[tx]
      [tx, "program_exec_failed", "execution reverted [caller refused]"] |> unwrap in
    let receipts = C.Tx_outcome.encode
      (List.map (fun receipt -> C.Preverify_receipt.to_yojson receipt |> Yojson.Safe.to_string)
        request.preverify.receipts) rejections in
    let owner_before = C.Ledger.find ledger owner in
    let other_accounts = List.map (fun addr -> addr, C.Ledger.find ledger addr) [payee; leader; address] in
    let storage_before = fields store address in
    let started = begin_apply ~store ~chaindata ~ledger ~head ~env receipts in
    need "paid start set differs" (started.ordered_txs = [tx]
      && started.epoch_rejections = rejections && started.epoch_receipts_json = receipts);
    let fees = started.confirmed_fees and pending = started.pending_tx_saves and count = ref 0 in
    let runtime = Shared.{ledger; store; chaindata; program_trust = V.Program_trust.empty; rules = rules ();
      wallet_addr = leader; pre_state_hash = request.prev_state_root;
      standard_env = (fun () -> env); current_epoch = (fun () -> env.epoch_id); consensus_mode = true;
      max_fhe_per_epoch = 8; max_stealth_per_epoch = 8; max_stealth_defer = 0;
      stealth_inline_verify_allowed = false; fhe_in_epoch_counter = ref 0; stealth_in_epoch_counter = ref 0;
      stealth_defer_count = Hashtbl.create 1; pending_tx_saves = pending; total_tx_count = count;
      confirmed_fees = fees; processed_hashes = started.processed_hashes;
      short = Fun.id; log_shared = (fun ~tx_count:_ -> ());
      fatal = failwith; exit = (fun () -> failwith "paid commit exited"); notify_new_account = ignore;
      notify_confirmed = (fun _ _ -> ()); notify_rejected = (fun _ _ -> ());
      legacy_replay = (fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "paid commit requested private history");
      private_result_policy = (fun _ -> C.Private_result_policy.Recoverable)} in
    ignore (run (Shared.run_node ~preverify:request.preverify ?parent_commit runtime started.ordered_txs));
    need "paid commit classified refusal as success" (!pending = [] && !count = 0);
    need "paid commit fee differs" (!fees = tx.ou);
    need "paid refusal hash missing" (!(started.processed_hashes) = [T.hash tx]);
    let owner_after = C.Ledger.find ledger owner in
    need "paid refusal debit differs" (owner_after.balance = Z.sub owner_before.balance tx.ou
      && owner_after.nonce = owner_before.nonce + 1);
    List.iter (fun (addr, before) ->
      need "paid refusal changed another account" (C.Ledger.find ledger addr = before)) other_accounts;
    need "paid refusal changed program storage" (fields store address = storage_before);
    need "paid rejection rows refused"
      (Consensus_epoch_apply_sender.save_live_rejections ~chaindata ~epoch_id:env.epoch_id
        ~ts:env.epoch_ts ~processed_hashes:started.processed_hashes
        ~notify_rejected:(fun _ _ -> ()) started.epoch_rejections = Ok ());
    let plan = E.build_reward_plan ~fee_burn_active:false ~supply_retired:Z.zero
      ~validator_count:(List.length reward.validators) ~emission_remaining:Z.zero
      ~confirmed_fees:!fees ~prev_supply:(Z.of_string (Option.get (run (S.get_meta store "total_supply")))) |> unwrap in
    run (E.apply_epoch_footer_with_reward ~reward ~backend ~env ~plan);
    run (C.Ledger.flush_dirty_lwt ledger);
    let root = Option.get (run (S.get_batch_tree_hash store)) in
    need "paid commit root differs from proposal" (root = expected.E.post_state_root);
    let index = Commit.index_plan ~next_txid:(D.next_txid chaindata) ~confirmed_count:0
      ~confirmed_txs:[] ~epoch_id:env.epoch_id (Some head) in
    let post = Index.folded_state_root ~ledger_state_root:root ~epoch_index_root:index.epoch_index_root in
    save (path dir "after.json") (state ~root store ledger);
    save (path dir "receipts.json") (`List (List.map (fun value -> `String value) receipts));
    let credits = E.reward_credits reward plan |> unwrap in
    let reward_recipients = Consensus_epoch_apply_rewards.recipients credits in
    let proposer = C.Epochlog.{creator_addr = leader; commit_round = 0} in
    let commit = Commit.{epoch_id = env.epoch_id; pre_state_root = Head.ledger_state_root head;
      post_state_root = root; post_consensus_root = post; prev_state_root = head.state_root;
      parent_commit = Option.get head.irmin_commit; start_txid = index.start_txid; tx_count = 0;
      finalized_by = "paid-test"; finalized_at = env.epoch_ts; proposer;
      confirmed_fees = !fees; plan; reward_recipients;
      reward_source = Consensus_reward_attribution.to_source reward |> unwrap;
      epoch_receipts_json = receipts; commit_id = "paid-commit"; prev_generation = head.generation;
      planned_txid_hi = index.planned_txid_hi; epoch_index_hash = index.epoch_index_hash;
      epoch_index_root = index.epoch_index_root; progress = Commit.commit_progress ()} in
    let deps = Commit.{data_dir = dir; store; ledger; chaindata; trace = ignore;
      fatal = failwith; log = ignore; exit = (fun () -> failwith "paid commit exit")} in
    let effects = Commit.live_commit_effects deps in
    let effects = {effects with Commit.chaos = (fun at ->
      if at = phase then begin
        Printf.printf "event = paid_commit kill = %s\n%!" at;
        Unix.kill (Unix.getpid ()) Sys.sigkill
      end)} in
    run (Commit.run_commit_effects effects commit);
    need "commit kill phase absent" (phase = "none"))

let recover dir accepted =
  with_stores dir (fun store chaindata ledger ->
    let recover () =
      let result = run (C.Startup_recovery.recover ~data_dir:dir ~chaindata ~store) in
      need "paid recovery checks failed" (result.boundary_ok && result.eic_ok && result.eic_errors = []);
      let head = Option.get (Head.load dir) in
      need "paid recovery execution profile differs"
        (run (Consensus_profile.check_execution_parent ~chain_id:chain
          ~epoch:(head.epoch_id + 1) ~store ~head:(Some head) Sys.getenv_opt) = Ok ());
      need "paid recovery chose wrong epoch" (head.epoch_id = if accepted then epoch + 1 else epoch);
      need "paid recovery index advanced" (head.txid_hi = 0L && D.next_txid chaindata = 1L);
      need "paid recovery state differs" (state store ledger = read (path dir (if accepted then "after.json" else "before.json")));
      C.Tx_staging.clear ();
      need "paid recovery nonce replay differs"
        (Result.is_error (ingress ~at:(epoch + 1) ledger (refused_tx dir)) = accepted);
      C.Tx_staging.clear ();
      let hash = T.hash (refused_tx dir) in
      need "paid recovery rejected record differs" (Option.is_some (D.get_rejected_tx chaindata hash) = accepted);
      need "paid recovery program receipt differs" (Option.is_some (D.get_contract_receipt_raw chaindata ~tx_hash:hash) = accepted);
      if accepted then begin
        let receipts = match C.Preverify_receipt_store.read dir ~epoch_id:(epoch + 1) with
          | Some receipts -> receipts | None -> failwith "paid recovery outcome receipt missing" in
        need "paid recovery outcome receipt differs"
          (`List (List.map (fun value -> `String value) receipts) = read (path dir "receipts.json"));
        let header = Option.get (C.Epochlog.get chaindata.epochlog (epoch + 1)) in
        need "paid recovery header differs" (header.tx_count = 0 && header.fees_total = "1000")
      end;
      need "paid recovery left WAL" (C.Wal.read_pending dir = []);
      need "paid recovery left marker" (C.Epoch_commit_marker.read_marker dir = None);
      head in
    let first = recover () in
    need "paid recovery repeat changed HEAD" (recover () = first);
    save (path dir "result.json") (`Assoc ["status", `String "passed"; "accepted", `Bool accepted;
      "repeat", `Bool true; "deployment_ready", `Bool false]))

let cycle () =
  let binary = Unix.realpath Sys.executable_name in
  List.iter (fun (phase, accepted) ->
    Test_workspace.with_dir ("paid-commit-" ^ phase) (fun dir ->
      let invoke expected args =
      let status = run (Lwt_process.exec ~timeout:90.
        (binary, Array.of_list (binary :: args))) in
      need ("paid process failed: " ^ phase) (status = expected) in
      invoke (Unix.WEXITED 0) ["prepare"; dir];
      invoke (if phase = "none" then Unix.WEXITED 0 else Unix.WSIGNALED Sys.sigkill)
        ["apply"; dir; phase];
      invoke (Unix.WEXITED 0) ["recover"; dir; string_of_bool accepted]))
    ["none", true; "after_prepare", false; "after_chaindata_committed", false;
     "after_irmin_committed", true; "after_head_write", true]

let () =
  Unix.putenv "OCTRA_EMISSION_PROFILE" "";
  Unix.putenv "OCTRA_EMISSION_ACTIVATION_EPOCH" "";
  if Array.length Sys.argv = 1 then cycle () else begin
  need "commit mode and output required" (Array.length Sys.argv >= 3);
  match Sys.argv.(1) with
  | "prepare" -> prepare Sys.argv.(2)
  | "apply" when Array.length Sys.argv = 4 -> apply Sys.argv.(2) Sys.argv.(3)
  | "recover" when Array.length Sys.argv = 4 -> recover Sys.argv.(2) (bool_of_string Sys.argv.(3))
  | _ -> failwith "commit arguments invalid"
  end;
  print_endline "event = paid_commit status = passed"