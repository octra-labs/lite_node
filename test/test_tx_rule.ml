(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Transaction
module Outcome = Octra_core.Tx_outcome
module Bundle = Octra_node_runtime.Consensus_bundle_validation
module Proposal = Octra_node_runtime.Consensus_proposal
module Types = Octra_consensus.C_types

let expect label value = if not value then failwith label
let envelope_epoch =
  (Option.get (Octra_core.Rule_graph.tx_envelope_activation_for_chain
    "octra-devnet-9871-cluster")).activation_epoch
let envelope_height = Int64.of_int envelope_epoch
let epochs = [envelope_epoch - 1; envelope_epoch; envelope_epoch + 1]
let heights = List.map Int64.of_int epochs

let signed () =
  let secret, public = Mirage_crypto_ec.Ed25519.generate () in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets secret |> Base64.encode_exn in
  let public = Mirage_crypto_ec.Ed25519.pub_to_octets public |> Base64.encode_exn in
  let from = Octra_core.Crypto.Address.address_from_pubkey public in
  let tx = Tx.{from; to_ = "oct5TWVJk7LZmzEeU73KAwd8HRuQjt2sdBiagm3rxcWDzYH";
    amount = Z.one; nonce = 1; ou = Z.of_int 10_000; timestamp = 1.;
    signature = ""; public_key = Some public; message = None;
    op_type = Standard; encrypted_data = None} in
  Tx.sign_with_privkey tx secret, secret

let alias tx =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let bytes = Bytes.of_string tx.Tx.signature in
  let code = String.index alphabet tx.signature.[85] in
  Bytes.set bytes 85 alphabet.[code + 1];
  {tx with signature = Bytes.to_string bytes}

let header epoch txs receipts = Types.{
  proto_version = proto_version_current; chain_id = "octra-devnet-9871-cluster";
  epoch_id = epoch; prev_state_root = String.make 32 'a';
  tx_list_hash = Octra_consensus.C_engine.tx_list_hash_for_header (List.map Tx.hash txs);
  receipt_root = Octra_consensus.C_hash.receipt_root receipts;
  proposed_state_root = String.make 32 'b'; parent_commit_hash = Octra_net.Hash_domain.nil_hash;
  creator_addr = "member0"; txid_hi = 0L; ts = 1.;
}

let certificate header =
  let module Hash = Octra_consensus.C_hash in
  let keys = List.init 4 (fun i ->
    let key, public = Mirage_crypto_ec.Ed25519.generate () in
    Types.{address = "member" ^ string_of_int i;
      pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets public}, key) in
  let validator_set = Types.make_validator_set (List.map fst keys) in
  let proposal_id = Hash.proposal_id header in
  let precommits = List.map (fun (validator, key) ->
    let vote = Types.{chain_id = header.chain_id; epoch_id = header.epoch_id;
      round = 0; vote_type = Precommit; proposal_id; validator = validator.address;
      signature = ""} in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (Hash.vote_sign_bytes vote)}) keys in
  validator_set, Types.{chain_id = header.chain_id; epoch_id = header.epoch_id;
    commit_round = 0; header; proposal_id; precommits; parent_commit = None}

let check_route route ~epoch ~trusted txs receipts =
  let txs = Tx.consensus_order txs in
  let header = header epoch txs receipts in
  let hashes = List.map Tx.hash txs in
  let response = Octra_consensus.C_driver.{responder_addr = "oct_peer";
    tx_hashes = hashes; txs_json = List.map (fun tx -> Tx.to_yojson tx |> Yojson.Safe.to_string) txs;
    receipts_json = receipts} in
  match route with
  | 0 -> Result.is_ok (Bundle.proposal ~header ~expected_hashes:hashes response)
  | 1 -> Result.is_ok (Bundle.finalized ~header response)
  | 4 ->
    let module Replay = Octra_node_runtime.Consensus_replay in
    (try ignore (Replay.build_plan ~parent_commit:None ~header ~commit_round:0 ~receipts_json:receipts ~txs); true
      with Failure _ -> false)
  | 5 | 6 ->
    let module Journal = Octra_node_runtime.Consensus_finality_journal in
    let validator_set, finalize = certificate header in
    Test_workspace.with_dir "tx_journal" (fun base ->
      let bundle = Journal.{tx_hashes = hashes; txs; receipts_json = receipts} in
      let file = Filename.concat base "finality/pending_finalized.json" in
      let saved () = if Sys.file_exists file then Some (Yojson.Safe.from_file file) else None in
      if route = 5 then Journal.persist_certificate base ~validator_set finalize;
      let before = saved () in
      let accepted = try
        if route = 5 then Journal.persist_bundle base finalize bundle
        else ignore (Journal.stage base ~chain_id:header.chain_id ~validator_set ~bundle finalize);
        true
      with Failure reason ->
        if epoch < envelope_height then failwith ("historical journal input: " ^ reason);
        false in
      if not accepted then expect "refused bundle does not alter journal" (saved () = before);
      accepted)
  | 7 ->
    let module Apply = Octra_node_runtime.Consensus_finalized_apply in
    let validator_set, finalize = certificate header in
    let writes = ref 0 in
    let touched () = incr writes in
    let deps = Apply.{check_finality = (fun _ -> ());
      write_finality = (fun _ -> touched ());
      persist_finality_certificate = (fun ~validator_set:_ value -> touched (); value);
      store_proposer = (fun _ -> touched ()); persist_finality_bundle = (fun _ _ -> touched ());
      chaos_after_finality_log = (fun () -> ()); cached_bundle = (fun ~proposal_id:_ -> true);
      cached_bundle_data = (fun ~proposal_id:_ -> Some (hashes, txs, receipts));
      cached_bundle_len = (fun ~proposal_id:_ -> List.length txs);
      header_has_empty_bundle = (fun _ -> false); store_empty_bundle = (fun _ -> touched ());
      query_bundle = (fun ~epoch_id:_ ~proposal_id:_ ~validate:_ -> Lwt.return_none);
      store_accepted_bundle = (fun ~proposal_id:_ _ -> touched ());
      sleep = (fun _ -> failwith "unexpected wait"); bundle_wait_timeout_seconds = 1.;
      bundle_wait_expired = (fun ~epoch_id:_ -> touched ());
      bundle_wait_recovered = (fun ~epoch_id:_ -> touched ());
      post_finalize = (fun ~epoch_id:_ ~proposed_root:_ -> touched (); Lwt.return_unit)} in
    let accepted = try Lwt_main.run (Apply.run deps ~validator_set finalize); true
      with Failure _ -> false in
    if not accepted then expect "cached invalid bundle precedes writes" (!writes = 0);
    accepted
  | 3 ->
    let module Source = Octra_node_runtime.Consensus_epoch_apply_source in
    let module Graph = Octra_core.Rule_graph in
    let plan = Graph.tx_envelope_activation_for_chain header.chain_id |> Option.get in
    let rules = Graph.create ~chain_id:header.chain_id
      ~root_at:(fun _ -> Graph.Root plan.anchor_state_root) in
    let deps = Source.{rules; check_override_receipts = (fun ~epoch_id:_ ~receipts:_ _ -> Ok ());
      find_finalized = (fun _ -> None); cached_bundle = (fun _ -> None);
      receipt_root_matches = (fun _ _ -> true); header_has_empty_bundle = (fun _ -> false);
      staging_txs = (fun () -> [])} in
    Result.is_ok (Source.choose deps Source.{epoch_id = Int64.to_int epoch;
      override_ordered_txs = Some txs; override_receipts_json = Some receipts;
      consensus_mode = true})
  | _ ->
    let deps = Proposal.{public_key_for_tx = (fun _ -> Some trusted);
      verify_address_pubkey = (fun ~addr ~pubkey -> Octra_core.Crypto.Address.verify_address_pubkey addr pubkey);
      verify_tx_signature = (fun tx ~pubkey -> Tx.verify tx pubkey)} in
    let limits = Proposal.limits ~max_txs:10 ~max_bytes:100_000 ~max_ou:(Z.of_int 1_000_000) in
    Result.is_ok (Proposal.verify_bundle deps ~limits ~header ~expected_tx_count:(List.length txs) txs receipts)

let test_route route =
  let tx, secret = signed () in
  let trusted = Option.get tx.public_key in
  let duplicate = Tx.sign_with_privkey {tx with ou = Z.of_int 20_000} secret in
  let changed = [alias tx; {tx with public_key = None}; {tx with public_key = Some "ignored"}] in
  List.iter (fun epoch ->
    expect "ordinary signed input accepted" (check_route route ~epoch ~trusted [tx] []);
    List.iter (fun input ->
      let expected = epoch < envelope_height in
      expect "envelope activation on confirmed input"
        (check_route route ~epoch ~trusted [input] [] = expected);
      let rejections = Outcome.build ~inputs:[input]
        [input, "execution_failed", "test rejection"] |> Result.get_ok in
      let receipts = Outcome.encode [] rejections in
      expect "envelope activation on rejected input"
        (check_route route ~epoch ~trusted [] receipts = expected)) changed;
    expect "sender nonce uniqueness activates"
      (check_route route ~epoch ~trusted [tx; duplicate] [] = (epoch < envelope_height));
    List.iter (fun rejected ->
      let inputs = Tx.consensus_order [tx; duplicate] in
      let rejections = Outcome.build ~inputs
        (List.map (fun item -> item, "execution_failed", "test rejection") rejected)
        |> Result.get_ok in
      let confirmed = List.filter (fun item -> not (List.mem item rejected)) inputs in
      expect "sender nonce uniqueness includes rejected input"
        (check_route route ~epoch ~trusted confirmed (Outcome.encode [] rejections)
         = (epoch < envelope_height))) [[duplicate]; [tx; duplicate]])
    heights

let test_anchor () =
  let module Graph = Octra_core.Rule_graph in
  let chain_id = "octra-devnet-9871-cluster" in
  let plan = Graph.tx_envelope_activation_for_chain chain_id |> Option.get in
  expect "owner selected activation" (plan.activation_epoch = 1_611_500);
  let missing = Graph.create ~chain_id ~root_at:(fun _ -> Graph.Missing) in
  List.iter (fun epoch ->
    expect "withdrawn activation stays prior"
      (Graph.tx_envelope missing ~epoch = Ok Graph.Prior))
    [1_582_999; 1_583_000; 1_583_001; 1_585_999; 1_586_000; 1_586_001];
  expect "history does not need new anchor"
    (Graph.tx_envelope missing ~epoch:(envelope_epoch - 1) = Ok Graph.Prior);
  List.iter (fun epoch ->
    expect "missing anchor refused" (Result.is_error (Graph.tx_envelope missing ~epoch));
    let wrong = Graph.create ~chain_id ~root_at:(fun _ -> Graph.Root (String.make 64 '0')) in
    expect "wrong anchor refused" (Result.is_error (Graph.tx_envelope wrong ~epoch));
    let exact = Graph.create ~chain_id ~root_at:(fun requested ->
      expect "exact anchor epoch" (requested = plan.anchor_epoch);
      Graph.Root plan.anchor_state_root) in
    expect "verified anchor enables rule" (Graph.tx_envelope exact ~epoch = Ok Graph.Active))
    [envelope_epoch; envelope_epoch + 1];
  expect "other chains not rescheduled"
    (Graph.tx_envelope_at ~chain_id:"octra-mainnet" ~epoch:Int64.max_int = Graph.Prior)

let test_exec_preflight () =
  let module Exec = Octra_core.Epoch_exec in
  let module Store = Octra_core.Store_irmin in
  let module Ledger = Octra_core.Ledger in
  let tx, _ = signed () in
  List.iter (fun epoch ->
    Test_workspace.with_dir "tx_rule" (fun dir ->
      let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
      Fun.protect ~finally:(fun () -> Lwt_main.run (Store.close store)) (fun () ->
        let ledger = Ledger.create store in
        ignore (Ledger.add_account ledger tx.from (Z.of_int 100) |> Result.get_ok);
        let backend = Exec.make_live_backend ~emission_policy:Octra_core.Emission_policy.Guard
          ~legacy_total_supply:"100" store ledger in
        let started = ref 0 in
        let backend = {backend with begin_batch = (fun _ ->
          incr started; Lwt.fail_with "test_execution_reached")} in
        let env = Exec.{chain_id = "octra-devnet-9871-cluster"; epoch_id = epoch;
          proposer_addr = tx.from; validator_addrs = []; validator_pubkeys = [];
          prev_state_root = ""; epoch_ts = 1.; ready_state_root_at = None; ready_max_lag = 0} in
        let root = Lwt_main.run (Store.get_head_hash store) in
        let result = Exec.run ~backend ~env ~txs:[alias tx]
          ~process_tx:(fun ~backend:_ ~env:_ _ -> failwith "unexpected execution") |> Lwt_main.run in
        expect "invalid epoch is refused" (Result.is_error result);
        expect "active refusal precedes batch writes" (!started = if epoch < envelope_epoch then 1 else 0);
        expect "store head is unchanged" (Lwt_main.run (Store.get_head_hash store) = root);
        expect "ledger is unchanged" ((Ledger.find ledger tx.from).balance = Z.of_int 100);
        expect "journal stays closed" (not (Ledger.journal_active ledger)))))
    epochs

let test_source_paths () =
  let module Source = Octra_node_runtime.Consensus_epoch_apply_source in
  let module Graph = Octra_core.Rule_graph in
  let tx, _ = signed () in
  let tx = alias tx in
  List.iter (fun epoch ->
    let header = header (Int64.of_int epoch) [tx] [] in
    let _, finalize = certificate header in
    let plan = Graph.tx_envelope_activation_for_chain header.chain_id |> Option.get in
    List.iter (fun root ->
      let rules = Graph.create ~chain_id:header.chain_id ~root_at:(fun _ -> root) in
      let deps = Source.{rules;
        check_override_receipts = (fun ~epoch_id:_ ~receipts:_ _ -> Ok ());
        find_finalized = (fun _ -> Some finalize);
        cached_bundle = (fun _ -> Some ([Tx.hash tx], [tx], []));
        receipt_root_matches = (fun _ _ -> true); header_has_empty_bundle = (fun _ -> true);
        staging_txs = (fun () -> [tx])} in
      List.iter (fun consensus_mode ->
        let request = Source.{epoch_id = epoch; override_ordered_txs = None;
          override_receipts_json = None; consensus_mode} in
        expect "cached and staged inputs honor activation"
          (Result.is_ok (Source.choose deps request) = (epoch < envelope_epoch));
        let deps = {deps with cached_bundle = (fun _ -> None); staging_txs = (fun () -> [])} in
        let effects = ref 0 in
        let accepted = try
          ignore (Source.run deps request ~apply_effect:(fun _ -> incr effects)
            ~fatal:(fun _ -> ()) ~exit:(fun () -> failwith "refused")); true
          with Failure _ -> false in
        let expected = epoch < envelope_epoch || root = Graph.Root plan.anchor_state_root in
        expect "empty epochs still prove rule anchor" (accepted = expected);
        if not accepted then expect "anchor refusal has no effects" (!effects = 0)) [true; false])
      [Graph.Missing; Graph.Unreadable "read failed"; Graph.Root (String.make 64 '0');
       Graph.Root plan.anchor_state_root]) epochs

let test_replay_receipts () =
  let module Replay = Octra_node_runtime.Consensus_replay in
  let tx, _ = signed () in
  let rejected = Outcome.build ~inputs:[tx]
    [tx, "execution_failed", "test rejection"] |> Result.get_ok in
  let receipts = Outcome.encode [] rejected in
  Test_workspace.with_dir "tx_replay" (fun base ->
    let header_path = Filename.concat base "header.json" in
    List.iter (fun epoch ->
      let h = header epoch [] receipts in
      let fields = ["chain_id", `String h.chain_id;
        "epoch_id", `Intlit (Int64.to_string epoch); "creator_addr", `String h.creator_addr;
        "prev_state_root", `String h.prev_state_root;
        "proposed_state_root", `String h.proposed_state_root;
        "tx_list_hash", `String h.tx_list_hash; "receipt_root", `String h.receipt_root;
        "txid_hi", `Int 0; "ts", `Float h.ts] in
      let load () = Replay.load_plan ~default_chain_id:h.chain_id ~header_path ~bundle_path:None in
      Yojson.Safe.to_file header_path (`Assoc
        (("receipts_json", `List (List.map (fun x -> `String x) receipts)) :: fields));
      let plan = load () in
      expect "replay keeps exact rejection receipts" (plan.receipts_json = receipts);
      let seen = ref None in
      let apply ?override_ordered_txs:_ ?override_receipts_json
          ?override_proposer_info:_ ?override_reward:_ ?override_epoch_ts
          ?override_validator_set:_ ?override_parent_commit:_ ~now:_ ~elapsed:_ () =
        seen := Some (override_receipts_json, override_epoch_ts); Lwt.return_unit in
      let callbacks = Octra_node_runtime.Consensus_startup_sync.apply_callbacks ~now:(fun () -> 0.) apply in
      Lwt_main.run (callbacks.replay plan);
      expect "replay forwards receipts and timestamp" (!seen = Some (Some receipts, Some h.ts));
      Yojson.Safe.to_file header_path (`Assoc fields);
      let accepted = try ignore (load ()); true with Failure _ -> false in
      expect "missing replay receipts refuse active commitment" (accepted = (epoch < envelope_height));
      let fields = List.map (fun (key, value) ->
        key, if key = "chain_id" then `String "octra-mainnet" else value) fields in
      Yojson.Safe.to_file header_path (`Assoc fields);
      let accepted = try ignore (load ()); true with Failure _ -> false in
      expect "chain override cannot disable new rule" (accepted = (epoch < envelope_height)))
      heights)

let () =
  Mirage_crypto_rng_unix.use_default ();
  test_anchor ();
  test_exec_preflight ();
  test_source_paths ();
  test_replay_receipts ();
  let failed = ref 0 in
  List.iter (fun route ->
    try test_route route; Printf.printf "event = test name = tx_rule route = %d status = passed\n%!" route
    with exn -> incr failed; Printf.eprintf "event = test name = tx_rule route = %d status = failed error = %s\n%!"
      route (Printexc.to_string exn)) [0; 1; 2; 3; 4; 5; 6; 7];
  if !failed <> 0 then exit 1