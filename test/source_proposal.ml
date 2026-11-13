(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Source_case

module P = Consensus_proposal
module Preview = Consensus_proposal_preview_shell
module CT = Octra_consensus.C_types
module H = Octra_consensus.C_hash

let raw root =
  let root = C.Epoch_index_commitment.root_hex root in
  String.init 32 (fun index -> Char.chr (int_of_string ("0x" ^ String.sub root (index * 2) 2)))

let parent height root timestamp =
  let keys = List.init 4 (fun index -> identity (210 + index)) in
  let members = List.map (fun (_, public, address) ->
    CT.{address; pubkey = Base64.decode_exn public}) keys in
  let validator_set = CT.make_weighted_validator_set (List.map (fun member -> member, Z.one) members) |> unwrap in
  let header = CT.{
    proto_version = proto_version_current; chain_id = chain; epoch_id = Int64.of_int height;
    prev_state_root = String.make 32 'a'; tx_list_hash = P.tx_list_hash [];
    receipt_root = H.receipt_root []; proposed_state_root = raw root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (List.hd members).address; txid_hi = 0L; ts = timestamp -. 10.;
  } in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (secret, _, address) ->
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn secret) |> unwrap in
    let vote = CT.{chain_id = chain; epoch_id = header.epoch_id; round = 0;
      vote_type = Precommit; proposal_id; validator = address; signature = ""} in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes vote)}) keys in
  CT.{validator_set; certificate = {chain_id = chain; epoch_id = header.epoch_id;
    commit_round = 0; header; proposal_id; precommits}}

let initialize ~store ~ledger ~env =
  let epoch = env.E.epoch_id in
  run (S.set_meta store "last_epoch" (string_of_int (epoch - 1)));
  run (S.set_meta store "current_epoch" (string_of_int epoch));
  Option.iter (fun profile ->
    let marker = E.execution_marker_to_string E.{epoch = epoch - 1; profile} |> unwrap in
    run (S.set_meta store E.execution_meta_key marker))
    (Consensus_profile.execution_profile ~chain_id:chain ~epoch:(epoch - 1) Sys.getenv_opt);
  let root = run (C.Ledger.hash ledger) in
  let parent = parent (epoch - 2) root (env.E.epoch_ts -. 10.) in
  let at = Int64.of_int (epoch - 1) in
  let state, reason, changed = C.Set_fold.advance ~marks:true C.Set_fold.participating
    ~chain_id:chain ~start:at ~at ~parent:(Some parent) C.Set_fold.empty |> unwrap in
  need "test duty initialization refused" (changed && reason = None);
  run (S.set_meta store C.Set_fold.meta_key (C.Set_fold.to_string state))

let verify_history ~txs ~receipts (proposal : Octra_consensus.C_driver.proposal_plan) =
  let header = proposal.header in
  let epoch = Int64.to_int header.epoch_id in
  let parent = Option.get proposal.parent_commit in
  let keys = identity 182 :: List.init 4 (fun index -> identity (210 + index)) in
  let validator_set = CT.make_weighted_validator_set (List.map (fun (_, public, address) ->
    CT.{address; pubkey = Base64.decode_exn public}, Z.one) keys) |> unwrap in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun (secret, _, validator) ->
    let vote = CT.{chain_id = chain; epoch_id = header.epoch_id; round = 0;
      vote_type = Precommit; proposal_id; validator; signature = ""} in
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn secret) |> unwrap in
    {vote with signature = Mirage_crypto_ec.Ed25519.sign ~key (H.vote_sign_bytes vote)}) keys in
  let finalize = CT.{chain_id = chain; epoch_id = header.epoch_id; commit_round = 0;
    header; proposal_id; precommits; parent_commit = proposal.parent_commit} in
  let finality = Octra_consensus.C_codec.{finalize; validator_set} in
  let reward = Consensus_reward_attribution.of_parent_commit parent |> unwrap
    |> Consensus_reward_attribution.to_source |> unwrap in
  let body = C.Epochlog.{empty_epoch_header with id = epoch;
    state_root = C.Epoch_index_commitment.root_hex header.proposed_state_root;
    prev_state_root = C.Epoch_index_commitment.root_hex header.prev_state_root;
    start_txid = 1L; tx_count = List.length txs;
    finalized_at = header.ts; proposer = {creator_addr = header.creator_addr; commit_round = 0};
    reward_source = Some reward} in
  let encoded = List.map (fun tx -> T.hash tx, T.to_yojson tx |> Yojson.Safe.to_string) txs in
  let read = Consensus_catchup_read.{chain_id = chain;
    get_epoch_json = (fun at -> if at = epoch then Some (C.Epochlog.epoch_to_json body) else None);
    epoch_time = (fun at -> if at = epoch then Some header.ts else None);
    get_tx_by_txid = (fun id -> if id > 0L && id <= Int64.of_int (List.length txs)
      then List.nth_opt encoded (Int64.to_int id - 1) else None);
    read_receipts = (fun at -> need "history receipt epoch differs" (at = epoch); receipts);
    root_to_raw32 = raw; reward_source = (fun _ _ -> Ok reward);
    read_finality = (fun at -> if at = epoch then Some finality else None)} in
  let record = match Consensus_catchup_read.range read ~from_epoch:header.epoch_id ~max_epochs:1 with
    | `Ok ([record], _) -> record
    | _ -> failwith "paid history serving refused" in
  let expected_validator_set_hash = Octra_consensus.C_config.validator_set_hash validator_set in
  begin match Consensus_catchup_shell.validate_record ~chain_id:chain ~expected_validator_set_hash
    ~prev_eic:C.Epoch_index_commitment.genesis_root ~start_txid:1L ~head_before_record:(epoch - 1) record with
  | Ok _ -> ()
  | Error reason -> failwith ("paid catchup refused: " ^ reason)
  end;
  let hex = C.Epoch_index_commitment.root_hex in
  let joined = Consensus_join_rpc.{epoch_id = record.epoch_id; prev_state_root = hex record.prev_state_root;
    state_root = hex record.state_root; tx_list_hash = hex record.tx_list_hash; tx_hashes = record.tx_hashes;
    txs_json = record.txs_json; receipts_json = record.receipts_json; receipt_root = hex record.receipt_root;
    epoch_ts = record.epoch_ts; creator_addr = record.creator_addr; commit_round = record.commit_round;
    reward_source = reward; finality} in
  let cursor = Consensus_join_rpc.{epoch = record.epoch_id; prev_root = joined.prev_state_root;
    eic = C.Epoch_index_commitment.genesis_root; txid = 1L} in
  let prepared = Consensus_join_rpc.prepare_record ~chain_id:chain ~expected_validator_set_hash ~cursor joined in
  need "paid join changed transactions" (prepared.txs = txs);
  need "paid join changed receipts" (prepared.record.receipts_json = receipts);
  begin match Octra_bootstrap.State_sync.check_range_record ~chain_id:chain
    ~reward_source:(fun _ _ -> Ok reward) ~epoch_id:epoch ~epoch:body
    ~finality ~rows:encoded ~receipts with
  | Ok (served, _) -> need "paid range changed record" (served = record)
  | Error reason -> failwith ("paid range refused: " ^ reason)
  end

let verify_wire ?(accepted = true) ~runtime ~ledger ~root ~env ~txs ~receipts
    (proposal : Octra_consensus.C_driver.proposal_plan) =
  let input = C.Tx_outcome.split_admit receipts |> unwrap in
  let encoded = List.map T.hash txs, List.map (fun tx -> T.to_yojson tx |> Yojson.Safe.to_string) txs, receipts in
  let cached = match Consensus_bundle_cache.decode encoded with
    | Ok value -> value
    | Error reason -> failwith ("cached proposal refused: " ^ reason) in
  let bundle = Consensus_bundle_fetch.{txs = cached.txs;
    receipts_json = cached.receipts_json; rejections = cached.rejections} in
  let response = Octra_consensus.C_driver.{responder_addr = env.E.proposer_addr;
    tx_hashes = List.map (fun tx -> raw (T.hash tx)) txs;
    txs_json = List.map (fun tx -> T.to_yojson tx |> Yojson.Safe.to_string) txs;
    receipts_json = receipts} in
  need "finalized bundle refused" (Result.is_ok
    (Consensus_bundle_validation.finalized ~header:proposal.header response));
  need "history receipts refused" (C.Preverify_receipt_policy.check
    ~epoch_id:env.E.epoch_id ~receipts:input.preverify ~rejections:input.rejections txs = Ok ());
  if accepted && List.exists (fun (item : C.Tx_outcome.rejection) -> item.charged
      && C.Preverify_commit.heavy item.tx) input.rejections then begin
    need "paid history accepted without proof receipt" (Result.is_error
      (C.Preverify_receipt_policy.check ~epoch_id:env.epoch_id
        ~receipts:[] ~rejections:input.rejections txs));
    verify_history ~txs ~receipts proposal
  end;
  let parent = Option.get proposal.parent_commit in
  let prev = ref 0 and state = ref 0 and selected = ref None in
  let deps : P.verify_proposal_deps = {
    current = (fun () -> true); now = (fun () -> env.E.epoch_ts);
    previous_epoch_ts = (fun _ -> Some (env.epoch_ts -. 10.));
    quarantine_active = (fun () -> false); quarantine_reason = (fun () -> "");
    mark_quarantine = (fun reason -> failwith reason);
    prev_root_streak = (fun () -> !prev); set_prev_root_streak = (fun value -> prev := value);
    state_root_streak = (fun () -> !state); set_state_root_streak = (fun value -> state := value);
    limits; layera_diag_live = (fun () -> false);
    layera_env_diag_context = (fun () -> failwith "unexpected environment diagnostic");
    layera_diag_context = (fun ~validator_addrs:_ -> failwith "unexpected state diagnostic");
    wait_prev_root = {read_root = (fun () -> Lwt.return (raw root));
      replay_stashed = (fun () -> Lwt.return_unit); sleep = Lwt_unix.sleep};
    max_prev_root_wait_tries = 0; prev_root_wait_delay_seconds = 0.01;
    quarantine_mismatch_threshold = 4; staging_txs = (fun () -> []);
    cached_bundle = (fun ~proposal_id ->
      if proposal_id = H.proposal_id proposal.header then Some bundle else None);
    validate_preverify_once = (fun ~state_root:_ ~tx_hashes:hashes txs ->
      need "wire preverify identities differ" (hashes = List.map T.hash txs);
      C.Preverify_worker.run_many ~registered:true ~keyed:true
        ~field_policy:C.Private_ledger.Unique_fields ~strict:true ~ledger txs);
    driver_available = (fun () -> true);
    validate_bundle = (fun ~header ~expected_hashes response ->
      Consensus_bundle_validation.proposal ~header ~expected_hashes response |> Result.to_option);
    query_bundle = (fun ~epoch_id ~proposal_id ~validate ->
      need "bundle query differs" (epoch_id = proposal.header.epoch_id && proposal_id = H.proposal_id proposal.header);
      need "received bundle refused" (validate response);
      Lwt.return_some response);
    store_bundle = (fun ~proposal_id:_ ~tx_hashes ~txs:saved ~receipts_json ->
      need "wire bundle changed" (saved = txs && tx_hashes = proposal.tx_hashes && receipts_json = receipts));
    public_key_for_tx = Tx_sender_key.resolve ~find_account:(C.Ledger.find_opt ledger);
    verify_address_pubkey = (fun ~addr ~pubkey -> C.Crypto.Address.verify_address_pubkey addr pubkey);
    verify_tx_signature = (fun ~signing tx ~pubkey -> C.Tx_signature.verify signing tx pubkey);
    validator_pubkeys = (fun _ -> List.map (fun member ->
      member.CT.address, Base64.encode_exn member.pubkey) parent.validator_set.validators);
    read_local_ledger_root = (fun () -> Lwt.return root); preview = Preview.run runtime;
    prev_eic_root = (fun () -> C.Epoch_index_commitment.genesis_root);
    next_txid = (fun () -> 1L); head_txid_hi = (fun () -> Some 0L); root_to_raw32 = raw;
    set_proposal = (fun txs hashes -> selected := Some (txs, hashes));
    share_txs = (fun _ -> ());
    verify_parent_commit = (fun ~epoch_id -> function
      | Some value when value.certificate.epoch_id = Int64.pred epoch_id ->
        C.Set_fold.read_parent ~chain_id:chain value |> Result.map (fun _ -> ())
      | _ -> Error "parent epoch differs");
  } in
  let wire = CT.{chain_id = chain; epoch_id = proposal.header.epoch_id; round = 1;
    header = proposal.header; tx_hashes = proposal.tx_hashes; parent_commit = proposal.parent_commit;
    valid_round = None; proposer = env.proposer_addr; signature = ""} in
  List.iter (fun deps ->
    selected := None;
    let verdict = run (P.verify_proposal ~prepare:(Preview.prepare runtime) deps ~chain_id:chain wire) in
    need "serialized proposal verdict differs" (verdict =
      if accepted then Octra_consensus.C_driver.Proposal_accept else Proposal_reject);
    need "validator selection differs" (!selected =
      if accepted then Some (txs, proposal.tx_hashes) else None))
    [deps; {deps with cached_bundle = (fun ~proposal_id:_ -> None)}]

let check_many ?(program_trust = V.Program_trust.empty) ?(refused = []) ?(paid = [])
    ?(deferred = []) ?(progress = true)
    ~store ~ledger ~env ~expected txs =
  let epoch = env.E.epoch_id in
  let root = run (C.Ledger.hash ledger) in
  let commit = run (S.get_commit_hash store) in
  let parent = parent (epoch - 1) root env.E.epoch_ts in
  let unchanged () =
    need "proposal changed live ledger" (run (C.Ledger.hash ledger) = root);
    need "proposal changed live commit" (run (S.get_commit_hash store) = commit);
    need "proposal removed staged source" (C.Tx_staging.staging_size () = List.length txs) in
  let backend = Preview.node_backend ~program_trust ~rules:(rules ())
    ~legacy_replay:(fun ~epoch:_ ~address:_ ~cipher:_ -> failwith "unexpected private replay")
    ~private_result_policy:(fun _ -> C.Private_result_policy.Recoverable)
    ~max_fhe:8 ~max_stealth:8 store ledger in
  let runtime = Preview.{chain_id = chain; program_trust; backend;
    ready_state_root_at = (fun _ -> failwith "unexpected history read"); ready_max_lag = 0;
    warn = (fun text -> Printf.eprintf "event = source_preview error = %s\n%!" text)} in
  let captures = ref [] in
  let evicted = ref [] in
  let held = ref [] in
  let prepare request =
    let open Lwt.Syntax in
    let* result = Preview.prepare runtime request in
    captures := (request, result) :: !captures;
    Lwt.return result in
  let saved = ref None in
  let deps : P.make_proposal_deps = {
    current = (fun () -> true); start_height = (fun _ -> Lwt.return_unit);
    current_epoch = (fun () -> epoch); state_attested = (fun () -> true);
    quarantine_active = (fun () -> false); quarantine_reason = (fun () -> "");
    read_prev_ledger_root = (fun () -> Lwt.return_some root); cached_head = (fun () -> None);
    current_round = (fun () -> 0); parent_commit = (fun ~epoch_id:_ -> Ok (Some parent));
    parent_txs = (fun _ -> None); frozen_bundle = (fun _ -> None);
    store_bundle = (fun ~proposal_id:_ ~tx_hashes:_ ~txs ~receipts_json -> saved := Some (txs, receipts_json));
    staging_txs = (fun ?circles:_ () -> txs);
    evict_preview = (fun ?epoch:_ tx ->
      need "proposal evicted valid source" (List.mem_assoc tx refused);
      evicted := tx :: !evicted);
    hold_preview = (fun ~epoch:_ tx ->
      need "proposal deferred source" (List.mem tx deferred);
      held := tx :: !held);
    admits_tx = (fun _ -> true);
    build_preverify_once = (fun ~state_root:_ ~tx_hashes txs ->
      need "proposal preverify identities differ" (tx_hashes = List.map T.hash txs);
      C.Preverify_worker.run_many ~registered:true ~keyed:true
        ~field_policy:C.Private_ledger.Unique_fields ~strict:true ~ledger txs);
    staging_total = (fun () -> C.Tx_staging.staging_size ()); proposer = (fun () -> env.proposer_addr);
    validator_pubkeys = (fun _ -> List.map (fun member ->
      member.CT.address, Base64.encode_exn member.pubkey) parent.validator_set.validators);
    preview = Preview.run runtime;
    prev_eic_root = (fun () -> C.Epoch_index_commitment.genesis_root);
    next_txid = (fun () -> 1L); set_proposal = (fun _ _ -> ());
    head_txid_hi = (fun () -> Some 0L); freeze = (fun _ _ -> ());
    now = (fun () -> env.epoch_ts); previous_epoch_ts = (fun _ -> Some (env.epoch_ts -. 10.));
  } in
  let clock () = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9 in
  let started = clock () in
  let active = ref true and ticks = ref 0 and longest = ref 0. in
  let previous = ref started and finished = ref started in
  let rec tick () =
    let open Lwt.Syntax in
    let* () = Lwt_unix.sleep 0.001 in
    if !active then begin
      let now = clock () in
      longest := max !longest (now -. !previous);
      previous := now;
      incr ticks;
      tick ()
    end else Lwt.return_unit in
  let ticker = tick () in
  let execution = Lwt.finalize (fun () -> P.make_proposal ~prepare deps ~chain_id:chain ~root_to_raw32:raw
    ~limits ~epoch_id:(Int64.of_int epoch)) (fun () ->
      finished := clock ();
      longest := max !longest (!finished -. !previous);
      active := false;
      Lwt.return_unit) in
  let proposal, () = run (Lwt.both execution ticker) in
  let seconds = !finished -. started in
  need "source proposal absent" (Option.is_some proposal);
  need "source preparation prevented scheduler progress" (not progress || expected = [] || !ticks > 0);
  let proposal = Option.get proposal in
  need "source excluded from proposal" (proposal.tx_hashes = List.map T.hash expected);
  let txs, receipts = Option.get !saved in
  need "source bundle changed transaction" (txs = expected);
  let verified = P.verify_bundle {
    public_key_for_tx = Tx_sender_key.resolve ~find_account:(C.Ledger.find_opt ledger);
    verify_address_pubkey = (fun ~addr ~pubkey -> C.Crypto.Address.verify_address_pubkey addr pubkey);
    verify_tx_signature = (fun ~signing tx ~pubkey -> C.Tx_signature.verify signing tx pubkey);
  } ~limits ~header:proposal.header ~expected_tx_count:(List.length txs) txs receipts in
  begin match verified with
  | Error (P.Preverify_gate_failed reason) -> failwith ("source preverify refused: " ^ reason)
  | Error _ -> failwith "source bundle refused"
  | Ok _ -> ()
  end;
  let verified = unwrap verified in
  need "proposal eviction list differs" (List.rev !evicted = List.map fst refused);
  need "proposal hold list differs" (List.sort compare !held = List.sort compare deferred);
  need "proposal refusal records differ" (List.map (fun (item : C.Tx_outcome.rejection) ->
    item.tx, item.reason) verified.rejections = refused @ paid);
  need "proposal refusal charge differs" (List.for_all (fun (item : C.Tx_outcome.rejection) ->
    item.charged = List.mem_assoc item.tx paid) verified.rejections);
  let module Apply = Consensus_epoch_apply_source in
  let proposal_id = Octra_consensus.C_hash.proposal_id proposal.header in
  let final = Octra_consensus.C_types.{chain_id = chain; epoch_id = Int64.of_int epoch;
    commit_round = 0; header = proposal.header; proposal_id;
    precommits = []; parent_commit = proposal.parent_commit} in
  let input = C.Tx_outcome.split_admit receipts |> unwrap in
  let wire_preverify = C.Preverify_commit.receipts_of_strings input.preverify |> unwrap
    |> C.Preverify_commit.create in
  let executed = C.Tx_outcome.executed ~confirmed:txs verified.rejections in
  begin match run (C.Preverify_commit.check_bound ledger wire_preverify executed) with
  | Ok () -> ()
  | Error reason -> failwith ("serialized preverify refused: " ^ reason)
  end;
  let select = Apply.{rules = rules ();
    find_finalized = (fun at -> if at = epoch then Some final else None);
    cached_bundle = (fun id -> if id = proposal_id then
      Some (List.map T.hash txs, txs, receipts) else None);
    receipt_root_matches = (fun header saved -> header = proposal.header && saved = receipts);
    header_has_empty_bundle = (fun _ -> false);
    staging_txs = (fun () -> failwith "unexpected apply staging read")} in
  List.iter (fun override_ordered_txs ->
    let selected = Apply.choose select {epoch_id = epoch; override_ordered_txs;
      override_receipts_json = Some receipts; consensus_mode = true} in
    need "receipt refused at apply selection" (Result.is_ok selected);
    let selected = unwrap selected in
    need "apply selection changed receipt" (selected.txs = txs && selected.rejections = verified.rejections))
    [None; Some txs];
  if List.exists (fun (item : C.Tx_outcome.rejection) -> item.charged
      && C.Preverify_commit.heavy item.tx) input.rejections then begin
    let receipts = C.Tx_outcome.encode [] input.rejections in
    need "paid override accepted without proof receipt" (match Apply.choose select
      {epoch_id = epoch; override_ordered_txs = Some txs;
       override_receipts_json = Some receipts; consensus_mode = true} with
      | Error (Apply.Override_preverify_failed _) -> true
      | _ -> false);
    need "paid range accepted without proof receipt" (Result.is_error
      (Octra_bootstrap.State_sync.check_outcome ~epoch_id:epoch receipts txs))
  end;
  unchanged ();
  let request, prepared = match !captures with
    | (request, Ok result) :: attempts ->
      need "valid preparation repeated" (refused <> [] || attempts = []);
      List.iter (function
        | attempt, Ok (checked : P.prepared) ->
          need "refusal execution differs" (List.map (fun (item : E.tx_reject) ->
            item.tx, item.reason) checked.execution.artifacts.rejected = refused);
          need "refusal unexpectedly charged" (List.for_all (fun (item : E.tx_reject) ->
            not item.charged) checked.execution.artifacts.rejected);
          let replayed = run (Preview.run runtime {attempt with preverify = checked.preverify}) |> unwrap in
          need "refusal validator replay differs" (replayed = checked.execution);
          let compare rejections = P.verify_preview_partition ~inputs:attempt.txs ~confirmed:[]
            ~rejections (Ok replayed) in
          need "serialized refusal differs from execution" (compare verified.rejections = Ok ());
          List.iter (fun (item : C.Tx_outcome.rejection) ->
            List.iter (fun changed -> need "changed refusal record accepted"
              (compare [changed] = Error "preview_rejection_mismatch"))
              [{item with error_type = "changed"}; {item with reason = "changed"};
               {item with charged = not item.charged}; {item with position = item.position + 1}])
            verified.rejections
        | _ -> failwith "refusal preparation failed") attempts;
      request, result
    | _ -> failwith "source preparation repeated or refused" in
  need "stable execution rejection differs" (List.map (fun (item : E.tx_reject) ->
    item.tx, item.reason, item.charged) prepared.execution.artifacts.rejected =
    List.map (fun (tx, reason) -> tx, reason, true) paid);
  need "source preparation did not confirm" (List.map fst prepared.execution.artifacts.confirmed =
    List.filter (fun tx -> not (List.mem_assoc tx paid)) txs);
  let request = {request with P.txs = List.filter (fun tx -> not (List.mem tx deferred)) request.txs} in
  let replayed = match run (Preview.run runtime {request with preverify = wire_preverify}) with
    | Ok result -> result
    | Error reason -> failwith ("source validator replay refused: " ^ reason) in
  need "source validator replay differs" (replayed = prepared.execution);
  if paid <> [] then begin
    let compare rejections = P.verify_preview_partition ~inputs:request.txs
      ~confirmed:(List.map fst replayed.artifacts.confirmed) ~rejections (Ok replayed) in
    need "paid receipt differs from replay" (compare verified.rejections = Ok ());
    List.iter (fun (item : C.Tx_outcome.rejection) ->
      List.iter (fun changed -> need "altered paid receipt accepted"
        (Result.is_error (compare [changed])))
        [{item with error_type = "changed"}; {item with reason = "changed"};
         {item with charged = false}; {item with position = item.position + 1};
         {item with tx = {item.tx with ou = Z.succ item.tx.ou}}]) verified.rejections
  end;
  List.iter (fun (tx, _) -> if tx.T.op_type = T.CircleCall then begin
    let receipt = List.find (fun item -> item.C.Preverify_receipt.tx_hash = T.hash tx)
      wire_preverify.receipts in
    let circle = Option.get receipt.circle in
    List.iter (fun circle ->
      let changed = match C.Preverify_worker.circle_receipt tx circle with
        | Ready value -> value
        | _ -> failwith "altered circle receipt invalid" in
      let proof_receipts = List.map (fun item -> if item.C.Preverify_receipt.tx_hash = T.hash tx
        then changed else item) wire_preverify.receipts in
      let preverify = {wire_preverify with receipts = proof_receipts} in
      need "altered circle binding accepted" (Result.is_error
        (run (Preview.run runtime {request with preverify})));
      let receipts = C.Tx_outcome.encode
        (List.map (fun receipt -> C.Preverify_receipt.to_yojson receipt |> Yojson.Safe.to_string)
          proof_receipts) verified.rejections in
      let proposal = {proposal with header = {proposal.header with receipt_root = H.receipt_root receipts}} in
      verify_wire ~accepted:false ~runtime ~ledger ~root ~env ~txs ~receipts proposal)
      [{circle with circle_id = tx.from}; {circle with code_hash = String.make 64 'b'};
       {circle with stable_root = String.make 64 'c'};
       {circle with public_reads_hash = String.make 64 'd'};
       {circle with context_hash = String.make 64 'e'}]
  end) paid;
  let decision = P.preview_decision ~root_to_raw32:raw ~epoch_id:(Int64.of_int epoch)
    ~tx_hashes:proposal.tx_hashes ~tx_count:replayed.artifacts.tx_count ~start_txid:1L
    ~prev_eic_root:C.Epoch_index_commitment.genesis_root ~local_ledger_root:root
    ~proposed_state_root:proposal.header.proposed_state_root
    ~preview:(P.preview_status_of_result (Ok replayed)) in
  need "source proposal root differs from replay" (match decision with P.Preview_accept _ -> true | _ -> false);
  verify_wire ~runtime ~ledger ~root ~env ~txs ~receipts proposal;
  unchanged ();
  Printf.printf "event = source_proposal status = passed seconds = %.6f transactions = %d ticks = %d gap_ms = %.3f\n%!"
    seconds (List.length txs) !ticks (!longest *. 1000.);
  {request with preverify = wire_preverify}, replayed, seconds

let check ~store ~ledger ~env tx =
  ignore (check_many ~store ~ledger ~env ~expected:[tx] [tx])

let apply ?(program_trust = V.Program_trust.empty)
    ?(save_receipt_raw = fun ~tx_hash:_ ~json:_ -> ()) ~store ~ledger request =
  let wiring = Consensus_driver_wiring.{chain_id = chain;
    ready_state_root_at = (fun _ -> failwith "unexpected history read"); ready_max_lag = 0;
    warn = (fun text -> failwith text);
    run_preview = (fun request ~reward ~env ->
      let fold = Set_rule.bind (rules ()) ~chain_id:chain ~parent:request.P.parent_commit
        ~epoch:env.E.epoch_id |> unwrap in
      let modes = fold env.epoch_id |> unwrap in
      let backend = E.make_live_backend ~fold ~proof_mode:modes.standard_mode ~math:modes.math
        ~execution_profile:(fun ~chain_id ~epoch ->
          Consensus_profile.execution_profile ~chain_id ~epoch Sys.getenv_opt) store ledger in
      E.run_core ~reward:(Some reward) ~preverify:(Some request.preverify) ~backend ~env ~txs:request.txs
        ~process_tx:(fun ~backend ~env tx ->
          Consensus_vm_transition.process_tx ~preverify:request.preverify ~save_receipt_raw ~backend ~env
            ~circle_mode:(R.circle (rules ()) ~epoch:env.epoch_id |> unwrap)
            ~wasm_compute_mode:(R.wasm_compute (rules ()) ~epoch:env.epoch_id |> unwrap)
            ~program_trust
            ~object_cost:(R.object_cost (rules ()) ~epoch:env.epoch_id = Ok R.Active) tx))} in
  run (Consensus_driver_wiring.node_proposal_preview wiring request) |> unwrap