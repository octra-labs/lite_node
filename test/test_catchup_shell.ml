(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module S = Octra_node_runtime.Consensus_catchup_shell

let () =
  Mirage_crypto_rng_unix.use_default ()

let run = Lwt_main.run

let assert_true msg value =
  if not value then failwith msg

let add events value =
  events := value :: !events

let snapshot events =
  List.rev !events

let has value events =
  List.exists (( = ) value) events

let unproved tag =
  S.finish_unverified tag

let add_finish events finish =
  add events ("finish:" ^ finish.S.tag);
  add events ("root_verified:" ^ string_of_bool finish.root_verified)

let empty_tx_list_hash =
  Octra_consensus.C_engine.tx_list_hash_for_header []

let base_root = String.make 32 '\x10'
let next_root = String.make 32 '\x11'
let wrong_root = String.make 32 '\x12'
let other_root = String.make 32 '\x13'

let finality_private_key, finality_public_key =
  Mirage_crypto_ec.Ed25519.generate ()

let trusted_validator_set_hash =
  Octra_consensus.C_types.make_validator_set [{
    Octra_consensus.C_types.address = "oct_creator";
    pubkey =
      Mirage_crypto_ec.Ed25519.pub_to_octets finality_public_key;
  }]
  |> Octra_consensus.C_config.validator_set_hash

let record ?(epoch = 11L) ?(round = 0) ?(prev = base_root) ?(root = next_root)
    ?(creator = "oct_creator") ?reward_creator ?reward_public_key ?parent () =
  let reward_creator = Option.value ~default:creator reward_creator in
  let reward_source =
    Some Octra_consensus.C_types.{
      reward_proposer_addr = reward_creator;
      reward_proposer_public_key = reward_public_key;
      reward_members = [{
        reward_address = reward_creator;
        reward_public_key;
        reward_weight = Z.one;
      }];
    }
  in
  let receipt_root = Octra_consensus.C_hash.receipt_root [] in
  let epoch_ts = Int64.to_float epoch *. 10. in
  let validator_set =
    Octra_consensus.C_types.make_validator_set [{
      Octra_consensus.C_types.address = creator;
      pubkey =
        Mirage_crypto_ec.Ed25519.pub_to_octets finality_public_key;
    }]
  in
  let header = Octra_consensus.C_types.{
    proto_version = proto_version_current;
    chain_id = "octra-test";
    epoch_id = epoch;
    prev_state_root = prev;
    tx_list_hash = empty_tx_list_hash;
    receipt_root;
    proposed_state_root = root;
    parent_commit_hash =
      Octra_consensus.C_hash.parent_commit_hash_opt parent;
    creator_addr = creator;
    txid_hi = 3L;
    ts = epoch_ts;
  } in
  let proposal_id = Octra_consensus.C_hash.proposal_id header in
  let unsigned_vote = Octra_consensus.C_types.{
    chain_id = "octra-test";
    epoch_id = epoch;
    round;
    vote_type = Precommit;
    proposal_id;
    validator = creator;
    signature = String.make 64 '\x00';
  } in
  let vote = {
    unsigned_vote with
    signature =
      Mirage_crypto_ec.Ed25519.sign
        ~key:finality_private_key
        (Octra_consensus.C_hash.vote_sign_bytes unsigned_vote);
  } in
  let finality =
    Some Octra_consensus.C_codec.{
      finalize = {
        Octra_consensus.C_types.chain_id = "octra-test";
        epoch_id = epoch;
        commit_round = round;
        header;
        proposal_id;
        precommits = [vote];
        parent_commit = parent;
      };
      validator_set;
    }
  in
  Octra_consensus.C_codec.{
    epoch_id = epoch;
    prev_state_root = prev;
    state_root = root;
    tx_list_hash = empty_tx_list_hash;
    tx_hashes = [];
    txs_json = [];
    receipt_root;
    receipts_json = [];
    epoch_ts;
    creator_addr = creator;
    commit_round = round;
    reward_source;
    finality;
  }

let chunk ?(records = [ record () ]) () =
  Octra_consensus.C_driver.{
    responder_addr = "peer";
    request_id = "req";
    status = "ok";
    records;
    next_epoch = None;
  }

let transaction_json amount =
  `Assoc [
    "from", `String "octFrom";
    "to_", `String "octTo";
    "amount", `String amount;
    "nonce", `Int 1;
    "ou", `String "1000";
    "timestamp", `Float 1.;
    "signature", `String "sig";
    "op_type", `String "standard";
  ]
  |> Yojson.Safe.to_string

let record_with_transaction tx_json =
  let tx =
    match
      Yojson.Safe.from_string tx_json
      |> Octra_core.Transaction.of_yojson
    with
    | Ok tx -> tx
    | Error error -> failwith error
  in
  let tx_hash = Octra_core.Transaction.hash tx in
  {
    (record ()) with
    tx_list_hash =
      Octra_consensus.C_engine.tx_list_hash_for_header [tx_hash];
    tx_hashes = [tx_hash];
    txs_json = [tx_json];
  }

let apply_point ?(epoch = 10L) ?(root = base_root) ?eic ?(txid = 0L) () =
  Octra_consensus.C_catchup.{
    epoch;
    root;
    eic;
    txid;
  }

let finalize validated ?(creator = "oct_finalized") ?state_root
    ?parent_commit () =
  let record = validated.S.record in
  let proposed_state_root =
    Option.value state_root ~default:record.state_root
  in
  let header : Octra_consensus.C_types.epoch_header = {
    proto_version = Octra_consensus.C_types.proto_version_current;
    chain_id = "octra-test";
    epoch_id = record.epoch_id;
    prev_state_root = record.prev_state_root;
    tx_list_hash = record.tx_list_hash;
    receipt_root = record.receipt_root;
    proposed_state_root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = creator;
    txid_hi = Int64.pred validated.expected_txid;
    ts = record.epoch_ts +. 1.;
  } in
  {
    Octra_consensus.C_types.chain_id = "octra-test";
    epoch_id = record.epoch_id;
    commit_round = 3;
    header;
    proposal_id = String.make 32 'p';
    precommits = [];
    parent_commit;
  }

let parent_commit ~epoch ~proposer ~public_key =
  let validator = Octra_consensus.C_types.{
    address = proposer;
    pubkey = public_key;
  } in
  let validator_set =
    Octra_consensus.C_types.make_validator_set [validator]
  in
  let header : Octra_consensus.C_types.epoch_header = {
    proto_version = Octra_consensus.C_types.proto_version_current;
    chain_id = "octra-test";
    epoch_id = epoch;
    prev_state_root = String.make 32 'a';
    tx_list_hash = empty_tx_list_hash;
    receipt_root = Octra_consensus.C_hash.receipt_root [];
    proposed_state_root = String.make 32 'b';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = proposer;
    txid_hi = 3L;
    ts = 1.;
  } in
  {
    Octra_consensus.C_types.validator_set;
    certificate = {
      chain_id = "octra-test";
      epoch_id = epoch;
      commit_round = 0;
      header;
      proposal_id = String.make 32 'q';
      precommits = [{
        chain_id = "octra-test";
        epoch_id = epoch;
        round = 0;
        vote_type = Octra_consensus.C_types.Precommit;
        proposal_id = String.make 32 'q';
        validator = proposer;
        signature = String.make 64 's';
      }];
    };
  }

let test_reject_stripped_parent () =
  let public_key = String.make 32 'k' in
  let parent =
    parent_commit
      ~epoch:10L
      ~proposer:"oct_creator"
      ~public_key
  in
  let intact = record ~parent () in
  let valid =
    Octra_consensus.C_catchup.verify_record_finality
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:trusted_validator_set_hash
      ~expected_txid:4L
      ~record:intact
  in
  if Result.is_error valid then
    failwith "intact parent commit was rejected";
  let stripped =
    {
      intact with
      finality =
        Option.map
          (fun (finality : Octra_consensus.C_codec.catchup_finality) ->
            Octra_consensus.C_codec.{
              finalize = {
                finality.finalize with
                parent_commit = None;
              };
              validator_set = finality.validator_set;
            })
          intact.finality;
    }
  in
  match
    Octra_consensus.C_catchup.verify_record_finality
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:trusted_validator_set_hash
      ~expected_txid:4L
      ~record:stripped
  with
  | Error "finality qc is invalid at epoch = 11 reason = parent_commit_hash" -> ()
  | Error error -> failwith ("unexpected stripped parent error: " ^ error)
  | Ok _ -> failwith "stripped parent commit was accepted"

let deps ?(active = false) ?(observer = false) ?queued events =
  let catchup_active = ref active in
  let head = ref 10 in
  S.{
    catchup_active = (fun () -> !catchup_active);
    set_catchup_active = (fun active ->
      catchup_active := active;
      add events (Printf.sprintf "active:%b" active));
    queue_target = (fun ~target_epoch ~reason ->
      add events (Printf.sprintf "queue:%Ld:%s" target_epoch reason);
      Int64.to_string target_epoch);
    committed_head_epoch = (fun () -> !head);
    start_height = (fun height ->
      add events (Printf.sprintf "start:%Ld" height);
      Lwt.return_unit);
    take_queued_after = (fun ~head:current ->
      add events (Printf.sprintf "take:%Ld" current);
      queued);
    clear_queue = (fun () ->
      add events "clear_queue");
    read_local_root = (fun () ->
      add events "read_root";
      Lwt.return "root");
    set_state_attested = (fun ~head:attested_head ~root ->
      head := attested_head;
      add events (Printf.sprintf "attest:%d:%s" attested_head root));
    clear_quarantine = (fun reason ->
      add events ("clear_quarantine:" ^ reason));
    mark_quarantine = (fun reason ->
      add events ("mark_quarantine:" ^ reason));
    observer;
    drain_pending_finalized = (fun () ->
      add events "drain";
      Lwt.return_unit);
    wake_ready = (fun () ->
      add events "wake";
      Lwt.return_unit);
  }

let test_active_call_queues_only () =
  let events = ref [] in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success:_ ~fail_catchup:_ =
    add events "run_one";
    Lwt.return_unit
  in
  run (S.run (deps ~active:true events) ~run_one ~target_epoch:20L ~reason:"peer");
  let events = snapshot events in
  assert_true "queued" (has "queue:20:peer" events);
  assert_true "run_one skipped" (not (has "run_one" events))

let test_success_finishes_and_wakes () =
  let events = ref [] in
  let run_one ~target_epoch ~reason ~finish_success ~fail_catchup:_ =
    add events (Printf.sprintf "run_one:%Ld:%s" target_epoch reason);
    finish_success (unproved "done")
  in
  run (S.run (deps events) ~run_one ~target_epoch:20L ~reason:"peer");
  let events = snapshot events in
  assert_true "start height" (has "start:11" events);
  assert_true "not attested" (not (has "attest:10:root" events));
  assert_true "drained" (has "drain" events);
  assert_true "woke" (has "wake" events);
  assert_true "unverified finish does not clear quarantine"
    (not (has "clear_quarantine:catchup_complete:root_verified:done" events))

let test_unverified_attestation_hold () =
  let events = ref [] in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success ~fail_catchup:_ =
    finish_success (unproved "already_advanced:peer")
  in
  run (S.run (deps events) ~run_one ~target_epoch:20L ~reason:"peer");
  let events = snapshot events in
  assert_true "unverified root not attested" (not (has "attest:10:root" events));
  assert_true "unverified root not cleared"
    (not
       (has
          "clear_quarantine:catchup_complete:root_verified:already_advanced:peer"
          events))

let test_continue_queued_target () =
  let events = ref [] in
  let queued = Some { S.target_epoch = 30L; reason = "queued" } in
  let calls = ref [] in
  let run_one ~target_epoch ~reason ~finish_success ~fail_catchup:_ =
    calls := (target_epoch, reason) :: !calls;
    add events (Printf.sprintf "run_one:%Ld:%s" target_epoch reason);
    if target_epoch = 20L then finish_success (unproved "first")
    else Lwt.return_unit
  in
  run (S.run (deps ?queued events) ~run_one ~target_epoch:20L ~reason:"first");
  let calls = List.rev !calls in
  if calls <> [ (20L, "first"); (30L, "queued") ] then
    failwith "queued target was not continued";
  assert_true "continued" (has "run_one:30:queued" (snapshot events))

let test_failure_quarantine_clear () =
  let events = ref [] in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success:_ ~fail_catchup =
    fail_catchup "catchup_failed:test"
  in
  run (S.run (deps events) ~run_one ~target_epoch:20L ~reason:"peer");
  let events = snapshot events in
  assert_true "queue cleared" (has "clear_queue" events);
  assert_true "inactive" (has "active:false" events);
  assert_true "quarantined" (has "mark_quarantine:catchup_failed:test" events)

let test_throw_quarantine_release () =
  let events = ref [] in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success:_ ~fail_catchup:_ =
    Lwt.fail (Invalid_argument "incomplete catchup record")
  in
  run (S.run (deps events) ~run_one ~target_epoch:20L ~reason:"peer");
  let events = snapshot events in
  assert_true "exception queue cleared" (has "clear_queue" events);
  assert_true "exception releases actor" (has "active:false" events);
  assert_true "exception quarantined"
    (has "mark_quarantine:catchup_failed:unexpected" events)

let test_observer_drain_no_wake () =
  let events = ref [] in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success ~fail_catchup:_ =
    finish_success (unproved "observer")
  in
  run (S.run (deps ~observer:true events) ~run_one ~target_epoch:20L ~reason:"peer");
  let events = snapshot events in
  assert_true "observer drained before run" (has "drain" events);
  assert_true "wake skipped" (not (has "wake" events))

let test_range_plan_defaults () =
  let plan =
    S.range_plan
      ~env_timeout:None
      ~from_epoch:10L
      ~target_epoch:14L
  in
  assert_true "remain" (plan.remain = 5);
  assert_true "max epochs" (plan.max_epochs = 5);
  assert_true "timeout" (plan.timeout_seconds = 8.0);
  assert_true "log progress" (plan.log_progress = false)

let test_query_progress_decisions () =
  assert_true "prior head awaits range"
    (S.query_progress ~head:10 ~from_epoch:11L = S.Await_range);
  assert_true "applied head restarts query"
    (S.query_progress ~head:11 ~from_epoch:11L = S.Restart_from_head);
  assert_true "advanced head restarts query"
    (S.query_progress ~head:12 ~from_epoch:11L = S.Restart_from_head)

let test_range_retry_success () =
  let attempts = ref 0 in
  let slept = ref 0 in
  let deps = S.{
    sleep = (fun _ ->
      incr slept;
      Lwt.return_unit);
    query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_ ~validate:_ ->
      incr attempts;
      if !attempts = 3 then Lwt.return_some (chunk ())
      else Lwt.return_none);
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
  } in
  let result =
    run
      (S.query_range
         deps
         ~attempts:3
         ~retry_delay:0.0
         ~from_epoch:10L
         ~max_epochs:4
         ~timeout_seconds:1.0
         ~reason:"test"
         ~validate:(fun _ -> true))
  in
  assert_true "query succeeded" (Option.is_some result);
  assert_true "attempts" (!attempts = 3);
  assert_true "sleeps" (!slept = 2)

let query_chunk_deps ?(local_root = base_root) ?(answer = Some (chunk ()))
    events =
  S.{
    env_timeout = (fun () -> None);
    verify_http = check_qcs
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
      ~start_txid:4L;
    read_query_root = (fun () ->
      add events "query_root";
      Lwt.return local_root);
    range_query = {
      sleep = (fun _ ->
        add events "query_sleep";
        Lwt.return_unit);
      query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
          ~validate ->
        add events "query_range";
        match answer with
        | Some response when validate response ->
          Lwt.return_some response
        | _ ->
          Lwt.return_none);
      http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
    };
  }

let test_query_chunk_success () =
  let events = ref [] in
  let step =
    run
      (S.query_chunk
         (query_chunk_deps events)
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"ok")
  in
  begin
    match step with
    | S.Query_chunk _ -> ()
    | _ -> failwith "query chunk should return chunk"
  end;
  let events = snapshot events in
  assert_true "root read" (has "query_root" events);
  assert_true "query called" (has "query_range" events)

let test_query_chunk_failure () =
  let events = ref [] in
  let step =
    run
      (S.query_chunk
         (query_chunk_deps ~answer:None events)
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"miss")
  in
  begin
    match step with
    | S.Query_failed "catchup_failed:miss" -> ()
    | _ -> failwith "query chunk should fail"
  end;
  let events = snapshot events in
  assert_true "retried sleep twice"
    (List.length (List.filter (( = ) "query_sleep") events) = 2)

let test_http_qc () =
  let check = S.check_qcs
    ~chain_id:"octra-test"
    ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
    ~start_txid:4L in
  let first = record () in
  let second = record ~epoch:12L ~prev:next_root () in
  let changed = record ~epoch:12L ~prev:next_root ~creator:"oct_other" () in
  let wrong_chain = S.check_qcs
    ~chain_id:"octra-other"
    ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
    ~start_txid:4L in
  let first_epoch = function
    | Ok [record] -> record.Octra_consensus.C_codec.epoch_id = 11L
    | _ -> false
  in
  assert_true "changed validator set keeps proved prefix"
    (first_epoch (check [first; changed]));
  assert_true "other chain rejected"
    (Result.is_error (wrong_chain [first]));
  assert_true "missing finality rejected"
    (Result.is_error (check [{ first with finality = None }]));
  assert_true "signed range accepted"
    (match check [first; second] with Ok records -> List.length records = 2 | _ -> false);
  let bad_vote =
    match first.finality with
    | None -> failwith "finality missing"
    | Some proof ->
      let finalize = proof.finalize in
      let precommits =
        List.map
          (fun (vote : Octra_consensus.C_types.vote) ->
            { vote with signature = String.make 64 '\000' })
          finalize.precommits
      in
      { first with finality = Some { proof with
        finalize = { finalize with precommits } } }
  in
  assert_true "unsigned first record rejected"
    (Result.is_error (check [bad_vote]));
  let failed_read = S.check_qcs
    ~chain_id:"octra-test"
    ~expected_validator_set_hash:(fun _ -> failwith "store read")
    ~start_txid:4L in
  assert_true "store read rejected"
    (Result.is_error (failed_read [first]));
  let failed_later = S.check_qcs
    ~chain_id:"octra-test"
    ~expected_validator_set_hash:(fun epoch ->
      if epoch = 12L then failwith "store read"
      else Ok trusted_validator_set_hash)
    ~start_txid:4L in
  assert_true "store read keeps proved prefix"
    (first_epoch (failed_later [first; second]))

let test_query_chunk_http_reject () =
  let events = ref [] in
  let calls = ref 0 in
  let deps = query_chunk_deps events in
  let range_query = S.{ deps.range_query with
    query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
        ~validate ->
      incr calls;
      add events "query_range";
      let response = chunk () in
      Lwt.return
        (if !calls > 1 && validate response then Some response else None));
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate:_ ->
      add events "http";
      Lwt.return_some (chunk ~records:[{ (record ()) with finality = None }] ()));
  } in
  let step =
    run
      (S.query_chunk
         { deps with range_query }
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"reject")
  in
  begin
    match step with
    | S.Query_chunk _ -> ()
    | _ -> failwith "peer retry should succeed"
  end;
  assert_true "invalid http rejected" (!calls = 2);
  assert_true "http precedes second peer"
    (snapshot events =
     ["query_root"; "query_range"; "http"; "query_sleep"; "query_range"])

let test_query_chunk_http_first () =
  let events = ref [] in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate ->
      add events "http";
      let response = chunk () in
      Lwt.return (if validate response then Some response else None));
  } in
  let step =
    run
      (S.query_chunk
         { deps with range_query }
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"http")
  in
  begin
    match step with
    | S.Query_chunk _ -> ()
    | _ -> failwith "http chunk should be used"
  end;
  assert_true "http precedes peer retry"
    (snapshot events = ["query_root"; "query_range"; "http"])

let test_query_chunk_http_prefix () =
  let events = ref [] in
  let checks = ref 0 in
  let first = record () in
  let changed = record ~epoch:12L ~prev:next_root ~creator:"oct_other" () in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate ->
      let response = chunk ~records:[first; changed] () in
      Lwt.return (if validate response then Some response else None));
  } in
  let verify_http records =
    incr checks;
    S.check_qcs
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
      ~start_txid:4L records
  in
  let step =
    run
      (S.query_chunk
         { deps with range_query; verify_http }
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"prefix")
  in
  begin
    match step with
    | S.Query_chunk response ->
      assert_true "only proved prefix accepted"
        (List.map (fun record -> record.Octra_consensus.C_codec.epoch_id)
           response.records = [11L]);
      assert_true "prefix cursor" (response.next_epoch = Some 12L)
    | _ -> failwith "proved prefix should be used"
  end;
  assert_true "proof checked once" (!checks = 1)

let test_query_chunk_http_read () =
  let events = ref [] in
  let calls = ref 0 in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
        ~validate ->
      incr calls;
      let response = chunk () in
      Lwt.return
        (if !calls > 1 && validate response then Some response else None));
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate ->
      let response = chunk () in
      Lwt.return (if validate response then Some response else None));
  } in
  let step =
    run
      (S.query_chunk
         { deps with range_query;
           verify_http = (fun _ -> failwith "store read") }
         ~target_epoch:11L
         ~from_epoch:11L
         ~reason:"read")
  in
  assert_true "read failure retries peer"
    (match step with S.Query_chunk _ -> !calls = 2 | _ -> false)

let test_query_chunk_http_last () =
  let events = ref [] in
  let calls = ref 0 in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate ->
      incr calls;
      let response =
        if !calls = 1 then
          chunk ~records:[{ (record ()) with finality = None }] ()
        else chunk ()
      in
      Lwt.return (if validate response then Some response else None));
  } in
  let step =
    run
      (S.query_chunk
         { deps with range_query }
         ~target_epoch:11L
         ~from_epoch:11L
         ~reason:"last")
  in
  assert_true "last http response checked"
    (match step with S.Query_chunk _ -> !calls = 2 | _ -> false)

let test_query_chunk_http_bad_last () =
  let events = ref [] in
  let calls = ref 0 in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate ->
      incr calls;
      let response =
        chunk ~records:[{ (record ()) with finality = None }] ()
      in
      Lwt.return (if validate response then Some response else None));
  } in
  let step =
    run
      (S.query_chunk
         { deps with range_query }
         ~target_epoch:11L
         ~from_epoch:11L
         ~reason:"bad_last")
  in
  assert_true "invalid final http rejected"
    (match step with S.Query_failed _ -> !calls = 2 | _ -> false)

let test_base_gate_decisions () =
  begin
    match S.base_gate
            ~target_epoch:12L
            ~start_head:10L
            ~current_head:10L
            ~from_epoch:11L
            ~reason:"ok"
            ~head:(apply_point ()) with
    | S.Gate_continue -> ()
    | _ -> failwith "base gate should continue"
  end;
  begin
    match S.base_gate
            ~target_epoch:12L
            ~start_head:10L
            ~current_head:12L
            ~from_epoch:11L
            ~reason:"done"
            ~head:(apply_point ~epoch:12L ()) with
    | S.Gate_finish "already_advanced:done" -> ()
    | _ -> failwith "base gate should finish"
  end;
  begin
    match S.base_gate
            ~target_epoch:12L
            ~start_head:10L
            ~current_head:11L
            ~from_epoch:11L
            ~reason:"retry"
            ~head:(apply_point ~epoch:11L ()) with
    | S.Gate_retry -> ()
    | _ -> failwith "base gate should retry"
  end;
  match S.base_gate
          ~target_epoch:12L
          ~start_head:10L
          ~current_head:10L
          ~from_epoch:11L
          ~reason:"bad"
          ~head:(apply_point ~root:"" ()) with
  | S.Gate_fail "catchup_base_gate_failed:bad" -> ()
  | _ -> failwith "base gate should fail"

let test_continuity_gate_failure () =
  match S.continuity_gate
          ~records:[ record ~prev:wrong_root () ]
          ~from_epoch:11L
          ~prev_root:base_root
          ~reason:"cont" with
  | S.Gate_fail "catchup_base_mismatch:cont" -> ()
  | _ -> failwith "continuity gate should fail"

let test_apply_result_gate_decisions () =
  begin
    match S.apply_result_gate
            ~gap_active:false
            ~target_epoch:12L
            ~start_head:10L
            ~current_head:10L
            ~from_epoch:11L
            ~reason:"ok"
            (Ok (chunk ())) with
    | S.Apply_chunk _ -> ()
    | _ -> failwith "apply gate should pass chunk"
  end;
  begin
    match S.apply_result_gate
            ~gap_active:false
            ~target_epoch:12L
            ~start_head:10L
            ~current_head:12L
            ~from_epoch:11L
            ~reason:"done"
            (Error "x") with
    | S.Apply_finish "apply_already_advanced:done" -> ()
    | _ -> failwith "apply gate should finish"
  end;
  begin
    match S.apply_result_gate
            ~gap_active:false
            ~target_epoch:12L
            ~start_head:10L
            ~current_head:11L
            ~from_epoch:11L
            ~reason:"retry"
            (Error "x") with
    | S.Apply_retry -> ()
    | _ -> failwith "apply gate should retry"
  end;
  match S.apply_result_gate
          ~gap_active:false
          ~target_epoch:12L
          ~start_head:10L
          ~current_head:10L
          ~from_epoch:11L
          ~reason:"bad"
          (Error "x") with
  | S.Apply_fail "catchup_apply_failed:bad" -> ()
  | _ -> failwith "apply gate should fail"

let test_final_apply_gate () =
  let valid_head =
    apply_point ~epoch:11L ~root:next_root ~eic:"eic" ~txid:2L ()
  in
  begin
    match S.final_apply_gate
            ~last_epoch:11L
            ~expected_root:next_root
            ~expected_eic:"eic"
            ~expected_txid:2L
            ~head:valid_head
            (chunk ()) with
    | Ok _ -> ()
    | Error e -> failwith ("final gate should pass: " ^ e)
  end;
  match S.final_apply_gate
          ~last_epoch:11L
          ~expected_root:other_root
          ~expected_eic:"eic"
          ~expected_txid:2L
          ~head:valid_head
          (chunk ()) with
  | Error _ -> ()
  | Ok _ -> failwith "final gate should fail"

let test_validate_record_success () =
  let root = String.make 32 'r' in
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record:10
          (record ~root ()) with
  | Error e -> failwith ("validate record should pass: " ^ e)
  | Ok validated ->
    let validated : S.validated_record = validated in
    assert_true "parsed txs" (validated.parsed_txs = []);
    assert_true "parsed hashes" (validated.parsed_tx_hashes = []);
    assert_true "txid" (validated.expected_txid = 4L);
    assert_true "proposer"
      (validated.proposer =
       Some {
         Octra_core.Epochlog.creator_addr = "oct_creator";
         commit_round = 0;
       });
    assert_true "root" (validated.expected_root = Some root);
    match validated.apply_action with
    | S.Record_apply -> ()
    | _ -> failwith "record should apply"

let test_reject_untrusted_set () =
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:(String.make 32 '\xff')
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record:10
          (record ()) with
  | Error error ->
    assert_true
      "validator set mismatch"
      (String.equal
         error
         "finality validator set mismatch at epoch = 11")
  | Ok _ ->
    failwith "untrusted validator set should fail"

let test_record_hash_failure () =
  let bad =
    let base = record () in
    Octra_consensus.C_codec.{ base with tx_list_hash = String.make 32 'x' }
  in
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record:10
          bad with
  | Error _ -> ()
  | Ok _ -> failwith "bad tx_list_hash should fail"

let test_record_missing_reward () =
  let bad =
    let base = record () in
    Octra_consensus.C_codec.{ base with reward_source = None }
  in
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record:10
          bad with
  | Error "catchup reward source is missing" -> ()
  | Error error -> failwith ("unexpected reward source error: " ^ error)
  | Ok _ -> failwith "missing reward source should fail"

let test_record_parent_reward () =
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record:10
          (record
             ~reward_creator:"oct_parent"
             ()) with
  | Error error ->
    failwith ("parent reward proposer should pass: " ^ error)
  | Ok validated ->
    assert_true "parent reward proposer"
      (validated.S.reward.proposer_addr = "oct_parent")

let test_record_retry_action () =
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash:trusted_validator_set_hash
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record:12
          (record ~epoch:11L ()) with
  | Error e -> failwith ("validate record should pass: " ^ e)
  | Ok validated ->
    let validated : S.validated_record = validated in
    match validated.apply_action with
    | S.Record_retry_moved_head reason ->
      assert_true "retry reason"
        (String.length reason > 0)
    | _ -> failwith "record should retry"

let validated_or_fail ~head_before_record recd =
  let expected_validator_set_hash =
    match recd.Octra_consensus.C_codec.finality with
    | Some finality ->
      Octra_consensus.C_config.validator_set_hash finality.validator_set
    | None ->
      failwith "record finality is missing"
  in
  match S.validate_record
          ~chain_id:"octra-test"
          ~expected_validator_set_hash
          ~prev_eic:"eic0"
          ~start_txid:4L
          ~head_before_record
          recd with
  | Ok validated ->
    validated
  | Error e ->
    failwith ("record validation failed: " ^ e)

let apply_deps events ~head_before_record ~point =
  S.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    head_before_record = (fun () -> head_before_record);
    find_finalized = (fun _ -> None);
    put_proposer = (fun epoch _ ->
      add events (Printf.sprintf "proposer:%d" epoch));
    put_expected_root = (fun epoch _ ->
      add events (Printf.sprintf "root:%d" epoch));
    activate_gap = (fun () ->
      add events "gap");
    point_source = {
      head_epoch = (fun () -> Int64.to_int point.Octra_consensus.C_catchup.epoch);
      read_root = (fun () ->
        add events "read_point";
        Lwt.return point.root);
      cached_head = (fun () ->
        add events "cached_point";
        {
          S.cached_root = point.root;
          cached_eic = point.eic;
        });
      next_txid = (fun () -> point.txid);
    };
    write_finality = (fun validated ->
      add events
        (Printf.sprintf
           "write:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id);
      validated);
    promote_finality = (fun validated ->
      add events
        (Printf.sprintf
           "promote:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id));
    apply_record = (fun validated ->
      add events
        (Printf.sprintf
           "apply:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id);
      Lwt.return_unit);
    advance_height = (fun height ->
      add events (Printf.sprintf "advance:%Ld" height);
      Lwt.return_unit);
  }

let chunk_apply_deps ?(local_root = base_root) ?(gap_active = false)
    events ~head ~point =
  S.{
    read_local_root = (fun () ->
      add events "chunk_root";
      Lwt.return local_root);
    base_eic = (fun () -> "eic0");
    next_txid = (fun () -> 4L);
    current_head = (fun () -> Int64.of_int head);
    gap_active = (fun () -> gap_active);
    record_apply = apply_deps events ~head_before_record:head ~point;
  }

let test_apply_chunk_records_apply () =
  let recd = record ~root:(String.make 32 'r') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  let result =
    run
      (S.apply_chunk_records
         (apply_deps events ~head_before_record:10 ~point)
         ~prev_eic:"eic0"
         ~start_txid:4L
         (chunk ~records:[recd] ()))
  in
  begin
    match result with
    | Ok _ -> ()
    | Error e -> failwith ("apply chunk should pass: " ^ e)
  end;
  let events = snapshot events in
  assert_true "root stored" (has "root:11" events);
  assert_true "finality written" (has "write:11" events);
  assert_true "finality promoted" (has "promote:11" events);
  assert_true "record applied" (has "apply:11" events);
  assert_true "height advanced" (has "advance:12" events);
  assert_true "post point" (has "cached_point" events);
  assert_true "final point" (has "read_point" events)

let test_saved_round () =
  let module Journal = Octra_node_runtime.Consensus_finality_journal in
  let module Log = Octra_consensus.Finality_log in
  List.iter (fun committed ->
    let dir = Test_workspace.unique_dir "catchup-round" in
    let first = record () in
    let finality = Option.get first.Octra_consensus.C_codec.finality in
    Journal.persist_certificate dir ~validator_set:finality.validator_set finality.finalize;
    if committed then begin
      Journal.persist_bundle dir finality.finalize { tx_hashes = []; txs = []; receipts_json = [] };
      Log.write dir (Log.of_finalize finality.finalize);
      Journal.promote dir
    end;
    let incoming = record ~round:1 () in
    let height = if committed then 11 else 10 in
    let checked = validated_or_fail ~head_before_record:height incoming in
    let point = apply_point ~epoch:11L ~root:incoming.state_root
      ~eic:checked.expected_eic ~txid:checked.expected_txid () in
    let seen = ref None in
    let applied = ref None in
    let deps = { (apply_deps (ref []) ~head_before_record:height ~point) with
      write_finality = (fun value ->
        let cert = Option.get value.S.record.finality in
        let saved = Journal.stage dir ~chain_id:"octra-test"
          ~validator_set:cert.validator_set
          ~bundle:{ tx_hashes = value.record.tx_hashes; txs = value.parsed_txs;
            receipts_json = value.record.receipts_json } cert.finalize in
        Log.write dir (Log.of_finalize saved);
        S.bind_finality value saved |> Result.get_ok);
      put_proposer = (fun _ proposer -> seen := Some proposer.Octra_core.Epochlog.commit_round);
      apply_record = (fun value -> applied := Some value.S.record; Lwt.return_unit);
      promote_finality = (fun value -> Journal.promote_applied dir
        ~epoch:value.S.record.epoch_id ~state_root:value.record.state_root);
    } in
    let result = run (S.apply_chunk_records deps ~prev_eic:"eic0" ~start_txid:4L
      (chunk ~records:[incoming] ())) in
    assert_true "round recovery completes" (Result.is_ok result);
    assert_true "retained round reaches metadata" (!seen = Some 0);
    assert_true "applied record uses retained round"
      (committed || Option.map (fun value -> value.Octra_consensus.C_codec.commit_round) !applied = Some 0);
    let stored = match Journal.read_committed_epoch ~chain_id:"octra-test" ~epoch:11L dir with
      | Journal.Valid value -> value
      | _ -> failwith "committed proof missing" in
    let exported = { first with finality = Some { finality with finalize = stored.finalize } } in
    assert_true "range accepts retained proof and metadata"
      (Result.is_ok (Octra_consensus.C_catchup.verify_record_finality
        ~chain_id:"octra-test" ~expected_validator_set_hash:trusted_validator_set_hash
        ~expected_txid:checked.expected_txid ~record:exported));
    assert_true "log round matches metadata"
      (Option.map (fun value -> value.Log.round) (Log.last_entry_fast dir) = Some 0)
  ) [false; true]

let test_apply_chunk_records_skip () =
  let recd = record ~root:(String.make 32 's') () in
  let validated = validated_or_fail ~head_before_record:11 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  let result =
    run
      (S.apply_chunk_records
         (apply_deps events ~head_before_record:11 ~point)
         ~prev_eic:"eic0"
         ~start_txid:4L
         (chunk ~records:[recd] ()))
  in
  begin
    match result with
    | Ok _ -> ()
    | Error e -> failwith ("skip chunk should pass: " ^ e)
  end;
  let events = snapshot events in
  assert_true "already point" (has "read_point" events);
  assert_true "apply skipped" (not (has "apply:11" events));
  assert_true "finality written" (has "write:11" events);
  assert_true "finality promoted" (has "promote:11" events);
  assert_true "height advanced" (has "advance:12" events);
  assert_true "final point" (has "cached_point" events)

let test_apply_chunk_records_retry () =
  let recd = record ~root:(String.make 32 't') () in
  let validated = validated_or_fail ~head_before_record:12 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  let result =
    run
      (S.apply_chunk_records
         (apply_deps events ~head_before_record:12 ~point)
         ~prev_eic:"eic0"
         ~start_txid:4L
         (chunk ~records:[recd] ()))
  in
  begin
    match result with
    | Error _ -> ()
    | Ok _ -> failwith "retry chunk should fail"
  end;
  let events = snapshot events in
  assert_true "gap activated" (has "gap" events);
  assert_true "apply skipped" (not (has "apply:11" events));
  assert_true "height held" (not (has "advance:12" events));
  assert_true "final skipped" (not (has "read_point" events))

let test_apply_chunk_gate_continue () =
  let recd = record ~root:(String.make 32 'u') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  let step =
    run
      (S.apply_chunk_gate
         (chunk_apply_deps events ~head:10 ~point)
         ~target_epoch:12L
         ~start_head:10L
         ~from_epoch:11L
         ~reason:"gate"
         (chunk ~records:[recd] ()))
  in
  begin
    match step with
    | S.Chunk_continue _ -> ()
    | _ -> failwith "chunk gate should continue"
  end;
  let events = snapshot events in
  assert_true "local root read" (has "chunk_root" events);
  assert_true "record applied" (has "apply:11" events)

let test_apply_chunk_gate_retry () =
  let events = ref [] in
  let step =
    run
      (S.apply_chunk_gate
         (chunk_apply_deps events ~head:11 ~point:(apply_point ()))
         ~target_epoch:12L
         ~start_head:10L
         ~from_epoch:11L
         ~reason:"retry"
         (chunk ()))
  in
  match step with
  | S.Chunk_retry ->
    assert_true "record apply skipped" (not (has "apply:11" (snapshot events)))
  | _ ->
    failwith "chunk gate should retry"

let test_chunk_continuity_fail () =
  let bad = record ~prev:wrong_root () in
  let events = ref [] in
  let step =
    run
      (S.apply_chunk_gate
         (chunk_apply_deps events ~head:10 ~point:(apply_point ()))
         ~target_epoch:12L
         ~start_head:10L
         ~from_epoch:11L
         ~reason:"bad"
         (chunk ~records:[bad] ()))
  in
  match step with
  | S.Chunk_fail "catchup_base_mismatch:bad" -> ()
  | _ -> failwith "chunk gate should fail on continuity"

let target_deps ?answer events ~head ~point =
  S.{
    normalize = (fun ~source ->
      add events ("normalize:" ^ source));
    head_epoch = (fun () -> head);
    query = query_chunk_deps ?answer events;
    apply = chunk_apply_deps events ~head ~point;
  }

let target_wiring ?answer events ~head ~point =
  let query = query_chunk_deps ?answer events in
  let head_ref = ref head in
  S.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    normalize = (fun ~source ->
      add events ("normalize:" ^ source));
    head_epoch = (fun () -> !head_ref);
    env_timeout = query.env_timeout;
    read_query_root = query.read_query_root;
    range_query = query.range_query;
    read_apply_root = (fun () ->
      add events "read_point";
      Lwt.return point.Octra_consensus.C_catchup.root);
    cached_head = (fun () ->
      add events "cached_point";
      {
        S.cached_root = point.root;
        cached_eic = point.eic;
      });
    next_txid = (fun () -> point.txid);
    find_finalized = (fun _ -> None);
    put_proposer = (fun epoch _ ->
      add events (Printf.sprintf "proposer:%d" epoch));
    put_expected_root = (fun epoch _ ->
      add events (Printf.sprintf "root:%d" epoch));
    activate_gap = (fun () ->
      add events "gap");
    write_finality = (fun validated ->
      add events
        (Printf.sprintf
           "write:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id);
      validated);
    promote_finality = (fun validated ->
      add events
        (Printf.sprintf
           "promote:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id));
    apply_record = (fun validated ->
      add events
        (Printf.sprintf
           "apply:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id);
      head_ref := Int64.to_int point.Octra_consensus.C_catchup.epoch;
      Lwt.return_unit);
    advance_height = (fun height ->
      add events (Printf.sprintf "advance:%Ld" height);
      Lwt.return_unit);
    read_local_root = (fun () ->
      add events "chunk_root";
      Lwt.return base_root);
    base_eic = (fun () -> "eic0");
    current_head = (fun () -> Int64.of_int !head_ref);
    gap_active = (fun () -> false);
  }

let test_bind_finality () =
  let validated =
    validated_or_fail
      ~head_before_record:10
      (record ())
  in
  let finalized = finalize validated ~creator:"oct_creator" () in
  match S.bind_finality validated finalized with
  | Error error ->
    failwith ("finality binding should pass: " ^ error)
  | Ok completed ->
    assert_true "finality creator"
      (completed.record.creator_addr = "oct_creator");
    assert_true "finality round" (completed.record.commit_round = 3);
    assert_true "finality timestamp"
      (completed.record.epoch_ts = finalized.header.ts);
    assert_true "finality proposer"
      (completed.proposer =
       Some {
         Octra_core.Epochlog.creator_addr = "oct_creator";
         commit_round = 3;
       })

let test_finality_root_mismatch () =
  let validated =
    validated_or_fail
      ~head_before_record:10
      (record ())
  in
  match
    S.bind_finality
      validated
      (finalize
         validated
         ~creator:"oct_creator"
         ~state_root:(String.make 32 'x')
         ())
  with
  | Error "catchup finality commitment mismatch" ->
    ()
  | Error error ->
    failwith ("unexpected finality binding error: " ^ error)
  | Ok _ ->
    failwith "finality binding should reject a different root"

let test_finality_parent_reward () =
  let public_key = String.make 32 'k' in
  let validated =
    validated_or_fail
      ~head_before_record:10
      (record
         ~creator:"oct_current"
         ~reward_creator:"oct_parent"
         ~reward_public_key:public_key
         ())
  in
  let parent =
    parent_commit
      ~epoch:10L
      ~proposer:"oct_parent"
      ~public_key
  in
  match
    S.bind_finality
      validated
      (finalize
         validated
         ~creator:"oct_current"
         ~parent_commit:parent
         ())
  with
  | Error error ->
    failwith ("parent reward binding should pass: " ^ error)
  | Ok completed ->
    assert_true "current proposer preserved"
      (completed.record.creator_addr = "oct_current");
    assert_true "parent reward preserved"
      (completed.reward.proposer_addr = "oct_parent")

let test_finality_reward_mismatch () =
  let public_key = String.make 32 'k' in
  let validated =
    validated_or_fail
      ~head_before_record:10
      (record
         ~creator:"oct_current"
         ~reward_creator:"oct_other"
         ~reward_public_key:public_key
         ())
  in
  let parent =
    parent_commit
      ~epoch:10L
      ~proposer:"oct_parent"
      ~public_key
  in
  match
    S.bind_finality
      validated
      (finalize
         validated
         ~creator:"oct_current"
         ~parent_commit:parent
         ())
  with
  | Error "catchup reward source does not match parent commit" -> ()
  | Error error ->
    failwith ("unexpected parent reward error: " ^ error)
  | Ok _ ->
    failwith "different parent reward should fail"

let test_run_target_already_in_sync () =
  let events = ref [] in
  let finished = ref [] in
  run
    (S.run_target
       (target_deps events ~head:12 ~point:(apply_point ()))
       ~target_epoch:11L
       ~reason:"sync"
       ~finish_success:(fun finish ->
         add_finish events finish;
         finished := finish.S.tag :: !finished;
         Lwt.return_unit)
       ~fail_catchup:(fun tag ->
         add events ("fail:" ^ tag);
         Lwt.return_unit));
  let events = snapshot events in
  assert_true "normalized" (has "normalize:catchup:sync" events);
  assert_true "finished" (has "finish:already_in_sync:sync" events);
  assert_true "already in sync is unverified"
    (has "root_verified:false" events);
  assert_true "query skipped" (not (has "query_range" events))

let test_run_target_continue () =
  let recd = record ~root:(String.make 32 'v') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  run
    (S.run_target
       (target_deps ~answer:(Some (chunk ~records:[recd] ())) events ~head:10 ~point)
       ~target_epoch:11L
       ~reason:"target"
       ~finish_success:(fun finish ->
         add_finish events finish;
         Lwt.return_unit)
       ~fail_catchup:(fun tag ->
         add events ("fail:" ^ tag);
         Lwt.return_unit));
  let events = snapshot events in
  assert_true "query called" (has "query_range" events);
  assert_true "record applied" (has "apply:11" events);
  assert_true "finished" (has "finish:target" events);
  assert_true "applied root is verified" (has "root_verified:true" events)

let test_run_target_query_failed () =
  let events = ref [] in
  run
    (S.run_target
       (target_deps ~answer:None events ~head:10 ~point:(apply_point ()))
       ~target_epoch:11L
       ~reason:"miss"
       ~finish_success:(fun finish ->
         add_finish events finish;
         Lwt.return_unit)
       ~fail_catchup:(fun tag ->
         add events ("fail:" ^ tag);
         Lwt.return_unit));
  let events = snapshot events in
  assert_true "failed" (has "fail:catchup_failed:miss" events);
  assert_true "not finished" (not (has "finish:miss" events))

let test_local_apply_cancels_query () =
  let events = ref [] in
  let head = ref 10 in
  let query_cancelled = ref false in
  let waiting, _ = Lwt.task () in
  let query = S.{
    env_timeout = (fun () -> None);
    verify_http = check_qcs
      ~chain_id:"octra-test"
      ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
      ~start_txid:4L;
    read_query_root = (fun () -> Lwt.return base_root);
    range_query = {
      sleep = (fun _ ->
        head := 11;
        Lwt.return_unit);
      query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
          ~validate:_ ->
        add events "query_range";
        Lwt.finalize
          (fun () -> waiting)
          (fun () ->
            query_cancelled := true;
            add events "query_cancelled";
            Lwt.return_unit));
      http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
    };
  } in
  let deps = S.{
    normalize = (fun ~source -> add events ("normalize:" ^ source));
    head_epoch = (fun () -> !head);
    query;
    apply = chunk_apply_deps events ~head:10 ~point:(apply_point ());
  } in
  run
    (S.run_target
       deps
       ~target_epoch:11L
       ~reason:"local_apply"
       ~finish_success:(fun finish ->
         add_finish events finish;
         Lwt.return_unit)
       ~fail_catchup:(fun tag ->
         add events ("fail:" ^ tag);
         Lwt.return_unit));
  let events = snapshot events in
  assert_true "range query started" (has "query_range" events);
  assert_true "obsolete query cancelled" !query_cancelled;
  assert_true "query cancellation finalized" (has "query_cancelled" events);
  assert_true "local apply completed target"
    (has "finish:already_in_sync:local_apply" events);
  assert_true "local apply race is unverified"
    (has "root_verified:false" events);
  assert_true "network chunk not applied" (not (has "apply:11" events))

let test_run_with_target_success () =
  let recd = record ~root:(String.make 32 'w') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  run
    (S.run_with_target
       (deps events)
       ~target:(target_deps ~answer:(Some (chunk ~records:[recd] ())) events
                  ~head:10
                  ~point)
       ~target_epoch:11L
       ~reason:"full");
  let events = snapshot events in
  assert_true "outer activated" (has "active:true" events);
  assert_true "target normalized" (has "normalize:catchup:full" events);
  assert_true "target queried" (has "query_range" events);
  assert_true "target applied" (has "apply:11" events);
  assert_true "outer finished" (has "active:false" events);
  assert_true "driver restarted" (has "start:11" events);
  assert_true "state attested" (has "attest:10:root" events);
  assert_true "driver woke" (has "wake" events)

let test_root_mismatch_attest_hold () =
  let recd = record ~root:(String.make 32 'w') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:(String.make 32 'z')
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  let target =
    target_deps ~answer:(Some (chunk ~records:[recd] ())) events
      ~head:10
      ~point
  in
  let target = S.{ target with
    apply = { target.apply with gap_active = (fun () -> true) };
  } in
  run
    (S.run_with_target
       (deps events)
       ~target
       ~target_epoch:11L
       ~reason:"root_mismatch");
  let events = snapshot events in
  assert_true "record applied before mismatch" (has "apply:11" events);
  assert_true "mismatched root not attested"
    (not (has "attest:10:root" events));
  assert_true "mismatched root does not clear quarantine"
    (not
       (List.exists
          (fun event ->
            String.starts_with ~prefix:"clear_quarantine:" event)
          events))

let test_run_wired_success () =
  let recd = record ~root:(String.make 32 'x') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let events = ref [] in
  run
    (S.run_wired
       (deps events)
       ~target:(target_wiring ~answer:(Some (chunk ~records:[recd] ())) events
                  ~head:10
                  ~point)
       ~target_epoch:11L
       ~reason:"wired");
  let events = snapshot events in
  assert_true "wired activated" (has "active:true" events);
  assert_true "wired normalized" (has "normalize:catchup:wired" events);
  assert_true "wired queried" (has "query_range" events);
  assert_true "wired applied" (has "apply:11" events);
  assert_true "wired woke" (has "wake" events)

let test_run_driver_wired_success () =
  let recd = record ~root:(String.make 32 'y') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point =
    apply_point
      ~epoch:11L
      ~root:recd.Octra_consensus.C_codec.state_root
      ~eic:validated.S.expected_eic
      ~txid:validated.expected_txid
      ()
  in
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let active = ref false in
  let events = ref [] in
  let state = Octra_node_runtime.Consensus_finality_state.create () in
  let finality = Octra_node_runtime.Consensus_finality_state.callbacks state in
  let head = ref 10 in
  let local_root = ref base_root in
  let query =
    query_chunk_deps ~answer:(Some (chunk ~records:[recd] ())) events
  in
  let io = S.{
    start_height = (fun height ->
      add events (Printf.sprintf "start:%Ld" height);
      Lwt.return_unit);
    advance_height = (fun height ->
      add events (Printf.sprintf "advance:%Ld" height);
      Lwt.return_unit);
    wake_ready = (fun () ->
      add events "wake";
      Lwt.return_unit);
    range_query = query.range_query;
  } in
  let wiring = S.{
    chain_id = "octra-test";
    expected_validator_set_hash = (fun _ ->
      Ok trusted_validator_set_hash);
    catchup_active = active;
    queue;
    committed_head_epoch = (fun () -> !head);
    normalize = (fun ~source ->
      add events ("normalize:" ^ source));
    env_timeout = query.env_timeout;
    read_local_root = (fun () ->
      add events "root";
      Lwt.return !local_root);
    cached_head = (fun () ->
      add events "cached";
      {
        S.cached_root = point.root;
        cached_eic = point.eic;
      });
    next_txid = (fun () -> point.txid);
    finality;
    write_finality = (fun validated ->
      add events
        (Printf.sprintf
           "write:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id);
      validated);
    promote_finality = (fun validated ->
      add events
        (Printf.sprintf
           "promote:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id));
    apply_record = (fun validated ->
      add events
        (Printf.sprintf
           "apply:%Ld"
           validated.S.record.Octra_consensus.C_codec.epoch_id);
      head := Int64.to_int point.Octra_consensus.C_catchup.epoch;
      local_root := point.root;
      Lwt.return_unit);
    base_eic = (fun () -> "eic0");
    current_head = (fun () -> Int64.of_int !head);
    set_state_attested = (fun ~head ~root ->
      add events (Printf.sprintf "attest:%d:%s" head root));
    clear_quarantine = (fun reason ->
      add events ("clear:" ^ reason));
    mark_quarantine = (fun reason ->
      add events ("mark:" ^ reason));
    observer = false;
    drain_pending_finalized = (fun () ->
      add events "drain";
      Lwt.return_unit);
    http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
  } in
  run
    (S.run_driver_wired
       wiring
       io
       ~target_epoch:11L
       ~reason:"driver");
  let events = snapshot events in
  assert_true "driver inactive after finish" (not !active);
  assert_true "driver normalized" (has "normalize:catchup:driver" events);
  assert_true "driver queried" (has "query_range" events);
  assert_true "driver applied" (has "apply:11" events);
  assert_true "driver wrote finality" (has "write:11" events);
  assert_true "driver promoted finality" (has "promote:11" events);
  assert_true "driver restarted" (has "start:12" events);
  assert_true "driver woke" (has "wake" events)

let test_node_queue_refs () =
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let active = ref false in
  let events = ref [] in
  let deps =
    S.node_deps
      {
        catchup_active = active;
        queue;
        committed_head_epoch = (fun () -> 10);
        start_height = (fun height ->
          add events (Printf.sprintf "start:%Ld" height);
          Lwt.return_unit);
        read_local_root = (fun () ->
          add events "read_root";
          Lwt.return base_root);
        set_state_attested = (fun ~head ~root ->
          add events (Printf.sprintf "attest:%d:%s" head root));
        clear_quarantine = (fun reason ->
          add events ("clear:" ^ reason));
        mark_quarantine = (fun reason ->
          add events ("mark:" ^ reason));
        observer = false;
        drain_pending_finalized = (fun () ->
          add events "drain";
          Lwt.return_unit);
        wake_ready = (fun () ->
          add events "wake";
          Lwt.return_unit);
      }
  in
  assert_true "adapter inactive" (not (deps.catchup_active ()));
  deps.set_catchup_active true;
  assert_true "adapter active" !active;
  assert_true "queue target label"
    (deps.queue_target ~target_epoch:20L ~reason:"manual" = "20");
  begin
    match deps.take_queued_after ~head:10L with
    | Some queued ->
      assert_true "queued epoch" (queued.S.target_epoch = 20L);
      assert_true "queued reason" (queued.reason = "manual")
    | None ->
      failwith "queued target missing"
  end;
  assert_true "queue drained"
    (deps.take_queued_after ~head:10L = None);
  ignore (deps.queue_target ~target_epoch:21L ~reason:"clear");
  deps.clear_queue ();
  assert_true "queue cleared"
    (deps.take_queued_after ~head:10L = None)

let test_target_finality_gap () =
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let events = ref [] in
  let state = Octra_node_runtime.Consensus_finality_state.create () in
  let finality = Octra_node_runtime.Consensus_finality_state.callbacks state in
  let point = apply_point ~eic:"eic0" ~txid:4L () in
  let query = query_chunk_deps events in
  let target =
    S.target_wiring_of_node
      {
        chain_id = "octra-test";
        expected_validator_set_hash = (fun _ ->
          Ok trusted_validator_set_hash);
        normalize = (fun ~source ->
          add events ("normalize:" ^ source));
        head_epoch = (fun () -> 10);
        env_timeout = (fun () -> None);
        read_local_root = (fun () ->
          add events "root";
          Lwt.return base_root);
        range_query = query.range_query;
        cached_head = (fun () ->
          add events "cached";
          {
            S.cached_root = point.root;
            cached_eic = point.eic;
          });
        next_txid = (fun () -> point.txid);
        finality;
        queue;
        write_finality = (fun validated ->
          add events
            (Printf.sprintf
               "write:%Ld"
               validated.S.record.Octra_consensus.C_codec.epoch_id);
          validated);
        promote_finality = (fun validated ->
          add events
            (Printf.sprintf
               "promote:%Ld"
               validated.S.record.Octra_consensus.C_codec.epoch_id));
        apply_record = (fun validated ->
          add events
            (Printf.sprintf
               "apply:%Ld"
               validated.S.record.Octra_consensus.C_codec.epoch_id);
          Lwt.return_unit);
        advance_height = (fun height ->
          add events (Printf.sprintf "advance:%Ld" height);
          Lwt.return_unit);
        base_eic = (fun () -> "eic0");
        current_head = (fun () -> 10L);
      }
  in
  target.put_proposer 12 {
    Octra_core.Epochlog.creator_addr = "oct_creator";
    commit_round = 4;
  };
  target.put_expected_root 12 "root12";
  assert_true "target proposer"
    (Octra_node_runtime.Consensus_finality_state.find_proposer state 12 =
     Some { Octra_core.Epochlog.creator_addr = "oct_creator"; commit_round = 4 });
  assert_true "target expected root"
    (Octra_node_runtime.Consensus_finality_state.find_expected_root state 12 =
     Some "root12");
  assert_true "gap inactive" (not (target.gap_active ()));
  target.activate_gap ();
  assert_true "gap active" (target.gap_active ());
  assert_true "same root reader"
    (Lwt_main.run (target.read_query_root ()) = base_root
     && Lwt_main.run (target.read_apply_root ()) = base_root
     && Lwt_main.run (target.read_local_root ()) = base_root)

let test_driver_runtime_fields () =
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let active = ref false in
  let state = Octra_node_runtime.Consensus_finality_state.create () in
  let finality = Octra_node_runtime.Consensus_finality_state.callbacks state in
  let cached_root =
    Octra_node_runtime.Consensus_driver_read.{
      root = "root";
      eic = Some "eic";
    }
  in
  let wiring =
    S.driver_runner_wiring_of_node
      S.{
        chain_id = "octra-test";
        expected_validator_set_hash = (fun _ ->
          Ok trusted_validator_set_hash);
        catchup_active = active;
        queue;
        committed_head_epoch = (fun () -> 17);
        normalize = (fun ~source:_ -> ());
        env_timeout = (fun () -> Some "3");
        read_local_root = (fun () -> Lwt.return "local");
        cached_root = (fun () -> cached_root);
        next_txid = (fun () -> 44L);
        finality;
        write_finality = Fun.id;
        promote_finality = ignore;
        apply_record = (fun _ -> Lwt.return_unit);
        base_eic = (fun () -> "base");
        set_state_attested = (fun ~head:_ ~root:_ -> ());
        clear_quarantine = ignore;
        mark_quarantine = ignore;
        observer = true;
        drain_pending_finalized = (fun () -> Lwt.return_unit);
        http_range = (fun ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
      }
  in
  let cached = wiring.S.cached_head () in
  assert_true "driver node current head" (wiring.current_head () = 17L);
  assert_true "driver node cached root" (cached.S.cached_root = "root");
  assert_true "driver node cached eic" (cached.cached_eic = Some "eic");
  assert_true "driver node keeps ref" (wiring.catchup_active == active);
  assert_true "driver node observer" wiring.observer;
  assert_true "driver node queue"
    (Octra_node_runtime.Consensus_catchup_queue.target wiring.queue = None)

let test_queue_event_labels_gap () =
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let event =
    S.queue_target_event
      queue
      ~active:true
      ~target_epoch:20L
      ~reason:"manual"
  in
  assert_true "target event epoch" (event.S.queued_target_epoch = 20L);
  assert_true "target event reason" (event.queued_reason = "manual");
  assert_true "target event active" event.queued_active;
  assert_true "target event label" (event.queued_label = "20");
  assert_true "queue target"
    (Octra_node_runtime.Consensus_catchup_queue.target queue = Some 20L);
  let gap_event =
    S.queue_gap_event
      queue
      ~active:false
      ~target_epoch:19L
      ~reason:"gap"
  in
  assert_true "gap event input epoch" (gap_event.S.queued_target_epoch = 19L);
  assert_true "gap event reason" (gap_event.queued_reason = "gap");
  assert_true "gap event inactive" (not gap_event.queued_active);
  assert_true "gap preserves higher target label" (gap_event.queued_label = "20");
  assert_true "gap active"
    (Octra_node_runtime.Consensus_catchup_queue.gap_active queue)

let test_queue_log_effects () =
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let clear_count = ref 0 in
  S.queue_target_and_log
    queue
    ~active:(fun () -> false)
    ~target_epoch:30L
    ~reason:"target";
  assert_true "target helper queued"
    (Octra_node_runtime.Consensus_catchup_queue.target queue = Some 30L);
  S.queue_gap_and_log
    queue
    ~active:(fun () -> true)
    ~clear_state_attested:(fun () -> incr clear_count)
    ~target_epoch:29L
    ~reason:"gap";
  assert_true "gap helper clears attestation" (!clear_count = 1);
  assert_true "gap helper active"
    (Octra_node_runtime.Consensus_catchup_queue.gap_active queue);
  assert_true "gap helper preserves target"
    (Octra_node_runtime.Consensus_catchup_queue.target queue = Some 30L)

let test_node_queue_target_gap () =
  let queue = Octra_node_runtime.Consensus_catchup_queue.create () in
  let clear_count = ref 0 in
  let runtime =
    S.{
      queue;
      catchup_active = ref false;
      clear_state_attested = (fun () -> incr clear_count);
    }
  in
  let node_queue = S.node_queue runtime in
  node_queue.queue_catchup_target ~target_epoch:40L ~reason:"target";
  assert_true "node queue target"
    (Octra_node_runtime.Consensus_catchup_queue.target queue = Some 40L);
  runtime.catchup_active := true;
  node_queue.queue_finalized_gap ~target_epoch:39L ~reason:"gap";
  assert_true "node queue clears attestation" (!clear_count = 1);
  assert_true "node queue gap active"
    (Octra_node_runtime.Consensus_catchup_queue.gap_active queue);
  assert_true "node queue keeps target"
    (Octra_node_runtime.Consensus_catchup_queue.target queue = Some 40L)

let test_response_payload_integrity () =
  let valid = record_with_transaction (transaction_json "1") in
  assert_true "valid response payload"
    (S.response_payload_valid [valid]);
  let changed_body = {
    valid with
    txs_json = [transaction_json "2"];
  } in
  assert_true "changed transaction body rejected"
    (not (S.response_payload_valid [changed_body]));
  let changed_receipts = {
    valid with
    receipts_json = ["changed"];
  } in
  assert_true "changed receipts rejected"
    (not (S.response_payload_valid [changed_receipts]));
  assert_true "empty response payload rejected"
    (not (S.response_payload_valid []));
  let oversized =
    transaction_json "1"
    |> Yojson.Safe.from_string
    |> function
      | `Assoc fields ->
        `Assoc (("message", `String (String.make 257 'm')) :: fields)
        |> Yojson.Safe.to_string
      | _ -> failwith "transaction json shape"
  in
  let oversized_record = record_with_transaction oversized in
  assert_true "finalized payload ignores admission cap"
    (S.response_payload_valid [oversized_record])

let tests = [
  "reject stripped parent", test_reject_stripped_parent;
  "active call queues only", test_active_call_queues_only;
  "success finishes and wakes", test_success_finishes_and_wakes;
  "unverified attestation hold", test_unverified_attestation_hold;
  "continue queued target", test_continue_queued_target;
  "failure quarantine clear", test_failure_quarantine_clear;
  "throw quarantine release",
    test_throw_quarantine_release;
  "observer drain no wake", test_observer_drain_no_wake;
  "range plan defaults", test_range_plan_defaults;
  "query progress decisions", test_query_progress_decisions;
  "range retry success", test_range_retry_success;
  "query chunk success", test_query_chunk_success;
  "query chunk failure", test_query_chunk_failure;
  "http qc", test_http_qc;
  "query chunk http reject", test_query_chunk_http_reject;
  "query chunk http first", test_query_chunk_http_first;
  "query chunk http prefix", test_query_chunk_http_prefix;
  "query chunk http read", test_query_chunk_http_read;
  "query chunk http last", test_query_chunk_http_last;
  "query chunk http bad last", test_query_chunk_http_bad_last;
  "base gate decisions", test_base_gate_decisions;
  "continuity gate failure", test_continuity_gate_failure;
  "apply result gate decisions", test_apply_result_gate_decisions;
  "final apply gate", test_final_apply_gate;
  "validate record success", test_validate_record_success;
  "reject untrusted set",
    test_reject_untrusted_set;
  "record hash failure", test_record_hash_failure;
  "record missing reward", test_record_missing_reward;
  "record parent reward", test_record_parent_reward;
  "record retry action", test_record_retry_action;
  "bind finality", test_bind_finality;
  "saved round", test_saved_round;
  "finality root mismatch", test_finality_root_mismatch;
  "finality parent reward", test_finality_parent_reward;
  "finality reward mismatch", test_finality_reward_mismatch;
  "apply chunk records apply", test_apply_chunk_records_apply;
  "apply chunk records skip", test_apply_chunk_records_skip;
  "apply chunk records retry", test_apply_chunk_records_retry;
  "apply chunk gate continue", test_apply_chunk_gate_continue;
  "apply chunk gate retry", test_apply_chunk_gate_retry;
  "chunk continuity fail", test_chunk_continuity_fail;
  "run target already in sync", test_run_target_already_in_sync;
  "run target continue", test_run_target_continue;
  "run target query failed", test_run_target_query_failed;
  "local apply cancels query", test_local_apply_cancels_query;
  "run with target success", test_run_with_target_success;
  "root mismatch attest hold",
    test_root_mismatch_attest_hold;
  "run wired success", test_run_wired_success;
  "run driver wired success", test_run_driver_wired_success;
  "node queue refs", test_node_queue_refs;
  "target finality gap", test_target_finality_gap;
  "driver runtime fields", test_driver_runtime_fields;
  "queue event labels gap", test_queue_event_labels_gap;
  "queue log effects", test_queue_log_effects;
  "node queue target gap", test_node_queue_target_gap;
  "response payload integrity", test_response_payload_integrity;
]

let () =
  List.iter (fun (name, f) ->
    try f () with exn ->
      failwith (Printf.sprintf "%s failed: %s" name (Printexc.to_string exn))
  ) tests;
  print_endline "status = pass test = node_runtime_consensus_catchup_shell"