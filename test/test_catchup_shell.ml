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

let proved_record ?(epoch = 11L) ?(prev = base_root)
    ?(creator = "oct_creator") () =
  let public_key = Mirage_crypto_ec.Ed25519.pub_to_octets finality_public_key in
  let parent = parent_commit ~epoch:(Int64.pred epoch)
    ~proposer:creator ~public_key in
  record ~epoch ~prev ~creator ~reward_public_key:public_key ~parent ()

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

let test_finish_keeps_owner () =
  let events = ref [] in
  let state = deps events in
  let active = ref [] in
  let state = { state with
    drain_pending_finalized = (fun () ->
      active := state.catchup_active () :: !active;
      Lwt.return_unit);
  } in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success ~fail_catchup:_ =
    finish_success (unproved "done") in
  run (S.run state ~run_one ~target_epoch:20L ~reason:"peer");
  assert_true "drain retains catchup ownership" (!active = [true; true]);
  assert_true "finished run releases ownership" (not (state.catchup_active ()))

let test_finish_queued_during_drain () =
  let events = ref [] in
  let state = deps events in
  let queued = ref None in
  let drains = ref 0 in
  let calls = ref [] in
  let state = { state with
    take_queued_after = (fun ~head:_ ->
      let next = !queued in queued := None; next);
    drain_pending_finalized = (fun () ->
      incr drains;
      if !drains = 2 then
        queued := Some { S.target_epoch = 30L; reason = "arrived" };
      Lwt.return_unit);
  } in
  let run_one ~target_epoch ~reason:_ ~finish_success ~fail_catchup:_ =
    calls := target_epoch :: !calls;
    finish_success (unproved "done") in
  run (S.run state ~run_one ~target_epoch:20L ~reason:"peer");
  assert_true "drain arrival runs before release" (!calls = [30L; 20L]);
  assert_true "one ownership release"
    (List.filter (( = ) "active:false") (snapshot events) = ["active:false"])

let health_deps active events =
  Octra_node_runtime.Consensus_health_wiring.{
    sleep = (fun _ -> add events "sleep"; Lwt.return_unit);
    catchup_active = (fun () -> !active);
    quarantine_active = (fun () -> true);
    replay_stashed = (fun ~source:_ -> active := true; Lwt.return_unit);
    probe_health = (fun () -> add events "probe"; Lwt.return_unit);
    reset_liveness = (fun ~source:_ -> add events "reset"; Lwt.return_unit);
  }

let test_wake_keeps_new_owner () =
  List.iter (fun cancel ->
    let events = ref [] in
    let wake, release = Lwt.task () in
    let next, finish = Lwt.task () in
    let state = { (deps events) with wake_ready = (fun () -> wake) } in
    let run_one ~target_epoch ~reason:_ ~finish_success ~fail_catchup:_ =
      if target_epoch = 20L then finish_success (unproved "done") else next in
    let first = S.run state ~run_one ~target_epoch:20L ~reason:"first" in
    assert_true "first run waits for wake" (Lwt.is_sleeping first);
    let second = S.run state ~run_one ~target_epoch:30L ~reason:"second" in
    assert_true "second run owns catchup" (state.catchup_active ());
    if cancel then Lwt.cancel first
    else Lwt.wakeup_later_exn release (Failure "wake failed");
    run (Lwt.catch (fun () -> first) (fun _ -> Lwt.return_unit));
    assert_true "old wake cannot release new owner" (state.catchup_active ());
    assert_true "old wake cannot quarantine new owner"
      (not (has "mark_quarantine:catchup_failed:unexpected" (snapshot events)));
    Lwt.wakeup_later finish ();
    run second
  ) [false; true]

let test_finish_stashed_arrival () =
  let module R = Octra_node_runtime.Consensus_finalized_replay in
  let module F = Octra_node_runtime.Consensus_finalized_shell in
  let module Q = Octra_node_runtime.Consensus_catchup_queue in
  let events = ref [] in
  let head = ref 10 in
  let saved = Hashtbl.create 2 in
  let queue = Q.create () in
  let state = deps events in
  let proof epoch = Option.get (record ~epoch ()).finality in
  let store epoch item = Hashtbl.replace saved epoch
    (item.Octra_consensus.C_codec.finalize, item.validator_set) in
  let apply ~validator_set:_ item =
    head := Int64.to_int item.Octra_consensus.C_types.epoch_id;
    Hashtbl.remove saved !head;
    Lwt.return_unit in
  let shell = F.{
    prune_frozen = (fun ~finalized_epoch:_ -> ());
    store_expected_root = (fun ~epoch:_ ~root:_ -> ());
    store_finalized = (fun ~epoch ~validator_set item ->
      Hashtbl.replace saved epoch (item, validator_set));
    remove_finalized = (fun ~epoch -> Hashtbl.remove saved epoch);
    remove_proposer = (fun ~epoch:_ -> ());
    committed_head_epoch = (fun () -> !head);
    driver = (fun () -> Some ()); observer_mode = false;
    catchup_active = state.catchup_active;
    quarantine_active = (fun () -> false); state_attested = (fun () -> true);
    current_epoch = (fun () -> !head + 1);
    read_local_root_raw = (fun () -> Lwt.return base_root);
    queue_catchup_target = (fun ~target_epoch ~reason ->
      ignore (Q.queue queue ~target_epoch ~reason));
    run_catchup_to_target = (fun () ~target_epoch:_ ~reason:_ ->
      failwith "unexpected range query");
    mark_quarantine = (fun _ -> failwith "unexpected quarantine");
    clear_quarantine = (fun _ -> ());
    apply_finalized = apply;
    replay_stashed_while_safe = (fun ~source:_ -> failwith "unexpected replay");
  } in
  let replay = R.{
    committed_head_epoch = shell.committed_head_epoch;
    catchup_active = state.catchup_active;
    quarantine_active = shell.quarantine_active;
    find_finalized = (fun epoch -> Hashtbl.find_opt saved epoch);
    read_local_root_raw = shell.read_local_root_raw;
    apply_finalized = (fun ~validator_set item ->
      let open Lwt.Syntax in
      let* () = apply ~validator_set item in
      if !head <> 11 then Lwt.return_unit else
      let arrival = proof 12L in
      F.handle shell ~validator_set:arrival.validator_set arrival.finalize);
  } in
  let state = { state with
    committed_head_epoch = shell.committed_head_epoch;
    take_queued_after = (fun ~head ->
      Option.map (fun (item : Q.queued) ->
        S.{ target_epoch = item.target_epoch; reason = item.reason })
        (Q.take_if_after queue ~head));
    clear_queue = (fun () -> Q.clear_all queue);
    drain_pending_finalized = (fun () -> R.drain replay);
  } in
  let run_one ~target_epoch ~reason:_ ~finish_success ~fail_catchup:_ =
    if target_epoch = 11L then store 11 (proof 11L);
    finish_success (unproved "done") in
  run (S.run state ~run_one ~target_epoch:11L ~reason:"peer");
  assert_true "arrival during drain is applied without a probe" (!head = 12);
  assert_true "arrival leaves no saved work" (Hashtbl.length saved = 0)

let test_startup_delay_has_no_owner () =
  let active = ref false in
  let events = ref [] in
  let state = { (health_deps active events) with
    sleep = (fun _ ->
      assert_true "startup delay leaves catchup free" (not !active);
      active := true;
      Lwt.return_unit);
  } in
  run (Octra_node_runtime.Consensus_health_wiring.startup_probe state);
  assert_true "startup keeps a run begun during delay" !active;
  assert_true "startup does not probe active run" (!events = [])

let test_startup_owns_no_run () =
  let active = ref true in
  let events = ref [] in
  run (Octra_node_runtime.Consensus_health_wiring.startup_probe
    (health_deps active events));
  assert_true "startup cannot release another run" !active;
  assert_true "startup skips running catchup" (!events = [])

let test_startup_hold_waits () =
  let active = ref false and events = ref [] in
  let calls = ref 0 and sleeps = ref 0 in
  let deps = { (health_deps active events) with
    sleep = (fun _ -> incr sleeps; Lwt.return_unit);
  } in
  let ready = run (Octra_node_runtime.Consensus_health_wiring.await_pending_recovery
    deps ~delay:12.0 (fun () ->
      incr calls;
      Lwt.return (!calls = 77))) in
  assert_true "startup peer wait stopped before recovery"
    (ready && !calls = 77 && !sleeps = 76);
  calls := 0;
  sleeps := 0;
  let ready = run (Octra_node_runtime.Consensus_health_wiring.await_pending_recovery
    deps ~delay:12.0 (fun () ->
      incr calls;
      Lwt.return (!calls = 75))) in
  assert_true "verified startup did not release"
    (ready && !calls = 75 && !sleeps = 74);
  calls := 0;
  sleeps := 0;
  let stopped = try
    ignore (run (Octra_node_runtime.Consensus_health_wiring.await_pending_recovery
      deps ~delay:12.0 (fun () ->
        incr calls;
        Lwt.fail (Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault "disk"))));
    false
  with Octra_node_runtime.Consensus_pending_commit_recovery.Store_fault "disk" -> true in
  assert_true "startup store fault retried" (stopped && !calls = 1 && !sleeps = 0)

let test_poll_rechecks_owner () =
  let active = ref false in
  let events = ref [] in
  run (Octra_node_runtime.Consensus_health_wiring.poll_once
    (health_deps active events));
  assert_true "poll skips probe after catchup starts" (!events = [])

module Health = Octra_node_runtime.Consensus_health_shell

let probe_config = Health.{
    active_f = 1; validator_count = 4; state_attest_configured = 2;
    snapshot_policy_threshold = 4096; soft_catchup_max_lag = 4;
    quarantine_ahead_streak_threshold = 3; quarantine_ahead_grace_epochs = 2;
    quarantine_ahead_drift_tolerance = 1;
  }

let probe_state active change = Health.{
    normalize_next_epoch_for_head = (fun ~source:_ -> ());
    committed_head_epoch = (fun () -> 10); current_epoch = (fun () -> 11);
    catchup_next_target = (fun () -> None); attested_head = (fun _ -> false);
    clear_state_attested = change; catchup_active = (fun () -> !active);
    set_state_attested = (fun ~head:_ ~root:_ -> change ());
    clear_quarantine = (fun _ -> change ()); mark_quarantine = (fun _ -> change ());
    query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ -> Lwt.return []);
    root_consensus_quorum = (fun ~epoch_id:_ ~root:_ _ -> false);
    read_local_root_raw = (fun () -> Lwt.return base_root);
    committed_epoch_root_raw = (fun _ -> Some base_root);
    peer_snapshot = (fun () -> "");
    drain_pending_finalized = (fun () -> change (); Lwt.return_unit);
    wake_ready = (fun () -> change (); Lwt.return_unit);
    require_sync = (fun _ -> change ());
    run_catchup_to_target = (fun ~target_epoch:_ ~reason:_ -> change (); Lwt.return_unit);
    quarantine_active = (fun () -> false); quarantine_reason = (fun () -> "");
    ahead_streak = (fun () -> 0); incr_ahead_streak = change;
  }

let probe_peer head address = Octra_consensus.C_driver.{
  responder_addr = address;
  responder_head_epoch = head;
  state_root = Some base_root;
}

let test_probe_keeps_owner () =
  let active = ref false and changes = ref 0 in
  let state = { (probe_state active (fun () -> incr changes)) with
    query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ ->
      Lwt.bind (Lwt.pause ()) (fun () -> active := true; Lwt.return []));
  } in
  run (Health.run probe_config state);
  assert_true "probe cannot alter active catchup" (!active && !changes = 0)

let test_probe_snapshot_cause () =
  List.iter (fun gap ->
    let active = ref false and need = ref None and starts = ref 0 in
    let target = Int64.of_int (10 + gap) in
    let state = { (probe_state active (fun () -> ())) with
      query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ ->
        Lwt.return [probe_peer target "left"; probe_peer target "right"]);
      require_sync = (fun value -> need := Some value);
      run_catchup_to_target = (fun ~target_epoch:_ ~reason:_ ->
        incr starts; Lwt.return_unit);
    } in
    run (Health.run probe_config state);
    let module N = Octra_node_runtime.Sync_need in
    assert_true "snapshot cause is not missing range"
      (if gap > 4096 then match !need with
        | Some value -> N.valid value && value.cause = N.Range
          && value.head = 10 && value.epoch = 11 && value.target = Some target && !starts = 0
        | None -> false
      else !need = None && !starts = 1)
  ) [4096; 4097; 7529]

let test_root_read_owner () =
  List.iter (fun changed ->
    let active = ref false and effects = ref 0 and reads = ref 0 in
    let root, wake = Lwt.wait () in
    let state = { (probe_state active (fun () -> incr effects)) with
      query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ ->
        Lwt.return [probe_peer 10L "left"; probe_peer 10L "right"]);
      read_local_root_raw = (fun () -> incr reads; root);
    } in
    let work = Health.run probe_config state in
    assert_true "probe did not await root" (!reads = 1 && Lwt.is_sleeping work);
    active := changed;
    Lwt.wakeup wake base_root;
    run work;
    assert_true "root read crossed owner change" (if changed then !effects = 0 else !effects > 0)
  ) [false; true]

let test_root_drain_owner () =
  List.iter (fun change_owner ->
    let active = ref false and wakes = ref 0 in
    let state = { (probe_state active (fun () -> ())) with
      query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ ->
        Lwt.return [probe_peer 10L "left"; probe_peer 10L "right"]);
      drain_pending_finalized = (fun () ->
        Lwt.bind (Lwt.pause ()) (fun () ->
          if change_owner then active := true;
          Lwt.return_unit));
      wake_ready = (fun () -> incr wakes; Lwt.return_unit);
    } in
    run (Health.run probe_config state);
    assert_true "root wake respects new owner"
      (!wakes = if change_owner then 0 else 1)
  ) [false; true]

let test_ready_drain_owner () =
  List.iter (fun change_owner ->
    let active = ref false and wakes = ref 0 in
    let state = { (probe_state active (fun () -> ())) with
      query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ ->
        Lwt.return [probe_peer 9L "left"; probe_peer 9L "right"]);
      drain_pending_finalized = (fun () ->
        Lwt.bind (Lwt.pause ()) (fun () ->
          if change_owner then active := true;
          Lwt.return_unit));
      wake_ready = (fun () -> incr wakes; Lwt.return_unit);
    } in
    run (Health.run probe_config state);
    assert_true "ready wake respects new owner"
      (!wakes = if change_owner then 0 else 1)
  ) [false; true]

let test_http_probe_owner () =
  List.iter (fun change_owner ->
    let active = ref false and starts = ref 0 in
    let state = { (probe_state active (fun () -> ())) with
      run_catchup_to_target = (fun ~target_epoch:_ ~reason:_ ->
        incr starts; Lwt.return_unit);
    } in
    let http_target () =
      Lwt.bind (Lwt.pause ()) (fun () ->
        if change_owner then active := true;
        Lwt.return_some 12L) in
    run (Health.run ~http_target probe_config state);
    assert_true "HTTP probe respects new owner"
      (!starts = if change_owner then 0 else 1)
  ) [false; true]

let test_probe_saved_target () =
  let check reason ~http ~queued expected =
    let active = ref false and target = ref None in
    let state = { (probe_state active (fun () -> ())) with
      catchup_next_target = (fun () -> queued);
      quarantine_active = (fun () -> true);
      quarantine_reason = (fun () -> reason);
      run_catchup_to_target = (fun ~target_epoch ~reason:_ ->
        target := Some target_epoch; Lwt.return_unit);
    } in
    run (Health.run ~http_target:(fun () -> Lwt.return http)
      probe_config state);
    assert_true "probe did not use verified target" (!target = expected)
  in
  check "finalized_bundle_unavailable:12" ~http:(Some 12L)
    ~queued:(Some 14L) (Some 14L);
  check "rule anchor unreadable epoch = 8" ~http:None
    ~queued:(Some 14L) (Some 14L);
  check "operator hold" ~http:(Some 14L)
    ~queued:(Some 15L) None

let test_soft_probe_owner () =
  List.iter (fun change_owner ->
    let active = ref false and starts = ref 0 in
    let state = { (probe_state active (fun () -> ())) with
      query_epoch_root = (fun ~epoch_id:_ ~timeout_seconds:_ ->
        Lwt.return [probe_peer 12L "left"; probe_peer 12L "right"]);
      drain_pending_finalized = (fun () ->
        Lwt.bind (Lwt.pause ()) (fun () ->
          if change_owner then active := true;
          Lwt.return_unit));
      run_catchup_to_target = (fun ~target_epoch:_ ~reason:_ ->
        incr starts; Lwt.return_unit);
    } in
    run (Health.run probe_config state);
    assert_true "soft probe respects new owner"
      (!starts = if change_owner then 0 else 1)
  ) [false; true]

let test_repair_probe_owner () =
  List.iter (fun stop_at ->
    let active = ref false and queries = ref 0 and repairs = ref 0 in
    let state = { (probe_state active (fun () -> ())) with
      query_epoch_root = (fun ~epoch_id ~timeout_seconds:_ ->
        Lwt.bind (Lwt.pause ()) (fun () ->
          incr queries;
          if epoch_id = 10L && !queries = 1 then
            let peer address = Octra_consensus.C_driver.{
              responder_addr = address;
              responder_head_epoch = 9L;
              state_root = None;
            } in
            Lwt.return [peer "left"; peer "right"]
          else if epoch_id = 10L then begin
            if stop_at = "current" then active := true;
            Lwt.return []
          end else begin
            if stop_at = "target" then active := true;
            Lwt.return [probe_peer 9L "left"; probe_peer 9L "right"]
          end));
      root_consensus_quorum = (fun ~epoch_id ~root:_ _ -> epoch_id = 9L);
      quarantine_active = (fun () -> true);
    } in
    let repair ~target_epoch:_ ~target_root:_ ~required:_
        ~current_root_quorum:_ =
      incr repairs;
      Lwt.return_true in
    run (Health.run ~repair probe_config state);
    assert_true "repair respects new owner"
      (!repairs = if stop_at = "none" then 1 else 0)
  ) ["none"; "current"; "target"]

let test_replay_rechecks_owner () =
  let module R = Octra_node_runtime.Consensus_finalized_replay in
  List.iter (fun change_head ->
    let head = ref 10 in
    let active = ref false in
    let calls = ref 0 in
    let finality = Option.get (record ()).finality in
    let state = R.{
      committed_head_epoch = (fun () -> !head);
      catchup_active = (fun () -> !active);
      quarantine_active = (fun () -> false);
      find_finalized = (fun epoch -> if epoch = 11 then
        Some (finality.finalize, finality.validator_set) else None);
      read_local_root_raw = (fun () ->
        if change_head then head := 11 else active := true;
        Lwt.return base_root);
      apply_finalized = (fun ~validator_set:_ _ -> incr calls; Lwt.return_unit);
    } in
    run (R.replay_while_safe state ~source:"test");
    assert_true "replay rechecks owner and head after read" (!calls = 0)
  ) [false; true]

let test_drain_stops_at_seen_head () =
  let module R = Octra_node_runtime.Consensus_finalized_replay in
  let head = ref 10 and visible = ref 11 in
  let state = R.{
    committed_head_epoch = (fun () -> !head);
    catchup_active = (fun () -> true);
    quarantine_active = (fun () -> false);
    find_finalized = (fun epoch ->
      if epoch > !visible then None else
      let saved = Option.get (record ~epoch:(Int64.of_int epoch) ()).finality in
      Some (saved.finalize, saved.validator_set));
    read_local_root_raw = (fun () -> Lwt.return base_root);
    apply_finalized = (fun ~validator_set:_ saved ->
      head := Int64.to_int saved.epoch_id;
      visible := 12;
      Lwt.return_unit);
  } in
  run (R.drain state);
  assert_true "drain stops at initially visible head" (!head = 11);
  run (R.drain state);
  assert_true "next drain consumes later arrival" (!head = 12)

let test_range_plan_defaults () =
  let probe ?(current = 10L) ?(allowed = true) now records =
    S.probe_target ~now ~head:10L ~current ~allowed records in
  assert_true "empty probe started catchup" (probe 1000. [] = None);
  assert_true "fresh next epoch started catchup" (probe 120. [record ()] = None);
  assert_true "old next epoch did not start" (probe 1000. [record ()] = Some 11L);
  assert_true "verified lag did not start"
    (probe 120. [record (); record ~epoch:12L ()] = Some 12L);
  assert_true "changed head started catchup"
    (probe ~current:11L 1000. [record ()] = None);
  assert_true "unsafe state started catchup"
    (probe ~allowed:false 1000. [record ()] = None);
  let next = S.next_target ~now:1000. ~initial:10L in
  let extended, short = S.next_window ~extended:false ~target:10L ~next:26L ~count:1 in
  assert_true "short extension not recorded" (extended && short);
  let extended, short = S.next_window ~extended ~target:26L ~next:26L ~count:16 in
  assert_true "full response retained short wait" (extended && not short);
  assert_true "fixed target grew"
    (next ~reason:"finalized_gap" ~target:10L ~epoch:10L ~epoch_ts:0. = 10L);
  assert_true "old verified head did not grow"
    (next ~reason:"lagging" ~target:10L ~epoch:10L ~epoch_ts:0. = 26L);
  assert_true "target moved before it was reached"
    (next ~reason:"lagging" ~target:26L ~epoch:20L ~epoch_ts:0. = 26L);
  assert_true "fresh head did not finish"
    (next ~reason:"lagging" ~target:26L ~epoch:20L ~epoch_ts:990. = 20L);
  let plan =
    S.range_plan
      ~env_timeout:None
      ~from_epoch:10L
      ~target_epoch:14L
  in
  assert_true "remain" (plan.remain = 5);
  assert_true "max epochs"
    (plan.max_epochs = 5);
  assert_true "timeout" (plan.timeout_seconds = 8.0);
  assert_true "log progress" (plan.log_progress = false);
  let near =
    S.range_plan ~env_timeout:None ~from_epoch:10L ~target_epoch:10L
  in
  assert_true "range stops at old target"
    (near.max_epochs = 1)

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
    query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
        ~validate:_ ~prove:_ ->
      incr attempts;
      if !attempts = 3 then Lwt.return_some (chunk ())
      else Lwt.return_none);
http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
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
         ~validate:(fun _ -> true)
         ~prove:(fun _ -> true))
  in
  assert_true "query succeeded" (Option.is_some result);
  assert_true "attempts" (!attempts = 3);
  assert_true "sleeps" (!slept = 2)

let query_chunk_deps ?(local_root = base_root) ?(answer = Some (chunk ()))
    ?(quorum = true) events =
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
          ~validate ~prove ->
        add events "query_range";
        match answer with
        | Some response when validate response && (quorum || prove response) ->
          Lwt.return_some response
        | _ ->
          Lwt.return_none);
      http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
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

let test_probe_moving_head () =
  let records = [proved_record (); proved_record ~epoch:12L ~prev:next_root ();
    proved_record ~epoch:13L ~prev:next_root ()] in
  List.iter (fun (after, permitted, expected) ->
    let events = ref [] and current = ref 10L and allowed = ref true in
    let started = ref [] in
    let query = query_chunk_deps ~quorum:false
      ~answer:(Some (chunk ~records ())) events in
    let read = query.S.range_query.query_range in
    let query = { query with S.range_query = { query.range_query with
      query_range = (fun ~from_epoch ~max_epochs ~timeout_seconds ~validate ~prove ->
        let open Lwt.Syntax in
        let* response = read ~from_epoch ~max_epochs ~timeout_seconds ~validate ~prove in
        current := after;
        allowed := permitted;
        Lwt.return response) } } in
    run (S.probe_range query ~now:(fun () -> 1000.) ~head:10L
      ~current_head:(fun () -> !current) ~allowed:(fun () -> !allowed)
      ~run_catchup:(fun ~target_epoch ~reason ->
        assert_true "probe changed reason" (reason = "verified_lag");
        started := target_epoch :: !started;
        Lwt.return_unit));
    assert_true "probe lost verified progress" (!started = expected)
  ) [10L, true, [13L]; 11L, true, [13L]; 12L, true, [13L];
     13L, true, []; 14L, true, []; 9L, true, []; 11L, false, []]

let test_probe_rejects_range () =
  List.iter (fun records ->
    let events = ref [] and started = ref false in
    let query = query_chunk_deps ~quorum:false
      ~answer:(Some (chunk ~records ())) events in
    run (S.probe_range query ~now:(fun () -> 1000.) ~head:10L
      ~current_head:(fun () -> 11L) ~allowed:(fun () -> true)
      ~run_catchup:(fun ~target_epoch:_ ~reason:_ ->
        started := true;
        Lwt.return_unit));
    assert_true "unverified range started catchup" (not !started)
  ) [[]; [{ (proved_record ()) with finality = None }];
     [proved_record ~prev:wrong_root ()]]

let test_query_peer_proof () =
  let events = ref [] in
  let bad = { (record ()) with finality = None } in
  let answer = Some (chunk ~records:[bad] ()) in
  let step =
    run
      (S.query_chunk
         (query_chunk_deps ~answer ~quorum:false events)
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"proof")
  in
  match step with
  | S.Query_failed _ -> ()
  | S.Query_chunk _ -> failwith "peer range lacks finality proof"

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
  assert_true "retry and early HTTP waits"
    (List.length (List.filter (( = ) "query_sleep") events) = 3)

let test_http_reward () =
  let first = record () in
  let check = S.check_qcs
    ~chain_id:"octra-test"
    ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
    ~start_txid:4L in
  assert_true "unproved reward rejected by single source"
    (Result.is_error (check [first]));
  let changed = record ~reward_creator:"oct_other" () in
  assert_true "changed unproved reward rejected by single source"
    (Result.is_error (check [changed]));
  let signed = proved_record () in
  let unproved = record ~epoch:12L ~prev:next_root () in
  assert_true "only proved reward prefix retained"
    (match check [signed; unproved] with
     | Ok [value] -> value = signed
     | _ -> false);
  let altered = { signed with reward_source = changed.reward_source } in
  assert_true "signed reward mismatch rejected"
    (Result.is_error (check [altered]));
  let module Reward = Octra_node_runtime.Consensus_reward_attribution in
  let proof = Option.get first.finality in
  let supplied = first.reward_source |> Option.get |> Reward.of_source |> Result.get_ok in
  assert_true "historical apply rule unchanged"
    (Reward.bind_finality ~validator_set:proof.validator_set proof.finalize supplied
     = Ok supplied)

let test_http_qc () =
  let check = S.check_qcs
    ~chain_id:"octra-test"
    ~expected_validator_set_hash:(fun _ -> Ok trusted_validator_set_hash)
    ~start_txid:4L in
  let first = proved_record () in
  let second = proved_record ~epoch:12L ~prev:next_root () in
  let changed = proved_record ~epoch:12L ~prev:next_root ~creator:"oct_other" () in
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
        ~validate ~prove:_ ->
      incr calls;
      add events "query_range";
      let response = chunk () in
      Lwt.return
        (if !calls > 1 && validate response then Some response else None));
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate:_ ->
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
  let events = snapshot events in
  assert_true ("http precedes second peer: " ^ String.concat "," events)
    (events =
     ["query_root"; "query_range"; "query_sleep"; "http";
      "query_sleep"; "query_range"])

let test_query_chunk_http_first () =
  let events = ref [] in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate ->
      add events "http";
      let response = chunk ~records:[proved_record ()] () in
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
  let events = snapshot events in
  assert_true ("http precedes peer retry: " ^ String.concat "," events)
    (events = ["query_root"; "query_range"; "query_sleep"; "http"])

let test_http_early () =
  let events = ref [] in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{
    sleep = (fun seconds ->
      if seconds = 1.0 then Lwt.return_unit else Lwt_unix.sleep seconds);
    query_range = (fun ~from_epoch:_ ~max_epochs:_ ~timeout_seconds:_
        ~validate:_ ~prove:_ ->
      add events "peer";
      let open Lwt.Syntax in
      let* () = Lwt_unix.sleep 0.05 in
      add events "peer_done";
      Lwt.return_none);
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate ->
      add events "http";
      let response = chunk ~records:[proved_record ()] () in
      Lwt.return (if validate response then Some response else None));
  } in
  let step =
    run
      (S.query_chunk
         { deps with range_query }
         ~target_epoch:12L
         ~from_epoch:11L
         ~reason:"early")
  in
  assert_true "HTTP did not win the first wait"
    (match step with S.Query_chunk _ -> true | _ -> false);
  assert_true "HTTP waited for the peer"
    (not (has "peer_done" (snapshot events)))

let test_query_chunk_http_prefix () =
  let events = ref [] in
  let checks = ref 0 in
  let first = proved_record () in
  let changed = proved_record ~epoch:12L ~prev:next_root ~creator:"oct_other" () in
  let deps = query_chunk_deps ~answer:None events in
  let range_query = S.{ deps.range_query with
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate ->
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
        ~validate ~prove:_ ->
      incr calls;
      let response = chunk () in
      Lwt.return
        (if !calls > 1 && validate response then Some response else None));
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate ->
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
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate ->
      incr calls;
      let response =
        if !calls = 1 then
          chunk ~records:[{ (record ()) with finality = None }] ()
        else chunk ~records:[proved_record ()] ()
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
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate ->
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

let test_worker_stop () =
  let events = ref [] in
  let point = apply_point ~epoch:10L () in
  let apply = { (apply_deps events ~head_before_record:10 ~point) with
    apply_record = (fun _ -> Lwt.fail (Octra_core.Private_ledger.Worker_stopped "test")) } in
  let run_one ~target_epoch:_ ~reason:_ ~finish_success:_ ~fail_catchup:_ =
    S.apply_chunk_records apply ~prev_eic:"eic0" ~start_txid:4L
      (chunk ~records:[record ()] ()) |> Lwt.map ignore in
  let stopped = try
    run (S.run (deps events) ~run_one ~target_epoch:11L ~reason:"peer");
    false
  with Octra_core.Private_ledger.Worker_stopped reason -> reason = "test" in
  assert_true "worker stop became catchup failure" stopped;
  let events = snapshot events in
  assert_true "worker stop lost certificate" (has "write:11" events);
  assert_true "worker stop promoted certificate" (not (has "promote:11" events));
  assert_true "worker stop advanced height" (not (has "advance:12" events));
  assert_true "worker stop created quarantine"
    (not (has "mark_quarantine:catchup_failed:unexpected" events))

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

let test_range_grows_head () =
  let first = proved_record () in
  let second = proved_record ~epoch:12L ~prev:next_root () in
  let head = ref 10 in
  let root = ref base_root in
  let eic = ref "eic0" in
  let txid = ref 4L in
  let events = ref [] in
  let point = apply_point ~epoch:10L ~root:base_root ~eic:"eic0" ~txid:4L () in
  let record_apply = S.{ (apply_deps events ~head_before_record:10 ~point) with
    head_before_record = (fun () -> !head);
    point_source = {
      head_epoch = (fun () -> !head);
      read_root = (fun () -> Lwt.return !root);
      cached_head = (fun () -> {
        S.cached_root = !root;
        cached_eic = Some !eic;
      });
      next_txid = (fun () -> !txid);
    };
    apply_record = (fun validated ->
      head := Int64.to_int validated.record.epoch_id;
      root := validated.record.state_root;
      eic := validated.expected_eic;
      txid := validated.expected_txid;
      add events (Printf.sprintf "apply:%d" !head);
      Lwt.return_unit);
  } in
  let deps = S.{ (chunk_apply_deps events ~head:10 ~point) with
    current_head = (fun () -> Int64.of_int !head);
    record_apply;
  } in
  let step =
    run (S.apply_chunk_gate deps ~target_epoch:11L ~start_head:10L
      ~from_epoch:11L ~reason:"growth" (chunk ~records:[first; second] ()))
  in
  assert_true "verified range did not advance beyond old target"
    (match step with S.Chunk_continue _ -> true | _ -> false);
  assert_true "second certified epoch was not applied"
    (!head = 12 && !root = second.state_root &&
     has "apply:11" (snapshot events) && has "apply:12" (snapshot events))

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
          ~validate:_ ~prove:_ ->
        add events "query_range";
        Lwt.finalize
          (fun () -> waiting)
          (fun () ->
            query_cancelled := true;
            add events "query_cancelled";
            Lwt.return_unit));
      http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
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

let test_catchup_ready_flow () =
  let module Actor = Octra_node_runtime.Set_actor in
  let module Fold = Octra_core.Set_fold in
  let module State = Octra_node_runtime.Consensus_runtime_state in
  List.iter (fun valid ->
    let recd = record ~root:(String.make 32 'w') () in
    let checked = validated_or_fail ~head_before_record:10 recd in
    let point = apply_point ~epoch:11L
      ~root:(if valid then recd.state_root else String.make 32 'z')
      ~eic:checked.expected_eic ~txid:checked.expected_txid () in
    let events = ref [] and head = ref 10 and sent = ref 0 in
    let state = State.create () and fold = ref Fold.empty in
    ignore (State.enter_quarantine state ~epoch:10 ~reason:"catchup_failed:lagging");
    let actor = Actor.create Actor.{
      sample = (fun () -> {
        epoch = Int64.of_int (!head + 1); active = false;
        bonded = if State.attested_head state !head && not (State.quarantine_active state)
          then Ok true else Error "sync pending";
      });
      read = (fun ~epoch:_ -> Lwt.return_ok (Fold.receipt ~address:"octA" !fold));
      peers = (fun () -> 1);
      send = (fun ~epoch action ->
        assert_true "rejoin sent a non-pulse" (action = Actor.Pulse);
        incr sent;
        fold := Fold.note_pulse Fold.standard ~epoch ~active:false ~address:"octA" !fold
          |> Result.get_ok;
        Lwt.return_ok ());
      warn = (fun _ reason -> if reason <> "sync pending" then failwith reason);
    } in
    let settle () = run (Lwt_list.iter_s (fun _ -> Lwt.pause ()) (List.init 20 Fun.id)) in
    let wake () = ignore (Actor.wake actor ~head:!head) in
    wake (); settle ();
    assert_true "unverified node sent ready" (!sent = 0);
    let outer = S.{ (deps events) with
      committed_head_epoch = (fun () -> !head);
      read_local_root = (fun () -> Lwt.return recd.state_root);
      set_state_attested = (fun ~head ~root -> State.set_state_attested state ~head ~root; wake ());
      clear_quarantine = (fun evidence -> ignore (State.clear_quarantine state ~evidence));
      wake_ready = (fun () -> wake (); Lwt.return_unit);
    } in
    let target = target_deps ~answer:(Some (chunk ~records:[recd] ())) events ~head:10 ~point in
    let prior_apply = target.apply.record_apply.apply_record in
    let target = S.{ target with
      head_epoch = (fun () -> !head);
      apply = {target.apply with
        gap_active = (fun () -> not valid);
        record_apply = {target.apply.record_apply with apply_record = (fun record ->
          let open Lwt.Syntax in
          let* () = prior_apply record in
          head := 11;
          Lwt.return_unit)};
      };
    } in
    run (S.run_with_target outer ~target ~target_epoch:11L ~reason:"lagging");
    settle ();
    assert_true "ready eligibility differs from checked root" ((!sent = 1) = valid);
    if valid then begin
      for next = 12 to 75 do
        head := next;
        State.set_state_attested state ~head:next ~root:recd.state_root;
        wake (); settle ()
      done;
      assert_true "automatic pulses did not restore membership eligibility"
        (Fold.allows Fold.standard ~start:0L ~source:76L ~address:"octA" !fold)
    end;
    run (Actor.shutdown actor)) [false; true]

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

let test_anchor_recovery () =
  let module State = Octra_node_runtime.Consensus_runtime_state in
  let recd = record ~root:(String.make 32 'x') () in
  let validated = validated_or_fail ~head_before_record:10 recd in
  let point = apply_point ~epoch:11L ~root:recd.state_root
    ~eic:validated.S.expected_eic ~txid:validated.expected_txid () in
  let check reason expected verified =
    let events = ref [] in
    let state = State.create () in
    ignore (State.enter_quarantine state ~epoch:10 ~reason);
    assert_true "peer root cleared a read failure"
      (State.root_recovery_evidence ~reason ~evidence:"in_sync"
        ~consensus_proved:true = None);
    let base = deps events in
    let target = target_wiring ~answer:(Some (chunk ~records:[recd] ()))
      events ~head:10 ~point in
    let input = S.{ base with
      committed_head_epoch = target.head_epoch;
      set_state_attested = (fun ~head ~root ->
        State.set_state_attested state ~head ~root);
      clear_quarantine = (fun evidence ->
        ignore (State.clear_quarantine state ~evidence));
    } in
    if verified then
      run (S.run_wired input
        ~target
        ~target_epoch:11L ~reason:"checked_chain")
    else
      run (S.run input ~run_one:(fun ~target_epoch:_ ~reason:_
        ~finish_success ~fail_catchup:_ -> finish_success (unproved "peer"))
        ~target_epoch:11L ~reason:"peer");
    assert_true "catchup changed the wrong quarantine"
      (State.quarantine_active state <> (expected && verified)) in
  List.iter (fun verified ->
    check "rule anchor unreadable epoch = 1504440 reason = epoch log entry missing"
      true verified;
    check "finalized_bundle_unavailable:11" true verified;
    check "finalized_bundle_unavailable:12" false verified;
    check "finalized_bundle_unavailable:invalid" false verified;
    check "rule anchor mismatch epoch = 1504440" false verified;
    check "operator hold" false verified) [false; true]

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
    http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
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
        http_range = (fun ~budget:_ ~from_epoch:_ ~max_epochs:_ ~validate:_ -> Lwt.return_none);
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
  "finish keeps owner", test_finish_keeps_owner;
  "wake keeps new owner", test_wake_keeps_new_owner;
  "finish stashed arrival", test_finish_stashed_arrival;
  "startup delay has no owner", test_startup_delay_has_no_owner;
  "finish queued during drain", test_finish_queued_during_drain;
  "startup owns no run", test_startup_owns_no_run;
  "startup hold waits", test_startup_hold_waits;
  "poll rechecks owner", test_poll_rechecks_owner;
  "probe keeps owner", test_probe_keeps_owner;
  "root read owner", test_root_read_owner;
  "root drain owner", test_root_drain_owner;
  "ready drain owner", test_ready_drain_owner;
  "HTTP probe owner", test_http_probe_owner;
  "probe saved target", test_probe_saved_target;
  "probe snapshot cause", test_probe_snapshot_cause;
  "soft probe owner", test_soft_probe_owner;
  "repair probe owner", test_repair_probe_owner;
  "replay rechecks owner", test_replay_rechecks_owner;
  "drain stops at seen head", test_drain_stops_at_seen_head;
  "reject stripped parent", test_reject_stripped_parent;
  "active call queues only", test_active_call_queues_only;
  "success finishes and wakes", test_success_finishes_and_wakes;
  "unverified attestation hold", test_unverified_attestation_hold;
  "anchor recovery", test_anchor_recovery;
  "continue queued target", test_continue_queued_target;
  "failure quarantine clear", test_failure_quarantine_clear;
  "throw quarantine release",
    test_throw_quarantine_release;
  "observer drain no wake", test_observer_drain_no_wake;
  "range plan defaults", test_range_plan_defaults;
  "probe moving head", test_probe_moving_head;
  "probe rejects range", test_probe_rejects_range;
  "query progress decisions", test_query_progress_decisions;
  "range retry success", test_range_retry_success;
  "query chunk success", test_query_chunk_success;
  "query peer proof", test_query_peer_proof;
  "query chunk failure", test_query_chunk_failure;
  "http reward", test_http_reward;
  "http qc", test_http_qc;
  "query chunk http reject", test_query_chunk_http_reject;
  "query chunk http first", test_query_chunk_http_first;
  "http early", test_http_early;
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
  "worker stop", test_worker_stop;
  "apply chunk records skip", test_apply_chunk_records_skip;
  "apply chunk records retry", test_apply_chunk_records_retry;
  "apply chunk gate continue", test_apply_chunk_gate_continue;
  "range grows head", test_range_grows_head;
  "apply chunk gate retry", test_apply_chunk_gate_retry;
  "chunk continuity fail", test_chunk_continuity_fail;
  "run target already in sync", test_run_target_already_in_sync;
  "run target continue", test_run_target_continue;
  "catchup ready flow", test_catchup_ready_flow;
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
  let failed = ref 0 in
  List.iter (fun (name, f) ->
    try f () with exn ->
      incr failed;
      Printf.eprintf "test = %s status = fail error = %s\n%!"
        name (Printexc.to_string exn)
  ) tests;
  if !failed > 0 then failwith (Printf.sprintf "failed tests = %d" !failed);
  print_endline "status = pass test = node_runtime_consensus_catchup_shell"