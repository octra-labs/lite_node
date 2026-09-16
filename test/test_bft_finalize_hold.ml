(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C_driver = Octra_consensus.C_driver
module C_engine = Octra_consensus.C_engine
module C_hash = Octra_consensus.C_hash
module C_types = Octra_consensus.C_types
module C_vote_log = Octra_consensus.C_vote_log

let fail reason =
  failwith ("test_bft_finalize_hold: " ^ reason)

let expect reason condition =
  if not condition then fail reason

let root value =
  String.make 32 value

let header ?(epoch_id = 1L) chain_id address =
  C_types.{
    proto_version = proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = root '\x00';
    tx_list_hash = C_engine.tx_list_hash_for_header [];
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = root '\x01';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = address;
    txid_hi = 0L;
    ts = 1.0;
  }

let finalize header =
  let proposal_id = C_hash.proposal_id header in
  C_types.{
    chain_id = header.chain_id;
    epoch_id = header.epoch_id;
    commit_round = 0;
    header;
    proposal_id;
    precommits = [];
    parent_commit = None;
  }

let driver ?(start_height = 1L) on_finalized =
  let chain_id = "octra-test-finalize-hold" in
  let address = "octFinalizeHold" in
  let pubkey = String.make 32 '\x02' in
  let swarm =
    Octra_net.P2p_swarm.create
      Octra_net.P2p_swarm.{
        listen_port = 0;
        chain_id;
        node_id = Octra_net.P2p_handshake.node_id_of_pubkey pubkey;
        node_addr = address;
        pubkey_raw = pubkey;
        consensus_config_hash = root '\x03';
        binary_hash = root '\x04';
        require_binary_hash = false;
        upgrade_plan = None;
        profile_plan = [];
        allowed_pubkeys = [];
        bootstrap_peers = [];
        max_peers = 1;
        sign_fn = (fun _ -> String.make 64 '\x05');
        best_epoch_fn = (fun () -> 0L);
        best_root_fn = (fun () -> root '\x00');
      }
  in
  let validator_set =
    C_types.make_validator_set [C_types.{ address; pubkey }]
  in
  let config =
    C_driver.{
      chain_id;
      my_addr = address;
      sign_fn = (fun _ -> String.make 64 '\x05');
      verify_fn = (fun _ _ _ -> false);
      role_can_vote = (fun () -> true);
      can_vote = (fun () -> false);
      execute_fn = (fun _ -> true);
      verify_proposal = (fun _ ->
        Lwt.return Octra_consensus.C_driver.Proposal_accept);
      verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
      on_finalized;
      make_proposal = (fun _ -> Lwt.return_none);
      before_precommit_broadcast =
        (fun ~epoch_id:_ ~round:_ ~proposal_id:_
          ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ ->
          Lwt.return_true);
      lookup_epoch_root = (fun _ -> None);
      local_head_epoch = (fun () -> 0L);
      lookup_bundle = (fun _ -> None);
      lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
      on_resource_attestation = (fun _ -> Lwt.return_unit);
      scheduled_validator_set_config = None;
      load_scheduled_validator_set_config = (fun () -> Lwt.return_none);
      resource_committee_config = None;
    }
  in
  C_driver.create
    ~config
    ~validator_set
    ~swarm
    ~start_height
    ~sync_log:(Octra_consensus.C_sync_log.memory ())
    ~relief_log:(Octra_consensus.C_relief_log.memory ())
    ~vote_log:(Octra_consensus.C_vote_log.memory ())

let test_finalize_holds_height () =
  let callback_started = ref false in
  let release, wake = Lwt.wait () in
  let driver =
    driver (fun ~validator_set:_ _ ->
      callback_started := true;
      release)
  in
  let finalized =
    finalize (header driver.config.chain_id driver.config.my_addr)
  in
  driver.running <- true;
  driver.epoch_start_mono <-
    Int64.sub (Mtime_clock.elapsed_ns ()) 60_000_000_000L;
  expect "finalize was not retained"
    (C_engine.emit_finalized ~send:false driver.engine finalized);
  let pending = C_driver.process_outputs driver in
  expect "callback did not start" !callback_started;
  expect "driver callback did not block" (Lwt.state pending = Lwt.Sleep);
  expect "height advanced before callback"
    (Int64.equal driver.engine.state.height 1L);
  Lwt.wakeup_later wake ();
  Lwt_main.run pending;
  expect "height did not advance after callback"
    (Int64.equal driver.engine.state.height 2L)

let test_finalize_keeps_catchup_height () =
  let driver =
    driver
      ~start_height:5L
      (fun ~validator_set:_ _ -> Lwt.return_unit)
  in
  let parent_header =
    header
      ~epoch_id:0L
      driver.config.chain_id
      driver.config.my_addr
  in
  let parent_proposal_id = C_hash.proposal_id parent_header in
  let vote =
    C_types.{
      chain_id = driver.config.chain_id;
      epoch_id = 0L;
      round = 0;
      vote_type = Precommit;
      proposal_id = parent_proposal_id;
      validator = driver.config.my_addr;
      signature = String.make 64 '\x06';
    }
  in
  ignore (C_vote_log.keep driver.vote_log vote |> Result.get_ok);
  let parent =
    C_types.{
      certificate = {
        chain_id = driver.config.chain_id;
        epoch_id = 0L;
        commit_round = 0;
        header = parent_header;
        proposal_id = parent_proposal_id;
        precommits = [];
      };
      validator_set = driver.engine.vs;
    }
  in
  let finalized =
    { (finalize (header driver.config.chain_id driver.config.my_addr)) with
      parent_commit = Some parent;
    }
  in
  let notice = ref None in
  C_driver.set_fold_handler driver (fun ~next_epoch event ->
    notice := Some (next_epoch, event));
  expect "old finalize was not retained"
    (C_engine.emit_finalized ~send:false driver.engine finalized);
  C_engine.realign_round driver.engine 3;
  let generation = driver.engine.generation in
  Lwt_main.run (C_driver.process_outputs driver);
  expect "catchup height regressed"
    (Int64.equal driver.engine.state.height 5L);
  expect "catchup round restarted" (driver.engine.state.round = 3);
  expect "catchup generation changed" (driver.engine.generation = generation);
  expect "old finalize was not acknowledged"
    (Option.is_none driver.engine.pending_finalized);
  expect "catchup finalize lost fold notice"
    (!notice = Some (2L, Some (vote, parent)))

let test_fold_retention () =
  let driver = driver (fun ~validator_set:_ _ -> Lwt.return_unit) in
  let first = finalize (header driver.config.chain_id driver.config.my_addr) in
  let vote = C_types.{
    chain_id = first.chain_id;
    epoch_id = first.epoch_id;
    round = first.commit_round;
    vote_type = Precommit;
    proposal_id = first.proposal_id;
    validator = driver.config.my_addr;
    signature = String.make 64 '\x06';
  } in
  ignore (C_vote_log.keep driver.vote_log vote |> Result.get_ok);
  let parent = C_types.{
    validator_set = driver.engine.vs;
    certificate = certificate_of_finalize first;
  } in
  let notice = ref None in
  C_driver.set_fold_handler driver (fun ~next_epoch event ->
    notice := Some (next_epoch, event));
  expect "first finalize was not retained"
    (C_engine.emit_finalized ~send:false driver.engine first);
  Lwt_main.run (C_driver.process_outputs driver);
  let saved () =
    C_vote_log.find_statement driver.vote_log
      ~chain_id:vote.chain_id ~validator:vote.validator
      ~epoch_id:vote.epoch_id ~round:vote.round
      ~vote_type:vote.vote_type ~proposal_id:vote.proposal_id
    |> Result.get_ok
  in
  expect "parent vote pruned before child finalization" (saved () = Some vote);
  let second = {
    (finalize (header ~epoch_id:2L driver.config.chain_id driver.config.my_addr))
    with parent_commit = Some parent;
  } in
  expect "second finalize was not retained"
    (C_engine.emit_finalized ~send:false driver.engine second);
  Lwt_main.run (C_driver.process_outputs driver);
  expect "finalized child lost parent appeal"
    (!notice = Some (3L, Some (vote, parent)));
  expect "parent vote retained beyond child finalization" (saved () = None)

let () =
  Mirage_crypto_rng_unix.use_default ();
  test_finalize_holds_height ();
  test_finalize_keeps_catchup_height ();
  test_fold_retention ();
  print_endline "status = pass test = bft_finalize_hold"