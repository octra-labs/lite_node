(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

module Driver = C_driver

let assert_msg cond msg =
  if not cond then failwith msg

let validator address =
  C_types.{ address; pubkey = String.make 32 '\x01' }

let header ~chain_id ~epoch_id ~creator_addr =
  C_types.{
    proto_version = C_types.proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = String.make 32 '\x02';
    tx_list_hash = String.make 32 '\x03';
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = String.make 32 '\x04';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr;
    txid_hi = 0L;
    ts = 0.0;
  }

let has_vote outputs =
  List.exists (function C_engine.SendVote _ -> true | _ -> false) outputs

let has_propose outputs =
  List.exists (function C_engine.SendPropose _ -> true | _ -> false) outputs

let has_finalize outputs =
  List.exists (function C_engine.SendFinalize _ | C_engine.Finalized _ -> true | _ -> false) outputs

let check_future_validator_engine_boundary () =
  let chain_id = "transition-test" in
  let a = validator "octAAAA" in
  let b = validator "octBBBB" in
  let c = validator "octCCCC" in
  let vs2 = C_types.make_validator_set [a; b] in
  let vs3 = C_types.make_validator_set [a; b; c] in
  let node_c = C_engine.create
    ~chain_id
    ~my_addr:c.address
    ~validator_set:vs2
    ~start_height:10L
    ~can_vote:(fun () -> true) in
  let h10 = header ~chain_id ~epoch_id:10L ~creator_addr:a.address in
  C_engine.do_propose node_c h10 [] ~sign_fn:(fun _ -> String.make 64 '\x05');
  let before_self = C_engine.drain_outputs node_c in
  assert_msg (not (has_propose before_self)) "future validator cannot propose before activation";
  assert_msg (not (has_vote before_self)) "future validator cannot vote before activation";
  let proposal = C_types.{
    chain_id;
    epoch_id = 10L;
    round = 0;
    valid_round = None;
    header = h10;
    tx_hashes = [];
    parent_commit = None;
    proposer = a.address;
    signature = String.make 64 '\x06';
  } in
  C_engine.on_propose node_c proposal
    ~verify_fn:(fun _ _ _ -> true)
    ~execute_fn:(fun _ -> true)
    ~sign_fn:(fun _ -> String.make 64 '\x07');
  let before_remote = C_engine.drain_outputs node_c in
  assert_msg
    (not (has_vote before_remote))
    "future validator verifies proposal without voting";
  assert_msg
    (not (has_finalize before_remote))
    "future validator does not finalize alone before activation";
  C_engine.replace_validator_set node_c vs3;
  C_engine.start_height node_c 11L;
  ignore (C_engine.drain_outputs node_c);
  let h11 = header ~chain_id ~epoch_id:11L ~creator_addr:c.address in
  C_engine.do_propose node_c h11 [] ~sign_fn:(fun _ -> String.make 64 '\x08');
  let after_activation = C_engine.drain_outputs node_c in
  assert_msg (has_propose after_activation) "activated validator can propose";
  assert_msg (has_vote after_activation) "activated validator can vote"

let make_swarm ~chain_id ~addr =
  let pubkey_raw = String.make 32 '\x09' in
  let swarm_config = Octra_net.P2p_swarm.{
    listen_port = 0;
    chain_id;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey pubkey_raw;
    node_addr = addr;
    pubkey_raw;
    consensus_config_hash = String.make 32 '\x00';
    binary_hash = String.make 32 '\x00';
    require_binary_hash = false;
    upgrade_plan = None;
    profile = None;
    allowed_pubkeys = [];
    bootstrap_peers = [];
    max_peers = 0;
    sign_fn = (fun _ -> String.make 64 '\x09');
    best_epoch_fn = (fun () -> 0L);
    best_root_fn = (fun () -> String.make 32 '\x00');
  } in
  Octra_net.P2p_swarm.create swarm_config

let check_profile_offer () =
  Mirage_crypto_rng_unix.use_default ();
  let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let pubkey = Mirage_crypto_ec.Ed25519.pub_to_octets pub in
  let node_id = Octra_net.P2p_handshake.node_id_of_pubkey pubkey in
  let sign value = Mirage_crypto_ec.Ed25519.sign ~key:priv value in
  let offer =
    Octra_net.P2p_handshake.make_profile
      ~chain_id:"profile-test"
      ~node_id
      ~epoch:20L
      ~config_hash:(String.make 32 '\x0a')
      ~sign_fn:sign
  in
  let decoded =
    offer
    |> Octra_net.P2p_handshake.encode_profile
    |> Octra_net.P2p_handshake.decode_profile
  in
  assert_msg
    (Octra_net.P2p_handshake.validate_profile
       ~chain_id:"profile-test"
       ~node_id
       ~pubkey
       decoded
     = Ok ())
    "signed profile offer is valid";
  let changed = { decoded with config_hash = String.make 32 '\x0b' } in
  assert_msg
    (Result.is_error
       (Octra_net.P2p_handshake.validate_profile
          ~chain_id:"profile-test"
          ~node_id
          ~pubkey
          changed))
    "changed profile offer is rejected";
  assert_msg
    (Result.is_error
       (Octra_net.P2p_handshake.validate_profile
          ~chain_id:"other-chain"
          ~node_id
          ~pubkey
          decoded))
    "cross-chain profile offer is rejected";
  let changed_epoch = { decoded with epoch = 21L } in
  assert_msg
    (Result.is_error
       (Octra_net.P2p_handshake.validate_profile
          ~chain_id:"profile-test"
          ~node_id
          ~pubkey
          changed_epoch))
    "changed profile epoch is rejected"

let check_profile_switch () =
  let old_hash = String.make 32 '\x0c' in
  let new_hash = String.make 32 '\x0d' in
  let binary_hash = String.make 32 '\x10' in
  let plan = Some Octra_net.P2p_upgrade_plan.{
    activate_epoch = 20L;
    target_binary_hash = binary_hash;
    target_config_hash = new_hash;
    rollback = None;
  } in
  assert_msg
    (Octra_net.P2p_swarm.keep_profile
       ~target:new_hash
       ~current:old_hash
       ~offered:(Some new_hash))
    "offered target keeps an existing session";
  assert_msg
    (Octra_net.P2p_swarm.keep_profile
       ~target:new_hash
       ~current:new_hash
       ~offered:None)
    "target handshake keeps a new session";
  assert_msg
    (not
       (Octra_net.P2p_swarm.keep_profile
          ~target:new_hash
          ~current:old_hash
          ~offered:None))
    "old session without an offer is removed";
  let pubkey_raw = String.make 32 '\x0e' in
  let swarm =
    Octra_net.P2p_swarm.create
      Octra_net.P2p_swarm.{
        listen_port = 1;
        chain_id = "profile-switch-test";
        node_id = Octra_net.P2p_handshake.node_id_of_pubkey pubkey_raw;
        node_addr = "oct-profile";
        pubkey_raw;
        consensus_config_hash = old_hash;
        binary_hash;
        require_binary_hash = true;
        upgrade_plan = plan;
        profile = Some {
          epoch = 20L;
          config_hash = new_hash;
          profile_hash = String.make 32 '\x0f';
        };
        allowed_pubkeys = [];
        bootstrap_peers = [];
        max_peers = 0;
        sign_fn = (fun _ -> String.make 64 '\x00');
        best_epoch_fn = (fun () -> 19L);
        best_root_fn = (fun () -> String.make 32 '\x00');
      }
  in
  let before = Octra_net.P2p_swarm.make_my_hello swarm in
  assert_msg
    (Octra_net.P2p_swarm.hello_current swarm before)
    "current handshake is accepted before activation";
  let scheduled =
    Octra_net.P2p_upgrade_plan.handshake_hash
      ~epoch:19L
      ~config_hash:old_hash
      ~binary_hash
      ~require_binary_hash:true
      plan
  in
  assert_msg
    (String.equal before.consensus_config_hash scheduled)
    "profile switch preserves the scheduled handshake before activation";
  let first =
    Octra_net.P2p_swarm.switch_profile swarm ~epoch:20L
    |> Lwt_main.run
    |> Result.get_ok
  in
  assert_msg
    (String.equal first (String.make 32 '\x0f'))
    "profile switch returns the runtime profile";
  assert_msg
    (String.equal (Octra_net.P2p_swarm.config_hash swarm) new_hash)
    "profile switch changes the handshake hash";
  assert_msg
    (not (Octra_net.P2p_swarm.hello_current swarm before))
    "crossing handshake is rejected after activation";
  let after = Octra_net.P2p_swarm.make_my_hello swarm in
  assert_msg
    (Octra_net.P2p_swarm.hello_current swarm after)
    "current handshake is accepted after activation";
  let active =
    Octra_net.P2p_upgrade_plan.handshake_hash
      ~epoch:20L
      ~config_hash:new_hash
      ~binary_hash
      ~require_binary_hash:true
      plan
  in
  assert_msg
    (String.equal after.consensus_config_hash active)
    "profile switch advances the wire epoch at a shared boundary";
  let repeated =
    Octra_net.P2p_swarm.switch_profile swarm ~epoch:20L
    |> Lwt_main.run
    |> Result.get_ok
  in
  assert_msg
    (String.equal repeated first)
    "profile switch is idempotent"

let make_activation_driver ~chain_id ~my_addr ~initial_vs ~target_vs ~activate_epoch =
  let cfg = Driver.{
    activate_epoch;
    validator_set = target_vs;
    fingerprint = "transition-fp";
  } in
  let config = Driver.{
    chain_id;
    my_addr;
    sign_fn = (fun _ -> String.make 64 '\x01');
    verify_fn = (fun _ _ _ -> true);
    role_can_vote = (fun () -> true);
    can_vote = (fun () -> true);
    execute_fn = (fun _ -> true);
    verify_proposal = (fun _ ->
      Lwt.return Octra_consensus.C_driver.Proposal_accept);
    verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
    on_finalized = (fun ~validator_set:_ _ -> Lwt.return_unit);
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
      ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ ->
      Lwt.return_true);
    lookup_epoch_root = (fun _ -> None);
    local_head_epoch = (fun () -> 0L);
    lookup_bundle = (fun _ -> None);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
    on_resource_attestation = (fun _ -> Lwt.return_unit);
    scheduled_validator_set_config = Some cfg;
    load_scheduled_validator_set_config = (fun () -> Lwt.return_none);
    resource_committee_config = None;
  } in
  Driver.create
    ~config
    ~validator_set:initial_vs
    ~swarm:(make_swarm ~chain_id ~addr:my_addr)
    ~start_height:activate_epoch
    ~sync_log:(C_sync_log.memory ())
    ~relief_log:(C_relief_log.memory ())
    ~vote_log:(C_vote_log.memory ())

let check_scheduled_activation_restart_boundaries () =
  let chain_id = "transition-driver-test" in
  let a = validator "octAAAA" in
  let b = validator "octBBBB" in
  let c = validator "octCCCC" in
  let vs2 = C_types.make_validator_set [a; b] in
  let vs3 = C_types.make_validator_set [a; b; c] in
  let run target_epoch =
    let driver = make_activation_driver
      ~chain_id
      ~my_addr:c.address
      ~initial_vs:vs2
      ~target_vs:vs3
      ~activate_epoch:20L in
    let activated = ref false in
    Driver.set_validator_set_activation_handler
      driver
      (fun validator_set fingerprint ->
        activated :=
          validator_set.C_types.n = 3
          && fingerprint = "transition-fp";
        Lwt.return_unit);
    Lwt_main.run (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch);
    driver, !activated in
  let before, before_callback = run 19L in
  assert_msg (before.Driver.engine.C_engine.vs.n = 2)
    "scheduled validator set does not activate before target";
  assert_msg (not before_callback)
    "validator activation callback does not run before target";
  assert_msg (not (C_types.is_validator before.Driver.engine.C_engine.vs c.address))
    "future validator stays inactive before target";
  let exact, exact_callback = run 20L in
  assert_msg (exact.Driver.engine.C_engine.vs.n = 3)
    "scheduled validator set activates at target";
  assert_msg exact_callback
    "validator activation callback runs at target";
  assert_msg (C_types.is_validator exact.Driver.engine.C_engine.vs c.address)
    "future validator is active at target";
  assert_msg (Hashtbl.length exact.Driver.activated_validator_set_fingerprints = 1)
    "activation fingerprint recorded at target";
  let after, after_callback = run 21L in
  assert_msg (after.Driver.engine.C_engine.vs.n = 3)
    "scheduled validator set activates after restart past target";
  assert_msg after_callback
    "validator activation callback runs after restart past target";
  assert_msg (C_types.is_validator after.Driver.engine.C_engine.vs c.address)
    "future validator is active after restart past target"

let check_live_plan () =
  let chain_id = "transition-plan-test" in
  let a = validator "octAAAA" in
  let b = validator "octBBBB" in
  let c = validator "octCCCC" in
  let stale_vs = C_types.make_validator_set [a; b] in
  let live_vs = C_types.make_validator_set [a; b; c] in
  let stale = Driver.{
    activate_epoch = 20L;
    validator_set = stale_vs;
    fingerprint = "stale-plan";
  } in
  let live = Driver.{
    activate_epoch = 20L;
    validator_set = live_vs;
    fingerprint = "live-plan";
  } in
  let dynamic = ref (Some live) in
  let config = Driver.{
    chain_id;
    my_addr = c.address;
    sign_fn = (fun _ -> String.make 64 '\x01');
    verify_fn = (fun _ _ _ -> true);
    role_can_vote = (fun () -> true);
    can_vote = (fun () -> true);
    execute_fn = (fun _ -> true);
    verify_proposal = (fun _ ->
      Lwt.return Octra_consensus.C_driver.Proposal_accept);
    verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
    on_finalized = (fun ~validator_set:_ _ -> Lwt.return_unit);
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
      ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ ->
      Lwt.return_true);
    lookup_epoch_root = (fun _ -> None);
    local_head_epoch = (fun () -> 0L);
    lookup_bundle = (fun _ -> None);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
    on_resource_attestation = (fun _ -> Lwt.return_unit);
    scheduled_validator_set_config = Some stale;
    load_scheduled_validator_set_config = (fun () -> Lwt.return !dynamic);
    resource_committee_config = None;
  } in
  let driver =
    Driver.create
      ~config
      ~validator_set:stale_vs
      ~swarm:(make_swarm ~chain_id ~addr:c.address)
      ~start_height:20L
      ~sync_log:(C_sync_log.memory ())
      ~relief_log:(C_relief_log.memory ())
      ~vote_log:(C_vote_log.memory ())
  in
  let activated = ref [] in
  Driver.set_validator_set_activation_handler
    driver
    (fun _ fingerprint ->
      activated := fingerprint :: !activated;
      Lwt.return_unit);
  Lwt_main.run
    (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:20L);
  dynamic := None;
  Lwt_main.run
    (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:21L);
  assert_msg (driver.Driver.engine.C_engine.vs.n = 3)
    "live validator set cannot fall back to stale startup plan";
  assert_msg (List.rev !activated = ["live-plan"])
    "stale startup plan never activates after live plan"

let check_start_height_activates_target_set () =
  let chain_id = "transition-start-height-test" in
  let a = validator "octAAAA" in
  let b = validator "octBBBB" in
  let c = validator "octCCCC" in
  let initial = C_types.make_validator_set [a; b] in
  let target = C_types.make_validator_set [a; b; c] in
  let driver =
    make_activation_driver
      ~chain_id
      ~my_addr:c.address
      ~initial_vs:initial
      ~target_vs:target
      ~activate_epoch:20L
  in
  let activated = ref false in
  Driver.set_validator_set_activation_handler
    driver
    (fun validator_set _ ->
      activated := C_types.is_validator validator_set c.address;
      Lwt.return_unit);
  Lwt_main.run (Driver.start_height driver 20L);
  assert_msg !activated
    "start height activates the scheduled validator set";
  assert_msg
    (C_types.is_validator driver.Driver.engine.C_engine.vs c.address)
    "start height uses the target validator set"

let check_marked_set_repairs_runtime () =
  let chain_id = "transition-repair-test" in
  let a = validator "octAAAA" in
  let b = validator "octBBBB" in
  let c = validator "octCCCC" in
  let initial = C_types.make_validator_set [a; b] in
  let target = C_types.make_validator_set [a; b; c] in
  let driver =
    make_activation_driver
      ~chain_id
      ~my_addr:c.address
      ~initial_vs:initial
      ~target_vs:target
      ~activate_epoch:20L
  in
  Hashtbl.replace
    driver.Driver.activated_validator_set_fingerprints
    "transition-fp"
    true;
  let activations = ref 0 in
  Driver.set_validator_set_activation_handler
    driver
    (fun _ _ ->
      incr activations;
      Lwt.return_unit);
  Lwt_main.run
    (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:20L);
  assert_msg (!activations = 1)
    "marked validator set repairs stale runtime";
  assert_msg
    (C_types.is_validator driver.Driver.engine.C_engine.vs c.address)
    "marked validator set restores active membership";
  Lwt_main.run
    (Driver.maybe_activate_scheduled_validator_set driver ~target_epoch:20L);
  assert_msg (!activations = 1)
    "matching validator set activation is idempotent"

let check_fold_event_binds_finalized_parent () =
  let chain_id = "transition-fold-test" in
  let a = validator "octAAAA" in
  let b = validator "octBBBB" in
  let c = validator "octCCCC" in
  let validator_set = C_types.make_validator_set [a; b; c] in
  let driver =
    make_activation_driver
      ~chain_id
      ~my_addr:c.address
      ~initial_vs:validator_set
      ~target_vs:validator_set
      ~activate_epoch:20L
  in
  let parent_header = header ~chain_id ~epoch_id:20L ~creator_addr:a.address in
  let proposal_id = C_hash.proposal_id parent_header in
  let vote = C_types.{
    chain_id;
    epoch_id = 20L;
    round = 1;
    vote_type = Precommit;
    proposal_id;
    validator = c.address;
    signature = String.make 64 '\x0a';
  } in
  ignore (C_vote_log.keep driver.Driver.vote_log vote |> Result.get_ok);
  let parent = C_types.{
    validator_set;
    certificate = {
      chain_id;
      epoch_id = 20L;
      commit_round = 1;
      header = parent_header;
      proposal_id;
      precommits = [];
    };
  } in
  let child_header =
    header ~chain_id ~epoch_id:21L ~creator_addr:b.address
  in
  let child_proposal_id = C_hash.proposal_id child_header in
  let local_child_vote = C_types.{
    vote with
    epoch_id = 21L;
    proposal_id = child_proposal_id;
  } in
  let finalize = C_types.{
    chain_id;
    epoch_id = 21L;
    commit_round = 1;
    header = child_header;
    proposal_id = child_proposal_id;
    precommits = [local_child_vote];
    parent_commit = Some parent;
  } in
  begin
    match Driver.fold_event driver finalize with
    | None -> failwith "finalized parent omission did not produce fold event"
    | Some (saved, committed) ->
      assert_msg (saved = vote) "fold event changed durable vote";
      assert_msg
        (committed = parent)
        "fold event changed finalized parent";
      assert_msg
        (C_config.validator_set_hash committed.C_types.validator_set
         = C_config.validator_set_hash validator_set)
        "fold event changed validator set"
  end;
  assert_msg
    (Option.is_some
       (Driver.fold_event driver { finalize with precommits = [] }))
    "local finalize subset suppressed finalized appeal";
  let included = C_types.{
    parent with
    certificate = { parent.certificate with precommits = [vote] };
  } in
  assert_msg
    (Driver.fold_event driver { finalize with parent_commit = Some included }
     = None)
    "finalized signer produced false fold event";
  assert_msg
    (Driver.fold_event driver { finalize with parent_commit = None } = None)
    "missing finalized parent produced fold event"

let () =
  check_future_validator_engine_boundary ();
  check_profile_offer ();
  check_profile_switch ();
  check_scheduled_activation_restart_boundaries ();
  check_live_plan ();
  check_start_height_activates_target_set ();
  check_marked_set_repairs_runtime ();
  check_fold_event_binds_finalized_parent ();
  Printf.printf "status = pass test = bft_validator_transition\n%!"