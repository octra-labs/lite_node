(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

let () = Mirage_crypto_rng_unix.use_default ()

let keys = Hashtbl.create 8

let keypair address =
  match Hashtbl.find_opt keys address with
  | Some pair -> pair
  | None ->
    let private_key, public_key = Mirage_crypto_ec.Ed25519.generate () in
    let pair = private_key, Mirage_crypto_ec.Ed25519.pub_to_octets public_key in
    Hashtbl.add keys address pair;
    pair

let public_key address =
  snd (keypair address)

let sign address message =
  Mirage_crypto_ec.Ed25519.sign ~key:(fst (keypair address)) message

let verify address message signature =
  match Mirage_crypto_ec.Ed25519.pub_of_octets (public_key address) with
  | Ok key -> Mirage_crypto_ec.Ed25519.verify ~key ~msg:message signature
  | Error _ -> false

let validators =
  ["v0"; "v1"; "v2"; "v3"]

let validator_set =
  C_engine.make_validator_set
    (List.map
       (fun address ->
         C_types.{ address; pubkey = public_key address })
       validators)

let proposal ?(members = validator_set) chain_id ~epoch_id ~round =
  let proposer =
    (C_engine.leader_of members ~epoch_id ~round).address
  in
  let header =
    C_types.{
      proto_version = C_protocol.version_for_epoch epoch_id;
      chain_id;
      epoch_id;
      prev_state_root = String.make 32 '\x00';
      tx_list_hash = C_engine.tx_list_hash_for_header [];
      receipt_root = C_hash.receipt_root [];
      proposed_state_root =
        Octra_net.Hash_domain.hash
          "test:ready:root"
          (Int64.to_string epoch_id);
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = proposer;
      txid_hi = 0L;
      ts = 1.0;
    }
  in
  let unsigned =
    C_types.{
      chain_id;
      epoch_id;
      round;
      valid_round = None;
      header;
      tx_hashes = [];
      parent_commit = None;
      proposer;
      signature = String.make 64 '\x00';
    }
  in
  {
    unsigned with
    signature = sign proposer (C_hash.propose_sign_bytes unsigned);
  }

let swarm chain_id =
  let address = "v0" in
  let public_key = public_key address in
  Octra_net.P2p_swarm.create
    Octra_net.P2p_swarm.{
      listen_port = 0;
      chain_id;
      node_id = Octra_net.P2p_handshake.node_id_of_pubkey public_key;
      node_addr = address;
      pubkey_raw = public_key;
      consensus_config_hash = String.make 32 '\x00';
      binary_hash = String.make 32 '\x00';
      require_binary_hash = false;
      upgrade_plan = None;
      profile_plan = [];
      allowed_pubkeys = [];
      bootstrap_peers = [];
      max_peers = 0;
      sign_fn = sign address;
      best_epoch_fn = (fun () -> 0L);
      best_root_fn = (fun () -> String.make 32 '\x00');
    }

let driver
    ?(vote_log = C_vote_log.memory ())
    ?policy
    ?(members = validator_set)
    ?scheduled_validator_set_config
    ?(load_scheduled_validator_set_config = fun () -> Lwt.return_none)
    ?(verify_proposal = fun _ -> Lwt.return C_driver.Proposal_accept)
    ?(make_proposal = fun _ -> Lwt.return_none)
    ?(persist = fun () -> Lwt.return_true)
    chain_id =
  let config =
    C_driver.{
      chain_id;
      my_addr = "v0";
      sign_fn = sign "v0";
      verify_fn = verify;
      role_can_vote = (fun () -> true);
      can_vote = (fun () -> true);
      execute_fn = (fun _ -> true);
      verify_proposal;
      verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
      on_finalized = (fun ~validator_set:_ _ -> Lwt.return_unit);
      make_proposal;
      before_precommit_broadcast =
        (fun
          ~epoch_id:_
          ~round:_
          ~proposal_id:_
          ~proposed_state_root:_
          ~txid_hi:_
          ~proposal_wire:_
          ~vote_wire:_ ->
          persist ());
      lookup_epoch_root = (fun _ -> None);
      local_head_epoch = (fun () -> 0L);
      lookup_bundle = (fun _ -> None);
      lookup_catchup_range =
        (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
      on_resource_attestation = (fun _ -> Lwt.return_unit);
      scheduled_validator_set_config;
      load_scheduled_validator_set_config;
      resource_committee_config = None;
    }
  in
  let value = C_driver.create_for_policy
    (Option.value policy ~default:(C_quorum_policy.for_chain chain_id))
    ~config
    ~validator_set:members
    ~swarm:(swarm chain_id)
    ~start_height:1L
    ~sync_log:(C_sync_log.memory ())
    ~relief_log:(C_relief_log.memory ())
    ~vote_log
  in
  value.running <- true;
  value

let future_vote chain_id validator =
  C_types.{
    chain_id;
    epoch_id = 1L;
    round = 1;
    vote_type = Prevote;
    proposal_id = String.make 32 '\x01';
    validator;
    signature = String.make 64 '\x00';
  }

let signed_vote chain_id ~epoch_id ~round ~vote_type ~proposal_id validator =
  let unsigned =
    C_types.{
      chain_id;
      epoch_id;
      round;
      vote_type;
      proposal_id;
      validator;
      signature = String.make 64 '\x00';
    }
  in
  {
    unsigned with
    signature = sign validator (C_hash.vote_sign_bytes unsigned);
  }

let settle () =
  Lwt_main.run (Lwt_list.iter_s (fun () -> Lwt.pause ()) (List.init 8 (fun _ -> ())))

let receive_proposal driver value =
  let local, peer = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1"))
    ~addr:"proposal-test" ~direction:Octra_net.P2p_conn.Inbound
  in
  Lwt_main.run (Lwt.finalize
    (fun () -> C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
      msg_type = msg_cons_propose;
      payload = C_codec.encode_propose value;
    })
    (fun () ->
      let open Lwt.Syntax in
      let* () = Octra_net.P2p_conn.close conn in
      Lwt_unix.close peer));
  settle ()

let certificate (value : C_types.propose) =
  let proposal_id = C_hash.proposal_id value.header in
  C_types.{
    chain_id = value.chain_id;
    epoch_id = value.epoch_id;
    commit_round = value.round;
    header = value.header;
    proposal_id;
    precommits = List.map
      (signed_vote value.chain_id ~epoch_id:value.epoch_id ~round:value.round
        ~vote_type:Precommit ~proposal_id) ["v1"; "v2"; "v3"];
    parent_commit = None;
  }

let receive_finalize driver value =
  let local, peer = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1"))
    ~addr:"finality-test" ~direction:Octra_net.P2p_conn.Inbound
  in
  Lwt_main.run (Lwt.finalize
    (fun () -> C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
      msg_type = msg_cons_finalize;
      payload = C_codec.encode_finalize (certificate value);
    })
    (fun () ->
      let open Lwt.Syntax in
      let* () = Octra_net.P2p_conn.close conn in
      Lwt_unix.close peer))

let test_work_slot () =
  let open C_work_slot in
  let first, effects = step empty (Submit "first") in
  let id = match effects with [Run (id, "first")] -> id | _ -> assert false in
  let repeat, effects = step first (Submit "second") in
  assert (repeat = first && effects = []);
  let wrong, effects = step first (Finish (id + 1, 9)) in
  assert (wrong = first && effects = []);
  let done_, effects = step first (Finish (id, 7)) in
  assert (ready done_ && effects = [Complete "first"]);
  let repeat, effects = step done_ (Finish (id, 9)) in
  assert (repeat = done_ && effects = []);
  let vacant, effects = step repeat Take in
  assert (idle vacant && effects = [Deliver ("first", 7)]);
  let vacant, effects = step vacant Take in
  assert (idle vacant && effects = []);
  let closed, effects = step first Close in
  assert (not (idle closed) && effects = []);
  let closed, effects = step closed (Finish (id, 8)) in
  assert (not (ready closed) && effects = []);
  let opened, _ = step closed Open in
  let next, effects = step opened (Submit "next") in
  assert (effects = [Run (id + 1, "next")]);
  let ignored, effects = step next (Finish (id, 8)) in
  assert (ignored = next && effects = [])

let test_frame_finality () =
  let chain_id = "octra-test-frame-finality" in
  let result, finish = Lwt.wait () in
  let calls = ref 0 in
  let driver = driver
    ~verify_proposal:(fun _ -> incr calls; Lwt.protected result) chain_id
  in
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let local, peer = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1"))
    ~addr:"frame-finality" ~direction:Octra_net.P2p_conn.Inbound
  in
  let delivery =
    let open Lwt.Syntax in
    let* () = C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
      msg_type = msg_cons_propose;
      payload = C_codec.encode_propose value;
    } in
    C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
      msg_type = msg_cons_finalize;
      payload = C_codec.encode_finalize (certificate value);
    }
  in
  settle ();
  let progressed = driver.engine.state.height = 2L && not (Lwt.is_sleeping delivery) in
  let pending = !calls = 1 && Lwt.is_sleeping result in
  Lwt.wakeup_later finish C_driver.Proposal_accept;
  settle ();
  Lwt_main.run delivery;
  let voted = C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
    ~epoch_id:1L ~round:0 ~vote_type:C_types.Prevote
    ~proposal_id:(C_hash.proposal_id value.header)
    |> Result.get_ok |> Option.is_some
  in
  Lwt_main.run (C_driver.stop driver);
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close peer);
  if not progressed || not pending then
    failwith "first proposal check held following finality frame";
  if voted then failwith "late frame check emitted a vote after finality"

let test_frame_round () =
  let chain_id = "octra-test-frame-round" in
  let first, finish = Lwt.wait () in
  let calls = ref [] in
  let driver = driver ~verify_proposal:(fun value ->
    calls := value.C_types.round :: !calls;
    if List.length !calls = 1 then Lwt.protected first
    else Lwt.return C_driver.Proposal_accept) chain_id
  in
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let round = List.init C_engine.max_round_ahead (fun index -> index + 1)
    |> List.find (fun round ->
      (C_engine.leader_of validator_set ~epoch_id:1L ~round).address = value.proposer)
  in
  let next = { value with round; valid_round = Some 0 } in
  let next = { next with signature = sign next.proposer (C_hash.propose_sign_bytes next) } in
  let local, peer = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1"))
    ~addr:"frame-round" ~direction:Octra_net.P2p_conn.Inbound
  in
  let send value = C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
    msg_type = msg_cons_propose;
    payload = C_codec.encode_propose value;
  } in
  Lwt_main.run (send value);
  Lwt_main.run (send value);
  let single = !calls = [0] && Lwt.is_sleeping first in
  let generation = driver.engine.generation in
  C_engine.realign_round driver.engine round;
  C_driver.clear_round_sync_jump driver ~height:1L ~round;
  if driver.engine.generation = generation then failwith "generation did not advance";
  Lwt_main.run (send next);
  let queued = match driver.proposal_wait with
    | Some wait -> wait.proposal.round = round
    | None -> false
  in
  Lwt.wakeup_later finish C_driver.Proposal_accept;
  settle ();
  let completed = !calls = [round; 0] && driver.proposal_wait = None in
  Lwt_main.run (C_driver.stop driver);
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close peer);
  if not single then failwith "duplicate frame launched another check";
  if not queued || not completed then failwith "old reply removed next round check"

let test_wait_order () =
  let chain_id = "octra-test-wait-order" in
  let driver = driver chain_id in
  let route = C_driver.Publish_verified_proposal in
  let first = proposal chain_id ~epoch_id:1L ~round:0 in
  let next = proposal chain_id ~epoch_id:1L ~round:1 in
  C_driver.retain_proposal_wait driver ~route first;
  C_driver.retain_proposal_wait driver ~route first;
  let waiting = driver.proposal_wait in
  C_driver.retain_proposal_wait ~initial:true driver ~route first;
  if driver.proposal_wait <> waiting then
    failwith "duplicate frame reset check retry";
  C_driver.retain_proposal_wait ~initial:true driver ~route next;
  if driver.proposal_wait <> waiting then
    failwith "future round displaced current check";
  if Hashtbl.find_opt driver.pending_proposals
       (C_driver.proposal_round_key 1L 1) <> Some next then
    failwith "future round proposal lost";
  C_engine.start_round driver.engine 1;
  C_driver.retain_proposal_wait ~initial:true driver ~route next;
  (match driver.proposal_wait with
   | Some wait when wait.proposal = next && wait.attempt = 0 -> ()
   | _ -> failwith "old round blocked new proposal");
  C_driver.clear_proposal_wait driver first;
  if driver.proposal_wait = None then
    failwith "old completion erased next proposal";
  Lwt_main.run (C_driver.stop driver)

let test_wait_jump () =
  let chain_id = "octra-test-wait-jump" in
  let driver = driver chain_id in
  let route = C_driver.Publish_verified_proposal in
  let value = proposal chain_id ~epoch_id:1L ~round:1 in
  C_driver.retain_proposal_wait driver ~route value;
  C_driver.retain_proposal_wait driver ~route value;
  C_driver.retain_proposal_wait ~initial:true driver ~route value;
  if Hashtbl.length driver.pending_proposals <> 0 then
    failwith "waiting duplicate occupied another slot";
  let waiting = driver.proposal_wait in
  let generation = driver.engine.generation in
  C_engine.realign_round driver.engine 1;
  if driver.engine.generation = generation then failwith "wait generation did not advance";
  C_driver.clear_round_sync_jump driver ~height:1L ~round:1;
  let other = { value with header = { value.header with ts = 2.0 } } in
  C_driver.retain_proposal_wait ~initial:true driver ~route other;
  if driver.proposal_wait <> waiting then
    failwith "round jump displaced retained check";
  C_driver.retain_proposal_wait ~initial:true driver ~route value;
  if driver.proposal_wait <> waiting then
    failwith "round jump reset retained retry";
  Lwt_main.run (C_driver.stop driver)

let test_future_wait () =
  List.iter (fun future_first ->
    let chain_id = "octra-test-future-wait" in
    let calls = ref [] in
    let driver = driver ~verify_proposal:(fun value ->
      calls := value.C_types.round :: !calls;
      Lwt.return C_driver.Proposal_wait) chain_id in
    let current = proposal chain_id ~epoch_id:1L ~round:0 in
    let future = proposal chain_id ~epoch_id:1L ~round:1 in
    if future_first then receive_proposal driver future;
    receive_proposal driver current;
    receive_proposal driver future;
    let kept = Hashtbl.find_opt driver.pending_proposals
      (C_driver.proposal_round_key 1L 1) = Some future in
    C_engine.start_round driver.engine 1;
    C_driver.clear_round_sync_jump driver ~height:1L ~round:1;
    calls := [];
    Lwt_main.run (C_driver.process_outputs driver);
    settle ();
    let replayed = List.mem 1 !calls in
    Lwt_main.run (C_driver.stop driver);
    if not kept || not replayed then
      failwith "future proposal required a second frame") [false; true]

let test_wait_capacity () =
  let chain_id = "octra-test-wait-capacity" in
  let result, finish = Lwt.wait () in
  let driver = driver ~verify_proposal:(fun _ -> Lwt.protected result) chain_id in
  receive_proposal driver (proposal chain_id ~epoch_id:1L ~round:0);
  List.iter (fun round ->
    let value = proposal chain_id ~epoch_id:1L ~round in
    receive_proposal driver value;
    receive_proposal driver value)
    (List.init (C_engine.max_round_ahead + 4) (fun index -> index + 1));
  let limited = Hashtbl.length driver.pending_proposals = C_engine.max_round_ahead in
  C_engine.start_round driver.engine C_engine.max_round_ahead;
  C_driver.clear_round_sync_jump driver ~height:1L ~round:C_engine.max_round_ahead;
  receive_proposal driver
    (proposal chain_id ~epoch_id:1L ~round:(C_engine.max_round_ahead + 1));
  let pruned = Hashtbl.length driver.pending_proposals <= 2 in
  Lwt_main.run (C_driver.stop driver);
  Lwt.wakeup_later finish C_driver.Proposal_wait;
  settle ();
  if not limited || not pruned then failwith "round queue exceeded retention window"

let test_check_flow () =
  List.iter (fun mode ->
    let chain_id = "octra-test-check-flow" in
    let result, finish = Lwt.wait () in
    let calls = ref 0 in
    let verify_proposal _ = incr calls; Lwt.protected result in
    let driver = driver ~verify_proposal chain_id in
    let value = proposal chain_id ~epoch_id:1L ~round:0 in
    Hashtbl.replace driver.pending_proposals (C_driver.proposal_round_key 1L 0) value;
    Lwt_main.run (C_driver.process_outputs driver);
    assert (!calls = 1 && Lwt.is_sleeping result);
    List.iter (fun _ -> Lwt_main.run (C_driver.process_outputs driver)) (List.init 4 Fun.id);
    assert (!calls = 1);
    let passed = match mode with
      | `Finalize ->
        receive_finalize driver value;
        driver.engine.state.height = 2L
      | `Vote ->
        C_engine.on_timeout driver.engine ~step:C_types.ProposeStep ~round:0
          ~generation:driver.engine.generation ~sign_fn:(sign "v0");
        Lwt_main.run (C_driver.process_outputs driver);
        C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
          ~epoch_id:1L ~round:0 ~vote_type:C_types.Prevote
          ~proposal_id:Octra_net.Hash_domain.nil_hash
        |> Result.get_ok |> Option.is_some
      | `Round ->
        let generation = driver.engine.generation in
        C_engine.start_round driver.engine 1;
        assert (driver.engine.generation = generation);
        true
      | `Stop ->
        Lwt_main.run (C_driver.stop driver);
        driver.proposal_verify = None
    in
    let height = driver.engine.state.height in
    let round = driver.engine.state.round in
    let waiting = Lwt.is_sleeping result in
    Lwt.wakeup_later finish C_driver.Proposal_accept;
    settle ();
    let pid = C_hash.proposal_id value.header in
    let voted = C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
      ~epoch_id:1L ~round:0 ~vote_type:C_types.Prevote ~proposal_id:pid
      |> Result.get_ok |> Option.is_some
    in
    Lwt_main.run (C_driver.stop driver);
    if not passed || not waiting then failwith "proposal check held driver progress";
    if voted || driver.engine.state.height <> height || driver.engine.state.round <> round then
      failwith "late check changed driver state") [`Finalize; `Vote; `Round; `Stop]

let test_build_flow () =
  List.iter (fun mode ->
    let chain_id = "octra-test-build-flow" in
    let height = List.init 32 (fun i -> Int64.of_int (i + 1))
      |> List.find (fun epoch_id ->
        (C_engine.leader_of validator_set ~epoch_id ~round:0).address = "v0")
    in
    let result, finish = Lwt.wait () in
    let calls = ref 0 in
    let make_proposal requested =
      if requested = height then begin incr calls; Lwt.protected result end
      else Lwt.return_none
    in
    let driver = driver ~make_proposal chain_id in
    C_engine.start_height driver.engine height;
    Lwt_main.run (C_driver.process_outputs driver);
    assert (!calls = 1 && Lwt.is_sleeping result);
    let value = proposal chain_id ~epoch_id:height ~round:0 in
    let passed = match mode with
      | `Finalize ->
        receive_finalize driver value;
        driver.engine.state.height = Int64.succ height
      | `Round ->
        let generation = driver.engine.generation in
        C_engine.start_round driver.engine 1;
        assert (driver.engine.generation = generation);
        true
      | `Stop ->
        Lwt_main.run (C_driver.stop driver);
        driver.proposal_build = None
    in
    let generation = driver.engine.generation in
    let waiting = Lwt.is_sleeping result in
    Lwt.wakeup_later finish (Some C_driver.{
      header = value.header; tx_hashes = []; parent_commit = None });
    settle ();
    let voted = C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
      ~epoch_id:height ~round:0 ~vote_type:C_types.Prevote
      ~proposal_id:(C_hash.proposal_id value.header)
      |> Result.get_ok |> Option.is_some
    in
    Lwt_main.run (C_driver.stop driver);
    if not passed || not waiting || voted || driver.engine.generation <> generation then
      failwith "proposal build held or changed driver progress") [`Finalize; `Round; `Stop]

let test_check_handoff () =
  let chain_id = "octra-test-check-handoff" in
  let first, finish_first = Lwt.wait () in
  let second, finish_second = Lwt.wait () in
  let calls = ref 0 in
  let verify_proposal _ =
    incr calls;
    Lwt.protected (if !calls = 1 then first else second)
  in
  let driver = driver ~verify_proposal chain_id in
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  receive_proposal driver value;
  assert (!calls = 1);
  receive_proposal driver value;
  assert (!calls = 1 && driver.proposal_verify <> None);
  Lwt.wakeup_later finish_first C_driver.Proposal_wait;
  settle ();
  let held = !calls = 2 && driver.proposal_verify <> None in
  Lwt_main.run (C_driver.stop driver);
  Lwt.wakeup_later finish_second C_driver.Proposal_wait;
  settle ();
  if not held then failwith "prior check cleared running work"

let test_check_invalid () =
  let chain_id = "octra-test-check-invalid" in
  let calls = ref 0 in
  let driver = driver ~verify_proposal:(fun _ -> incr calls; Lwt.return C_driver.Proposal_accept)
    chain_id
  in
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let value = { value with signature = String.make 64 '\x00' } in
  C_driver.retain_proposal_wait driver ~route:C_driver.Publish_verified_proposal value;
  Lwt_main.run (C_driver.process_outputs driver);
  let cleared = driver.proposal_wait = None in
  Lwt_main.run (C_driver.stop driver);
  if not cleared || !calls <> 0 then failwith "invalid waiting proposal repeated"

let test_reply_grace () =
  let chain_id = "octra-test-reply-grace" in
  let result, finish = Lwt.wait () in
  let driver = driver ~verify_proposal:(fun _ -> Lwt.protected result) chain_id in
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let job = C_driver.check_work driver value C_driver.Publish_verified_proposal in
  receive_proposal driver value;
  let mark = driver.proposal_verify in
  assert (mark <> None && Lwt.is_sleeping result);
  Lwt_main.run (C_driver.finish_proposal_work driver job
    (C_driver.Checked_proposal None));
  let kept = driver.proposal_verify = mark in
  Lwt_main.run (C_driver.stop driver);
  Lwt.wakeup_later finish C_driver.Proposal_wait;
  settle ();
  if not kept then failwith "delivered reply cleared another check"

let test_work_timer () =
  List.iter (fun mode ->
    let chain_id = "octra-test-work-timer" in
    let height = List.init 32 (fun i -> Int64.of_int (i + 1))
      |> List.find (fun epoch_id ->
        (C_engine.leader_of validator_set ~epoch_id ~round:0).address = "v0")
    in
    let check, finish_check = Lwt.wait () in
    let build, finish_build = Lwt.wait () in
    let driver = driver
      ~verify_proposal:(fun _ -> Lwt.protected check)
      ~make_proposal:(fun _ -> Lwt.protected build) chain_id
    in
    C_engine.start_height driver.engine height;
    if mode = `Check then
      Hashtbl.replace driver.pending_proposals (C_driver.proposal_round_key height 0)
        (proposal chain_id ~epoch_id:height ~round:0);
    Lwt_main.run (C_driver.process_outputs driver);
    let expire (work : C_driver.proposal_build) =
      { work with started_at = Int64.sub (Mtime_clock.elapsed_ns ()) 301_000_000_000L }
    in
    (match mode with
     | `Check ->
       assert (driver.proposal_verify <> None);
       driver.proposal_verify <- Option.map expire driver.proposal_verify
     | `Build ->
       assert (driver.proposal_build <> None);
       driver.proposal_build <- Option.map expire driver.proposal_build);
    C_engine.emit driver.engine (C_engine.ScheduleTimeout {
      step = C_types.ProposeStep; round = 0; delay_ms = 0;
      generation = driver.engine.generation });
    Lwt_main.run (C_driver.process_outputs driver);
    Lwt_main.run (Lwt_unix.sleep 0.02);
    let voted = C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
      ~epoch_id:height ~round:0 ~vote_type:C_types.Prevote
      ~proposal_id:Octra_net.Hash_domain.nil_hash
      |> Result.get_ok |> Option.is_some
    in
    let waiting = Lwt.is_sleeping check && Lwt.is_sleeping build in
    Lwt_main.run (C_driver.stop driver);
    Lwt.wakeup_later finish_check C_driver.Proposal_accept;
    Lwt.wakeup_later finish_build None;
    settle ();
    if not voted || not waiting then failwith "running work held expired timer") [`Check; `Build]

let test_build_error () =
  let chain_id = "octra-test-build-error" in
  let height = List.init 32 (fun i -> Int64.of_int (i + 1))
    |> List.find (fun epoch_id ->
      (C_engine.leader_of validator_set ~epoch_id ~round:0).address = "v0")
  in
  let calls = ref 0 in
  let driver = driver ~make_proposal:(fun _ -> incr calls; Lwt.fail_with "local build error")
    chain_id
  in
  C_engine.start_height driver.engine height;
  Lwt_main.run (C_driver.process_outputs driver);
  let cleared = driver.proposal_build = None && driver.proposal_retry <> None in
  receive_finalize driver (proposal chain_id ~epoch_id:height ~round:0);
  let progressed = driver.engine.state.height = Int64.succ height in
  Lwt_main.run (C_driver.stop driver);
  if not cleared || not progressed || !calls = 0 then
    failwith "proposal error stopped driver progress"

let test_pace_votes () =
  let chain_id = "octra-test-pace-votes" in
  let driver = driver chain_id in
  driver.running <- true;
  driver.epoch_start_mono <-
    Int64.add (Mtime_clock.elapsed_ns ()) 60_000_000_000L;
  let first = proposal chain_id ~epoch_id:1L ~round:0 in
  let value = C_types.{
    chain_id;
    epoch_id = 1L;
    commit_round = 0;
    header = first.header;
    proposal_id = C_hash.proposal_id first.header;
    precommits = [];
    parent_commit = None;
  } in
  assert (C_engine.emit_finalized ~send:false driver.engine value);
  let pending = C_driver.process_outputs driver in
  let next = proposal chain_id ~epoch_id:2L ~round:0 in
  let pid = C_hash.proposal_id next.header in
  receive_proposal driver next;
  Lwt_main.run (C_driver.process_outputs driver);
  Lwt_main.run (Lwt_list.iter_s (fun _ -> Lwt.pause ()) (List.init 4 Fun.id));
  let saved vote_type =
    C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
      ~epoch_id:2L ~round:0 ~vote_type ~proposal_id:pid
    |> Result.get_ok
  in
  let prevote = saved C_types.Prevote in
  List.iter (fun address ->
    C_engine.on_vote driver.engine
      (signed_vote chain_id ~epoch_id:2L ~round:0
        ~vote_type:C_types.Prevote ~proposal_id:pid address)
      ~sign_fn:(sign "v0")) ["v1"; "v2"];
  Lwt_main.run (C_driver.process_outputs driver);
  let precommit = saved C_types.Precommit in
  let waiting = C_driver.pace_left driver > 0L in
  Lwt_main.run (C_driver.stop driver);
  Lwt.cancel pending;
  if not waiting then failwith "pace vote test lost its wait";
  List.iter (fun vote ->
    match vote with
    | Some value when C_hash.verify_vote ~pubkey_raw:(public_key "v0") value -> ()
    | _ -> failwith "pace held local durable vote") [prevote; precommit]

let test_qc_vote () =
  List.iter (fun durable ->
    let chain_id = "octra-test-qc-vote" in
    let writes = ref 0 in
    let driver = driver ~persist:(fun () -> incr writes; Lwt.return durable) chain_id in
    driver.running <- true;
    driver.epoch_start_mono <-
      Int64.sub (Mtime_clock.elapsed_ns ()) 60_000_000_000L;
    let value = proposal chain_id ~epoch_id:1L ~round:0 in
    let pid = C_hash.proposal_id value.header in
    receive_proposal driver value;
    let vote vote_type address = signed_vote chain_id ~epoch_id:1L ~round:0
      ~vote_type ~proposal_id:pid address
    in
    List.iter (fun address -> C_engine.on_vote driver.engine
      (vote C_types.Prevote address) ~sign_fn:(sign "v0")) ["v1"; "v2"];
    let finalized = C_types.{
      chain_id;
      epoch_id = 1L;
      commit_round = 0;
      header = value.header;
      proposal_id = pid;
      precommits = List.map (vote Precommit) ["v1"; "v2"; "v3"];
      parent_commit = None;
    } in
    if not (C_engine.accept_finalize_batch driver.engine finalized) then
      failwith "valid peer certificate refused";
    Lwt_main.run (C_driver.process_outputs driver);
    let parent = C_types.{
      validator_set;
      certificate = certificate_of_finalize finalized;
    } in
    let proof = C_driver.fold_event driver { finalized with parent_commit = Some parent } in
    let height = driver.engine.state.height in
    let pending = driver.engine.pending_finalized in
    Lwt_main.run (C_driver.stop driver);
    if height <> 2L || pending <> None then
      failwith "vote storage blocked accepted finality";
    if !writes <> 1 then failwith "finalized vote did not use durable path";
    match durable, proof with
    | true, Some (vote, parent) when
        vote.proposal_id = pid
        && vote.round = parent.certificate.commit_round
        && C_hash.verify_vote ~pubkey_raw:(public_key "v0") vote -> ()
    | false, None -> ()
    | _ -> failwith "finalized vote appeal did not match durability") [true; false]

let test_proposal_round_replay () =
  let chain_id = "octra-test-ready-replay" in
  let driver = driver chain_id in
  let proposal = proposal chain_id ~epoch_id:1L ~round:1 in
  C_driver.defer_verified_proposal driver proposal;
  C_engine.on_vote
    driver.engine
    (future_vote chain_id "v1")
    ~sign_fn:(sign "v0");
  C_engine.on_vote
    driver.engine
    (future_vote chain_id "v2")
    ~sign_fn:(sign "v0");
  if driver.engine.state.round <> 1 then
    failwith "round did not advance";
  Lwt_main.run (C_driver.process_outputs driver);
  let local_vote =
    Hashtbl.find_opt driver.engine.prevotes.votes "v0"
  in
  let expected = C_hash.proposal_id proposal.header in
  (match local_vote with
   | Some vote when vote.proposal_id = expected -> ()
   | _ -> failwith "deferred proposal was not prevoted");
  if Hashtbl.length driver.deferred_proposals <> 0 then
    failwith "deferred proposal was not consumed";
  let vote_count = Hashtbl.length driver.engine.prevotes.votes in
  Lwt_main.run (C_driver.process_outputs driver);
  if Hashtbl.length driver.engine.prevotes.votes <> vote_count then
    failwith "deferred proposal replayed more than once"

let test_proposal_height_replay () =
  let chain_id = "octra-test-pending-proposal" in
  let driver = driver chain_id in
  let next_proposal = proposal chain_id ~epoch_id:2L ~round:0 in
  if not (C_driver.defer_pending_proposal driver next_proposal) then
    failwith "next-height proposal was not retained";
  if
    C_driver.defer_pending_proposal
      driver
      (proposal chain_id ~epoch_id:3L ~round:0)
  then
    failwith "far-future proposal was retained";
  C_engine.start_height driver.engine 2L;
  Lwt_main.run (C_driver.process_outputs driver);
  let local_vote =
    Hashtbl.find_opt driver.engine.prevotes.votes "v0"
  in
  let expected = C_hash.proposal_id next_proposal.header in
  (match local_vote with
   | Some vote when vote.proposal_id = expected -> ()
   | _ -> failwith "pending proposal was not prevoted");
  if Hashtbl.length driver.pending_proposals <> 0 then
    failwith "pending proposal was not consumed"

let test_unretained_replay () =
  let chain_id = "octra-test-proposal-reentry" in
  let driver = driver chain_id in
  driver.running <- true;
  let early = proposal chain_id ~epoch_id:3L ~round:0 in
  let frame = Octra_net.P2p_frame.{
    msg_type = msg_cons_propose;
    payload = C_codec.encode_propose early;
  } in
  let local_fd, remote_fd =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Validator
      local_fd
      ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1"))
      ~addr:"198.51.100.21:19000"
      ~direction:Octra_net.P2p_conn.Inbound
  in
  Lwt_main.run (C_driver.on_p2p_message driver conn frame);
  if Hashtbl.length driver.pending_proposals <> 0 then
    failwith "far-future proposal was retained";
  C_engine.start_height driver.engine 2L;
  Lwt_main.run (C_driver.on_p2p_message driver conn frame);
  if Hashtbl.length driver.pending_proposals <> 1 then
    failwith "proposal id was consumed before retention";
  C_engine.start_height driver.engine 3L;
  Lwt_main.run (C_driver.process_outputs driver);
  let expected = C_hash.proposal_id early.header in
  (match Hashtbl.find_opt driver.engine.prevotes.votes "v0" with
   | Some vote when vote.proposal_id = expected -> ()
   | _ -> failwith "reentered proposal was not prevoted");
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd)

let test_future_vote_replay ?(active = false) () =
  let chain_id = "octra-test-future-votes" in
  let members = if active then
      match C_types.make_weighted_validator_set
        (List.map (fun member -> member, Z.one) validator_set.validators) with
      | Ok value -> value | Error reason -> failwith reason
    else validator_set in
  let policy = if active then
      Some (C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L))
    else None in
  let driver = driver ?policy ~members chain_id in
  let next_proposal = proposal ~members chain_id ~epoch_id:2L ~round:0 in
  let proposal_id = C_hash.proposal_id next_proposal.header in
  if not (C_driver.defer_pending_proposal driver next_proposal) then
    failwith "next-height proposal was not retained";
  List.iter
    (fun vote_type ->
      List.iter
        (fun validator ->
          let vote =
            signed_vote
              chain_id
              ~epoch_id:2L
              ~round:0
              ~vote_type
              ~proposal_id
              validator
          in
          match C_driver.defer_future_vote driver vote with
          | C_driver.Future_vote_deferred -> ()
          | _ -> failwith "next-height vote was not retained")
        ["v1"; "v2"; "v3"])
    [C_types.Prevote; C_types.Precommit];
  (match
    C_driver.defer_future_vote
      driver
      (signed_vote
         chain_id
         ~epoch_id:3L
         ~round:0
         ~vote_type:C_types.Prevote
         ~proposal_id
         "v1")
   with
   | C_driver.Future_vote_not_applicable -> ()
   | _ -> failwith "far-future vote was retained");
  C_engine.start_height driver.engine 2L;
  Lwt_main.run (C_driver.process_outputs driver);
  if driver.engine.finalized_height <> 2L then
    failwith "retained votes did not finalize next height";
  if Hashtbl.length driver.future_votes <> 0 then
    failwith "retained votes were not consumed";
  match C_vote_log.find_statement driver.vote_log ~chain_id ~validator:"v0"
    ~epoch_id:2L ~round:0 ~vote_type:C_types.Precommit ~proposal_id with
  | Ok (Some vote) when C_hash.verify_vote ~pubkey_raw:(public_key "v0") vote -> ()
  | _ -> failwith "finalized output lost local appeal vote"

let test_vote_conflict_retained () =
  let chain_id = "octra-test-future-vote-conflict" in
  let queued_driver = driver chain_id in
  let first =
    signed_vote
      chain_id
      ~epoch_id:2L
      ~round:0
      ~vote_type:C_types.Prevote
      ~proposal_id:(String.make 32 '\x01')
      "v1"
  in
  let second =
    signed_vote
      chain_id
      ~epoch_id:2L
      ~round:0
      ~vote_type:C_types.Prevote
      ~proposal_id:(String.make 32 '\x02')
      "v1"
  in
  (match C_driver.defer_future_vote queued_driver first with
   | C_driver.Future_vote_deferred -> ()
   | _ -> failwith "first future vote was not retained");
  (match C_driver.defer_future_vote queued_driver first with
   | C_driver.Future_vote_same -> ()
   | _ -> failwith "same future vote was not classified");
  (match C_driver.defer_future_vote queued_driver second with
   | C_driver.Future_vote_conflict prior
     when prior.proposal_id = first.proposal_id -> ()
   | _ -> failwith "future vote conflict was not attributed");
  if Hashtbl.length queued_driver.future_votes <> 1 then
    failwith "future vote conflict changed retained votes";
  let replay_driver = driver chain_id in
  (match C_driver.defer_future_vote replay_driver first with
   | C_driver.Future_vote_deferred -> ()
   | _ -> failwith "replay future vote was not retained");
  C_engine.start_height replay_driver.engine 2L;
  C_engine.on_vote
    replay_driver.engine
    second
    ~sign_fn:replay_driver.config.sign_fn;
  let evidence = C_driver.replay_future_votes replay_driver in
  if List.length evidence <> 1 then
    failwith "replay conflict did not produce evidence";
  if List.length (C_driver.vote_evidence replay_driver) <> 1 then
    failwith "replay conflict evidence was not retained";
  if Hashtbl.length replay_driver.future_votes <> 0 then
    failwith "replayed conflict remained queued"

let test_activation_vote_replay () =
  let chain_id = "octra-test-activation-vote-reentry" in
  let next_set =
    C_engine.make_validator_set
      (List.map
         (fun address ->
           C_types.{ address; pubkey = public_key address })
         ["r0"; "r1"; "r2"; "r3"])
  in
  let scheduled = C_driver.{
    activate_epoch = 2L;
    validator_set = next_set;
    fingerprint = "activation-vote-reentry";
  } in
  let plan = ref None in
  let driver =
    driver
      ~load_scheduled_validator_set_config:(fun () -> Lwt.return !plan)
      chain_id
  in
  driver.running <- true;
  let value =
    signed_vote
      chain_id
      ~epoch_id:2L
      ~round:0
      ~vote_type:C_types.Prevote
      ~proposal_id:(String.make 32 '\x33')
      "r1"
  in
  let frame = Octra_net.P2p_frame.{
    msg_type = msg_cons_vote;
    payload = C_codec.encode_vote value;
  } in
  let local_fd, remote_fd =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Validator
      local_fd
      ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v2"))
      ~addr:"198.51.100.23:19000"
      ~direction:Octra_net.P2p_conn.Inbound
  in
  Lwt_main.run (C_driver.on_p2p_message driver conn frame);
  if Hashtbl.length driver.future_votes <> 0 then
    failwith "vote entered before its validator set resolved";
  if Octra_net.P2p_swarm.peer_scores driver.swarm <> [] then
    failwith "unresolved vote penalized its relay";
  plan := Some scheduled;
  Lwt_main.run (C_driver.on_p2p_message driver conn frame);
  if Hashtbl.length driver.future_votes <> 1 then
    failwith "vote id was consumed before validator set resolution";
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd)

let test_activation_vote_signer () =
  let chain_id = "octra-test-activation-evidence" in
  let next_addresses = ["n0"; "n1"; "n2"; "n3"] in
  let next_set =
    C_engine.make_validator_set
      (List.map
         (fun address ->
           C_types.{ address; pubkey = public_key address })
         next_addresses)
  in
  let scheduled_validator_set_config = C_driver.{
    activate_epoch = 2L;
    validator_set = next_set;
    fingerprint = "activation-evidence";
  } in
  let source = driver ~scheduled_validator_set_config chain_id in
  source.running <- true;
  let relay_id =
    Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v2")
  in
  let local_fd, remote_fd =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Validator
      local_fd
      ~peer_id:relay_id
      ~addr:"198.51.100.22:19000"
      ~direction:Octra_net.P2p_conn.Inbound
  in
  let vote proposal_id =
    signed_vote
      chain_id
      ~epoch_id:2L
      ~round:0
      ~vote_type:C_types.Prevote
      ~proposal_id
      "n1"
  in
  let send target value =
    C_driver.on_p2p_message
      target
      conn
      Octra_net.P2p_frame.{
        msg_type = msg_cons_vote;
        payload = C_codec.encode_vote value;
      }
  in
  Lwt_main.run (send source (vote (String.make 32 '\x11')));
  Lwt_main.run (send source (vote (String.make 32 '\x22')));
  let evidence =
    match C_driver.vote_evidence source with
    | [value] -> value
    | _ -> failwith "next-set equivocation evidence was not retained"
  in
  let signer_id =
    Octra_net.P2p_handshake.node_id_of_pubkey (public_key "n1")
  in
  let signer_key = Octra_net.P2p_peer_guard.identity_key signer_id in
  let relay_key = Octra_net.P2p_peer_guard.identity_key relay_id in
  let source_scores = Octra_net.P2p_swarm.peer_scores source.swarm in
  if not (List.exists (fun row -> row.Octra_net.P2p_peer_guard.key = signer_key) source_scores) then
    failwith "equivocation was not attributed to its signer";
  if List.exists (fun row -> row.Octra_net.P2p_peer_guard.key = relay_key) source_scores then
    failwith "equivocation was attributed to its relay";
  let receiver = driver ~scheduled_validator_set_config chain_id in
  receiver.running <- true;
  Lwt_main.run
    (C_driver.on_p2p_message
       receiver
       conn
       Octra_net.P2p_frame.{
         msg_type = msg_vote_evidence;
         payload = C_evidence.encode_vote_conflict evidence;
       });
  if List.length (C_driver.vote_evidence receiver) <> 1 then
    failwith "next-height evidence was rejected at the prior height";
  let receiver_scores = Octra_net.P2p_swarm.peer_scores receiver.swarm in
  if not (List.exists (fun row -> row.Octra_net.P2p_peer_guard.key = signer_key) receiver_scores) then
    failwith "received evidence did not identify its signer";
  if List.exists (fun row -> row.Octra_net.P2p_peer_guard.key = relay_key) receiver_scores then
    failwith "received evidence penalized its relay";
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd)

let test_waiting_proposal_retry () =
  let chain_id = "octra-test-proposal-wait" in
  let ready = ref false in
  let calls = ref 0 in
  let verify_proposal _ =
    incr calls;
    Lwt.return
      (if !ready then C_driver.Proposal_accept
       else C_driver.Proposal_wait)
  in
  let driver = driver ~verify_proposal chain_id in
  driver.running <- true;
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let local_fd, remote_fd =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Observer
      local_fd
      ~peer_id:"proposal-wait-peer"
      ~addr:"198.51.100.28:19000"
      ~direction:Octra_net.P2p_conn.Inbound
  in
  Lwt_main.run
    (C_driver.on_p2p_message
       driver
       conn
       Octra_net.P2p_frame.{
         msg_type = msg_cons_propose;
         payload = C_codec.encode_propose value;
       });
  if !calls <> 2 then
    failwith "proposal wait did not run one immediate retry";
  if driver.proposal_wait = None then
    failwith "proposal wait was not retained";
  if Hashtbl.length driver.engine.prevotes.votes <> 0 then
    failwith "proposal wait emitted a vote";
  if Octra_net.P2p_swarm.peer_scores driver.swarm <> [] then
    failwith "proposal wait penalized its relay";
  ready := true;
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt_unix.sleep 0.6 in
     C_driver.process_outputs driver);
  if !calls <> 3 then
    failwith "proposal retry did not rerun verification";
  if driver.proposal_wait <> None then
    failwith "accepted proposal wait was retained";
  let expected = C_hash.proposal_id value.header in
  (match Hashtbl.find_opt driver.engine.prevotes.votes "v0" with
   | Some vote when vote.proposal_id = expected -> ()
   | _ -> failwith "proposal retry did not emit the local vote");
  if Octra_net.P2p_swarm.peer_scores driver.swarm <> [] then
    failwith "accepted proposal retry penalized its relay";
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd)

let test_verify_error_retry () =
  let chain_id = "octra-test-proposal-error" in
  let verify_proposal _ = Lwt.fail_with "local verify error" in
  let driver = driver ~verify_proposal chain_id in
  driver.running <- true;
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let local_fd, remote_fd =
    Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  let conn =
    Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Observer
      local_fd
      ~peer_id:"proposal-error-peer"
      ~addr:"198.51.100.29:19000"
      ~direction:Octra_net.P2p_conn.Inbound
  in
  Lwt_main.run
    (C_driver.on_p2p_message
       driver
       conn
       Octra_net.P2p_frame.{
         msg_type = msg_cons_propose;
         payload = C_codec.encode_propose value;
       });
  if driver.proposal_wait = None then
    failwith "proposal verify error was not retained";
  if Hashtbl.length driver.engine.prevotes.votes <> 0 then
    failwith "proposal verify error emitted a vote";
  if Octra_net.P2p_swarm.peer_scores driver.swarm <> [] then
    failwith "proposal verify error penalized its relay";
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd)

let test_relay_backpressure () =
  let chain_id = "octra-test-relay-pressure" in
  let driver = driver chain_id in
  driver.running <- true;
  let local_fd, remote_fd = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Observer local_fd
    ~peer_id:"blocked-peer" ~addr:"198.51.100.29:19000"
    ~direction:Octra_net.P2p_conn.Inbound in
  Hashtbl.add driver.swarm.peers conn.peer_id conn;
  Lwt_main.run (Lwt_mvar.put conn.write_queue
    Octra_net.P2p_frame.{ msg_type = msg_cons_propose; payload = "occupied" });
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let delivery = C_driver.finish_check driver
    ~route:C_driver.Publish_verified_proposal value (Some C_driver.Proposal_accept) in
  let immediate = not (Lwt.is_sleeping delivery) in
  Lwt_main.run (C_driver.stop driver);
  Lwt.cancel delivery;
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd);
  if not immediate then failwith "verified proposal waited for blocked peer"

let test_future_relay () =
  List.iter (fun valid ->
    let chain_id = "octra-test-future-relay" in
    let sent = ref [] in
    let relay = C_relay.create ~now:(fun () -> 0.)
      ~send:(fun (_, frame) ->
        sent := frame :: !sent;
        fst (Lwt.task ()))
      ~wait:(fun _ -> fst (Lwt.task ())) ~warn:ignore
    in
    let driver = { (driver chain_id) with C_driver.proposal_relay = relay } in
    let value = proposal chain_id ~epoch_id:1L ~round:1 in
    Lwt_main.run (C_driver.finish_check driver
      ~route:C_driver.Publish_verified_proposal value (Some C_driver.Proposal_accept));
    settle ();
    let first = List.length !sent in
    if not valid then
      Hashtbl.replace driver.deferred_proposals (C_driver.proposal_round_key 1L 1)
        { value with signature = String.make 64 '\x00' };
    C_engine.realign_round driver.engine 1;
    C_relay.progress relay ~generation:driver.engine.generation;
    C_driver.replay_deferred_proposal driver;
    settle ();
    let expected = if valid then 2 else 1 in
    let repeated = List.length !sent in
    C_driver.replay_deferred_proposal driver;
    settle ();
    let once = List.length !sent = repeated in
    Lwt_main.run (C_driver.stop driver);
    if first <> 1 || repeated <> expected || not once then
      failwith "future proposal relay did not follow accepted round replay") [true; false]

let test_catchup_keeps_round () =
  List.iter (fun step ->
    let chain_id = "octra-test-catchup-clock" in
    let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
    let members = match C_types.make_weighted_validator_set
        (List.map (fun member -> member, Z.one) validator_set.validators) with
      | Ok value -> value | Error reason -> failwith reason in
    let driver = driver ~policy ~members chain_id in
    let engine = driver.engine in
    let locked = proposal ~members chain_id ~epoch_id:1L ~round:110 in
    if Result.is_error (C_engine.restore_precommit_lock engine locked) then
      failwith "catchup clock test could not restore its lock";
    C_engine.realign_round engine 117;
    engine.state <- {engine.state with step};
    (match step with
     | C_types.ProposeStep -> ()
     | C_types.PrevoteStep | C_types.PrecommitStep ->
       let kind = if step = C_types.PrevoteStep then C_types.Prevote else C_types.Precommit in
       List.iter (fun address ->
         let proposal_id = if address = "v0" then Octra_net.Hash_domain.nil_hash
           else Digestif.SHA256.(to_raw_string (digest_string address)) in
         C_engine.on_vote engine
           (signed_vote chain_id ~epoch_id:1L ~round:117 ~vote_type:kind
             ~proposal_id address)
           ~sign_fn:(sign "v0")) ["v0"; "v1"; "v2"]);
    Hashtbl.replace engine.higher_round_evidence "v1" 118;
    ignore (C_engine.drain_outputs engine);
    let generation = engine.generation in
    let state = engine.state in
    let io = Octra_node_runtime.Consensus_catchup_shell.driver_io_of_driver driver in
    List.iter (fun _ ->
      Lwt_main.run (io.advance_height 1L);
      Lwt_main.run (io.start_height 1L)) [28; 56; 84; 112];
    if engine.generation <> generation || engine.state <> state then
      failwith "same height range retry restarted the consensus clock";
    if Hashtbl.find_opt engine.higher_round_evidence "v1" <> Some 118 then
      failwith "same height range retry discarded round evidence";
    Lwt_main.run (io.advance_height 0L);
    if engine.generation <> generation || engine.state <> state then
      failwith "older range retry rewound the consensus clock";
    C_engine.on_timeout engine ~step ~round:117 ~generation ~sign_fn:(sign "v0");
    if engine.state = state then
      failwith (Printf.sprintf "original consensus timer stopped after range retries step = %s prevotes = %d precommits = %d"
        (match step with C_types.ProposeStep -> "propose" | C_types.PrevoteStep -> "prevote" | C_types.PrecommitStep -> "precommit")
        engine.prevotes.count_total engine.precommits.count_total);
    ignore (C_engine.drain_outputs engine);
    Lwt_main.run (io.advance_height 2L);
    if engine.state.height <> 2L || engine.state.round <> 0 then
      failwith "new finalized height did not advance consensus";
    Lwt_main.run (C_driver.stop driver))
    C_types.[ProposeStep; PrevoteStep; PrecommitStep]

let test_vote_round_restart () =
  List.iter (fun kind ->
    let chain_id = "octra-test-vote-round-restart" in
    let vote_log = C_vote_log.memory () in
    let vote = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:kind
      ~proposal_id:Octra_net.Hash_domain.nil_hash "v0" in
    if Result.is_error (C_vote_log.keep vote_log vote) then
      failwith "saved vote was refused";
    let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
    let driver = driver ~vote_log ~policy chain_id in
    C_driver.resume_local_vote driver;
    let expected = if kind = C_types.Prevote then C_types.PrevoteStep
      else C_types.PrecommitStep in
    if driver.engine.state.round <> 7 || driver.engine.state.step <> expected then
      failwith "restart skipped saved voting round";
    let votes = C_engine.vote_set_for driver.engine kind in
    if Hashtbl.find_opt votes.votes "v0" <> Some vote then
      failwith "restart lost exact signed vote";
    let invalid = {vote with signature = String.make 64 '\000'} in
    if Result.is_ok (C_engine.restore_local_votes driver.engine [invalid]) then
      failwith "restart accepted an invalid saved signature";
    if Result.is_ok (C_engine.restore_local_votes driver.engine [vote; vote]) then
      failwith "restart counted a saved vote twice";
    let state = driver.engine.state and generation = driver.engine.generation in
    C_driver.resume_local_vote driver;
    if state <> driver.engine.state || generation <> driver.engine.generation then
      failwith "repeated vote restore advanced the round";
    let conflict = {vote with proposal_id = String.make 32 '\001'} in
    if not (Result.is_error (C_vote_log.check vote_log conflict)) then
      failwith "restart allowed a conflicting vote";
    Lwt_main.run (C_driver.stop driver)) C_types.[Prevote; Precommit]

let test_past_vote_reply mode =
  let chain_id = "octra-test-past-vote-reply" in
  let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
  let vote_log = C_vote_log.memory () in
  let saved = List.filter_map (fun kind ->
    let vote = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:kind
      ~proposal_id:(String.make 32 '\001') "v0" in
    if mode = "absent" then None else
    let vote = if mode = "invalid" then {vote with signature = String.make 64 '\000'}
      else vote in
    match C_vote_log.keep vote_log vote with
    | Ok vote -> Some vote
    | Error reason -> failwith reason) C_types.[Prevote; Precommit] in
  let driver = driver ~vote_log ~policy chain_id in
  if mode = "failed" then C_driver.hold_vote_log driver "test write refused";
  if mode = "proof" then driver.finality_proof_needed := true;
  C_engine.realign_round driver.engine 8;
  let relayed = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:C_types.Prevote
    ~proposal_id:(String.make 32 '\002') "v1" in
  let second = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:C_types.Prevote
    ~proposal_id:(String.make 32 '\002') "v2" in
  let unsaved = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:C_types.Prevote
    ~proposal_id:(String.make 32 '\002') "v0" in
  let votes = C_engine.create_vote_set () in
  List.iter (fun vote -> ignore (C_engine.add_vote ~policy votes vote
    ~validator_set:driver.engine.vs)) [unsaved; relayed; second];
  Hashtbl.replace driver.engine.prevotes_by_round 7 votes;
  C_engine.record_polc driver.engine 7 unsaved.proposal_id;
  let precommit = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:C_types.Precommit
    ~proposal_id:relayed.proposal_id "v1" in
  let unsigned = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:C_types.Precommit
    ~proposal_id:unsaved.proposal_id "v0" in
  let precommits = C_engine.create_vote_set () in
  List.iter (fun vote -> ignore (C_engine.add_vote ~policy precommits vote
    ~validator_set:driver.engine.vs)) [precommit; unsigned];
  Hashtbl.replace driver.engine.precommits_by_round 7 precommits;
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
    local ~peer_id:"past-round" ~addr:"local" ~direction:Octra_net.P2p_conn.Inbound in
  let received = ref [] in
  let rec consume () =
    let open Lwt.Syntax in
    let* frame = Lwt_mvar.take conn.write_queue in
    if frame.Octra_net.P2p_frame.msg_type = Octra_net.P2p_frame.msg_cons_vote then
      received := C_codec.decode_vote frame.payload :: !received;
    consume () in
  let reading = consume () in
  Lwt_main.run (Lwt_unix.with_timeout 120. (fun () ->
    C_driver.send_round_sync_response driver conn ~requested_round:7 ~with_witness:false));
  settle ();
  Lwt.cancel reading;
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote);
  Lwt_main.run (C_driver.stop driver);
  let expected = precommit :: relayed :: second :: (if List.mem mode ["invalid"; "failed"; "proof"]
    then [] else saved) in
  if List.sort compare !received <> List.sort compare expected then
    failwith "past round reply did not use durable local votes";
  if mode = "invalid" && driver.vote_log_issue = None then
    failwith "invalid saved signature did not hold voting"

let test_progress_clock () =
  let members = match C_types.make_weighted_validator_set
    (List.map (fun member -> member, Z.one) validator_set.validators) with
    | Ok value -> value | Error reason -> failwith reason in
  List.iter (fun active ->
    let policy = C_quorum_policy.make ~count_epoch:0L
      ~stake_epoch:(if active then Some 1L else Some 2L)
      ~round_epoch:(if active then Some 1L else Some 2L) in
    let node = driver ~members ~policy "octra-test-progress-clock" in
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      local ~peer_id:"progress-peer" ~addr:"local" ~direction:Octra_net.P2p_conn.Inbound in
    Hashtbl.add node.swarm.peers conn.peer_id conn;
    let syncs = ref [] in
    let rec consume () =
      let open Lwt.Syntax in
      let* frame = Lwt_mvar.take conn.write_queue in
      if frame.Octra_net.P2p_frame.msg_type = Octra_net.P2p_frame.msg_cons_round_sync then
        syncs := C_codec.decode_round_sync frame.payload :: !syncs;
      consume () in
    let reading = consume () in
    let engine = node.engine in
    C_engine.realign_round engine 7;
    let offer = proposal engine.chain_id ~epoch_id:1L ~round:6 in
    engine.state <- {engine.state with locked_round = 6; locked_value = Some offer.header};
    ignore (C_engine.drain_outputs engine);
    let generation = engine.generation in
    for _ = 1 to 3 do
      Lwt_main.run (C_driver.realign_progress node ~height:1L ~round:7)
    done;
    let expected = if active then 7 else 10 in
    if engine.state.round <> expected then failwith "progress clock advanced without evidence";
    if active && engine.generation <> generation then failwith "progress clock restarted the round";
    settle ();
    if active && (List.length !syncs < 3 || not (List.for_all (fun sync ->
      sync.C_codec.request && sync.round = 7
      && C_hash.verify_round_sync ~pubkey_raw:(public_key "v0") sync) !syncs)) then
      failwith "progress clock did not repeat signed requests";
    if active then begin
      node.progress_step_at <- 0L;
      Hashtbl.replace engine.higher_round_evidence "v1" 100;
      let tick now = Lwt_main.run
        (C_driver.realign_progress_at node ~now ~height:1L ~round:100) in
      tick 179_999_999_999L;
      if engine.state.round <> 7 then failwith "single report bypassed stall interval";
      tick 180_000_000_000L;
      if engine.state.round <> 8 then failwith "signed stall did not advance exactly one round";
      tick 180_000_000_000L;
      tick 359_999_999_999L;
      if engine.state.round <> 8 then failwith "repeated report bypassed step interval";
      Hashtbl.replace engine.higher_round_evidence "v2" 20;
      C_engine.set_round_skip_ready engine (fun () -> false);
      tick 360_000_000_000L;
      if engine.state.round <> 8 then failwith "progress clock interrupted proposal work";
      C_engine.set_round_skip_ready engine (fun () -> true);
      tick 360_000_000_000L;
      if engine.state.round <> 20 then failwith "progress clock did not use signed floor"
    end;
    if engine.state.locked_round <> 6 || engine.state.locked_value <> Some offer.header then
      failwith "progress clock discarded the lock";
    Lwt.cancel reading;
    Lwt_main.run (C_driver.stop node);
    Lwt_main.run (Octra_net.P2p_conn.close conn);
    Lwt_main.run (Lwt_unix.close remote)
  ) [false; true]

let test_locked_round_restart () =
  let chain_id = "octra-test-locked-round" in
  let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
  let vote_log = C_vote_log.memory () in
  let node = driver ~vote_log ~policy chain_id in
  node.running <- false;
  let offer = proposal chain_id ~epoch_id:1L ~round:7 in
  if Result.is_error (C_driver.restore_precommit_lock node offer) then
    failwith "saved lock was not restored";
  let vote = signed_vote chain_id ~epoch_id:1L ~round:7 ~vote_type:C_types.Precommit
    ~proposal_id:(C_hash.proposal_id offer.header) "v0" in
  ignore (C_vote_log.keep vote_log vote);
  C_driver.resume_local_vote node;
  if node.engine.state.round <> 7 || node.engine.state.step <> C_types.PrecommitStep
     || node.engine.state.locked_value <> Some offer.header then
    failwith "saved lock restart skipped the voting round";
  if Result.is_error (C_driver.restore_precommit_lock node offer) then
    failwith "saved lock restore was not idempotent";
  Lwt_main.run (C_driver.stop node)

let test_polc_round_restart () =
  let chain_id = "octra-test-polc-round" in
  let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
  let members = match C_types.make_weighted_validator_set
    (List.map (fun member -> member, Z.one) validator_set.validators) with
    | Ok value -> value | Error reason -> failwith reason in
  let vote_log = C_vote_log.memory () in
  let offer = proposal ~members chain_id ~epoch_id:1L ~round:7 in
  let pid = C_hash.proposal_id offer.header in
  let votes = List.map (signed_vote chain_id ~epoch_id:1L ~round:7
    ~vote_type:C_types.Prevote ~proposal_id:pid) ["v0"; "v1"; "v2"] in
  (match C_vote_log.polc_keep ~policy vote_log ~chain_id ~epoch_id:1L
      ~validator_set:members ~locked_round:(-1) votes with
   | Ok () -> () | Error reason -> failwith reason);
  let node = driver ~members ~vote_log ~policy chain_id in
  (match C_driver.restore_polc node with
   | Ok () -> () | Error reason -> failwith reason);
  if node.engine.state.round <> 7 || List.length (C_engine.polc_votes_for_round node.engine 7) <> 3 then
    failwith "stored prevote proof skipped its round";
  if not (C_driver.proposal_verify_current node offer) then
    failwith "stored proof cannot fetch its proposal";
  receive_proposal node offer;
  if node.engine.state.locked_round <> 7 || node.engine.state.locked_value <> Some offer.header then
    failwith "stored proof did not restore its lock after fetching the proposal";
  Lwt_main.run (C_driver.stop node)

let test_late_quorum_offer () =
  let chain_id = "octra-test-late-quorum" in
  let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
  let members = match C_types.make_weighted_validator_set
    (List.map (fun member -> member, Z.one) validator_set.validators) with
    | Ok value -> value | Error reason -> failwith reason in
  let node = driver ~policy ~members chain_id in
  C_engine.start_height node.engine 1L;
  C_engine.on_timeout node.engine ~step:C_types.ProposeStep ~round:0
    ~generation:node.engine.generation ~sign_fn:(sign "v0");
  ignore (C_engine.drain_outputs node.engine);
  let offer = proposal ~members chain_id ~epoch_id:1L ~round:0 in
  let pid = C_hash.proposal_id offer.header in
  List.iter (fun address -> C_engine.on_vote node.engine
    (signed_vote chain_id ~epoch_id:1L ~round:0 ~vote_type:C_types.Prevote ~proposal_id:pid address)
    ~sign_fn:(sign "v0")) ["v1"; "v2"; "v3"];
  if not (C_driver.proposal_verify_current node offer) then
    failwith "late quorum proposal was not eligible for checking";
  receive_proposal node offer;
  if node.engine.state.locked_value <> Some offer.header then
    failwith "late quorum proposal did not restore progress";
  Lwt_main.run (C_driver.stop node)

let test_relief_wait_plan () =
  let chain_id = "octra-devnet-9871-cluster" in
  let epoch = Option.get (C_quorum_policy.round_epoch ~chain_id) in
  let make count = match C_types.make_weighted_validator_set (List.init count (fun i ->
    let address = "v" ^ string_of_int i in
    C_types.{address; pubkey = public_key address}, Z.one)) with
    | Ok value -> value | Error reason -> failwith reason in
  let plan = ref (Some C_driver.{activate_epoch = Int64.add epoch 10L;
    validator_set = make 6; fingerprint = String.make 64 'a'}) in
  let after = ref (fun () -> ()) in
  let base = driver ~load_scheduled_validator_set_config:(fun () ->
    (!after) (); Lwt.return !plan)
    chain_id in
  let node = C_driver.create ~config:base.config ~validator_set:(make 7)
    ~swarm:base.swarm ~start_height:epoch ~sync_log:(C_sync_log.memory ())
    ~relief_log:(C_relief_log.memory ()) ~vote_log:(C_vote_log.memory ()) in
  Lwt_main.run (C_driver.maybe_activate_relief node);
  if node.engine.relief_limit <> Some C_relief.min_round then
    failwith "checked set plan did not enable relief clock";
  after := (fun () -> C_engine.start_height node.engine (Int64.succ epoch));
  Lwt_main.run (C_driver.maybe_activate_relief node);
  if node.engine.relief_limit <> None then
    failwith "old height plan enabled relief clock";
  after := (fun () -> ());
  plan := None;
  Lwt_main.run (C_driver.maybe_activate_relief node);
  if node.engine.relief_limit <> None then
    failwith "removed set plan retained relief clock";
  Lwt_main.run (C_driver.stop node);
  Lwt_main.run (C_driver.stop base)

let test_relief_plan_race () =
  let chain_id = "octra-devnet-9871-cluster" in
  let make count = match C_types.make_weighted_validator_set (List.init count (fun i ->
    let address = "v" ^ string_of_int i in
    C_types.{address; pubkey = public_key address}, Z.one)) with
    | Ok value -> value | Error reason -> failwith reason in
  let first = Int64.of_int (Option.get (C_quorum_policy.stake_activation_for_chain chain_id)).activation_epoch in
  let second = Option.get (C_quorum_policy.round_epoch ~chain_id) in
  List.iter (fun epoch -> List.iter (fun change_set ->
    let pending, wake = Lwt.wait () in
    let base = driver ~load_scheduled_validator_set_config:(fun () -> pending) chain_id in
    let node = C_driver.create ~config:base.config ~validator_set:(make 7)
      ~swarm:base.swarm ~start_height:epoch ~sync_log:(C_sync_log.memory ())
      ~relief_log:(C_relief_log.memory ()) ~vote_log:(C_vote_log.memory ()) in
    node.engine.state <- {node.engine.state with round = C_relief.min_round};
    let work = C_driver.maybe_activate_relief node in
    if not (Lwt.is_sleeping work) then failwith "relief did not await plan";
    if change_set then C_engine.reconfigure_validator_set node.engine ~round:(C_relief.min_round + 1) (make 8)
    else C_engine.start_height node.engine (Int64.succ epoch);
    Lwt.wakeup wake (Some C_driver.{activate_epoch = Int64.add epoch 10L;
      validator_set = make 6; fingerprint = String.make 64 'a'});
    Lwt_main.run work;
    if node.engine.relief_limit <> None || node.relief_plan <> None then
      failwith "changed relief context installed old plan";
    Lwt_main.run (C_driver.stop node);
    Lwt_main.run (C_driver.stop base)) [false; true]) [Int64.pred first; first; Int64.pred second; second]

let test_reply_header () =
  let module O = Octra_net.Oce1 in
  let module F = Octra_net.P2p_frame in
  let chain_id = "octra-test-reply-header" in
  let make_payload request_id =
    let prefix = O.encode (fun b ->
      O.put_string b chain_id;
      O.put_string b request_id;
      O.put_string b "ok";
      O.put_option O.put_string b None;
      O.put_u32_int b 1;
      O.put_u64 b 1L;
      List.iter (O.put_hash32 b) (List.init 3 (fun _ -> String.make 32 '\000'));
      O.put_list O.put_hash32 b [];
      O.put_u32_int b 1) in
    prefix ^ O.encode (fun b ->
      O.put_string b (String.make (8 * 1024 * 1024 - String.length prefix - 4) ' ')) in
  List.iter (fun msg_type ->
    List.iter (fun mode ->
      let driver = driver chain_id in
      let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let peer_id = Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1") in
      let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Observer
        local ~peer_id ~addr:"reply-header-test" ~direction:Octra_net.P2p_conn.Inbound in
      let request_id = C_hash.mark_complete_catchup_request (String.make 32 'r') in
      let replies = C_reply_window.create
        ~deadline:(if mode = "expired" then 0L else Int64.max_int)
        (if mode = "other" then ["another-peer"] else [peer_id]) in
      let replies = if mode <> "repeat" then replies else
        match C_reply_window.take ~now:0L ~peer:peer_id replies with
        | C_reply_window.Read value -> value | _ -> assert false in
      if mode <> "unknown" then Hashtbl.add driver.catchup_query_windows request_id
        C_driver.{from_epoch = 1L; max_epochs = 1; replies};
      let payload = if mode = "malformed" then "\000\000\001\000"
        else if mode = "chain" then
        let value = make_payload request_id in
        let changed = Bytes.of_string value in
        Bytes.set changed 4 'X'; Bytes.to_string changed
        else make_payload request_id in
      let allocated = Gc.allocated_bytes () in
      let start = Sys.time () in
      Lwt_main.run (C_driver.on_p2p_message driver conn F.{msg_type; payload});
      let cpu = Sys.time () -. start in
      let allocated = Gc.allocated_bytes () -. allocated in
      if allocated > 65_536. then failwith "unrequested reply decoded its records";
      if Hashtbl.length driver.catchup_responses <> 0 then failwith "unexpected reply was retained";
      let scores = Octra_net.P2p_swarm.peer_scores driver.swarm in
      if (scores <> []) <> (mode = "other" || mode = "chain" || mode = "malformed") then
        failwith "reply peer penalty differs";
      Printf.printf "event = reply_header mode = %s wire = %d bytes = %d allocated = %.0f cpu_s = %.6f\n"
        mode msg_type (String.length payload) allocated cpu;
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Octra_net.P2p_conn.close conn in Lwt_unix.close remote)
    ) ["unknown"; "other"; "expired"; "repeat"; "chain"; "malformed"]
  ) [F.msg_catchup_range_response; F.msg_catchup_range_response_v2]

let test_reply_valid () =
  let chain_id = "octra-test-reply-valid" in
  let driver = driver chain_id in
  let request_id = C_hash.mark_complete_catchup_request (String.make 32 'v') in
  let peer_id = Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1") in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
    local ~peer_id ~addr:"reply-valid-test" ~direction:Octra_net.P2p_conn.Inbound in
  let set_window () = Hashtbl.replace driver.catchup_query_windows request_id C_driver.{
    from_epoch = 1L; max_epochs = 1;
    replies = C_reply_window.create ~deadline:Int64.max_int [peer_id]} in
  let unsigned = C_codec.{chain_id; request_id; status = "not_found";
    error_code = Some "no_data"; records = []; next_epoch = None;
    responder_addr = "v1"; signature = ""} in
  let response = {unsigned with signature = sign "v1"
    (C_hash.catchup_range_response_complete_sign_bytes ~from_epoch:1L unsigned)} in
  let frame = Octra_net.P2p_frame.{msg_type = msg_catchup_range_response_v2;
    payload = C_codec.encode_catchup_range_response_v2 response} in
  set_window ();
  Lwt_main.run (C_driver.on_p2p_message driver conn frame);
  if List.length (Hashtbl.find driver.catchup_responses request_id) <> 1 then
    failwith "signed expected reply was lost";
  Lwt_main.run (C_driver.on_p2p_message driver conn frame);
  if Octra_net.P2p_swarm.peer_scores driver.swarm <> [] then failwith "repeat reply penalized";
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let record = C_codec.{epoch_id = 1L; prev_state_root = value.header.prev_state_root;
    state_root = value.header.proposed_state_root; tx_list_hash = value.header.tx_list_hash;
    tx_hashes = []; txs_json = []; receipt_root = value.header.receipt_root;
    receipts_json = []; epoch_ts = 1.; creator_addr = value.header.creator_addr;
    commit_round = 0; reward_source = Some C_types.{reward_proposer_addr = "v001";
      reward_proposer_public_key = Some (public_key "v1");
      reward_members = [{reward_address = "v001"; reward_public_key = Some (public_key "v1");
        reward_weight = Z.one}]};
    finality = Some {finalize = certificate value; validator_set}} in
  List.iter (fun complete ->
    set_window ();
    Hashtbl.remove driver.catchup_responses request_id;
    let unsigned = {unsigned with status = "ok"; error_code = None;
      records = [record]; next_epoch = Some 2L} in
    let bytes = if complete then
      C_hash.catchup_range_response_complete_sign_bytes ~from_epoch:1L unsigned
      else C_hash.catchup_range_response_sign_bytes ~chain_id ~request_id
        ~responder_addr:"v1" ~from_epoch:1L ~records_root:(C_hash.catchup_records_root [record]) in
    let payload = C_codec.encode_catchup_range_response_v2
      {unsigned with signature = sign "v1" bytes} in
    Lwt_main.run (C_driver.on_p2p_message driver conn {frame with payload});
    if not (Hashtbl.mem driver.catchup_responses request_id) then
      failwith "old or new signed record was lost"
  ) [false; true];
  set_window ();
  Hashtbl.remove driver.catchup_responses request_id;
  let invalid = {frame with payload = C_codec.encode_catchup_range_response_v2
    {response with signature = String.make 64 '\000'}} in
  Lwt_main.run (C_driver.on_p2p_message driver conn invalid);
  if Hashtbl.mem driver.catchup_responses request_id then failwith "bad signature reply accepted";
  if Octra_net.P2p_swarm.peer_scores driver.swarm = [] then failwith "bad signature not reported";
  Lwt_main.run (let open Lwt.Syntax in
    let* () = Octra_net.P2p_conn.close conn in Lwt_unix.close remote)

let test_reply_owners () =
  let chain_id = "octra-test-reply-owners" in
  let driver = driver chain_id in
  let records = [C_driver.{responder_addr = "v1";
    state_root = Some (String.make 32 '\001'); responder_head_epoch = 1L}] in
  let peer_id = Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1") in
  let check_owner requests =
    if Hashtbl.length requests <> 1 then failwith "finality request was not registered";
    Hashtbl.iter (fun id _ ->
      let window = Hashtbl.find driver.catchup_query_windows id in
      match C_reply_window.take ~now:0L ~peer:peer_id window.replies with
      | C_reply_window.Read _ -> ()
      | _ -> failwith "finality request lost its peer") requests in
  Lwt_main.run (C_driver.request_missing_finalize driver ~epoch_id:1L records);
  check_owner driver.finality_query_requests;
  C_driver.close_finality_query_windows driver;
  if Hashtbl.length driver.catchup_query_windows <> 0 then failwith "closed finality request retained";
  Lwt_main.run (C_driver.request_finality_proof driver ~epoch_id:1L records);
  check_owner driver.finality_proof_requests;
  C_driver.clear_finality_proof_requests driver;
  if Hashtbl.length driver.catchup_query_windows <> 0 then failwith "closed proof request retained"

let test_reply_local_error () =
  List.iter (fun (close_window, fail_lookup) ->
    let chain_id = "octra-test-reply-store" in
    let lookup, complete = Lwt.task () in
    let driver = driver ~load_scheduled_validator_set_config:(fun () -> lookup) chain_id in
    let request_id = C_hash.mark_complete_catchup_request (String.make 32 'l') in
    let peer_id = Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1") in
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      local ~peer_id ~addr:"reply-store-test" ~direction:Octra_net.P2p_conn.Inbound in
    Hashtbl.add driver.catchup_query_windows request_id C_driver.{
      from_epoch = 2L; max_epochs = 1;
      replies = C_reply_window.create ~deadline:Int64.max_int [peer_id]};
    let unsigned = C_codec.{chain_id; request_id; status = "not_found";
      error_code = Some "no_data"; records = []; next_epoch = None;
      responder_addr = "v1"; signature = ""} in
    let response = {unsigned with signature = sign "v1"
      (C_hash.catchup_range_response_complete_sign_bytes ~from_epoch:2L unsigned)} in
    let handled = C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
      msg_type = msg_catchup_range_response_v2;
      payload = C_codec.encode_catchup_range_response_v2 response} in
    if not (Lwt.is_sleeping handled) then failwith "reply did not await local lookup";
    if close_window then Hashtbl.remove driver.catchup_query_windows request_id;
    if fail_lookup then Lwt.wakeup_later_exn complete (Failure "local store unavailable")
    else Lwt.wakeup_later complete None;
    Lwt_main.run handled;
    if Octra_net.P2p_swarm.peer_scores driver.swarm <> [] then
      failwith "local read failure penalized the peer";
    if Hashtbl.mem driver.catchup_responses request_id <> (not fail_lookup && not close_window) then
      failwith "reply after lookup did not honor its window";
    Lwt_main.run (let open Lwt.Syntax in
      let* () = Octra_net.P2p_conn.close conn in Lwt_unix.close remote)
  ) [false, true; true, true; false, false; true, false]

let test_reply_query_cancel () =
  let chain_id = "octra-test-reply-cancel" in
  let driver = driver chain_id in
  let peer_id = Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1") in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
    local ~peer_id ~addr:"reply-cancel-test" ~direction:Octra_net.P2p_conn.Inbound in
  Hashtbl.add driver.swarm.peers peer_id conn;
  let pending = C_driver.query_catchup_range driver ~from_epoch:1L ~max_epochs:1
    ~timeout_seconds:120. ~validate:(fun _ -> true) ~prove:(fun _ -> false) in
  if Lwt_mvar.is_empty conn.write_queue then failwith "range query was not sent";
  let frame = Lwt_main.run (Lwt_mvar.take conn.write_queue) in
  let query = C_codec.decode_catchup_query_range frame.payload in
  let window = Hashtbl.find driver.catchup_query_windows query.request_id in
  (match C_reply_window.take ~now:(Mtime_clock.elapsed_ns ()) ~peer:peer_id window.replies with
   | C_reply_window.Read _ -> () | _ -> failwith "sent range query did not register peer");
  Lwt.cancel pending;
  Lwt_main.run (Lwt.catch
    (fun () -> let open Lwt.Syntax in let* _ = pending in Lwt.return_unit)
    (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error));
  if Hashtbl.length driver.catchup_query_windows <> 0
    || Hashtbl.length driver.catchup_responses <> 0 then failwith "cancelled range query retained state";
  Lwt_main.run (let open Lwt.Syntax in
    let* () = Octra_net.P2p_conn.close conn in Lwt_unix.close remote)

let test_catchup_keeps_reports active =
  let chain_id = "octra-test-round-reports" in
  let members = List.init 34 (fun index ->
    let address = "v" ^ string_of_int index in
    C_types.{address; pubkey = public_key address}) in
  let members = match C_types.make_weighted_validator_set (List.map (fun member -> member, Z.one) members) with
    | Ok value -> value | Error reason -> failwith reason in
  let policy = C_quorum_policy.make ~count_epoch:0L ~stake_epoch:(if active then Some 1L else None)
    ~round_epoch:(if active then Some 1L else None) in
  let base = driver chain_id in
  let driver = C_driver.create_for_policy policy ~config:base.config ~validator_set:members
    ~swarm:base.swarm ~start_height:1L ~sync_log:(C_sync_log.memory ())
    ~relief_log:(C_relief_log.memory ()) ~vote_log:(C_vote_log.memory ()) in
  driver.running <- true;
  C_engine.start_height driver.engine 1L;
  C_engine.realign_round driver.engine 117;
  let io = Octra_node_runtime.Consensus_catchup_shell.driver_io_of_driver driver in
  let local_fd, remote_fd = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Observer local_fd
    ~peer_id:"round-reports" ~addr:"198.51.100.29:19000"
    ~direction:Octra_net.P2p_conn.Inbound in
  for index = 1 to 12 do
    if not active && driver.engine.state.round < 117 then C_engine.realign_round driver.engine 117;
    let before_round = driver.engine.state.round in
    let validator = "v" ^ string_of_int index in
    let sync = C_codec.{chain_id; epoch_id = 1L; round = 140;
      step = C_types.ProposeStep; validator; request = false; signature = ""} in
    let sync = {sync with signature = sign validator (C_hash.round_sync_sign_bytes sync)} in
    if index = 12 then begin
      let invalid = {sync with signature = String.make 64 '\x00'} in
      Lwt_main.run (C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
        msg_type = msg_cons_round_sync; payload = C_codec.encode_round_sync invalid});
      if driver.engine.state.round <> before_round then
        failwith "invalid signature advanced the consensus round"
    end;
    Lwt_main.run (C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
      msg_type = msg_cons_round_sync; payload = C_codec.encode_round_sync sync});
    let expected = if index < 12 then 117 else 140 in
    if active && driver.engine.state.round <> expected then
      failwith (Printf.sprintf "round reports = %d got = %d expected = %d weight = %s"
        index driver.engine.state.round expected
        (Z.to_string (C_types.round_skip_weight driver.engine.vs)));
    let generation = driver.engine.generation in
    Lwt_main.run (io.advance_height 1L);
    Lwt_main.run (io.start_height 1L);
    if not active && driver.engine.generation <= generation then
      failwith "published catchup clock changed before activation"
  done;
  if members.quorum <> 23 || active && driver.engine.state.round <> 140 then
    failwith "round convergence changed the validator quorum";
  Lwt_main.run (C_driver.stop driver);
  Lwt_main.run (Octra_net.P2p_conn.close conn);
  Lwt_main.run (Lwt_unix.close remote_fd)

let test_query_service () =
  let module F = Octra_net.P2p_frame in
  let module N = Octra_net.P2p_conn in
  List.iter (fun msg_type -> List.iter (fun member ->
    let chain_id = "octra-test-query-service" in
    let base = driver chain_id in
    let reads = ref 0 and signs = ref 0 and epochs = ref 0 in
    let config = C_driver.{base.config with
      sign_fn = (fun bytes -> incr signs; sign "v0" bytes);
      lookup_bundle = (fun _ -> incr reads; Some ([], [], []));
      lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs ->
        incr reads; epochs := max_epochs; `NotFound)} in
    let driver = {base with C_driver.config} in
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let peer_id = if member then Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1")
      else "observer" in
    let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Observer local
      ~peer_id ~addr:"local" ~direction:N.Inbound in
    let request_id = C_hash.mark_complete_catchup_request (String.make 32 's') in
    let proposal_id = String.make 32 'b' in
    let query tag = F.{msg_type = tag; payload =
      if tag = msg_query_bundle then C_codec.encode_bundle_query
        C_codec.{chain_id; epoch_id = 1L; proposal_id}
      else C_codec.encode_catchup_query_range C_codec.{chain_id; request_id;
        from_epoch = 1L; max_epochs = 16}} in
    let pending = C_driver.on_p2p_message driver conn (query msg_type) in
    if !reads <> 1 || !signs <> 1 || not (Lwt.is_sleeping pending) then
      failwith "initial service did not retain write permit";
    for _ = 1 to 20 do
      List.iter (fun tag -> Lwt_main.run (C_driver.on_p2p_message driver conn (query tag)))
        [F.msg_query_bundle; F.msg_query_catchup_range; F.msg_query_catchup_range_v2]
    done;
    if !reads <> 1 || !signs <> 1 || List.length driver.query_work.active <> 1 then
      failwith "busy service performed lookup or signing";
    if msg_type <> F.msg_query_bundle && !epochs <> (if member then 16 else 4) then
      failwith "range request work differs";
    Lwt_main.run (let open Lwt.Syntax in
      let writer = N.write_loop conn in
      let* () = pending in
      let* frame = F.read_frame remote in
      if driver.query_work.active <> [] then failwith "written reply retained permit";
      if msg_type = F.msg_query_bundle then begin
        let reply = C_codec.decode_bundle_response frame.payload in
        if reply.proposal_id <> proposal_id || reply.txs_json <> []
           || not (verify "v0" (C_hash.bundle_response_sign_bytes reply) reply.signature)
        then failwith "bundle reply changed"
      end else begin
        let modern = msg_type = F.msg_query_catchup_range_v2 in
        let reply = (if modern then C_codec.decode_catchup_range_response_v2
          else C_codec.decode_catchup_range_response_v1) frame.payload in
        let bytes = if modern then C_hash.catchup_range_response_complete_sign_bytes
          ~from_epoch:1L reply else C_hash.catchup_range_response_sign_bytes
          ~chain_id ~request_id ~responder_addr:"v0" ~from_epoch:1L
          ~records_root:(C_hash.catchup_records_root_v1_wire []) in
        if reply.status <> "not_found" || reply.request_id <> request_id
           || not (verify "v0" bytes reply.signature)
        then failwith "range reply changed"
      end;
      let* () = N.close conn in let* () = writer in Lwt_unix.close remote)
  ) [false; true]) [F.msg_query_bundle; F.msg_query_catchup_range; F.msg_query_catchup_range_v2]

let test_query_readers () =
  let module F = Octra_net.P2p_frame in
  let module N = Octra_net.P2p_conn in
  let chain_id = "octra-test-query-readers" in
  let base = driver chain_id in
  let reads = ref 0 in
  let config = C_driver.{base.config with lookup_bundle = (fun id ->
    incr reads;
    if id = String.make 32 'h' then Some ([], [], [])
    else Some ([String.make 32 'x'], [String.make (1024 * 1024) 'x'], []))} in
  let driver = {base with C_driver.config} in
  let connection name =
    let fd, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt_int fd Unix.SO_SNDBUF 4096;
    N.create ~peer_class:Octra_net.P2p_frame_budget.Observer fd
      ~peer_id:name ~addr:"local" ~direction:N.Inbound, remote in
  let a, ar = connection "a" and b, br = connection "b" and c, cr = connection "c" in
  let request conn symbol = C_driver.on_p2p_message driver conn F.{
    msg_type = msg_query_bundle; payload = C_codec.encode_bundle_query
      C_codec.{chain_id; epoch_id = 1L; proposal_id = String.make 32 symbol}} in
  Lwt_main.run (let open Lwt.Syntax in
    let aw = N.write_loop a and bw = N.write_loop b in
    let ap = request a 'a' and bp = request b 'b' in
    let* () = Lwt.pause () in
    if not (Lwt.is_sleeping ap && Lwt.is_sleeping bp) then failwith "slow sockets completed";
    let notice = F.{msg_type = msg_tx_gossip; payload = "notice"} in
    let* () = N.send c notice in
    let cp = request c 'h' in
    if !reads <> 2 || not (Lwt.is_sleeping cp) then
      failwith "query did not wait for ordinary write";
    let cw = N.write_loop c in
    let* prior = F.read_frame cr in
    if prior.payload <> notice.payload then failwith "ordinary frame changed";
    let* () = cp in
    if !reads <> 3 then failwith "slow readers captured query lane";
    let* reply = F.read_frame cr in
    if (C_codec.decode_bundle_response reply.payload).txs_json <> [] then
      failwith "honest reader got different reply";
    let* () = N.close a in let* () = N.close b in let* () = N.close c in
    let* () = Lwt.join [ap; bp; aw; bw; cw] in
    if driver.query_work.active <> [] then failwith "closed readers retained query bytes";
    Lwt_list.iter_s Lwt_unix.close [ar; br; cr])

let test_query_exceptions () =
  List.iter (fun at_sign ->
    let base = driver "octra-test-query-errors" in
    let config = C_driver.{base.config with
      lookup_bundle = (fun _ -> if at_sign then Some ([], [], []) else failwith "lookup failed");
      sign_fn = (fun _ -> failwith "sign failed")} in
    let driver = {base with C_driver.config} in
    let fd, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Observer fd
      ~peer_id:"error-reader" ~addr:"local" ~direction:Octra_net.P2p_conn.Inbound in
    Lwt_main.run (let open Lwt.Syntax in
      let* () = C_driver.on_p2p_message driver conn Octra_net.P2p_frame.{
        msg_type = msg_query_bundle; payload = C_codec.encode_bundle_query
          C_codec.{chain_id = config.chain_id; epoch_id = 1L; proposal_id = String.make 32 'e'}} in
      if driver.query_work.active <> [] then failwith "query exception retained reservation";
      let* () = Octra_net.P2p_conn.close conn in Lwt_unix.close remote)) [false; true]

let test_query_cache () =
  let module F = Octra_net.P2p_frame in
  let module N = Octra_net.P2p_conn in
  let chain_id = "octra-test-query-cache" in
  let base = driver chain_id in
  let head = ref 1L and reads = ref 0 in
  let value = proposal chain_id ~epoch_id:1L ~round:0 in
  let record = C_codec.{epoch_id = 1L; prev_state_root = value.header.prev_state_root;
    state_root = value.header.proposed_state_root; tx_list_hash = value.header.tx_list_hash;
    tx_hashes = []; txs_json = []; receipt_root = value.header.receipt_root;
    receipts_json = []; epoch_ts = 1.; creator_addr = value.header.creator_addr;
    commit_round = 0; reward_source = Some C_types.{reward_proposer_addr = "v001";
      reward_proposer_public_key = Some (public_key "v1");
      reward_members = [{reward_address = "v001"; reward_public_key = Some (public_key "v1");
        reward_weight = Z.one}]};
    finality = Some {finalize = certificate value; validator_set}} in
  let config = C_driver.{base.config with local_head_epoch = (fun () -> !head);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ ->
      incr reads; `Ok ([record], Some 2L))} in
  let driver = {base with C_driver.config} in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey (public_key "v1"))
    ~addr:"local" ~direction:N.Inbound in
  Lwt_main.run (let open Lwt.Syntax in
    let writer = N.write_loop conn in
    let request index modern =
      let request_id = C_hash.mark_complete_catchup_request (String.make 31 'c' ^ string_of_int index) in
      let payload = C_codec.encode_catchup_query_range
        C_codec.{chain_id; request_id; from_epoch = 1L; max_epochs = 1} in
      let* () = C_driver.on_p2p_message driver conn F.{payload;
        msg_type = if modern then msg_query_catchup_range_v2 else msg_query_catchup_range} in
      let* reply = F.read_frame remote in
      let decoded = (if modern then C_codec.decode_catchup_range_response_v2
        else C_codec.decode_catchup_range_response_v1) reply.payload in
      if decoded.request_id <> request_id || decoded.status <> "ok"
         || List.length decoded.records <> 1 then failwith "cached query changed reply";
      Lwt.return_unit in
    let* () = request 1 true in let* () = request 2 true in
    if !reads <> 1 then failwith "range cache did not reuse immutable records";
    let* () = request 3 false in
    if !reads <> 2 then failwith "range cache crossed wire mode";
    head := 2L;
    let* () = request 4 true in
    if !reads <> 3 || List.length driver.query_cache > 2 then failwith "range cache ignored head";
    let* () = N.close conn in let* () = writer in Lwt_unix.close remote)

let test_reply_generation () =
  List.iter (fun same_height ->
    let chain_id = "octra-test-reply-generation" in
    let writes = ref 0 in
    let base = driver ~persist:(fun () -> incr writes; Lwt.return_true) chain_id in
    let node = {base with C_driver.config = {base.config with can_vote = (fun () -> false)}} in
    let value = proposal chain_id ~epoch_id:1L ~round:0 in
    C_engine.cache_proposal_message node.engine value;
    let pid = C_hash.proposal_id value.header in
    List.iter (fun validator ->
      let vote = signed_vote chain_id ~epoch_id:1L ~round:0 ~vote_type:C_types.Prevote
        ~proposal_id:pid validator in
      ignore (C_engine.add_vote ~policy:node.policy node.engine.prevotes vote
        ~validator_set:node.engine.vs)) ["v1"; "v2"];
    let vote = signed_vote chain_id ~epoch_id:1L ~round:0 ~vote_type:C_types.Precommit
      ~proposal_id:pid "v0" in
    ignore (C_engine.add_vote ~policy:node.policy node.engine.precommits vote
      ~validator_set:node.engine.vs);
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      local ~peer_id:"reply-generation" ~addr:"local" ~direction:Octra_net.P2p_conn.Inbound in
    Fun.protect ~finally:(fun () -> Lwt_main.run (let open Lwt.Syntax in
      let* () = Octra_net.P2p_conn.close conn in
      let* () = Lwt_unix.close remote in C_driver.stop node)) (fun () ->
      let response = C_driver.send_round_sync_response node conn ~requested_round:0 ~with_witness:false in
      if not (Lwt.is_sleeping response) then failwith "reply did not suspend on queue";
      if !writes <> 0 then failwith "test suspension occurred after persistence";
      if same_height then C_engine.realign_round node.engine 1
      else C_engine.start_height node.engine 2L;
      let fault = Some C_driver.{epoch_id = node.engine.state.height; round = 1; reason = "current fault"} in
      node.vote_fault <- fault;
      let proof = Some (node.engine.state.height, 1) in
      node.proof_lock <- proof;
      let received = ref [] in
      let rec drain turns =
        if turns = 0 then failwith "reply failed to finish";
        Option.iter (fun frame -> received := frame :: !received)
          (Lwt_mvar.take_available conn.write_queue);
        settle ();
        if Lwt.is_sleeping response then drain (turns - 1) else Lwt_main.run response in
      drain 16;
      if !writes <> 0 || node.vote_fault <> fault || node.proof_lock <> proof
         || Hashtbl.length node.durable_votes <> 0 then
        failwith (Printf.sprintf "old reply changed current vote state same_height = %b writes = %d fault = %b proof = %b durable = %d"
          same_height !writes (node.vote_fault = fault) (node.proof_lock = proof)
          (Hashtbl.length node.durable_votes));
      List.iter (fun frame ->
        if frame.Octra_net.P2p_frame.msg_type = Octra_net.P2p_frame.msg_cons_vote
           && (C_codec.decode_vote frame.payload).validator = "v0" then
          failwith "old reply emitted local vote") !received)) [false; true]

let test_precommit_generation () =
  List.iter (fun same_height -> List.iter (fun result ->
    let chain_id = "octra-test-precommit-generation" in
    let pending, finish = Lwt.wait () in
    let calls = ref 0 in
    let node = driver ~persist:(fun () -> incr calls; pending) chain_id in
    let value = proposal chain_id ~epoch_id:1L ~round:0 in
    C_engine.cache_proposal_message node.engine value;
    let vote = signed_vote chain_id ~epoch_id:1L ~round:0 ~vote_type:C_types.Precommit
      ~proposal_id:(C_hash.proposal_id value.header) "v0" in
    let saving = C_driver.saved_vote node vote in
    if not (Lwt.is_sleeping saving) || !calls <> 1 then failwith "save did not suspend";
    if same_height then C_engine.realign_round node.engine 1 else C_engine.start_height node.engine 2L;
    let fault = Some C_driver.{epoch_id = node.engine.state.height; round = 1; reason = "new fault"} in
    let proof = Some (node.engine.state.height, 1) in
    node.vote_fault <- fault;
    node.proof_lock <- proof;
    Lwt.wakeup finish result;
    if Lwt_main.run saving <> None then failwith "old save returned a live vote";
    if node.vote_fault <> fault || node.proof_lock <> proof
       || Hashtbl.length node.durable_votes <> 0 then
      failwith "old save changed current vote state";
    if C_vote_log.max_round node.vote_log ~chain_id ~validator:"v0" ~epoch_id:1L <> Ok None then
      failwith "old continuation wrote the vote journal";
    Lwt_main.run (C_driver.stop node)) [false; true]) [false; true]

let test_precommit_order () =
  List.iter (fun old_result -> List.iter (fun next_result ->
    let chain_id = "octra-devnet-9871-cluster" in
    let epoch_id = Option.get (C_quorum_policy.linear_epoch ~chain_id) in
    let members = C_types.make_weighted_validator_set
      (List.map (fun member -> member, Z.one) validator_set.validators) |> Result.get_ok in
    let pending = Queue.create () in
    let node = driver ~members ~persist:(fun () ->
      let waiting, finish = Lwt.wait () in Queue.add finish pending; waiting) chain_id in
    C_engine.start_height node.engine epoch_id;
    let generation = node.engine.generation in
    let save round =
      C_engine.start_round node.engine round;
      if node.engine.generation <> generation then failwith "ordinary round changed generation";
      let offer = proposal ~members chain_id ~epoch_id ~round in
      C_engine.cache_proposal_message node.engine offer;
      let pid = C_hash.proposal_id offer.header in
      List.iter (fun validator -> ignore (C_engine.add_vote ~policy:node.policy
        node.engine.prevotes (signed_vote chain_id ~epoch_id ~round
          ~vote_type:C_types.Prevote ~proposal_id:pid validator)
        ~validator_set:node.engine.vs)) ["v0"; "v1"; "v2"];
      Hashtbl.replace node.engine.polc_by_round round pid;
      let vote = signed_vote chain_id ~epoch_id ~round
        ~vote_type:C_types.Precommit ~proposal_id:pid "v0" in
      let saving = C_driver.saved_vote node vote in
      if not (Lwt.is_sleeping saving) then failwith "round save did not suspend";
      saving, Queue.take pending in
    let first, finish_first = save 0 in
    let next, finish_next = save 1 in
    Lwt.wakeup finish_next next_result;
    if Option.is_some (Lwt_main.run next) <> next_result then failwith "new save result differs";
    let fault = node.vote_fault in
    Lwt.wakeup finish_first old_result;
    if Option.is_some (Lwt_main.run first) <> old_result then failwith "historical save result differs";
    if node.vote_fault <> fault then failwith "old completion changed newer-round fault";
    let expected = if next_result then Some (epoch_id, 1)
      else if old_result then Some (epoch_id, 0) else None in
    if node.proof_lock <> expected then failwith "old completion replaced newer proof";
    List.iter (fun (round, durable) ->
      let votes = C_vote_log.round_votes node.vote_log ~chain_id ~validator:"v0" ~epoch_id ~round |> Result.get_ok in
      if (votes <> []) <> durable then failwith "journal differs from completed persistence")
      [0, old_result; 1, next_result];
    let latest, finish_latest = save 2 in
    let proofs = C_vote_log.polc_load ~policy:node.policy node.vote_log ~chain_id
      ~epoch_id ~validator_set:node.engine.vs |> Result.get_ok in
    Option.iter (fun (_, round) ->
      if not (List.exists (fun (vote : C_types.vote) -> vote.round = round) proofs) then
        failwith "incomplete save removed durable proof") expected;
    Lwt.wakeup finish_latest true;
    if Option.is_none (Lwt_main.run latest) then failwith "latest save failed";
    if Hashtbl.length node.pending_polc <> 0 then failwith "completed saves retained proof reservations";
    Lwt_main.run (C_driver.stop node)) [false; true]) [false; true]

let test_precommit_repeat () =
  List.iter (fun changed -> List.iter (fun (first_result, second_result) ->
    let chain_id = "octra-test-precommit-repeat" in
    let pending = Queue.create () in
    let node = driver ~persist:(fun () ->
      let waiting, finish = Lwt.wait () in Queue.add finish pending; waiting) chain_id in
    let offer = proposal chain_id ~epoch_id:1L ~round:0 in
    C_engine.cache_proposal_message node.engine offer;
    let vote = signed_vote chain_id ~epoch_id:1L ~round:0 ~vote_type:C_types.Precommit
      ~proposal_id:(C_hash.proposal_id offer.header) "v0" in
    let first = C_driver.saved_vote node vote in
    let finish_first = Queue.take pending in
    if changed then C_engine.realign_round node.engine 1;
    let second = C_driver.saved_vote node vote in
    let finish_second = Queue.take pending in
    Lwt.wakeup finish_second second_result;
    if Option.is_some (Lwt_main.run second) <> second_result then failwith "repeat save result differs";
    Lwt.wakeup finish_first first_result;
    let durable = second_result || first_result && not changed in
    if Option.is_some (Lwt_main.run first) <> (not changed && durable) then
      failwith "reordered save ignored generation or durable copy";
    if Hashtbl.length node.pending_polc <> 0 then failwith "repeat retained proof reservations";
    if durable then begin
      if node.vote_fault <> None then failwith "failed repeat replaced durable success";
      C_engine.start_round node.engine 2;
      let fault = Some C_driver.{epoch_id = 1L; round = 2; reason = "newer fault"} in
      node.vote_fault <- fault;
      if Lwt_main.run (C_driver.saved_vote node vote) <> Some vote then
        failwith "saved historical vote lost";
      if node.vote_fault <> fault then failwith "saved historical vote cleared newer fault"
    end;
    Lwt_main.run (C_driver.stop node)) [true, false; false, true; true, true]) [false; true]

let () = if Array.length Sys.argv = 2 && Sys.argv.(1) = "generation" then begin
  test_reply_generation ();
  test_precommit_generation ();
  test_precommit_order ();
  test_precommit_repeat ();
  print_endline "status = pass test = reply_generation"
end else begin
  test_reply_generation ();
  test_precommit_generation ();
  test_precommit_order ();
  test_precommit_repeat ();
  test_query_readers ();
  test_query_exceptions ();
  test_query_service ();
  test_query_cache ();
  test_vote_round_restart ();
  List.iter test_past_vote_reply ["saved"; "absent"; "failed"; "invalid"; "proof"];
  test_progress_clock ();
  test_locked_round_restart ();
  test_polc_round_restart ();
  test_late_quorum_offer ();
  test_future_vote_replay ~active:true ();
  test_relief_wait_plan ();
  test_relief_plan_race ();
  test_catchup_keeps_round ();
  List.iter test_catchup_keeps_reports [false; true];
  test_reply_header ();
  test_reply_valid ();
  test_reply_owners ();
  test_reply_local_error ();
  test_reply_query_cancel ();
  test_future_relay ();
  test_relay_backpressure ();
  test_wait_order ();
  test_wait_jump ();
  test_future_wait ();
  test_wait_capacity ();
  test_frame_finality ();
  test_frame_round ();
  test_work_slot ();
  test_check_handoff ();
  test_reply_grace ();
  test_check_flow ();
  test_build_flow ();
  test_work_timer ();
  test_check_invalid ();
  test_build_error ();
  test_pace_votes ();
  test_qc_vote ();
  test_proposal_round_replay ();
  test_proposal_height_replay ();
  test_unretained_replay ();
  test_future_vote_replay ();
  test_vote_conflict_retained ();
  test_activation_vote_replay ();
  test_activation_vote_signer ();
  test_waiting_proposal_retry ();
  test_verify_error_retry ();
  Printf.printf "status = pass test = bft_ready_replay\n%!"
end