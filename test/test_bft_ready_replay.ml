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

let proposal chain_id ~epoch_id ~round =
  let proposer =
    (C_engine.leader_of validator_set ~epoch_id ~round).address
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
  let value = C_driver.create
    ~config
    ~validator_set
    ~swarm:(swarm chain_id)
    ~start_height:1L
    ~sync_log:(C_sync_log.memory ())
    ~relief_log:(C_relief_log.memory ())
    ~vote_log:(C_vote_log.memory ())
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

let test_deferred_proposal_replays_after_round_skip () =
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

let test_pending_proposal_replays_after_height_advance () =
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

let test_unretained_proposal_reenters_after_height_advance () =
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

let test_future_votes_replay_after_height_advance () =
  let chain_id = "octra-test-future-votes" in
  let driver = driver chain_id in
  let next_proposal = proposal chain_id ~epoch_id:2L ~round:0 in
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

let test_future_vote_conflict_is_retained () =
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

let test_activation_vote_reenters_after_set_resolution () =
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

let test_activation_vote_evidence_targets_signer () =
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

let test_waiting_proposal_retries_without_penalty () =
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

let test_proposal_verify_error_waits_without_penalty () =
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

let () =
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
  test_deferred_proposal_replays_after_round_skip ();
  test_pending_proposal_replays_after_height_advance ();
  test_unretained_proposal_reenters_after_height_advance ();
  test_future_votes_replay_after_height_advance ();
  test_future_vote_conflict_is_retained ();
  test_activation_vote_reenters_after_set_resolution ();
  test_activation_vote_evidence_targets_signer ();
  test_waiting_proposal_retries_without_penalty ();
  test_proposal_verify_error_waits_without_penalty ();
  Printf.printf "status = pass test = bft_ready_replay\n%!"