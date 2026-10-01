(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module D = Octra_consensus.C_driver
module E = Octra_consensus.C_engine
module T = Octra_consensus.C_types
module Swarm = Octra_net.P2p_swarm
module Q = Octra_consensus.C_root_query

let expect label value = if not value then failwith label
let forbid _ = failwith "signing is not part of the query test"
let chain_id = "octra-test-query"
let address = "octQuery"
let pubkey = String.make 32 '\001'
let root = String.make 32 '\002'

let make ?(head = fun () -> 40L) () =
  let swarm = Swarm.create {
    listen_port = 0; chain_id;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey pubkey;
    node_addr = address; pubkey_raw = pubkey;
    consensus_config_hash = String.make 32 '\000';
    binary_hash = String.make 32 '\000'; require_binary_hash = false;
    upgrade_plan = None; profile_plan = []; allowed_pubkeys = [];
    bootstrap_peers = []; max_peers = 4; sign_fn = forbid;
    best_epoch_fn = (fun () -> 40L); best_root_fn = (fun () -> root);
  } in
  let validator_set = E.make_validator_set
    (List.init 4 (fun index -> T.{address = "octQuery" ^ string_of_int index;
      pubkey = String.make 32 (Char.chr (index + 1))})) in
  let config = D.{
    chain_id; my_addr = address; sign_fn = forbid;
    verify_fn = (fun _ _ _ -> failwith "signature verification is not part of this test");
    role_can_vote = (fun () -> false); can_vote = (fun () -> false);
    execute_fn = (fun _ -> failwith "query executed a proposal");
    verify_proposal = (fun _ -> failwith "query checked a proposal");
    verify_parent_commit = (fun ~epoch_id:_ _ -> Error "not a proposal test");
    on_finalized = (fun ~validator_set:_ _ -> failwith "query finalized an epoch");
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
      ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ ->
      failwith "query attempted a vote");
    lookup_epoch_root = (fun _ -> Some root);
    local_head_epoch = head;
    lookup_bundle = (fun _ -> None);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
    on_resource_attestation = (fun _ -> Lwt.return_unit);
    scheduled_validator_set_config = None;
    load_scheduled_validator_set_config = (fun () -> Lwt.return_none);
    resource_committee_config = None;
  } in
  D.create ~config ~validator_set ~swarm ~start_height:41L
    ~sync_log:(Octra_consensus.C_sync_log.memory ())
    ~relief_log:(Octra_consensus.C_relief_log.memory ())
    ~vote_log:(Octra_consensus.C_vote_log.memory ())

let reply = D.{responder_addr = "octQuery1"; responder_head_epoch = 41L;
  state_root = Some root}

let query ?(epoch = 40L) ?(wait_for = D.Source_agreement)
    ?(request_next = false) driver seconds =
  D.query_epoch_root ~wait_for ~request_next driver ~epoch_id:epoch ~timeout_seconds:seconds

let listening driver epoch = Q.listening ~epoch driver.D.epoch_root_queries

let add ?(epoch = 40L) driver reply =
  driver.D.epoch_root_queries <- Q.add ~epoch
    ~same:(fun (a : D.epoch_root_response_record) b ->
      String.equal a.responder_addr b.responder_addr)
    reply driver.D.epoch_root_queries

let cancelled promise =
  let open Lwt.Syntax in
  Lwt.cancel promise;
  let* () = Lwt.pause () in
  expect "query did not propagate cancellation"
    (match Lwt.state promise with Lwt.Fail Lwt.Canceled -> true | _ -> false);
  Lwt.return_unit

let joined () =
  let open Lwt.Syntax in
  let driver = make () in
  let first = query driver 0.3 in
  add driver reply;
  let second = query driver 0.1 in
  let* left, right = Lwt.both first second in
  expect "concurrent queries lost validated responses" (left = [reply] && right = [reply]);
  expect "finished queries retained slot" (not (listening driver 40L));
  Lwt.return_unit

let cancel_one () =
  let open Lwt.Syntax in
  let driver = make () in
  let first = query driver 10. in
  add driver reply;
  let second = query driver 0.2 in
  let* () = cancelled first in
  expect "cancel removed another reader" (listening driver 40L);
  let* result = second in
  expect "cancel erased another reader replies" (result = [reply]);
  expect "last reader retained slot" (not (listening driver 40L));
  Lwt.return_unit

let cancel_all () =
  let open Lwt.Syntax in
  let driver = make () in
  let first = query driver 10. in
  let second = query driver 10. in
  add driver reply;
  let* () = cancelled first in
  let* () = cancelled second in
  expect "cancelled readers retained slot" (not (listening driver 40L));
  let next = query driver 0.1 in
  Lwt.cancel first;
  Lwt.cancel second;
  let* result = next in
  expect "reopened query retained previous replies" (result = []);
  expect "reopened query retained slot" (not (listening driver 40L));
  Lwt.return_unit

exception Head_read

let read_error () =
  let open Lwt.Syntax in
  let fail = ref true in
  let driver = make ~head:(fun () ->
    if !fail then (fail := false; raise Head_read) else 40L) () in
  let first = query driver 0.1 in
  add driver reply;
  let second = query driver 0.3 in
  let* refused = Lwt.catch
    (fun () -> let* _ = first in Lwt.return_false)
    (function Head_read -> Lwt.return_true | exn -> Lwt.fail exn) in
  expect "query suppressed head failure" refused;
  expect "failed query removed another reader" (listening driver 40L);
  let* result = second in
  expect "failed query erased replies" (result = [reply]);
  expect "failed query retained slot" (not (listening driver 40L));
  Lwt.return_unit

let epochs () =
  let open Lwt.Syntax in
  let driver = make () in
  let first = query driver 0.1 in
  let second = query ~epoch:39L driver 0.2 in
  add driver reply;
  let other = { reply with D.state_root = Some (String.make 32 '\003') } in
  add ~epoch:39L driver other;
  let* left, right = Lwt.both first second in
  expect "queries shared different epoch replies" (left = [reply] && right = [other]);
  expect "different epochs retained slots" (not (listening driver 40L || listening driver 39L));
  Lwt.return_unit

let wait_modes () =
  let open Lwt.Syntax in
  let driver = make () in
  let source = query driver 3. in
  let quorum = query ~wait_for:D.Consensus_quorum driver 3. in
  add driver reply;
  add driver { reply with D.responder_addr = "octQuery2" };
  let* result = source in
  expect "source agreement lost replies" (List.length result = 2);
  expect "source agreement ended quorum query" (Lwt.is_sleeping quorum);
  add driver { reply with D.responder_addr = "octQuery3" };
  let* result = quorum in
  expect "quorum query lost replies" (List.length result = 3);
  expect "wait modes retained slot" (not (listening driver 40L));
  Lwt.return_unit

let request_mode enabled () =
  let open Lwt.Syntax in
  let driver = make () in
  let pending = query ~request_next:enabled driver 0.1 in
  add driver reply;
  let* result = pending in
  expect "request mode erased replies" (result = [reply]);
  let sent = match driver.D.finality_query with
    | Octra_consensus.C_finality_query.Sent _ -> true
    | Octra_consensus.C_finality_query.Idle -> false in
  expect "request mode changed" (sent = enabled);
  expect "request mode retained slot" (not (listening driver 40L));
  Lwt.return_unit

let with_queue run =
  let open Lwt.Syntax in
  let driver = make () in
  let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let conn = Octra_net.P2p_conn.create
    ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:"query-test" ~addr:"query-test" ~direction:Octra_net.P2p_conn.Inbound in
  Lwt.finalize
    (fun () ->
      let frame = Octra_net.P2p_frame.{msg_type = msg_query_epoch_root; payload = ""} in
      let* () = Lwt_mvar.put conn.write_queue frame in
      Hashtbl.add driver.D.swarm.peers conn.peer_id conn;
      run driver conn frame)
    (fun () ->
      Hashtbl.clear driver.D.swarm.peers;
      Octra_net.P2p_conn.close conn)

let cancel_send () = with_queue (fun driver _ _ ->
  let open Lwt.Syntax in
  let pending = query driver 10. in
  Lwt.finalize
    (fun () ->
      expect "send did not wait for queue" (Lwt.is_sleeping pending);
      let* () = cancelled pending in
      expect "cancelled send retained reader" (not (listening driver 40L));
      Lwt.return_unit)
    (fun () -> Lwt.cancel pending; Lwt.return_unit))

let cancel_send_joined () = with_queue (fun driver conn _ ->
  let open Lwt.Syntax in
  let first = query driver 10. in
  let second = query driver 0.2 in
  Lwt.finalize
    (fun () ->
      let* () = cancelled first in
      expect "cancelled send removed other reader" (listening driver 40L);
      let* _ = Lwt_mvar.take conn.write_queue in
      add driver reply;
      let* result = second in
      expect "cancelled send erased replies" (result = [reply]);
      expect "finished sends retained readers" (not (listening driver 40L));
      Lwt.return_unit)
    (fun () -> List.iter Lwt.cancel [first; second]; Lwt.return_unit))

let send_timeout () = with_queue (fun driver conn frame ->
  let open Lwt.Syntax in
  let* () = Swarm.send_with_timeout conn frame 0.01 in
  let* queued = Lwt_mvar.take conn.write_queue in
  expect "send timeout replaced queued frame" (queued = frame);
  expect "send timeout changed reader pool" (not (listening driver 40L));
  let* () = Lwt.pause () in
  expect "send timeout retained pending enqueue" (Lwt_mvar.is_empty conn.write_queue);
  Lwt.return_unit)

let () =
  Lwt_main.run (Lwt_list.iter_s (fun (name, run) ->
    let open Lwt.Syntax in
    let* () = run () in
    Printf.printf "event = root_query case = %s status = pass\n%!" name;
    Lwt.return_unit)
    ["joined", joined; "cancel_one", cancel_one; "cancel_all", cancel_all;
     "read_error", read_error; "epochs", epochs; "wait_modes", wait_modes;
     "request_disabled", request_mode false; "request_enabled", request_mode true;
     "send_timeout", send_timeout; "cancel_send", cancel_send;
     "cancel_send_joined", cancel_send_joined])