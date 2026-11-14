(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module D = Octra_consensus.C_driver
module E = Octra_consensus.C_engine
module T = Octra_consensus.C_types
module Swarm = Octra_net.P2p_swarm
module Q = Octra_consensus.C_root_query
module P = Octra_consensus.C_quorum_policy

let () =
  Octra_node_runtime.Status_read_rpc.sync_reader ();
  match Array.to_list Sys.argv with
  | [_; "--sync-test"; parent; action] ->
    Octra_node_runtime.Sync_probe.child (int_of_string parent) (fun () ->
      match action with
      | "sleep" -> Unix.sleep 10; "late"
      | "ready" -> "ready"
      | "fail" -> failwith "read refused"
      | "size" -> String.make (8 * 1024 * 1024 + 1) 'x'
      | "heap" -> string_of_int (Gc.quick_stat ()).Gc.heap_words
      | "fds" ->
        let path = if Sys.file_exists "/proc/self/fd" then "/proc/self/fd" else "/dev/fd" in
        Sys.readdir path |> Array.to_list
        |> List.filter (fun name -> Sys.file_exists (Filename.concat path name))
        |> List.sort String.compare |> String.concat ","
      | _ -> failwith "unknown child action")
  | _ -> ()

let expect label value = if not value then failwith label
let forbid _ = failwith "signing is not part of the query test"
let chain_id = "octra-test-query"
let address = "octQuery"
let pubkey = String.make 32 '\001'
let root = String.make 32 '\002'

let validators = E.make_validator_set
  (List.init 4 (fun index -> T.{address = "octQuery" ^ string_of_int index;
    pubkey = String.make 32 (Char.chr (index + 1))}))

let weighted_set entries = match T.make_weighted_validator_set entries with
  | Ok value -> value | Error reason -> failwith reason

let make ?(head = fun () -> 40L) ?(policy = P.for_chain chain_id)
    ?(validator_set = validators) () =
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
  D.create_for_policy policy ~config ~validator_set ~swarm ~start_height:41L
    ~sync_log:(Octra_consensus.C_sync_log.memory ())
    ~relief_log:(Octra_consensus.C_relief_log.memory ())
    ~vote_log:(Octra_consensus.C_vote_log.memory ())

let reply = D.{responder_addr = "octQuery1"; responder_head_epoch = 41L;
  state_root = Some root}

let query ?(epoch = 40L) ?(wait_for = D.Source_agreement)
    ?(request_next = false) ?(fresh = false) driver seconds =
  D.query_epoch_root ~wait_for ~request_next ~fresh driver
    ~epoch_id:epoch ~timeout_seconds:seconds

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

let with_bundle action =
  let open Lwt.Syntax in
  let module H = Octra_consensus.C_hash in
  let module Codec = Octra_consensus.C_codec in
  let secret = String.make 32 '\029' in
  let public_key = match Mirage_crypto_ec.Ed25519.priv_of_octets secret with
    | Error _ -> failwith "invalid bundle test key"
    | Ok key -> Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let validator_set = T.make_validator_set
    [T.{address = reply.responder_addr; pubkey = public_key}] in
  let driver = make ~validator_set () in
  let proposal_id = String.make 32 '\030' in
  let expected = D.{responder_addr = reply.responder_addr;
    tx_hashes = []; txs_json = []; receipts_json = []} in
  let query ?(epoch = 40L) ?(seconds = 1.) ?(id = proposal_id) validate =
    D.query_bundle driver ~epoch_id:epoch ~proposal_id:id ~timeout_seconds:seconds ~validate in
  let receive ?(epoch = 40L) () =
    let response = Codec.{chain_id; epoch_id = epoch; proposal_id;
      responder_addr = expected.responder_addr; tx_hashes = []; txs_json = [];
      receipts_json = []; signature = ""} in
    let response = {response with signature = H.sign_ed25519 ~priv_raw:secret
      ~msg:(H.bundle_response_sign_bytes response)} in
    let frame = Octra_net.P2p_frame.{msg_type = msg_bundle_response;
      payload = Codec.encode_bundle_response response} in
    let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let conn = Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Validator fd
      ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey public_key)
      ~addr:"bundle-query-test" ~direction:Octra_net.P2p_conn.Inbound in
    Lwt.finalize (fun () -> D.on_p2p_message driver conn frame)
      (fun () -> Octra_net.P2p_conn.close conn) in
  let+ () = action query receive expected in
  expect "finished bundle query retained slot" (Hashtbl.length driver.bundle_responses = 0)

let bundle_join () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let first = query ~seconds:10. (fun _ -> false) in
  let* () = receive () in
  let* result = query (fun _ -> true) in
  let* () = cancelled first in
  expect "joined bundle query erased replies" (result = Some expected);
  Lwt.return_unit)

let bundle_cancel () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let first = query ~seconds:10. (fun _ -> false) in
  let second = query (fun _ -> true) in
  let* () = cancelled first in
  let* () = receive () in
  let* result = second in
  expect "bundle cancellation erased another reader" (result = Some expected);
  let next = query (fun _ -> true) in
  Lwt.cancel first;
  let+ result = next in
  expect "reopened bundle reused prior replies" (result = None))

let bundle_expire () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let first = query ~seconds:0.01 (fun _ -> false) in
  let second = query (fun _ -> true) in
  let* result = first in
  expect "empty bundle query did not expire" (result = None);
  let* () = receive () in
  let+ result = second in
  expect "bundle expiry erased another reader" (result = Some expected))

let bundle_epoch () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let pending = query (fun _ -> true) in
  let* wrong = query ~epoch:41L (fun _ -> true) in
  expect "bundle query joined a different epoch" (wrong = None);
  let* () = receive ~epoch:41L () in
  let* () = Lwt_unix.sleep 0.11 in
  expect "bundle query accepted another epoch" (Lwt.is_sleeping pending);
  let* () = receive () in
  let+ result = pending in
  expect "wrong epoch removed active bundle query" (result = Some expected))

let bundle_readers () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let pending = List.init 64 (fun _ -> query ~seconds:10. (fun _ -> true)) in
  let extra = query (fun _ -> true) in
  expect "bundle reader capacity was exceeded"
    (match Lwt.state extra with Lwt.Return None -> true | _ -> false);
  let* () = receive () in
  let+ results = Lwt.all pending in
  expect "bundle reader limit erased pending results"
    (List.for_all (fun result -> result = Some expected) results))

let bundle_slots () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let pending = List.init 64 (fun index ->
    query ~id:(String.make 32 (Char.chr index)) ~seconds:10. (fun _ -> true)) in
  let extra = query ~id:(String.make 32 '\064') (fun _ -> true) in
  expect "bundle slot capacity was exceeded"
    (match Lwt.state extra with Lwt.Return None -> true | _ -> false);
  let* () = Lwt_list.iter_s cancelled pending in
  let next = query (fun _ -> true) in
  let* () = receive () in
  let+ result = next in
  expect "bundle slot capacity was not released" (result = Some expected))

let bundle_exception () = with_bundle (fun query receive expected ->
  let open Lwt.Syntax in
  let first = query (fun _ -> failwith "bundle check refused") in
  let second = query (fun _ -> true) in
  let* () = receive () in
  let* refused = Lwt.catch (fun () -> let+ _ = first in false)
    (function Failure reason when reason = "bundle check refused" -> Lwt.return_true
      | exn -> Lwt.fail exn) in
  expect "bundle check failure was hidden" refused;
  let+ result = second in
  expect "bundle check failure removed another reader" (result = Some expected))

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

let fresh_overlap () =
  let open Lwt.Syntax in
  let driver = make () in
  let first = query ~wait_for:D.Collection_window driver 0.1 in
  add driver reply;
  let fresh = query ~fresh:true ~wait_for:D.Collection_window driver 0.1 in
  let shared = query ~wait_for:D.Collection_window driver 0.1 in
  let* results = Lwt.all [first; fresh; shared] in
  expect "fresh query reused shared replies" (results = [[reply]; []; [reply]]);
  expect "fresh overlap retained slot" (not (listening driver 40L));
  Lwt.return_unit

let fresh_same_signer () =
  let open Lwt.Syntax in
  let driver = make () in
  let shared = query ~wait_for:D.Collection_window driver 0.1 in
  add driver reply;
  let first = query ~fresh:true ~wait_for:D.Collection_window driver 0.1 in
  let changed = {reply with D.responder_head_epoch = 42L;
    state_root = Some (String.make 32 '\003')} in
  add driver changed;
  let second = query ~fresh:true ~wait_for:D.Collection_window driver 0.1 in
  let latest = {changed with D.responder_head_epoch = 43L} in
  add driver latest;
  let third = query ~fresh:true ~wait_for:D.Collection_window driver 0.1 in
  add driver latest;
  let* results = Lwt.all [shared; first; second; third] in
  expect "fresh readers lost their first signer reply"
    (results = [[reply]; [changed]; [latest]; [latest]]);
  expect "fresh signer queries retained slot" (not (listening driver 40L));
  Lwt.return_unit

let fresh_cancel () =
  let open Lwt.Syntax in
  let driver = make () in
  let shared = query ~wait_for:D.Collection_window driver 10. in
  add driver reply;
  let first = query ~fresh:true ~wait_for:D.Collection_window driver 10. in
  let second = query ~fresh:true ~wait_for:D.Collection_window driver 0.1 in
  let changed = {reply with D.responder_head_epoch = 42L} in
  add driver changed;
  let* () = cancelled shared in
  let* () = cancelled first in
  expect "fresh cancellation removed another reader" (listening driver 40L);
  let* replies = second in
  expect "fresh cancellation erased another reader" (replies = [changed]);
  expect "fresh cancellation retained slot" (not (listening driver 40L));
  let next = query ~fresh:true ~wait_for:D.Collection_window driver 0.1 in
  Lwt.cancel shared;
  Lwt.cancel first;
  let* replies = next in
  expect "fresh query reopened with prior replies" (replies = []);
  expect "reopened fresh query retained slot" (not (listening driver 40L));
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

let collection_window () =
  let open Lwt.Syntax in
  let driver = make () in
  let replies = List.map (fun member ->
    {reply with D.responder_addr = member.T.address}) validators.T.validators in
  expect "collection window ended on quorum"
    (not (D.epoch_root_wait_reached driver ~epoch_id:40L
      ~wait_for:D.Collection_window replies));
  expect "window test did not contain quorum"
    (D.epoch_root_wait_reached driver ~epoch_id:40L
      ~wait_for:D.Consensus_quorum replies);
  let started = Mtime_clock.elapsed_ns () in
  let pending = query ~fresh:true ~wait_for:D.Collection_window driver 0.2 in
  List.iter (add driver) replies;
  let* result = pending in
  expect "collection window returned before its deadline"
    (Int64.sub (Mtime_clock.elapsed_ns ()) started >= 200_000_000L);
  expect "collection window lost replies" (result = List.rev replies);
  expect "collection window retained slot" (not (listening driver 40L));
  Lwt.return_unit

let large_roster () =
  let open Lwt.Syntax in
  let validator_set = T.make_validator_set (List.init 385 (fun index ->
    T.{address = Printf.sprintf "octLarge%03d" index;
      pubkey = String.init 32 (fun byte ->
        if byte = 0 then Char.chr (index land 255)
        else if byte = 1 then Char.chr (index lsr 8) else '\001')})) in
  expect "large roster quorum changed" (validator_set.n = 385 && validator_set.quorum = 257);
  let driver = make ~validator_set () in
  let replies = validator_set.T.validators
    |> List.filteri (fun index _ -> index < 257)
    |> List.map (fun member -> {reply with D.responder_addr = member.T.address}) in
  let partial = List.filteri (fun index _ -> index < 256) replies in
  expect "large roster accepted 256 replies"
    (not (D.epoch_root_wait_reached driver ~epoch_id:40L ~wait_for:D.Consensus_quorum partial));
  let shared = query ~wait_for:D.Consensus_quorum driver 0.2 in
  let fresh = query ~fresh:true ~wait_for:D.Consensus_quorum driver 0.2 in
  List.iter (add driver) replies;
  let* results = Lwt.all [shared; fresh] in
  List.iter (fun responses ->
    expect "large roster truncated valid replies" (List.length responses = 257);
    expect "large roster did not retain consensus quorum"
      (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root responses)) results;
  expect "large roster retained query slot" (not (listening driver 40L));
  Lwt.return_unit

let edge (weights : D.root_weights) value =
  weights.ahead value && not (weights.ahead (Z.pred value))

let weight_snapshot () =
  let policy = P.make ~count_epoch:20L ~stake_epoch:(Some 30L) ~round_epoch:None in
  let entries = List.mapi (fun index member ->
    member, Z.of_int (if index = 0 then 100 else 1)) validators.T.validators in
  let validator_set = match T.make_weighted_validator_set entries with
    | Ok value -> value | Error reason -> failwith reason in
  let driver = make ~policy ~validator_set () in
  let read epoch = match D.epoch_root_weights driver ~epoch_id:epoch with
    | Some value -> value | None -> failwith "root weights unavailable" in
  let prior = read 10L in
  let capped = read 20L in
  let stake = read 40L in
  expect "root weight snapshot lost its epoch"
    (prior.epoch_id = 10L && capped.epoch_id = 20L && stake.epoch_id = 40L);
  expect "root weight snapshot lost its attester counts"
    (List.for_all (fun (snapshot : D.root_weights) ->
      snapshot.required ~configured:0 = 2 && snapshot.required ~configured:max_int = 3)
      [prior; capped; stake]);
  expect "Stake root weight threshold differs"
    (edge stake (Z.of_int 35));
  expect "Prior root evidence required a count floor"
    (prior.agrees ~count:1 ~weight:(Z.of_int 100));
  expect "Stake root evidence ignored its count floor"
    (not (stake.agrees ~count:1 ~weight:(Z.of_int 100)));
  expect "Stake root evidence rejected sufficient support"
    (stake.agrees ~count:2 ~weight:(Z.of_int 101));
  expect "root evidence accepted insufficient weight"
    (not (stake.agrees ~count:4 ~weight:(Z.of_int 4)));
  expect "root weights counted an unknown signer" (stake.weight "absent" = None);
  let effective = T.validator_set_for_epoch ~policy ~chain_id ~epoch_id:20L validator_set in
  expect "root weights ignored the effective set"
    (capped.weight "octQuery0" = T.weight_of_addr effective "octQuery0");
  expect "root weight threshold ignored the effective set"
    (edge capped (T.round_skip_weight effective));
  driver.D.stake_vs <- validators;
  expect "Stake root weights used unit weights"
    (Option.is_none (D.epoch_root_weights driver ~epoch_id:40L));
  expect "Prior root weights rejected a unit set"
    (Option.is_some (D.epoch_root_weights driver ~epoch_id:10L));
  expect "root weights changed after capture"
    (stake.weight "octQuery0" = Some (Z.of_int 100)
      && stake.agrees ~count:2 ~weight:(Z.of_int 101)
      && not (stake.agrees ~count:4 ~weight:(Z.of_int 4)));
  let unit_set = match T.make_weighted_validator_set
    (List.map (fun member -> member, Z.one) validators.T.validators) with
    | Ok value -> value | Error reason -> failwith reason in
  driver.D.stake_vs <- unit_set;
  let changed = read 40L in
  expect "root weight stamp ignored set changes" (stake.stamp <> changed.stamp);
  expect "root weight stamp ignored rule changes"
    ((read 10L).stamp <> (read 20L).stamp && (read 20L).stamp <> changed.stamp);
  expect "new root weights retained the prior threshold"
    (changed.agrees ~count:2 ~weight:(Z.of_int 2)
      && edge changed (Z.of_int 2)
      && edge stake (Z.of_int 35));
  driver.D.stake_vs <- validator_set;
  expect "root weight stamp was not repeatable" ((read 40L).stamp = stake.stamp);
  Lwt.return_unit

let source_reachability () =
  let policy = P.make ~count_epoch:20L ~stake_epoch:(Some 30L) ~round_epoch:None in
  List.iter (fun (n, faults, quorum, floor, reachable) ->
    let validator_set = weighted_set (List.init n (fun index ->
      T.{address = Printf.sprintf "octReach%03d" index;
        pubkey = String.make 32 (Char.chr (index + 1))},
      Z.of_int (if index < 32 then 100 else 1))) in
    expect "reachability roster changed"
      (validator_set.n = n && validator_set.f = faults
        && validator_set.quorum = quorum && faults + 1 = floor);
    let driver = make ~policy ~validator_set () in
    let read epoch = match D.epoch_root_weights driver ~epoch_id:epoch with
      | Some value -> value | None -> failwith "reachability weights unavailable" in
    let stake = read 40L in
    let prior = read 10L in
    let members count = List.filteri (fun index _ -> index < count)
      validator_set.T.validators in
    let weight (snapshot : D.root_weights) members =
      List.fold_left (fun total member ->
        match snapshot.weight member.T.address with
        | Some value -> Z.add total value
        | None -> failwith "reachability identity lost its weight") Z.zero members in
    let connected = members 32 in
    let connected_weight = weight stake connected in
    expect "reachability connected identity count changed" (List.length connected = 32);
    expect "reachability weights cannot isolate the count floor"
      (Z.geq connected_weight validator_set.quorum_weight);
    expect "Stake source reachability changed"
      (stake.agrees ~count:(List.length connected) ~weight:connected_weight = reachable);
    let sufficient = members floor in
    expect "Stake source floor rejected sufficient weight"
      (stake.agrees ~count:(List.length sufficient) ~weight:(weight stake sufficient));
    let fewer = members (floor - 1) in
    expect "Prior source evidence acquired a Stake count floor"
      (prior.agrees ~count:(List.length fewer) ~weight:(weight prior fewer));
    expect "Stake source floor was reduced to connected peers"
      (not (stake.agrees ~count:(List.length fewer) ~weight:(weight stake fewer)));
    List.iter (fun count ->
      expect "all weight bypassed the Stake source count floor"
        (not (stake.agrees ~count ~weight:validator_set.total_weight)))
      (List.init floor Fun.id);
    expect "Prior source evidence rejected connected support"
      (prior.agrees ~count:(List.length connected) ~weight:(weight prior connected));
    let responses members = List.map (fun member ->
      {reply with D.responder_addr = member.T.address}) members in
    expect "connected source support became finality"
      (not (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root
        (responses connected)));
    expect "finality count floor was reduced"
      (not (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root
        (responses (members (quorum - 1)))));
    expect "full roster quorum became unreachable"
      (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root
        (responses (members quorum))))
    [59, 19, 40, 20, true; 97, 32, 65, 33, false];
  Lwt.return_unit

let historical_weights () =
  let policy = P.make ~count_epoch:20L ~stake_epoch:(Some 30L) ~round_epoch:None in
  let historical = weighted_set (List.mapi (fun index member ->
    member, Z.of_int (if index = 0 then 100 else 1)) validators.T.validators) in
  let current = weighted_set (List.mapi (fun index member ->
    member, Z.of_int (if index = 3 then 1000 else 1)) validators.T.validators) in
  let driver = make ~policy ~validator_set:current () in
  let read () = match D.epoch_root_weights ~validator_set:historical driver ~epoch_id:40L with
    | Some value -> value | None -> failwith "historical weights unavailable" in
  let before = read () in
  expect "historical root weight used current stake"
    (before.weight "octQuery0" = Some (Z.of_int 100)
      && before.epoch_id = 40L && edge before (Z.of_int 35)
      && before.required ~configured:0 = 2 && before.required ~configured:max_int = 3);
  expect "historical source threshold used current stake"
    (before.agrees ~count:2 ~weight:(Z.of_int 101));
  let live = match D.epoch_root_weights driver ~epoch_id:40L with
    | Some value -> value | None -> failwith "current weights unavailable" in
  expect "current source threshold changed"
    (live.weight "octQuery0" = Some Z.one && not (live.agrees ~count:2 ~weight:(Z.of_int 101)));
  let replies = historical.T.validators
    |> List.filteri (fun index _ -> index < 3)
    |> List.map (fun member -> {reply with D.responder_addr = member.T.address}) in
  let quorum epoch responses =
    D.epoch_root_consensus_quorum ~validator_set:historical driver ~epoch_id:epoch ~root responses in
  expect "historical quorum used current stake" (quorum 40L replies);
  expect "current quorum used historical stake"
    (not (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root replies));
  let two = List.filteri (fun index _ -> index < 2) replies in
  expect "source agreement replaced consensus quorum" (not (quorum 40L two));
  expect "duplicate signers supplied a consensus count" (not (quorum 40L (two @ two)));
  let first = List.hd replies in
  expect "Prior historical quorum required a count floor" (quorum 10L [first]);
  let wrong_root = {first with D.state_root = Some (String.make 32 '\009')} in
  expect "historical quorum counted another root"
    (not (quorum 40L (wrong_root :: List.tl replies)));
  expect "unknown signer discarded valid historical support"
    (quorum 40L ({reply with D.responder_addr = "absent"} :: replies));
  expect "invalid historical weights used current weights"
    (Option.is_none (D.epoch_root_weights ~validator_set:validators driver ~epoch_id:40L));
  expect "invalid historical set supplied consensus quorum"
    (not (D.epoch_root_consensus_quorum ~validator_set:validators driver ~epoch_id:40L ~root replies));
  driver.D.stake_vs <- validators;
  let after = read () in
  expect "historical evidence depended on current stake availability"
    (before.stamp = after.stamp && after.weight "octQuery0" = Some (Z.of_int 100)
      && after.agrees ~count:2 ~weight:(Z.of_int 101) && quorum 40L replies);
  Lwt.return_unit

let health_query_link () =
  let open Lwt.Syntax in
  let module W = Octra_node_runtime.Consensus_health_wiring in
  let policy = P.make ~count_epoch:20L ~stake_epoch:(Some 30L) ~round_epoch:None in
  let historical = weighted_set (List.mapi (fun index member ->
    member, Z.of_int (if index = 0 then 100 else 1)) validators.T.validators) in
  let current = weighted_set (List.mapi (fun index member ->
    member, Z.of_int (if index = 3 then 1000 else 1)) validators.T.validators) in
  let driver = make ~policy ~validator_set:current () in
  let unused () = failwith "health query invoked unrelated effect" in
  let deps = W.{
    root_set = (fun epoch -> if epoch = 40L then Some historical else None);
    head_time = (fun _ -> Some 0.); now = (fun () -> 1.);
    reset_ahead_streak = unused; probe_range = (fun _ -> unused ());
    env_int = (fun _ value -> value); getenv = (fun _ -> None);
    soft_catchup_max_lag = 10; quarantine_ahead_streak_threshold = 3;
    quarantine_ahead_grace_epochs = 2; quarantine_ahead_drift_tolerance = 0;
    normalize_next_epoch_for_head = (fun ~source:_ -> unused ());
    committed_head_epoch = (fun () -> 40); current_epoch = (fun () -> 41);
    catchup_next_target = (fun () -> None); attested_head = (fun _ -> false);
    clear_state_attested = unused; catchup_active = (fun () -> false);
    set_state_attested = (fun ~head:_ ~root:_ -> unused ());
    clear_quarantine = (fun _ -> unused ()); mark_quarantine = (fun _ -> unused ());
    read_local_root_raw = (fun () -> Lwt.return root);
    committed_epoch_root_raw = (fun _ -> Some root); drain_pending_finalized = unused;
    quarantine_active = (fun () -> false); quarantine_reason = (fun () -> "");
    ahead_streak = (fun () -> 0); incr_ahead_streak = unused;
    require_sync = (fun _ -> unused ());
    run_catchup_to_target = (fun _ ~target_epoch:_ ~reason:_ -> unused ());
    fork_repair = {
      committed_head_epoch = (fun () -> 40);
      rewind_allowed = (fun ~target:_ ~head:_ -> unused ());
      target_matches = (fun ~target:_ ~root:_ -> unused ());
      empty_after = (fun ~target:_ ~head:_ -> unused ());
      finality_target_ready = (fun ~target:_ ~root:_ -> unused ());
      stage_empty = (fun ~target:_ ~root:_ -> unused ());
      mark_quarantine = (fun _ -> unused ()); stop = (fun _ -> unused ());
    };
  } in
  let linked = W.driver_probe_deps deps driver in
  let weights = match linked.root_weights ~epoch_id:40L with
    | Some value -> value | None -> failwith "health historical weights unavailable" in
  expect "health link selected current weights"
    (weights.weight "octQuery0" = Some (Z.of_int 100));
  let replies = List.map (fun member -> {reply with D.responder_addr = member.T.address})
    historical.T.validators in
  let old = List.filteri (fun index _ -> index < 3) replies in
  let recent = List.filteri (fun index _ -> index > 0) replies in
  expect "health link ignored historical quorum"
    (linked.root_consensus_quorum ~epoch_id:40L ~root old);
  expect "health link accepted only current quorum"
    (not (linked.root_consensus_quorum ~epoch_id:40L ~root recent));
  expect "health link guessed missing historical weights"
    (linked.root_weights ~epoch_id:41L = None
      && not (linked.root_consensus_quorum ~epoch_id:41L ~root recent));
  let shared = query ~wait_for:D.Collection_window driver 0.8 in
  List.iter (add driver) recent;
  let pending = linked.query_epoch_root ~epoch_id:40L ~timeout_seconds:0.6 in
  Lwt.finalize (fun () ->
    let* () = Lwt_unix.sleep 0.03 in
    expect "health query reused old responses" (Lwt.is_sleeping pending);
    let early = List.filteri (fun index _ -> index > 0) recent in
    List.iter (add driver) early;
    let* () = Lwt_unix.sleep 0.25 in
    expect "health query closed before collection window" (Lwt.is_sleeping pending);
    let later = List.hd replies in
    add driver later;
    let* found = pending in
    expect "health query lost fresh responses"
      (List.sort compare found = List.sort compare (later :: early));
    Lwt.return_unit)
    (fun () -> Lwt.cancel shared; Lwt.cancel pending; Lwt.return_unit)

let historical_identity () =
  let policy = P.make ~count_epoch:20L ~stake_epoch:(Some 30L) ~round_epoch:None in
  let historical = weighted_set (List.mapi (fun index member ->
    member, Z.of_int (if index = 0 then 100 else 1)) validators.T.validators) in
  let driver = make ~policy ~validator_set:historical () in
  let read () = match D.epoch_root_weights ~validator_set:historical driver ~epoch_id:40L with
    | Some value -> value | None -> failwith "historical weights unavailable" in
  let before = read () in
  let current = weighted_set (List.map (fun member -> member, Z.one) validators.T.validators) in
  E.replace_validator_set driver.D.engine current;
  let reweighted = read () in
  expect "root stamp ignored authentication set weights" (before.stamp <> reweighted.stamp);
  expect "authentication weights replaced historical weights"
    (reweighted.weight "octQuery0" = Some (Z.of_int 100));
  let changed = weighted_set (List.map (fun (member, weight) ->
    let member = if member.T.address = "octQuery0" then
      {member with T.pubkey = String.make 32 '\009'} else member in
    member, weight) (T.weights_of_set current)) in
  E.replace_validator_set driver.D.engine changed;
  let after = read () in
  expect "root stamp ignored authentication key changes" (after.stamp <> reweighted.stamp);
  expect "historical evidence counted a changed identity" (after.weight "octQuery0" = None);
  expect "historical evidence lost unchanged identity" (after.weight "octQuery1" = Some Z.one);
  expect "captured authentication set changed" (before.weight "octQuery0" = Some (Z.of_int 100));
  let replies = List.map (fun member ->
    {reply with D.responder_addr = member.T.address}) historical.T.validators in
  expect "historical quorum counted a changed identity"
    (not (D.epoch_root_consensus_quorum ~validator_set:historical driver ~epoch_id:40L ~root replies));
  expect "default quorum counted a changed identity"
    (not (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root replies));
  let reduced = weighted_set (List.filter (fun (member, _) -> member.T.address <> "octQuery3")
    (T.weights_of_set changed)) in
  E.replace_validator_set driver.D.engine reduced;
  expect "historical evidence counted a removed identity" ((read ()).weight "octQuery3" = None);
  E.replace_validator_set driver.D.engine historical;
  expect "restored authentication set changed stamp" ((read ()).stamp = before.stamp);
  Lwt.return_unit

let reader_capacity () =
  List.iter (fun separate ->
    let state, leases = List.fold_left (fun (state, leases) index ->
      let epoch = if separate then Int64.of_int index else 40L in
      match Q.join ~fresh:(index mod 2 = 0) ~epoch state with
      | Ok (state, lease) -> state, lease :: leases
      | Error reason -> failwith reason) (Q.empty, []) (List.init 64 Fun.id) in
    expect "reader capacity admitted another epoch"
      (Result.is_error (Q.join ~epoch:100L state));
    expect "reader capacity admitted another fresh reader"
      (Result.is_error (Q.join ~fresh:true ~epoch:40L state));
    let released = Q.leave (List.hd leases) state in
    let released = Q.leave (List.hd leases) released in
    let state, lease = match Q.join ~fresh:true ~epoch:100L released with
      | Ok value -> value | Error reason -> failwith reason in
    expect "repeat release increased reader capacity"
      (Result.is_error (Q.join ~epoch:100L state));
    let state = List.fold_left (fun state lease -> Q.leave lease state)
      state (lease :: leases) in
    expect "reader capacity retained released epochs"
      (List.for_all (fun epoch -> not (Q.listening ~epoch state))
        (100L :: 40L :: List.init 64 Int64.of_int))) [false; true];
  Lwt.return_unit

let reply_capacity () =
  let join fresh state = match Q.join ~fresh ~epoch:40L state with
    | Ok value -> value | Error reason -> failwith reason in
  let state, shared = join false Q.empty in
  let state, first = join true state in
  let state = List.fold_left (fun state reply ->
    Q.add ~epoch:40L ~same:Int.equal reply state) state (List.init 300 Fun.id) in
  expect "reply capacity was exceeded"
    (List.length (Q.read shared state) = 256 && List.length (Q.read first state) = 256);
  let state, second = join true state in
  let state = Q.add ~epoch:40L ~same:Int.equal 0 state in
  let state = Q.add ~epoch:40L ~same:Int.equal 0 state in
  expect "full shared collection suppressed fresh replies" (Q.read second state = [0]);
  let state = List.fold_left (fun state lease -> Q.leave lease state)
    state [shared; first; second] in
  expect "reply capacity retained released epoch" (not (Q.listening ~epoch:40L state));
  Lwt.return_unit

let capacity_growth () =
  let join ?capacity fresh state = match Q.join ?capacity ~fresh ~epoch:40L state with
    | Ok value -> value | Error reason -> failwith reason in
  expect "negative reply capacity was accepted"
    (Result.is_error (Q.join ~capacity:(-1) ~epoch:40L Q.empty));
  let state, shared = join false Q.empty in
  let state, larger = join ~capacity:385 true state in
  let state, smaller = join ~capacity:1 true state in
  let state = Q.leave larger state in
  let state = List.fold_left (fun state reply ->
    Q.add ~epoch:40L ~same:Int.equal reply state) state (List.init 400 Fun.id) in
  expect "slot did not retain its largest requested capacity"
    (List.length (Q.read shared state) = 385 && List.length (Q.read smaller state) = 385);
  expect "released capacity reader retained replies" (Q.read larger state = []);
  let state = Q.leave shared state |> Q.leave smaller in
  expect "capacity growth retained released epoch" (not (Q.listening ~epoch:40L state));
  let state, lease = join false state in
  let state = List.fold_left (fun state reply ->
    Q.add ~epoch:40L ~same:Int.equal reply state) state (List.init 300 Fun.id) in
  expect "reopened slot retained prior capacity" (List.length (Q.read lease state) = 256);
  let state = Q.leave lease state in
  expect "reopened capacity slot retained reader" (not (Q.listening ~epoch:40L state));
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

let with_root_query action =
  let open Lwt.Syntax in
  let module H = Octra_consensus.C_hash in
  let module Codec = Octra_consensus.C_codec in
  let secret = String.make 32 '\027' in
  let public_key = match Mirage_crypto_ec.Ed25519.priv_of_octets secret with
    | Error _ -> failwith "invalid root query test key"
    | Ok key -> Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let validator_set = T.make_validator_set
    [T.{address = reply.responder_addr; pubkey = public_key}] in
  let driver = make ~validator_set () in
  let state, lease = match Q.join ~fresh:true ~epoch:40L driver.D.epoch_root_queries with
    | Ok value -> value | Error reason -> failwith reason in
  driver.D.epoch_root_queries <- state;
  let receive ~direct (record : D.epoch_root_response_record) =
    let bytes = H.epoch_root_response_sign_bytes ~chain_id ~epoch_id:40L
      ~state_root:record.state_root ~responder_addr:record.responder_addr
      ~responder_head_epoch:record.responder_head_epoch in
    let response = Codec.{chain_id; epoch_id = 40L; state_root = record.state_root;
      responder_addr = record.responder_addr; responder_head_epoch = record.responder_head_epoch;
      signature = H.sign_ed25519 ~priv_raw:secret ~msg:bytes} in
    let frame = Octra_net.P2p_frame.{msg_type = msg_epoch_root_response;
      payload = Codec.encode_epoch_root_response response} in
    let peer_id = Octra_net.P2p_handshake.node_id_of_pubkey
      (if direct then public_key else pubkey) in
    let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let conn = Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Validator fd
      ~peer_id ~addr:"root-query-test" ~direction:Octra_net.P2p_conn.Inbound in
    Lwt.finalize
      (fun () -> D.on_p2p_message driver conn frame)
      (fun () -> Octra_net.P2p_conn.close conn) in
  Lwt.finalize
    (fun () -> action driver lease receive)
    (fun () ->
      driver.D.epoch_root_queries <- Q.leave lease driver.D.epoch_root_queries;
      Lwt.return_unit)

let source_mismatch () = with_root_query (fun driver lease receive ->
  let open Lwt.Syntax in
  let* () = receive ~direct:false reply in
  expect "relayed root reply entered query state"
    (Q.read lease driver.D.epoch_root_queries = []);
  expect "relayed root reply changed peer state" (D.peer_state_snapshot driver = []);
  let* () = receive ~direct:true reply in
  expect "direct signed root reply was rejected"
    (Q.read lease driver.D.epoch_root_queries = [reply]);
  expect "direct signed root reply did not update peer state"
    (List.length (D.peer_state_snapshot driver) = 1);
  Lwt.return_unit)

let root_ingestion () =
  Lwt_list.iter_s (fun (record, accepted) -> with_root_query (fun driver lease receive ->
    let open Lwt.Syntax in
    let peer_values () = D.peer_state_snapshot driver
      |> List.map (fun (peer : D.peer_state_record) ->
        peer.responder_addr, peer.head_epoch, peer.checked_epoch,
        peer.state_root, peer.last_seen, peer.source) in
    let* () = receive ~direct:true record in
    let records = Q.read lease driver.D.epoch_root_queries in
    let peers = peer_values () in
    expect "signed root height entered query incorrectly"
      (records = if accepted then [record] else []);
    expect "signed root height changed peer state incorrectly"
      (List.length peers = if accepted then 1 else 0);
    let* () = receive ~direct:true {reply with responder_head_epoch = 38L} in
    expect "contradictory signed root changed query state"
      (Q.read lease driver.D.epoch_root_queries = records);
    expect "contradictory signed root changed peer state" (peer_values () = peers);
    Lwt.return_unit))
    [{reply with responder_head_epoch = 39L}, false;
     {reply with responder_head_epoch = 40L}, true;
     reply, true;
     {reply with responder_head_epoch = 39L; state_root = None}, true]

let root_height_rules () =
  let driver = make () in
  let replies = validators.T.validators |> List.filteri (fun index _ -> index < 3)
    |> List.map (fun member ->
      {reply with D.responder_addr = member.T.address; responder_head_epoch = 40L}) in
  let check records quorum source =
    expect "root height supplied consensus quorum"
      (D.epoch_root_consensus_quorum driver ~epoch_id:40L ~root records = quorum);
    expect "root height supplied consensus wait"
      (D.epoch_root_wait_reached driver ~epoch_id:40L ~wait_for:D.Consensus_quorum records = quorum);
    expect "root height supplied source agreement"
      (D.epoch_root_wait_reached driver ~epoch_id:40L ~wait_for:D.Source_agreement records = source) in
  check replies true true;
  check (replies @ replies) true true;
  check (List.map (fun record -> {record with D.responder_head_epoch = 39L}) replies) false false;
  let first = List.hd replies in
  check [first; first; first] false false;
  let two = List.filteri (fun index _ -> index < 2) replies in
  check two false true;
  List.iter (fun conflict ->
    List.iter (fun records -> check records false true) [conflict :: replies; replies @ [conflict]];
    List.iter (fun records -> check records false false) [conflict :: two; two @ [conflict]])
    [{first with responder_head_epoch = 39L};
     {first with state_root = Some (String.make 32 '\009')};
     {first with state_root = None}];
  let low = {first with responder_head_epoch = 39L; state_root = None} in
  expect "valid empty low head was discarded" (D.epoch_root_records ~epoch_id:40L [low] = [low]);
  Lwt.return_unit

let root_query_height () =
  Lwt_list.iter_s (fun head ->
    let open Lwt.Syntax in
    let driver = make () in
    let state, lease = match Q.join ~epoch:40L driver.D.epoch_root_queries with
      | Ok value -> value | Error reason -> failwith reason in
    driver.D.epoch_root_queries <- state;
    let replies = validators.T.validators |> List.filteri (fun index _ -> index < 3)
      |> List.map (fun member ->
        {reply with D.responder_addr = member.T.address; responder_head_epoch = head}) in
    List.iter (add driver) replies;
    let source = query ~wait_for:D.Source_agreement driver 10. in
    let quorum = query ~wait_for:D.Consensus_quorum driver 10. in
    let* () = Lwt.finalize (fun () ->
      if head < 40L then begin
        expect "invalid root height completed source query" (Lwt.is_sleeping source);
        expect "invalid root height completed quorum query" (Lwt.is_sleeping quorum);
        let* () = cancelled source in
        cancelled quorum
      end else
        let* left, right = Lwt.both source quorum in
        expect "valid root height query lost replies"
          (left = List.rev replies && right = List.rev replies);
        Lwt.return_unit)
      (fun () ->
        List.iter Lwt.cancel [source; quorum];
        driver.D.epoch_root_queries <- Q.leave lease driver.D.epoch_root_queries;
        Lwt.return_unit) in
    expect "root height queries retained reader" (not (listening driver 40L));
    Lwt.return_unit) [39L; 40L; 41L]

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

let send_collection () = with_queue (fun driver conn _ ->
  let open Lwt.Syntax in
  let pending = query ~wait_for:D.Collection_window driver 0.05 in
  Lwt.finalize (fun () ->
    let* () = Lwt_unix.sleep 0.1 in
    let* _ = Lwt_mvar.take conn.write_queue in
    let* () = Lwt.pause () in
    let* () = Lwt.pause () in
    expect "default collection window moved before send" (Lwt.is_sleeping pending);
    let+ replies = pending in
    expect "empty collection produced replies" (replies = []))
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

let cancel_fresh_send () = with_queue (fun driver conn _ ->
  let open Lwt.Syntax in
  let first = query ~fresh:true driver 10. in
  let second = query ~wait_for:D.Collection_window driver 0.1 in
  Lwt.finalize
    (fun () ->
      let* () = cancelled first in
      expect "fresh send cancellation removed shared reader" (listening driver 40L);
      let* _ = Lwt_mvar.take conn.write_queue in
      add driver reply;
      let* result = second in
      expect "fresh send cancellation erased shared replies" (result = [reply]);
      expect "fresh send cancellation retained slot" (not (listening driver 40L));
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

let target_keys = List.init 7 (fun index ->
  let secret = String.make 32 (Char.chr (index + 41)) in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets secret |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  T.{address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey); pubkey}, secret)

let target_set = weighted_set (List.map2 (fun (member, _) weight -> member, Z.of_int weight)
  target_keys [3; 2; 1; 1; 1; 1; 1])

let target_final ?(epoch = 40L) time =
  let module H = Octra_consensus.C_hash in
  let header = T.{proto_version = proto_version_current; chain_id; epoch_id = epoch;
    prev_state_root = String.make 32 '\003'; tx_list_hash = E.tx_list_hash_for_header [];
    receipt_root = H.receipt_root []; proposed_state_root = root;
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = (fst (List.hd target_keys)).address; txid_hi = 0L; ts = time} in
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun ((member : T.validator_info), secret) ->
    let vote = T.{chain_id; epoch_id = epoch; round = 0; vote_type = Precommit;
      proposal_id; validator = member.address; signature = ""} in
    {vote with signature = H.sign_ed25519 ~priv_raw:secret ~msg:(H.vote_sign_bytes vote)}) target_keys in
  T.{chain_id; epoch_id = epoch; commit_round = 0; header; proposal_id;
    precommits; parent_commit = None}

let with_target ?(history = 0) action =
  let module Store = Octra_core.Store_chaindata in
  let module Head = Octra_core.Head_manifest in
  let module Log = Octra_core.Epochlog in
  let module J = Octra_node_runtime.Consensus_finality_journal in
  let directory = Test_workspace.unique_dir "sync-target" in
  let chaindata = Store.open_chaindata (Filename.concat directory "chain") in
  let epoch = 40 + history in
  let padding = if history = 0 then "" else String.make 32768 'a' in
  for id = 0 to history - 1 do
    Log.append chaindata.epochlog {Log.empty_epoch_header with id; parent_commit = padding}
  done;
  let time = Unix.gettimeofday () -. 2. in
  let final = target_final ~epoch:(Int64.of_int epoch) time in
  let head = Head.{schema_version; generation = epoch; epoch_id = epoch;
    state_root = Octra_node_runtime.Text.raw_to_hex root; ledger_state_root = None;
    irmin_commit = None; txid_hi = 0L; txlog_seg = None; txlog_off = None;
    epochlog_off = None; commit_id = "sync-target"; ts = time;
    quorum_cert_hash = None; epoch_index_hash = None; epoch_index_root = None} in
  Head.atomic_write directory head;
  Store.begin_batch chaindata;
  Store.set_epoch chaindata Log.{empty_epoch_header with id = epoch;
    state_root = head.state_root; finalized_at = time};
  Store.commit_batch chaindata;
  Store.fsync chaindata;
  let head = {head with epochlog_off = Some (Log.current_offset chaindata.epochlog)} in
  Head.atomic_write directory head;
  (match J.seed ~chain_id ~validator_set:target_set ~finalize:final directory with
   | Ok _ -> () | Error _ -> failwith "target journal seed failed");
  let policy = P.make ~count_epoch:0L ~stake_epoch:(Some 1L) ~round_epoch:(Some 1L) in
  let driver = make ~head:(fun () -> Int64.of_int epoch) ~policy ~validator_set:target_set () in
  driver.engine.state <- {driver.engine.state with height = Int64.of_int (epoch + 1)};
  driver.D.running <- true;
  let targets = ref [] in
  let open_target ?(selected = ref (Some driver)) () =
    let target = Octra_node_runtime.Status_read_rpc.create_sync_target ~chain_id
      ~data_dir:directory ~chaindata ~driver_ref:selected in
    targets := target :: !targets;
    target in
  Lwt.finalize (fun () -> action directory chaindata head driver open_target)
    (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt_list.iter_s (fun target ->
        let+ stopped = Octra_node_runtime.Status_read_rpc.close_sync_target target in
        expect "target shutdown did not finish" stopped) !targets in
      driver.running <- false; Store.close chaindata;
      Test_workspace.remove directory; Lwt.return_unit)

let send_target driver (index, head, forged) =
  let module H = Octra_consensus.C_hash in
  let module Codec = Octra_consensus.C_codec in
  let open Lwt.Syntax in
  let member, secret = List.nth target_keys index in
  let epoch_id = driver.D.config.local_head_epoch () in
  let bytes = H.epoch_root_response_sign_bytes ~chain_id ~epoch_id
    ~state_root:(Some root) ~responder_addr:member.T.address ~responder_head_epoch:head in
  let response = Codec.{chain_id; epoch_id; state_root = Some root;
    responder_addr = member.address; responder_head_epoch = head;
    signature = if forged then String.make 64 '\000'
      else H.sign_ed25519 ~priv_raw:secret ~msg:bytes} in
  let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey member.pubkey)
    ~addr:"target-test" ~direction:Octra_net.P2p_conn.Inbound in
  Lwt.finalize (fun () -> D.on_p2p_message driver conn
    Octra_net.P2p_frame.{msg_type = msg_epoch_root_response;
      payload = Codec.encode_epoch_root_response response})
    (fun () -> Octra_net.P2p_conn.close conn)

let await_target driver =
  let rec wait left =
    if listening driver (driver.D.config.local_head_epoch ()) then Lwt.return_unit
    else if left = 0 then Lwt.fail_with "target query did not open"
    else let open Lwt.Syntax in let* () = Lwt_unix.sleep 0.005 in wait (left - 1) in
  wait 200

let target_json = function
  | Ok json -> json
  | Error _ -> failwith "target RPC returned an error"

let target_value name result = Yojson.Safe.Util.member name (target_json result)

let target_route () =
  let routes = Octra_node_runtime.Status_read_rpc.dispatch {status_read = Fun.id} in
  expect "sync target is public" (not (List.mem_assoc "octra_syncTarget" routes));
  Lwt.return_unit

let target_replies () =
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun (name, replies, target) -> with_target (fun _ _ _ driver open_target ->
    let pending = Octra_node_runtime.Status_read_rpc.sync_target (open_target ()) in
    let* () = await_target driver in
    let* () = Lwt_list.iter_s (send_target driver) replies in
    let* result = pending in
    expect (name ^ " target differs") (target_value "targetEpoch" result = target);
    expect (name ^ " status differs") (target_value "status" result
      = `String (if target = `Null then "no_agreement" else "agreed"));
    expect "target lost local head" (target_value "localEpoch" result = `String "40");
    expect "target lost local root" (target_value "headRoot" result
      = `String (Octra_node_runtime.Text.raw_to_hex root));
    expect "target retained its query" (not (listening driver 40L));
    Lwt.return_unit)) [
      "agreed", [0, 42L, false; 1, 42L, false; 2, 42L, false], `String "42";
      "low_weight", [2, 42L, false], `Null;
      "forged", [0, 42L, true; 1, 42L, false; 2, 42L, false], `Null;
      "ceiling", [0, 999999L, false; 1, 999999L, false; 2, 999999L, false], `Null;
      "duplicate", [0, 42L, false; 0, 42L, false; 0, 42L, false], `Null;
    ]

let target_changes () =
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun change -> with_target (fun directory store head driver open_target ->
    let selected = ref (Some driver) in
    let pending = Octra_node_runtime.Status_read_rpc.sync_target (open_target ~selected ()) in
    let* () = await_target driver in
    let* () = Lwt_list.iter_s (send_target driver) [0, 42L, false; 1, 42L, false; 2, 42L, false] in
    (match change with
     | "head" -> Octra_core.Head_manifest.atomic_write directory {head with generation = 41}
     | "generation" -> driver.engine.generation <- driver.engine.generation + 1
     | "height" -> driver.engine.state <- {driver.engine.state with height = 42L}
     | "driver" -> selected := None
     | "set" -> driver.engine.vs <- weighted_set
         (List.map (fun (member, weight) -> member, Z.succ weight) (T.weights_of_set target_set))
     | "index" ->
       let module Index = Octra_core.Chaindata_index in
       let module Epoch = Octra_core.Epochlog in
       let header = Epoch.get store.epochlog head.epoch_id |> Option.get in
       Index.begin_write store.index;
       Index.buffer_epoch store.index head.epoch_id
         (Epoch.epoch_to_json {header with finalized_at = header.finalized_at +. 1.});
       Index.commit_write store.index
     | "journal" ->
       let module Store = Octra_core.Store_chaindata in
       let module Epoch = Octra_core.Epochlog in
       let header = Epoch.get store.epochlog head.epoch_id |> Option.get in
       Store.begin_batch store;
       Store.set_epoch store {header with finalized_at = header.finalized_at +. 1.};
       Store.commit_batch store;
       Store.fsync store
     | _ -> failwith "unknown target mutation");
    let* result = pending in
    expect (change ^ " change was ignored") (target_value "status" result = `String "changed"
      && target_value "targetEpoch" result = `Null);
    Lwt.return_unit)) ["head"; "generation"; "height"; "driver"; "set"; "index"; "journal"]

let target_fresh () = with_target (fun _ _ _ driver open_target ->
  let open Lwt.Syntax in
  let queries, lease = Q.join ~epoch:40L driver.D.epoch_root_queries |> Result.get_ok in
  driver.epoch_root_queries <- queries;
  let* () = Lwt_list.iter_s (send_target driver) [0, 42L, false; 1, 42L, false; 2, 42L, false] in
  let* result = Octra_node_runtime.Status_read_rpc.sync_target (open_target ()) in
  driver.epoch_root_queries <- Q.leave lease driver.epoch_root_queries;
  expect "target reused old replies" (target_value "targetEpoch" result = `Null);
  Lwt.return_unit)

let target_offline () = with_target (fun _ _ _ driver open_target ->
  let open Lwt.Syntax in
  driver.D.running <- false;
  let* result = Octra_node_runtime.Status_read_rpc.sync_target (open_target ()) in
  expect "offline target was trusted" (target_value "status" result = `String "unavailable"
    && target_value "targetEpoch" result = `Null);
  expect "offline target opened query" (not (listening driver 40L));
  Lwt.return_unit)

let target_timeout () = with_target (fun _ _ _ driver open_target ->
  let open Lwt.Syntax in
  let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:"blocked-target" ~addr:"target-test" ~direction:Octra_net.P2p_conn.Inbound in
  let frame = Octra_net.P2p_frame.{msg_type = msg_query_epoch_root; payload = ""} in
  let* () = Lwt_mvar.put conn.write_queue frame in
  Hashtbl.add driver.D.swarm.peers conn.peer_id conn;
  Lwt.finalize (fun () ->
    let started = Unix.gettimeofday () in
    let* result = Octra_node_runtime.Status_read_rpc.sync_target (open_target ()) in
    expect "target exceeded client deadline" (Unix.gettimeofday () -. started < 3.0);
    expect "blocked target was trusted" (target_value "status" result = `String "no_agreement"
      && target_value "targetEpoch" result = `Null);
    expect "blocked target retained query" (not (listening driver 40L));
    Lwt.return_unit)
    (fun () -> Hashtbl.clear driver.swarm.peers; Octra_net.P2p_conn.close conn))

let target_records () =
  let module Store = Octra_core.Store_chaindata in
  let module Head = Octra_core.Head_manifest in
  let module Epoch = Octra_core.Epochlog in
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun change -> with_target (fun directory store head driver open_target ->
    let offset = Hashtbl.find store.Store.epochlog.offsets head.Head.epoch_id in
    let header = Epoch.get store.epochlog head.epoch_id |> Option.get in
    (match change with
     | "missing" -> Hashtbl.remove store.epochlog.offsets head.epoch_id
     | "offset" -> Hashtbl.replace store.epochlog.offsets head.epoch_id (offset + 1)
     | "prefix" -> Hashtbl.replace store.epochlog.offsets head.epoch_id 0
     | "extent" -> Head.atomic_write directory {head with epochlog_off = Some (offset + 4)}
     | "index" ->
       Octra_core.Chaindata_index.begin_write store.index;
       Octra_core.Chaindata_index.buffer_epoch store.index head.epoch_id
         (Epoch.epoch_to_json {header with finalized_at = header.finalized_at +. 1.});
       Octra_core.Chaindata_index.commit_write store.index
     | "checksum" | "length" | "short" | "identity" ->
       let path = Filename.concat store.epochlog.dir "epochs.dat" in
       let fd = Unix.openfile path [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
       Fun.protect ~finally:(fun () -> Unix.close fd) (fun () ->
         if change = "short" then Unix.ftruncate fd (offset + 5)
         else if change = "identity" then begin
           let payload = Epoch.epoch_to_json {header with id = head.epoch_id + 1} in
           let bytes = Bytes.create (String.length payload + 8) in
           Epoch.write_u32_le bytes 0 (String.length payload + 4);
           Bytes.blit_string payload 0 bytes 4 (String.length payload);
           Bytes.blit_string (Epoch.checksum payload) 0 bytes (String.length payload + 4) 4;
           ignore (Unix.lseek fd offset Unix.SEEK_SET);
           expect "record edit was short" (Unix.write fd bytes 0 (Bytes.length bytes) = Bytes.length bytes)
         end else begin
           let at = if change = "length" then offset
             else Option.get (Epoch.offset_after store.epochlog head.epoch_id) - 4 in
           ignore (Unix.lseek fd at Unix.SEEK_SET);
           expect "record edit was short" (Unix.write fd (Bytes.make 4 '\255') 0 4 = 4)
         end)
     | _ -> failwith "unknown record edit");
    let* value = Octra_node_runtime.Status_read_rpc.sync_head_read ~chain_id ~data_dir:directory
      ~chaindata:store ~epoch_id:(Int64.of_int head.epoch_id) () in
    let reason = match value with Error reason -> reason | Ok _ -> "accepted" in
    let expected = match change with
      | "missing" -> "sync head location is unavailable"
      | "prefix" -> "epoch location is invalid"
      | "extent" -> "sync head journal offset differs"
      | "index" -> "sync head index differs from journal"
      | _ -> "epoch record is invalid" in
    expect (change ^ " refusal cause differs") (reason = expected);
    let pending = Octra_node_runtime.Status_read_rpc.sync_target (open_target ()) in
    let opened = ref false in
    let rec observe () =
      if listening driver 40L then opened := true;
      if Lwt.is_sleeping pending then
        let* () = Lwt_unix.sleep 0.001 in observe () else Lwt.return_unit in
    let* result, () = Lwt.both pending (observe ()) in
    expect (change ^ " record was trusted") (target_value "status" result = `String "unavailable"
      && target_value "targetEpoch" result = `Null);
    expect "invalid record opened peer query" (not !opened && not (listening driver 40L));
    Lwt.return_unit)) ["missing"; "offset"; "prefix"; "extent"; "index";
      "checksum"; "length"; "short"; "identity"]

let target_floor () = with_target (fun directory store head _ _ ->
  let module Index = Octra_core.Chaindata_index in
  let module Read = Octra_node_runtime.Status_read_rpc in
  let open Lwt.Syntax in
  Index.begin_write store.Octra_core.Store_chaindata.index;
  Index.buffer_meta store.index "history_floor" (String.make (Octra_core.Epochlog.max_record_len + 1) 'x');
  Index.commit_write store.index;
  let* value = Read.sync_head_read ~chain_id ~data_dir:directory ~chaindata:store
    ~epoch_id:(Int64.of_int head.Octra_core.Head_manifest.epoch_id) () in
  expect "unused floor rejected valid certificate" (Result.is_ok value);
  Lwt.return_unit)

let target_history () = with_target ~history:100000 (fun _ store head driver open_target ->
  let open Lwt.Syntax in
  let bytes = (Unix.fstat store.Octra_core.Store_chaindata.epochlog.fd).Unix.st_size in
  expect "history is smaller than live journal" (bytes > 3_206_236_757);
  let started = Octra_node_runtime.Sync_probe.monotonic () in
  let pending = Octra_node_runtime.Status_read_rpc.sync_target (open_target ()) in
  let target = Int64.of_int (head.Octra_core.Head_manifest.epoch_id + 2) in
  let rec answer () =
    if listening driver (Int64.of_int head.epoch_id) then
      Lwt_list.iter_s (send_target driver) [0, target, false; 1, target, false; 2, target, false]
    else if not (Lwt.is_sleeping pending) then Lwt.return_unit
    else let* () = Lwt_unix.sleep 0.005 in answer () in
  let* (), result = Lwt.both (answer ()) pending in
  let elapsed = Octra_node_runtime.Sync_probe.monotonic () -. started in
  let status = match target_value "status" result with `String value -> value | _ -> "invalid" in
  Printf.printf "event = sync_history bytes = %d seconds = %.6f status = %s\n%!" bytes elapsed status;
  expect "large history target unavailable" (target_value "status" result = `String "agreed"
    && target_value "targetEpoch" result = `String (Int64.to_string target));
  expect "large history exceeded deadline" (elapsed < 2.5);
  Lwt.return_unit)

let probe_coalesce () =
  let module S = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let now = ref 0. and key = ref 1 and calls = ref 0 in
  let gate, release = Lwt.wait () in
  let probe = S.create ~clock:(fun () -> !now) ~valid:(fun value -> value = !key)
    (fun ~background:_ ~deadline:_ -> incr calls; let+ () = gate in !key) in
  let jobs = List.init 100 (fun _ -> S.query probe) in
  let* () = Lwt.pause () in
  expect "probe did not coalesce" (!calls = 1);
  expect "probe waiters exceeded limit"
    (List.length (List.filter (fun job -> Lwt.state job = Lwt.Return None) jobs) = 84);
  Lwt.cancel (List.hd jobs);
  let extra = S.query probe in
  expect "client cancellation started another probe" (!calls = 1);
  Lwt.wakeup_later release ();
  let* values = Lwt_list.map_s (fun job -> Lwt.catch (fun () -> job)
    (function Lwt.Canceled -> Lwt.return_none | error -> Lwt.fail error)) (extra :: jobs) in
  expect "probe lost shared readers" (List.length (List.filter ((=) (Some 1)) values) = 16);
  let* value = S.query probe in
  expect "probe did not cache" (value = Some 1 && !calls = 1);
  key := 2;
  let* value = S.query probe in
  expect "key change bypassed cooldown" (value = None && !calls = 1);
  now := 3.;
  let* value = S.query probe in
  expect "probe did not refresh changed key" (value = Some 2 && !calls = 2);
  now := 6.1;
  let* value = S.query probe in
  expect "probe cache did not expire" (value = Some 2 && !calls = 3);
  let+ stopped = S.close probe in expect "probe did not close" stopped

let probe_delta () =
  let module S = Octra_node_runtime.Sync_probe in
  let state, effects = S.delta S.empty (S.Ask (1, 0., false)) in
  let token = match effects with [S.Run token] -> token | _ -> failwith "probe did not start" in
  let state = List.fold_left (fun state id ->
    let state, effects = S.delta state (S.Ask (id, 0., false)) in
    expect "second probe started" (not (List.exists (function S.Run _ -> true | _ -> false) effects));
    state) state (List.init 99 (fun index -> index + 2)) in
  expect "typed probe exceeded waiter limit" (List.length state.readers = 16);
  let expired, effects = S.delta state (S.Tick 2.5) in
  expect "expiry lost active ownership" (expired.active <> None);
  expect "expiry did not interrupt" (List.mem (S.Interrupt token) effects);
  let blocked, effects = S.delta expired (S.Ask (101, 4., false)) in
  expect "expired worker was replaced" (effects = [S.Reply (101, None)]);
  let wrong = {token with S.generation = token.generation + 1} in
  let unchanged, _ = S.delta blocked (S.Completed (wrong, 4., true, Some 1)) in
  expect "another generation completed work" (unchanged = blocked);
  let stopped, _ = S.delta blocked S.Stop in
  let stopped, _ = S.delta stopped (S.Completed (token, 4., true, Some 1)) in
  expect "closed completion revived cache" (stopped.phase = S.Closed && stopped.cached = None);
  let state, effects = S.delta S.empty (S.Refresh 0.) in
  let token = match effects with [S.Run token] -> token | _ -> failwith "refresh did not start" in
  let state, _ = S.delta state (S.Ask (2, 0., false)) in
  let state, effects = S.delta state (S.Cancel 2) in
  expect "client cancelled background owner" (effects = []);
  let state, effects = S.delta state (S.Tick 2.5) in
  expect "client deadline interrupted background owner" (effects = []);
  let state, _ = S.delta state (S.Completed (token, 5., true, Some 8)) in
  expect "background completion renewed sample age" (state.sample = Some (0., 8));
  let state, effects = S.delta S.empty (S.Refresh 10.) in
  let token = match effects with [S.Run token] -> token | _ -> failwith "refresh did not start" in
  let state, effects = S.delta state (S.Tick 20.) in
  expect "background budget was unlimited" (effects = [S.Interrupt token]);
  let state, _ = S.delta state (S.Completed (token, 20.1, true, Some 8)) in
  expect "expired work published a sample" (state.sample = None);
  let state, effects = S.delta S.empty (S.Refresh 0.) in
  let token = match effects with [S.Run token] -> token | _ -> failwith "refresh did not start" in
  let state, _ = S.delta state (S.Ask (2, 0., false)) in
  let state, _ = S.delta state (S.Ask (3, 1., false)) in
  let state, effects = S.delta state (S.Completed (token, 3., true, Some 8)) in
  expect "late completion answered an expired reader"
    (List.mem (S.Reply (2, None)) effects && List.mem (S.Reply (3, Some 8)) effects);
  expect "reader expiry discarded background sample" (state.sample = Some (0., 8));
  Lwt.return_unit

let probe_child () =
  let module S = Octra_node_runtime.Sync_probe in
  let module F = Octra_core.Proof_frame in
  let open Lwt.Syntax in
  let prior = F.children (Unix.getpid ()) in
  let work = S.read_child ~mode:"--sync-test" ["sleep"] in
  let rec started left =
    if F.children (Unix.getpid ()) <> prior then Lwt.return_unit
    else if left = 0 then Lwt.fail_with "sync read child did not start"
    else let* () = Lwt_unix.sleep 0.005 in started (left - 1) in
  let* () = started 200 in
  let start = Unix.gettimeofday () in
  Lwt.cancel work;
  let* stopped = Lwt.catch (fun () -> let+ _ = work in false)
    (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error) in
  expect "sync child cancellation exceeded deadline"
    (stopped && Unix.gettimeofday () -. start < 1.);
  expect "sync child survived cancellation" (F.children (Unix.getpid ()) = prior);
  let+ value = S.read_child ~mode:"--sync-test" ["ready"] in
  expect "reaped child retained read slot" (value = "ready")

let probe_process () =
  let module S = Octra_node_runtime.Sync_probe in
  let module F = Octra_core.Proof_frame in
  let open Lwt.Syntax in
  let prior = F.children (Unix.getpid ()) in
  let* () = Lwt_list.iter_s (fun (mode, args) ->
    let* refused = Lwt.catch (fun () -> let+ _ = S.read_child ~mode args in false)
      (fun _ -> Lwt.return_true) in
    expect "invalid child response was accepted" refused;
    expect "failed child was not reaped" (F.children (Unix.getpid ()) = prior);
    Lwt.return_unit)
    ["--sync-test", ["fail"]; "--sync-test", ["size"];
      "--sync-head", []; "--sync-head", ["chain"; "."; "."; "bad"; "0"]] in
  let heap = Bytes.make (64 * 1024 * 1024) 'h' in
  let words = (Gc.quick_stat ()).Gc.heap_words in
  let* raw = S.read_child ~mode:"--sync-test" ["heap"] in
  expect "reader inherited parent heap" (int_of_string raw < words / 2);
  expect "parent heap was discarded" (Bytes.get (Sys.opaque_identity heap) 0 = 'h');
  let files = List.init 32 (fun _ -> Unix.openfile "/dev/null" [Unix.O_RDONLY] 0) in
  let* () = Lwt.finalize (fun () ->
    let+ fds = S.read_child ~mode:"--sync-test" ["fds"] in
    expect "reader inherited descriptors" (fds = "3"))
    (fun () -> List.iter Unix.close files; Lwt.return_unit) in
  let+ value = S.read_child ~mode:"--sync-test" ["ready"] in
  expect "failed reader retained process slot" (value = "ready")

let probe_snapshot () =
  let module S = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let* () = Lwt_list.iter_s (fun delay ->
    let calls = ref 0 in
    let probe = S.create ~valid:(fun _ -> true) (fun ~background ~deadline ->
      expect "refresh lost background mode" background;
      expect "background budget was not passed" (deadline -. S.monotonic () > 9.);
      incr calls; let+ () = Lwt_unix.sleep delay in 7) in
    S.start probe;
    let* () = Lwt.pause () in
    Lwt.finalize (fun () ->
      let begun = S.monotonic () in
      for _ = 1 to 300 do
        expect "pending snapshot was published" (S.snapshot probe = S.Starting)
      done;
      expect "snapshot waited for acquisition" (S.monotonic () -. begun < 0.05);
      expect "snapshot request started more work" (!calls = 1);
      let* () = Lwt_unix.sleep (delay +. 0.1) in
      expect "background work inherited client deadline" (match S.snapshot probe with
        | S.Snapshot (age, 7) -> age >= delay && age < delay +. 0.5
        | _ -> false);
      Lwt.return_unit)
      (fun () -> let+ closed = S.close probe in expect "snapshot did not close" closed))
    [0.5; 2.; 5.] in
  let now = ref 0. in
  let probe = S.create ~clock:(fun () -> !now) ~valid:(fun _ -> true)
    (fun ~background ~deadline:_ ->
      expect "query acquired background mode" (not background);
      Lwt.return 9) in
  let* _ = S.query probe in
  now := 19.9;
  expect "snapshot age was renewed" (S.snapshot probe = S.Snapshot (19.9, 9));
  now := 20.1;
  expect "expired snapshot was accepted" (S.snapshot probe = S.Expired 20.1);
  let+ closed = S.close probe in
  expect "closed actor retained snapshot" (closed && S.snapshot probe = S.Read_failed)

let probe_refresh () =
  let module S = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let current = ref true and calls = ref 0 in
  let probe = S.create ~valid:(fun _ -> !current) (fun ~background:_ ~deadline:_ ->
    incr calls; Lwt.return 7) in
  let* _ = S.query probe in
  S.start probe;
  current := false;
  let* () = Lwt_unix.sleep 0.15 in
  expect "known invalid snapshot remained visible" (S.snapshot probe = S.Read_failed && !calls = 1);
  let* stopped = S.close probe in
  expect "invalid snapshot owner did not close" stopped;
  let offset = ref 0. and cancelled = ref false in
  let clock () = S.monotonic () +. !offset in
  let probe = S.create ~clock ~valid:(fun _ -> true) (fun ~background:_ ~deadline ->
    offset := !offset +. 9.;
    expect "background budget was restarted" (deadline -. clock () <= 1.);
    let work = Lwt_unix.sleep 10. in
    Lwt.on_cancel work (fun () -> cancelled := true);
    Lwt.map (fun () -> 1) work) in
  S.start probe;
  let* () = Lwt_unix.sleep 1.2 in
  expect "setup renewed the job deadline" !cancelled;
  let+ stopped = S.close probe in expect "expired refresh did not close" stopped

let probe_retry () =
  let module S = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let now = ref 0. and calls = ref 0 in
  let probe = S.create ~clock:(fun () -> !now) ~valid:(fun _ -> true)
    ~retry:(( = ) 0) (fun ~background:_ ~deadline:_ ->
      incr calls;
      Lwt.return (if !calls = 1 then 0 else 7)) in
  S.start probe;
  Lwt.finalize (fun () ->
    let* () = Lwt_unix.sleep 0.15 in
    expect "negative sample missing" (S.snapshot probe = S.Snapshot (0., 0));
    now := 2.9;
    let* () = Lwt_unix.sleep 0.15 in
    expect "negative sample ignored cooldown" (!calls = 1);
    now := 3.;
    let* () = Lwt_unix.sleep 0.15 in
    expect "negative sample prevented retry" (!calls = 2 && S.snapshot probe = S.Snapshot (0., 7));
    now := 6.;
    let* () = Lwt_unix.sleep 0.15 in
    expect "positive sample retried early" (!calls = 2 && S.snapshot probe = S.Snapshot (3., 7));
    now := 13.;
    let+ () = Lwt_unix.sleep 0.15 in
    expect "positive sample did not refresh" (!calls = 3 && S.snapshot probe = S.Snapshot (0., 7)))
    (fun () -> let+ closed = S.close probe in expect "retry owner did not close" closed)

let probe_failure () =
  let module S = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let now = ref 0. and calls = ref 0 in
  let probe = S.create ~clock:(fun () -> !now) ~valid:(fun _ -> true)
    (fun ~background:_ ~deadline:_ -> incr calls; Lwt.fail_with "read failed") in
  let* first = S.query probe in
  let* second = S.query probe in
  expect "failed probe bypassed cooldown" (first = None && second = None && !calls = 1);
  now := 3.;
  let* third = S.query probe in
  expect "failed probe could not retry" (third = None && !calls = 2);
  let+ stopped = S.close probe in expect "failed probe did not close" stopped

let probe_drain () =
  let module S = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let now = ref 0. and calls = ref 0 in
  let gate, release = Lwt.wait () in
  let probe = S.create ~clock:(fun () -> !now) ~valid:(fun _ -> true)
    (fun ~background:_ ~deadline:_ -> incr calls; gate) in
  let pending = S.query probe in
  now := 3.;
  let* result = pending in
  expect "probe deadline accepted unfinished work" (result = None);
  now := 10.;
  let pending = S.query probe in
  let* result = pending in
  expect "timeout released active work" (!calls = 1 && result = None);
  let closed = S.close probe in
  let started = Unix.gettimeofday () in
  let* stopped = closed in
  expect "shutdown waited indefinitely" (not stopped && Unix.gettimeofday () -. started < 1.);
  let* result = S.query probe in
  expect "closed probe restarted" (result = None && !calls = 1);
  Lwt.wakeup_later release 1;
  let+ stopped = S.close probe in expect "completed work stayed active" stopped

let root_readers_free driver =
  let rec fill count readers = match Q.join ~epoch:40L readers with
    | Ok (next, _) -> fill (count + 1) next
    | Error _ -> count in
  fill 0 driver.D.epoch_root_queries

let target_send_window () = with_target (fun _ _ _ driver open_target ->
  let module Read = Octra_node_runtime.Status_read_rpc in
  let open Lwt.Syntax in
  let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:"blocked-target" ~addr:"target-test" ~direction:Octra_net.P2p_conn.Inbound in
  let frame = Octra_net.P2p_frame.{msg_type = msg_query_epoch_root; payload = ""} in
  let* () = Lwt_mvar.put conn.write_queue frame in
  Hashtbl.add driver.D.swarm.peers conn.peer_id conn;
  Lwt.finalize (fun () ->
    let pending = Read.sync_target (open_target ()) in
    let* () = await_target driver in
    expect "blocked peer did not retain queue" (not (Lwt_mvar.is_empty conn.write_queue));
    let* () = Lwt_list.iter_s (send_target driver) [0, 42L, false; 1, 42L, false; 2, 42L, false] in
    let+ result = pending in
    expect "blocked peer discarded valid target" (target_value "status" result = `String "agreed"
      && target_value "targetEpoch" result = `String "42");
    expect "blocked target retained reader" (not (listening driver 40L)))
    (fun () -> Hashtbl.clear driver.swarm.peers; Octra_net.P2p_conn.close conn))

let target_parallel () = with_target (fun _ _ _ driver open_target ->
  let open Lwt.Syntax in
  let* fd = Lwt_unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
  let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:"count-target" ~addr:"target-test" ~direction:Octra_net.P2p_conn.Inbound in
  Hashtbl.add driver.D.swarm.peers conn.peer_id conn;
  Lwt.finalize (fun () ->
    let target = open_target () in
    let jobs = List.init 100 (fun _ -> Octra_node_runtime.Status_read_rpc.sync_target target) in
    let* () = await_target driver in
    expect "target exhausted root readers" (root_readers_free driver = 63);
    let* frame = Lwt_mvar.take conn.write_queue in
    expect "target sent another frame" (frame.msg_type = Octra_net.P2p_frame.msg_query_epoch_root);
    let internal = query driver 0.2 in
    expect "target blocked internal reader" (root_readers_free driver = 62);
    let* _ = Lwt_mvar.take conn.write_queue in
    let* () = Lwt_list.iter_s (send_target driver) [0, 42L, false; 1, 42L, false; 2, 42L, false] in
    let* replies = internal in
    expect "internal root query lost replies" (replies <> []);
    let* values = Lwt.all jobs in
    expect "target waiter count differs"
      (List.length (List.filter (fun value -> target_value "targetEpoch" value = `String "42") values) = 16);
    expect "target repeated its broadcast" (Lwt_mvar.is_empty conn.write_queue);
    let* cached = Octra_node_runtime.Status_read_rpc.sync_target target in
    expect "cached target differs" (target_value "targetEpoch" cached = `String "42");
    expect "cache opened a root reader" (root_readers_free driver = 64);
    driver.engine.generation <- driver.engine.generation + 1;
    let* changed = Octra_node_runtime.Status_read_rpc.sync_target target in
    expect "target served another generation" (target_value "targetEpoch" changed = `Null);
    expect "generation change bypassed cooldown" (Lwt_mvar.is_empty conn.write_queue);
    Lwt.return_unit)
    (fun () -> Hashtbl.clear driver.swarm.peers; Octra_net.P2p_conn.close conn))

let control_routes () =
  let module R = Octra_node_runtime in
  let open Lwt.Syntax in
  let calls = ref 0 in
  let query () = incr calls; Lwt.return_ok (`Int 42) in
  let wire method_ params = Yojson.Safe.to_string (`Assoc [
    "jsonrpc", `String "2.0"; "id", `Int 1; "method", `String method_; "params", params]) in
  let body = wire "octra_syncTarget" (`List []) in
  let* response = R.Rpc_http.control_reply ~query body in
  expect "control did not return target" (Yojson.Safe.Util.member "result" response = `Int 42);
  let* () = Lwt_list.iter_s (fun body ->
    let+ response = R.Rpc_http.control_reply ~query body in
    expect "control accepted another route" (Yojson.Safe.Util.member "error" response <> `Null))
    [wire "node_status" (`List []); wire "octra_syncTarget" (`List [`Int 1]); "[" ^ body ^ "]"] in
  expect "refused control request reached probe" (!calls = 1);
  let routes = R.Status_read_rpc.dispatch {status_read = (fun _ _ () -> query ())} in
  let* () = Lwt_list.iter_s (fun headers ->
    let request = Cohttp.Request.make ~meth:`POST ~headers:(Cohttp.Header.of_list headers)
      (Uri.of_string "/rpc") in
    let* _, reply = R.Rpc_http.handle_rpc_post ~process:(fun meta body ->
      R.Rpc_dispatch.process_body meta body () routes) request (Cohttp_lwt.Body.of_string body) in
    let+ reply = Cohttp_lwt.Body.to_string reply in
    let rejected = match Yojson.Safe.from_string reply with
      | `Assoc fields ->
        (match List.assoc_opt "error" fields with
         | Some (`Assoc fields) -> List.assoc_opt "code" fields = Some (`Int (-32601))
         | _ -> false)
      | _ -> false in
    expect "HTTP exposed control route" rejected)
    [[]; ["x-forwarded-for", "127.0.0.1"; "x-real-ip", "::1"; "cf-connecting-ip", "127.0.0.1"]] in
  expect "HTTP reached target probe" (!calls = 1);
  let meta = R.Rpc_http.{rpc_peer = "local"; rpc_user_agent = "root-query"; rpc_body_bytes = 0} in
  let* response = R.Grpc_service.invoke
    ~call:(fun meta request -> R.Rpc_dispatch.handle_request meta request () routes)
    ~meta ~path:"/octra.node.v1.Node/SyncTarget" Bytes.empty in
  expect "gRPC exposed target route"
    (response.status.code = R.Grpc_status.Unimplemented
      && response.body = None && response.rpc_method = None && !calls = 1);
  Lwt.return_unit

let control_exchange directory body =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let wire = body ^ "\n" in
  let rec send offset =
    if offset = String.length wire then Lwt.return_unit else
    let* count = Lwt_unix.write_string socket wire offset (String.length wire - offset) in
    if count = 0 then Lwt.fail End_of_file else send (offset + count) in
  let bytes = Bytes.create 4096 in
  let rec receive data =
    let* count = Lwt_unix.read socket bytes 0 (Bytes.length bytes) in
    if count = 0 then Lwt.fail End_of_file else
    let data = data ^ Bytes.sub_string bytes 0 count in
    if String.contains data '\n' then Lwt.return (Yojson.Safe.from_string data)
    else receive data in
  Lwt.finalize (fun () -> Lwt_unix.with_timeout 3. (fun () ->
    let* () = Lwt_unix.connect socket (Http.control_address (Http.control_path directory)) in
    let* () = Lwt_unix.sleep 0.01 in
    let* () = send 0 in receive "")) (fun () -> Lwt_unix.close socket)

let control_socket () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "sync-control" in
  let alias = directory ^ "-link" in
  Unix.symlink directory alias;
  let calls = ref 0 in
  let query () = incr calls; Lwt.return_ok (`Int 42) in
  let run () =
    let path = Http.control_path directory in
    expect "control directory permissions differ"
      ((Unix.stat (Filename.dirname path)).st_perm = 0o700);
    expect "control socket permissions differ" ((Unix.stat path).st_perm = 0o600);
    let note = ref None in
    let* () = Http.with_sync_control ~data_dir:directory ~query
      ~unavailable:(fun reason -> note := Some reason) (fun () -> Lwt.return_unit) in
    expect "duplicate listener was accepted" (Option.is_some !note);
    let* response = control_exchange directory
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
    expect "Unix control did not reach probe"
      (!calls = 1 && Yojson.Safe.Util.member "result" response = `Int 42);
    let* response = control_exchange directory
      "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"node_status\",\"params\":[]}" in
    expect "Unix control exposed public routes"
      (!calls = 1 && Yojson.Safe.Util.member "error" response <> `Null);
    Lwt_list.iter_s (fun count ->
      let+ response = control_exchange directory
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
      expect "Unix control lost completed client capacity"
        (!calls = count && Yojson.Safe.Util.member "result" response = `Int 42))
      (List.init 40 (fun index -> index + 2)) in
  Lwt.finalize (fun () ->
    let+ () = Http.with_sync_control ~data_dir:alias ~query run in
    expect "control socket survived shutdown" (not (Sys.file_exists (Http.control_path directory))))
    (fun () -> Test_workspace.remove alias; Test_workspace.remove directory; Lwt.return_unit)

let control_paths () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  Lwt_list.iter_s (fun mode ->
    let directory = Test_workspace.unique_dir "control-path" in
    let folder = Filename.concat directory "control" in
    let calls = ref 0 in
    Lwt.finalize (fun () ->
      (match mode with
       | "missing" -> Unix.rmdir directory
       | "link" -> Unix.symlink directory folder
       | "wide" -> Unix.mkdir folder 0o755; Unix.chmod folder 0o755
       | "stage_link" ->
         Unix.mkdir folder 0o700;
         Unix.symlink directory (Filename.concat folder "next")
       | "socket_link" ->
         Unix.mkdir folder 0o700;
         Unix.symlink directory (Http.control_path directory)
       | _ ->
         Unix.mkdir folder 0o700;
         let path = if mode = "stage_file" then Filename.concat folder "next"
           else Http.control_path directory in
         let fd = Unix.openfile path
           [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL] 0o600 in Unix.close fd);
      let preserved = List.filter_map (fun path ->
        try Some (path, Unix.lstat path)
        with Unix.Unix_error (Unix.ENOENT, _, _) -> None)
        [Http.control_path directory; Filename.concat folder "next"] in
      let note = ref None and served = ref false in
      let* () = Http.with_sync_control ~data_dir:directory
        ~query:(fun () -> incr calls; Lwt.return_ok `Null)
        ~unavailable:(fun reason -> note := Some reason)
        (fun () -> served := true; Lwt.return_unit) in
      expect "unsafe control path was accepted" (Option.is_some !note && !calls = 0);
      expect "unavailable control stopped public service" !served;
      List.iter (fun (path, (prior : Unix.stats)) ->
        let after = Unix.lstat path in
        expect "unsafe control path was changed"
          (prior.st_dev = after.st_dev && prior.st_ino = after.st_ino
            && prior.st_kind = after.st_kind && prior.st_perm = after.st_perm)) preserved;
      Lwt.return_unit)
      (fun () -> Test_workspace.remove directory; Lwt.return_unit))
    ["missing"; "link"; "wide"; "file"; "stage_link"; "stage_file"; "socket_link"]

let control_pressure () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-pressure" in
  let body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
  let full, signal = Lwt.wait () in
  let released, release = Lwt.wait () in
  let calls = ref 0 in
  let query () =
    incr calls;
    if !calls = 16 then Lwt.wakeup_later signal ();
    let+ () = Lwt.protected released in Ok (`Int 42) in
  let run () =
    let requests = List.init 16 (fun _ -> control_exchange directory body) in
    Lwt.finalize (fun () ->
      let* () = Lwt_unix.with_timeout 2. (fun () -> Lwt.protected full) in
      let* refused = Lwt.catch
        (fun () -> let+ _ = control_exchange directory body in false)
        (function
          | End_of_file | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> Lwt.return_true
          | error -> Lwt.fail error) in
      expect "control accepted excess clients" (refused && !calls = 16);
      Lwt.wakeup_later release ();
      let* responses = Lwt.all requests in
      expect "control lost admitted requests"
        (List.for_all (fun response -> Yojson.Safe.Util.member "result" response = `Int 42) responses);
      let+ response = control_exchange directory body in
      expect "control did not recover client capacity"
        (!calls = 17 && Yojson.Safe.Util.member "result" response = `Int 42))
      (fun () -> List.iter Lwt.cancel requests; Lwt.return_unit) in
  Lwt.finalize (fun () -> Http.with_sync_control ~data_dir:directory ~query run)
    (fun () -> Test_workspace.remove directory; Lwt.return_unit)

let control_failures () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-failures" in
  let body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
  let failed = ref true in
  let calls = ref 0 in
  let query () =
    incr calls;
    if !failed then Lwt.fail_with "control query failed" else Lwt.return_ok (`Int 42) in
  let run () =
    let* () = Lwt_list.iter_s (fun count ->
      let* refused = Lwt.catch
        (fun () -> let+ _ = control_exchange directory body in false)
        (function
          | End_of_file | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> Lwt.return_true
          | error -> Lwt.fail error) in
      expect "control lost failed client capacity" (refused && !calls = count);
      Lwt.return_unit) (List.init 40 (fun index -> index + 1)) in
    failed := false;
    let+ response = control_exchange directory body in
    expect "control did not recover after query errors"
      (!calls = 41 && Yojson.Safe.Util.member "result" response = `Int 42) in
  Lwt.finalize (fun () -> Http.with_sync_control ~data_dir:directory ~query run)
    (fun () -> Test_workspace.remove directory; Lwt.return_unit)

let control_stop () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-stop" in
  let body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
  let entered, signal = Lwt.wait () in
  let pending, _ = Lwt.task () in
  let stopped = ref false in
  let query () =
    Lwt.wakeup_later signal ();
    Lwt.finalize (fun () -> pending) (fun () -> stopped := true; Lwt.return_unit) in
  let idle, _ = Lwt.task () in
  let listening, ready = Lwt.wait () in
  let server = Http.with_sync_control ~data_dir:directory ~query
    (fun () -> Lwt.wakeup_later ready (); idle) in
  let request = let* () = listening in control_exchange directory body in
  Lwt.finalize (fun () ->
    let* () = Lwt.catch
      (fun () -> Lwt_unix.with_timeout 2. (fun () -> Lwt.protected entered))
      (function Lwt_unix.Timeout -> Lwt.fail_with "control query did not start" | error -> Lwt.fail error) in
    Lwt.cancel server;
    let* () = Lwt.catch (fun () -> server)
      (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error) in
    let* refused = Lwt.catch (fun () -> let+ _ = request in false)
      (function
        | End_of_file | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> Lwt.return_true
        | Lwt_unix.Timeout -> Lwt.fail_with "control shutdown left request waiting"
        | error -> Lwt.fail error) in
    expect "control shutdown retained active client"
      (refused && !stopped && not (Sys.file_exists (Http.control_path directory)));
    Http.with_sync_control ~data_dir:directory ~query:(fun () -> Lwt.return_ok (`Int 42))
      (fun () -> let+ response = control_exchange directory body in
        expect "control restart did not accept client"
          (Yojson.Safe.Util.member "result" response = `Int 42)))
    (fun () -> Lwt.cancel server; Lwt.cancel request; Test_workspace.remove directory; Lwt.return_unit)

let control_accept () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-accept" in
  let body = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
  let errors = [Unix.EMFILE; Unix.ENFILE; Unix.ENOBUFS; Unix.ENOMEM;
    Unix.EINTR; Unix.EAGAIN; Unix.ECONNABORTED; Unix.EWOULDBLOCK;
    Unix.EMFILE; Unix.ENFILE] in
  let attempts = ref 0 and delays = ref [] in
  let take socket =
    let index = !attempts in
    incr attempts;
    if index < List.length errors then
      Lwt.fail (Unix.Unix_error (List.nth errors index, "accept", ""))
    else Lwt_unix.accept ~cloexec:true socket in
  let sleep delay = delays := delay :: !delays; Lwt.pause () in
  let run () =
    let+ response = control_exchange directory body in
    expect "control did not recover accept"
      (Yojson.Safe.Util.member "result" response = `Int 42);
    let expected = List.init (List.length errors) (fun index ->
      min 1.0 (0.01 *. Float.of_int (1 lsl min 7 index))) in
    expect "control accept retry delay differs" (List.rev !delays = expected) in
  Lwt.finalize (fun () ->
    Http.with_sync_control ~take ~sleep ~data_dir:directory
      ~query:(fun () -> Lwt.return_ok (`Int 42)) run)
    (fun () -> Test_workspace.remove directory; Lwt.return_unit)

let control_accept_stop () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-accept-stop" in
  let attempts = ref 0 and notes = ref 0 in
  let idle, _ = Lwt.task () in
  let waiting, signal = Lwt.wait () in
  let pause, _ = Lwt.task () in
  let take _ = incr attempts; Lwt.fail (Unix.Unix_error (Unix.EMFILE, "accept", "")) in
  let sleep _ = Lwt.wakeup_later signal (); pause in
  let server = Http.with_sync_control ~take ~sleep ~data_dir:directory
    ~unavailable:(fun _ -> incr notes) ~query:(fun () -> failwith "accept invoked query")
    (fun () -> idle) in
  Lwt.finalize (fun () ->
    let* () = Lwt_unix.with_timeout 1. (fun () -> Lwt.protected waiting) in
    Lwt.cancel server;
    let* () = Lwt.catch (fun () -> server)
      (function Lwt.Canceled -> Lwt.return_unit | error -> Lwt.fail error) in
    expect "control accept cancellation was lost"
      (!attempts = 1 && !notes = 1 && Lwt.state pause = Lwt.Fail Lwt.Canceled);
    let take _ = incr attempts; Lwt.fail (Unix.Unix_error (Unix.EBADF, "accept", "")) in
    let* () = Http.with_sync_control ~take
      ~sleep:(fun _ -> failwith "permanent accept error retried")
      ~unavailable:(fun _ -> incr notes) ~data_dir:directory
      ~query:(fun () -> failwith "closed listener invoked query")
      (fun () -> Lwt.pause ()) in
    expect "permanent accept error was not isolated" (!attempts = 2 && !notes = 2);
    Lwt.return_unit)
    (fun () -> Lwt.cancel server; Test_workspace.remove directory; Lwt.return_unit)

let control_publish () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-publish" in
  let path = Http.control_path directory in
  let staged = Filename.concat (Filename.dirname path) "next" in
  let interrupted = ref false and notes = ref [] in
  let bind socket address =
    let* () = Lwt_unix.bind socket address in
    expect "unsecured control socket was published" (not (Sys.file_exists path));
    Unix.chmod staged 0o777;
    interrupted := true;
    Lwt.fail_with "control bind interrupted" in
  let query () = Lwt.return_ok (`Int 42) in
  let run () =
    expect "published control mode differs" ((Unix.stat path).st_perm = 0o600);
    expect "staged socket survived publication" (not (Sys.file_exists staged));
    let+ response = control_exchange directory
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
    expect "control did not recover interrupted bind"
      (Yojson.Safe.Util.member "result" response = `Int 42) in
  Lwt.finalize (fun () ->
    let* () = Http.with_sync_control ~bind ~data_dir:directory ~query
      ~unavailable:(fun reason -> notes := reason :: !notes) (fun () -> Lwt.return_unit) in
    expect "control bind interruption was not preserved"
      (!interrupted && List.length !notes = 1 && not (Sys.file_exists path));
    let* () = Http.with_sync_control ~data_dir:directory ~query run in
    expect "control path survived shutdown"
      (not (Sys.file_exists path) && not (Sys.file_exists staged));
    let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    Fun.protect ~finally:(fun () -> Unix.close socket) (fun () ->
      Unix.bind socket (Http.control_address path); Unix.chmod path 0o777);
    Http.with_sync_control ~data_dir:directory ~query run)
    (fun () -> Test_workspace.remove directory; Lwt.return_unit)

let control_pause_stop () =
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let directory = Test_workspace.unique_dir "control-pause-stop" in
  let done_, finish = Lwt.task () in
  let waiting, ready = Lwt.wait () in
  let attempts = ref 0 and called = ref false and errors = ref [] in
  let prior = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := (fun error -> errors := error :: !errors);
  let take socket =
    incr attempts;
    if !attempts = 2 then begin
      Lwt.wakeup_later finish ();
      fst (Lwt.task ())
    end else Lwt_unix.accept ~cloexec:true socket in
  let server = Http.with_sync_control ~take ~data_dir:directory
    ~query:(fun () -> called := true; Lwt.return_ok `Null)
    (fun () -> Lwt.wakeup_later ready (); done_) in
  let request = let* () = waiting in control_exchange directory
    "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"octra_syncTarget\",\"params\":[]}" in
  Lwt.finalize (fun () ->
    let* () = Lwt_unix.with_timeout 2. (fun () -> server) in
    let* refused = Lwt.catch (fun () -> let+ _ = request in false)
      (function
        | End_of_file | Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> Lwt.return_true
        | error -> Lwt.fail error) in
    let* () = Lwt.pause () in
    expect "paused control client escaped cancellation"
      (refused && not !called && !errors = [] && !attempts = 2);
    Lwt.return_unit)
    (fun () -> Lwt.cancel server; Lwt.cancel request; Lwt.async_exception_hook := prior;
      Test_workspace.remove directory; Lwt.return_unit)

let target_delayed () = with_target (fun _ _ _ driver open_target ->
  let module Read = Octra_node_runtime.Status_read_rpc in
  let open Lwt.Syntax in
  let target = open_target () in
  Read.start_sync_target target;
  let* () = await_target driver in
  let* () = Lwt_unix.sleep 2.6 in
  expect "background ended before delayed replies" (listening driver 40L);
  let* first = Read.sync_snapshot target in
  expect "unfinished background invented agreement" (target_value "targetEpoch" first = `Null);
  let* () = Lwt_list.iter_s (send_target driver)
    [0, 42L, true; 2, 42L, false; 2, 42L, false] in
  let* () = Lwt_unix.sleep 0.2 in
  expect "invalid replies finished background collection" (listening driver 40L);
  let* () = Lwt_list.iter_s (send_target driver)
    [0, 42L, false; 1, 42L, false; 2, 42L, false] in
  let* checked = Read.sync_target target in
  expect "delayed signed target was lost" (target_value "targetEpoch" checked = `String "42");
  let+ snapshot = Read.sync_snapshot target in
  expect "delayed target was not published" (target_value "status" snapshot = `String "agreed"))

let target_delayed_change () = with_target (fun _ _ _ driver open_target ->
  let module Read = Octra_node_runtime.Status_read_rpc in
  let open Lwt.Syntax in
  let target = open_target () in
  Read.start_sync_target target;
  let* () = await_target driver in
  let* () = Lwt_unix.sleep 2.6 in
  driver.engine.generation <- driver.engine.generation + 1;
  let* () = Lwt_list.iter_s (send_target driver)
    [0, 42L, false; 1, 42L, false; 2, 42L, false] in
  let* result = Read.sync_target target in
  expect "delayed reply ignored generation" (target_value "targetEpoch" result = `Null);
  let+ snapshot = Read.sync_snapshot target in
  expect "changed background published agreement" (target_value "targetEpoch" snapshot = `Null))

let target_collection_end () = with_target (fun _ _ _ driver open_target ->
  let module Read = Octra_node_runtime.Status_read_rpc in
  let module Probe = Octra_node_runtime.Sync_probe in
  let open Lwt.Syntax in
  let target = open_target () in
  Read.start_sync_target target;
  let* () = await_target driver in
  let started = Probe.monotonic () in
  let* () = Lwt_list.iter_s (send_target driver)
    [0, 999999L, false; 1, 999999L, false; 2, 999999L, false] in
  let* () = Lwt_unix.sleep 2.6 in
  expect "ceiling bypassed background collection" (listening driver 40L);
  let rec completed () =
    let* snapshot = Read.sync_snapshot target in
    if target_value "status" snapshot = `String "no_agreement" then Lwt.return snapshot
    else if Probe.monotonic () -. started > 7. then Lwt.fail_with "collection did not finish"
    else let* () = Lwt_unix.sleep 0.05 in completed () in
  let+ result = completed () in
  expect "collection lost finite deadline" (Probe.monotonic () -. started < 7.);
  expect "collection invented target" (target_value "targetEpoch" result = `Null);
  expect "completed collection retained reader" (not (listening driver 40L)))

let target_snapshot () = with_target (fun _ _ head driver open_target ->
  let module Read = Octra_node_runtime.Status_read_rpc in
  let module Http = Octra_node_runtime.Rpc_http in
  let open Lwt.Syntax in
  let target = open_target () in
  Read.start_sync_target target;
  let begun = Octra_node_runtime.Sync_probe.monotonic () in
  let* first = Read.sync_snapshot target in
  expect "snapshot waited for background read"
    (Octra_node_runtime.Sync_probe.monotonic () -. begun < 0.05);
  expect "empty snapshot was not explicit" (target_value "status" first = `String "starting");
  let* () = await_target driver in
  let* () = Lwt_list.iter_s (send_target driver) [0, 42L, false; 1, 42L, false; 2, 42L, false] in
  let* checked = Read.sync_target target in
  expect "background sample was not verified" (target_value "status" checked = `String "agreed");
  let started = Octra_node_runtime.Sync_probe.monotonic () in
  let* results = Lwt_list.map_s (fun _ -> Read.sync_snapshot target) (List.init 300 Fun.id) in
  expect "snapshot started a new read" (Octra_node_runtime.Sync_probe.monotonic () -. started < 0.05);
  List.iter (fun value ->
    expect "snapshot changed head" (target_value "headRoot" value = `String head.state_root);
    expect "snapshot lost agreement" (target_value "status" value = `String "agreed");
    expect "snapshot age missing" (match target_value "ageMs" value with
      | `Int age -> age >= 2000 && age < 2500 | _ -> false)) results;
  let wire params = Yojson.Safe.to_string (`Assoc [
    "jsonrpc", `String "2.0"; "id", `Int 1; "method", `String "octra_syncTarget";
    "params", params]) in
  let* old = Http.control_reply ~query:(fun () -> Read.sync_snapshot target) (wire (`List [])) in
  let fields = Yojson.Safe.Util.member "result" old |> Yojson.Safe.Util.to_assoc in
  expect "snapshot broke existing clients" (List.length fields = 6 && not (List.mem_assoc "ageMs" fields));
  let* current = Http.control_reply ~query:(fun () -> Read.sync_snapshot target)
    (wire (`List [`String "snapshot"])) in
  let fields = Yojson.Safe.Util.member "result" current |> Yojson.Safe.Util.to_assoc in
  expect "snapshot omitted diagnostics" (List.length fields = 8 && List.mem_assoc "ageMs" fields);
  driver.engine.generation <- driver.engine.generation + 1;
  let+ changed = Read.sync_snapshot target in
  expect "snapshot survived generation change" (target_value "status" changed = `String "changed"
    && target_value "targetEpoch" changed = `Null))

let () =
  let controls = [
    "control_socket", control_socket; "control_paths", control_paths;
    "control_pressure", control_pressure; "control_failures", control_failures;
    "control_stop", control_stop; "control_accept", control_accept;
    "control_accept_stop", control_accept_stop; "control_publish", control_publish;
    "control_pause_stop", control_pause_stop
  ] in
  let cases = if Array.to_list Sys.argv = [Sys.argv.(0); "--history"] then
    ["target_history", target_history]
    else if Array.to_list Sys.argv = [Sys.argv.(0); "--control"] then
      controls
    else if Array.length Sys.argv = 3 && Sys.argv.(1) = "--control" then
      [Sys.argv.(2), List.assoc Sys.argv.(2) controls]
    else if Array.to_list Sys.argv = [Sys.argv.(0); "--records"] then
      ["target_records", target_records; "target_floor", target_floor] else
    ["bundle_join", bundle_join; "bundle_cancel", bundle_cancel; "bundle_expire", bundle_expire;
      "bundle_epoch", bundle_epoch; "bundle_readers", bundle_readers;
      "bundle_slots", bundle_slots; "bundle_exception", bundle_exception;
      "probe_delta", probe_delta; "probe_child", probe_child; "probe_process", probe_process;
      "probe_snapshot", probe_snapshot; "probe_refresh", probe_refresh;
      "probe_retry", probe_retry;
      "target_snapshot", target_snapshot; "target_delayed", target_delayed;
      "target_delayed_change", target_delayed_change;
      "target_collection_end", target_collection_end;
      "target_records", target_records; "target_floor", target_floor;
      "probe_coalesce", probe_coalesce; "probe_failure", probe_failure;
      "probe_drain", probe_drain; "control_routes", control_routes] @ controls @
    ["target_send_window", target_send_window; "target_parallel", target_parallel;
      "target_route", target_route; "target_replies", target_replies;
      "target_changes", target_changes; "target_fresh", target_fresh;
      "target_offline", target_offline; "target_timeout", target_timeout;
      "joined", joined; "cancel_one", cancel_one; "cancel_all", cancel_all;
      "fresh_overlap", fresh_overlap; "fresh_same_signer", fresh_same_signer;
      "fresh_cancel", fresh_cancel; "collection_window", collection_window;
      "large_roster", large_roster; "historical_weights", historical_weights;
      "health_query_link", health_query_link;
      "historical_identity", historical_identity; "capacity_growth", capacity_growth;
      "weight_snapshot", weight_snapshot; "source_reachability", source_reachability;
      "reader_capacity", reader_capacity;
      "reply_capacity", reply_capacity; "source_mismatch", source_mismatch;
      "root_ingestion", root_ingestion; "root_height_rules", root_height_rules;
      "root_query_height", root_query_height;
      "read_error", read_error; "epochs", epochs; "wait_modes", wait_modes;
      "request_disabled", request_mode false; "request_enabled", request_mode true;
      "send_timeout", send_timeout; "send_collection", send_collection; "cancel_send", cancel_send;
      "cancel_send_joined", cancel_send_joined; "cancel_fresh_send", cancel_fresh_send] in
  Lwt_main.run (Lwt_list.iter_s (fun (name, run) ->
    let open Lwt.Syntax in
    let* () = run () in
    Printf.printf "event = root_query case = %s status = pass\n%!" name;
    Lwt.return_unit)
    cases)