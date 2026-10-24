(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus
module C = C_types
module E = C_engine
module D = C_driver
module V = C_vote_log
module H = C_hash

let get = function Ok value -> value | Error reason -> failwith reason
let check name yes = if not yes then failwith name
let chain = "octra-devnet-9871-cluster"
let height = Option.get (C_quorum_policy.linear_epoch ~chain_id:chain)
let raw text = Digestif.SHA256.(digest_string text |> to_raw_string)
let keys = List.init 200 (fun index ->
  let secret = raw ("polc-key-" ^ string_of_int index) in
  let priv = Mirage_crypto_ec.Ed25519.priv_of_octets secret |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv priv |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, secret)
let sign address msg =
  let _, secret = List.find (fun (member, _) -> member.C.address = address) keys in
  H.sign_ed25519 ~priv_raw:secret ~msg
let set count = get (C.make_weighted_validator_set
  (List.filteri (fun i _ -> i < count) keys |> List.map (fun (v, _) -> v, Z.one)))
let header epoch creator = C.{proto_version = C.proto_version_current; chain_id = chain;
  epoch_id = epoch; prev_state_root = raw "prior"; tx_list_hash = E.tx_list_hash_for_header [];
  receipt_root = H.receipt_root []; proposed_state_root = raw "next";
  parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = creator;
  txid_hi = 0L; ts = 0.}
let vote epoch round pid kind member =
  let value = C.{chain_id = chain; epoch_id = epoch; round; proposal_id = pid;
    vote_type = kind; validator = member.C.address; signature = ""} in
  {value with signature = sign member.address (H.vote_sign_bytes value)}
let path () = Test_workspace.unique_dir "polc-cache"

let make ?(durable = ref true) directory members epoch member role ready pending =
  let store = V.disk ~data_dir:directory in
  let swarm = Octra_net.P2p_swarm.create Octra_net.P2p_swarm.{
    listen_port = 0; chain_id = chain;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey member.C.pubkey;
    node_addr = member.address; pubkey_raw = member.pubkey;
    consensus_config_hash = raw "config"; binary_hash = raw "binary";
    require_binary_hash = false; upgrade_plan = None; profile_plan = [];
    allowed_pubkeys = []; bootstrap_peers = []; max_peers = 0;
    sign_fn = sign member.address; best_epoch_fn = (fun () -> Int64.pred epoch);
    best_root_fn = (fun () -> raw "prior")} in
  let config = D.{chain_id = chain; my_addr = member.address; sign_fn = sign member.address;
    verify_fn = (fun address msg signature -> match C.pubkey_of_addr members address with
      | None -> false | Some pubkey -> H.verify_ed25519 ~pubkey_raw:pubkey ~msg ~signature);
    role_can_vote = (fun () -> !role); can_vote = (fun () -> !ready);
    execute_fn = (fun _ -> true); verify_proposal = (fun _ -> Lwt.return Proposal_accept);
    verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
    on_finalized = (fun ~validator_set:_ _ -> Lwt.fail_with "unexpected final apply");
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id ~round ~proposal_id ~proposed_state_root:_
      ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ ->
      let proofs = get (V.polc_load store ~chain_id:chain ~epoch_id ~validator_set:members) in
      check "proof durable before pending" (List.exists (fun (v : C.vote) ->
        v.round = round && v.proposal_id = proposal_id) proofs);
      incr pending; Lwt.return !durable);
    lookup_epoch_root = (fun _ -> None); local_head_epoch = (fun () -> Int64.pred epoch);
    lookup_bundle = (fun _ -> None);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
    on_resource_attestation = (fun _ -> Lwt.return_unit);
    scheduled_validator_set_config = None;
    load_scheduled_validator_set_config = (fun () -> Lwt.return_none);
    resource_committee_config = None} in
  D.create ~config ~validator_set:members ~swarm ~start_height:epoch
    ~sync_log:(C_sync_log.memory ()) ~relief_log:(C_relief_log.memory ()) ~vote_log:store

let test_local_proof () =
  List.iter (fun count -> List.iter (fun active ->
    let epoch = if active then height else Int64.pred height in
    let members = set count in
    let leader = C.leader_of members ~epoch_id:epoch ~round:0 in
    let pending = ref 0 in
    let durable = ref false in
    let node = make ~durable (path ()) members epoch leader (ref true) (ref true) pending in
    let value = header epoch leader.address in
    let pid = H.proposal_id value in
    E.start_height node.engine epoch;
    ignore (E.drain_outputs node.engine);
    let remote = members.validators |> List.filter (fun v -> v.C.address <> leader.address)
      |> List.filteri (fun i _ -> i < members.quorum - 1) in
    List.iter (fun member -> ignore (E.add_vote ~policy:node.policy node.engine.prevotes
      (vote epoch 0 pid C.Prevote member) ~validator_set:members)) remote;
    E.do_propose node.engine value [] ~sign_fn:node.config.sign_fn;
    let proof = E.polc_votes_for_round node.engine 0 in
    check "local quorum recorded by rule" (if active then List.length proof >= members.quorum else proof = []);
    if active then begin
      let precommit = E.drain_outputs node.engine |> List.find_map (function
        | E.SendVote (v, _) when v.C.vote_type = C.Precommit -> Some v | _ -> None) |> Option.get in
      check "pending failure stops vote" (not (Lwt_main.run (D.persist_precommit node precommit)));
      check "failed pending keeps old lock" (node.proof_lock = None && Hashtbl.length node.durable_votes = 0);
      let receipt = Option.get node.saved_polc in
      durable := true;
      check "local proof saved" (Lwt_main.run (D.persist_precommit node precommit));
      check "pending follows proof" (!pending = 2 && node.proof_lock = Some (epoch, 0));
      check "pending retry reuses proof" (Option.get node.saved_polc == receipt)
    end) [false; true]) [1; 4]

let inode directory =
  try Some (Unix.stat (Filename.concat directory "vote_log/polc")).Unix.st_ino
  with Unix.Unix_error (Unix.ENOENT, _, _) -> None

let test_many_votes () =
  let members = set 200 in
  let member = List.hd members.validators in
  let directory = path () in
  let role = ref true in
  let node = make directory members height member role (ref false) (ref 0) in
  E.start_height node.engine height;
  node.running <- true;
  let leader = C.leader_of members ~epoch_id:height ~round:0 in
  let value = header height leader.address in
  let pid = H.proposal_id value in
  let proposal = C.{chain_id = chain; epoch_id = height; round = 0; valid_round = None;
    header = value; tx_hashes = []; parent_commit = None; proposer = leader.address; signature = ""} in
  E.cache_proposal_message node.engine {proposal with signature = sign leader.address (H.propose_sign_bytes proposal)};
  node.engine.state <- {node.engine.state with step = C.PrevoteStep};
  let fd, writer = Lwt_unix.pipe () in
  let peer = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:"polc-peer" ~addr:"polc-peer" ~direction:Octra_net.P2p_conn.Inbound in
  let previous = ref None in
  let writes = ref 0 in
  let send value =
    Lwt_main.run (D.on_p2p_message node peer Octra_net.P2p_frame.{
      msg_type = msg_cons_vote; payload = C_codec.encode_vote value});
    let current = inode directory in
    if current <> !previous then incr writes;
    previous := current in
  Fun.protect ~finally:(fun () -> Lwt_main.run (let open Lwt.Syntax in
    let* () = Octra_net.P2p_conn.close peer in Lwt_unix.close writer)) (fun () ->
    List.iteri (fun index member ->
      send (vote height 0 pid C.Prevote member);
      check "no save before quorum" (index + 1 >= members.quorum || inode directory = None)) members.validators;
    check "one proof publication for 200 votes" (!writes = 1);
    check "driver kept receipt" (Option.is_some node.saved_polc);
    let receipt = Option.get node.saved_polc in
    List.iter (fun member -> send (vote height 0 pid C.Precommit member)) members.validators;
    check "extra precommits keep receipt" (Option.get node.saved_polc == receipt && !writes = 1);
    let saved = get (V.polc_load node.vote_log ~policy:node.policy ~chain_id:chain ~epoch_id:height
      ~validator_set:members) in
    check "saved quorum not all extra votes" (List.length saved = members.quorum);
    get (D.keep_polc node 0 ~required:true);
    check "required proof reuses sync" (inode directory = !previous);
    let node = make directory members height member role (ref false) (ref 0) in
    check "restart has no receipt" (node.saved_polc = None);
    get (D.restore_polc node);
    check "restart restores quorum" (List.length (E.polc_votes_for_round node.engine 0) = members.quorum))

let test_observer () =
  let members = set 4 in
  List.iter (fun member ->
    let role = ref (C.is_validator members member.C.address) in
    let directory = path () in
    let node = make directory members height member role (ref false) (ref 0) in
    E.start_height node.engine height;
    E.start_round node.engine 1;
    let pid = raw "observer" in
    List.iter (fun v -> E.record_historical_prevote node.engine (vote height 0 pid C.Prevote v))
      members.validators;
    node.engine.state <- {node.engine.state with valid_round = 0};
    role := false;
    get (D.keep_polc node 0 ~required:false);
    check "observer does not write" (inode directory = None && node.saved_polc = None);
    if C.is_validator members member.address then begin
      role := true;
      get (D.keep_polc node 0 ~required:false);
      check "returning voter saves proof" (inode directory <> None)
    end) [List.hd members.validators; fst (List.nth keys 5)]

let test_receipt_context () =
  let members = set 4 in
  let store = V.disk ~data_dir:(path ()) in
  let policy = C_quorum_policy.for_chain chain in
  let pid = raw "receipt" in
  let proof round = List.map (vote height round pid C.Prevote) members.validators in
  let save ?sync round lock = V.polc_save ~policy ?sync store ~chain_id:chain ~epoch_id:height
    ~validator_set:members ~locked_round:lock (proof round) in
  let saved receipt ?(members = members) ?(policy = policy) ?(epoch = height) ?(chain = chain)
      ?(pid = pid) round = V.polc_saved receipt ~policy ~chain_id:chain ~epoch_id:epoch
    ~validator_set:members ~round ~proposal_id:pid in
  let calls = ref 0 in
  let sync _ = incr calls; if !calls = 1 then Error "sync failed" else Ok () in
  check "sync failure has no receipt" (Result.is_error (save ~sync 0 (-1)));
  let first = get (save ~sync 0 (-1)) in
  check "retry sync" (!calls = 2 && saved first 0);
  check "other height not saved" (not (saved first ~epoch:(Int64.succ height) 0));
  check "other chain not saved" (not (saved first ~chain:"other" 0));
  check "other value not saved" (not (saved first ~pid:(raw "other") 0));
  check "other set not saved" (not (saved first ~members:(set 3) 0));
  check "other policy not saved" (not (saved first ~policy:(C_quorum_policy.for_chain "other") 0));
  ignore (get (save 1 0));
  let third = get (save 2 0) in
  check "kept receipt rounds" (saved third 0 && saved third 2 && not (saved third 1));
  let revisited = get (save 1 0) in
  check "revisited proof synced" (saved revisited 0 && saved revisited 1 && saved revisited 2);
  check "bad proof cannot get receipt" (Result.is_error (V.polc_save ~policy store
    ~chain_id:chain ~epoch_id:height ~validator_set:members ~locked_round:0
    (List.map (fun (v : C.vote) -> {v with signature = String.make 64 '\000'}) (proof 3))))

let test_pending_proofs () =
  Test_workspace.with_dir "pending-proofs" (fun directory ->
    let members = set 4 in
    let store = V.disk ~data_dir:directory in
    let policy = C_quorum_policy.for_chain chain in
    let proof round = List.map (vote height round (raw (string_of_int round)) C.Prevote) members.validators in
    let save keep_rounds round = V.polc_save ~policy ~keep_rounds store ~chain_id:chain ~epoch_id:height
      ~validator_set:members ~locked_round:(-1) (proof round) in
    List.iter (fun round -> ignore (get (save [0; 1; 2] round))) [0; 1; 2];
    let original = inode directory in
    check "four pending proofs must wait" (save [0; 1; 2; 3] 3 = Error "vote proof retention is full");
    check "capacity refusal replaced proof file" (inode directory = original);
    let rounds () = get (V.polc_load store ~policy ~chain_id:chain ~epoch_id:height ~validator_set:members)
      |> List.map (fun (vote : C.vote) -> vote.round) |> List.sort_uniq Int.compare in
    check "protected proofs no longer readable" (rounds () = [0; 1; 2]);
    ignore (get (save [1; 2] 3));
    check "released reservation did not allow progress" (rounds () = [1; 2; 3]))

let () =
  test_local_proof ();
  test_many_votes ();
  test_observer ();
  test_receipt_context ();
  test_pending_proofs ();
  Printf.printf "status = pass test = polc_cache\n%!"