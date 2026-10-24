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

let with_peer run =
  let fd, writer = Lwt_unix.pipe () in
  let peer = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:"proof-peer" ~addr:"proof-peer" ~direction:Octra_net.P2p_conn.Inbound in
  Fun.protect ~finally:(fun () -> Lwt_main.run (let open Lwt.Syntax in
    let* () = Octra_net.P2p_conn.close peer in Lwt_unix.close writer)) (fun () -> run peer)

let test_late_proof () =
  List.iter (fun epoch -> List.iter (fun late ->
    let members = set 4 in
    let local = C.leader_of members ~epoch_id:epoch ~round:1 in
    let directory = path () in
    let pending = ref 0 in
    let node = make directory members epoch local (ref true) (ref true) pending in
    E.start_height node.engine epoch;
    ignore (E.drain_outputs node.engine);
    let leader = C.leader_of members ~epoch_id:epoch ~round:0 in
    let value = header epoch leader.address in
    let pid = H.proposal_id value in
    let proposal = C.{chain_id = chain; epoch_id = epoch; round = 0; valid_round = None;
      header = value; tx_hashes = []; parent_commit = None; proposer = leader.address; signature = ""} in
    let proposal = {proposal with signature = sign leader.address (H.propose_sign_bytes proposal)} in
    E.on_propose node.engine proposal ~verify_fn:node.config.verify_fn
      ~execute_fn:node.config.execute_fn ~sign_fn:node.config.sign_fn;
    ignore (E.drain_outputs node.engine);
    let deliver member pid = E.on_vote node.engine (vote epoch 0 pid C.Prevote member)
      ~sign_fn:node.config.sign_fn in
    let remote = List.filter (fun member -> member.C.address <> local.address) members.validators in
    deliver (List.nth remote 0) pid;
    deliver (List.nth remote 2) Octra_net.Hash_domain.nil_hash;
    ignore (E.drain_outputs node.engine);
    if late then begin
      E.on_timeout node.engine ~step:C.PrevoteStep ~round:0
        ~generation:node.engine.generation ~sign_fn:node.config.sign_fn;
      let sent = E.drain_outputs node.engine |> List.filter_map (function
        | E.SendVote (v, _) -> Some v | _ -> None) in
      check "timeout must emit one nil precommit" (match sent with
        | [v] -> v.vote_type = C.Precommit && Octra_net.Hash_domain.is_nil v.proposal_id
        | _ -> false);
      ignore (get (V.keep node.vote_log (List.hd sent)))
    end;
    deliver (List.nth remote 1) pid;
    check "late quorum must retain proof" (E.polc_matches node.engine ~round:0 ~proposal_id:pid);
    check "late quorum must retain value" (node.engine.state.valid_round = 0
      && node.engine.state.valid_value = Some value);
    check "late proof must not create a lock" (node.engine.state.locked_round = if late then -1 else 0);
    let outputs = E.drain_outputs node.engine in
    check "late proof must not sign again" (not late || not (List.exists (function
      | E.SendVote _ | E.SendFinalize _ | E.Finalized _ -> true | _ -> false) outputs));
    get (D.keep_polc node 0 ~required:false);
    if epoch >= height then begin
      let restarted = make directory members epoch local (ref true) (ref true) (ref 0) in
      get (D.restore_polc restarted);
      check "restart lost late proof" (E.polc_matches restarted.engine ~round:0 ~proposal_id:pid);
      check "restart did not retain proposal for checking" (restarted.value_restore = Some proposal);
      check "unexecuted proposal promoted on restart" (restarted.engine.state.valid_value = None);
      E.start_round restarted.engine 1;
      restarted.running <- true;
      Lwt_main.run (D.finish_check restarted ~route:D.Publish_verified_proposal proposal
        (Some D.Proposal_accept));
      check "restart lost full value" (restarted.engine.state.valid_value = Some value);
      ignore (E.drain_outputs restarted.engine);
      E.do_propose restarted.engine {value with creator_addr = local.address}
        [] ~sign_fn:restarted.config.sign_fn;
      check "restarted leader did not reuse value" (List.exists (function
        | E.SendPropose p -> p.valid_round = Some 0 && p.header = value
        | _ -> false) (E.drain_outputs restarted.engine));
      Lwt_main.run (D.stop restarted)
    end;
    E.start_round node.engine 1;
    ignore (E.drain_outputs node.engine);
    E.do_propose node.engine {value with creator_addr = local.address; proposed_state_root = raw "other"}
      [] ~sign_fn:node.config.sign_fn;
    check "next leader lost proven value" (List.exists (function
      | E.SendPropose p -> p.valid_round = Some 0 && p.header = value
      | _ -> false) (E.drain_outputs node.engine));
    if late then check "stored nil vote was replaced" (Result.is_error
      (V.keep node.vote_log (vote epoch 0 pid C.Precommit local)))
  ) [false; true]) [Int64.pred height; height; 1_739_999L; 1_740_000L]

let test_proof_replay () = with_peer (fun peer ->
  let members = set 4 in
  let local = List.hd members.validators in
  let node = make (path ()) members height local (ref true) (ref false) (ref 0) in
  node.running <- true;
  E.start_height node.engine height;
  let value = header height (C.leader_of members ~epoch_id:height ~round:0).address in
  let pid = H.proposal_id value in
  let proof = List.map (vote height 0 pid C.Prevote) members.validators in
  let frame vote = Octra_net.P2p_frame.{msg_type = msg_cons_vote; payload = C_codec.encode_vote vote} in
  List.iter (fun v ->
    ignore (E.add_vote ~policy:node.policy node.engine.prevotes v ~validator_set:members);
    let wire = frame v in D.remember_frame node wire.msg_type wire.payload) proof;
  E.cache_proposal_header_only node.engine pid value;
  E.start_round node.engine 1;
  ignore (E.drain_outputs node.engine);
  let wire = frame (List.hd proof) in
  Lwt_main.run (D.on_p2p_message node peer wire);
  check "duplicate replay lost quorum proof" (E.polc_matches node.engine ~round:0 ~proposal_id:pid);
  check "duplicate replay changed lock" (node.engine.state.locked_round = -1);
  check "duplicate replay did not promote value" (node.engine.state.valid_round = 0);
  check "complete proof replay must remain deduped" (not (D.historical_vote_replay_needed node wire.payload));
  check "proof replay was not saved" (List.length (get (V.polc_load node.vote_log ~policy:node.policy
    ~chain_id:chain ~epoch_id:height ~validator_set:members)) >= members.quorum))

let test_proof_limits () =
  let selected = List.filteri (fun index _ -> index < 4) keys |> List.map fst in
  let members = get (C.make_weighted_validator_set (List.mapi (fun index member ->
    member, Z.of_int (if index = 0 then 100 else 1)) selected)) in
  let leader = C.leader_of members ~epoch_id:height ~round:0 in
  let value = header height leader.address in
  let pid = H.proposal_id value in
  let run indices =
    let node = E.create ~chain_id:chain ~my_addr:(List.hd selected).C.address
      ~validator_set:members ~start_height:height ~can_vote:(fun () -> false) in
    E.start_height node height;
    List.iter (fun index -> E.on_vote node (vote height 0 pid C.Prevote (List.nth selected index))
      ~sign_fn:(sign (List.hd selected).address)) indices;
    node in
  check "stake alone learned proof" (not (E.polc_matches (run [0]) ~round:0 ~proposal_id:pid));
  check "count alone learned proof" (not (E.polc_matches (run [1; 2; 3]) ~round:0 ~proposal_id:pid));
  let node = run [0; 1; 2] in
  check "proof before header was lost" (E.polc_matches node ~round:0 ~proposal_id:pid);
  check "missing header invented valid value" (node.state.valid_value = None);
  check "proof before proposal emitted vote" (not (List.exists (function
    | E.SendVote _ -> true | _ -> false) (E.drain_outputs node)));
  let proposal = C.{chain_id = chain; epoch_id = height; round = 0; valid_round = None;
    header = value; tx_hashes = []; parent_commit = None; proposer = leader.address; signature = ""} in
  let proposal = {proposal with signature = sign leader.address (H.propose_sign_bytes proposal)} in
  let verify address msg signature = match C.pubkey_of_addr members address with
    | None -> false | Some pubkey_raw -> H.verify_ed25519 ~pubkey_raw ~msg ~signature in
  E.on_propose node proposal ~verify_fn:verify ~execute_fn:(fun _ -> false)
    ~sign_fn:(sign node.my_addr);
  check "rejected header promoted value" (node.state.valid_value = None);
  E.on_propose node proposal ~verify_fn:verify ~execute_fn:(fun _ -> true)
    ~sign_fn:(sign node.my_addr);
  check "verified late header did not promote value" (node.state.valid_value = Some value);
  let node = run [] in
  let newer = {value with proposed_state_root = raw "newer"} in
  node.state <- {node.state with locked_round = 2; locked_value = Some newer;
    valid_round = 2; valid_value = Some newer};
  E.cache_proposal_message node proposal;
  E.start_round node 3;
  ignore (E.drain_outputs node);
  List.iter (fun member -> E.on_vote node (vote height 0 pid C.Prevote member)
    ~sign_fn:(sign node.my_addr)) selected;
  check "old proof replaced newer lock or value" (node.state.locked_round = 2
    && node.state.locked_value = Some newer && node.state.valid_round = 2
    && node.state.valid_value = Some newer);
  check "old proof emitted a vote" (not (List.exists (function
    | E.SendVote _ -> true | _ -> false) (E.drain_outputs node)))

let test_bad_proof () = with_peer (fun peer ->
  let members = set 4 in
  let node = make (path ()) members height (List.hd members.validators)
    (ref true) (ref false) (ref 0) in
  node.running <- true;
  E.start_height node.engine height;
  let value = header height (List.hd members.validators).address in
  let pid = H.proposal_id value in
  E.cache_proposal_header_only node.engine pid value;
  let send v = Lwt_main.run (D.on_p2p_message node peer Octra_net.P2p_frame.{
    msg_type = msg_cons_vote; payload = C_codec.encode_vote v}) in
  let votes = List.map (vote height 0 pid C.Prevote) members.validators in
  send (List.nth votes 0);
  send (List.nth votes 1);
  let third = List.nth votes 2 in
  send {third with signature = String.make 64 '\000'};
  check "invalid signature completed proof" (not (E.polc_matches node.engine ~round:0 ~proposal_id:pid));
  check "invalid signature polluted table" (node.engine.prevotes.count_total = 2);
  send third;
  check "valid signature did not complete proof" (E.polc_matches node.engine ~round:0 ~proposal_id:pid);
  let saved = node.engine.state in
  let conflict = vote height 0 (raw "conflict") C.Prevote (List.nth members.validators 2) in
  send conflict;
  check "conflicting duplicate changed proof" (node.engine.state = saved
    && not (E.polc_matches node.engine ~round:0 ~proposal_id:conflict.proposal_id)))

let test_search_clock () =
  let open C_proof_search in
  let next prior now = step prior ~height ~round:71 ~known:None ~enabled:true ~now in
  let first, request = next None 0L in
  check "search must begin at zero" (request = Some 0);
  let same, request = next first 1_000_000_000L in
  check "search exceeded request rate" (same = first && request = None);
  let second, request = next same interval in
  check "search did not advance once" (request = Some 1);
  let third, request = next second (Int64.mul 2L interval) in
  check "search missed old round" (request = Some 2);
  let pinned, request = step third ~height ~round:80 ~known:(Some 2) ~enabled:true
    ~now:(Int64.mul 3L interval) in
  check "known proof did not pin header query" (request = Some 2);
  let _, request = step pinned ~height:(Int64.succ height) ~round:80 ~known:None
    ~enabled:true ~now:0L in
  check "height change kept old cursor" (request = Some 0);
  let paused, request = step third ~height ~round:80 ~known:None ~enabled:false ~now:0L in
  check "inactive search sent a request" (request = None && paused = third);
  let _, request = step paused ~height ~round:81 ~known:None ~enabled:true
    ~now:(Int64.mul 3L interval) in
  check "leadership change reset search" (request = Some 3)

let test_search_policy () =
  let members = set 4 in
  List.iter (fun epoch ->
    let member = C.leader_of members ~epoch_id:epoch ~round:71 in
    let node = make (path ()) members epoch member (ref true) (ref true) (ref 0) in
    E.start_round node.engine 71;
    node.running <- true;
    let expected = if epoch >= height && epoch < 1_740_000L then Some 0 else None in
    check "proof search crossed its policy interval"
      (D.proof_search_step node ~now:0L = expected && node.engine.proof_search = expected);
    Lwt_main.run (D.stop node))
    [Int64.pred height; height; 1_739_999L; 1_740_000L]

let test_past_value () =
  let members = set 4 in
  let local = List.hd members.validators in
  let leader = C.leader_of members ~epoch_id:height ~round:2 in
  let value = header height leader.address in
  let proposal = C.{chain_id = chain; epoch_id = height; round = 2; valid_round = None;
    header = value; tx_hashes = []; parent_commit = None; proposer = leader.address; signature = ""} in
  let proposal = {proposal with signature = sign leader.address (H.propose_sign_bytes proposal)} in
  let pid = H.proposal_id value in
  let node = make (path ()) members height local (ref true) (ref true) (ref 0) in
  E.start_round node.engine 71;
  ignore (E.drain_outputs node.engine);
  let learn p accepted = E.learn_past_proposal node.engine p
    ~execute_fn:(fun _ -> accepted) in
  check "past proposal accepted without proof" (not (learn proposal true));
  node.engine.proof_search <- Some 2;
  List.iter (fun member -> E.on_vote node.engine (vote height 2 pid C.Prevote member)
    ~sign_fn:node.config.sign_fn) members.validators;
  check "requested old proof rejected by retention" (E.polc_matches node.engine ~round:2 ~proposal_id:pid);
  check "bad signature promoted past proposal" (not (learn {proposal with signature = String.make 64 '\000'} true));
  check "failed execution promoted past proposal" (not (learn proposal false));
  check "wrong transaction list promoted past proposal" (not (learn {proposal with tx_hashes = [raw "other"]} true));
  check "valid past proposal was not learned" (learn proposal true);
  check "learning changed lock or signed" (node.engine.state.locked_round = -1
    && not (List.exists (function E.SendVote _ -> true | _ -> false) (E.drain_outputs node.engine)));
  get (D.keep_polc node 2 ~required:false);
  check "proposal and proof not saved together" (Option.is_some (get (C_value_log.load node.vote_log
    ~policy:node.policy ~chain_id:chain ~epoch_id:height ~validator_set:members)));
  let rejected = {C_value_log.proposal; votes = [vote height 2 pid C.Prevote leader]} in
  check "short proof persisted" (Result.is_error (C_value_log.save node.vote_log
    ~policy:node.policy ~chain_id:chain ~epoch_id:height ~validator_set:members rejected));
  let fresh = make (path ()) members height local (ref true) (ref true) (ref 0) in
  get (V.value_write fresh.vote_log "[]");
  check "damaged record blocks startup" (Result.is_ok (D.restore_polc fresh)
    && fresh.value_restore = None && fresh.engine.state.locked_round = -1);
  get (V.prune fresh.vote_log ~through_epoch:height);
  check "damaged record survives prune" (get (V.value_read fresh.vote_log) = None);
  let stored = vote height 71 Octra_net.Hash_domain.nil_hash C.Precommit local in
  ignore (get (V.keep fresh.vote_log stored));
  List.iter (fun p ->
    let wire = `List [`String (Base64.encode_exn (C_codec.encode_propose p));
      `List (List.map (fun member -> `String (Base64.encode_exn
        (C_codec.encode_vote (vote height 2 pid C.Prevote member)))) members.validators)]
      |> Yojson.Safe.to_string in
    get (V.value_write fresh.vote_log wire);
    get (D.restore_polc fresh);
    check "unusable proposal changed signed vote" (fresh.value_restore = None
      && get (V.find_statement fresh.vote_log ~chain_id:chain ~validator:local.address
        ~epoch_id:height ~round:71 ~vote_type:C.Precommit
        ~proposal_id:Octra_net.Hash_domain.nil_hash) = Some stored))
    [{proposal with chain_id = "different-chain"};
     {proposal with epoch_id = Int64.succ height};
     {proposal with signature = String.make 64 '\000'}];
  List.iter (fun store ->
    let record = C_value_log.{proposal; votes = List.map (vote height 2 pid C.Prevote) members.validators} in
    get (C_value_log.save store ~policy:node.policy ~chain_id:chain ~epoch_id:height
      ~validator_set:members record);
    get (V.prune store ~through_epoch:(Int64.pred height));
    check "prune removed future proposal" (Option.is_some (get (V.value_read store)));
    get (V.prune store ~through_epoch:height);
    check "prune kept finalized proposal" (get (V.value_read store) = None))
    [V.memory (); V.disk ~data_dir:(path ())];
  let directory = path () in
  let store = V.disk ~data_dir:directory in
  ignore (get (V.keep store stored));
  Unix.mkdir (Filename.concat directory "vote_log/proposal") 0o700;
  let invalid = {node with D.vote_log = store; saved_polc = None; saved_value = None} in
  get (D.restore_polc invalid);
  get (V.prune store ~through_epoch:(Int64.pred height));
  check "optional directory removed signed vote"
    (get (V.find_statement store ~chain_id:chain ~validator:local.address
      ~epoch_id:height ~round:71 ~vote_type:C.Precommit
      ~proposal_id:Octra_net.Hash_domain.nil_hash) = Some stored);
  get (D.keep_polc invalid 2 ~required:false);
  check "optional write blocked mandatory proof" (invalid.vote_log_issue = None
    && List.length (get (V.polc_load store ~chain_id:chain ~epoch_id:height
      ~validator_set:members)) = members.n);
  node.engine.state <- {node.engine.state with locked_round = 3; locked_value = Some value;
    valid_round = 3; valid_value = Some value};
  check "old proposal replaced newer lock" (not (learn proposal true))

let test_search_limits () =
  let search = ref None and requests = ref [] in
  for tenth = 0 to 1_799 do
    let now = Int64.mul (Int64.of_int tenth) 100_000_000L in
    let state, request = C_proof_search.step !search ~height ~round:83
      ~known:(Some 2) ~enabled:true ~now in
    search := state;
    Option.iter (fun target -> requests := (now, target) :: !requests) request
  done;
  check "proof target exceeded four requests" (List.length !requests = 4);
  check "proof retries lost exponential delay"
    (List.rev_map fst !requests = [0L; 2_100_000_000L; 6_300_000_000L; 14_700_000_000L]);
  let _, request = C_proof_search.step !search ~height ~round:84 ~known:(Some 2)
    ~enabled:true ~now:180_000_000_000L in
  check "next round did not retry target" (request = Some 2);
  let cursor = ref None in
  let targets = ref [] in
  for index = 0 to 90 do
    let state, request = C_proof_search.step !cursor ~height ~round:3 ~known:None
      ~enabled:true ~now:(Int64.mul (Int64.of_int index) C_proof_search.interval) in
    cursor := state;
    Option.iter (fun target -> targets := target :: !targets) request
  done;
  check "unknown search exceeded round cap" (List.rev !targets = [0; 1; 2; 0; 1; 2; 0; 1])

let test_search_round_churn () =
  let state = ref None in
  let queries = ref [] in
  for second = 0 to 179 do
    let next, query = C_proof_search.step ~validators:200 !state ~height
      ~round:(83 + second) ~known:None ~enabled:true
      ~now:(Int64.mul (Int64.of_int second) 1_000_000_000L) in
    state := next;
    Option.iter (fun _ -> queries := second :: !queries) query
  done;
  check "round change reset response credits" (List.rev !queries = [0; 60; 120])

let test_wait_priority () =
  let members = set 4 in
  let local = C.leader_of members ~epoch_id:height ~round:122 in
  let now = ref 0L in
  let node = make (path ()) members height local (ref true) (ref false) (ref 0) in
  let node = {node with D.proof_clock = (fun () -> !now)} in
  E.start_round node.engine 122;
  node.running <- true;
  let proposal round =
    let author = C.leader_of members ~epoch_id:height ~round in
    let p = C.{chain_id = chain; epoch_id = height; round; valid_round = None;
      header = header height author.address; tx_hashes = []; parent_commit = None;
      proposer = author.address; signature = ""} in
    {p with signature = sign author.address (H.propose_sign_bytes p)} in
  let past = proposal 2 and current = proposal 122 in
  let pid = H.proposal_id past.header in
  let early = C_codec.encode_propose past in
  D.remember_frame node Octra_net.P2p_frame.msg_cons_propose early;
  check "unproven historical replay started work"
    (not (D.historical_replay_needed node Octra_net.P2p_frame.msg_cons_propose early));
  E.record_polc node.engine 2 pid;
  List.iter (fun member -> E.record_historical_prevote node.engine
    (vote height 2 pid C.Prevote member)) members.validators;
  Lwt_main.run (D.finish_check node ~route:D.Publish_verified_proposal past (Some D.Proposal_wait));
  D.retain_proposal_wait ~initial:true node ~route:D.Publish_verified_proposal current;
  check "historical wait displaced current proposal" (match D.next_proposal_work node with
    | Some (D.Check_proposal (_, p, _)) -> p = current
    | _ -> false);
  D.clear_proposal_wait node current;
  check "current clear discarded historical wait" (Option.is_some node.past_wait);
  Lwt_main.run (D.finish_check node ~route:D.Publish_verified_proposal past (Some D.Proposal_wait));
  E.start_round node.engine 123;
  now := 500_000_000L;
  check "historical wait lost across round" (match D.next_proposal_work node with
    | Some (D.Check_proposal (_, p, _)) -> p = past
    | _ -> false);
  let payload = C_codec.encode_propose past in
  let search, _ = C_proof_search.step None ~height ~round:123 ~known:(Some 2)
    ~enabled:true ~now:0L in
  node.proof_search <- search;
  check "first historical replay refused"
    (D.historical_replay_needed node Octra_net.P2p_frame.msg_cons_propose payload);
  check "same attempt repeated historical verification"
    (not (D.historical_replay_needed node Octra_net.P2p_frame.msg_cons_propose payload));
  Lwt_main.run (D.finish_check node ~route:D.Publish_verified_proposal past (Some D.Proposal_reject));
  let search, _ = C_proof_search.step search ~height ~round:123 ~known:(Some 2)
    ~enabled:true ~now:C_proof_search.interval in
  node.proof_search <- search;
  check "new attempt cannot repeat rejected proposal"
    (D.historical_replay_needed node Octra_net.P2p_frame.msg_cons_propose payload);
  Lwt_main.run (D.finish_check node ~route:D.Publish_verified_proposal past (Some D.Proposal_accept));
  check "historical wait failed after round change" (node.engine.state.valid_round = 2);
  Lwt_main.run (D.stop node)

let test_wait_realign () =
  let members = set 4 in
  let author = C.leader_of members ~epoch_id:height ~round:83 in
  let node = make (path ()) members height author (ref true) (ref false) (ref 0) in
  let node = {node with D.proof_clock = (fun () -> 1_000_000_000L)} in
  E.start_round node.engine 83;
  let proposal = C.{chain_id = chain; epoch_id = height; round = 83; valid_round = None;
    header = header height author.address; tx_hashes = []; parent_commit = None;
    proposer = author.address; signature = ""} in
  let proposal = {proposal with signature = sign author.address (H.propose_sign_bytes proposal)} in
  let pid = H.proposal_id proposal.header in
  E.record_polc node.engine 83 pid;
  List.iter (fun member -> E.record_historical_prevote node.engine
    (vote height 83 pid C.Prevote member)) members.validators;
  D.retain_proposal_wait ~initial:true node ~route:D.Publish_verified_proposal proposal;
  Lwt_main.run (D.realign_progress_at node ~now:1_000_000_000L ~height ~round:83);
  check "realign discarded proven wait" (node.engine.state.round = 84
    && Option.fold ~none:false ~some:(fun wait -> wait.D.proposal = proposal) node.past_wait);
  Lwt_main.run (D.stop node)

let test_wake_error () =
  let members = set 4 in
  let member = C.leader_of members ~epoch_id:height ~round:71 in
  let node = make (path ()) members height member (ref true) (ref true) (ref 0) in
  E.start_round node.engine 71;
  node.running <- true;
  let calls = ref 0 in
  let wake = D.proof_wake_loop node ~sleep:(fun _ -> Lwt.pause ()) ~drain:(fun () ->
    incr calls;
    if !calls = 1 then Lwt.fail_with "injected drain failure"
    else (node.running <- false; Lwt.return_unit)) in
  Lwt_main.run wake;
  check "wake failure disabled voting" (!calls = 2 && node.vote_log_issue = None);
  node.running <- true;
  let sleep, _ = Lwt.task () in
  let wake = D.proof_wake_loop node ~sleep:(fun _ -> sleep)
    ~drain:(fun () -> incr calls; Lwt.return_unit) in
  node.proof_wake <- Some wake;
  Lwt_main.run (D.stop node);
  Lwt_main.run wake;
  check "stopped wake ran work" (!calls = 2 && node.proof_wake = None);
  node.running <- true;
  let drain, finish = Lwt.task () in
  let wake = D.proof_wake_loop node ~sleep:(fun _ -> Lwt.pause ())
    ~drain:(fun () -> incr calls; drain) in
  node.proof_wake <- Some wake;
  Lwt_main.run (Lwt.pause ());
  check "wake did not enter drain" (!calls = 3);
  Lwt_main.run (D.stop node);
  Lwt_main.run wake;
  check "wake canceled shared drain" (Lwt.is_sleeping drain && node.vote_log_issue = None);
  Lwt.wakeup finish ();
  check "stopped wake restarted drain" (!calls = 3)

let test_search_frames count known =
  let module F = Octra_net.P2p_frame in
  let module B = Octra_net.P2p_frame_budget in
  let module Conn = Octra_net.P2p_conn in
  let members = set count in
  let donor = List.hd members.validators and requester = List.nth members.validators 1 in
  let now = ref 0L in
  let node = make (path ()) members height donor (ref true) (ref true) (ref 0) in
  let node = {node with D.proof_clock = (fun () -> !now)} in
  E.start_round node.engine 83;
  ignore (E.drain_outputs node.engine);
  node.running <- true;
  let pid = raw "frame-proof" in
  if known then E.record_polc node.engine 2 pid;
  List.iter (fun member ->
    let old = vote height 2 pid C.Prevote member in
    if known then E.record_historical_prevote node.engine old;
    if member = donor then ignore (get (V.keep node.vote_log old));
    List.iter (fun (kind, table) ->
      ignore (E.add_vote ~policy:node.policy table (vote height 83 pid kind member)
        ~validator_set:members)) [C.Prevote, node.engine.prevotes; C.Precommit, node.engine.precommits])
    members.validators;
  let frames = ref [] in
  let budget = B.create ~now:0. ~peer_class:B.Validator in
  let capture peer_id =
    let fd, writer = Lwt_unix.pipe () in
    let conn = Conn.create ~peer_class:B.Validator fd ~peer_id ~addr:"response-test"
      ~direction:Conn.Inbound in
    let count = ref 0 in
    let rec consume () =
      let open Lwt.Syntax in
      let* frame = Lwt_mvar.take conn.write_queue in
      conn.pending_writes <- max 0 (conn.pending_writes - 1);
      incr count;
      frames := frame :: !frames;
      check "proof replies exceeded frame budget" (B.admit budget
        ~now:(Int64.to_float !now /. 1_000_000_000.) ~msg_type:frame.F.msg_type
        ~bytes:(String.length frame.payload) = B.Accept);
      consume () in
    conn, writer, count, consume () in
  let own_id = Octra_net.P2p_handshake.node_id_of_pubkey requester.pubkey in
  let proper, writer, delivered, consumer = capture own_id in
  let copied, other_writer, stolen, other_consumer = capture "other-authenticated-peer" in
  Fun.protect ~finally:(fun () ->
    List.iter Lwt.cancel [consumer; other_consumer];
    Lwt_main.run (Conn.close proper);
    Lwt_main.run (Conn.close copied);
    Lwt_main.run (Lwt_unix.close writer);
    Lwt_main.run (Lwt_unix.close other_writer);
    Lwt_main.run (D.stop node)) (fun () ->
    let request = C_codec.{chain_id = chain; epoch_id = height; round = 2;
      step = C.ProposeStep; request = true; validator = requester.address; signature = ""} in
    let request = {request with signature = sign requester.address (H.round_sync_sign_bytes request)} in
    let frame = F.{msg_type = msg_cons_round_sync; payload = C_codec.encode_round_sync request} in
    Lwt_main.run (D.on_p2p_message node copied frame);
    check "copied request consumed signer slot" (!stolen = 0
      && not (Hashtbl.mem node.round_sync_replies requester.address));
    Lwt_main.run (D.on_p2p_message node proper frame);
    check "real signer received no proof" (!delivered >= 30);
    check "proven response included current votes" (not known || List.for_all (fun frame ->
      frame.F.msg_type <> F.msg_cons_vote || (C_codec.decode_vote frame.payload).round = 2) !frames);
    let first = !delivered in
    Lwt_main.run (D.on_p2p_message node copied frame);
    Lwt_main.run (D.on_p2p_message node proper frame);
    check "duplicate request bypassed reply window" (!stolen = 0 && !delivered = first);
    let search = ref None and sent = ref 0 in
    for tenth = 0 to 1_799 do
      now := Int64.mul (Int64.of_int tenth) 100_000_000L;
      if tenth mod 600 = 0 then
        for _ = 1 to 1_000 do
          check "competing votes exhausted budget" (B.admit budget
            ~now:(Int64.to_float !now /. 1_000_000_000.) ~msg_type:F.msg_cons_vote ~bytes:256 = B.Accept)
        done;
      let state, query = C_proof_search.step ~validators:count !search ~height ~round:83
        ~known:(if known then Some 2 else None) ~enabled:true ~now:!now in
      search := state;
      Option.iter (fun _ -> incr sent; Lwt_main.run (D.on_p2p_message node proper frame)) query
    done;
    check "incomplete value queried forever" (!sent = if known then 4 else 3);
    check "historical response fanout grew" (!delivered <= if known then 5 * 34 else 3 * (2 * count + 4));
    now := 180_000_000_000L;
    frames := [];
    Lwt_main.run (D.send_round_sync_response node proper ~requested_round:1 ~with_witness:true);
    check "missing proof hid current progress" (List.exists (fun frame ->
      frame.F.msg_type = F.msg_cons_round_sync && (C_codec.decode_round_sync frame.payload).round = 83) !frames);
    Hashtbl.remove node.engine.prevotes_by_round 2;
    Hashtbl.remove node.engine.source_prevotes 2;
    frames := [];
    now := 240_000_000_000L;
    Lwt_main.run (D.send_round_sync_response node proper ~requested_round:2 ~with_witness:true);
    check "empty proof marker hid current votes" (List.exists (fun frame ->
      frame.F.msg_type = F.msg_cons_vote && (C_codec.decode_vote frame.payload).round = 83) !frames))

let test_value_source () =
  let source = set 4 in
  let author = C.leader_of source ~epoch_id:height ~round:2 in
  let voters = List.filter (fun member -> member.C.address <> author.address) source.validators in
  let selected = get (C.make_weighted_validator_set (List.map (fun member -> member, Z.one) voters)) in
  let selected = {selected with C.authors = Some (height, [author, Z.one])} in
  let value = header height author.address in
  let proposal = C.{chain_id = chain; epoch_id = height; round = 2; valid_round = None;
    header = value; tx_hashes = []; parent_commit = None; proposer = author.address; signature = ""} in
  let proposal = {proposal with signature = sign author.address (H.propose_sign_bytes proposal)} in
  let record = C_value_log.{proposal; votes = List.map
    (vote height 2 (H.proposal_id value) C.Prevote) source.validators} in
  let node = make (path ()) selected height (List.hd voters) (ref true) (ref true) (ref 0) in
  get (C_value_log.save node.vote_log ~policy:node.policy ~chain_id:chain ~epoch_id:height
    ~validator_set:source record);
  get (D.restore_polc node);
  E.start_round node.engine 71;
  node.running <- true;
  Lwt_main.run (D.finish_check node ~route:D.Publish_verified_proposal proposal (Some D.Proposal_accept));
  check "source proposal lost after set change" (node.engine.state.valid_value = Some value);
  check "source proof changed local lock" (node.engine.state.locked_round = -1);
  let unknown = make (path ()) selected height (List.hd voters) (ref true) (ref true) (ref 0) in
  E.start_round unknown.engine 71;
  check "source author accepted without proof" (not (D.check_proposal_frame unknown proposal));
  Lwt_main.run (D.stop node)

let test_driver_recovery () =
  let recovered = ref [] in
  let weights = [1; 1; 1; 500; 1; 1; 1; 1; 1; 1; 2; 1; 1; 1; 1; 1;
    2; 10_000; 1; 1; 1; 1; 1; 2; 1; 1; 1; 701; 13; 1; 1] in
  let selected = List.mapi (fun index weight -> fst (List.nth keys index),
    Z.mul (Z.of_int weight) (Z.of_int 1_000_000)) weights in
  let members = get (C.make_weighted_validator_set selected) in
  let epoch = 1_737_349L in
  let heavy = fst (List.nth selected 17) in
  let locked = selected |> List.filter (fun (_, weight) -> Z.leq weight (Z.of_int 2_000_000))
    |> List.filteri (fun index _ -> index < 17) |> List.map fst in
  let donor = List.hd locked in
  let txs = List.init 15 (fun index ->
    let tx = Octra_core.Transaction.{from = donor.address; to_ = heavy.address;
      amount = Z.one; nonce = index + 1; ou = Z.of_int 1_000; timestamp = 1.;
      signature = ""; public_key = Some (Base64.encode_exn donor.pubkey);
      message = None; op_type = Standard; encrypted_data = None} in
    {tx with signature = Base64.encode_exn (sign donor.address
      (Octra_core.Transaction.serialize_for_signing tx))}) in
  let bodies = List.map (fun tx -> Octra_core.Transaction.to_yojson tx |> Yojson.Safe.to_string) txs in
  let hashes = List.map (fun tx -> Octra_node_runtime.Consensus_pending_commit_recovery.raw32_of_hex
    (Octra_core.Transaction.hash tx) |> Option.get) txs in
  let prior = header (Int64.pred epoch)
    (C.leader_of members ~epoch_id:(Int64.pred epoch) ~round:0).address in
  let prior_id = H.proposal_id prior in
  let parent = C.{certificate = {chain_id = chain; epoch_id = Int64.pred epoch;
    commit_round = 0; header = prior; proposal_id = prior_id;
    precommits = List.map (vote (Int64.pred epoch) 0 prior_id C.Precommit) members.validators};
    validator_set = members} in
  let author = C.leader_of members ~epoch_id:epoch ~round:2 in
  let value = { (header epoch author.address) with tx_list_hash = E.tx_list_hash_for_header hashes;
    parent_commit_hash = H.parent_commit_hash_opt (Some parent); txid_hi = 15L } in
  let pid = H.proposal_id value in
  let proposal = C.{chain_id = chain; epoch_id = epoch; round = 2; valid_round = None;
    header = value; tx_hashes = hashes; parent_commit = Some parent;
    proposer = author.address; signature = ""} in
  let proposal = {proposal with signature = sign author.address (H.propose_sign_bytes proposal)} in
  let proof = List.map (vote epoch 2 pid C.Prevote) members.validators in
  List.iter (fun (minimum, restart, old_reply) ->
    let rec leader_round round =
      if (C.leader_of members ~epoch_id:epoch ~round).address = heavy.address then round
      else leader_round (round + 1) in
    let round = leader_round minimum in
    let now = ref 0L in
    let checks = ref 0 in
    let directory = path () in
    let receiver_ref = ref None in
    let build () =
      let node = make directory members epoch heavy (ref true) (ref true) (ref 0) in
      {node with D.proof_clock = (fun () -> !now); config = {node.config with
        verify_proposal = (fun p ->
          incr checks;
          let open Lwt.Syntax in
          let* bundle = D.query_bundle ~now:(fun () -> !now) ~sleep:(fun _ -> Lwt.pause ())
            (Option.get !receiver_ref) ~epoch_id:epoch ~proposal_id:pid ~timeout_seconds:10.
            ~validate:(fun (reply : D.bundle_response_record) ->
              reply.tx_hashes = hashes && reply.txs_json = bodies && reply.receipts_json = []) in
          Lwt.return (if Option.is_some bundle && p.header = value
            && p.tx_hashes = hashes && p.parent_commit = Some parent
            then D.Proposal_accept else D.Proposal_reject))}} in
    let receiver = build () in
    ignore (get (V.keep receiver.vote_log
      (vote epoch (round - 1) Octra_net.Hash_domain.nil_hash C.Precommit heavy)));
    let receiver = if restart then build () else receiver in
    get (D.restore_polc receiver);
    D.resume_local_vote receiver;
    if not restart then begin
      List.iter (fun step ->
        E.on_timeout receiver.engine ~step ~round ~generation:receiver.engine.generation
          ~sign_fn:receiver.config.sign_fn;
        E.drain_outputs receiver.engine |> List.iter (function
          | E.SendVote (vote, _) -> ignore (get (V.keep receiver.vote_log vote))
          | _ -> ())) [C.ProposeStep; C.PrevoteStep];
      List.iter (fun vote -> D.remember_frame receiver Octra_net.P2p_frame.msg_cons_vote
        (C_codec.encode_vote vote)) proof;
      D.remember_frame receiver Octra_net.P2p_frame.msg_cons_propose (C_codec.encode_propose proposal)
    end;
    check "restart round differs" (receiver.engine.state.round = round);
    receiver.running <- true;
    receiver_ref := Some receiver;
    ignore (E.drain_outputs receiver.engine);
    let directory = path () in
    let store = V.disk ~data_dir:directory in
    ignore (get (V.polc_keep store ~chain_id:chain ~epoch_id:epoch
      ~validator_set:members ~locked_round:2 proof));
    let precommit = vote epoch 2 pid C.Precommit donor in
    ignore (get (V.keep store precommit));
    let hex = Octra_node_runtime.Text.raw_to_hex in
    Octra_core.Wal.write_pending_commit directory Octra_core.Wal.{
      epoch_id = Int64.to_int epoch; round = 2; proposal_id = hex pid;
      proposed_state_root = hex value.proposed_state_root; txid_hi = value.txid_hi;
      ts = 1.; validator_addr = donor.address;
      proposal_b64 = Some (Base64.encode_exn (C_codec.encode_propose proposal));
      vote_b64 = Some (Base64.encode_exn (C_codec.encode_vote precommit));
      tx_hashes = List.map hex hashes; txs_json = bodies; receipts_json = []};
    let cache = ref None in
    let sender = make directory members epoch donor (ref true) (ref false) (ref 0) in
    let sender = {sender with D.proof_clock = (fun () -> !now); config = {sender.config with
      lookup_bundle = (fun requested -> if requested = pid then !cache else None)}} in
    let module R = Octra_node_runtime.Consensus_pending_commit_recovery in
    let saved_head = Octra_core.Head_manifest.get_cached () in
    let head = Octra_core.Head_manifest.{schema_version; generation = 1;
      epoch_id = Int64.to_int (Int64.pred epoch); state_root = hex value.prev_state_root;
      ledger_state_root = None; irmin_commit = None; txid_hi = 0L;
      txlog_seg = None; txlog_off = None; epochlog_off = None; commit_id = "proof-restart";
      ts = 0.; quorum_cert_hash = None; epoch_index_hash = None; epoch_index_root = None} in
    Octra_core.Head_manifest.set_cached head;
    let runtime = R.node_driver_runtime R.{data_dir = directory; query_timeout = 0.;
      run_catchup_to_target = (fun _ ~target_epoch:_ ~reason:_ -> Lwt.fail_with "unexpected catchup");
      chain_id = chain; select_set = (fun ~epoch:_ ~round:_ -> Ok members);
      store_bundle = (fun ~proposal_id ~tx_hashes ~txs ~receipts_json ->
        check "pending bundle differs" (proposal_id = pid && tx_hashes = List.map hex hashes);
        cache := Some (hashes, List.map (fun tx -> Octra_core.Transaction.to_yojson tx
          |> Yojson.Safe.to_string) txs, receipts_json));
      validator_count = members.n; quorum = members.quorum} in
    Fun.protect ~finally:(fun () -> Octra_core.Head_manifest.cached := saved_head) (fun () ->
      check "pending did not restore" (Lwt_main.run (R.run_with_driver runtime sender)));
    get (Lwt_main.run (D.restore_relief sender));
    Lwt_main.run (D.start ~sleep:(fun seconds -> if seconds = 1.1
      then fst (Lwt.task ()) else Lwt.return_unit) sender);
    check "restart lost locked proposal" (sender.engine.state.locked_round = 2
      && List.length (E.polc_votes_for_round sender.engine 2) = List.length proof
      && Option.is_some !cache);
    E.start_round sender.engine round;
    ignore (E.drain_outputs sender.engine);
    sender.running <- true;
    let left, right = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn fd member = Octra_net.P2p_conn.create
      ~peer_class:Octra_net.P2p_frame_budget.Validator fd
      ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey member.C.pubkey)
      ~addr:"proof-test" ~direction:Octra_net.P2p_conn.Inbound in
    let outgoing = conn left donor and incoming = conn right heavy in
    Hashtbl.add receiver.swarm.peers outgoing.peer_id outgoing;
    Hashtbl.add sender.swarm.peers incoming.peer_id incoming;
    let jobs = [Octra_net.P2p_conn.write_loop outgoing; Octra_net.P2p_conn.write_loop incoming;
      Octra_net.P2p_conn.read_loop outgoing ~on_message:(fun _ frame ->
        if old_reply && frame.Octra_net.P2p_frame.msg_type = Octra_net.P2p_frame.msg_cons_propose
        then Lwt.return_unit
        else D.on_p2p_message (Option.get !receiver_ref) outgoing frame);
      Octra_net.P2p_conn.read_loop incoming ~on_message:(fun _ frame ->
        D.on_p2p_message sender incoming frame)] in
    let rec settle count = if count = 0 then Lwt.return_unit else
      Lwt.bind (Lwt.pause ()) (fun () -> settle (count - 1)) in
    Fun.protect ~finally:(fun () ->
      Lwt_main.run (D.stop (Option.get !receiver_ref));
      Lwt_main.run (D.stop sender);
      Lwt_main.run (Octra_net.P2p_conn.close outgoing);
      Lwt_main.run (Octra_net.P2p_conn.close incoming);
      List.iter Lwt.cancel jobs) (fun () ->
      List.iter (fun target ->
        now := Int64.mul (Int64.of_int target) C_proof_search.interval;
        Lwt_main.run (D.process_outputs receiver);
        Lwt_main.run (settle 1000)) [0; 1; 2];
      if old_reply then begin
        for second = 7 to 180 do
          now := Int64.mul (Int64.of_int second) 1_000_000_000L;
          Lwt_main.run (D.process_outputs receiver);
          Lwt_main.run (settle 100)
        done;
        check "missing old proposal falsely recovered" (receiver.engine.state.valid_value = None
          && receiver.engine.state.locked_round = -1 && !checks = 0);
        check "old responders did not pin search" (Option.fold ~none:false
          ~some:(fun query -> query.C_proof_search.target = 2 && query.attempts = 4)
          receiver.proof_search);
        Printf.printf "status = pass test = missing_old_value round = %d\n%!" round
      end else begin
      check "driver failed to recover old full proposal"
        (receiver.engine.state.valid_value = Some value && receiver.engine.state.valid_round = 2
         && E.find_tx_hashes receiver.engine pid = Some hashes && !checks > 0);
      check "recovery signed a precommit" (receiver.engine.state.locked_round = -1);
      if not restart then check "old nil precommit was replaced"
        (get (V.find_statement receiver.vote_log ~chain_id:chain ~validator:heavy.address
          ~epoch_id:epoch ~round ~vote_type:C.Precommit ~proposal_id:Octra_net.Hash_domain.nil_hash)
          = Some (vote epoch round Octra_net.Hash_domain.nil_hash C.Precommit heavy));
      let restarted = build () in
      get (D.restore_polc restarted);
      D.resume_local_vote restarted;
      let round = leader_round restarted.engine.state.round in
      E.start_round restarted.engine round;
      restarted.running <- true;
      Hashtbl.add restarted.swarm.peers outgoing.peer_id outgoing;
      receiver_ref := Some restarted;
      Lwt_main.run (D.stop receiver);
      Lwt_main.run (D.process_outputs restarted);
      Lwt_main.run (settle 1000);
      check "second restart lost executed value" (restarted.engine.state.valid_value = Some value);
      let others = members.validators |> List.filter (fun member -> member.C.address <> heavy.address)
        |> List.map (fun member ->
          let node = make (path ()) members epoch member (ref true) (ref true) (ref 0) in
          if List.mem member locked then get (D.restore_precommit_lock node proposal);
          E.start_round node.engine round;
          ignore (E.drain_outputs node.engine);
          node) in
      ignore (E.drain_outputs restarted.engine);
      E.do_propose restarted.engine (header epoch heavy.address) [] ~sign_fn:restarted.config.sign_fn;
      let proposed = E.drain_outputs restarted.engine in
      let repeated = proposed |> List.find_map (function E.SendPropose p -> Some p | _ -> None)
        |> Option.get in
      check "recovered leader changed value" (repeated.valid_round = Some 2
        && repeated.header = value && repeated.tx_hashes = hashes && repeated.parent_commit = Some parent);
      let votes outputs = List.filter_map (function E.SendVote (vote, _) -> Some vote | _ -> None) outputs in
      let prevotes = ref (votes proposed) in
      List.iter (fun node ->
        E.on_propose node.D.engine repeated ~verify_fn:node.config.verify_fn
          ~execute_fn:(fun p -> p.header = value) ~sign_fn:node.config.sign_fn;
        prevotes := votes (E.drain_outputs node.engine) @ !prevotes) others;
      let nodes = restarted :: others in
      check "locked members refused recovered proposal"
        (List.length !prevotes = 31 && List.for_all (fun (vote : C.vote) -> vote.proposal_id = pid) !prevotes);
      let precommits = ref [] in
      List.iter (fun node ->
        List.iter (fun v -> E.on_vote node.D.engine v ~sign_fn:node.config.sign_fn) !prevotes;
        precommits := votes (E.drain_outputs node.engine) @ !precommits) nodes;
      check "recovered proposal did not reach precommit quorum" (List.length !precommits = 31);
      List.iter (fun node ->
        List.iter (fun v -> E.on_vote node.D.engine v ~sign_fn:node.config.sign_fn) !precommits;
        check "31-member execution did not finalize original value"
          (Option.fold ~none:false ~some:(fun f -> f.C.proposal_id = pid) node.engine.pending_finalized)) nodes;
      recovered := (members, locked, proposal, repeated) :: !recovered;
      Printf.printf "status = pass test = driver_value round = %d restart = %b validators = 31 locked = 17\n%!"
        round restart end))
    [83, true, true; 56, true, false; 67, true, false; 71, true, false;
      83, true, false; 122, true, false; 56, false, false; 67, false, false;
      71, false, false; 83, false, false];
  !recovered

let () =
  test_wait_priority ();
  test_wait_realign ();
  test_wake_error ();
  test_search_limits ();
  test_search_round_churn ();
  test_search_frames 31 true;
  test_search_frames 200 false;
  test_search_policy ();
  let recovered = test_driver_recovery () in
  (match Sys.getenv_opt "OCTRA_VALUE_TRACE" with
   | None -> ()
   | Some file ->
     if not (String.starts_with ~prefix:"runtime_data/" file)
       || List.mem ".." (String.split_on_char '/' file) then invalid_arg "trace path";
     Yojson.Safe.to_file file (`List (List.map (fun (members, locked, prior, proposal) ->
       `List [`String (Base64.encode_exn (C_codec.encode_validator_set members));
         `List (List.map (fun member -> `String member.C.address) locked);
         `String (Base64.encode_exn (C_codec.encode_propose prior));
         `String (Base64.encode_exn (C_codec.encode_propose proposal))]) recovered)));
  test_value_source ();
  test_search_clock ();
  test_past_value ();
  test_late_proof ();
  test_proof_replay ();
  test_proof_limits ();
  test_bad_proof ();
  test_local_proof ();
  test_many_votes ();
  test_observer ();
  test_receipt_context ();
  test_pending_proofs ();
  Printf.printf "status = pass test = polc_cache\n%!"