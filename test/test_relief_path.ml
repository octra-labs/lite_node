(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_consensus.C_types
module H = Octra_consensus.C_hash
module E = Octra_consensus.C_engine
module R = Octra_consensus.C_relief
module A = Octra_bootstrap.Sync_anchor
module I = Octra_core.Store_irmin
module U = Octra_core.Validator_set_update
module J = Octra_node_runtime.Consensus_finality_journal

let get = function Ok value -> value | Error reason -> failwith reason
let check name yes = if not yes then failwith name
let chain = "octra-devnet-9871-cluster"
let height = Int64.add 2L (Option.get (Octra_consensus.C_quorum_policy.linear_epoch ~chain_id:chain))
let raw s = Digestif.SHA256.(digest_string s |> to_raw_string)
let hex s = Digestif.SHA256.(digest_string s |> to_hex)
let keys = List.init 7 (fun n ->
  let secret = String.make 32 (Char.chr (40 + n)) in
  let key = Mirage_crypto_ec.Ed25519.priv_of_octets secret |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, secret)
let source = get (C.make_weighted_validator_set
  (List.map (fun (member, _) -> member, Z.one) keys))
let trusted = get (A.encoded_validator_set source)
let creator = (C.leader_of source ~epoch_id:height ~round:0).address
let target = get (C.make_weighted_validator_set
  (C.weights_of_set source |> List.filter (fun (v, _) -> v.C.address <> creator)))
let sign addr bytes =
  let _, secret = List.find (fun (v, _) -> v.C.address = addr) keys in
  H.sign_ed25519 ~priv_raw:secret ~msg:bytes
let verify addr msg signature = match C.pubkey_of_addr source addr with
  | None -> false | Some pubkey -> H.verify_ed25519 ~pubkey_raw:pubkey ~msg ~signature
let plan = get (U.make_weighted ~source_epoch:(Int64.pred height)
  ~activate_epoch:(Int64.add height 8L)
  (C.weights_of_set target |> List.map (fun (v, weight) ->
    Octra_core.Validator_entry.{address = v.C.address; pubkey = v.pubkey; weight})))
let proof = List.map (fun (v, _) ->
  let sync = Octra_consensus.C_codec.{chain_id = chain; epoch_id = height;
    round = 64; step = C.PrecommitStep; request = false;
    validator = v.C.address; signature = String.make 64 '\000'} in
  {sync with signature = sign v.address (H.round_sync_sign_bytes sync)}) keys
let mark = match R.decide ~chain_id:chain ~height ~current:source
  ~activate_epoch:plan.activate_epoch ~target ~fingerprint:plan.fingerprint ~proof with
  | R.Apply mark -> mark | R.Refuse reason -> failwith reason | R.Wait -> failwith "relief wait"
let selected = get (R.restore ~chain_id:chain ~height ~current:source
  ~activate_epoch:plan.activate_epoch ~target ~fingerprint:plan.fingerprint mark) |> Option.get
let header epoch root = C.{proto_version = C.proto_version_current; chain_id = chain;
  epoch_id = epoch; prev_state_root = raw "prior"; tx_list_hash = E.tx_list_hash_for_header [];
  receipt_root = H.receipt_root []; proposed_state_root = root;
  parent_commit_hash = Octra_net.Hash_domain.nil_hash; creator_addr = creator;
  txid_hi = 0L; ts = Int64.to_float epoch}
let finalize set round header =
  let proposal_id = H.proposal_id header in
  let precommits = List.map (fun v ->
    let vote = C.{chain_id = chain; epoch_id = header.C.epoch_id; round;
      vote_type = Precommit; proposal_id; validator = v.C.address;
      signature = String.make 64 '\000'} in
    {vote with signature = sign v.address (H.vote_sign_bytes vote)}) set.C.validators in
  C.{chain_id = chain; epoch_id = header.epoch_id; commit_round = round;
    header; proposal_id; precommits; parent_commit = None}
let drain node = E.drain_outputs node

let certificate steps final (proof : I.merkle_proof) index set =
  let module P = Octra_bootstrap.State_sync_checkpoint in
  let module M = Octra_bootstrap.State_sync_manifest in
  let checkpoint = P.{chain_id = chain; epoch = final.C.epoch_id;
    state_root = P.raw_to_hex final.header.proposed_state_root;
    ledger_state_root = proof.ledger_state_root; txid_hi = 0L;
    config_hash = hex "config"; validator_set_hash = P.raw_to_hex
      (Octra_consensus.C_config.validator_set_hash set);
    quorum_cert_hash = Some (A.finalize_hash final);
    epoch_index_hash = Some (hex "entry"); epoch_index_root = Some index;
    created_at = 0L; valid_until = 1000L} in
  let checkpoint_hash = get (P.hash checkpoint) in
  let files = ["HEAD.json"; "ledger.dat"; Octra_bootstrap.Root_win.name; "state_root"]
    |> List.sort String.compare |> List.map (fun path -> M.{path; size = 1L;
      sha256 = hex "file"; chunks = [{index = 0; offset = 0L; size = 1; sha256 = hex "file"}]}) in
  let manifest = M.{checkpoint_hash; snapshot_id = checkpoint_hash; irmin_commit = None;
    chunk_size = chunk_size_min; total_size = 4L; file_count = 4; chunk_count = 4;
    chunks_root = chunks_root files; files} in
  let exporter_signatures = List.map (fun ((member : C.validator_info), secret) ->
    let wallet = Octra_core.Crypto.Wallet.{address = member.C.address;
      pub = Base64.encode_exn member.pubkey; priv = Base64.encode_exn secret} in
    get (M.make_exporter_signature ~wallet manifest)) keys |> List.sort
      (fun (left : P.signature) right -> String.compare left.signer right.signer) in
  let value = M.{checkpoint; checkpoint_hash; manifest;
    manifest_hash = get (manifest_hash manifest); exporter_signatures;
    authority = Finalized (A.make ~steps ~finalize:final ~validator_set:set |> A.encode)} in
  get (M.verify_certificate ~validator_set:trusted ~exporter_set:trusted value)

let test_roles () =
  check "author role" (C.is_author ~chain_id:chain ~epoch_id:height selected creator);
  check "author cannot vote" (not (C.is_validator selected creator));
  check "author has no vote weight" (C.weight_of_addr selected creator = None);
  check "voters unchanged" (C.weights_of_set selected = C.weights_of_set target);
  check "quorum unchanged" (selected.quorum_weight = target.quorum_weight);
  check "author expires" (not (C.is_author ~chain_id:chain ~epoch_id:(Int64.succ height)
    selected creator));
  check "author chain" (not (C.is_author ~chain_id:"other" ~epoch_id:height selected creator));
  let wire = Octra_consensus.C_codec.encode_validator_set selected in
  check "author wire" (Octra_consensus.C_codec.decode_validator_set wire = selected);
  let refused set = try ignore (Octra_consensus.C_codec.decode_validator_set
    (Octra_consensus.C_codec.encode_validator_set set)); false with _ -> true in
  let entries = Option.get selected.authors |> snd in
  let author, weight = List.hd entries in
  check "duplicate author codec refused" (refused {selected with authors = Some (height, entries @ entries)});
  check "voting author codec refused" (refused {selected with authors = Some (height,
    (List.hd selected.validators, weight) :: entries)});
  check "zero author weight refused" (refused {selected with authors = Some (height, [author, Z.zero])});
  check "unweighted author refused" (refused {selected with weighted = false});
  let members = List.init 3 (fun i ->
    let key = Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 (Char.chr (i + 1)))
      |> Result.get_ok in
    let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
    C.{pubkey; address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey)}) in
  let voters = get (C.make_weighted_validator_set
    (List.filteri (fun i _ -> i < 2) members |> List.map (fun member -> member, Z.one))) in
  let cross = {voters with authors = Some (1_710_743L, [List.nth members 2, Z.one])} in
  check "wallet author hash agrees" (Octra_bootstrap.State_sync_manifest.raw_to_hex
    (Octra_consensus.C_config.validator_set_hash cross)
    = "5c4f470596cf5147bcff628fd4e12693c7de8e23729e3b15b4fe1b0f199394a0");
  check "wallet author height is hashed"
    (Octra_consensus.C_config.validator_set_hash cross <>
      Octra_consensus.C_config.validator_set_hash
        {cross with authors = Some (1_710_744L, [List.nth members 2, Z.one])});
  let module Light = Octra_consensus.C_light_validator_set in
  let view = Light.of_validator_set ~chain_id:chain ~config_hash:
    (Octra_consensus.C_config.hash ~chain_id:chain ~validator_set:selected ()) selected in
  check "light proof keeps author" (Light.verify view);
  let json = Octra_node_runtime.Rpc_view.validator_set_proof view in
  let decoded = get (Octra_consensus.C_light_json.validator_set json) in
  check "light author roundtrip" (decoded = view && Light.verify decoded);
  check "light missing role refused" (not (Light.verify {view with authors = None}));
  check "light changed role refused" (not (Light.verify {view with
    authors = Option.map (fun (epoch, entries) -> Int64.succ epoch, entries) view.authors}));
  let cert = finalize selected 65 (header height (raw "done")) in
  get (A.verify_finalize ~chain_id:chain ~validator_set:selected cert);
  let extra = finalize source 65 cert.header in
  let removed = List.find (fun (vote : C.vote) -> vote.validator = creator) extra.precommits in
  check "removed precommit refused" (Result.is_error (A.verify_finalize ~chain_id:chain
    ~validator_set:selected {cert with precommits = removed :: cert.precommits}));
  let older = Int64.pred (Option.get (Octra_consensus.C_quorum_policy.linear_epoch ~chain_id:chain)) in
  let before = {selected with C.authors = Some (older, Option.get selected.authors |> snd)} in
  check "historical author refused" (Result.is_error (A.verify_finalize ~chain_id:chain
    ~validator_set:before (finalize target 65 {cert.header with epoch_id = older})));
  let leader = (C.leader_of target ~epoch_id:height ~round:65).address in
  let fresh = C.{chain_id = chain; epoch_id = height; round = 65; valid_round = None;
    header = cert.header; tx_hashes = []; parent_commit = None; proposer = leader; signature = ""} in
  check "fresh proposal requires elected author"
    (not (C.proposal_is_well_formed ~chain_id:chain ~validator_set:selected fresh));
  check "plain target rejects" (Result.is_error (A.verify_finalize ~chain_id:chain
    ~validator_set:target cert));
  let parent = C.{certificate = C.certificate_of_finalize cert; validator_set = selected} in
  let author = Octra_consensus.C_parent_commit.proposer parent |> get in
  check "reward author" (author.address = creator);
  let directory = Test_workspace.unique_dir "relief-journal" in
  check "journal seed" (J.seed ~chain_id:chain ~validator_set:selected ~finalize:cert directory = Ok J.Seeded);
  check "journal restore" (match J.read_committed_epoch_validated ~chain_id:chain
    ~validator_set:selected ~epoch:height directory with J.Valid _ -> true | _ -> false);
  check "wrong origin refused" (match J.read_committed_epoch_validated ~chain_id:chain
    ~validator_set:target ~epoch:height directory with J.Invalid _ -> true | _ -> false)

let test_engine () =
  let nodes = List.map (fun (v, _) -> E.create ~chain_id:chain ~my_addr:v.C.address
    ~validator_set:source ~start_height:height ~can_vote:(fun () -> true)) keys in
  let value = header height (raw "engine") in
  let pid = H.proposal_id value in
  let p = C.{chain_id = chain; epoch_id = height; round = 0; valid_round = None;
    header = value; tx_hashes = []; parent_commit = None; proposer = creator;
    signature = String.make 64 '\000'} in
  let p = {p with signature = sign creator (H.propose_sign_bytes p)} in
  let votes = List.concat_map (fun node ->
    E.on_propose node p ~verify_fn:verify ~execute_fn:(fun _ -> true)
      ~sign_fn:(sign node.E.my_addr);
    drain node |> List.filter_map (function E.SendVote (v, _) when v.C.vote_type = C.Prevote -> Some v | _ -> None)) nodes in
  let votes = List.find (fun (v : C.vote) -> v.validator = creator) votes ::
    (List.filter (fun (v : C.vote) -> v.validator <> creator) votes |> List.filteri (fun i _ -> i < 4)) in
  List.iter (fun node ->
    List.iter (fun vote -> E.on_vote node vote ~sign_fn:(sign node.E.my_addr)) votes;
    check "lock before relief" (node.E.state.locked_value = Some value);
    ignore (drain node);
    E.reconfigure_validator_set node ~round:65 selected;
    E.start_height node height;
    ignore (drain node)) nodes;
  let active = List.filter (fun node -> C.is_validator target node.E.my_addr) nodes in
  let leader = (C.leader_of target ~epoch_id:height ~round:65).address in
  let sender = List.find (fun node -> node.E.my_addr = leader) active in
  E.do_propose sender {value with creator_addr = leader; proposed_state_root = raw "fresh"}
    [] ~sign_fn:(sign leader);
  let finals = ref [] in
  let rec deliver fuel outputs =
    check "delivery ends" (fuel > 0);
    List.iter (function
      | E.SendPropose p ->
        check "same value" (H.proposal_id p.header = pid);
        List.iter (fun node -> if node.E.my_addr <> p.proposer then
          E.on_propose node p ~verify_fn:verify ~execute_fn:(fun _ -> true)
            ~sign_fn:(sign node.my_addr)) active
      | E.SendVote (v, _) ->
        check "vote signed" (verify v.validator (H.vote_sign_bytes v) v.signature);
        check "new votes target only" (C.is_validator target v.validator);
        List.iter (fun node -> E.on_vote node v ~sign_fn:(sign node.E.my_addr)) active
      | E.Finalized {finalize; _} ->
        get (A.verify_finalize ~chain_id:chain ~validator_set:selected finalize);
        finals := finalize.proposal_id :: !finals
      | _ -> ()) outputs;
    let more = List.concat_map drain active |> List.filter (function
      | E.Finalized {finalize; _} ->
        get (A.verify_finalize ~chain_id:chain ~validator_set:selected finalize);
        finals := finalize.proposal_id :: !finals; false
      | E.SendPropose _ | E.SendVote _ -> true | _ -> false) in
    if more <> [] then deliver (fuel - 1) more in
  deliver 100 (drain sender);
  check "removed author finalizes" (List.length !finals >= List.length active);
  check "only locked value finalizes" (List.for_all ((=) pid) !finals)

let test_source_votes proof_round =
  let value = header height (raw "prior-polc") in
  let pid = H.proposal_id value in
  let round = max 65 (proof_round + 1) in
  let leader = (C.leader_of target ~epoch_id:height ~round).address in
  let member = List.find (fun v -> v.C.address <> leader) target.validators in
  let node = E.create ~chain_id:chain ~my_addr:member.address ~validator_set:selected
    ~start_height:height ~can_vote:(fun () -> true) in
  E.start_round node round;
  ignore (drain node);
  let proposal = C.{chain_id = chain; epoch_id = height; round; valid_round = Some proof_round;
    header = value; tx_hashes = []; parent_commit = None; proposer = leader;
    signature = String.make 64 '\000'} in
  let proposal = {proposal with signature = sign leader (H.propose_sign_bytes proposal)} in
  E.on_propose node proposal ~verify_fn:verify ~execute_fn:(fun _ -> true)
    ~sign_fn:(sign member.address);
  check "unproven author cannot get vote" (drain node |> List.for_all (function
    | E.SendVote (vote, _) -> vote.C.proposal_id <> pid | _ -> true));
  let selected_keys = List.find (fun (v, _) -> v.C.address = creator) keys ::
    (List.filter (fun (v, _) -> v.C.address <> creator) keys |> List.filteri (fun n _ -> n < 4)) in
  List.iter (fun (v, _) ->
    let vote = C.{chain_id = chain; epoch_id = height; round = proof_round; vote_type = Prevote;
      proposal_id = pid; validator = v.address; signature = String.make 64 '\000'} in
    let vote = {vote with signature = sign v.address (H.vote_sign_bytes vote)} in
    E.on_vote node vote ~sign_fn:(sign member.address)) selected_keys;
  check "source polc restored" (E.polc_matches node ~round:proof_round ~proposal_id:pid);
  check "source proof can be served" (List.length (E.polc_votes_for_round node proof_round) = 5);
  check "source votes not in target quorum"
    (not (Hashtbl.mem (Hashtbl.find node.prevotes_by_round proof_round).votes creator));
  E.start_height node (Int64.succ height);
  check "source votes expire" (Hashtbl.length node.source_prevotes = 0)

let test_restart_split () =
  let module D = Octra_consensus.C_driver in
  let module V = Octra_consensus.C_vote_log in
  let module Codec = Octra_consensus.C_codec in
  let module Wal = Octra_core.Wal in
  let module Peer = Octra_net.P2p_conn in
  let with_peer ?pubkey run =
    let fd, writer = Lwt_unix.pipe () in
    let conn = Peer.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
      ~peer_id:(Option.fold ~none:"proof-peer"
        ~some:Octra_net.P2p_handshake.node_id_of_pubkey pubkey)
      ~addr:"proof-peer" ~direction:Peer.Inbound in
    let sent = ref [] in
    let rec collect () =
      let open Lwt.Syntax in
      let* item = Peer.until_closed conn (Lwt_mvar.take conn.write_queue) in
      match item with None -> Lwt.return_unit
      | Some frame -> sent := frame :: !sent; collect () in
    let reader = collect () in
    Lwt_main.run (Lwt.finalize
      (fun () -> Lwt.pick [run conn sent;
        (let open Lwt.Syntax in let* () = Lwt_unix.sleep 120. in
          Lwt.fail_with "peer exchange deadline")])
      (fun () -> let open Lwt.Syntax in let* () = Peer.close conn in
        let* () = reader in Lwt_unix.close writer)) in
  let value = header height (raw "split") in
  let pid = H.proposal_id value in
  let proposal = C.{chain_id = chain; epoch_id = height; round = 0; valid_round = None;
    header = value; tx_hashes = []; parent_commit = None; proposer = creator; signature = ""} in
  let proposal = {proposal with signature = sign creator (H.propose_sign_bytes proposal)} in
  let cfg = D.{activate_epoch = plan.activate_epoch; validator_set = target;
    fingerprint = plan.fingerprint} in
  let finals = ref [] in
  let make path member =
    let head = ref (Int64.pred height) in
    let swarm = Octra_net.P2p_swarm.create Octra_net.P2p_swarm.{
      listen_port = 0; chain_id = chain;
      node_id = Octra_net.P2p_handshake.node_id_of_pubkey member.C.pubkey;
      node_addr = member.address; pubkey_raw = member.pubkey;
      consensus_config_hash = raw "config"; binary_hash = raw "binary";
      require_binary_hash = false; upgrade_plan = None; profile_plan = [];
      allowed_pubkeys = []; bootstrap_peers = []; max_peers = 0;
      sign_fn = sign member.address; best_epoch_fn = (fun () -> Int64.pred height);
      best_root_fn = (fun () -> value.prev_state_root)} in
    let config = D.{chain_id = chain; my_addr = member.address;
      sign_fn = sign member.address; verify_fn = verify;
      role_can_vote = (fun () -> true); can_vote = (fun () -> true);
      execute_fn = (fun _ -> true); verify_proposal = (fun _ -> Lwt.return Proposal_accept);
      verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ());
      on_finalized = (fun ~validator_set final ->
        get (A.verify_finalize ~chain_id:chain ~validator_set final);
        check "driver final set" (validator_set = selected);
        check "driver final stored" (Result.is_ok
          (J.seed ~chain_id:chain ~validator_set ~finalize:final path));
        finals := (member.address, final.proposal_id) :: !finals;
        head := final.epoch_id;
        Lwt.return_unit);
      make_proposal = (fun _ -> Lwt.return_none);
      before_precommit_broadcast = (fun ~epoch_id ~round ~proposal_id
        ~proposed_state_root ~txid_hi ~proposal_wire ~vote_wire ->
        Wal.write_pending_commit path Wal.{epoch_id = Int64.to_int epoch_id; round;
          proposal_id = Octra_node_runtime.Text.hash32_hex proposal_id;
          proposed_state_root = Octra_node_runtime.Text.hash32_hex proposed_state_root;
          txid_hi; ts = 0.; validator_addr = member.address;
          proposal_b64 = Some (Base64.encode_exn proposal_wire);
          vote_b64 = Some (Base64.encode_exn vote_wire);
          tx_hashes = []; txs_json = []; receipts_json = []}; Lwt.return_true);
      lookup_epoch_root = (fun _ -> None); local_head_epoch = (fun () -> !head);
      lookup_bundle = (fun _ -> None);
      lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
      on_resource_attestation = (fun _ -> Lwt.return_unit);
      scheduled_validator_set_config = Some cfg;
      load_scheduled_validator_set_config = (fun () -> Lwt.return_some cfg);
      resource_committee_config = None} in
    D.create ~config ~validator_set:source ~swarm ~start_height:height
      ~sync_log:(Octra_consensus.C_sync_log.memory ())
      ~relief_log:(Octra_consensus.C_relief_log.disk ~data_dir:path)
      ~vote_log:(V.disk ~data_dir:path) in
  let members = target.validators in
  let nodes = List.map (fun member ->
    let path = Test_workspace.unique_dir "relief-restart" in path, member, make path member) members in
  let voters = List.find (fun (member, _) -> member.C.address = creator) keys ::
    (List.filter (fun (member, _) -> member.C.address <> creator) keys
      |> List.filteri (fun i _ -> i < 4)) in
  let votes = List.map (fun (member, _) ->
    let vote = C.{chain_id = chain; epoch_id = height; round = 0; vote_type = Prevote;
      proposal_id = pid; validator = member.address; signature = ""} in
    {vote with signature = sign member.address (H.vote_sign_bytes vote)}) voters in
  List.iteri (fun index (_, member, node) ->
    if index < 2 then begin
      E.on_propose node.D.engine proposal ~verify_fn:verify ~execute_fn:(fun _ -> true)
        ~sign_fn:(sign member.C.address);
      List.iter (fun vote -> E.on_vote node.engine vote ~sign_fn:(sign member.address)) votes;
      check "two nodes lock" (node.engine.state.locked_value = Some value);
      let emitted = drain node.engine |> List.filter_map (function
        | E.SendVote (vote, _) -> Some vote | _ -> None) in
      check "locked node emitted precommit"
        (List.exists (fun vote -> vote.C.vote_type = Precommit) emitted);
      List.iter (fun vote ->
        check "proof saved before precommit"
          (Option.is_some (Lwt_main.run (D.saved_vote ~proposal node vote)))) emitted
    end;
    E.start_round node.engine 64;
    ignore (drain node.engine);
    node.running <- true;
    with_peer (fun conn _ ->
      Lwt_list.iter_s (fun sync -> D.on_p2p_message node conn
        Octra_net.P2p_frame.{msg_type = msg_cons_round_sync;
          payload = Codec.encode_round_sync sync}) proof);
    check "signed sync installs relief" (node.engine.vs = selected
      && Option.is_some node.relief_plan);
    node.running <- false) nodes;
  let nodes = List.mapi (fun index (path, member, _) ->
    let node = make path member in
    let records = Wal.read_pending_commits path in
    check "only locked nodes have WAL" (List.length records = if index < 2 then 1 else 0);
    List.iter (fun record ->
      let restored = Octra_node_runtime.Consensus_pending_commit_recovery.recover_record
        ~chain_id:chain ~validator_set:source record |> get in
      ignore restored;
      let p = Option.get record.Wal.proposal_b64 |> Base64.decode_exn |> Codec.decode_propose in
      ignore (get (D.restore_precommit_lock node p))) records;
    ignore (get (Lwt_main.run (D.restore_relief node)));
    node.running <- true;
    ignore (drain node.engine);
    path, member, node) nodes in
  let _, _, witness = List.hd nodes in
  let restored = E.polc_votes_for_round witness.D.engine 0 in
  check "disk proof survives fleet restart" (List.length restored >= 5);
  List.iteri (fun index (_, _, node) ->
      if index >= 2 then begin
        check "four nodes restart free" (node.D.engine.state.locked_value = None);
        let round = node.engine.state.round in
        let leader = (C.leader_of selected ~epoch_id:height ~round).address in
        let offer = {proposal with round; valid_round = Some 0; proposer = leader; signature = ""} in
        let offer = {offer with signature = sign leader (H.propose_sign_bytes offer)} in
        E.on_propose node.engine offer ~verify_fn:verify ~execute_fn:(fun _ -> true)
          ~sign_fn:(sign node.config.my_addr);
        check "free node waits for saved proof" (drain node.engine |> List.for_all (function
          | E.SendVote (v, _) -> v.C.proposal_id <> pid | _ -> true));
        with_peer ~pubkey:(Option.get (C.pubkey_of_addr selected node.config.my_addr)) (fun conn sent ->
          let open Lwt.Syntax in
          let request = Codec.{chain_id = chain; epoch_id = height; round = 0;
            step = C.PrevoteStep; request = true; validator = node.config.my_addr;
            signature = ""} in
          let request = {request with signature = sign request.validator
            (H.round_sync_sign_bytes request)} in
          let* () = D.on_p2p_message witness conn Octra_net.P2p_frame.{
            msg_type = msg_cons_round_sync; payload = Codec.encode_round_sync request} in
          let* () = Lwt.pause () in
          let frames = List.rev !sent |> List.filter (fun (frame : Octra_net.P2p_frame.frame) ->
            frame.msg_type = Octra_net.P2p_frame.msg_cons_vote) in
          check "request returns saved proof" (List.length frames >= 5);
          Lwt_list.iter_s (D.on_p2p_message node conn) frames);
        check "driver accepts restored proof" (E.polc_matches node.engine ~round:0 ~proposal_id:pid)
      end) nodes;
  let drivers = List.map (fun (_, _, node) -> node) nodes in
  let round = (List.hd drivers).D.engine.state.round + 1 in
  List.iter (fun node -> E.start_round node.D.engine round;
    ignore (drain node.engine)) drivers;
  let leader = (C.leader_of selected ~epoch_id:height ~round).address in
  let p = {proposal with round; valid_round = Some 0; proposer = leader; signature = ""} in
  let p = {p with signature = sign leader (H.propose_sign_bytes p)} in
  let frames = Queue.create () in
  let receive node frame = with_peer (fun conn sent ->
    let open Lwt.Syntax in
    Hashtbl.replace node.D.swarm.peers conn.peer_id conn;
    Lwt.finalize (fun () ->
      let* () = D.on_p2p_message node conn frame in
      let rec settle turns =
        check "driver work ends" (turns > 0);
        let* () = Lwt.pause () in
        if Octra_consensus.C_work_slot.idle node.proposal_slot
          && Octra_consensus.C_output_actor.is_idle node.output_actor then Lwt.return_unit
        else let* () = D.process_outputs node in settle (turns - 1) in
      let* () = settle 100 in
      List.rev !sent |> List.iter (fun (frame : Octra_net.P2p_frame.frame) ->
        if frame.msg_type = Octra_net.P2p_frame.msg_cons_vote
          || frame.msg_type = Octra_net.P2p_frame.msg_cons_finalize then
          Queue.add (node.config.my_addr, frame) frames);
      Lwt.return_unit)
      (fun () -> Hashtbl.remove node.swarm.peers conn.peer_id; Lwt.return_unit)) in
  check "driver final starts empty" (!finals = []);
  Fun.protect ~finally:(fun () -> Lwt_main.run (Lwt_list.iter_s D.stop drivers)) (fun () ->
    let frame = Octra_net.P2p_frame.{msg_type = msg_cons_propose;
      payload = Codec.encode_propose p} in
    List.iter (fun node -> receive node frame) drivers;
    let rec deliver fuel =
      if not (Queue.is_empty frames) then begin
        check "split delivery ends" (fuel > 0);
        let sender, frame = Queue.take frames in
        List.iter (fun node -> if node.D.config.my_addr <> sender then receive node frame) drivers;
        deliver (fuel - 1)
      end in
    deliver 200;
    check "split nodes finalize after restart" (List.length !finals = 6);
    check "split only finalizes original value" (List.for_all (fun (_, value) -> value = pid) !finals);
    List.iter (fun (path, member, node) ->
      check "driver final recorded once" (List.length (List.filter
        (fun (address, _) -> address = member.C.address) !finals) = 1);
      check "driver advances after apply" (node.D.engine.state.height = Int64.succ height
        && node.config.local_head_epoch () = height && node.engine.pending_finalized = None);
      check "driver final journal" (match J.read_committed_epoch_validated
        ~chain_id:chain ~validator_set:selected ~epoch:height path with
        | J.Valid entry -> entry.finalize.proposal_id = pid | _ -> false)) nodes);
  let disk = V.disk ~data_dir:(Test_workspace.unique_dir "polc-controls") in
  let keep votes = V.polc_keep disk ~chain_id:chain ~epoch_id:height
    ~validator_set:selected ~locked_round:0 votes in
  check "short proof refused" (Result.is_error (keep [List.hd restored]));
  check "duplicate proof refused" (Result.is_error (keep (List.hd restored :: restored)));
  check "changed signature refused" (Result.is_error (keep
    (List.map (fun (v : C.vote) -> {v with C.signature = String.make 64 '\000'}) restored)));
  ignore (get (keep restored));
  let newer = List.map (fun (v : C.vote) ->
    let v = {v with round = 1; signature = ""} in
    {v with signature = sign v.validator (H.vote_sign_bytes v)}) restored in
  ignore (get (keep newer));
  let saved = get (V.polc_load disk ~chain_id:chain ~epoch_id:height ~validator_set:selected) in
  check "lock and valid proofs retained" (List.length saved = List.length restored * 2
    && List.exists (fun (v : C.vote) -> v.round = 0) saved
    && List.exists (fun (v : C.vote) -> v.round = 1) saved);
  let third = List.map (fun (v : C.vote) ->
    let v = {v with round = 2; signature = ""} in
    {v with signature = sign v.validator (H.vote_sign_bytes v)}) restored in
  ignore (get (keep third));
  ignore (get (keep newer));
  let saved = get (V.polc_load disk ~chain_id:chain ~epoch_id:height ~validator_set:selected) in
  check "incoming proof retained" (List.length saved = List.length restored * 3
    && List.exists (fun (v : C.vote) -> v.round = 0) saved
    && List.exists (fun (v : C.vote) -> v.round = 1) saved
    && List.exists (fun (v : C.vote) -> v.round = 2) saved);
  let sync_disk = V.disk ~data_dir:(Test_workspace.unique_dir "polc-sync") in
  let sync_calls = ref 0 in
  let sync _ = incr sync_calls;
    if !sync_calls = 1 then Error "sync refused" else Ok () in
  check "first sync refuses write" (Result.is_error (V.polc_keep ~sync sync_disk
    ~chain_id:chain ~epoch_id:height ~validator_set:selected ~locked_round:0 restored));
  check "renamed proof stays readable" (List.length (get (V.polc_load sync_disk
    ~chain_id:chain ~epoch_id:height ~validator_set:selected)) = List.length restored);
  ignore (get (V.polc_keep ~sync sync_disk ~chain_id:chain ~epoch_id:height
    ~validator_set:selected ~locked_round:0 restored));
  check "repeat sync fences proof" (!sync_calls = 2);
  let plain_path = Test_workspace.unique_dir "polc-plain-restart" in
  let plain_disk = V.disk ~data_dir:plain_path in
  ignore (get (V.polc_keep plain_disk ~chain_id:chain ~epoch_id:height
    ~validator_set:source ~locked_round:(-1) restored));
  let plain_node = make plain_path (List.hd target.validators) in
  ignore (get (Lwt_main.run (D.restore_relief plain_node)));
  check "normal restart restores proof" (List.length
    (E.polc_votes_for_round plain_node.engine 0) = List.length restored);
  let partial_path = Test_workspace.unique_dir "polc-partial" in
  let partial_node = make partial_path (List.hd target.validators) in
  E.record_polc partial_node.engine 0 pid;
  E.record_historical_prevote partial_node.engine (List.hd restored);
  partial_node.engine.state <- {partial_node.engine.state with valid_round = 0};
  ignore (get (D.keep_polc partial_node 0 ~required:false));
  check "partial proof stays unrecorded" (V.polc_load (V.disk ~data_dir:partial_path)
    ~chain_id:chain ~epoch_id:height ~validator_set:source = Ok []);
  let path = Test_workspace.unique_dir "polc-write-order" in
  let node = make path (List.hd target.validators) in
  ignore (get (D.restore_precommit_lock node proposal));
  E.start_round node.engine 3;
  List.iter (fun (vote : C.vote) ->
    E.record_polc node.engine vote.round vote.proposal_id;
    E.record_historical_prevote node.engine vote) (restored @ newer @ third);
  ignore (get (D.keep_polc node 0 ~required:true));
  node.engine.state <- {node.engine.state with locked_round = 1; valid_round = 1};
  ignore (get (D.keep_polc node 2 ~required:true));
  ignore (get (D.keep_polc node 1 ~required:true));
  let before_wal = get (V.polc_load (V.disk ~data_dir:path)
    ~chain_id:chain ~epoch_id:height ~validator_set:source) in
  check "prior WAL proof kept until write" (List.length before_wal = List.length restored * 3
    && List.exists (fun (v : C.vote) -> v.round = 0) before_wal
    && List.exists (fun (v : C.vote) -> v.round = 1) before_wal
    && List.exists (fun (v : C.vote) -> v.round = 2) before_wal);
  ignore (get (V.prune disk ~through_epoch:height));
  check "applied proof expires" (V.polc_load disk ~chain_id:chain ~epoch_id:height
    ~validator_set:selected = Ok [])

let test_split_conflict epoch lose_valid =
  let author = (C.leader_of source ~epoch_id:epoch ~round:0).address in
  let target = get (C.make_weighted_validator_set
    (C.weights_of_set source |> List.filter (fun (v, _) -> v.C.address <> author))) in
  let leader = (C.leader_of target ~epoch_id:epoch ~round:65).address in
  let rest = List.filter (fun v -> v.C.address <> leader) target.validators in
  let locked = [leader; (List.nth rest 0).address] in
  let faulty = [(List.nth rest 1).address; (List.nth rest 2).address] in
  let signers = author :: locked @ faulty in
  let nodes = source.validators |> List.map (fun member -> E.create ~chain_id:chain
    ~my_addr:member.C.address ~validator_set:source ~start_height:epoch ~can_vote:(fun () -> true)) in
  let a = {(header epoch (raw "old-final")) with creator_addr = author} in
  let pid_a = H.proposal_id a in
  let offer round proposer valid_round header =
    let p = C.{chain_id = chain; epoch_id = epoch; round; valid_round; header;
      tx_hashes = []; parent_commit = None; proposer; signature = ""} in
    {p with signature = sign proposer (H.propose_sign_bytes p)} in
  let vote round vote_type proposal_id validator =
    let v = C.{chain_id = chain; epoch_id = epoch; round; vote_type; proposal_id; validator; signature = ""} in
    {v with signature = sign validator (H.vote_sign_bytes v)} in
  List.iter (fun node ->
    if List.mem node.E.my_addr signers then begin
      E.on_propose node (offer 0 author None a) ~verify_fn:verify
        ~execute_fn:(fun _ -> true) ~sign_fn:(sign node.my_addr);
      List.iter (fun addr -> E.on_vote node (vote 0 Prevote pid_a addr)
        ~sign_fn:(sign node.my_addr)) signers;
      check "old quorum locks A" (node.state.locked_value = Some a)
    end;
    ignore (drain node)) nodes;
  let old = List.find (fun node -> node.E.my_addr = author) nodes in
  List.iter (fun addr -> E.on_vote old (vote 0 Precommit pid_a addr)
    ~sign_fn:(sign author)) signers;
  check "old peer finalizes A" (drain old |> List.exists (function
    | E.Finalized {finalize; _} -> finalize.proposal_id = pid_a | _ -> false));
  let update = get (U.make_weighted ~source_epoch:(Int64.pred epoch)
    ~activate_epoch:(Int64.add epoch 8L) (C.weights_of_set target |> List.map (fun (v, weight) ->
      Octra_core.Validator_entry.{address = v.C.address; pubkey = v.pubkey; weight}))) in
  let proof = List.map (fun (v, _) ->
    let sync = Octra_consensus.C_codec.{chain_id = chain; epoch_id = epoch;
      round = 64; step = C.PrecommitStep; request = false; validator = v.C.address; signature = ""} in
    {sync with signature = sign v.address (H.round_sync_sign_bytes sync)}) keys in
  let mark = match R.decide ~chain_id:chain ~height:epoch ~current:source
    ~activate_epoch:update.activate_epoch ~target ~fingerprint:update.fingerprint ~proof with
    | R.Apply mark -> mark | _ -> failwith "split relief refused" in
  let selected = get (R.restore ~chain_id:chain ~height:epoch ~current:source
    ~activate_epoch:update.activate_epoch ~target ~fingerprint:update.fingerprint mark) |> Option.get in
  let honest = List.filter (fun node -> node.E.my_addr <> author && not (List.mem node.my_addr faulty)) nodes in
  List.iter (fun node ->
    E.reconfigure_validator_set node ~round:65 selected;
    if List.mem node.E.my_addr locked then
      check "relief keeps lock and valid" (node.state.locked_value = Some a && node.state.valid_value = Some a);
    if lose_valid then node.state <- {node.state with valid_round = -1; valid_value = None};
    ignore (drain node)) honest;
  let proposer = List.find (fun node -> node.E.my_addr = leader) honest in
  let b = {(header epoch (raw "conflict-final")) with creator_addr = leader} in
  let pid_b = H.proposal_id b in
  E.do_propose proposer b [] ~sign_fn:(sign leader);
  let finals = ref [] in
  let rec deliver fuel outputs =
    check "conflict schedule ends" (fuel > 0);
    List.iter (function
      | E.SendPropose p -> List.iter (fun node ->
        if node.E.my_addr <> leader then E.on_propose node p ~verify_fn:verify
          ~execute_fn:(fun _ -> true) ~sign_fn:(sign node.my_addr)) honest
      | E.SendVote (v, _) -> List.iter (fun node -> E.on_vote node v ~sign_fn:(sign node.E.my_addr)) honest
      | E.Finalized {finalize; _} -> finals := finalize.proposal_id :: !finals;
        List.iter (fun node -> ignore (E.ack_finalized node finalize)) honest
      | _ -> ()) outputs;
    let more = List.concat_map drain honest in
    if more <> [] then deliver (fuel - 1) more in
  deliver 30 (drain proposer);
  List.iter (fun kind ->
    List.iter (fun addr -> List.iter (fun node ->
      E.on_vote node (vote 65 kind pid_b addr) ~sign_fn:(sign node.E.my_addr)) honest) faulty;
    deliver 30 (List.concat_map drain honest)) [C.Prevote; Precommit];
  check "conflicting B never finalizes" (not (List.mem pid_b !finals));
  check "old final remains A" (old.state.locked_value = Some a)

let test_snapshot () =
  let directory = Test_workspace.unique_dir "relief-image" in
  let store = Lwt_main.run (I.open_store ~fresh:true directory) in
  Fun.protect ~finally:(fun () -> Lwt_main.run (I.close store)) (fun () ->
    let epoch = Int64.to_int (Int64.pred height) in
    let update = U.to_string plan in
    Lwt_main.run (I.set_meta store U.pending_meta_key update);
    Lwt_main.run (I.tag_epoch store epoch);
    let proof = get (Lwt_main.run (I.merkle_proof_at_epoch store epoch ["meta"; U.pending_meta_key])) in
    let index = hex "index" in
    let folded = Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:proof.ledger_state_root ~epoch_index_root:index in
    let parent = finalize source 0 (header (Int64.pred height)
      Digestif.SHA256.(of_hex folded |> to_raw_string)) in
    let step = A.{source = Relief mark; finalize = parent;
      ledger_state_root = proof.ledger_state_root; epoch_index_root = index;
      update; proof = proof.proof} in
    let cert = finalize selected 65 (header height (raw "snapshot")) in
    let make step set = A.make ~steps:[step] ~finalize:cert ~validator_set:set |> A.encode in
    let wire = make step selected in
    let checked = get (A.verify_path ~validator_set:trusted wire) in
    let reduced = get (Lwt_main.run (A.compact_lwt ~validator_set:trusted checked)) in
    check "relief compaction keeps checked link" (A.encode reduced = wire);
    check "snapshot roles" (A.validator_set checked = selected);
    check "snapshot marker" (get (A.relief checked) = Some mark);
    let next_epoch = Int64.succ height in
    let next_update = get (U.make_weighted ~source_epoch:height
      ~activate_epoch:(Int64.succ next_epoch)
      (C.weights_of_set target |> List.map (fun (v, weight) ->
        Octra_core.Validator_entry.{address = v.C.address; pubkey = v.pubkey; weight})))
      |> U.to_string in
    Lwt_main.run (I.set_meta store U.pending_meta_key next_update);
    Lwt_main.run (I.tag_epoch store (Int64.to_int next_epoch));
    let next_proof = get (Lwt_main.run (I.merkle_proof_at_epoch store
      (Int64.to_int next_epoch) ["meta"; U.pending_meta_key])) in
    let next_root = Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:next_proof.ledger_state_root ~epoch_index_root:index
      |> Digestif.SHA256.of_hex |> Digestif.SHA256.to_raw_string in
    let next_header epoch root = { (header epoch root) with
      creator_addr = (List.hd target.validators).address } in
    let link = A.{source = Pending; finalize = finalize target 0 (next_header next_epoch next_root);
      ledger_state_root = next_proof.ledger_state_root; epoch_index_root = index;
      update = next_update; proof = next_proof.proof} in
    List.iter (fun epoch ->
      let final = finalize target 0 (next_header epoch (raw "after-relief")) in
      let original = A.make ~steps:[step; link] ~finalize:final ~validator_set:target in
      ignore (get (A.verify_path ~validator_set:trusted (A.encode original)));
      let short = get (Lwt_main.run (A.compact_lwt ~validator_set:trusted original)) in
      let checked = get (A.verify_path ~validator_set:trusted (A.encode short)) in
      let live = epoch < mark.activate_epoch in
      check "compaction lost live relief" (get (A.relief checked) = if live then Some mark else None);
      check "relief shortest path" (match A.steps short with
        | [{A.source = Relief kept; _}] when live -> kept = mark
        | [{A.source = Pending; _}] when not live -> true
        | _ -> false);
      let again = get (Lwt_main.run (A.compact_lwt ~validator_set:trusted checked)) in
      check "relief compaction repeats" (A.encode again = A.encode short))
      [Int64.succ next_epoch; mark.activate_epoch];
    Lwt_main.run (I.set_meta store U.pending_meta_key update);
    check "snapshot tree restored" (Lwt_main.run (I.state_hash store) = proof.ledger_state_root);
    let module Store = Octra_core.Store_chaindata in
    let module Chain = Octra_node_runtime.Sync_chain in
    let data = Test_workspace.unique_dir "relief-export" in
    let chaindata = Store.open_chaindata (Filename.concat data "chain") in
    Fun.protect ~finally:(fun () -> Store.close chaindata) (fun () ->
      Store.set_epoch_index_commitment_direct chaindata ~epoch_id:epoch
        ~epoch_hash:(hex "entry") ~root:index;
      let log = Octra_consensus.C_relief_log.disk ~data_dir:data in
      ignore (get (Octra_consensus.C_relief_log.keep log mark));
      let deps = Chain.{data_dir = data; chain_id = chain; store; chaindata;
        read_finality = (fun requested -> if requested = Int64.pred height then
          J.Valid {finalize = parent; validator_set = source; bundle = None} else J.Missing);
        certificate_path = (fun () -> Filename.concat data "absent")} in
      let steps = get (Lwt_main.run (Chain.build deps ~head_epoch:height trusted selected)) in
      check "producer relief step" (steps = [step]);
      ignore (get (Lwt_main.run (Chain.keep_relief deps selected cert)));
      ignore (get (Octra_consensus.C_relief_log.prune log ~through_epoch:plan.activate_epoch));
      let cold = {deps with read_finality = (fun _ -> J.Missing)} in
      ignore (get (Lwt_main.run (Chain.keep_relief cold selected cert)));
      check "local replay rejects another value" (Result.is_error
        (Lwt_main.run (Chain.keep_relief cold selected
          (finalize selected 65 {cert.header with proposed_state_root = raw "conflict"}))));
      List.iter (fun epoch ->
        let current = if epoch = height then selected else target in
        let steps = get (Lwt_main.run (Chain.build cold ~head_epoch:epoch trusted current)) in
        check "export after history loss" (steps = [step]))
        [height; Int64.succ height; Int64.pred plan.activate_epoch; plan.activate_epoch]);
    check "snapshot key codecs" (get (A.raw_validator_set (get (A.encoded_validator_set selected))) = selected);
    let bad_step changed =
      check "damaged transition refused"
        (Result.is_error (A.verify_path ~validator_set:trusted (make changed selected)));
      let damaged = A.decode (make changed selected) |> get in
      check "compaction accepted damaged relief"
        (Result.is_error (Lwt_main.run (A.compact_lwt ~validator_set:trusted damaged))) in
    bad_step {step with proof = "invalid"};
    bad_step {step with source = Relief {mark with fingerprint = String.make 64 'a'}};
    bad_step {step with source = Relief {mark with proof = []}};
    bad_step {step with source = Relief {mark with height = Int64.succ height}};
    check "no author metadata trust" (Result.is_error
      (A.verify_path ~validator_set:trusted (make step {selected with authors = None})));
    let module Floor = Octra_core.History_floor in
    let image_header = {cert.header with proposed_state_root = parent.header.proposed_state_root} in
    let image_final = finalize selected 65 image_header in
    let parent_commit = C.{certificate = C.certificate_of_finalize image_final; validator_set = selected} in
    let floor = get (Floor.create ~chain_id:chain ~epoch:(Int64.to_int height)
      ~state_root:folded ~ledger_state_root:proof.ledger_state_root ~txid_hi:0L
      ~config_hash:(hex "config")
      ~validator_set_hash:(Octra_node_runtime.Text.raw_to_hex
        (Octra_consensus.C_config.validator_set_hash selected))
      ~epoch_index_hash:(hex "entry") ~epoch_index_root:index ~parent_commit) in
    let loaded = Floor.to_string floor |> Floor.of_string |> get in
    check "image keeps author role" ((Floor.parent_commit loaded).validator_set = selected);
    let module M = Octra_bootstrap.State_sync_manifest in
    let module Sync = Octra_bootstrap.State_sync in
    let module Seed = Octra_node_runtime.Sync_finality in
    let imported = Test_workspace.unique_dir "relief-import" in
    let published = certificate [step] image_final proof index selected in
    let module Image = Octra_core.Ledger_image in
    let image = Filename.concat imported "ledger.dat" in
    let restored = Filename.concat imported "restored" in
    let commit = Lwt_main.run (I.get_commit_hash store) |> Option.get in
    ignore (get (Lwt_main.run (Image.write store ~commit ~path:image)));
    ignore (get (Lwt_main.run (Image.restore ~source:image ~target:restored
      ~expected_root:proof.ledger_state_root)));
    let restored = Lwt_main.run (I.open_store restored) in
    Fun.protect ~finally:(fun () -> Lwt_main.run (I.close restored)) (fun () ->
      check "image keeps plan" (Lwt_main.run (I.get_meta restored U.pending_meta_key) = Some update));
    M.write_json (Sync.anchor_path imported) (M.certificate_json published);
    let seed () = Seed.seed ~data_dir:imported ~chain_id:chain ~floor:(Ok (Some loaded))
      ~head:(Int64.to_int height) ~root:(Some folded) ~txid:(Some 0L)
      ~trusted:(fun () -> Ok trusted) ~exporters:(fun () -> Ok trusted) ~active:source in
    check "snapshot seeds journal" (seed () = Ok Seed.Seeded);
    check "snapshot seed repeats" (seed () = Ok Seed.Current);
    check "snapshot seeds relief" (Octra_consensus.C_relief_log.latest
      (Octra_consensus.C_relief_log.disk ~data_dir:imported) ~through_height:height = Ok (Some mark));
    check "snapshot journal has roles" (match J.read_committed_epoch_validated ~chain_id:chain
      ~validator_set:selected ~epoch:height imported with J.Valid _ -> true | _ -> false);
    let second_height = Int64.add height 10L in
    let second_author = (List.hd target.validators).C.address in
    let second_target = get (C.make_weighted_validator_set
      (C.weights_of_set target |> List.filter (fun (v, _) -> v.C.address <> second_author))) in
    let second_plan = get (U.make_weighted ~source_epoch:(Int64.pred second_height)
      ~activate_epoch:(Int64.add second_height 8L)
      (C.weights_of_set second_target |> List.map (fun (v, weight) ->
        Octra_core.Validator_entry.{address = v.C.address; pubkey = v.pubkey; weight}))) in
    let votes = List.map (fun v ->
      let vote = Octra_consensus.C_codec.{chain_id = chain; epoch_id = second_height;
        round = 64; step = C.PrecommitStep; request = false;
        validator = v.C.address; signature = ""} in
      {vote with signature = sign vote.validator (H.round_sync_sign_bytes vote)})
      target.validators in
    let second_mark = match R.decide ~chain_id:chain ~height:second_height ~current:target
      ~activate_epoch:second_plan.activate_epoch ~target:second_target
      ~fingerprint:second_plan.fingerprint ~proof:votes with
      | R.Apply mark -> mark | _ -> failwith "second relief refused" in
    let second_set = get (R.restore ~chain_id:chain ~height:second_height ~current:target
      ~activate_epoch:second_plan.activate_epoch ~target:second_target
      ~fingerprint:second_plan.fingerprint second_mark) |> Option.get in
    Lwt_main.run (I.set_meta store U.active_meta_key (U.to_string plan));
    Lwt_main.run (I.set_meta store U.pending_meta_key (U.to_string second_plan));
    let second_epoch = Int64.to_int (Int64.pred second_height) in
    Lwt_main.run (I.tag_epoch store second_epoch);
    let state = get (Lwt_main.run (I.merkle_proof_at_epoch store second_epoch ["meta"; U.pending_meta_key])) in
    let root = Octra_core.Epoch_index_commitment.folded_state_root
      ~ledger_state_root:state.ledger_state_root ~epoch_index_root:index in
    let parent = finalize target 0 {cert.header with epoch_id = Int64.pred second_height;
      creator_addr = second_author; proposed_state_root = Digestif.SHA256.(of_hex root |> to_raw_string)} in
    let directory = Test_workspace.unique_dir "relief-published" in
    let chaindata = Store.open_chaindata (Filename.concat directory "chain") in
    let prior_path = Sys.getenv_opt "OCTRA_STATE_SYNC_CERT" in
    Fun.protect ~finally:(fun () -> Store.close chaindata;
      Unix.putenv "OCTRA_STATE_SYNC_CERT" (Option.value prior_path ~default:"")) (fun () ->
      let path = Filename.concat directory "published.json" in
      Unix.putenv "OCTRA_STATE_SYNC_CERT" path;
      M.write_json path (M.certificate_json published);
      check "no imported proof" (not (Sys.file_exists (Sync.anchor_path directory)));
      Store.set_epoch_index_commitment_direct chaindata ~epoch_id:second_epoch
        ~epoch_hash:(hex "second") ~root:index;
      ignore (get (Octra_consensus.C_relief_log.keep
        (Octra_consensus.C_relief_log.disk ~data_dir:directory) second_mark));
      let deps = Chain.{data_dir = directory; chain_id = chain; store; chaindata;
        certificate_path = (fun () -> Octra_node_runtime.State_sync_http.certificate_path ~data_dir:directory);
        read_finality = (fun epoch -> if epoch = Int64.pred second_height then
          J.Valid {validator_set = target; finalize = parent; bundle = None} else J.Missing)} in
      let next = finalize second_set 65 {cert.header with epoch_id = second_height;
        creator_addr = second_author} in
      let wrong = {deps with certificate_path = (fun () -> Sync.anchor_path directory)} in
      ignore (get (Lwt_main.run (Chain.keep_relief wrong second_set next)));
      check "export still needs trusted prefix"
        (Result.is_error (Lwt_main.run (Chain.build wrong
          ~head_epoch:second_height trusted second_set)));
      let module N = Octra_node_runtime in
      let head = ref second_epoch in
      let applied = ref false in
      let cache = N.Consensus_bundle_cache.create ~cap:4 in
      let before_apply set final =
        let open Lwt.Syntax in
        let* saved = Chain.keep_relief wrong set final in
        match saved with Ok () -> Lwt.return_unit | Error why -> Lwt.fail_with why in
      let runtime = N.Consensus_finality_runtime.create_node_runtime ~before_apply
        N.Consensus_finality_runtime.{
          committed_epoch_root_raw = (fun epoch -> if epoch = !head then Some next.header.proposed_state_root else None);
          data_dir = directory; bundles = N.Consensus_bundle_cache.node_runtime cache;
          driver_ref = ref None; proposal_state = N.Consensus_proposal_state.create ();
          catchup_queue = N.Consensus_catchup_queue.create (); consensus_finalized = ref false;
          current_epoch = ref (Int64.to_int second_height); committed_head_epoch = (fun () -> !head);
          sleep = (fun _ -> head := Int64.to_int second_height; Lwt.return_unit);
          read_pre_finalize_root = (fun () -> Some next.header.prev_state_root);
          read_commit_root = (fun () -> Lwt.return_some next.header.proposed_state_root);
          read_local_root_raw = (fun () -> Lwt.return next.header.proposed_state_root);
          apply_timeout_seconds = 120.; stall_timeout_seconds = 120.;
          require_sync = (fun _ -> failwith "local proof required unrelated sync");
          fatal_exit = (fun () -> failwith "local proof stopped valid finality");
          catchup_active = ref false; runtime_state = N.Consensus_runtime_state.create ();
          set_state_attested = (fun ~head:_ ~root:_ -> applied := true);
          finality = N.Consensus_finality_state.callbacks (N.Consensus_finality_state.create ());
        } in
      Lwt_main.run (Lwt.pick [runtime.apply_finalized ~validator_set:second_set next;
        (let open Lwt.Syntax in let* () = Lwt_unix.sleep 120. in Lwt.fail_with "local apply deadline")]);
      check "finality applies without export prefix" (!applied && match
        J.read_committed_epoch_validated ~chain_id:chain ~validator_set:second_set
          ~epoch:second_height directory with J.Valid _ -> true | _ -> false);
      ignore (get (Lwt_main.run (Chain.keep_relief deps second_set next)));
      let steps = get (Lwt_main.run (Chain.build {deps with read_finality = (fun _ -> J.Missing)}
        ~head_epoch:second_height trusted second_set)) in
      check "published proof joins relief" (List.length steps = 2);
      ignore (get (A.verify_path ~validator_set:trusted
        (A.make ~steps ~finalize:next ~validator_set:second_set |> A.encode))));
    let later = finalize target 0 {cert.header with epoch_id = Int64.succ height;
      creator_addr = (List.hd target.validators).address} in
    let wire = A.make ~steps:[step] ~finalize:later ~validator_set:target |> A.encode in
    ignore (get (A.verify_path ~validator_set:trusted wire)))

let () =
  test_roles ();
  test_engine ();
  List.iter test_source_votes [0; 65];
  test_restart_split ();
  List.iter (fun epoch -> List.iter (test_split_conflict epoch) [false; true]) [1_500_000L; height];
  test_snapshot ();
  Printf.printf "test = relief_path status = pass\n"