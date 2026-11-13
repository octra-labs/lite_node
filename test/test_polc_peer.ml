(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus
module C = C_types
module E = C_engine
module H = C_hash
module J = Yojson.Safe.Util
module D = C_driver
module F = Octra_net.P2p_frame

let need reason yes = if not yes then failwith reason
let raw text = Digestif.SHA256.(digest_string text |> to_raw_string)
let decode f value = f (Base64.decode_exn (J.to_string value))
let encode f value = `String (Base64.encode_exn (f value))
let get = function Ok value -> value | Error reason -> failwith reason

let keys = List.init 31 (fun index ->
  let secret = raw ("polc-key-" ^ string_of_int index) in
  let priv = Mirage_crypto_ec.Ed25519.priv_of_octets secret |> Result.get_ok in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv priv |> pub_to_octets) in
  let address = Octra_core.Crypto.Address.address_from_pubkey (Base64.encode_exn pubkey) in
  C.{address; pubkey}, secret)

let sign address msg =
  let _, secret = List.find (fun (member, _) -> member.C.address = address) keys in
  H.sign_ed25519 ~priv_raw:secret ~msg

let answer value =
  print_endline ("peer = " ^ Yojson.Safe.to_string value);
  flush stdout

let vote_snapshot (votes : E.vote_set) =
  let entries table = Hashtbl.fold (fun key value rows -> (key, value) :: rows) table []
    |> List.sort compare in
  votes.count_total, votes.count_nil, votes.weight_total, votes.weight_nil,
  entries votes.votes, entries votes.by_pid, entries votes.by_pid_weight

let peer_snapshot node =
  let engine = node.D.engine in
  let proofs = Hashtbl.fold (fun round pid rows -> (round, pid) :: rows)
    engine.E.polc_by_round [] |> List.sort compare in
  engine.state, vote_snapshot engine.prevotes, vote_snapshot engine.precommits,
  proofs, engine.pending_finalized

let vote_probe exchange peers bad good =
  let forged = decode C_codec.decode_vote bad in
  let valid = decode C_codec.decode_vote good in
  need "probe statements differ" ({forged with C.signature = valid.C.signature} = valid
    && forged.signature <> valid.signature);
  let address, node = List.find (fun (address, _) -> address <> valid.validator) peers in
  let engine = node.D.engine in
  need "probe context differs" (valid.chain_id = engine.E.chain_id
    && valid.epoch_id = engine.state.height && valid.round = engine.state.round
    && valid.vote_type = C.Prevote);
  need "probe vote already present" (not (Hashtbl.mem engine.prevotes.votes valid.validator));
  let before = peer_snapshot node in
  let count = engine.prevotes.count_total in
  need "probe vote count differs" (count = 1);
  ignore (exchange node valid.validator [F.{msg_type = msg_cons_vote;
    payload = C_codec.encode_vote forged}]);
  let rejected = engine.prevotes.count_total in
  need "driver accepted forged vote" (peer_snapshot node = before);
  ignore (exchange node valid.validator [F.{msg_type = msg_cons_vote;
    payload = C_codec.encode_vote valid}]);
  let accepted = engine.prevotes.count_total in
  let stored = Hashtbl.find_opt engine.prevotes.votes valid.validator in
  need "driver refused valid control" (stored = Some valid && accepted = count + 1);
  `Assoc ["receiver", `String address; "validator", `String valid.validator;
    "counts", `List (List.map (fun value -> `Int value) [count; rejected; accepted]);
    "stored_vote", encode C_codec.encode_vote (Option.get stored)]

let run () =
  let first = Yojson.Safe.from_string (read_line ()) in
  let sample = J.member "sample" first |> J.to_list in
  let members, locked, prior, proposal = match sample with
    | [members; locked; prior; proposal] ->
      decode C_codec.decode_validator_set members,
      List.map J.to_string (J.to_list locked),
      decode C_codec.decode_propose prior, decode C_codec.decode_propose proposal
    | _ -> failwith "sample shape differs" in
  let epoch = prior.C.epoch_id in
  need "unexpected epoch" (List.mem epoch [1_737_349L; 1_740_000L]);
  need "unexpected validator count" (members.C.n = 31 && List.length locked = 17);
  List.iter (fun member ->
    need "test key differs" (List.exists (fun (key, _) -> key = member) keys)) members.validators;
  let verify address msg signature = match C.pubkey_of_addr members address with
    | None -> false | Some pubkey -> H.verify_ed25519 ~pubkey_raw:pubkey ~msg ~signature in
  List.iter (fun (value : C.propose) ->
    need "proposal signature differs" (verify value.proposer (H.propose_sign_bytes value) value.signature))
    [prior; proposal];
  need "recovered value differs" (proposal.epoch_id = epoch && proposal.valid_round = Some prior.round
    && proposal.header = prior.header && proposal.tx_hashes = prior.tx_hashes
    && proposal.parent_commit = prior.parent_commit);
  let selected = members.validators |> List.filter_map (fun member ->
    let chosen = match J.member "members" first |> J.to_string with
      | "locked" -> List.mem member.C.address locked
      | "unlocked" -> not (List.mem member.C.address locked)
      | _ -> failwith "member role differs" in
    if chosen then Some member.address else None) in
  need "selected members differ" (selected <> []
    && List.length selected = List.length (List.sort_uniq String.compare selected)
    && List.for_all (fun address -> Option.is_some (C.pubkey_of_addr members address)) selected);
  let proof = List.map (fun member ->
    let vote = C.{chain_id = prior.chain_id; epoch_id = epoch; round = prior.round;
      proposal_id = H.proposal_id prior.header; vote_type = Prevote;
      validator = member.address; signature = ""} in
    {vote with signature = sign member.address (H.vote_sign_bytes vote)}) members.validators in
  get (C_vote_log.polc_check ~chain_id:prior.chain_id ~epoch_id:epoch ~validator_set:members proof);
  let peers = List.map (fun address ->
    let member = List.find (fun member -> member.C.address = address) members.validators in
    let swarm = Octra_net.P2p_swarm.create Octra_net.P2p_swarm.{
      listen_port = 0; chain_id = prior.chain_id;
      node_id = Octra_net.P2p_handshake.node_id_of_pubkey member.pubkey;
      node_addr = address; pubkey_raw = member.pubkey;
      consensus_config_hash = raw "config"; binary_hash = raw "binary";
      require_binary_hash = false; upgrade_plan = None; profile_plan = [];
      allowed_pubkeys = []; bootstrap_peers = []; max_peers = 0;
      sign_fn = sign address; best_epoch_fn = (fun () -> Int64.pred epoch);
      best_root_fn = (fun () -> prior.header.prev_state_root)} in
    let config = D.{chain_id = prior.chain_id; my_addr = address; sign_fn = sign address;
      verify_fn = verify; role_can_vote = (fun () -> true); can_vote = (fun () -> true);
      execute_fn = (fun p -> p.C.header = prior.header && p.tx_hashes = prior.tx_hashes);
      verify_proposal = (fun p -> Lwt.return (if p.C.header = prior.header
        && p.tx_hashes = prior.tx_hashes && p.parent_commit = prior.parent_commit
        then Proposal_accept else Proposal_reject));
      verify_parent_commit = (fun ~epoch_id:_ parent ->
        if parent = prior.parent_commit then Ok () else Error "parent differs");
      on_finalized = (fun ~validator_set:_ _ -> Lwt.fail_with "unexpected final apply");
      make_proposal = (fun _ -> Lwt.return_none);
      before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
        ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ -> Lwt.return_true);
      lookup_epoch_root = (fun _ -> None); local_head_epoch = (fun () -> Int64.pred epoch);
      lookup_bundle = (fun _ -> None);
      lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
      on_resource_attestation = (fun _ -> Lwt.return_unit);
      scheduled_validator_set_config = None;
      load_scheduled_validator_set_config = (fun () -> Lwt.return_none);
      resource_committee_config = None} in
    let node = D.create ~config ~validator_set:members ~swarm ~start_height:epoch
      ~sync_log:(C_sync_log.memory ()) ~relief_log:(C_relief_log.memory ()) ~vote_log:(C_vote_log.memory ()) in
    node.running <- true;
    let engine = node.engine in
    if List.mem address locked then begin
      get (E.restore_precommit_lock engine prior);
      get (C_vote_log.polc_keep node.vote_log ~chain_id:prior.chain_id ~epoch_id:epoch
        ~validator_set:members ~locked_round:prior.round proof);
      E.record_polc engine prior.round (H.proposal_id prior.header);
      List.iter (E.record_historical_prevote engine) proof;
      E.cache_proposal_message engine prior;
      List.iter (fun (vote : C.vote) -> if vote.validator = address then
        ignore (get (C_vote_log.keep node.vote_log vote))) proof
    end;
    E.start_round engine proposal.round;
    ignore (E.drain_outputs engine);
    address, node) selected in
  let nodes = List.map (fun (address, node) -> address, node.D.engine) peers in
  let frame_json (frame : F.frame) = `List [`Int frame.msg_type;
    `String (Base64.encode_exn frame.payload)] in
  let frame_of_json = function
    | `List [`Int msg_type; `String payload] -> F.{msg_type; payload = Base64.decode_exn payload}
    | _ -> failwith "frame shape differs" in
  let exchange node address frames =
    let pubkey = C.pubkey_of_addr members address |> Option.get in
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = Octra_net.P2p_conn.create ~peer_class:Octra_net.P2p_frame_budget.Validator
      local ~peer_id:(Octra_net.P2p_handshake.node_id_of_pubkey pubkey)
      ~addr:"polc-peer" ~direction:Octra_net.P2p_conn.Inbound in
    Lwt_main.run (Lwt.finalize (fun () ->
      let open Lwt.Syntax in
      let marker = F.{msg_type = 0; payload = ""} in
      let rec consume saved =
        let* frame = Lwt_mvar.take conn.write_queue in
        if frame == marker then Lwt.return (List.rev saved)
        else consume (frame :: saved) in
      let reading = consume [] in
      let* () = Lwt_list.iter_s (D.on_p2p_message node conn) frames in
      let* () = Lwt_mvar.put conn.write_queue marker in
      reading) (fun () ->
        let open Lwt.Syntax in
        let* () = Octra_net.P2p_conn.close conn in Lwt_unix.close remote)) in
  let outputs () = `List (List.concat_map (fun (address, engine) ->
    E.drain_outputs engine |> List.filter_map (function
      | E.SendVote (vote, _) ->
        need "local vote identity differs" (vote.C.validator = address
          && verify address (H.vote_sign_bytes vote) vote.signature);
        Some (encode C_codec.encode_vote vote)
      | _ -> None)) nodes) in
  answer (`Assoc ["status", `String "ready"; "count", `Int (List.length nodes);
    "pid", `String (Base64.encode_exn (H.proposal_id prior.header));
    "epoch", `Intlit (Int64.to_string epoch);
    "round", `Int proposal.round]);
  let rec receive () =
    match Yojson.Safe.from_string (read_line ()) with
    | `List [`String "request"] ->
      let address, node = List.hd peers in
      need "locked peer requested evidence" (not (List.mem address locked));
      need "unlocked peer already had evidence" (not (E.polc_matches node.engine
        ~round:prior.round ~proposal_id:(H.proposal_id prior.header)));
      List.iter (fun (_, peer) ->
        E.request_polc peer.D.engine prior.round;
        ignore (E.drain_outputs peer.engine)) peers;
      let sync = D.make_round_sync_at node ~round:prior.round ~step:C.PrevoteStep ~request:true in
      answer (`List [`String address; frame_json F.{msg_type = F.msg_cons_round_sync;
        payload = C_codec.encode_round_sync sync}]); receive ()
    | `List [`String "reply"; `String address; frame] ->
      let own, node = List.hd peers in
      need "unlocked peer answered evidence" (List.mem own locked);
      let frames = exchange node address [frame_of_json frame] in
      answer (`List [`String own; `List (List.map frame_json frames)]); receive ()
    | `List [`String "evidence"; `String address; `List frames] ->
      List.iter (fun (_, node) ->
        ignore (exchange node address (List.map frame_of_json frames))) peers;
      answer (`List (List.map (fun (_, node) -> `Bool (E.polc_matches node.D.engine
        ~round:prior.round ~proposal_id:(H.proposal_id prior.header))) peers)); receive ()
    | `List [`String "propose"] ->
      List.iter (fun (address, engine) ->
        E.on_propose engine proposal ~verify_fn:verify
          ~execute_fn:(fun value -> value.header = prior.header
            && value.tx_hashes = prior.tx_hashes && value.parent_commit = prior.parent_commit)
          ~sign_fn:(sign address)) nodes;
      answer (outputs ()); receive ()
    | `List [`String "vote_probe"; bad; good] ->
      answer (vote_probe exchange peers bad good); receive ()
    | `List [`String "state_control"] ->
      let node = snd (List.hd peers) in
      let value = Option.get node.D.engine.pending_finalized in
      let root = value.C.header.proposed_state_root in
      let changed = String.mapi (fun index byte ->
        if index = 0 then Char.chr (Char.code byte lxor 1) else byte) root in
      let header = {value.header with C.proposed_state_root = changed} in
      let proposal_id = H.proposal_id header in
      let precommits = List.map (fun member ->
        let vote = C.{chain_id = value.chain_id; epoch_id = value.epoch_id;
          round = value.commit_round; proposal_id; vote_type = Precommit;
          validator = member.address; signature = ""} in
        {vote with signature = sign member.address (H.vote_sign_bytes vote)}) members.validators in
      let changed = {value with C.header = header; proposal_id; precommits} in
      need "state control certificate rejected" (C_qc.validate_finalize ~chain_id:prior.chain_id
        ~validator_set:members ~verify_vote:(fun vote -> verify vote.C.validator
          (H.vote_sign_bytes vote) vote.signature) changed = C_qc.Valid);
      answer (encode C_codec.encode_finalize changed); receive ()
    | `List [`String "votes"; `List values] ->
      need "message count exceeds limit" (List.length values <= 31);
      let votes = List.map (decode C_codec.decode_vote) values in
      List.iter (fun (vote : C.vote) ->
        need "vote signature differs" (verify vote.validator (H.vote_sign_bytes vote) vote.signature);
        need "vote context differs" (vote.chain_id = prior.chain_id && vote.epoch_id = epoch
          && vote.round = proposal.round)) votes;
      List.iter (fun (address, engine) ->
        List.iter (fun vote -> E.on_vote engine vote ~sign_fn:(sign address)) votes) nodes;
      answer (outputs ()); receive ()
    | `List [`String "final"] ->
      answer (`List (List.map (fun (address, engine) ->
        `Assoc ["address", `String address; "locked", `Int engine.E.state.locked_round;
          "certificate", (match engine.pending_finalized with
            | None -> `Null | Some value -> encode C_codec.encode_finalize value);
          "final", (match engine.pending_finalized with
            | None -> `Null | Some value ->
              need "final certificate rejected"
                (C_qc.validate_finalize ~chain_id:prior.chain_id ~validator_set:members
                  ~verify_vote:(fun vote -> verify vote.C.validator (H.vote_sign_bytes vote) vote.signature) value
                  = C_qc.Valid);
              need "final value differs" (value.header = prior.header
                && value.proposal_id = H.proposal_id prior.header);
              `String (Base64.encode_exn value.proposal_id))]) nodes));
      receive ()
    | `List [`String "stop"] ->
      List.iter (fun (_, node) -> Lwt_main.run (D.stop node)) peers;
      answer (`String "stopped")
    | _ -> failwith "peer command differs" in
  receive ()

let () = run ()