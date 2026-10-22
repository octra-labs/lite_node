(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

let mk_validator addr =
  let pubkey = String.make 32 '\x00' in
  C_types.{ address = addr; pubkey }

let mk_vs addrs =
  C_engine.make_validator_set (List.map mk_validator addrs)

let mk_header ?(chain_id = "octra-test") ?(epoch_id = 1L) ?(creator = "v1") tag =
  C_types.{
    proto_version = C_types.proto_version_current;
    chain_id;
    epoch_id;
    prev_state_root = String.make 32 '\xaa';
    tx_list_hash = String.make 32 '\xbb';
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = String.make 32 (Char.chr (Char.code 'A' + tag));
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = creator;
    txid_hi = 0L;
    ts = 0.0;
  }

let mk_propose ?(chain_id = "octra-test") ?(epoch_id = 1L) ?(round = 0)
    ?(valid_round = None) ?(proposer = "v0") header =
  C_types.{
    chain_id;
    epoch_id;
    round;
    valid_round;
    header;
    tx_hashes = [];
    parent_commit = None;
    proposer;
    signature = String.make 64 '\x00';
  }

let mk_vote ?(chain_id = "octra-test") ?(epoch_id = 1L) ?(round = 0)
    vote_type proposal_id validator =
  C_types.{
    chain_id;
    epoch_id;
    round;
    vote_type;
    proposal_id;
    validator;
    signature = String.make 64 '\x00';
  }

let mk_finalize ?(chain_id = "octra-test") ?(epoch_id = 1L) ?(commit_round = 0) header =
  let proposal_id = C_hash.proposal_id header in
  C_types.{
    chain_id;
    epoch_id;
    commit_round;
    header;
    proposal_id;
    precommits = [];
    parent_commit = None;
  }

let four_validators = ["v0"; "v1"; "v2"; "v3"]
let one_validator = ["v0"]

let make_engine ?(my_addr = "v0") validators =
  let vs = mk_vs validators in
  C_engine.create
    ~chain_id:"octra-test"
    ~my_addr
    ~validator_set:vs
    ~start_height:1L
    ~can_vote:(fun () -> true)

let drain t = C_engine.drain_outputs t
let drain_clear t = let _ = drain t in ()

let dummy_sign _msg = String.make 64 '\x00'
let always_true _addr _msg _sig = true
let always_exec _propose = true

let last_prevote outputs =
  List.fold_left (fun acc o ->
    match o with
    | C_engine.SendVote (v, _) when v.vote_type = C_types.Prevote -> Some v
    | _ -> acc
  ) None outputs

let leader_addr t round =
  (C_engine.leader_of t.C_engine.vs ~epoch_id:t.state.height ~round).address

let inject_prevote_quorum t round pid =
  let needed = t.C_engine.vs.quorum - 1 in
  let voters =
    four_validators
    |> List.filter (fun a -> a <> t.C_engine.my_addr)
    |> List.filteri (fun i _ -> i < needed)
  in
  List.iter (fun addr ->
    let v = mk_vote ~round C_types.Prevote pid addr in
    C_engine.on_vote t v ~sign_fn:dummy_sign
  ) voters

let lock_on_round_0 t header =
  let p = mk_propose ~proposer:(leader_addr t 0) header in
  C_engine.on_propose t p ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let pid = C_hash.proposal_id header in
  inject_prevote_quorum t 0 pid;
  drain_clear t;
  pid

let advance_to_round t r =
  C_engine.start_round t r;
  drain_clear t

let test_no_lock_accepts_anything () =
  let t = make_engine four_validators in
  drain_clear t;
  let h = mk_header 0 in
  let p = mk_propose ~proposer:(leader_addr t 0) h in
  C_engine.on_propose t p ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (v.proposal_id = C_hash.proposal_id h)
  | None -> failwith "no prevote emitted"

let test_lock_conflict () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _pid_a = lock_on_round_0 t h_a in
  assert (t.state.locked_round = 0);
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let p_b = mk_propose ~round:1 ~proposer:(leader_addr t 1) h_b in
  C_engine.on_propose t p_b ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (Octra_net.Hash_domain.is_nil v.proposal_id)
  | None -> failwith "no prevote emitted"

let test_lock_same_value () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let pid_a = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let p_a_again =
    mk_propose ~round:1 ~valid_round:(Some 0) ~proposer:(leader_addr t 1) h_a
  in
  C_engine.on_propose t p_a_again
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (v.proposal_id = pid_a)
  | None -> failwith "no prevote emitted"

let test_valid_round_prior () =
  let t = make_engine four_validators in
  advance_to_round t 1;
  let h_a = mk_header 0 in
  let proposal =
    mk_propose
      ~round:1
      ~valid_round:(Some 1)
      ~proposer:(leader_addr t 1)
      h_a
  in
  C_engine.on_propose t proposal
    ~verify_fn:always_true
    ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  match last_prevote (drain t) with
  | Some vote ->
    assert (Octra_net.Hash_domain.is_nil vote.proposal_id)
  | None -> failwith "no prevote emitted"

let test_unlock_with_polc () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let pid_a = lock_on_round_0 t h_a in
  assert (Hashtbl.find_opt t.polc_by_round 0 = Some pid_a);
  advance_to_round t 1;
  let p_a_round1 =
    mk_propose ~round:1 ~valid_round:(Some 0) ~proposer:(leader_addr t 1) h_a
  in
  C_engine.on_propose t p_a_round1
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (v.proposal_id = pid_a)
  | None -> failwith "no prevote emitted"

let test_polc_other_value () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let pid_a = lock_on_round_0 t h_a in
  assert (Hashtbl.find_opt t.polc_by_round 0 = Some pid_a);
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let p_b_round1 =
    mk_propose ~round:1 ~valid_round:(Some 0) ~proposer:(leader_addr t 1) h_b
  in
  C_engine.on_propose t p_b_round1
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (Octra_net.Hash_domain.is_nil v.proposal_id)
  | None -> failwith "no prevote emitted"

let test_polc_missing_round () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let p_b_round1 = mk_propose ~round:1 ~proposer:(leader_addr t 1) h_b in
  C_engine.on_propose t p_b_round1
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (Octra_net.Hash_domain.is_nil v.proposal_id)
  | None -> failwith "no prevote emitted"

let test_polc_locked_round () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let p_b_round1 =
    mk_propose ~round:1 ~valid_round:(Some 0) ~proposer:(leader_addr t 1) h_b
  in
  C_engine.on_propose t p_b_round1
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let outs = drain t in
  match last_prevote outs with
  | Some v ->
    assert (Octra_net.Hash_domain.is_nil v.proposal_id)
  | None -> failwith "no prevote emitted"

let test_timeout_lock () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  let locked_before = t.state.locked_round in
  let valid_before = t.state.valid_round in
  let value_before = t.state.locked_value in
  C_engine.on_timeout t ~step:C_types.PrecommitStep ~round:0 ~generation:t.generation
    ~sign_fn:dummy_sign;
  drain_clear t;
  assert (t.state.round = 1);
  assert (t.state.locked_round = locked_before);
  assert (t.state.valid_round = valid_before);
  assert (t.state.locked_value = value_before)

let test_historical_polc_recovery () =
  let t = make_engine four_validators in
  let proposer = leader_addr t 0 in
  let header = mk_header ~creator:proposer 2 in
  let proposal = mk_propose ~proposer header in
  let proposal_id = C_hash.proposal_id header in
  C_engine.on_propose
    t
    proposal
    ~verify_fn:always_true
    ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  drain_clear t;
  C_engine.on_vote
    t
    (mk_vote C_types.Prevote proposal_id "v1")
    ~sign_fn:dummy_sign;
  assert (not (C_engine.polc_matches t ~round:0 ~proposal_id));
  C_engine.start_round t 1;
  let requested =
    List.exists
      (function C_engine.RequestRoundEvidence 0 -> true | _ -> false)
      (drain t)
  in
  assert requested;
  C_engine.on_vote
    t
    (mk_vote C_types.Prevote proposal_id "v2")
    ~sign_fn:dummy_sign;
  assert (C_engine.polc_matches t ~round:0 ~proposal_id);
  assert (not (C_engine.polc_request_pending t 0));
  assert
    (List.map
       (fun (vote : C_types.vote) -> vote.validator)
       (C_engine.polc_votes_for_round t 0)
     = ["v0"; "v1"; "v2"]);
  match t.state.valid_value with
  | Some value ->
    assert (t.state.valid_round = 0);
    assert (C_hash.proposal_id value = proposal_id)
  | None -> failwith "historical PoLC did not restore valid value"

let test_polc_request_retry () =
  let t = make_engine four_validators in
  drain_clear t;
  C_engine.start_round t 1;
  let first =
    List.filter_map
      (function C_engine.RequestRoundEvidence round -> Some round | _ -> None)
      (drain t)
  in
  assert (first = [0]);
  C_engine.start_round t 2;
  let second =
    List.filter_map
      (function C_engine.RequestRoundEvidence round -> Some round | _ -> None)
      (drain t)
  in
  assert (List.sort Int.compare second = [0; 1])

let test_polc_storage_limit () =
  let t = make_engine four_validators in
  drain_clear t;
  for round = 1 to 200 do
    C_engine.start_round t round;
    drain_clear t
  done;
  assert (Hashtbl.length t.polc_requests = C_engine.round_history_limit);
  assert
    (Hashtbl.length t.prevotes_by_round <= C_engine.round_history_limit + 1)

let test_lock_conflict_cache () =
  let t = make_engine four_validators in
  let header_a = mk_header ~creator:(leader_addr t 0) 3 in
  let _ = lock_on_round_0 t header_a in
  advance_to_round t 1;
  let round_one_leader = leader_addr t 1 in
  let header_b = mk_header ~creator:round_one_leader 4 in
  let proposal_id_b = C_hash.proposal_id header_b in
  C_engine.on_propose
    t
    (mk_propose ~round:1 ~proposer:round_one_leader header_b)
    ~verify_fn:always_true
    ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  drain_clear t;
  advance_to_round t 2;
  C_engine.on_propose
    t
    (mk_propose
       ~round:2
       ~valid_round:(Some 1)
       ~proposer:(leader_addr t 2)
       header_b)
    ~verify_fn:always_true
    ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  assert (C_engine.polc_request_pending t 1);
  List.iter
    (fun validator ->
      C_engine.on_vote
        t
        (mk_vote ~round:1 C_types.Prevote proposal_id_b validator)
        ~sign_fn:dummy_sign)
    ["v1"; "v2"; "v3"];
  assert (C_engine.polc_matches t ~round:1 ~proposal_id:proposal_id_b);
  match t.state.valid_value with
  | Some value ->
    assert (t.state.valid_round = 1);
    assert (C_hash.proposal_id value = proposal_id_b)
  | None -> failwith "cached value was not promoted by historical PoLC"

let test_vote_quorum_pid () =
  let vs = C_engine.create_vote_set () in
  let pid_a = String.make 32 '\x11' in
  let pid_b = String.make 32 '\x22' in
  let validator_set =
    C_engine.make_validator_set
      (List.init 7 (fun index -> mk_validator (Printf.sprintf "v%d" index)))
  in
  let add validator pid =
    C_engine.add_vote
      vs
      (mk_vote C_types.Prevote pid validator)
      ~validator_set
  in
  assert (add "v0" pid_b = `Added);
  assert (add "v1" pid_b = `Added);
  assert (add "v2" pid_a = `Added);
  assert (add "v3" pid_a = `Added);
  assert (add "v4" pid_a = `QuorumAny);
  assert (add "v5" pid_a = `QuorumAny);
  assert (add "v6" pid_a = `QuorumOf pid_a);
  assert (C_engine.count_for_pid vs pid_a = 5);
  assert (C_engine.count_for_pid vs pid_b = 2)

let test_finalize_lower_lock () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let accepted = C_engine.accept_finalize_batch t (mk_finalize ~commit_round:0 h_b) in
  assert (not accepted);
  assert (drain t = [])

let test_finalize_higher_lock () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let accepted = C_engine.accept_finalize_batch t (mk_finalize ~commit_round:1 h_b) in
  assert accepted;
  let outs = drain t in
  let finalized =
    List.find_map (function
      | C_engine.Finalized { finalize; _ } ->
        Some (finalize.C_types.header, finalize.C_types.commit_round)
      | _ -> None
    ) outs
  in
  match finalized with
  | Some (header, round) ->
    assert (C_hash.proposal_id header = C_hash.proposal_id h_b);
    assert (round = 1)
  | None -> failwith "no Finalized emitted for higher-round finalize batch"

let test_polc_reset_on_height_change () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  assert (Hashtbl.length t.polc_by_round > 0);
  assert (t.state.locked_round >= 0);
  C_engine.start_height t 2L;
  assert (Hashtbl.length t.polc_by_round = 0);
  assert (t.state.locked_round = -1);
  assert (t.state.valid_round = -1);
  assert (t.state.locked_value = None)

let test_single_reproposal () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let empty_tlh = Octra_net.Hash_domain.hash "octra:tx_list:v1" "" in
  let h_a = {
    (mk_header ~creator:"v0" 0) with
    Octra_consensus.C_types.tx_list_hash = empty_tlh;
  } in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_fresh = mk_header 5 in
  C_engine.do_propose t h_fresh ["fresh_tx_hash"] ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function
      | C_engine.SendPropose p -> Some p
      | _ -> None
    ) outs
  in
  match send_propose with
  | Some p ->
    let pid = C_hash.proposal_id p.header in
    assert (pid = C_hash.proposal_id h_a);
    assert (p.valid_round = Some 0);
    assert (p.tx_hashes = [])
  | None -> failwith "no SendPropose emitted"

let test_multi_reproposal () =
  let t = make_engine four_validators ~my_addr:(leader_addr (make_engine four_validators) 1) in
  let empty_tlh = Octra_net.Hash_domain.hash "octra:tx_list:v1" "" in
  let h_a = { (mk_header 0) with Octra_consensus.C_types.tx_list_hash = empty_tlh } in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_fresh = mk_header 5 in
  C_engine.do_propose t h_fresh ["fresh_tx_hash"] ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function
      | C_engine.SendPropose p -> Some p
      | _ -> None
    ) outs
  in
  match send_propose with
  | Some p ->
    let pid = C_hash.proposal_id p.header in
    assert (pid = C_hash.proposal_id h_a);
    assert (p.valid_round = Some 0);
    assert (p.tx_hashes = [])
  | None -> failwith "no SendPropose emitted"

let test_single_hash_cache () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let tx1 = String.make 32 '\xa1' in
  let tx2 = String.make 32 '\xa2' in
  let agreed_hashes = [tx1; tx2] in
  let raw_to_hex s =
    String.concat "" (List.init (String.length s) (fun i ->
      Printf.sprintf "%02x" (Char.code s.[i])))
  in
  let agreed_tlh = Octra_net.Hash_domain.hash
    "octra:tx_list:v1"
    (String.concat "" (List.map raw_to_hex agreed_hashes)) in
  let h_a = {
    (mk_header ~creator:"v0" 0) with
    Octra_consensus.C_types.tx_list_hash = agreed_tlh;
  } in
  let p_a = mk_propose ~proposer:(leader_addr t 0) { h_a with tx_list_hash = agreed_tlh } in
  let p_a = { p_a with tx_hashes = agreed_hashes } in
  C_engine.on_propose t p_a ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  let pid = C_hash.proposal_id h_a in
  inject_prevote_quorum t 0 pid;
  drain_clear t;
  advance_to_round t 1;
  let h_fresh = mk_header 7 in
  let fresh_hashes = [String.make 32 '\xff'] in
  C_engine.do_propose t h_fresh fresh_hashes ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function C_engine.SendPropose p -> Some p | _ -> None) outs
  in
  match send_propose with
  | Some p ->
    assert (C_hash.proposal_id p.header = pid);
    assert (p.tx_hashes = agreed_hashes)
  | None -> failwith "no SendPropose emitted"

let test_single_cache_miss () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let h_a = mk_header ~creator:"v0" 0 in
  let pid = C_hash.proposal_id h_a in
  C_engine.cache_proposal_header_only t pid h_a;
  t.state <- { t.state with
    locked_round = 0;
    locked_value = Some h_a;
    valid_round = 0;
    valid_value = Some h_a;
  };
  advance_to_round t 1;
  let h_fresh = mk_header ~creator:"v0" 5 in
  C_engine.do_propose t h_fresh ["fresh"] ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function C_engine.SendPropose p -> Some p | _ -> None) outs
  in
  assert (send_propose = None)

let test_single_cache_hash () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let h_a = mk_header ~creator:"v0" 0 in
  let pid = C_hash.proposal_id h_a in
  let bogus_hashes = [String.make 32 '\x99'] in
  C_engine.cache_proposal_bundle
    t
    pid
    h_a
    bogus_hashes
    ~parent_commit:None;
  t.state <- { t.state with
    locked_round = 0;
    locked_value = Some h_a;
    valid_round = 0;
    valid_value = Some h_a;
  };
  advance_to_round t 1;
  let h_fresh = mk_header ~creator:"v0" 5 in
  C_engine.do_propose t h_fresh ["fresh"] ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function C_engine.SendPropose p -> Some p | _ -> None) outs
  in
  assert (send_propose = None)

let test_precommit_header_wait () =
  let t = make_engine four_validators ~my_addr:"v0" in
  drain_clear t;
  let h = mk_header 8 in
  let pid = C_hash.proposal_id h in
  List.iter (fun validator ->
    C_engine.on_vote t
      (mk_vote C_types.Precommit pid validator)
      ~sign_fn:dummy_sign
  ) ["v1"; "v2"; "v3"];
  let outs = drain t in
  let local_finalize =
    List.exists (function
      | C_engine.SendFinalize _ | C_engine.Finalized _ -> true
      | _ -> false
    ) outs
  in
  assert (not local_finalize);
  let accepted = C_engine.accept_finalize_batch t (mk_finalize ~commit_round:0 h) in
  assert accepted;
  let outs = drain t in
  let finalized =
    List.exists (function C_engine.Finalized _ -> true | _ -> false) outs
  in
  assert finalized

let test_reproposal_parent () =
  let t = make_engine four_validators ~my_addr:"v0" in
  drain_clear t;
  let parent_header = mk_header ~epoch_id:0L 2 in
  let parent_finalize = mk_finalize ~epoch_id:0L parent_header in
  let parent_commit = C_types.{
    certificate = certificate_of_finalize parent_finalize;
    validator_set = t.vs;
  } in
  let header = {
    (mk_header 9) with
    parent_commit_hash =
      C_hash.parent_commit_hash_opt (Some parent_commit);
  } in
  let proposal = {
    (mk_propose ~proposer:(leader_addr t 0) header) with
    parent_commit = Some parent_commit;
  } in
  let proposal_id = C_hash.proposal_id header in
  C_engine.cache_proposal_message t proposal;
  C_engine.start_round t 1;
  drain_clear t;
  List.iter
    (fun validator ->
      C_engine.on_vote
        t
        (mk_vote ~round:1 C_types.Precommit proposal_id validator)
        ~sign_fn:dummy_sign)
    ["v1"; "v2"; "v3"];
  let finalize =
    List.find_map
      (function
        | C_engine.Finalized { finalize; _ } -> Some finalize
        | _ -> None)
      (drain t)
  in
  match finalize with
  | Some value ->
    assert
      (C_hash.parent_commit_hash_opt value.parent_commit
       = header.parent_commit_hash)
  | None -> failwith "reproposal did not finalize"

let test_local_parent_required () =
  let t = make_engine four_validators ~my_addr:"v0" in
  drain_clear t;
  let parent_header = mk_header ~epoch_id:0L 3 in
  let parent_finalize = mk_finalize ~epoch_id:0L parent_header in
  let parent_commit = C_types.{
    certificate = certificate_of_finalize parent_finalize;
    validator_set = t.vs;
  } in
  let proposer = leader_addr t 0 in
  let header = {
    (mk_header ~creator:proposer 10) with
    parent_commit_hash =
      C_hash.parent_commit_hash_opt (Some parent_commit);
  } in
  let proposal_id = C_hash.proposal_id header in
  C_engine.cache_proposal_header_only t proposal_id header;
  List.iter
    (fun validator ->
      C_engine.on_vote
        t
        (mk_vote C_types.Precommit proposal_id validator)
        ~sign_fn:dummy_sign)
    ["v1"; "v2"; "v3"];
  let waiting = drain t in
  assert
    (not
       (List.exists
          (function
            | C_engine.SendFinalize _
            | C_engine.Finalized _ -> true
            | _ -> false)
          waiting));
  assert
    (List.exists
       (function
         | C_engine.RequestProposal { round = 0; proposal_id = value } ->
           value = proposal_id
         | _ -> false)
       waiting);
  let proposal = {
    (mk_propose ~proposer header) with
    parent_commit = Some parent_commit;
  } in
  C_engine.on_propose
    t
    proposal
    ~verify_fn:always_true
    ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  let finalize =
    List.find_map
      (function
        | C_engine.Finalized { finalize; _ } -> Some finalize
        | _ -> None)
      (drain t)
  in
  match finalize with
  | Some value ->
    assert
      (C_hash.parent_commit_hash_opt value.parent_commit
       = header.parent_commit_hash)
  | None -> failwith "parent commit recovery did not finalize"

let test_local_parent_mismatch () =
  let t = make_engine four_validators ~my_addr:"v0" in
  drain_clear t;
  let expected_header = mk_header ~epoch_id:0L 4 in
  let carried_header = mk_header ~epoch_id:0L 5 in
  let expected = C_types.{
    certificate = certificate_of_finalize (mk_finalize ~epoch_id:0L expected_header);
    validator_set = t.vs;
  } in
  let carried = C_types.{
    certificate = certificate_of_finalize (mk_finalize ~epoch_id:0L carried_header);
    validator_set = t.vs;
  } in
  let header = {
    (mk_header 11) with
    parent_commit_hash = C_hash.parent_commit_hash_opt (Some expected);
  } in
  let proposal_id = C_hash.proposal_id header in
  C_engine.cache_proposal_bundle
    t
    proposal_id
    header
    []
    ~parent_commit:(Some carried);
  List.iter
    (fun validator ->
      C_engine.on_vote
        t
        (mk_vote C_types.Precommit proposal_id validator)
        ~sign_fn:dummy_sign)
    ["v1"; "v2"; "v3"];
  let outputs = drain t in
  assert
    (not
       (List.exists
          (function
            | C_engine.SendFinalize _
            | C_engine.Finalized _ -> true
            | _ -> false)
          outputs));
  assert
    (List.exists
       (function
         | C_engine.RequestProposal { proposal_id = value; _ } ->
           value = proposal_id
         | _ -> false)
       outputs)

let test_nil_votes () =
  let validator_set = mk_vs four_validators in
  let votes = C_engine.create_vote_set () in
  let nil = Octra_net.Hash_domain.nil_hash in
  let add address =
    C_engine.add_vote votes (mk_vote C_types.Prevote nil address) ~validator_set
  in
  assert (add "outside" = `Rejected);
  assert (add "v0" = `Added);
  assert (add "v1" = `Added);
  assert (add "v1" = `Duplicate);
  assert (add "v2" = `QuorumOf nil);
  assert
    (C_engine.quorum_result votes ~chain_id:"octra-test" ~epoch_id:1L
       ~validator_set = `QuorumOf nil)

let check_nil_step t =
  assert (t.C_engine.state.step = C_types.PrecommitStep);
  let outputs = drain t in
  assert
    (List.filter_map
       (function
         | C_engine.SendVote (vote, _) when vote.vote_type = C_types.Precommit ->
           Some vote.proposal_id
         | _ -> None)
       outputs = [Octra_net.Hash_domain.nil_hash]);
  assert
    (List.exists
       (function
         | C_engine.ScheduleTimeout { step = C_types.PrecommitStep; _ } -> true
         | _ -> false)
       outputs);
  assert
    (not (List.exists
       (function C_engine.Finalized _ | C_engine.SendFinalize _ -> true | _ -> false)
       outputs));
  assert (not (Hashtbl.mem t.polc_by_round t.state.round))

let nil_vote t address =
  C_engine.on_vote t
    (mk_vote ~round:t.C_engine.state.round C_types.Prevote
       Octra_net.Hash_domain.nil_hash address)
    ~sign_fn:dummy_sign

let expire_step t =
  C_engine.on_timeout t ~step:t.C_engine.state.step ~round:t.state.round
    ~generation:t.generation ~sign_fn:dummy_sign

let test_nil_lock () =
  let t = make_engine four_validators in
  let header = mk_header ~creator:(leader_addr t 0) 3 in
  let _ = lock_on_round_0 t header in
  advance_to_round t 1;
  let lock = t.state.locked_value, t.state.locked_round in
  let valid = t.state.valid_value, t.state.valid_round in
  expire_step t;
  drain_clear t;
  nil_vote t "v1";
  nil_vote t "v1";
  assert (t.state.step = C_types.PrevoteStep);
  nil_vote t "v2";
  check_nil_step t;
  assert ((t.state.locked_value, t.state.locked_round) = lock);
  assert ((t.state.valid_value, t.state.valid_round) = valid);
  C_engine.on_propose t
    (mk_propose ~round:1 ~valid_round:(Some 0) ~proposer:(leader_addr t 1) header)
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  assert (t.state.step = C_types.PrecommitStep);
  assert (drain t = []);
  C_engine.on_timeout t ~step:C_types.PrevoteStep ~round:t.state.round
    ~generation:t.generation ~sign_fn:dummy_sign;
  assert (drain t = []);
  List.iter
    (fun address ->
      C_engine.on_vote t
        (mk_vote ~round:1 C_types.Precommit Octra_net.Hash_domain.nil_hash address)
        ~sign_fn:dummy_sign)
    ["v1"; "v2"];
  assert (drain t = []);
  expire_step t;
  assert (t.state.round = 2);
  assert ((t.state.locked_value, t.state.locked_round) = lock);
  drain_clear t;
  List.iter
    (fun kind ->
      C_engine.on_vote t
        (mk_vote ~round:1 kind Octra_net.Hash_domain.nil_hash "v3")
        ~sign_fn:dummy_sign)
    [C_types.Prevote; C_types.Precommit];
  assert (not (Hashtbl.mem t.polc_by_round 1));
  assert (drain t = [])

let test_nil_resume pending =
  let ready = ref true in
  let t =
    C_engine.create ~chain_id:"octra-test" ~my_addr:"v0"
      ~validator_set:(mk_vs four_validators) ~start_height:1L
      ~can_vote:(fun () -> !ready)
  in
  let header = mk_header ~creator:(leader_addr t 0) 3 in
  let _ = lock_on_round_0 t header in
  advance_to_round t 1;
  let lock = t.state.locked_value, t.state.locked_round in
  let valid = t.state.valid_value, t.state.valid_round in
  if not pending then expire_step t;
  ready := false;
  if pending then
    C_engine.on_propose t
      (mk_propose ~round:1 ~valid_round:(Some 0) ~proposer:(leader_addr t 1) header)
      ~verify_fn:always_true ~execute_fn:(fun _ -> false) ~sign_fn:dummy_sign;
  drain_clear t;
  List.iter (nil_vote t) ["v1"; "v2"; "v3"];
  assert (t.state.step =
    (if pending then C_types.ProposeStep else C_types.PrevoteStep));
  assert (Option.is_some t.pending_prevote = pending);
  ready := true;
  C_engine.on_ready t ~sign_fn:dummy_sign;
  assert (t.pending_prevote = None);
  assert
    ((Hashtbl.find t.prevotes.votes t.my_addr).proposal_id
     = Octra_net.Hash_domain.nil_hash);
  check_nil_step t;
  assert ((t.state.locked_value, t.state.locked_round) = lock);
  assert ((t.state.valid_value, t.state.valid_round) = valid);
  C_engine.on_ready t ~sign_fn:dummy_sign;
  assert (drain t = [])

let test_nil_ready () =
  let ready = ref false in
  let validator_set = mk_vs four_validators in
  let my_addr = (C_engine.leader_of validator_set ~epoch_id:1L ~round:0).address in
  let t =
    C_engine.create ~chain_id:"octra-test" ~my_addr ~validator_set
      ~start_height:1L ~can_vote:(fun () -> !ready)
  in
  drain_clear t;
  List.iter (nil_vote t) (List.filter ((<>) my_addr) four_validators);
  assert (t.state.step = C_types.ProposeStep);
  assert (t.pending_prevote = None);
  ready := true;
  List.iter
    (fun () ->
      C_engine.on_ready t ~sign_fn:dummy_sign;
      assert (t.state.step = C_types.ProposeStep);
      assert (not (Hashtbl.mem t.prevotes.votes my_addr));
      assert (not (Hashtbl.mem t.precommits.votes my_addr));
      assert (drain t = []))
    [(); ()];
  let header = { (mk_header ~creator:my_addr 2) with
    tx_list_hash = C_engine.tx_list_hash_for_header [];
  } in
  C_engine.do_propose t header [] ~sign_fn:dummy_sign;
  assert (t.state.step = C_types.PrevoteStep);
  let outputs = drain t in
  assert
    (List.filter_map
       (function C_engine.SendPropose proposal -> Some proposal.header | _ -> None)
       outputs = [header]);
  assert
    (Option.map (fun (vote : C_types.vote) -> vote.proposal_id) (last_prevote outputs)
     = Some (C_hash.proposal_id header));
  assert
    (not (List.exists
       (function
         | C_engine.SendVote (vote, _) -> vote.vote_type = C_types.Precommit
         | C_engine.Finalized _ | C_engine.SendFinalize _ -> true
         | _ -> false)
       outputs));
  C_engine.on_ready t ~sign_fn:dummy_sign;
  check_nil_step t;
  assert (t.state.locked_value = None && t.state.locked_round = -1);
  assert (t.state.valid_value = None && t.state.valid_round = -1)

let test_nil_timeout () =
  let t = make_engine four_validators in
  drain_clear t;
  List.iter (nil_vote t) ["v1"; "v2"];
  expire_step t;
  check_nil_step t

let test_nil_propose () =
  let t = make_engine four_validators in
  drain_clear t;
  List.iter (nil_vote t) ["v1"; "v2"; "v3"];
  let proposer = leader_addr t 0 in
  let proposal = mk_propose ~proposer (mk_header ~creator:proposer 2) in
  C_engine.on_propose t proposal ~verify_fn:always_true
    ~execute_fn:(fun _ -> false) ~sign_fn:dummy_sign;
  check_nil_step t

let test_value_timeout () =
  let ready = ref false in
  let t =
    C_engine.create ~chain_id:"octra-test" ~my_addr:"v0"
      ~validator_set:(mk_vs four_validators) ~start_height:1L
      ~can_vote:(fun () -> !ready)
  in
  let proposer = leader_addr t 0 in
  let header = { (mk_header ~creator:proposer 2) with
    tx_list_hash = C_engine.tx_list_hash_for_header [];
  } in
  let pid = C_hash.proposal_id header in
  C_engine.on_propose t (mk_propose ~proposer header)
    ~verify_fn:always_true ~execute_fn:always_exec ~sign_fn:dummy_sign;
  List.iter
    (fun address ->
      C_engine.on_vote t (mk_vote C_types.Prevote pid address) ~sign_fn:dummy_sign)
    ["v1"; "v2"; "v3"];
  ready := true;
  drain_clear t;
  expire_step t;
  assert (t.state.step = C_types.PrevoteStep);
  assert (t.state.locked_value = None);
  assert
    (not (List.exists
       (function
         | C_engine.SendVote (vote, _) -> vote.vote_type = C_types.Precommit
         | _ -> false)
       (drain t)));
  C_engine.on_ready t ~sign_fn:dummy_sign;
  assert (t.state.step = C_types.PrecommitStep);
  assert (t.state.locked_value = Some header)

let test_split_timer () =
  let t = make_engine four_validators in
  expire_step t;
  drain_clear t;
  List.iter
    (fun (address, tag) ->
      C_engine.on_vote t
        (mk_vote C_types.Prevote (String.make 32 tag) address)
        ~sign_fn:dummy_sign)
    ["v1", 'a'; "v2", 'b'];
  assert (t.state.step = C_types.PrevoteStep);
  let timers =
    List.filter_map
      (function C_engine.ScheduleTimeout { step; _ } -> Some step | _ -> None)
      (drain t)
  in
  assert (timers = [C_types.PrevoteStep]);
  expire_step t;
  check_nil_step t

let local_lock () =
  let probe = make_engine four_validators in
  let t = make_engine ~my_addr:(leader_addr probe 1) four_validators in
  let header = { (mk_header ~creator:(leader_addr t 0) 4) with
    tx_list_hash = C_engine.tx_list_hash_for_header [];
  } in
  ignore (lock_on_round_0 t header);
  advance_to_round t 1;
  t, header

let test_local_prevote_lock nil_votes =
  let t, header = local_lock () in
  t.state <- {t.state with valid_round = -1; valid_value = None};
  if nil_votes then
    List.iter (nil_vote t) (List.filter ((<>) t.my_addr) four_validators);
  let fresh = {header with creator_addr = t.my_addr;
    proposed_state_root = String.make 32 'z'} in
  C_engine.do_propose t fresh [] ~sign_fn:dummy_sign;
  let outputs = drain t in
  assert (Option.map (fun (v : C_types.vote) -> v.proposal_id)
    (last_prevote outputs) = Some Octra_net.Hash_domain.nil_hash);
  assert (t.state.locked_value = Some header && t.state.locked_round = 0);
  assert (not (List.exists (function
    | C_engine.SendVote (v, _) -> v.vote_type = C_types.Precommit
      && not (Octra_net.Hash_domain.is_nil v.proposal_id)
    | C_engine.SendFinalize _ | C_engine.Finalized _ -> true
    | _ -> false) outputs))

let test_pending_lock () =
  let t, header = local_lock () in
  let other = {header with proposed_state_root = String.make 32 'z'} in
  let pid = C_hash.proposal_id other in
  C_engine.cache_proposal_message t (mk_propose ~round:1 other);
  t.pending_prevote <- Some (1L, 1, pid);
  C_engine.on_ready t ~sign_fn:dummy_sign;
  assert (Option.map (fun (v : C_types.vote) -> v.proposal_id)
    (last_prevote (drain t)) = Some Octra_net.Hash_domain.nil_hash)

let test_same_height_lock () =
  let t, header = local_lock () in
  C_engine.start_height t t.state.height;
  assert (t.state.locked_value = Some header && t.state.locked_round = 0);
  assert (t.state.valid_value = Some header && t.state.valid_round >= 0);
  assert (t.state.round >= 1);
  assert (C_engine.polc_matches t ~round:0 ~proposal_id:(C_hash.proposal_id header));
  assert (Hashtbl.length t.prevotes.votes = 0);
  assert (Hashtbl.length t.precommits.votes = 0);
  assert (C_engine.find_tx_hashes t (C_hash.proposal_id header) = Some []);
  C_engine.start_height t 2L;
  assert (t.state.locked_round = -1 && t.state.valid_round = -1)

let test_same_height_polc () =
  let t, header = local_lock () in
  let other = {header with creator_addr = leader_addr t 1;
    proposed_state_root = String.make 32 'z'} in
  let pid = C_hash.proposal_id other in
  C_engine.cache_proposal_message t
    (mk_propose ~round:1 ~proposer:(leader_addr t 1) other);
  C_engine.start_round t 2;
  drain_clear t;
  List.iter (fun addr -> C_engine.on_vote t
    (mk_vote ~round:1 C_types.Prevote pid addr) ~sign_fn:dummy_sign)
    (List.filter ((<>) t.my_addr) four_validators);
  assert (C_engine.polc_matches t ~round:1 ~proposal_id:pid);
  C_engine.start_height t t.state.height;
  assert (C_engine.polc_matches t ~round:1 ~proposal_id:pid);
  assert (List.length (C_engine.polc_votes_for_round t 1) = 3);
  let proposal = mk_propose ~round:t.state.round ~valid_round:(Some 1)
    ~proposer:(leader_addr t t.state.round) other in
  C_engine.on_propose t proposal ~verify_fn:always_true ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  assert (Option.map (fun (v : C_types.vote) -> v.proposal_id)
    (last_prevote (drain t)) = Some pid)

let test_old_polc_gate () =
  let module P = Octra_consensus.C_quorum_policy in
  let chain = "octra-devnet-9871-cluster" in
  let activation = Option.get (P.linear_epoch ~chain_id:chain) in
  List.iter (fun epoch ->
    let original = make_engine four_validators in
    let node = C_engine.create ~chain_id:chain ~my_addr:"v0"
      ~validator_set:original.vs ~start_height:epoch ~can_vote:(fun () -> true) in
    let header = mk_header ~chain_id:chain ~epoch_id:epoch 9 in
    let pid = C_hash.proposal_id header in
    node.state <- {node.state with round = 100};
    Hashtbl.replace node.polc_by_round 0 pid;
    C_engine.record_historical_prevote node (mk_vote ~chain_id:chain ~epoch_id:epoch
      C_types.Prevote pid "v1");
    assert (C_engine.historical_prevote_known node ~round:0 ~validator:"v1"
      = (epoch >= activation));
    C_engine.record_historical_prevote node (mk_vote ~chain_id:chain ~epoch_id:epoch
      C_types.Prevote (String.make 32 'x') "v2");
    assert (not (C_engine.historical_prevote_known node ~round:0 ~validator:"v2"));
    C_engine.record_historical_prevote node (mk_vote ~chain_id:chain ~epoch_id:epoch
      ~round:99 C_types.Prevote pid "v2");
    assert (C_engine.historical_prevote_known node ~round:99 ~validator:"v2")
  ) [Int64.pred activation; activation; Int64.succ activation]

let test_same_height_vote () =
  let t = make_engine four_validators in
  expire_step t;
  assert (Hashtbl.mem t.prevotes.votes t.my_addr);
  C_engine.start_height t 1L;
  assert (t.state.round > 0);
  assert (t.state.locked_round = -1)

let test_set_projects_votes remove_voter =
  let t, header = local_lock () in
  let pid = C_hash.proposal_id header in
  let votes = C_engine.polc_votes_for_round t 0 in
  let voted = List.map (fun (vote : C_types.vote) -> vote.validator) votes in
  let removed = if remove_voter then
    List.find (fun addr -> addr <> t.my_addr) voted
  else List.find (fun addr -> not (List.mem addr voted)) four_validators in
  let next = mk_vs (List.filter ((<>) removed) four_validators) in
  C_engine.replace_validator_set t next;
  assert (t.prevotes == Hashtbl.find t.prevotes_by_round t.state.round);
  assert (t.precommits == Hashtbl.find t.precommits_by_round t.state.round);
  assert (not (Hashtbl.mem t.prevotes.votes removed));
  assert (not (Hashtbl.mem t.precommits.votes removed));
  C_engine.start_height t t.state.height;
  assert (C_engine.polc_matches t ~round:0 ~proposal_id:pid = not remove_voter);
  assert (t.state.locked_value = Some header);
  assert (t.state.valid_round >= t.state.locked_round)

let test_valid_lock_floor () =
  let t, header = local_lock () in
  t.state <- {t.state with valid_round = -1; valid_value = None};
  let other = {header with proposed_state_root = String.make 32 'z'} in
  let pid = C_hash.proposal_id other in
  C_engine.cache_proposal_header_only t pid other;
  C_engine.promote_known_header t 0 pid;
  assert (t.state.valid_value <> Some other)

let test_relief_floor remove_creator =
  let chain = "octra-devnet-9871-cluster" in
  let height = 1_600_000L in
  let entries = List.init 7 (fun i ->
    C_types.{address = "member" ^ string_of_int i;
      pubkey = String.make 32 (Char.chr (i + 1))}, Z.one) in
  let full = Result.get_ok (C_types.make_weighted_validator_set entries) in
  let creator = (C_types.leader_of full ~epoch_id:height ~round:0).address in
  let removed = if remove_creator then creator else
    (List.find (fun (v, _) -> v.C_types.address <> creator) entries |> fst).address in
  let next = Result.get_ok (C_types.make_weighted_validator_set
    (List.filter (fun (v, _) -> v.C_types.address <> removed) entries)) in
  assert (full.n = 7 && next.n = 6 && full.quorum = 5 && next.quorum = 5);
  let my_addr = (C_types.leader_of next ~epoch_id:height ~round:64).address in
  let t = C_engine.create ~chain_id:chain ~my_addr ~validator_set:full
    ~start_height:height ~can_vote:(fun () -> true) in
  let header = { (mk_header ~chain_id:chain ~epoch_id:height ~creator 4) with
    tx_list_hash = C_engine.tx_list_hash_for_header [];
  } in
  C_engine.on_propose t (mk_propose ~chain_id:chain ~epoch_id:height
    ~proposer:creator header) ~verify_fn:always_true ~execute_fn:always_exec
    ~sign_fn:dummy_sign;
  let pid = C_hash.proposal_id header in
  let voters = my_addr :: removed ::
    (List.map (fun (v, _) -> v.C_types.address) entries
      |> List.filter (fun addr -> addr <> my_addr && addr <> removed)
      |> List.filteri (fun i _ -> i < 3)) in
  List.iter (fun addr -> C_engine.on_vote t
    (mk_vote ~chain_id:chain ~epoch_id:height C_types.Prevote pid addr)
    ~sign_fn:dummy_sign) voters;
  assert (t.state.locked_round = 0);
  C_engine.reconfigure_validator_set t ~round:64 next;
  assert (not (Hashtbl.mem t.polc_by_round 0));
  assert (t.state.valid_round = 0 && t.state.valid_value = Some header);
  drain_clear t;
  let fresh = {header with creator_addr = my_addr;
    proposed_state_root = String.make 32 'z'} in
  C_engine.do_propose t fresh [] ~sign_fn:dummy_sign;
  let outputs = drain t in
  let sent = List.find_map (function
    | C_engine.SendPropose proposal -> Some proposal
    | _ -> None) outputs |> Option.get in
  assert (C_types.proposal_is_well_formed ~chain_id:chain ~validator_set:next sent);
  assert (Option.map (fun (v : C_types.vote) -> v.proposal_id)
    (last_prevote outputs) = Some (if remove_creator then
      Octra_net.Hash_domain.nil_hash else pid))

let test_finalize_valid_floor () =
  let t, header = local_lock () in
  t.state <- {t.state with locked_round = 4; valid_round = 4};
  assert (C_engine.accept_finalize_batch t (mk_finalize header));
  assert (t.state.valid_round = t.state.locked_round)

let test_lock_group reset remove_creator =
  let chain = "octra-devnet-9871-cluster" in
  let height = 1_600_000L in
  let entries = List.init 7 (fun i ->
    C_types.{address = "member" ^ string_of_int i;
      pubkey = String.make 32 (Char.chr (i + 1))}, Z.one) in
  let full = Result.get_ok (C_types.make_weighted_validator_set entries) in
  let creator = (C_types.leader_of full ~epoch_id:height ~round:0).address in
  let removed = if remove_creator then creator else
    (List.find (fun (v, _) -> v.C_types.address <> creator) entries |> fst).address in
  let next = if reset then full else
    Result.get_ok (C_types.make_weighted_validator_set
      (List.filter (fun (v, _) -> v.C_types.address <> removed) entries)) in
  let nodes = List.map (fun (member, _) ->
    C_engine.create ~chain_id:chain ~my_addr:member.C_types.address
      ~validator_set:full ~start_height:height ~can_vote:(fun () -> true)) entries in
  let header = { (mk_header ~chain_id:chain ~epoch_id:height ~creator 4) with
    tx_list_hash = C_engine.tx_list_hash_for_header [];
  } in
  let pid = C_hash.proposal_id header in
  let proposal = mk_propose ~chain_id:chain ~epoch_id:height ~proposer:creator header in
  let votes = List.map (fun node ->
    C_engine.on_propose node proposal ~verify_fn:always_true ~execute_fn:always_exec
      ~sign_fn:dummy_sign;
    Option.get (last_prevote (drain node))) nodes in
  let selected = List.find (fun (v : C_types.vote) -> v.validator = removed) votes ::
    (List.filter (fun (v : C_types.vote) -> v.validator <> removed) votes
      |> List.filteri (fun i _ -> i < 4)) in
  List.iter (fun node ->
    List.iter (fun vote -> C_engine.on_vote node vote ~sign_fn:dummy_sign) selected;
    assert (node.C_engine.state.locked_value = Some header);
    drain_clear node;
    if reset then C_engine.start_height node height
    else C_engine.reconfigure_validator_set node ~round:64 next;
    C_engine.start_round node 64;
    drain_clear node) nodes;
  let active = List.filter (fun node -> C_types.is_validator next node.C_engine.my_addr) nodes in
  let leader = (C_types.leader_of next ~epoch_id:height ~round:64).address in
  let sender = List.find (fun node -> node.C_engine.my_addr = leader) active in
  let fresh = {header with creator_addr = leader; proposed_state_root = String.make 32 'z'} in
  C_engine.do_propose sender fresh [] ~sign_fn:dummy_sign;
  let finals = ref [] in
  let rec deliver fuel outputs =
    assert (fuel > 0);
    List.iter (function
      | C_engine.SendPropose sent ->
        assert (C_types.proposal_is_well_formed ~chain_id:chain ~validator_set:next sent);
        List.iter (fun node -> if node.C_engine.my_addr <> sent.proposer then
          C_engine.on_propose node sent ~verify_fn:always_true ~execute_fn:always_exec
            ~sign_fn:dummy_sign) active
      | C_engine.SendVote (vote, _) ->
        assert (Octra_net.Hash_domain.is_nil vote.proposal_id || vote.proposal_id = pid);
        List.iter (fun node -> C_engine.on_vote node vote ~sign_fn:dummy_sign) active
      | C_engine.Finalized {finalize; _} -> finals := finalize.proposal_id :: !finals
      | _ -> ()) outputs;
    let more = List.concat_map (fun node ->
      drain node |> List.filter (function
        | C_engine.Finalized {finalize; _} ->
          finals := finalize.proposal_id :: !finals; false
        | C_engine.SendPropose _ | C_engine.SendVote _ -> true
        | _ -> false)) active in
    if more <> [] then deliver (fuel - 1) more in
  deliver 100 (drain sender);
  if reset || not remove_creator then assert (List.mem pid !finals)
  else assert (!finals = []);
  assert (List.for_all (fun done_ -> done_ = pid) !finals)

let () =
  test_local_prevote_lock false;
  test_local_prevote_lock true;
  test_pending_lock ();
  test_same_height_lock ();
  test_same_height_polc ();
  test_old_polc_gate ();
  test_same_height_vote ();
  test_set_projects_votes false;
  test_set_projects_votes true;
  test_valid_lock_floor ();
  test_relief_floor false;
  test_relief_floor true;
  test_finalize_valid_floor ();
  test_lock_group true false;
  test_lock_group false false;
  test_lock_group false true;
  test_nil_votes ();
  test_nil_lock ();
  test_nil_resume false;
  test_nil_resume true;
  test_nil_ready ();
  test_nil_timeout ();
  test_nil_propose ();
  test_value_timeout ();
  test_split_timer ();
  test_no_lock_accepts_anything ();
  test_lock_conflict ();
  test_lock_same_value ();
  test_valid_round_prior ();
  test_unlock_with_polc ();
  test_polc_other_value ();
  test_polc_missing_round ();
  test_polc_locked_round ();
  test_timeout_lock ();
  test_historical_polc_recovery ();
  test_polc_request_retry ();
  test_polc_storage_limit ();
  test_lock_conflict_cache ();
  test_vote_quorum_pid ();
  test_finalize_lower_lock ();
  test_finalize_higher_lock ();
  test_polc_reset_on_height_change ();
  test_single_reproposal ();
  test_multi_reproposal ();
  test_single_hash_cache ();
  test_single_cache_miss ();
  test_single_cache_hash ();
  test_precommit_header_wait ();
  test_reproposal_parent ();
  test_local_parent_required ();
  test_local_parent_mismatch ();
  Printf.printf "status = pass test = lock_rule\n%!"