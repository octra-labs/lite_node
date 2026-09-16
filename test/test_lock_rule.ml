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

let test_lock_blocks_conflicting_proposal () =
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

let test_lock_accepts_same_locked_value () =
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

let test_rejects_non_prior_valid_round () =
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

let test_no_unlock_when_polc_for_other_value () =
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

let test_no_unlock_without_valid_round () =
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

let test_no_unlock_when_valid_round_le_locked () =
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

let test_round_timeout_preserves_lock () =
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

let test_historical_polc_request_retries () =
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

let test_historical_polc_storage_is_bounded () =
  let t = make_engine four_validators in
  drain_clear t;
  for round = 1 to 200 do
    C_engine.start_round t round;
    drain_clear t
  done;
  assert (Hashtbl.length t.polc_requests = C_engine.round_history_limit);
  assert
    (Hashtbl.length t.prevotes_by_round <= C_engine.round_history_limit + 1)

let test_lock_conflict_caches_value_until_polc () =
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

let test_add_vote_reports_exact_quorum_pid () =
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

let test_accept_finalize_batch_rejects_conflicting_lower_round_lock () =
  let t = make_engine four_validators in
  let h_a = mk_header 0 in
  let _ = lock_on_round_0 t h_a in
  advance_to_round t 1;
  let h_b = mk_header 1 in
  let accepted = C_engine.accept_finalize_batch t (mk_finalize ~commit_round:0 h_b) in
  assert (not accepted);
  assert (drain t = [])

let test_accept_finalize_batch_accepts_conflicting_higher_round_lock () =
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

let test_make_proposal_re_proposes_valid_value_single_validator () =
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

let test_multi_validator_re_proposes_valid_value () =
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

let test_re_propose_uses_cached_tx_hashes_single_validator () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let tx1 = String.make 32 '\xa1' in
  let tx2 = String.make 32 '\xa2' in
  let canonical_hashes = [tx1; tx2] in
  let raw_to_hex s =
    String.concat "" (List.init (String.length s) (fun i ->
      Printf.sprintf "%02x" (Char.code s.[i])))
  in
  let canonical_tlh = Octra_net.Hash_domain.hash
    "octra:tx_list:v1"
    (String.concat "" (List.map raw_to_hex canonical_hashes)) in
  let h_a = {
    (mk_header ~creator:"v0" 0) with
    Octra_consensus.C_types.tx_list_hash = canonical_tlh;
  } in
  let p_a = mk_propose ~proposer:(leader_addr t 0) { h_a with tx_list_hash = canonical_tlh } in
  let p_a = { p_a with tx_hashes = canonical_hashes } in
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
    assert (p.tx_hashes = canonical_hashes)
  | None -> failwith "no SendPropose emitted"

let test_re_propose_refuses_when_cache_miss_single_validator () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let h_a = mk_header 0 in
  let pid = C_hash.proposal_id h_a in
  C_engine.cache_proposal_header_only t pid h_a;
  t.state <- { t.state with
    locked_round = 0;
    locked_value = Some h_a;
    valid_round = 0;
    valid_value = Some h_a;
  };
  advance_to_round t 1;
  let h_fresh = mk_header 5 in
  C_engine.do_propose t h_fresh ["fresh"] ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function C_engine.SendPropose p -> Some p | _ -> None) outs
  in
  assert (send_propose = None)

let test_re_propose_refuses_when_cached_hashes_dont_match_tlh_single_validator () =
  let t = make_engine one_validator ~my_addr:"v0" in
  let h_a = mk_header 0 in
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
  let h_fresh = mk_header 5 in
  C_engine.do_propose t h_fresh ["fresh"] ~sign_fn:dummy_sign;
  let outs = drain t in
  let send_propose =
    List.find_map (function C_engine.SendPropose p -> Some p | _ -> None) outs
  in
  assert (send_propose = None)

let test_precommit_quorum_without_header_waits_for_finalize_batch () =
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

let test_reproposal_finalize_keeps_parent_commit () =
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

let test_parent_commit_required_before_local_finalize () =
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

let test_parent_commit_mismatch_blocks_local_finalize () =
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

let () =
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
  test_lock_blocks_conflicting_proposal ();
  test_lock_accepts_same_locked_value ();
  test_rejects_non_prior_valid_round ();
  test_unlock_with_polc ();
  test_no_unlock_when_polc_for_other_value ();
  test_no_unlock_without_valid_round ();
  test_no_unlock_when_valid_round_le_locked ();
  test_round_timeout_preserves_lock ();
  test_historical_polc_recovery ();
  test_historical_polc_request_retries ();
  test_historical_polc_storage_is_bounded ();
  test_lock_conflict_caches_value_until_polc ();
  test_add_vote_reports_exact_quorum_pid ();
  test_accept_finalize_batch_rejects_conflicting_lower_round_lock ();
  test_accept_finalize_batch_accepts_conflicting_higher_round_lock ();
  test_polc_reset_on_height_change ();
  test_make_proposal_re_proposes_valid_value_single_validator ();
  test_multi_validator_re_proposes_valid_value ();
  test_re_propose_uses_cached_tx_hashes_single_validator ();
  test_re_propose_refuses_when_cache_miss_single_validator ();
  test_re_propose_refuses_when_cached_hashes_dont_match_tlh_single_validator ();
  test_precommit_quorum_without_header_waits_for_finalize_batch ();
  test_reproposal_finalize_keeps_parent_commit ();
  test_parent_commit_required_before_local_finalize ();
  test_parent_commit_mismatch_blocks_local_finalize ();
  Printf.printf "status = pass test = lock_rule\n%!"