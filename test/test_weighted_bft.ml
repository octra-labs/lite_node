(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus

let expect label value =
  if not value then failwith label

let validator address key =
  C_types.{ address; pubkey = String.make 32 key }

let alice = validator "alice" 'a'
let bob = validator "bob" 'b'
let carol = validator "carol" 'c'
let dave = validator "dave" 'd'

let validator_set =
  match C_types.make_weighted_validator_set [
    alice, Z.of_int 4;
    bob, Z.of_int 3;
    carol, Z.of_int 2;
    dave, Z.of_int 1;
  ] with
  | Ok value -> value
  | Error error -> failwith error

let header epoch =
  C_types.{
    proto_version = proto_version_current;
    chain_id = "weighted-bft-test";
    epoch_id = epoch;
    prev_state_root = String.make 32 '\x01';
    tx_list_hash = C_engine.tx_list_hash_for_header [];
    receipt_root = C_hash.receipt_root [];
    proposed_state_root = String.make 32 '\x02';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "alice";
    txid_hi = 1L;
    ts = 0.0;
  }

let vote epoch round proposal_id address =
  C_types.{
    chain_id = "weighted-bft-test";
    epoch_id = epoch;
    round;
    vote_type = Precommit;
    proposal_id;
    validator = address;
    signature = String.make 64 '\x00';
  }

let finalize signers =
  let epoch = 7L in
  let round = 2 in
  let header = header epoch in
  let proposal_id = C_hash.proposal_id header in
  C_types.{
    chain_id = "weighted-bft-test";
    epoch_id = epoch;
    commit_round = round;
    header;
    proposal_id;
    precommits =
      List.map
        (vote epoch round proposal_id)
        signers;
    parent_commit = None;
  }

let test_threshold () =
  expect "weighted flag" validator_set.weighted;
  expect "weighted total"
    Z.(equal validator_set.total_weight (of_int 10));
  expect "weighted quorum"
    Z.(equal validator_set.quorum_weight (of_int 7));
  expect "two heavy signers reach quorum"
    (C_types.has_weight_quorum validator_set ["alice"; "bob"]);
  expect "two heavy signers do not satisfy dual quorum"
    (not (C_types.has_quorum validator_set ["alice"; "bob"]));
  expect "three light signers do not reach quorum"
    (not (C_types.has_quorum validator_set ["bob"; "carol"; "dave"]));
  expect "duplicate signer does not add weight"
    (C_types.signed_weight validator_set ["alice"; "alice"; "bob"]
     = Some (Z.of_int 7));
  expect "unknown signer rejected"
    (C_types.signed_weight validator_set ["alice"; "mallory"] = None)

let devnet_chain_id = "octra-devnet-9871-cluster"

let quorum_activation () =
  match C_quorum_policy.activation_for_chain devnet_chain_id with
  | Some value -> value
  | None -> failwith "validator quorum activation missing"

let live_validator index weight =
  validator ("live-" ^ string_of_int index) (Char.chr (96 + index)), Z.of_int weight

let live_validator_set =
  match C_types.make_weighted_validator_set [
    live_validator 1 21_000_000;
    live_validator 2 2_000_000;
    live_validator 3 1_000_000;
    live_validator 4 1_000_000;
    live_validator 5 1_000_000;
    live_validator 6 1_000_000;
    live_validator 7 1_000_000;
  ] with
  | Ok value -> value
  | Error error -> failwith error

let catchup_record epoch_id =
  C_codec.{
    epoch_id;
    prev_state_root = String.make 32 '\x10';
    state_root = String.make 32 '\x11';
    tx_list_hash = C_engine.tx_list_hash_for_header [];
    tx_hashes = [];
    txs_json = [];
    receipt_root = C_hash.receipt_root [];
    receipts_json = [];
    epoch_ts = 0.0;
    creator_addr = "live-2";
    commit_round = 0;
    reward_source = None;
    finality = None;
  }

let rec choose count values =
  if count = 0 then [[]]
  else
    match values with
    | [] -> []
    | value :: rest ->
      List.map (fun selected -> value :: selected) (choose (count - 1) rest)
      @ choose count rest

let test_live_activation () =
  let activation = quorum_activation () in
  let before = Int64.of_int (activation.activation_epoch - 1) in
  let boundary = Int64.of_int activation.activation_epoch in
  let addresses = List.map (fun v -> v.C_types.address) live_validator_set.validators in
  let heavy = List.hd addresses in
  expect "legacy heavy signer retained before activation"
    (C_types.has_quorum_at
       ~chain_id:devnet_chain_id
       ~epoch_id:before
       live_validator_set
       [heavy]);
  let prior =
    C_types.validator_set_for_epoch
      ~chain_id:devnet_chain_id
      ~epoch_id:before
      live_validator_set
  in
  expect "validator set changed before activation"
    (C_config.validator_set_hash prior
     = C_config.validator_set_hash live_validator_set);
  let effective =
    C_types.validator_set_for_epoch
      ~chain_id:devnet_chain_id
      ~epoch_id:boundary
      live_validator_set
  in
  expect "live cap"
    Z.(equal (C_types.count_weight_cap live_validator_set) (of_int 1_249_999));
  expect "live effective total"
    Z.(equal effective.total_weight (of_int 7_499_998));
  expect "live effective quorum weight"
    Z.(equal effective.quorum_weight (of_int 4_999_999));
  expect "heavy signer rejected at activation"
    (not
       (C_types.has_quorum_at
          ~chain_id:devnet_chain_id
          ~epoch_id:boundary
          live_validator_set
          [heavy]));
  let without_heavy = List.tl addresses in
  expect "six remaining validators reach quorum"
    (C_types.has_quorum_at
       ~chain_id:devnet_chain_id
       ~epoch_id:boundary
       live_validator_set
       without_heavy);
  List.iter
    (fun signers ->
      expect "every n-f signer set reaches quorum"
        (C_types.has_quorum_at
           ~chain_id:devnet_chain_id
           ~epoch_id:boundary
           live_validator_set
           signers))
    (choose effective.quorum addresses);
  List.iter
    (fun signers ->
      expect "sub-quorum signer count rejected"
        (not
           (C_types.has_quorum_at
              ~chain_id:devnet_chain_id
              ~epoch_id:boundary
              live_validator_set
              signers)))
    (choose (effective.quorum - 1) addresses);
  let top_two = [List.nth addresses 0; List.nth addresses 1] in
  let top_two_weight =
    match C_types.signed_weight effective top_two with
    | Some value -> value
    | None -> failwith "top-two weight missing"
  in
  expect "f validators cannot skip a round"
    (not
       (C_types.round_skip_reached_at
          ~chain_id:devnet_chain_id
          ~epoch_id:boundary
          effective
          ~signer_count:2
          ~signed_weight:top_two_weight));
  expect "f+1 validators can skip a round"
    (C_types.round_skip_reached_at
       ~chain_id:devnet_chain_id
       ~epoch_id:boundary
       effective
       ~signer_count:3
       ~signed_weight:Z.(add top_two_weight (of_int 1_000_000)));
  let second = C_types.count_capped_set effective in
  expect "effective transformation idempotent"
    (C_config.validator_set_hash second = C_config.validator_set_hash effective);
  let finalize epoch signers =
    let header = C_types.{
      proto_version = C_protocol.version_for_epoch epoch;
      chain_id = devnet_chain_id;
      epoch_id = epoch;
      prev_state_root = String.make 32 '\x01';
      tx_list_hash = C_engine.tx_list_hash_for_header [];
      receipt_root = C_hash.receipt_root [];
      proposed_state_root = String.make 32 '\x02';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = heavy;
      txid_hi = 1L;
      ts = 0.0;
    } in
    let proposal_id = C_hash.proposal_id header in
    C_types.{
      chain_id = devnet_chain_id;
      epoch_id = epoch;
      commit_round = 0;
      header;
      proposal_id;
      precommits =
        List.map
          (fun address -> C_types.{
             chain_id = devnet_chain_id;
             epoch_id = epoch;
             round = 0;
             vote_type = Precommit;
             proposal_id;
             validator = address;
             signature = String.make 64 '\x00';
           })
          signers;
      parent_commit = None;
    }
  in
  let validate set epoch signers =
    C_qc.validate_finalize
      ~chain_id:devnet_chain_id
      ~validator_set:set
      ~verify_vote:(fun _ -> true)
      (finalize epoch signers)
  in
  expect "legacy single-signer certificate retained before activation"
    (validate live_validator_set before [heavy] = C_qc.Valid);
  expect "single-signer certificate rejected at activation"
    (validate live_validator_set boundary [heavy] = C_qc.Invalid "quorum");
  expect "raw validator set derives the activation policy for qc"
    (validate live_validator_set boundary (List.tl (List.tl addresses))
     = C_qc.Valid);
  expect "effective validator set remains accepted at activation"
    (validate effective boundary (List.tl (List.tl addresses)) = C_qc.Valid);
  let votes = C_engine.create_vote_set () in
  let proposal_id = String.make 32 '\x31' in
  let add address =
    C_engine.add_vote
      votes
      C_types.{
        chain_id = devnet_chain_id;
        epoch_id = boundary;
        round = 0;
        vote_type = Precommit;
        proposal_id;
        validator = address;
        signature = String.make 64 '\x00';
      }
      ~validator_set:effective
  in
  let lowest_five = List.tl (List.tl addresses) in
  List.iteri
    (fun index address ->
      let result = add address in
      if index < 4 then
        expect "engine waited for n-f signers" (result = `Added)
      else
        expect "engine reached dual quorum" (result = `QuorumOf proposal_id))
    lowest_five

let test_catchup_activation_boundary () =
  let activation = quorum_activation () in
  let boundary = Int64.of_int activation.activation_epoch in
  let before = Int64.pred boundary in
  let from_epoch = Int64.pred before in
  let records = [
    catchup_record from_epoch;
    catchup_record before;
    catchup_record boundary;
  ] in
  expect "catchup agreement follows last proved epoch"
    (C_driver.catchup_agreement_epoch ~from_epoch records = boundary);
  let responders =
    live_validator_set.validators
    |> List.tl
    |> List.map (fun validator -> validator.C_types.address)
  in
  let prior_weight =
    match C_types.signed_weight live_validator_set responders with
    | Some value -> value
    | None -> failwith "pre-activation responder weight missing"
  in
  expect "pre-activation responder weight is insufficient"
    (not
       (C_types.round_skip_reached_at
          ~chain_id:devnet_chain_id
          ~epoch_id:before
          live_validator_set
          ~signer_count:(List.length responders)
          ~signed_weight:prior_weight));
  let transport =
    C_types.validator_set_for_epoch
      ~chain_id:devnet_chain_id
      ~epoch_id:boundary
      live_validator_set
  in
  let transport_weight =
    match C_types.signed_weight transport responders with
    | Some value -> value
    | None -> failwith "catchup responder weight missing"
  in
  expect "catchup follows proved activation"
    (C_driver.catchup_source_agreement_reached
       transport
       ~signer_count:(List.length responders)
       ~signed_weight:transport_weight);
  let effective =
    C_types.validator_set_for_epoch
      ~chain_id:devnet_chain_id
      ~epoch_id:boundary
      live_validator_set
  in
  let effective_weight =
    match C_types.signed_weight effective responders with
    | Some value -> value
    | None -> failwith "post-activation responder weight missing"
  in
  expect "proved activation enables responder agreement"
    (C_types.round_skip_reached_at
       ~chain_id:devnet_chain_id
       ~epoch_id:boundary
       effective
       ~signer_count:(List.length responders)
       ~signed_weight:effective_weight)

let test_genesis_quorum () =
  let chain_id = "new-private-network" in
  let epoch_id = 0L in
  expect "new chain has no historical quorum anchor"
    (C_quorum_policy.activation_for_chain chain_id = None);
  expect "new chain protected at genesis"
    (C_quorum_policy.active ~chain_id ~epoch_id);
  let effective =
    C_types.validator_set_for_epoch
      ~chain_id
      ~epoch_id
      live_validator_set
  in
  let addresses =
    List.map (fun value -> value.C_types.address) effective.validators
  in
  expect "new chain rejects one heavy signer"
    (not
       (C_types.has_quorum_at
          ~chain_id
          ~epoch_id
          live_validator_set
          [List.hd addresses]));
  List.iter
    (fun signers ->
      expect "new chain accepts every n-f signer set"
        (C_types.has_quorum_at
           ~chain_id
           ~epoch_id
           live_validator_set
           signers))
    (choose effective.quorum addresses);
  expect "new chain cannot rewind before protected genesis"
    (not
       (C_quorum_policy.rewind_allowed
          ~chain_id
          ~from_epoch:0L
          ~to_epoch:(-1L)))

let test_small_distributions () =
  let rec distributions count prefix =
    if count = 0 then [List.rev prefix]
    else
      List.concat_map
        (fun weight -> distributions (count - 1) (weight :: prefix))
        [1; 2; 3; 4]
  in
  for count = 4 to 7 do
    List.iter
      (fun weights ->
        let entries =
          List.mapi
            (fun index weight -> live_validator (index + 1) weight)
            weights
        in
        let original =
          match C_types.make_weighted_validator_set entries with
          | Ok value -> value
          | Error error -> failwith error
        in
        let cap = C_types.count_weight_cap original in
        expect "positive effective cap" (Z.sign cap > 0);
        expect "selected cap is resilient"
          (C_types.count_live_at_cap
             ~faults:original.f
             cap
             (C_types.weights_of_set original));
        let maximum =
          List.fold_left
            (fun current (_, weight) -> Z.max current weight)
            Z.zero
            (C_types.weights_of_set original)
        in
        if Z.lt cap maximum then
          expect "selected cap is maximal"
            (not
               (C_types.count_live_at_cap
                  ~faults:original.f
                  Z.(add cap one)
                  (C_types.weights_of_set original))))
      (distributions count [])
  done

let test_activation_rewind_boundary () =
  let chain_id = "octra-devnet-9871-cluster" in
  expect "rewind below activation allowed"
    (C_quorum_policy.rewind_allowed
       ~chain_id
       ~from_epoch:1_306_546L
       ~to_epoch:1_306_544L);
  expect "rewind above activation allowed"
    (C_quorum_policy.rewind_allowed
       ~chain_id
       ~from_epoch:1_306_550L
       ~to_epoch:1_306_547L);
  expect "rewind through activation refused"
    (not
       (C_quorum_policy.rewind_allowed
          ~chain_id
          ~from_epoch:1_306_547L
          ~to_epoch:1_306_546L))

let test_qc () =
  let validate signers =
    C_qc.validate_finalize
      ~chain_id:"weighted-bft-test"
      ~validator_set
      ~verify_vote:(fun _ -> true)
      (finalize signers)
  in
  expect "new-chain qc rejects two heavy signers"
    (validate ["alice"; "bob"] = C_qc.Invalid "quorum");
  expect "new-chain qc accepts n-f signers"
    (validate ["alice"; "bob"; "carol"] = C_qc.Valid)

let test_vote_collection () =
  let votes = C_engine.create_vote_set () in
  let proposal_id = String.make 32 '\x11' in
  let add address =
    C_engine.add_vote
      votes
      (vote 7L 2 proposal_id address)
      ~validator_set
  in
  expect "light vote one" (add "dave" = `Added);
  expect "light vote two" (add "carol" = `Added);
  expect "light vote three" (add "bob" = `Added);
  expect "heavy vote completes quorum"
    (add "alice" = `QuorumOf proposal_id);
  expect "proposal weight"
    Z.(equal (C_engine.weight_for_pid votes proposal_id) (of_int 10));
  expect "unknown vote rejected"
    (C_engine.add_vote
       (C_engine.create_vote_set ())
       (vote 7L 2 proposal_id "mallory")
       ~validator_set
     = `Rejected)

let test_nil_weights () =
  let nil = Octra_net.Hash_domain.nil_hash in
  let at = Int64.of_int (quorum_activation ()).activation_epoch in
  List.iter
    (fun (chain_id, epoch_id) ->
      let validator_set =
        C_types.validator_set_for_epoch ~chain_id ~epoch_id validator_set
      in
      List.iter
        (fun signers ->
          let votes = C_engine.create_vote_set () in
          List.iter
            (fun signer ->
              let ballot = { (vote epoch_id 2 nil signer) with C_types.chain_id } in
              ignore (C_engine.add_vote votes ballot ~validator_set))
            signers;
          let result =
            C_engine.quorum_result votes ~chain_id ~epoch_id ~validator_set
          in
          let count = List.length (List.sort_uniq String.compare signers) in
          let weight = Option.get (C_types.signed_weight validator_set signers) in
          let expected =
            Z.geq weight validator_set.quorum_weight
            && (not (C_quorum_policy.count_floor_required ~chain_id ~epoch_id)
                || count >= validator_set.quorum)
          in
          expect "nil quorum matches count and weight"
            ((result = `QuorumOf nil) = expected))
        [["alice"; "bob"]; ["bob"; "carol"; "dave"];
         ["alice"; "bob"; "carol"]; ["alice"; "alice"; "bob"]])
    ["weighted-bft-test", 7L; devnet_chain_id, Int64.pred at;
     devnet_chain_id, at; devnet_chain_id, Int64.succ at]

let test_leader () =
  let first =
    C_types.leader_of validator_set ~epoch_id:100L ~round:4
  in
  let second =
    C_types.leader_of validator_set ~epoch_id:100L ~round:4
  in
  expect "weighted leader deterministic" (first = second);
  let counts = Hashtbl.create 4 in
  for epoch = 0 to 999 do
    let selected =
      C_types.leader_of
        validator_set
        ~epoch_id:(Int64.of_int epoch)
        ~round:0
    in
    let count = Option.value ~default:0 (Hashtbl.find_opt counts selected.address) in
    Hashtbl.replace counts selected.address (count + 1)
  done;
  let count address = Option.value ~default:0 (Hashtbl.find_opt counts address) in
  expect "weighted leader favors larger stake"
    (count "alice" > count "dave");
  expect "weighted leader covers every member"
    (List.for_all
       (fun address -> count address > 0)
       ["alice"; "bob"; "carol"; "dave"])

let test_config_binding () =
  let reordered =
    match C_types.make_weighted_validator_set [
      dave, Z.of_int 1;
      carol, Z.of_int 2;
      bob, Z.of_int 3;
      alice, Z.of_int 4;
    ] with
    | Ok value -> value
    | Error error -> failwith error
  in
  let unit_set =
    C_types.make_validator_set [alice; bob; carol; dave]
  in
  expect "weighted hash order independent"
    (C_config.validator_set_hash validator_set
     = C_config.validator_set_hash reordered);
  expect "weight changes config hash"
    (C_config.validator_set_hash validator_set
     <> C_config.validator_set_hash unit_set)

let test_invalid_sets () =
  expect "duplicate address rejected"
    (Result.is_error
       (C_types.make_weighted_validator_set [
          alice, Z.one;
          alice, Z.one;
        ]));
  expect "zero weight rejected"
    (Result.is_error
       (C_types.make_weighted_validator_set [
          alice, Z.zero;
        ]))

let test_stake_plan () =
  let module P = C_quorum_policy in
  let rule epoch_id = P.rule_at ~count_epoch:10L ~stake_epoch:(Some 20L) ~epoch_id in
  expect "prior rule" (rule 9L = P.Prior);
  expect "count rule" (rule 10L = P.Capped && rule 19L = P.Capped);
  expect "stake rule" (rule 20L = P.Stake && rule 21L = P.Stake);
  List.iter (fun chain_id -> List.iter (fun epoch_id ->
    let prior = match P.activation_for_chain chain_id with
      | None -> epoch_id >= 0L
      | Some activation -> epoch_id >= Int64.of_int activation.activation_epoch in
    let stake = match P.linear_epoch ~chain_id with
      | Some start -> epoch_id >= start
      | None -> false in
    expect "cap before stake" (P.count_cap_required ~chain_id ~epoch_id = (prior && not stake));
    expect "stake keeps floor" (P.count_floor_required ~chain_id ~epoch_id = prior);
    expect "stake selects rule" (P.linear_weight ~chain_id ~epoch_id = stake);
    expect "stake selects profile"
      ((P.consensus_id_at ~chain_id ~epoch_id = P.consensus_id ~chain_id) = not stake))
    [-1L; 0L; 1_306_546L; 1_306_547L; 1_688_733L; 1_688_734L;
     1_702_237L; 1_702_238L; 1_702_239L;
     Int64.pred (Option.get (P.linear_epoch ~chain_id:devnet_chain_id));
     Option.get (P.linear_epoch ~chain_id:devnet_chain_id);
     Int64.succ (Option.get (P.linear_epoch ~chain_id:devnet_chain_id)); Int64.max_int])
    [devnet_chain_id; "other-chain"];
  expect "stake rewind refused"
    (not (P.rewind_at ~count_epoch:10L ~stake_epoch:(Some 20L)
      ~from_epoch:20L ~to_epoch:19L));
  expect "stake rewind same rule"
    (P.rewind_at ~count_epoch:10L ~stake_epoch:(Some 20L)
      ~from_epoch:21L ~to_epoch:20L);
  List.iter (fun stake_epoch ->
    expect "invalid stake order" (try
      ignore (P.rule_at ~count_epoch:10L ~stake_epoch ~epoch_id:20L); false
      with Invalid_argument _ -> true)) [Some 9L; Some 10L];
  let raw = C_types.validator_set_for_rule P.Stake live_validator_set in
  let unweighted = C_types.make_validator_set raw.validators in
  expect "stake requires bonded weights"
    (not (C_types.has_quorum_for_rule P.Stake unweighted
      (List.map (fun (member : C_types.validator_info) -> member.address) unweighted.validators)));
  let policy = P.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:(Some 20L) in
  expect "unweighted votes cannot finalize" (not (C_types.quorum_reached_at
    ~policy ~chain_id:devnet_chain_id ~epoch_id:20L unweighted
    ~signer_count:unweighted.n ~signed_weight:unweighted.total_weight));
  expect "unweighted votes cannot skip round" (not (C_types.round_skip_reached_at
    ~policy ~chain_id:devnet_chain_id ~epoch_id:20L unweighted
    ~signer_count:unweighted.n ~signed_weight:unweighted.total_weight));
  expect "raw stake hash preserved"
    (C_config.validator_set_hash raw = C_config.validator_set_hash live_validator_set);
  expect "raw stake amount preserved" (Z.equal raw.total_weight (Z.of_int 28_000_000));
  let heavy = "live-1" in
  let light = ["live-2"; "live-3"; "live-4"; "live-5"; "live-6"; "live-7"] in
  let accepts = C_types.has_quorum_for_rule P.Stake live_validator_set in
  expect "weight alone is insufficient" (not (accepts [heavy]));
  expect "count alone is insufficient" (not (accepts light));
  expect "both thresholds" (accepts [heavy; "live-2"; "live-3"; "live-4"; "live-5"]);
  expect "duplicates do not add seats" (not (accepts [heavy; heavy; heavy; heavy; heavy]));
  expect "unknown signer refused" (not (accepts [heavy; "live-2"; "live-3"; "live-4"; "unknown"]));
  let decoded = C_codec.decode_validator_set (C_codec.encode_validator_set raw) in
  expect "stake restore hash" (C_config.validator_set_hash decoded = C_config.validator_set_hash raw);
  expect "stake restore quorum" (C_types.has_quorum_for_rule P.Stake decoded
    [heavy; "live-2"; "live-3"; "live-4"; "live-5"]);
  List.iter (fun epoch ->
    let selected = C_types.validator_set_for_rule (rule epoch) live_validator_set in
    let expected = if epoch < 10L || epoch >= 20L then live_validator_set
      else C_types.count_capped_set live_validator_set in
    expect "scheduled set hash" (C_config.validator_set_hash selected = C_config.validator_set_hash expected))
    [9L; 10L; 19L; 20L; 21L]

let test_codec () =
  let encoded = C_codec.encode_validator_set validator_set in
  let decoded = C_codec.decode_validator_set encoded in
  expect "validator codec weighted" decoded.weighted;
  expect "validator codec hash"
    (C_config.validator_set_hash decoded
     = C_config.validator_set_hash validator_set);
  expect "validator codec trailing bytes rejected"
    (try
       ignore (C_codec.decode_validator_set (encoded ^ "\x00"));
       false
     with _ -> true)

let () =
  test_nil_weights ();
  test_threshold ();
  test_live_activation ();
  test_catchup_activation_boundary ();
  test_genesis_quorum ();
  test_small_distributions ();
  test_activation_rewind_boundary ();
  test_qc ();
  test_vote_collection ();
  test_leader ();
  test_config_binding ();
  test_invalid_sets ();
  test_stake_plan ();
  test_codec ();
  Printf.printf "status = pass test = weighted_bft\n"