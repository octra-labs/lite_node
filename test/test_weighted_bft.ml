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

let plan = C_quorum_policy.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:None

let test_threshold () =
  expect "weighted flag" validator_set.weighted;
  expect "weighted total"
    Z.(equal validator_set.total_weight (of_int 10));
  expect "weighted quorum"
    Z.(equal validator_set.quorum_weight (of_int 7));
  expect "two heavy signers reach quorum"
    (C_types.has_quorum_at ~policy:plan ~chain_id:"weighted-bft-test" ~epoch_id:9L
      validator_set ["alice"; "bob"]);
  expect "two heavy signers do not satisfy dual quorum"
    (not (C_types.pair_at ~policy:plan ~chain_id:"weighted-bft-test" ~epoch_id:9L
      validator_set ["alice"; "bob"]));
  expect "three light signers do not reach quorum"
    (not (C_types.pair_at ~policy:plan ~chain_id:"weighted-bft-test" ~epoch_id:9L
      validator_set ["bob"; "carol"; "dave"]));
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
    (Octra_consensus.C_measure.meets
       (C_types.space ~chain_id:devnet_chain_id ~epoch_id:boundary transport)
       Octra_consensus.C_measure.Source
       ~seats:(List.length responders)
       ~weight:transport_weight);
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
            && (C_quorum_policy.select (C_quorum_policy.for_chain chain_id) epoch_id
                  = C_quorum_policy.Prior
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
    let selected = P.select (P.for_chain chain_id) epoch_id in
    expect "cap before stake" ((selected = P.Capped) = (prior && not stake));
    expect "stake keeps floor" ((selected <> P.Prior) = prior);
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
    (not (C_types.has_quorum_at ~policy:plan ~chain_id:devnet_chain_id ~epoch_id:20L unweighted
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
  let accepts =
    C_types.has_quorum_at ~policy:plan ~chain_id:devnet_chain_id ~epoch_id:20L live_validator_set in
  expect "weight alone is insufficient" (not (accepts [heavy]));
  expect "count alone is insufficient" (not (accepts light));
  expect "both thresholds" (accepts [heavy; "live-2"; "live-3"; "live-4"; "live-5"]);
  expect "duplicates do not add seats" (not (accepts [heavy; heavy; heavy; heavy; heavy]));
  expect "unknown signer refused" (not (accepts [heavy; "live-2"; "live-3"; "live-4"; "unknown"]));
  let decoded = C_codec.decode_validator_set (C_codec.encode_validator_set raw) in
  expect "stake restore hash" (C_config.validator_set_hash decoded = C_config.validator_set_hash raw);
  expect "stake restore quorum" (C_types.has_quorum_at ~policy:plan ~chain_id:devnet_chain_id
    ~epoch_id:20L decoded
    [heavy; "live-2"; "live-3"; "live-4"; "live-5"]);
  List.iter (fun epoch ->
    let selected = C_types.validator_set_for_rule (rule epoch) live_validator_set in
    let expected = if epoch < 10L || epoch >= 20L then live_validator_set
      else C_types.count_capped_set live_validator_set in
    expect "scheduled set hash" (C_config.validator_set_hash selected = C_config.validator_set_hash expected))
    [9L; 10L; 19L; 20L; 21L]

let test_seatless () =
  let module P = C_quorum_policy in
  let module M = C_measure in
  let chain_id = "weighted-bft-test" in
  let policy = P.with_seatless_epoch plan (Some 30L) in
  expect "seatless phase changed stake selection" (P.select policy 30L = P.Stake);
  expect "seatless phase differs" (not (P.seatless policy 29L) && P.seatless policy 30L);
  expect "seatless schedule omitted epoch" (P.epochs policy = [20L; 30L]);
  expect "seatless rewind crossed activation"
    (not (P.rewind policy ~from_epoch:30L ~to_epoch:29L)
      && P.rewind policy ~from_epoch:31L ~to_epoch:30L);
  expect "seatless profile omitted activation"
    (P.id_at policy ~chain_id ~epoch_id:30L
      = P.id_at plan ~chain_id ~epoch_id:30L ^ ":seatless:30:single_seat_survival");
  List.iter (fun epoch_id ->
    expect "seatless changed historical profile"
      (P.id_at policy ~chain_id ~epoch_id = P.id_at plan ~chain_id ~epoch_id);
    List.iter (fun signers ->
      expect "seatless changed historical quorum"
        (C_types.has_quorum_at ~policy ~chain_id ~epoch_id validator_set signers
          = C_types.has_quorum_at ~policy:plan ~chain_id ~epoch_id validator_set signers))
      [[]; ["alice"]; ["alice"; "bob"]; ["alice"; "bob"; "carol"]])
    [-1L; 0L; 9L; 10L; 19L; 20L; 29L];
  List.iter (fun base ->
    expect "seatless accepted invalid activation"
      (try ignore (P.with_seatless_epoch base (Some 19L)); false
       with Invalid_argument _ -> true))
    [plan; P.make ~count_epoch:10L ~stake_epoch:None ~round_epoch:None];
  let epoch_id = 30L in
  let quorum = C_types.has_quorum_at ~policy ~chain_id ~epoch_id validator_set in
  expect "seatless quorum retained seat count" (quorum ["alice"; "bob"]);
  expect "seatless quorum accepted insufficient weight" (not (quorum ["bob"; "carol"; "dave"]));
  expect "seatless quorum counted duplicate weight" (not (quorum ["alice"; "alice"]));
  expect "seatless quorum accepted unknown signer" (not (quorum ["alice"; "bob"; "unknown"]));
  expect "seatless pair retained seat count"
    (C_types.pair_at ~policy ~chain_id ~epoch_id validator_set ["alice"; "bob"]);
  expect "seatless engine quorum retained seat count"
    (C_types.quorum_reached_at ~policy ~chain_id ~epoch_id validator_set
      ~signer_count:2 ~signed_weight:(Z.of_int 7));
  expect "seatless skip retained seat count"
    (C_types.round_skip_reached_at ~policy ~chain_id ~epoch_id validator_set
      ~signer_count:1 ~signed_weight:(Z.of_int 4));
  expect "seatless promise lost known weight"
    (C_types.promise_at ~policy ~chain_id ~epoch_id validator_set ["alice"; "unknown"]);
  expect "seatless promise counted duplicate or unknown weight"
    (not (C_types.promise_at ~policy ~chain_id ~epoch_id validator_set ["bob"; "bob"; "unknown"]));
  let space = C_types.space ~policy ~chain_id ~epoch_id validator_set in
  List.iter (fun ask ->
    expect "seatless minority threshold differs"
      (M.meets space ask ~seats:1 ~weight:(Z.of_int 4)
        && not (M.meets space ask ~seats:4 ~weight:(Z.of_int 3))))
    M.[Skip; Source; Ahead; Promise];
  expect "seatless attestation retained configured count"
    (M.attested space ~required:100 ~seats:1 ~weight:(Z.of_int 4)
      && not (M.attested space ~required:0 ~seats:4 ~weight:(Z.of_int 3)));
  expect "seatless query routing differs"
    (M.replies space ~configured:100 = 1 && M.fanout space = 4 && M.wave space = 3);
  expect "seatless same-weight intersection rejected" (M.overlap space validator_set.weights = Ok ());
  let empty = M.space policy epoch_id ~declared:true [] in
  let undeclared = M.space policy epoch_id ~declared:false validator_set.weights in
  List.iter (fun space ->
    List.iter (fun ask ->
      expect "seatless accepted unproved weight" (not (M.meets space ask ~seats:100 ~weight:(Z.of_int 100))))
      M.[Quorum; Skip; Source; Pair; Ahead; Promise];
    expect "seatless attested unproved weight"
      (not (M.attested space ~required:0 ~seats:100 ~weight:(Z.of_int 100))))
    [empty; undeclared];
  let header = header epoch_id in
  let proposal_id = C_hash.proposal_id header in
  let unsafe = validator_set in
  let validator_set = C_types.make_weighted_validator_set
    (List.mapi (fun index name -> validator name (Char.chr (65 + index)),
      Z.of_int (if index < 3 then 3 else 1)) ["alice"; "bob"; "carol"; "dave"; "erin"; "frank"; "grace"])
    |> function Ok set -> set | Error reason -> failwith reason in
  let certificate = C_types.{chain_id; epoch_id; commit_round = 2; header; proposal_id;
    precommits = List.map (vote epoch_id 2 proposal_id) ["alice"; "bob"; "carol"]} in
  let check policy certificate = C_qc.validate_certificate ~policy ~chain_id ~validator_set
    ~verify_vote:(fun _ -> true) certificate in
  expect "seatless certificate retained seat count" (check policy certificate = C_qc.Valid);
  expect "trusted certificate reapplied entry cap"
    (C_qc.validate_certificate ~policy ~chain_id ~validator_set:unsafe ~verify_vote:(fun _ -> true)
      certificate = C_qc.Valid);
  expect "ordinary heavy set entered"
    (Result.is_error (C_types.checked_validator_set_for_epoch ~policy ~chain_id ~epoch_id unsafe));
  expect "historical certificate accepted weight-only quorum"
    (check plan certificate = C_qc.Invalid "quorum");
  expect "seatless certificate accepted duplicate signer"
    (check policy {certificate with precommits = certificate.precommits @ certificate.precommits}
      = C_qc.Invalid "duplicate_validator");
  expect "seatless certificate skipped signature check"
    (C_qc.validate_certificate ~policy ~chain_id ~validator_set ~verify_vote:(fun _ -> false)
      certificate = C_qc.Invalid "signature")

let test_seatless_votes () =
  let policy = C_quorum_policy.with_seatless_epoch plan (Some 30L) in
  List.iter (fun epoch_id ->
    List.iter (fun proposal_id ->
      let votes = C_engine.create_vote_set () in
      let add name = C_engine.add_vote ~policy votes (vote epoch_id 2 proposal_id name) ~validator_set in
      expect "weighted vote counted unknown" (add "unknown" = `Rejected);
      expect "weighted vote reached quorum too soon" (add "alice" = `Added);
      expect "weighted vote counted repeat" (add "alice" = `Duplicate);
      expect "weighted vote collection selected another rule"
        (add "bob" = if epoch_id = 30L then `QuorumOf proposal_id else `Added);
      expect "weighted vote total differs"
        (votes.count_total = 2 && Z.equal votes.weight_total (Z.of_int 7));
      if epoch_id = 29L then
        expect "historical vote collection lost quorum" (add "carol" = `QuorumOf proposal_id))
      [String.make 32 '\x21'; Octra_net.Hash_domain.nil_hash];
    let pool = C_round_pool.create () in
    let add validator = C_round_pool.add pool C_codec.{
      chain_id = "weighted-bft-test"; epoch_id; round = 3; step = PrevoteStep;
      request = false; validator; signature = String.make 64 '\000'} in
    let witness () = C_round_pool.witness ~policy pool ~chain_id:"weighted-bft-test" ~epoch_id
      ~after_round:0 ~through_round:3 ~validator_set in
    List.iter add ["unknown"; "alice"; "alice"];
    expect "weighted round witness selected another rule"
      (List.map (fun row -> row.C_codec.validator) (witness ())
        = if epoch_id = 30L then ["alice"] else []);
    add "bob";
    expect "weighted round witness retained unknown or duplicate"
      (List.map (fun row -> row.C_codec.validator) (witness ())
        = if epoch_id = 30L then ["alice"] else ["alice"; "bob"])) [29L; 30L]

let weight_set weights =
  List.mapi (fun index weight ->
    validator ("seat-" ^ string_of_int index) (Char.chr (40 + index)), weight) weights
  |> C_types.make_weighted_validator_set
  |> function Ok value -> value | Error reason -> failwith reason

let seat_policy = C_quorum_policy.with_seatless_epoch plan (Some 30L)

let check_weight ?(policy = seat_policy) ?(epoch_id = 30L) set =
  C_types.checked_validator_set_for_epoch ~policy ~chain_id:"weighted-bft-test" ~epoch_id set

let weight_rows total weight =
  let rest = Z.sub total weight in
  let part = Z.div rest (Z.of_int 3) in
  [weight; part; part; Z.sub rest (Z.mul (Z.of_int 2) part)]

let test_weight_residues unit =
  List.iter (fun residue ->
    let total = Z.add (Z.mul (Z.of_int 3) unit) (Z.of_int residue) in
    let limit = Z.div (Z.add total (Z.of_int 2)) (Z.of_int 3) in
    List.iter (fun offset ->
      let rows = weight_rows total (Z.add limit (Z.of_int offset)) in
      List.iter (fun position ->
        let rotated = List.init 4 (fun index -> List.nth rows ((index + position) mod 4)) in
        let set = weight_set rotated in
        let result = check_weight set in
        expect "weight threshold residue differs"
          (Result.is_ok result = (offset < 0));
        if offset < 0 then begin
          expect "accepted weights changed" (result = Ok set);
          List.iter (fun (_, weight) ->
            expect "accepted seat prevents quorum"
              (C_measure.live ~total:set.total_weight ~lost:weight)) set.weights
        end else
          expect "heavy weight was not refused"
            (result = Error "validator weight reaches blocking threshold"))
        [0; 1; 2; 3]) [-1; 0; 1]) [0; 1; 2]

let test_weight_edges () =
  let set rows = weight_set (List.map Z.of_int rows) in
  let safe = set [1; 1; 1; 1] in
  List.iter (fun epoch_id ->
    expect "equal seats refused" (check_weight ~epoch_id safe = Ok safe);
    let heavy = set [20; 4; 3; 2; 1; 1; 1] in
    expect "heavy set accepted" (Result.is_error (check_weight ~epoch_id heavy));
    expect "raw selection changed"
      (C_types.validator_set_for_epoch ~policy:seat_policy ~chain_id:"weighted-bft-test"
        ~epoch_id heavy = heavy)) [30L; 31L; Int64.max_int];
  List.iter (fun rows ->
    expect "small set accepted" (Result.is_error (check_weight (set rows))))
    [[1]; [1; 1]; [1; 1; 1]; [100; 1; 1]];
  List.iter (fun (before, after) ->
    let before = set before in
    expect "source weights refused" (check_weight before = Ok before);
    expect "changed weights accepted" (Result.is_error (check_weight (set after))))
    [[5; 5; 5; 5], [10; 5; 5; 5; 5];
     [9; 5; 5; 5; 5], [9; 5; 5; 5];
     [9; 8; 8; 1; 1; 1; 1], [9; 8; 8; 1; 1];
     [5; 5; 5; 5], [10; 5; 5; 5]];
  test_weight_residues (Z.of_int 10);
  let huge = Z.shift_left Z.one 4096 in
  test_weight_residues huge;
  let large = weight_set [huge; huge; huge; huge] in
  expect "large weights changed" (check_weight large = Ok large);
  expect "large heavy seat accepted"
    (Result.is_error (check_weight (weight_set [huge; Z.one; Z.one; Z.one])));
  expect "reordered weights differ"
    (check_weight {safe with validators = List.rev safe.validators;
      weights = List.rev safe.weights} = Ok safe)

let weight_faults () =
  let set = weight_set [Z.one; Z.one; Z.one; Z.one] in
  let entries = C_types.weights_of_set set in
  let first = List.hd set.validators in
  let rest = List.tl set.weights in
  let build = C_types.build_validator_set ~weighted:true in
  let replace member weight = build ((member, weight) :: List.tl entries) in
  ["undeclared", {set with weighted = false};
   "empty", build [];
   "zero", replace first Z.zero;
   "negative", replace first Z.minus_one;
   "empty address", replace {first with address = ""} Z.one;
   "short key", replace {first with pubkey = String.make 31 'x'} Z.one;
   "long key", replace {first with pubkey = String.make 33 'x'} Z.one;
   "duplicate address", build (({first with pubkey = String.make 32 'x'}, Z.one) :: entries);
   "duplicate key", build (({first with address = "other"}, Z.one) :: entries);
   "duplicate member", build ((first, Z.one) :: entries);
   "missing weight", {set with weights = rest};
   "extra weight", {set with weights = ("other", Z.one) :: set.weights};
   "duplicate weight", {set with weights = List.hd set.weights :: set.weights};
   "repeated weight address", {set with weights =
     List.hd set.weights :: List.hd set.weights :: List.tl rest};
   "unknown weight", {set with weights = ("other", Z.one) :: rest};
   "seat count", {set with n = set.n + 1};
   "fault count", {set with f = set.f + 1};
   "seat quorum", {set with quorum = set.quorum + 1};
   "total", {set with total_weight = Z.succ set.total_weight};
   "weight quorum", {set with quorum_weight = Z.pred set.quorum_weight};
   "inflated total", {set with total_weight = Z.shift_left Z.one 4096};
   "missing validators", {set with validators = List.tl set.validators}]

let test_weight_shape () =
  List.iter (fun (name, set) ->
    expect ("malformed set accepted: " ^ name) (Result.is_error (check_weight set)))
    (weight_faults ());
  let heavy = weight_set (List.map Z.of_int [20; 4; 3; 2; 1; 1; 1]) in
  expect "forged shape accepted"
    (check_weight {heavy with total_weight = Z.of_int 1000; quorum_weight = Z.one}
      = Error "validator set shape differs");
  expect "undeclared non-unit weights accepted"
    (check_weight {heavy with weighted = false} = Error "validator weights are not declared")

let test_weight_authors () =
  let safe = weight_set [Z.one; Z.one; Z.one; Z.one] in
  let author = validator "author" 'z' in
  let huge = Z.shift_left Z.one 4096 in
  let authors = Some (30L, [author, huge]) in
  let with_authors = {safe with C_types.authors} in
  expect "author weight entered voting total" (check_weight with_authors = Ok with_authors);
  let heavy = weight_set (List.map Z.of_int [20; 4; 3; 2; 1; 1; 1]) in
  expect "author weight concealed heavy voter"
    (check_weight {heavy with authors} = Error "validator weight reaches blocking threshold");
  List.iter (fun height ->
    expect "other epoch authors retained"
      (check_weight {safe with authors = Some (height, [author, huge])} = Ok safe))
    [29L; 31L];
  let first = List.hd safe.validators in
  List.iter (fun entries ->
    expect "invalid author context accepted"
      (Result.is_error (check_weight {safe with authors = Some (30L, entries)})))
    [[]; [author, Z.zero]; [author, Z.minus_one];
     [{author with address = ""}, Z.one]; [{author with pubkey = ""}, Z.one];
     [author, Z.one; author, Z.one]; [first, Z.one];
     [{author with pubkey = first.pubkey}, Z.one];
     [{author with address = first.address}, Z.one]]

let test_weight_history () =
  let chain_id = "weighted-bft-test" in
  let heavy = weight_set (List.map Z.of_int [20; 4; 3; 2; 1; 1; 1]) in
  let with_authors = {heavy with C_types.authors = Some (29L, [validator "author" 'z', Z.one])} in
  let result run = try `Returned (run ()) with exn -> `Raised (Printexc.to_string exn) in
  List.iter (fun epoch_id ->
    List.iter (fun (name, set) ->
      let prior = result (fun () -> Ok (C_types.validator_set_for_epoch
        ~policy:seat_policy ~chain_id ~epoch_id set)) in
      let checked = result (fun () -> check_weight ~epoch_id set) in
      expect ("historical set behavior changed: " ^ name) (checked = prior))
      (("heavy", heavy) :: ("authors", with_authors) :: weight_faults ()))
    [-1L; 0L; 9L; 10L; 19L; 20L; 29L];
  List.iter (fun epoch_id ->
    expect "disabled rule changed stake"
      (check_weight ~policy:plan ~epoch_id heavy = Ok heavy)) [30L; Int64.max_int];
  let together = C_quorum_policy.with_seatless_epoch plan (Some 20L) in
  expect "joint activation checked too early"
    (check_weight ~policy:together ~epoch_id:19L heavy
      = Ok (C_types.validator_set_for_epoch ~policy:together ~chain_id ~epoch_id:19L heavy));
  expect "joint activation missed heavy seat"
    (Result.is_error (check_weight ~policy:together ~epoch_id:20L heavy))

let test_weight_chain () =
  let heavy = weight_set (List.map Z.of_int [20; 4; 3; 2; 1; 1; 1]) in
  let at = match C_quorum_policy.seatless_activation_for_chain devnet_chain_id with
    | Some activation -> Int64.of_int activation.activation_epoch
    | None -> failwith "seatless activation missing" in
  let check ?policy chain_id epoch_id =
    C_types.checked_validator_set_for_epoch ?policy ~chain_id ~epoch_id heavy in
  expect "default policy checked historical set"
    (check devnet_chain_id (Int64.pred at) = Ok heavy);
  List.iter (fun epoch_id ->
    expect "default policy accepted heavy seat"
      (Result.is_error (check devnet_chain_id epoch_id));
    expect "explicit policy ignored" (check ~policy:plan devnet_chain_id epoch_id = Ok heavy))
    [at; Int64.succ at];
  let chain_id = "other-chain" in
  expect "seatless applied to another chain"
    (check chain_id at = Ok (C_types.validator_set_for_epoch ~chain_id ~epoch_id:at heavy));
  expect "explicit seatless policy ignored"
    (Result.is_error (check ~policy:seat_policy chain_id 30L))

let test_weight_check () =
  test_weight_edges ();
  test_weight_shape ();
  test_weight_authors ();
  test_weight_history ();
  test_weight_chain ()

let test_weight_structure () =
  let structural ?(epoch_id = 30L) set = C_types.structural_validator_set_for_epoch
    ~policy:seat_policy ~chain_id:"weighted-bft-test" ~epoch_id set in
  let heavy = weight_set (List.map Z.of_int [3; 3; 2; 1]) in
  expect "structural check applies entry cap" (structural heavy = Ok heavy);
  expect "default check lost strict cap" (Result.is_error (check_weight heavy));
  List.iter (fun (name, set) ->
    expect ("structural check accepted malformed set: " ^ name)
      (match structural set with Error (C_types.Set_shape _) -> true | _ -> false))
    (weight_faults ());
  let author = List.hd heavy.validators, Z.one in
  expect "structural check skipped author identity"
    (match structural {heavy with authors = Some (30L, [author])} with
     | Error (C_types.Set_shape _) -> true | _ -> false);
  let result run = try `Returned (run ()) with exn -> `Raised (Printexc.to_string exn) in
  List.iter (fun epoch_id -> List.iter (fun (name, set) ->
    expect ("structural history changed: " ^ name)
      (result (fun () -> structural ~epoch_id set)
        = result (fun () -> Ok (C_types.validator_set_for_epoch ~policy:seat_policy
         ~chain_id:"weighted-bft-test" ~epoch_id set))))
    (("heavy", heavy) :: weight_faults ())) [-1L; 0L; 9L; 10L; 19L; 20L; 29L]

let test_eviction_exact () =
  let old = weight_set (List.map Z.of_int [3; 3; 2; 1; 1]) in
  let evict offenders set = C_types.evict_validator_set ~offenders set in
  let next, removed = match evict ["seat-4"] old with
    | Ok value -> value | Error reason -> failwith reason in
  expect "eviction changed old set" (check_weight old = Ok old);
  expect "eviction did not use old voting weight" (Z.equal removed Z.one);
  expect "eviction changed remaining keys or weights"
    (C_types.weights_of_set next = List.filter (fun (member, _) ->
      member.C_types.address <> "seat-4") (C_types.weights_of_set old));
  expect "eviction total differs" (Z.equal next.total_weight (Z.of_int 9));
  expect "eviction clipped a remaining seat"
    (List.assoc "seat-0" next.weights = Z.of_int 3);
  expect "eviction result bypasses default cap"
    (C_types.validate_validator_set_for_epoch ~policy:seat_policy
      ~chain_id:"weighted-bft-test" ~epoch_id:30L next = Error C_types.Seat_cap);
  expect "eviction result has invalid structure"
    (C_types.validate_validator_set_shape next = Ok next);
  expect "eviction requires old active entry cap"
    (Result.is_ok (evict ["seat-3"] next))

let test_eviction_reject () =
  let old = weight_set (List.map Z.of_int [3; 3; 2; 1; 1]) in
  let evict offenders set = C_types.evict_validator_set ~offenders set in
  List.iter (fun offenders -> expect "invalid eviction accepted"
    (Result.is_error (evict offenders old)))
    [[]; ["seat-4"; "seat-4"]; ["unknown"]; ["seat-0"; "seat-4"];
     List.map fst old.weights];
  List.iter (fun (name, set) -> expect ("eviction accepted malformed set: " ^ name)
    (Result.is_error (evict ["seat-0"] set))) (weight_faults ());
  expect "eviction admitted exactly one third removal"
    (Result.is_error (evict ["seat-0"] (weight_set [Z.one; Z.one; Z.one])))

let test_eviction_authors () =
  let old = weight_set (List.map Z.of_int [3; 3; 2; 1; 1]) in
  let evict set = C_types.evict_validator_set ~offenders:["seat-4"] set in
  let expected = evict old in
  let author = validator "evicted-author" 'z', Z.one in
  expect "eviction retained old author context"
    (evict {old with authors = Some (30L, [author])} = expected);
  expect "eviction ignored malformed author context"
    (Result.is_error (evict {old with authors = Some (30L, [List.hd
      (C_types.weights_of_set old)])}))

let test_eviction_large () =
  let huge = Z.shift_left Z.one 4096 in
  let large = weight_set (List.init 7 (fun _ -> huge)) in
  let large_next, large_removed = match C_types.evict_validator_set
    ~offenders:["seat-0"; "seat-4"] large with
    | Ok value -> value | Error reason -> failwith reason in
  expect "eviction narrowed huge removed weight"
    (Z.equal large_removed (Z.mul (Z.of_int 2) huge));
  expect "eviction narrowed huge total"
    (Z.equal large_next.total_weight (Z.mul (Z.of_int 5) huge))

let test_eviction_range unit =
  List.iter (fun residue ->
    let total = Z.add (Z.mul (Z.of_int 3) unit) (Z.of_int residue) in
    let limit = Z.div (Z.add total (Z.of_int 2)) (Z.of_int 3) in
    List.iter (fun offset ->
      let weight = Z.add limit (Z.of_int offset) in
      let old = weight_set (weight_rows total weight) in
      let result = C_types.evict_validator_set ~offenders:["seat-0"] old in
      expect "eviction threshold residue differs" (Result.is_ok result = (offset < 0));
      match result with
      | Error _ -> ()
      | Ok (next, removed) ->
        expect "eviction residue removed weight differs" (Z.equal removed weight);
        expect "eviction residue remaining total differs"
          (Z.equal next.total_weight (Z.sub total weight))) [-1; 0; 1]) [0; 1; 2]

let eviction_driver ?resolve ?static ?(start_height = 30L) ~current ~head ~pending ~on_finalized () =
  let chain_id = "weighted-bft-test" in
  let member = List.hd current.C_types.validators in
  let sign_fn _ = String.make 64 's' in
  let swarm = Octra_net.P2p_swarm.create Octra_net.P2p_swarm.{
    listen_port = 0; chain_id;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey member.pubkey;
    node_addr = member.address; pubkey_raw = member.pubkey;
    consensus_config_hash = String.make 32 'c'; binary_hash = String.make 32 'b';
    require_binary_hash = false; upgrade_plan = None; profile_plan = [];
    allowed_pubkeys = []; bootstrap_peers = []; max_peers = 0; sign_fn;
    best_epoch_fn = (fun () -> !head); best_root_fn = (fun () -> String.make 32 'r');
  } in
  let config = C_driver.{
    chain_id; my_addr = member.address; sign_fn; verify_fn = (fun _ _ _ -> true);
    role_can_vote = (fun () -> false); can_vote = (fun () -> false);
    execute_fn = (fun _ -> true); verify_proposal = (fun _ -> Lwt.return Proposal_accept);
    verify_parent_commit = (fun ~epoch_id:_ _ -> Ok ()); on_finalized;
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
      ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ -> Lwt.return_true);
    lookup_epoch_root = (fun _ -> None); local_head_epoch = (fun () -> !head);
    lookup_bundle = (fun _ -> None);
    lookup_catchup_range = (fun ~from_epoch:_ ~max_epochs:_ -> `NotFound);
    on_resource_attestation = (fun _ -> Lwt.return_unit);
    scheduled_validator_set_config = static;
    load_scheduled_validator_set_config = (fun () -> Lwt.return !pending);
    resource_committee_config = None;
  } in
  C_driver.create_for_policy ?resolve seat_policy ~config ~validator_set:current ~swarm
    ~start_height ~sync_log:(C_sync_log.memory ()) ~relief_log:(C_relief_log.memory ())
    ~vote_log:(C_vote_log.memory ())

let test_plan_activation_scope () =
  let source = weight_set (List.init 4 (fun _ -> Z.one)) in
  let target = weight_set (List.map Z.of_int [7; 1; 1; 1]) in
  let config = C_driver.{activate_epoch = 32L; validator_set = target;
    fingerprint = "future-heavy"; eviction = None} in
  List.iter (fun height ->
    let actual = C_driver.check_validator_plan ~policy:seat_policy
      ~chain_id:"weighted-bft-test" ~head:height ~current_epoch:height
      ~current:source ~finalized:None config in
    expect "future activation changed earlier plan acceptance"
      (Result.is_ok actual = (height < 30L))) [28L; 29L; 30L; 31L];
  let driver = eviction_driver ~start_height:28L ~current:source ~head:(ref 27L)
    ~pending:(ref None) ~on_finalized:(fun ~validator_set:_ _ -> Lwt.return_unit) () in
  expect "preactivation driver refused future heavy plan"
    (C_driver.accept_dynamic_plan driver config)

let eviction_plan source =
  let next, _ = C_types.evict_validator_set ~offenders:["seat-4"] source |> Result.get_ok in
  C_driver.{activate_epoch = 31L; validator_set = next; fingerprint = "exact-eviction";
    eviction = Some C_types.{epoch = 30L; source; offenders = ["seat-4"]}}

let test_driver_eviction () =
  let source = weight_set (List.map Z.of_int [3; 3; 2; 1; 1]) in
  let exact = eviction_plan source in
  let head = ref 29L and pending = ref None and observed = ref None in
  let driver = eviction_driver ~current:source ~head ~pending
    ~on_finalized:(fun ~validator_set finalize ->
      observed := Some validator_set;
      pending := Some exact;
      head := finalize.C_types.epoch_id;
      Lwt.return_unit) () in
  let future = C_driver.{activate_epoch = 40L; validator_set = source;
    fingerprint = "future-set"; eviction = None} in
  driver.relief_plan <- Some future;
  driver.plan_mark <- Some (40L, future.fingerprint);
  driver.stake_vs <- weight_set (List.map Z.of_int [4; 3; 2; 1; 1]);
  let installed = ref [] in
  C_driver.set_validator_set_activation_handler driver (fun set fingerprint ->
    installed := (set, fingerprint) :: !installed; Lwt.return_unit);
  pending := Some exact;
  expect "uncommitted eviction accepted" (not (C_driver.accept_dynamic_plan driver exact));
  expect "uncommitted eviction changed relief" (driver.relief_plan = Some future && !installed = []);
  pending := None;
  let final_header = {(header 30L) with C_types.creator_addr = "seat-0"} in
  let finalize = C_types.{chain_id = "weighted-bft-test"; epoch_id = 30L; commit_round = 0;
    header = final_header; proposal_id = C_hash.proposal_id final_header;
    precommits = []; parent_commit = None} in
  expect "driver did not accept finalized epoch" (C_engine.accept_finalize_batch driver.engine finalize);
  Lwt_main.run (C_driver.process_outputs driver);
  expect "finalized source weights changed" (!observed = Some source
    && driver.finalized_set = Some (30L, source));
  expect "eviction did not replace future relief" (driver.relief_plan = None
    && driver.plan_mark = Some (31L, exact.fingerprint));
  expect "driver changed exact remaining weights" (driver.engine.vs = exact.validator_set
    && driver.stake_vs = exact.validator_set && driver.engine.state.height = 31L);
  expect "driver did not publish exact set" (!installed = [exact.validator_set, exact.fingerprint]);
  expect "postslash set should exceed entry cap" (Result.is_error (check_weight exact.validator_set));
  expect "next engine lost finalized source" (C_driver.accept_dynamic_plan driver exact);
  Lwt_main.run (C_driver.maybe_activate_scheduled_validator_set driver ~target_epoch:32L);
  expect "later epoch clipped eviction weights" (driver.engine.vs = exact.validator_set
    && !installed = [exact.validator_set, exact.fingerprint]);
  expect "ordinary earlier plan accepted"
    (not (C_driver.accept_dynamic_plan driver {future with activate_epoch = 30L}));
  let next_header = {(header 31L) with C_types.creator_addr = "seat-0"} in
  let next_finalize = {finalize with epoch_id = 31L; header = next_header;
    proposal_id = C_hash.proposal_id next_header} in
  expect "next finalized epoch refused" (C_engine.accept_finalize_batch driver.engine next_finalize);
  Lwt_main.run (C_driver.process_outputs driver);
  expect "old pending replaced trusted current weights"
    (driver.finalized_set = Some (31L, exact.validator_set)
      && driver.engine.state.height = 32L && driver.engine.vs = exact.validator_set
      && driver.stake_vs = exact.validator_set && !installed = [exact.validator_set, exact.fingerprint]);
  expect "old pending witness remained trusted" (not (C_driver.accept_dynamic_plan driver exact));
  expect "trusted current projection reapplied entry cap"
    (C_driver.planned_set driver ~epoch:33L None = exact.validator_set);
  let restored = eviction_driver ~resolve:(fun _ -> Ok exact.validator_set)
    ~current:exact.validator_set ~head ~pending:(ref None)
    ~on_finalized:(fun ~validator_set:_ _ -> Lwt.return_unit) () in
  expect "trusted restart changed eviction weights" (restored.engine.vs = exact.validator_set);
  let refused resolve = try
    ignore (eviction_driver ?resolve ~current:exact.validator_set ~head ~pending:(ref None)
      ~on_finalized:(fun ~validator_set:_ _ -> Lwt.return_unit) ()); false
    with Invalid_argument _ -> true in
  expect "ordinary heavy startup entered driver" (refused None);
  expect "forged startup resolver entered driver" (refused (Some (fun _ -> Ok source)))

let test_driver_eviction_refusal () =
  let source = weight_set (List.map Z.of_int [3; 3; 2; 1; 1]) in
  let exact = eviction_plan source in
  let witness = Option.get exact.eviction in
  let changed_source = weight_set (List.map Z.of_int [3; 3; 1; 1; 1]) in
  let forged = eviction_plan changed_source in
  let cases = [
    "ordinary heavy", {exact with eviction = None};
    "source", forged;
    "source weight", {exact with eviction = Some {witness with source = changed_source}};
    "target weight", {exact with validator_set = weight_set (List.map Z.of_int [3; 3; 1; 1])};
    "offender", {exact with eviction = Some {witness with offenders = ["seat-3"]}};
    "duplicate offender", {exact with eviction = Some {witness with offenders = ["seat-4"; "seat-4"]}};
    "unknown offender", {exact with eviction = Some {witness with offenders = ["unknown"]}};
    "source epoch", {exact with eviction = Some {witness with epoch = 29L}};
    "future source", {exact with activate_epoch = 32L; eviction = Some {witness with epoch = 31L}};
    "target epoch", {exact with activate_epoch = 32L};
    "negative epoch", {exact with activate_epoch = 0L; eviction = Some {witness with epoch = -1L}};
    "overflow epoch", {exact with activate_epoch = Int64.min_int;
      eviction = Some {witness with epoch = Int64.max_int}};
  ] in
  List.iter (fun (label, config) ->
    let pending = ref (Some config) in
    let driver = eviction_driver ~current:source ~head:(ref 30L) ~pending
      ~on_finalized:(fun ~validator_set:_ _ -> Lwt.return_unit) () in
    let installed = ref false in
    C_driver.set_validator_set_activation_handler driver
      (fun _ _ -> installed := true; Lwt.return_unit);
    expect ("invalid plan accepted: " ^ label) (not (C_driver.accept_dynamic_plan driver config));
    Lwt_main.run (C_driver.maybe_activate_scheduled_validator_set driver ~target_epoch:31L);
    expect ("invalid plan changed driver: " ^ label)
      (not !installed && driver.engine.vs = source && driver.stake_vs = source)) cases

let test_dynamic_plan_priority () =
  let source = weight_set (List.map Z.of_int [3; 3; 2; 1; 1]) in
  let target = weight_set (List.init 5 (fun _ -> Z.of_int 2)) in
  let static = C_driver.{activate_epoch = 32L; validator_set = source;
    fingerprint = "static-plan"; eviction = None} in
  let dynamic = {static with validator_set = target; fingerprint = "dynamic-plan"} in
  let pending = ref None in
  let driver = eviction_driver ~static ~current:source ~head:(ref 30L) ~pending
    ~on_finalized:(fun ~validator_set:_ _ -> Lwt.return_unit) () in
  expect "static plan recorded dynamic priority"
    (C_driver.static_plan driver = Some static && driver.plan_mark = None);
  let unsafe = {(eviction_plan source) with eviction = None} in
  expect "refused plan recorded dynamic priority"
    (not (C_driver.accept_dynamic_plan driver unsafe) && driver.plan_mark = None);
  pending := Some dynamic;
  Lwt_main.run (C_driver.maybe_activate_scheduled_validator_set driver ~target_epoch:32L);
  C_engine.start_height driver.engine 32L;
  expect "first dynamic plan did not replace static plan"
    (driver.engine.vs = target && driver.plan_mark = Some (32L, dynamic.fingerprint));
  List.iter (fun plan ->
    pending := plan;
    Lwt_main.run (C_driver.maybe_activate_scheduled_validator_set driver ~target_epoch:33L);
    expect "accepted dynamic plan lost priority"
      (driver.engine.vs = target && driver.stake_vs = target
        && driver.plan_mark = Some (32L, dynamic.fingerprint)))
    [None; Some static; Some {static with activate_epoch = 31L}]

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
  test_weight_check ();
  test_weight_structure ();
  test_eviction_exact ();
  test_eviction_reject ();
  test_eviction_authors ();
  test_eviction_large ();
  test_eviction_range (Z.of_int 10);
  test_eviction_range (Z.shift_left Z.one 4096);
  test_plan_activation_scope ();
  test_driver_eviction ();
  test_driver_eviction_refusal ();
  test_dynamic_plan_priority ();
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
  test_seatless ();
  test_seatless_votes ();
  test_codec ();
  Printf.printf "status = pass test = weighted_bft\n"