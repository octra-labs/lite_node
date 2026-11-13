(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.C_types
module D = Octra_consensus.C_driver
module M = Octra_consensus.C_measure
module P = Octra_consensus.C_quorum_policy
module R = Octra_consensus.C_relief
module H = Octra_node_runtime.Consensus_health_probe
module W = Octra_node_runtime.Consensus_pending_commit_recovery
module Swarm = Octra_net.P2p_swarm
module K = Quorum_pinned

let expect label value = if not value then failwith ("test_measure_pinned: " ^ label)
let seen = Hashtbl.create 64
let note label = Hashtbl.replace seen label ()
let noted label = expect ("case was not reached " ^ label) (Hashtbl.mem seen label)

let same label where left right =
  note (label ^ " = " ^ string_of_bool right);
  if left <> right then
    failwith (Printf.sprintf "test_measure_pinned: %s differs library = %b pinned = %b %s"
      label left right (where ()))

let both label = List.iter (fun value -> noted (label ^ " = " ^ string_of_bool value)) [true; false]

let chain_id = "octra-test-measure"
let devnet = "octra-devnet-9871-cluster"
let forbid _ = failwith "signing is not part of the pinned test"

let member index =
  T.{address = "v" ^ string_of_int index; pubkey = String.make 32 (Char.chr (index + 1))}

let range first last = List.init (max 0 (last - first + 1)) (fun index -> first + index)

let subsets items =
  List.fold_right (fun item rest -> rest @ List.map (fun chosen -> item :: chosen) rest) items [[]]

let positive n = [
  (fun _ -> 1);
  (fun index -> index + 1);
  (fun index -> if index = 0 then 3 * n else 1);
  (fun index -> if index < 2 then 5 else 1);
]

let entries n weight = List.init n (fun index -> member index, Z.of_int (weight index))

let raw_sets =
  List.concat_map (fun n ->
    T.make_validator_set (List.init n member)
    :: List.map (fun weight -> T.build_validator_set ~weighted:true (entries n weight))
      (positive n @ [fun _ -> 0]))
    (range 0 8)

let sets = raw_sets @ List.map T.count_capped_set raw_sets

let weighted_set members = match T.make_weighted_validator_set members with
  | Ok value -> value | Error reason -> failwith reason

let same_entries left right =
  List.equal (fun (a, x) (b, y) -> a = b && Z.equal x y) left right

let same_set (left : T.validator_set) (right : T.validator_set) =
  left.validators = right.validators
  && List.equal (fun (a, x) (b, y) -> String.equal a b && Z.equal x y) left.weights right.weights
  && left.n = right.n && left.f = right.f && left.quorum = right.quorum
  && Z.equal left.total_weight right.total_weight
  && Z.equal left.quorum_weight right.quorum_weight
  && left.weighted = right.weighted
  && left.authors = right.authors

let describe (vs : T.validator_set) () =
  Printf.sprintf "n = %d weighted = %b weights = %s" vs.n vs.weighted
    (String.concat "," (List.map (fun (_, weight) -> Z.to_string weight) vs.weights))

let planned = P.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:(Some 30L)
let unplanned = P.make ~count_epoch:10L ~stake_epoch:None ~round_epoch:None

let points =
  List.concat_map (fun (policy, chain_id, epochs) ->
    List.map (fun epoch_id -> policy, chain_id, epoch_id) epochs) [
    None, chain_id, [-1L; 0L; 1L];
    None, devnet, [1_306_546L; 1_306_547L; 1_737_332L; 1_737_333L; 1_739_999L; 1_740_000L];
    Some planned, chain_id, [9L; 10L; 19L; 20L; 29L; 30L];
    Some unplanned, devnet, [9L; 10L; 11L; 1_737_333L];
  ]

let rule_of (policy, chain_id, epoch_id) =
  P.select (Option.value policy ~default:(P.for_chain chain_id)) epoch_id

let rules = [P.Prior; P.Capped; P.Stake]

let rule_name = function P.Prior -> "prior" | P.Capped -> "capped" | P.Stake -> "stake"

let at vs (_, chain_id, epoch_id as point) () =
  Printf.sprintf "%s chain = %s epoch = %Ld rule = %s" (describe vs ()) chain_id epoch_id
    (rule_name (rule_of point))

let shape () =
  List.iter (fun total ->
    let total = Z.of_int total in
    expect "quorum_weight differs"
      (Z.equal (M.quota total) (K.C_types.quorum_weight total)))
    (range 0 300);
  expect "rule borders are not covered" (List.for_all (fun rule ->
    List.length (List.filter (fun point -> rule_of point = rule) points) >= 4) rules);
  List.iter (fun (vs : T.validator_set) ->
    let entries = T.weights_of_set vs in
    expect "weights_of_set differs" (same_entries entries (K.C_types.weights_of_set vs));
    expect "set fields differ from its entries"
      (same_set vs (T.build_validator_set ~weighted:vs.weighted entries));
    List.iter (fun entries ->
      expect ("build_validator_set differs " ^ describe vs ())
        (same_set (T.build_validator_set ~weighted:vs.weighted entries)
          (K.C_types.build_validator_set ~weighted:vs.weighted entries)))
      [entries; List.rev entries];
    let top = List.fold_left (fun top (_, weight) -> max top (Z.to_int weight)) 0 entries in
    List.iter (fun cap ->
      let cap = Z.of_int cap in
      expect "cap_entries differs"
        (same_entries (T.cap_entries cap entries) (K.C_types.cap_entries cap entries));
      List.iter (fun faults ->
        same "count_live_at_cap" (describe vs)
          (T.count_live_at_cap ~faults cap entries)
          (K.C_types.count_live_at_cap ~faults cap entries))
        (range (-1) (vs.n + 1)))
      (range 0 (top + 1));
    List.iter (fun count ->
      expect "largest_weight_sum differs"
        (Z.equal (T.largest_weight_sum count entries)
          (K.C_types.largest_weight_sum count entries)))
      (range (-1) (vs.n + 1));
    expect ("count_weight_cap differs " ^ describe vs ())
      (Z.equal (T.count_weight_cap vs) (K.C_types.count_weight_cap vs));
    expect ("count_capped_set differs " ^ describe vs ())
      (same_set (T.count_capped_set vs) (K.C_types.count_capped_set vs));
    List.iter (fun rule ->
      expect ("validator_set_for_rule differs " ^ describe vs ())
        (same_set (T.validator_set_for_rule rule vs) (K.C_types.validator_set_for_rule rule vs));
      same "weights_valid" (describe vs)
        (T.weights_valid rule vs) (K.C_types.weights_valid rule vs))
      rules;
    List.iter (fun (policy, chain_id, epoch_id as point) ->
      List.iter (fun vs ->
        expect ("validator_set_for_epoch differs " ^ at vs point ())
          (same_set (T.validator_set_for_epoch ?policy ~chain_id ~epoch_id vs)
            (K.C_types.validator_set_for_epoch ?policy ~chain_id ~epoch_id vs)))
        [vs; {vs with authors = Some (epoch_id, [member 9, Z.one])};
          {vs with authors = Some (Int64.succ epoch_id, [member 9, Z.one])}])
      points)
    sets;
  both "count_live_at_cap";
  both "weights_valid"

let signer_lists (vs : T.validator_set) =
  List.map (fun (validator : T.validator_info) -> validator.address) vs.validators
  |> subsets
  |> List.concat_map (fun signers -> [
    signers;
    List.rev signers;
    (match signers with [] -> ["absent"] | first :: _ -> first :: signers);
    signers @ ["absent"];
  ])

let unique signers = List.length (List.sort_uniq String.compare signers) = List.length signers

let signers () =
  List.iter (fun (vs : T.validator_set) ->
    List.iter (fun signers ->
      let where () = describe vs () ^ " signers = " ^ String.concat "," signers in
      expect ("signed_weight differs " ^ where ())
        (Option.equal Z.equal (T.signed_weight vs signers) (K.C_types.signed_weight vs signers));
      expect "unique_signer_count differs"
        (T.unique_signer_count signers = K.C_types.unique_signer_count signers);
      same "has_weight_quorum" where
        (T.has_quorum_at ~policy:planned ~chain_id ~epoch_id:9L vs signers)
        (K.C_types.has_weight_quorum vs signers);
      same "has_quorum" where
        (T.pair_at ~policy:planned ~chain_id ~epoch_id:9L vs signers)
        (K.C_types.has_quorum vs signers);
      List.iter2 (fun rule epoch_id ->
        same ("has_quorum_for_rule " ^ rule_name rule) where
          (T.has_quorum_at ~policy:planned ~chain_id ~epoch_id vs signers)
          (K.C_types.has_quorum_for_rule rule vs signers))
        rules [9L; 10L; 20L];
      List.iter (fun (policy, chain_id, epoch_id as point) ->
        let rule = rule_of point in
        let reached = T.has_quorum_at ?policy ~chain_id ~epoch_id vs signers in
        same ("has_quorum_at " ^ rule_name rule) (fun () -> at vs point () ^ " " ^ where ())
          reached (K.C_types.has_quorum_at ?policy ~chain_id ~epoch_id vs signers);
        same ("pair_at " ^ rule_name rule) (fun () -> at vs point () ^ " " ^ where ())
          (T.pair_at ?policy ~chain_id ~epoch_id vs signers) (K.C_types.has_quorum vs signers);
        if reached && unique signers && rule <> P.Prior then begin
          note ("vote set " ^ rule_name rule);
          expect ("policy quorum lacks vote set quorum " ^ at vs point () ^ " " ^ where ())
            (K.C_types.has_quorum
              (T.validator_set_for_epoch ?policy ~chain_id ~epoch_id vs) signers)
        end)
        points)
      (signer_lists vs))
    sets;
  List.iter both ["has_weight_quorum"; "has_quorum"];
  List.iter (fun rule ->
    both ("has_quorum_for_rule " ^ rule_name rule);
    both ("has_quorum_at " ^ rule_name rule);
    both ("pair_at " ^ rule_name rule))
    rules;
  List.iter (fun rule -> noted ("vote set " ^ rule_name rule)) [P.Capped; P.Stake];
  let vs = List.nth raw_sets 30 in
  expect "unknown signer was counted"
    (vs.n = 5 && T.signed_weight vs ["v0"; "absent"] = None
      && not (T.has_quorum_at ~policy:planned ~chain_id ~epoch_id:9L vs
        ["v0"; "v1"; "v2"; "v3"; "v4"; "absent"]));
  expect "repeated signer was counted twice"
    (Option.equal Z.equal (T.signed_weight vs ["v0"; "v0"; "v1"]) (Some (Z.of_int 2))
      && T.unique_signer_count ["v0"; "v0"; "v1"] = 2)

let grid () =
  List.iter (fun (vs : T.validator_set) ->
    expect "round_skip_weight differs"
      (Z.equal (T.round_skip_weight vs) (K.C_types.round_skip_weight vs));
    List.iter (fun signer_count ->
      List.iter (fun weight ->
        let signed_weight = Z.of_int weight in
        let votes () = Printf.sprintf "count = %d weight = %d" signer_count weight in
        let where () = describe vs () ^ " " ^ votes () in
        same "has_quorum_values" where
          (M.meets (T.space_for_rule P.Capped vs) M.Pair ~seats:signer_count ~weight:signed_weight)
          (K.C_types.has_quorum_values vs ~signer_count ~signed_weight);
        List.iter (fun (policy, chain_id, epoch_id as point) ->
          let rule = rule_name (rule_of point) in
          let where () = at vs point () ^ " " ^ votes () in
          same ("quorum_reached_at " ^ rule) where
            (T.quorum_reached_at ?policy ~chain_id ~epoch_id vs ~signer_count ~signed_weight)
            (K.C_types.quorum_reached_at ?policy ~chain_id ~epoch_id vs
              ~signer_count ~signed_weight);
          same ("round_skip_reached_at " ^ rule) where
            (T.round_skip_reached_at ?policy ~chain_id ~epoch_id vs
              ~signer_count ~signed_weight)
            (K.C_types.round_skip_reached_at ?policy ~chain_id ~epoch_id vs
              ~signer_count ~signed_weight))
          points)
        (range (-1) (Z.to_int vs.total_weight + 2)))
      (range (-1) (vs.n + 1)))
    sets;
  both "has_quorum_values";
  List.iter (fun rule ->
    both ("quorum_reached_at " ^ rule_name rule);
    both ("round_skip_reached_at " ^ rule_name rule))
    rules

let rows () =
  List.iter (fun (vs : T.validator_set) ->
    let copy = K.C_types.build_validator_set ~weighted:vs.weighted (K.C_types.weights_of_set vs) in
    let shape = M.shape ~seats:(List.length vs.weights) ~total:(K.C_types.sum_weights vs.weights) in
    expect ("row shape differs " ^ describe vs ())
      (shape.n = copy.n && shape.f = copy.f && shape.quorum = copy.quorum
        && Z.equal shape.total_weight copy.total_weight
        && Z.equal shape.quorum_weight copy.quorum_weight);
    expect ("row minority differs " ^ describe vs ())
      (Z.equal (M.minority shape) (K.C_types.round_skip_weight vs));
    let entries = K.C_types.weights_of_set vs in
    let top = List.fold_left (fun top (_, weight) -> max top (Z.to_int weight)) 0 entries in
    List.iter (fun cap ->
      let effective = K.C_types.cap_entries (Z.of_int cap) entries in
      List.iter (fun faults ->
        same "row live" (describe vs)
          (M.live ~total:(K.C_types.sum_weights effective)
            ~lost:(K.C_types.largest_weight_sum faults effective))
          (K.C_types.count_live_at_cap ~faults (Z.of_int cap) entries))
        (range (-1) (vs.n + 1)))
      (range 0 (top + 1));
    List.iter (fun rule ->
      let space = M.ruled rule ~declared:vs.weighted vs.weights in
      same ("row valid " ^ rule_name rule) (describe vs)
        (M.valid space) (K.C_types.weights_valid rule vs);
      expect "row label differs" (String.equal (M.label space) (rule_name rule)))
      rules;
    List.iter (fun (policy, chain_id, epoch_id as point) ->
      let rule = rule_of point in
      let name = rule_name rule in
      let space = M.space (Option.value policy ~default:(P.for_chain chain_id)) epoch_id
        ~declared:vs.weighted vs.weights in
      expect "row space differs" (String.equal (M.label space) name);
      List.iter (fun seats ->
        List.iter (fun weight ->
          let where () = Printf.sprintf "%s seats = %d weight = %d" (at vs point ()) seats weight in
          let weight = Z.of_int weight in
          same ("row quorum " ^ name) where
            (M.meets space M.Quorum ~seats ~weight)
            (K.C_types.quorum_reached_at ?policy ~chain_id ~epoch_id vs
              ~signer_count:seats ~signed_weight:weight);
          same ("row skip " ^ name) where
            (M.meets space M.Skip ~seats ~weight)
            (K.C_types.round_skip_reached_at ?policy ~chain_id ~epoch_id vs
              ~signer_count:seats ~signed_weight:weight);
          same ("row source " ^ name) where
            (M.meets space M.Source ~seats ~weight)
            (K.C_driver.catchup_source_agreement_reached ~count_floor:(rule <> P.Prior) vs
              ~signer_count:seats ~signed_weight:weight);
          same ("row pair " ^ name) where
            (M.meets space M.Pair ~seats ~weight)
            (K.C_types.has_quorum_values vs ~signer_count:seats ~signed_weight:weight))
          (range (-1) (Z.to_int vs.total_weight + 2)))
        (range (-1) (vs.n + 1)))
      points)
    sets;
  both "row live";
  List.iter (fun rule ->
    let name = rule_name rule in
    noted ("row valid " ^ name ^ " = true");
    List.iter (fun row -> both (row ^ name)) ["row quorum "; "row skip "; "row source "; "row pair "])
    rules;
  noted "row valid stake = false"

let contains text part =
  let size = String.length part in
  List.exists (fun start -> String.equal (String.sub text start size) part)
    (range 0 (String.length text - size))

let body text name =
  let head = "\nlet " ^ name ^ " " in
  match List.find_opt (fun start -> String.equal (String.sub text start (String.length head)) head)
    (range 0 (String.length text - String.length head)) with
  | None -> failwith ("test_measure_pinned: function is absent name = " ^ name)
  | Some start ->
    let rest = String.sub text (start + 1) (String.length text - start - 1) in
    let stop = List.find_opt (fun index -> String.equal (String.sub rest index 5) "\nlet ")
      (range 0 (String.length rest - 5)) in
    String.sub rest 0 (Option.value stop ~default:(String.length rest))

let owned = [
  "build_validator_set"; "count_live_at_cap"; "weights_valid"; "has_quorum_at";
  "quorum_reached_at"; "pair_at"; "promise_at"; "round_skip_weight"; "round_skip_reached_at";
]

let compared = [">="; "<="; "Z.geq"; "Z.gt"; "Z.leq"; "Z.lt"; "/ 3"; "C_quorum_policy.Prior"]

let callers () =
  let text = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  List.iter (fun name ->
    let code = body text name in
    expect ("threshold is not asked from the module function = " ^ name)
      (contains code "C_measure.");
    List.iter (fun part ->
      expect (Printf.sprintf "threshold is compared beside the module call function = %s text = %s"
        name part)
        (not (contains code part)))
      compared)
    owned

let auth = weighted_set (entries 8 (fun _ -> 1))

let make policy validator_set =
  let pubkey = String.make 32 '\200' in
  let root = String.make 32 '\002' in
  let swarm = Swarm.create {
    listen_port = 0; chain_id;
    node_id = Octra_net.P2p_handshake.node_id_of_pubkey pubkey;
    node_addr = "observer"; pubkey_raw = pubkey;
    consensus_config_hash = String.make 32 '\000';
    binary_hash = String.make 32 '\000'; require_binary_hash = false;
    upgrade_plan = None; profile_plan = []; allowed_pubkeys = [];
    bootstrap_peers = []; max_peers = 4; sign_fn = forbid;
    best_epoch_fn = (fun () -> 40L); best_root_fn = (fun () -> root);
  } in
  let config = D.{
    chain_id; my_addr = "observer"; sign_fn = forbid;
    verify_fn = (fun _ _ _ -> failwith "signature verification is not part of this test");
    role_can_vote = (fun () -> false); can_vote = (fun () -> false);
    execute_fn = (fun _ -> failwith "pinned test executed a proposal");
    verify_proposal = (fun _ -> failwith "pinned test checked a proposal");
    verify_parent_commit = (fun ~epoch_id:_ _ -> Error "not a proposal test");
    on_finalized = (fun ~validator_set:_ _ -> failwith "pinned test finalized an epoch");
    make_proposal = (fun _ -> Lwt.return_none);
    before_precommit_broadcast = (fun ~epoch_id:_ ~round:_ ~proposal_id:_
      ~proposed_state_root:_ ~txid_hi:_ ~proposal_wire:_ ~vote_wire:_ ->
      failwith "pinned test attempted a vote");
    lookup_epoch_root = (fun _ -> Some root);
    local_head_epoch = (fun () -> 40L);
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

let rule_epochs = [9L; 10L; 20L]

let pinned_weights rule vs (held : D.root_weights) =
  K.C_driver.root_weights rule vs ~epoch_id:held.epoch_id ~stamp:held.stamp
    ~weight:(K.C_types.weight_of_addr vs)

let root_rows rule (vs : T.validator_set) result =
  let where () = describe vs () ^ " rule = " ^ rule_name rule in
  same ("root weights " ^ rule_name rule) where
    (Option.is_some result) (K.C_types.weights_valid rule vs);
  Option.iter (fun (held : D.root_weights) ->
    let pinned = pinned_weights rule vs held in
    expect ("root weight_required differs " ^ where ())
      (Z.sign pinned.weight_required <= 0
        || held.ahead pinned.weight_required && not (held.ahead (Z.pred pinned.weight_required)));
    List.iter (fun weight ->
      let weight = Z.of_int weight in
      same ("root ahead " ^ rule_name rule)
        (fun () -> Printf.sprintf "%s weight = %s" (where ()) (Z.to_string weight))
        (held.ahead weight)
        (Z.sign pinned.weight_required > 0 && Z.geq weight pinned.weight_required))
      (range (-1) (Z.to_int vs.total_weight + 2));
    List.iter (fun count ->
      List.iter (fun weight ->
        let weight = Z.of_int weight in
        same ("root agrees " ^ rule_name rule)
          (fun () -> Printf.sprintf "%s count = %d weight = %s" (where ()) count
            (Z.to_string weight))
          (held.agrees ~count ~weight) (pinned.agrees ~count ~weight))
        (range (-1) (Z.to_int vs.total_weight + 2));
      List.iter (fun required ->
        same ("root attested " ^ rule_name rule)
          (fun () -> Printf.sprintf "%s count = %d required = %d" (where ()) count required)
          (held.attested ~required ~count ~weight:Z.zero) (count >= required))
        (range (-1) (vs.n + 2)))
      (range (-1) (vs.n + 1));
    List.iter (fun configured ->
      expect (Printf.sprintf "root count_required or count_limit differs %s configured = %d"
        (where ()) configured)
        (held.required ~configured = K.Probe.state_attest_required pinned ~configured);
      expect ("state attest count differs " ^ where ())
        (K.Probe.state_attest_required pinned ~configured
          = M.replies (T.space_for_rule rule vs) ~configured))
      (max_int :: range (-1) (vs.n + 2)))
    result

let source_rows site reached rule (vs : T.validator_set) =
  let label = site ^ (if rule = P.Prior then "source agreement by weight"
    else "source agreement with count") in
  List.iter (fun count ->
    List.iter (fun weight ->
      let weight = Z.of_int weight in
      let pinned = K.C_driver.catchup_source_agreement_reached ~count_floor:(rule <> P.Prior)
        vs ~signer_count:count ~signed_weight:weight in
      let where () = Printf.sprintf "%s rule = %s count = %d weight = %s" (describe vs ())
        (rule_name rule) count (Z.to_string weight) in
      same label where (reached ~count ~weight) pinned)
      (range (-1) (Z.to_int vs.total_weight + 2)))
    (range (-1) (vs.n + 1))

let roots () =
  let driver = make planned auth in
  expect "rule epochs differ"
    (List.map (P.select planned) rule_epochs = rules);
  List.iter (fun vs ->
    List.iter (fun epoch_id ->
      let rule = P.select planned epoch_id in
      driver.D.stake_vs <- vs;
      let effective = K.C_types.validator_set_for_epoch ~policy:planned ~chain_id ~epoch_id vs in
      root_rows rule effective (D.epoch_root_weights driver ~epoch_id);
      root_rows rule vs (D.epoch_root_weights ~validator_set:vs driver ~epoch_id);
      source_rows "" (D.catchup_agreement_reached driver ~epoch_id) rule effective;
      source_rows "unplanned " (D.planned_agreement_reached driver None ~epoch_id) rule effective;
      driver.D.stake_vs <- auth;
      source_rows "planned "
        (D.planned_agreement_reached driver
          (Some D.{activate_epoch = epoch_id; validator_set = vs; fingerprint = "pinned"; eviction = None})
          ~epoch_id)
        rule effective;
      let space = T.space ~policy:planned ~chain_id ~epoch_id vs in
      let peers = List.init 24 string_of_int in
      List.iter (fun count ->
        let peers = List.filteri (fun index _ -> index < count) peers in
        expect ("catchup query waves differ " ^ describe vs ())
          (D.catchup_query_waves space peers = K.C_driver.catchup_query_waves vs peers))
        (range 0 24);
      let records = List.init 10 (fun index ->
        D.{responder_addr = (member index).address;
          responder_head_epoch = Int64.of_int (4 + index mod 3); state_root = None}) in
      expect ("finality query peers differ " ^ describe vs ())
        (D.finality_query_peer_ids space vs ~epoch_id:5L records
          = K.C_driver.finality_query_peer_ids vs ~epoch_id:5L records))
      rule_epochs)
    sets;
  List.iter (fun rule ->
    noted ("root weights " ^ rule_name rule ^ " = true");
    both ("root agrees " ^ rule_name rule);
    both ("root ahead " ^ rule_name rule);
    both ("root attested " ^ rule_name rule))
    rules;
  List.iter (fun site ->
    both (site ^ "source agreement with count");
    both (site ^ "source agreement by weight"))
    [""; "unplanned "; "planned "];
  noted "root weights stake = false";
  let empty = T.make_validator_set [] in
  let single = T.make_validator_set [member 0] in
  let read vs = match D.epoch_root_weights ~validator_set:vs driver ~epoch_id:9L with
    | Some value -> value | None -> failwith "test_measure_pinned: root weights unavailable" in
  expect "empty set root counts differ"
    ((read empty).required ~configured:3 = 0
      && (read single).required ~configured:3 = 0
      && (pinned_weights P.Prior empty (read empty)).count_limit = 0
      && (pinned_weights P.Prior empty (read empty)).count_required = 1
      && (pinned_weights P.Prior single (read single)).count_limit = 0
      && K.Probe.state_attest_required (pinned_weights P.Prior empty (read empty))
        ~configured:3 = 0
      && K.Probe.state_attest_required (pinned_weights P.Prior single (read single))
        ~configured:3 = 0);
  expect "start peer count differs"
    (List.map (fun (rule, n_validators, quorum) ->
      K.C_driver.min_peers rule ~n_validators ~quorum) [
      P.Stake, 0, 0; P.Stake, 1, 1; P.Prior, 4, 3; P.Capped, 2, 2; P.Capped, 4, 3;
      P.Stake, 7, 5; P.Stake, 8, 6;
    ] = [0; 0; 1; 1; 2; 4; 5])

let relief () =
  let fingerprint = String.make 64 'a' in
  let height = R.activation_epoch in
  let activate_epoch = Int64.succ height in
  List.iter (fun n ->
    List.iter (fun weight ->
      let members = entries n weight in
      let current = weighted_set members in
      subsets members
      |> List.filter (fun chosen -> List.length chosen >= 4 && List.length chosen < n)
      |> List.iter (fun chosen ->
        let target = weighted_set chosen in
        let pinned = K.C_relief.check_intersection current target in
        let where () = describe current () ^ " target " ^ describe target () in
        same "relief plan" where
          (R.clock_limit ~chain_id:devnet ~height ~current ~activate_epoch ~target ~fingerprint
            = Some R.min_round)
          (Result.is_ok pinned);
        match R.decide ~chain_id:devnet ~height ~current ~activate_epoch ~target ~fingerprint
          ~proof:[], pinned with
        | R.Wait, Ok () -> ()
        | R.Refuse reason, Error expected when String.equal reason expected -> note reason
        | _ -> failwith ("test_measure_pinned: relief reason differs " ^ where ())))
      (positive n))
    (range 4 8);
  both "relief plan";
  List.iter noted [
    "validator relief count intersection is unsafe";
    "validator relief weight intersection is unsafe";
  ]

let majority (value : H.majority) =
  K.Probe.{root = value.root; count = value.count; weight = value.weight}

let root_quorum = function
  | H.No_root -> K.Probe.No_root
  | H.Missing_quorum value -> K.Probe.Missing_quorum (majority value)
  | H.Matching_quorum value -> K.Probe.Matching_quorum (majority value)
  | H.Mismatching_quorum value -> K.Probe.Mismatching_quorum (majority value)

let sync_plan = function
  | H.Peer_root_mismatch value -> K.Probe.Peer_root_mismatch (majority value)
  | H.Accept_genesis -> K.Probe.Accept_genesis
  | H.Missing_peer_root_quorum count -> K.Probe.Missing_peer_root_quorum count
  | H.Accept_current_root -> K.Probe.Accept_current_root

let quorum_name = function
  | K.Probe.No_root -> "no root"
  | K.Probe.Missing_quorum _ -> "missing quorum"
  | K.Probe.Matching_quorum _ -> "matching quorum"
  | K.Probe.Mismatching_quorum _ -> "mismatching quorum"

let choices options count =
  List.fold_left (fun rows _ ->
    List.concat_map (fun row -> List.map (fun option -> option :: row) options) rows)
    [[]] (range 1 count)

let flags = [false; true]

let probe () =
  let driver = make planned auth in
  let local_root = String.make 32 'a' in
  let roots = [None; Some None; Some (Some local_root); Some (Some (String.make 32 'b'));
    Some (Some (String.make 32 'c'))] in
  let probe_sets =
    List.map (fun weight -> weighted_set (entries 4 weight)) (positive 4)
    @ [weighted_set (entries 5 (fun index -> index + 1));
      T.make_validator_set (List.init 4 member)] in
  List.iter (fun (vs : T.validator_set) ->
    List.iter (fun epoch_id ->
      let rule = P.select planned epoch_id in
      Option.iter (fun (held : D.root_weights) ->
        let pinned = pinned_weights rule vs held in
        let where () = describe vs () ^ " rule = " ^ rule_name rule in
        List.iter (fun row ->
          let replies = List.concat (List.mapi (fun index choice ->
            Option.to_list (Option.map (fun state_root ->
              D.{responder_addr = (member index).address; responder_head_epoch = epoch_id;
                state_root}) choice)) row) in
          List.iter (fun extra ->
            let responses = replies @ extra in
            List.iter (fun required ->
              let library = H.peer_root_quorum ~weights:held ~required ~local_root responses in
              let copy = K.Probe.peer_root_quorum ~weights:pinned ~required ~local_root
                responses in
              note (quorum_name copy);
              expect ("peer root quorum differs " ^ where ()) (root_quorum library = copy);
              List.iter (fun has_committed_root ->
                List.iter (fun genesis_attested ->
                  expect ("in sync plan differs " ^ where ())
                    (sync_plan (H.in_sync_plan ~required ~has_committed_root ~genesis_attested
                      library)
                    = K.Probe.in_sync_plan ~required ~has_committed_root ~genesis_attested copy))
                  flags)
                flags)
              (range 0 (vs.n + 1)))
            [[]; [D.{responder_addr = "absent"; responder_head_epoch = epoch_id;
              state_root = Some (String.make 32 'b')}]])
          (choices roots vs.n);
        let heads = None :: List.map (fun step -> Some (Int64.add epoch_id step))
          [-2L; -1L; 0L; 1L] in
        List.iter (fun row ->
          let responses = List.concat (List.mapi (fun index choice ->
            Option.to_list (Option.map (fun responder_head_epoch ->
              D.{responder_addr = (member index).address; responder_head_epoch;
                state_root = None}) choice)) row) in
          List.iter (fun ceiling ->
            let library = H.peer_ahead ~weights:held ~head:epoch_id ~ceiling responses in
            note ("peer ahead = " ^ string_of_bool (Option.is_some library));
            expect ("peer ahead differs " ^ where ())
              (Option.equal Int64.equal library
                (K.Probe.peer_ahead ~weights:pinned ~head:epoch_id ~ceiling responses)))
            [epoch_id; Int64.succ epoch_id])
          (choices heads vs.n))
        (D.epoch_root_weights ~validator_set:vs driver ~epoch_id))
      rule_epochs)
    probe_sets;
  List.iter noted ["no root"; "missing quorum"; "matching quorum"; "mismatching quorum"];
  both "peer ahead";
  expect "zero attest count rejected the committed root"
    (H.in_sync_plan ~required:0 ~has_committed_root:true ~genesis_attested:false H.No_root
      = H.Accept_current_root
      && H.in_sync_plan ~required:1 ~has_committed_root:true ~genesis_attested:false H.No_root
        = H.Missing_peer_root_quorum 0)

let decision = function
  | W.Confirmed_elsewhere {agreed; level} ->
    K.Pending.Confirmed_elsewhere {agreed; quorum = int_of_string level}
  | W.All_peers_missing {responses} -> K.Pending.All_peers_missing {responses}
  | W.Inconclusive {agreed; peers; responses; level} ->
    K.Pending.Inconclusive {agreed; peers; responses; quorum = int_of_string level}

let counted peer_quorum matched = List.length matched >= peer_quorum

let pending () =
  let promised = String.make 32 'p' in
  let states = [None; Some promised; Some (String.make 32 'q')] in
  let rows = List.concat_map (choices states) (range 0 4) in
  List.iter (fun row ->
    let responses = List.mapi (fun index state_root ->
      D.{responder_addr = "absent" ^ string_of_int index; responder_head_epoch = 7L;
        state_root}) row in
    List.iter (fun promised_root_raw ->
      List.iter (fun validator_count ->
        List.iter (fun peer_quorum ->
          let library = W.decide ~validator_count ~reached:(counted peer_quorum)
            ~level:(string_of_int peer_quorum) ~promised_root_raw responses in
          note (match library with
            | W.Confirmed_elsewhere _ -> "confirmed"
            | W.All_peers_missing _ -> "all missing"
            | W.Inconclusive _ -> "inconclusive");
          expect "pending decision differs"
            (decision library
              = K.Pending.decide ~validator_count ~peer_quorum ~promised_root_raw responses))
          (range 0 6))
        (range 0 6))
      [None; Some promised])
    rows;
  List.iter noted ["confirmed"; "all missing"; "inconclusive"];
  let outside = List.map (fun responder_addr ->
    D.{responder_addr; responder_head_epoch = 7L; state_root = Some promised})
    ["absent0"; "absent1"] in
  expect "pending decision read the responder set"
    (W.decide ~validator_count:4 ~reached:(counted 2) ~level:"2"
      ~promised_root_raw:(Some promised) outside
      = W.Confirmed_elsewhere {agreed = 2; level = "2"}
    && W.decide ~validator_count:9 ~reached:(counted 2) ~level:"2"
      ~promised_root_raw:(Some promised) outside
      = W.Confirmed_elsewhere {agreed = 2; level = "2"});
  List.iter (fun (vs : T.validator_set) ->
    let peer_quorum = K.Pending.peer_quorum ~quorum:vs.quorum in
    List.iter (fun (policy, chain_id, epoch_id as point) ->
      let runtime = W.driver_runtime
        ~read_pending_commits:(fun () -> [])
        ~head:(fun () -> None)
        ~query_epoch_root:(fun () ~epoch_id:_ -> Lwt.return_nil)
        ~run_catchup_to_target:(fun () ~target_epoch:_ ~reason:_ -> Lwt.return_unit)
        ~delete_pending_commit:(fun ~epoch_id:_ ~round:_ -> ())
        ~restore_pending:(fun () _ -> W.Legacy)
        ~validator_count:4
        ~reached:(T.promise_at ?policy ~chain_id ~epoch_id vs)
        ~level:(M.promise_text (T.space ?policy ~chain_id ~epoch_id vs)) in
      expect (Printf.sprintf "pending peer quorum differs quorum = %d" vs.quorum)
        (runtime.level = string_of_int peer_quorum);
      List.iter (fun count ->
        List.iter (fun name ->
          let matched = List.init count name in
          same ("pending promise " ^ rule_name (rule_of point))
            (fun () -> Printf.sprintf "%s matched = %s" (at vs point ()) (String.concat "," matched))
            (runtime.reached matched) (count >= peer_quorum))
          [(fun index -> (member index).address); (fun index -> "absent" ^ string_of_int index);
            (fun _ -> (member 0).address)])
        (range 0 (vs.n + 1)))
      points)
    sets;
  List.iter (fun rule -> both ("pending promise " ^ rule_name rule)) rules

let () =
  shape ();
  signers ();
  grid ();
  rows ();
  callers ();
  roots ();
  relief ();
  probe ();
  pending ();
  print_endline "status = pass test = measure_pinned"