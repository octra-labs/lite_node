(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.C_types
module M = Octra_consensus.C_measure
module P = Octra_consensus.C_quorum_policy
module K = Quorum_pinned

let expect label value = if not value then failwith ("test_overlap_pinned: " ^ label)
let seen = Hashtbl.create 16
let note label = Hashtbl.replace seen label ()
let noted label = expect ("case was not reached " ^ label) (Hashtbl.mem seen label)

let devnet = "octra-devnet-9871-cluster"
let height = Octra_consensus.C_relief.activation_epoch
let prior = P.make ~count_epoch:(Int64.succ height) ~stake_epoch:None ~round_epoch:None
let stake = P.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:None
let policies = [Some prior, P.Prior; None, P.Capped; Some stake, P.Stake]
let rule_name = function P.Prior -> "prior" | P.Capped -> "capped" | P.Stake -> "stake"

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

let sets =
  List.concat_map (fun n ->
    T.make_validator_set (List.init n member)
    :: List.map (fun weight -> T.build_validator_set ~weighted:true (entries n weight))
      (positive n @ [fun _ -> 0]))
    (range 0 8)

let describe (vs : T.validator_set) =
  Printf.sprintf "n = %d weighted = %b weights = %s" vs.n vs.weighted
    (String.concat "," (List.map (fun (_, weight) -> Z.to_string weight) vs.weights))

let reason = function
  | Ok () -> "safe"
  | Error M.Few_seats -> "validator relief count intersection is unsafe"
  | Error M.Low_weight -> "validator relief weight intersection is unsafe"
  | Error M.No_proof -> failwith "historical relief selected weight-only rule"

let pinned current target =
  match K.C_relief.check_intersection current target with
  | Ok () -> "safe"
  | Error text -> text

let compare_pair (current : T.validator_set) (target : T.validator_set) =
  let expected = pinned current target in
  List.iter (fun (policy, rule) ->
    expect "policy does not select the rule"
      (P.select (Option.value policy ~default:(P.for_chain devnet)) height = rule);
    let where () =
      Printf.sprintf "rule = %s current %s target %s expected = %s" (rule_name rule)
        (describe current) (describe target) expected in
    let direct = reason (M.overlap (M.ruled rule ~declared:current.weighted current.weights)
      target.weights) in
    let spaced = reason (M.overlap (T.space ?policy ~chain_id:devnet ~epoch_id:height current)
      target.weights) in
    note (rule_name rule ^ " " ^ expected);
    if not (String.equal direct expected) then
      failwith ("test_overlap_pinned: overlap differs library = " ^ direct ^ " " ^ where ());
    if not (String.equal spaced expected) then
      failwith ("test_overlap_pinned: overlap by epoch differs library = " ^ spaced ^ " "
        ^ where ()))
    policies

let pairs () =
  List.iter (fun current -> List.iter (compare_pair current) sets) sets;
  List.iter (fun n ->
    List.iter (fun weight ->
      let members = entries n weight in
      let current = T.build_validator_set ~weighted:true members in
      List.iter (fun chosen ->
        compare_pair current (T.build_validator_set ~weighted:true chosen))
        (subsets members))
      (positive n))
    (range 4 8);
  List.iter (fun (_, rule) ->
    List.iter (fun text -> noted (rule_name rule ^ " " ^ text)) [
      "safe";
      "validator relief count intersection is unsafe";
      "validator relief weight intersection is unsafe";
    ])
    policies

let weighted_pairs () =
  let policy = P.with_seatless_epoch stake (Some 30L) in
  let sum members = List.fold_left (fun acc (_, weight) -> Z.add acc weight) Z.zero members in
  let reached level members = Z.geq (sum members) level in
  let restore current target =
    let activate_epoch = Int64.succ height in
    let fingerprint = String.make 64 '0' in
    let mark = Octra_consensus.C_relief.{height; round = min_round; activate_epoch;
      source_hash = Octra_consensus.C_config.validator_set_hash current;
      target_hash = Octra_consensus.C_config.validator_set_hash target;
      fingerprint; proof = []} in
    Octra_consensus.C_relief.restore ~policy ~chain_id:devnet ~height ~current
      ~activate_epoch ~target ~fingerprint mark in
  let capped (vs : T.validator_set) =
    let share = Z.div (Z.add vs.total_weight (Z.of_int 2)) (Z.of_int 3) in
    List.exists (fun (_, weight) -> Z.geq weight share) vs.weights in
  let pairs = List.concat_map (fun n -> List.concat_map (fun weight ->
    let members = entries n weight in
    let current = T.build_validator_set ~weighted:true members in
    let space = T.space ~policy ~chain_id:devnet ~epoch_id:30L current in
    List.filter_map (fun kept ->
      let target = T.build_validator_set ~weighted:true kept in
      if List.length kept >= 4 && List.length kept < n
         && M.overlap space target.weights = Ok () then Some (current, target)
      else None) (subsets members)) (positive n)) (range 5 8) in
  let checked, refused = List.fold_left (fun (checked, refused) (current, target) ->
    let limited = capped current || capped target in
    let expected = if limited then "validator weight reaches blocking threshold"
      else "validator relief proof is empty" in
    expect "weighted relief lost production path"
      (restore current target = Error expected);
    let members = T.weights_of_set current in
    let kept = T.weights_of_set target in
    let old_votes = List.filter (reached current.quorum_weight) (subsets members) in
    let new_votes = List.filter (reached target.quorum_weight) (subsets kept) in
    let shared left right =
      sum (List.filter (fun (member, _) -> List.exists (fun (other, _) ->
        member.T.address = other.T.address) right) left) in
    let faults = Z.sub current.total_weight current.quorum_weight in
    List.iter (fun left -> List.iter (fun right ->
      expect "weighted relief accepted disjoint quorum weight"
        (Z.gt (shared left right) faults)) new_votes) old_votes;
    List.iter (fun left -> List.iter (fun right ->
      expect "weighted target lost original fault budget"
        (Z.gt (shared left right) faults)) new_votes) new_votes;
    if limited then checked, refused + 1 else checked + 1, refused)
    (0, 0) pairs in
  expect "weighted relief has no accepted shrinking sets" (checked > 0);
  expect "weighted relief has no weight cap refusals" (refused > 0);
  let current = T.build_validator_set ~weighted:true (entries 7 (fun _ -> 10)) in
  let target = T.build_validator_set ~weighted:true (entries 6 (fun _ -> 10)) in
  expect "weighted relief kept unsafe count-only path"
    (restore current target
      = Error "validator relief weight intersection is unsafe");
  List.iter (fun weights ->
    expect "weighted relief accepted invalid target"
      (M.overlap (T.space ~policy ~chain_id:devnet ~epoch_id:30L current) weights = Error M.No_proof))
    [[]; ["v0", Z.zero]; ["v0", Z.minus_one]; ["v0", Z.one; "v0", Z.one]; ["v0", Z.of_int 71]]

let () =
  pairs ();
  weighted_pairs ();
  print_endline "status = pass test = overlap_pinned"