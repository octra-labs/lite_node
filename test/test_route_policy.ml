(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.C_types
module P = Octra_consensus.C_quorum_policy
module R = Octra_consensus.C_relief
module Pool = Octra_consensus.C_round_pool
module Codec = Octra_consensus.C_codec
module Config = Octra_consensus.C_config
module H = Octra_consensus.C_hash
module K = Quorum_pinned

let expect label value = if not value then failwith ("test_route_policy: " ^ label)

let devnet = "octra-devnet-9871-cluster"
let height = R.activation_epoch
let prior = P.make ~count_epoch:(Int64.succ height) ~stake_epoch:None ~round_epoch:None
let stake = P.make ~count_epoch:10L ~stake_epoch:(Some 20L) ~round_epoch:None
let policies = [Some prior, P.Prior; None, P.Capped; Some stake, P.Stake]
let rule_name = function P.Prior -> "prior" | P.Capped -> "capped" | P.Stake -> "stake"

let keys = List.init 5 (fun index ->
  let secret = String.make 32 (Char.chr (index + 41)) in
  let key = match Mirage_crypto_ec.Ed25519.priv_of_octets secret with
    | Ok key -> key | Error _ -> failwith "test_route_policy: key is invalid" in
  let pubkey = Mirage_crypto_ec.Ed25519.(pub_of_priv key |> pub_to_octets) in
  T.{address = "v" ^ string_of_int index; pubkey}, secret)

let members = List.map fst keys

let weighted weights =
  match T.make_weighted_validator_set
    (List.map2 (fun member weight -> member, Z.of_int weight)
      (List.filteri (fun index _ -> index < List.length weights) members) weights) with
  | Ok value -> value | Error reason -> failwith reason

let sync ~epoch_id ~round index =
  let member, secret = List.nth keys index in
  let unsigned = Codec.{chain_id = devnet; epoch_id; round; step = T.PrecommitStep;
    request = false; validator = member.address; signature = ""} in
  {unsigned with signature = H.sign_ed25519 ~priv_raw:secret
    ~msg:(H.round_sync_sign_bytes unsigned)}

let names proof = List.map (fun (item : Codec.round_sync) -> item.validator) proof

let skip_copy policy (vs : T.validator_set) signers =
  let signed_weight = match K.C_types.signed_weight vs signers with
    | Some weight -> weight | None -> failwith "test_route_policy: signer is unknown" in
  K.C_types.round_skip_reached_at ?policy ~chain_id:devnet ~epoch_id:height vs
    ~signer_count:(List.length signers) ~signed_weight

let witness () =
  let heavy = weighted [100; 1; 1; 1] in
  let plain = T.make_validator_set (List.filteri (fun index _ -> index < 4) members) in
  let read ?policy pool validator_set =
    names (Pool.witness ?policy pool ~chain_id:devnet ~epoch_id:height ~after_round:0
      ~through_round:9 ~validator_set) in
  let one = Pool.create () in
  Pool.add one (sync ~epoch_id:height ~round:3 0);
  let two = Pool.create () in
  List.iter (fun index -> Pool.add two (sync ~epoch_id:height ~round:3 index)) [0; 1];
  List.iter (fun (policy, rule, pool, validator_set, expected) ->
    expect "policy does not select the rule"
      (P.select (Option.value policy ~default:(P.for_chain devnet)) height = rule);
    let found = read ?policy pool validator_set in
    expect (Printf.sprintf "witness differs rule = %s found = %s expected = %s" (rule_name rule)
      (String.concat "," found) (String.concat "," expected))
      (found = expected);
    expect ("witness disagrees with the pinned skip rule = " ^ rule_name rule)
      (expected = [] || skip_copy policy validator_set expected)) [
    Some prior, P.Prior, one, heavy, ["v0"];
    None, P.Capped, one, heavy, [];
    Some stake, P.Stake, one, heavy, [];
    Some prior, P.Prior, two, heavy, ["v0"];
    None, P.Capped, two, heavy, ["v0"; "v1"];
    Some stake, P.Stake, two, heavy, ["v0"; "v1"];
    Some prior, P.Prior, two, plain, ["v0"; "v1"];
    None, P.Capped, two, plain, ["v0"; "v1"];
    Some stake, P.Stake, two, plain, [];
  ]

let relief () =
  let current = weighted [15; 1; 1; 1; 1] in
  let target = weighted [15; 1; 1; 1] in
  let fingerprint = String.make 64 'a' in
  let activate_epoch = Int64.succ height in
  let short = "validator relief proof is below threshold" in
  let proof signers = List.map (sync ~epoch_id:height ~round:R.min_round) signers in
  let restore ?policy mark =
    match R.restore ?policy ~chain_id:devnet ~height ~current ~activate_epoch ~target
      ~fingerprint mark with
    | Ok (Some set) when Config.validator_set_hash set = Config.validator_set_hash target ->
      "restored"
    | Ok (Some _) -> "another set"
    | Ok None -> "expired"
    | Error reason -> reason in
  let seated = match R.decide ~chain_id:devnet ~height ~current ~activate_epoch ~target
    ~fingerprint ~proof:(proof [0; 1]) with
    | R.Apply mark -> mark
    | R.Refuse reason -> failwith ("test_route_policy: two signers refused reason = " ^ reason)
    | R.Wait -> failwith "test_route_policy: two signers wait" in
  expect "relief mark differs" (names seated.proof = ["v0"; "v1"] && seated.round = R.min_round);
  expect "one heavy signer passed the seat floor"
    (R.decide ~chain_id:devnet ~height ~current ~activate_epoch ~target ~fingerprint
      ~proof:(proof [0]) = R.Refuse short);
  let heavy = {seated with proof = proof [0]} in
  List.iter (fun (policy, rule) ->
    List.iter (fun (label, mark, signers) ->
      let expected = if skip_copy policy current signers then "restored" else short in
      let found = restore ?policy mark in
      expect (Printf.sprintf "relief restore differs rule = %s proof = %s found = %s expected = %s"
        (rule_name rule) label found expected)
        (String.equal found expected))
      ["two signers", seated, ["v0"; "v1"]; "one heavy signer", heavy, ["v0"]])
    policies;
  expect "restore outcomes differ from the rules"
    (List.map (fun (policy, _) -> restore ?policy heavy) policies = ["restored"; short; short]
      && List.for_all (fun (policy, _) -> restore ?policy seated = "restored") policies)

let () =
  witness ();
  relief ();
  print_endline "status = pass test = route_policy"