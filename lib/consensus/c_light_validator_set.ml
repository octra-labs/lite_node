(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type validator = {
  address : string;
  pubkey : string;
  weight : Z.t;
}

type scheduled = {
  activate_epoch : int64;
  validators : validator list;
  weighted : bool;
}

type t = {
  chain_id : string;
  config_hash : string;
  validator_set_hash : string;
  n : int;
  f : int;
  quorum : int;
  total_weight : Z.t;
  quorum_weight : Z.t;
  weighted : bool;
  validators : validator list;
  scheduled : scheduled option;
  authors : (int64 * validator list) option;
  program_trust_hash : string option;
  runtime_profile_hash : string option;
}

let hash_ok s =
  String.length s = 32

let to_consensus_validator (v : validator) =
  C_types.{ address = v.address; pubkey = v.pubkey }

let of_consensus_validator (vs : C_types.validator_set)
    (v : C_types.validator_info) =
  let weight =
    match C_types.weight_of_addr vs v.address with
    | Some value -> value
    | None -> Z.zero
  in
  { address = v.address; pubkey = v.pubkey; weight }

let to_validator_set validators =
  validators
  |> List.map to_consensus_validator
  |> C_types.make_validator_set

let to_weighted_validator_set validators =
  validators
  |> List.map (fun validator ->
    to_consensus_validator validator, validator.weight)
  |> C_types.make_weighted_validator_set

let validator_set ?(authors = None) ~weighted validators =
  let result = if weighted then
    to_weighted_validator_set validators
  else
    Ok (to_validator_set validators) in
  Result.bind result (fun set -> match authors with
    | None -> Ok set
    | Some (height, entries) ->
      let entries = List.map (fun v -> to_consensus_validator v, v.weight) entries in
      let set = {set with C_types.authors = Some (height, entries)} in
      try Ok (C_codec.decode_validator_set (C_codec.encode_validator_set set))
      with _ -> Error "validator author context is invalid")

let scheduled_to_config (s : scheduled) =
  match validator_set ~weighted:s.weighted s.validators with
  | Error _ -> None
  | Ok validator_set ->
    Some C_config.{
      activate_epoch = s.activate_epoch;
      validator_set;
    }

let scheduled_of_config (s : C_config.scheduled) =
  {
    activate_epoch = s.C_config.activate_epoch;
    validators =
      List.map
        (of_consensus_validator s.validator_set)
        s.validator_set.validators;
    weighted = s.validator_set.weighted;
  }

let of_validator_set ~chain_id ~config_hash ?scheduled ?program_trust_hash
    ?runtime_profile_hash (vs : C_types.validator_set) =
  let validators = List.map (of_consensus_validator vs) vs.validators in
  {
    chain_id;
    config_hash;
    validator_set_hash = C_config.validator_set_hash vs;
    n = vs.n;
    f = vs.f;
    quorum = vs.quorum;
    total_weight = vs.total_weight;
    quorum_weight = vs.quorum_weight;
    weighted = vs.weighted;
    validators;
    scheduled = Option.map scheduled_of_config scheduled;
    authors = Option.map (fun (height, entries) -> height,
      List.map (fun ((member : C_types.validator_info), weight) ->
        {address = member.address; pubkey = member.pubkey; weight}) entries) vs.authors;
    program_trust_hash;
    runtime_profile_hash;
  }

let recomputed_config_hash t =
  match validator_set ~authors:t.authors ~weighted:t.weighted t.validators with
  | Error _ -> None
  | Ok vs ->
    let scheduled =
      match t.scheduled with
      | None -> Some None
      | Some value ->
        Option.map
          (fun scheduled -> Some scheduled)
          (scheduled_to_config value)
    in
    Option.map
      (fun scheduled ->
        C_config.hash
          ~chain_id:t.chain_id
          ~validator_set:vs
          ?scheduled
          ?program_trust_hash:t.program_trust_hash
          ?runtime_profile_hash:t.runtime_profile_hash
          ())
      scheduled

let verify t =
  match validator_set ~authors:t.authors ~weighted:t.weighted t.validators with
  | Error _ -> false
  | Ok vs ->
    t.chain_id <> ""
    && hash_ok t.config_hash
    && hash_ok t.validator_set_hash
    && t.n = vs.n
    && t.f = vs.f
    && t.quorum = vs.quorum
    && Z.equal t.total_weight vs.total_weight
    && Z.equal t.quorum_weight vs.quorum_weight
    && t.validator_set_hash = C_config.validator_set_hash vs
    && Option.fold ~none:true ~some:(fun (height, _) ->
      C_quorum_policy.linear_weight ~chain_id:t.chain_id ~epoch_id:height) t.authors
    && (match t.program_trust_hash with
        | None -> true
        | Some value -> String.length value = 32)
    && (match t.runtime_profile_hash with
        | None -> true
        | Some value -> String.length value = 32)
    && recomputed_config_hash t = Some t.config_hash