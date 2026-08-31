(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epoch_exec = Octra_core.Epoch_exec
module Parent = Octra_consensus.C_parent_commit
module Reward_source = Octra_consensus.C_reward_source
module C_types = Octra_consensus.C_types

type t = Epoch_exec.reward_attribution = {
  proposer_addr : string;
  proposer_public_key : string option;
  validators : Epoch_exec.reward_validator list;
}

let fallback ~proposer_addr ~validator_pubkeys =
  {
    proposer_addr;
    proposer_public_key = List.assoc_opt proposer_addr validator_pubkeys;
    validators =
      List.map
        (fun (address, public_key) ->
          Epoch_exec.{
            address;
            public_key = Some public_key;
            weight = Z.one;
          })
        validator_pubkeys;
  }

let raw_public_key = function
  | None -> Ok None
  | Some encoded ->
    begin
      try
        let value = Base64.decode_exn encoded in
        if String.length value = 32 then Ok (Some value)
        else Error "reward public key length is invalid"
      with _ ->
        Error "reward public key is invalid"
    end

let source_member (validator : Epoch_exec.reward_validator) =
  match raw_public_key validator.public_key with
  | Error _ as error -> error
  | Ok reward_public_key ->
    Ok C_types.{
      reward_address = validator.address;
      reward_public_key;
      reward_weight = validator.weight;
    }

let source_members validators =
  let rec loop result = function
    | [] -> Ok (List.rev result)
    | validator :: rest ->
      begin
        match source_member validator with
        | Error _ as error -> error
        | Ok member -> loop (member :: result) rest
      end
  in
  loop [] validators

let to_source reward =
  match raw_public_key reward.proposer_public_key with
  | Error _ as error -> error
  | Ok reward_proposer_public_key ->
    begin
      match source_members reward.validators with
      | Error _ as error -> error
      | Ok reward_members ->
        Reward_source.validate C_types.{
          reward_proposer_addr = reward.proposer_addr;
          reward_proposer_public_key;
          reward_members;
        }
    end

let encoded_public_key =
  Option.map Base64.encode_exn

let of_source source =
  match Reward_source.validate source with
  | Error _ as error -> error
  | Ok source ->
    Ok {
      proposer_addr = source.reward_proposer_addr;
      proposer_public_key =
        encoded_public_key source.reward_proposer_public_key;
      validators =
        List.map
          (fun member ->
            Epoch_exec.{
              address = member.C_types.reward_address;
              public_key = encoded_public_key member.reward_public_key;
              weight = member.reward_weight;
            })
          source.reward_members;
    }

let reward_participant role =
  role = "validator" || role = "proposer_validator"

let legacy_current_reward ~proposer_addr ~validator_pubkeys header =
  let participant_addresses =
    header.Octra_core.Epochlog.reward_recipients
    |> List.filter (fun recipient ->
      reward_participant recipient.Octra_core.Epochlog.reward_role)
    |> List.map (fun recipient -> recipient.Octra_core.Epochlog.reward_addr)
    |> List.sort_uniq String.compare
  in
  let participant_addresses =
    if participant_addresses = []
       && header.validator_reward_each = "0"
    then
      List.map fst validator_pubkeys
    else
      participant_addresses
  in
  let rec members result = function
    | [] -> Ok (List.rev result)
    | address :: rest ->
      begin
        match List.assoc_opt address validator_pubkeys with
        | None ->
          Error ("legacy reward validator key is missing: " ^ address)
        | Some public_key ->
          members
            (Epoch_exec.{
              address;
              public_key = Some public_key;
              weight = Z.one;
            } :: result)
            rest
      end
  in
  match members [] participant_addresses with
  | Error _ as error -> error
  | Ok validators ->
    to_source {
      proposer_addr;
      proposer_public_key = List.assoc_opt proposer_addr validator_pubkeys;
      validators;
    }

let epoch_source ~validator_activation_epoch ~validator_pubkeys header =
  match header.Octra_core.Epochlog.reward_source with
  | Some source -> Reward_source.validate source
  | None ->
    let epoch_id = header.Octra_core.Epochlog.id in
    let proposer_addr =
      let recorded = header.proposer.creator_addr in
      if String.length recorded > 3 then recorded else header.finalized_by
    in
    let protocol =
      Octra_consensus.C_protocol.version_for_epoch (Int64.of_int epoch_id)
    in
    if
      Option.fold
        ~none:false
        ~some:(fun activation -> epoch_id >= activation)
        validator_activation_epoch
    then
      Error "weighted reward source is missing"
    else if
      protocol = Octra_consensus.C_types.proto_version_parent_legacy
    then
      fallback ~proposer_addr ~validator_pubkeys
      |> to_source
    else
      legacy_current_reward ~proposer_addr ~validator_pubkeys header

let of_parent_commit parent =
  match Parent.proposer parent, Parent.participant_set parent with
  | Error error, _
  | _, Error error -> Error error
  | Ok proposer, Ok participants ->
    Ok {
      proposer_addr = proposer.address;
      proposer_public_key = Some (Base64.encode_exn proposer.pubkey);
      validators =
        List.map
          (fun (participant : Parent.participant) ->
            Epoch_exec.{
              address = participant.address;
              public_key = Some (Base64.encode_exn participant.pubkey);
              weight = participant.weight;
            })
          participants;
    }

let legacy_finality_reward ~validator_set finalize =
  let validator_pubkeys =
    List.map
      (fun (validator : C_types.validator_info) ->
        validator.address,
        Base64.encode_exn validator.pubkey)
      validator_set.C_types.validators
  in
  fallback
    ~proposer_addr:finalize.C_types.header.creator_addr
    ~validator_pubkeys
  |> to_source
  |> fun source -> Result.bind source of_source

let bind_finality ~validator_set finalize supplied =
  match finalize.C_types.header.proto_version with
  | version when version = C_types.proto_version_parent_legacy ->
    legacy_finality_reward ~validator_set finalize
    |> fun expected ->
    Result.bind expected (fun expected ->
      if expected = supplied then Ok expected
      else Error "reward source does not match legacy finality")
  | version when version = C_types.proto_version_current ->
    begin
      match finalize.C_types.parent_commit with
      | None -> Error "reward parent commit is missing"
      | Some parent ->
        of_parent_commit parent
        |> fun expected ->
        Result.bind expected (fun expected ->
          if expected = supplied then Ok expected
          else Error "reward source does not match parent commit")
    end
  | _ -> Error "reward finality protocol is unsupported"

let resolve ~proposer_addr ~validator_pubkeys = function
  | None -> Ok (fallback ~proposer_addr ~validator_pubkeys)
  | Some parent -> of_parent_commit parent