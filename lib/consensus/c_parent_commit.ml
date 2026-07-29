(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

type t = parent_commit

type expected = {
  chain_id : string;
  epoch_id : int64;
  proposal_id : string;
  state_root : string;
  validator_set_hash : string;
}

type verdict =
  | Valid
  | Invalid of string

type participant = {
  address : string;
  pubkey : string;
  weight : Z.t;
}

let canonical_certificate
    (certificate : commit_certificate) : commit_certificate =
  {
    certificate with
    precommits =
      List.sort
        (fun (left : vote) (right : vote) ->
          String.compare left.validator right.validator)
        certificate.precommits;
  }

let canonical value =
  {
    value with
    certificate = canonical_certificate value.certificate;
  }

let encode value =
  value
  |> canonical
  |> C_codec.encode_parent_commit

let decode payload =
  let value = C_codec.decode_parent_commit payload in
  let canonical_value = canonical value in
  if encode canonical_value <> payload then
    failwith "parent commit is not canonical";
  canonical_value

let hash value =
  C_hash.parent_commit_hash value

let participants value =
  value.certificate.precommits
  |> List.map (fun (vote : vote) -> vote.validator)
  |> List.sort_uniq String.compare

let signed_weight value =
  C_types.signed_weight value.validator_set (participants value)

let participant_set value =
  let validators =
    value.validator_set.validators
    |> List.fold_left
         (fun table (validator : validator_info) ->
           (validator.address, validator.pubkey) :: table)
         []
  in
  let rec build result = function
    | [] -> Ok (List.rev result)
    | address :: rest ->
      begin
        match
          List.assoc_opt address validators,
          C_types.weight_of_addr value.validator_set address
        with
        | Some pubkey, Some weight when Z.sign weight > 0 ->
          build ({ address; pubkey; weight } :: result) rest
        | _ -> Error "parent commit participant is not in validator set"
      end
  in
  build [] (participants value)

let proposer value =
  let address = value.certificate.header.creator_addr in
  match
    List.find_opt
      (fun (validator : validator_info) ->
        String.equal validator.address address)
      value.validator_set.validators,
    C_types.weight_of_addr value.validator_set address
  with
  | Some validator, Some weight when Z.sign weight > 0 ->
    Ok { address; pubkey = validator.pubkey; weight }
  | _ -> Error "parent commit proposer is not in validator set"

let same_block left right =
  Int64.equal left.certificate.epoch_id right.certificate.epoch_id
  && left.certificate.proposal_id = right.certificate.proposal_id
  && left.certificate.header = right.certificate.header

let validate expected value =
  let value = canonical value in
  let certificate = value.certificate in
  if certificate.chain_id <> expected.chain_id then
    Invalid "chain_id"
  else if not (Int64.equal certificate.epoch_id expected.epoch_id) then
    Invalid "epoch_id"
  else if certificate.proposal_id <> expected.proposal_id then
    Invalid "proposal_id"
  else if certificate.header.proposed_state_root <> expected.state_root then
    Invalid "state_root"
  else if
    C_config.validator_set_hash value.validator_set
    <> expected.validator_set_hash
  then
    Invalid "validator_set"
  else
    let verify_vote (vote : vote) =
      match
        C_types.pubkey_of_addr
          value.validator_set
          vote.validator
      with
      | None -> false
      | Some pubkey -> C_hash.verify_vote ~pubkey_raw:pubkey vote
    in
    match C_qc.validate_certificate
      ~chain_id:expected.chain_id
      ~validator_set:value.validator_set
      ~verify_vote
      certificate with
    | C_qc.Valid -> Valid
    | C_qc.Invalid reason -> Invalid ("qc_" ^ reason)