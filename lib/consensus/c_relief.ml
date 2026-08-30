(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mark = {
  height : int64;
  round : int;
  activate_epoch : int64;
  source_hash : string;
  target_hash : string;
  fingerprint : string;
  proof : C_codec.round_sync list;
}

type decision =
  | Wait
  | Refuse of string
  | Apply of mark

let devnet = "octra-devnet-9871-cluster"
let activation_epoch = 1_380_000L
let min_round = 64
let max_lead = 64L

let active ~chain_id ~height =
  String.equal chain_id devnet
  && Int64.compare height activation_epoch >= 0

let hex_char = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let valid_fingerprint value =
  String.length value = 64
  && String.for_all hex_char value

let same_plan left right =
  Int64.equal left.height right.height
  && Int64.equal left.activate_epoch right.activate_epoch
  && String.equal left.source_hash right.source_hash
  && String.equal left.target_hash right.target_hash
  && String.equal left.fingerprint right.fingerprint

let same_member current target member =
  match
    C_types.pubkey_of_addr current member.C_types.address,
    C_types.weight_of_addr current member.address,
    C_types.weight_of_addr target member.address
  with
  | Some pubkey, Some old_weight, Some new_weight ->
    String.equal pubkey member.pubkey
    && Z.equal old_weight new_weight
  | _ -> false

let lowest_weight count target =
  target.C_types.weights
  |> List.map snd
  |> List.sort Z.compare
  |> fun weights ->
    let rec sum left total = function
      | _ when left <= 0 -> total
      | [] -> total
      | weight :: rest -> sum (left - 1) (Z.add total weight) rest
    in
    sum count Z.zero weights

let check_intersection current target =
  let shared = current.C_types.quorum + target.C_types.quorum - current.n in
  if shared <= current.f then
    Error "validator relief count intersection is unsafe"
  else
    let by_count = lowest_weight shared target in
    let by_weight =
      Z.sub
        (Z.add current.quorum_weight target.quorum_weight)
        current.total_weight
    in
    let overlap = Z.max by_count by_weight in
    let faults = Z.sub current.total_weight current.quorum_weight in
    if Z.leq overlap faults then
      Error "validator relief weight intersection is unsafe"
    else
      Ok ()

let check_set current target =
  if not current.C_types.weighted || not target.C_types.weighted then
    Error "validator relief requires weighted sets"
  else if current.n < 4 || target.n < 4 then
    Error "validator relief set is too small"
  else if target.n >= current.n then
    Error "validator relief target must shrink"
  else if not (List.for_all (same_member current target) target.validators) then
    Error "validator relief target changes a retained member"
  else
    check_intersection current target

let check_plan ~height ~current ~activate_epoch ~target ~fingerprint =
  let lead = Int64.sub activate_epoch height in
  if Int64.compare lead 0L <= 0 then
    Error "validator relief plan is not ahead"
  else if Int64.compare lead max_lead > 0 then
    Error "validator relief plan is too far ahead"
  else if not (valid_fingerprint fingerprint) then
    Error "validator relief fingerprint is invalid"
  else
    check_set current target

let check_proof ~chain_id ~height current proof =
  match proof with
  | [] -> Error "validator relief proof is empty"
  | first :: _ ->
    let round = first.C_codec.round in
    let proof =
      List.sort
        (fun left right ->
          String.compare left.C_codec.validator right.C_codec.validator)
        proof
    in
    let rec check prior = function
      | [] -> Ok ()
      | sync :: rest ->
        if Option.equal String.equal prior (Some sync.C_codec.validator) then
          Error "validator relief proof repeats a signer"
        else if not (String.equal sync.chain_id chain_id) then
          Error "validator relief proof chain differs"
        else if not (Int64.equal sync.epoch_id height) then
          Error "validator relief proof height differs"
        else if sync.round <> round || round < min_round then
          Error "validator relief proof round is invalid"
        else
          match C_types.pubkey_of_addr current sync.validator with
          | None -> Error "validator relief proof signer is unknown"
          | Some pubkey when not (C_hash.verify_round_sync ~pubkey_raw:pubkey sync) ->
            Error "validator relief proof signature is invalid"
          | Some _ -> check (Some sync.validator) rest
    in
    begin
      match check None proof with
      | Error _ as error -> error
      | Ok () ->
        let signers = List.map (fun sync -> sync.C_codec.validator) proof in
        begin
          match C_types.signed_weight current signers with
          | None -> Error "validator relief proof weight is invalid"
          | Some signed_weight ->
            let signer_count = List.length signers in
            if
              C_types.round_skip_reached_at
                ~chain_id
                ~epoch_id:height
                current
                ~signer_count
                ~signed_weight
            then Ok (round, proof)
            else Error "validator relief proof is below threshold"
        end
    end

let decide ~chain_id ~height ~current ~activate_epoch ~target ~fingerprint
    ~proof =
  if not (active ~chain_id ~height) then Wait
  else if Int64.compare height activate_epoch >= 0 then Wait
  else
    match check_plan ~height ~current ~activate_epoch ~target ~fingerprint with
    | Error reason -> Refuse reason
    | Ok () ->
      begin
        match proof with
        | [] -> Wait
        | _ ->
          match check_proof ~chain_id ~height current proof with
          | Error reason -> Refuse reason
          | Ok (round, proof) ->
            Apply {
              height;
              round;
              activate_epoch;
              source_hash = C_config.validator_set_hash current;
              target_hash = C_config.validator_set_hash target;
              fingerprint;
              proof;
            }
      end

let restore ~chain_id ~height ~current ~activate_epoch ~target ~fingerprint
    mark =
  if Int64.compare height mark.activate_epoch >= 0 then Ok None
  else if Int64.compare height mark.height < 0 then
    Error "validator relief marker is ahead of local height"
  else if not (active ~chain_id ~height:mark.height) then
    Error "validator relief marker predates activation"
  else if not (Int64.equal activate_epoch mark.activate_epoch) then
    Error "validator relief activation differs"
  else if not (String.equal fingerprint mark.fingerprint) then
    Error "validator relief fingerprint differs"
  else if
    not
      (String.equal
         (C_config.validator_set_hash current)
         mark.source_hash)
  then
    Error "validator relief source set differs"
  else if
    not
      (String.equal
         (C_config.validator_set_hash target)
         mark.target_hash)
  then
    Error "validator relief target set differs"
  else
    match
      check_plan
        ~height:mark.height
        ~current
        ~activate_epoch
        ~target
        ~fingerprint
    with
    | Error _ as error -> error
    | Ok () ->
      begin
        match check_proof ~chain_id ~height:mark.height current mark.proof with
        | Error _ as error -> error
        | Ok (round, _) when round <> mark.round ->
          Error "validator relief marker round differs"
        | Ok _ -> Ok (Some target)
      end