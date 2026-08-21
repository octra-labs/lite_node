(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module String_set = Set.Make (String)

type candidate = {
  address : string;
  pubkey : string;
  bond : Z.t;
  bonded_epoch : int64;
  ready_epoch : int64 option;
  exit_epoch : int64 option;
}

type member = {
  address : string;
  pubkey : string;
  weight : Z.t;
}

type parameters = {
  min_bond : Z.t;
  max_validators : int;
  activation_delay : int64;
  unbonding_epochs : int64;
}

type snapshot = {
  source_epoch : int64;
  activate_epoch : int64;
  validators : member list;
  total_weight : Z.t;
  quorum_weight : Z.t;
  fingerprint : string;
}

let validate_parameters (parameters : parameters) =
  if Z.sign parameters.min_bond <= 0 then
    Error "validator minimum bond must be positive"
  else if parameters.max_validators < 4 then
    Error "validator set requires at least four slots"
  else if Int64.compare parameters.activation_delay 0L <= 0 then
    Error "validator activation delay must be positive"
  else if Int64.compare parameters.unbonding_epochs 0L <= 0 then
    Error "validator unbonding period must be positive"
  else
    Ok ()

let validate_candidate (candidate : candidate) =
  if candidate.address = "" then
    Error "validator address is empty"
  else if String.length candidate.pubkey <> 32 then
    Error "validator public key must contain 32 bytes"
  else if Z.sign candidate.bond <= 0 then
    Error "validator bond must be positive"
  else if Int64.compare candidate.bonded_epoch 0L < 0 then
    Error "validator bonded epoch is negative"
  else
    match candidate.ready_epoch, candidate.exit_epoch with
    | Some ready_epoch, _
      when Int64.compare ready_epoch candidate.bonded_epoch < 0 ->
      Error "validator readiness predates bond"
    | _, Some exit_epoch
      when Int64.compare exit_epoch candidate.bonded_epoch < 0 ->
      Error "validator exit predates bond"
    | _ -> Ok ()

let quorum_weight total_weight =
  if Z.sign total_weight <= 0 then
    Error "validator total weight must be positive"
  else
    Ok Z.(add (div (mul total_weight (of_int 2)) (of_int 3)) one)

let compare_candidate (left : candidate) (right : candidate) =
  let by_bond = Z.compare right.bond left.bond in
  if by_bond <> 0 then by_bond
  else
    let by_address = String.compare left.address right.address in
    if by_address <> 0 then by_address
    else String.compare left.pubkey right.pubkey

let compare_selected incumbents (left : candidate) (right : candidate) =
  match String_set.mem left.address incumbents,
        String_set.mem right.address incumbents with
  | true, false -> -1
  | false, true -> 1
  | _ -> compare_candidate left right

let rec take count values =
  if count <= 0 then []
  else
    match values with
    | [] -> []
    | value :: rest -> value :: take (count - 1) rest

let validate_unique (candidates : candidate list) =
  let addresses = Hashtbl.create (List.length candidates) in
  let pubkeys = Hashtbl.create (List.length candidates) in
  let rec loop (remaining : candidate list) =
    match remaining with
    | [] -> Ok ()
    | candidate :: rest ->
      if Hashtbl.mem addresses candidate.address then
        Error "duplicate validator address"
      else if Hashtbl.mem pubkeys candidate.pubkey then
        Error "duplicate validator public key"
      else begin
        Hashtbl.add addresses candidate.address ();
        Hashtbl.add pubkeys candidate.pubkey ();
        loop rest
      end
  in
  loop candidates

let eligible ~(parameters : parameters) ~source_epoch (candidate : candidate) =
  Z.geq candidate.bond parameters.min_bond
  && Int64.compare candidate.bonded_epoch source_epoch <= 0
  && Option.fold
       ~none:false
       ~some:(fun ready_epoch -> Int64.compare ready_epoch source_epoch <= 0)
       candidate.ready_epoch
  && Option.is_none candidate.exit_epoch

let put_string buffer value =
  Buffer.add_string buffer (string_of_int (String.length value));
  Buffer.add_char buffer ':';
  Buffer.add_string buffer value

let put_int64 buffer value =
  put_string buffer (Int64.to_string value)

let put_z buffer value =
  put_string buffer (Z.to_string value)

let snapshot_fingerprint ~source_epoch ~activate_epoch validators =
  let buffer = Buffer.create 512 in
  put_string buffer "octra:validator_snapshot:v1";
  put_int64 buffer source_epoch;
  put_int64 buffer activate_epoch;
  List.iter
    (fun validator ->
      put_string buffer validator.address;
      put_string buffer validator.pubkey;
      put_z buffer validator.weight)
    validators;
  buffer
  |> Buffer.contents
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex

let source_epoch (parameters : parameters) activate_epoch =
  if Int64.compare activate_epoch parameters.activation_delay < 0 then
    Error "validator activation epoch precedes snapshot delay"
  else
    Ok (Int64.sub activate_epoch parameters.activation_delay)

let snapshot ?(incumbents=[]) (parameters : parameters) ~activate_epoch
    (candidates : candidate list) =
  match validate_parameters parameters with
  | Error _ as error -> error
  | Ok () ->
    begin
      match source_epoch parameters activate_epoch with
      | Error _ as error -> error
      | Ok source_epoch ->
        let rec validate_all = function
          | [] -> Ok ()
          | candidate :: rest ->
            begin
              match validate_candidate candidate with
              | Error _ as error -> error
              | Ok () -> validate_all rest
            end
        in
        begin
          match validate_all candidates with
          | Error _ as error -> error
          | Ok () ->
            begin
              match validate_unique candidates with
              | Error _ as error -> error
              | Ok () ->
                let incumbents =
                  List.fold_left
                    (fun set address -> String_set.add address set)
                    String_set.empty
                    incumbents
                in
                let validators =
                  candidates
                  |> List.filter (eligible ~parameters ~source_epoch)
                  |> List.sort (compare_selected incumbents)
                  |> take parameters.max_validators
                  |> List.map
                    (fun (candidate : candidate) -> {
                      address = candidate.address;
                      pubkey = candidate.pubkey;
                      weight = candidate.bond;
                    })
                in
                if List.length validators < 4 then
                  Error "validator snapshot has fewer than four eligible members"
                else
                  let total_weight =
                    List.fold_left
                      (fun total validator -> Z.add total validator.weight)
                      Z.zero
                      validators
                  in
                  begin
                    match quorum_weight total_weight with
                    | Error _ as error -> error
                    | Ok quorum_weight ->
                      Ok {
                        source_epoch;
                        activate_epoch;
                        validators;
                        total_weight;
                        quorum_weight;
                        fingerprint =
                          snapshot_fingerprint
                            ~source_epoch
                            ~activate_epoch
                            validators;
                      }
                  end
            end
        end
    end

let signed_weight snapshot signers =
  let weights = Hashtbl.create (List.length snapshot.validators) in
  List.iter
    (fun validator ->
      Hashtbl.add weights validator.address validator.weight)
    snapshot.validators;
  let seen = Hashtbl.create (List.length signers) in
  let rec loop total = function
    | [] -> Ok total
    | signer :: rest ->
      if Hashtbl.mem seen signer then
        loop total rest
      else
        match Hashtbl.find_opt weights signer with
        | None -> Error "validator signature is not in snapshot"
        | Some weight ->
          Hashtbl.add seen signer ();
          loop (Z.add total weight) rest
  in
  loop Z.zero signers

let has_quorum snapshot signers =
  match signed_weight snapshot signers with
  | Error _ as error -> error
  | Ok weight ->
    let n = List.length snapshot.validators in
    let f = (n - 1) / 3 in
    let quorum = if n < 4 then n else n - f in
    let signer_count = List.length (List.sort_uniq String.compare signers) in
    Ok (signer_count >= quorum && Z.geq weight snapshot.quorum_weight)

let leader_hash ~seed ~epoch_id ~round =
  let buffer = Buffer.create 128 in
  put_string buffer "octra:validator_leader:v1";
  put_string buffer seed;
  put_int64 buffer epoch_id;
  put_string buffer (string_of_int round);
  buffer
  |> Buffer.contents
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
  |> Z.of_string_base 16

let leader snapshot ~seed ~epoch_id ~round =
  if seed = "" then
    Error "validator leader seed is empty"
  else if Int64.compare epoch_id 0L < 0 then
    Error "validator leader epoch is negative"
  else if round < 0 then
    Error "validator leader round is negative"
  else if snapshot.validators = [] || Z.sign snapshot.total_weight <= 0 then
    Error "validator snapshot is empty"
  else
    let target =
      Z.erem
        (leader_hash ~seed ~epoch_id ~round)
        snapshot.total_weight
    in
    let rec select offset = function
      | [] -> Error "validator leader selection failed"
      | validator :: rest ->
        let next = Z.add offset validator.weight in
        if Z.lt target next then Ok validator
        else select next rest
    in
    select Z.zero snapshot.validators

let add_epoch left right =
  if Int64.compare right 0L < 0 then
    Error "epoch delta is negative"
  else
    let result = Int64.add left right in
    if Int64.compare result left < 0 then
      Error "epoch arithmetic overflow"
    else
      Ok result

let withdraw_epoch (parameters : parameters) (candidate : candidate) =
  match validate_parameters parameters with
  | Error _ as error -> error
  | Ok () ->
    begin
      match validate_candidate candidate with
      | Error _ as error -> error
      | Ok () ->
        match candidate.exit_epoch with
        | None -> Error "validator exit was not requested"
        | Some exit_epoch -> add_epoch exit_epoch parameters.unbonding_epochs
    end

let can_withdraw (parameters : parameters) ~current_epoch
    (candidate : candidate) =
  match withdraw_epoch parameters candidate with
  | Error _ as error -> error
  | Ok epoch -> Ok (Int64.compare current_epoch epoch >= 0)