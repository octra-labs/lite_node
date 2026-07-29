(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open C_types

module O = Octra_net.Oce1

let max_address_bytes = 128
let max_members = 4_096
let max_weight_bytes = 256

let valid_address value =
  String.length value > 3 && String.length value <= max_address_bytes

let valid_public_key = function
  | None -> true
  | Some value -> String.length value = 32

let canonical source =
  {
    source with
    reward_members =
      List.sort
        (fun left right ->
          String.compare left.reward_address right.reward_address)
        source.reward_members;
  }

let validate source =
  let source = canonical source in
  let addresses =
    List.map (fun member -> member.reward_address) source.reward_members
  in
  let public_keys =
    List.filter_map
      (fun member -> member.reward_public_key)
      source.reward_members
  in
  if not (valid_address source.reward_proposer_addr) then
    Error "reward proposer address is invalid"
  else if not (valid_public_key source.reward_proposer_public_key) then
    Error "reward proposer public key is invalid"
  else if source.reward_members = [] then
    Error "reward source has no members"
  else if List.length source.reward_members > max_members then
    Error "reward source has too many members"
  else if
    List.exists
      (fun member ->
        not (valid_address member.reward_address)
        || not (valid_public_key member.reward_public_key)
        || Z.sign member.reward_weight <= 0)
      source.reward_members
  then
    Error "reward source member is invalid"
  else if addresses <> List.sort_uniq String.compare addresses then
    Error "reward source has duplicate addresses"
  else if
    List.length public_keys
    <> List.length (List.sort_uniq String.compare public_keys)
  then
    Error "reward source has duplicate public keys"
  else
    Ok source

let encode_public_key buf =
  O.put_option O.put_bytes buf

let encode_member buf member =
  O.put_string buf member.reward_address;
  encode_public_key buf member.reward_public_key;
  O.put_string buf (Z.to_string member.reward_weight)

let encode_into buf source =
  match validate source with
  | Error error -> invalid_arg error
  | Ok source ->
    O.put_string buf source.reward_proposer_addr;
    encode_public_key buf source.reward_proposer_public_key;
    O.put_list encode_member buf source.reward_members

let decode_public_key cursor =
  O.get_option
    (fun cursor ->
      let value = O.get_bytes_bounded ~max:32 cursor in
      if String.length value <> 32 then
        failwith "reward source public key length is invalid";
      value)
    cursor

let decode_weight cursor =
  let raw = O.get_string_bounded ~max:max_weight_bytes cursor in
  try Z.of_string raw
  with _ -> failwith "reward source weight is invalid"

let decode_member cursor =
  let reward_address =
    O.get_string_bounded ~max:max_address_bytes cursor
  in
  let reward_public_key = decode_public_key cursor in
  let reward_weight = decode_weight cursor in
  {
    reward_address;
    reward_public_key;
    reward_weight;
  }

let decode_from cursor =
  let reward_proposer_addr =
    O.get_string_bounded ~max:max_address_bytes cursor
  in
  let reward_proposer_public_key = decode_public_key cursor in
  let reward_members =
    O.get_list_bounded ~max:max_members decode_member cursor
  in
  let source = {
    reward_proposer_addr;
    reward_proposer_public_key;
    reward_members;
  } in
  match validate source with
  | Error error -> failwith error
  | Ok canonical_source ->
    if source <> canonical_source then
      failwith "reward source is not canonical";
    canonical_source

let public_key_json = function
  | None -> `Null
  | Some value -> `String (Base64.encode_exn value)

let member_json member =
  `Assoc [
    "address", `String member.reward_address;
    "public_key", public_key_json member.reward_public_key;
    "weight", `String (Z.to_string member.reward_weight);
  ]

let to_yojson source =
  match validate source with
  | Error error -> invalid_arg error
  | Ok source ->
    `Assoc [
      "proposer_address", `String source.reward_proposer_addr;
      "proposer_public_key", public_key_json source.reward_proposer_public_key;
      "members", `List (List.map member_json source.reward_members);
    ]

let public_key_of_json = function
  | `Null -> Ok None
  | `String encoded ->
    begin
      try
        let value = Base64.decode_exn encoded in
        if String.length value = 32 then Ok (Some value)
        else Error "reward source public key is invalid"
      with _ ->
        Error "reward source public key is invalid"
    end
  | _ -> Error "reward source public key is invalid"

let member_of_json json =
  let open Yojson.Safe.Util in
  try
    match public_key_of_json (json |> member "public_key") with
    | Error _ as error -> error
    | Ok reward_public_key ->
      let weight_raw = json |> member "weight" |> to_string in
      if String.length weight_raw > max_weight_bytes then
        Error "reward source weight is invalid"
      else
        let reward_weight =
          try Ok (Z.of_string weight_raw)
          with _ -> Error "reward source weight is invalid"
        in
        Result.map
          (fun reward_weight ->
            {
              reward_address = json |> member "address" |> to_string;
              reward_public_key;
              reward_weight;
            })
          reward_weight
  with _ ->
    Error "reward source member is invalid"

let members_of_json values =
  let rec loop result = function
    | [] -> Ok (List.rev result)
    | value :: rest ->
      begin
        match member_of_json value with
        | Error _ as error -> error
        | Ok member -> loop (member :: result) rest
      end
  in
  loop [] values

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    match public_key_of_json (json |> member "proposer_public_key") with
    | Error _ as error -> error
    | Ok reward_proposer_public_key ->
      begin
        match members_of_json (json |> member "members" |> to_list) with
        | Error _ as error -> error
        | Ok reward_members ->
          validate {
            reward_proposer_addr =
              json |> member "proposer_address" |> to_string;
            reward_proposer_public_key;
            reward_members;
          }
      end
  with _ ->
    Error "reward source is invalid"