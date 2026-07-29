(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Allow
  | Guard

let of_env env =
  match env "OCTRA_EMISSION_GUARD" with
  | Some "1" -> Guard
  | _ -> Allow

let legacy_total_env = "OCTRA_LEGACY_TOTAL_SUPPLY_RAW"

let legacy_total env =
  env legacy_total_env

let remaining = function
  | None -> Ok Z.zero
  | Some raw ->
    try Ok (Z.of_string raw)
    with _ -> Error "invalid emission_remaining"

let check_remaining policy raw =
  match policy with
  | Allow -> Ok ()
  | Guard ->
    match remaining raw with
    | Error error -> Error error
    | Ok value when Z.gt value Z.zero ->
      Error "emission guard rejected nonzero remaining pool"
    | Ok _ -> Ok ()

let check_reward policy base_reward =
  match policy with
  | Allow -> Ok ()
  | Guard when Z.gt base_reward Z.zero ->
    Error "emission guard rejected positive base reward"
  | Guard -> Ok ()

let validate_state ~emission_remaining ~total_supply =
  if Z.sign emission_remaining < 0 then
    Error "negative emission_remaining"
  else if Z.sign total_supply < 0 then
    Error "negative total_supply"
  else if Z.gt total_supply Denomination.max_supply then
    Error "total_supply exceeds hard cap"
  else
    let headroom = Z.sub Denomination.max_supply total_supply in
    if Z.gt emission_remaining headroom then
      Error "emission_remaining exceeds supply headroom"
    else
      Ok headroom

type state = {
  emission_remaining : Z.t;
  total_supply : Z.t;
}

let amount_of_meta key = function
  | None -> Error ("missing " ^ key)
  | Some raw ->
    try Ok (Z.of_string raw)
    with _ -> Error ("invalid " ^ key)

let resolve_total ~policy ~stored ~legacy ~public_supply =
  let raw =
    match stored, policy, legacy with
    | Some raw, _, _ -> Ok raw
    | None, Guard, Some raw -> Ok raw
    | None, _, _ -> Error "missing total_supply"
  in
  match raw with
  | Error error -> Error error
  | Ok raw ->
    match amount_of_meta "total_supply" (Some raw) with
    | Error error -> Error error
    | Ok total_supply ->
      if Z.sign public_supply < 0 then
        Error "negative public supply"
      else if Z.gt public_supply total_supply then
        Error "public supply exceeds total supply"
      else
        match validate_state ~emission_remaining:Z.zero ~total_supply with
        | Error error -> Error error
        | Ok _ -> Ok total_supply

let state_of_meta ~emission_remaining ~total_supply =
  match remaining emission_remaining with
  | Error error -> Error error
  | Ok emission_remaining ->
    match amount_of_meta "total_supply" total_supply with
    | Error error -> Error error
    | Ok total_supply ->
      match validate_state ~emission_remaining ~total_supply with
      | Error error -> Error error
      | Ok _ -> Ok { emission_remaining; total_supply }

let runtime_state ~policy ~public_supply ~emission ~total ~legacy =
  match remaining emission with
  | Error error -> Error error
  | Ok emission_remaining ->
    match resolve_total ~policy ~stored:total ~legacy ~public_supply with
    | Error error -> Error error
    | Ok total_supply ->
      match validate_state ~emission_remaining ~total_supply with
      | Error error -> Error error
      | Ok _ -> Ok { emission_remaining; total_supply }

let hidden_supply ~total_supply ~public_supply =
  if Z.sign total_supply < 0 then
    Error "negative total supply"
  else if Z.gt total_supply Denomination.max_supply then
    Error "total supply exceeds hard cap"
  else if Z.sign public_supply < 0 then
    Error "negative public supply"
  else if Z.gt public_supply total_supply then
    Error "public supply exceeds total supply"
  else
    Ok (Z.sub total_supply public_supply)