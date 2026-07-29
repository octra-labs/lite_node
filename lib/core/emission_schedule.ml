(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Inactive
  | Security_curve of {
      activation_epoch : int;
    }

type binding = {
  initial : Z.t;
  remaining : Z.t;
  retired : Z.t;
  active : bool;
  activated : bool;
  writes : (string * string) list;
}

let duration_epochs = 47_304_000
let standard_key = "emission_standard"
let activation_key = "emission_activation_epoch"
let initial_key = "emission_initial"
let retired_key = "supply_retired"
let standard_name = "security_curve"
let growth_weight = Z.of_int 17
let decay_weight = Z.of_int 7
let weight_denominator = Z.of_int 10

let parse_epoch label raw =
  try
    let value = int_of_string raw in
    if value < 0 then Error (label ^ " must be nonnegative")
    else Ok value
  with _ ->
    Error ("invalid " ^ label)

let of_env env =
  match env "OCTRA_EMISSION_PROFILE" with
  | Some value when value <> "" ->
    Error "OCTRA_EMISSION_PROFILE is not supported"
  | _ ->
    begin
      match env "OCTRA_EMISSION_ACTIVATION_EPOCH" with
      | None | Some "" -> Ok Inactive
      | Some raw ->
        Result.map
          (fun activation_epoch -> Security_curve { activation_epoch })
          (parse_epoch "emission activation epoch" raw)
    end

let of_env_exn env =
  match of_env env with
  | Ok value -> value
  | Error error -> failwith error

let name = function
  | Inactive -> "inactive"
  | Security_curve _ -> standard_name

let consensus_id = function
  | Inactive -> "inactive"
  | Security_curve _ ->
    String.concat ":" [
      standard_name;
      string_of_int duration_epochs;
      Z.to_string growth_weight;
      Z.to_string decay_weight;
      Z.to_string weight_denominator;
    ]

let activation_epoch = function
  | Inactive -> None
  | Security_curve value -> Some value.activation_epoch

let parse_nonnegative label raw =
  try
    let value = Z.of_string raw in
    if Z.sign value < 0 then Error (label ^ " must be nonnegative")
    else Ok value
  with _ ->
    Error ("invalid " ^ label)

let validate_stored ~activation_epoch ~epoch_id ~remaining ~headroom
    standard activation initial retired =
  if not (String.equal standard standard_name) then
    Error "emission standard metadata mismatch"
  else if epoch_id < activation_epoch then
    Error "emission activation is ahead of canonical epoch"
  else if Z.sign headroom < 0 then
    Error "emission headroom is negative"
  else if Z.gt remaining headroom then
    Error "emission remaining exceeds supply headroom"
  else
    match parse_epoch "stored emission activation epoch" activation with
    | Error error -> Error error
    | Ok stored_activation when stored_activation <> activation_epoch ->
      Error "emission activation metadata mismatch"
    | Ok _ ->
      begin
        match parse_nonnegative "emission initial reserve" initial with
        | Error error -> Error error
        | Ok initial when Z.gt remaining initial ->
          Error "emission remaining exceeds initial reserve"
        | Ok initial ->
          begin
            match parse_nonnegative "retired supply" retired with
            | Error error -> Error error
            | Ok retired when not (Z.equal (Z.add remaining retired) headroom) ->
              Error "supply envelope metadata mismatch"
            | Ok retired ->
              Ok {
                initial;
                remaining;
                retired;
                active = true;
                activated = false;
                writes = [];
              }
          end
      end

let activation_reserve ~remaining ~headroom =
  if Z.sign headroom < 0 then
    Error "emission headroom is negative"
  else if Z.sign remaining < 0 then
    Error "emission remaining is negative"
  else if Z.gt remaining headroom then
    Error "emission remaining exceeds supply headroom"
  else if Z.gt remaining Z.zero then
    Ok remaining
  else
    Ok headroom

let bind ~schedule ~epoch_id ~remaining ~headroom ~stored_standard
    ~stored_activation ~stored_initial ~stored_retired =
  match schedule with
  | Inactive ->
    begin
      match stored_standard, stored_activation, stored_initial, stored_retired with
      | None, None, None, None ->
        Ok {
          initial = remaining;
          remaining;
          retired = Z.zero;
          active = false;
          activated = false;
          writes = [];
        }
      | _ ->
        Error "active emission standard requires activation configuration"
    end
  | Security_curve { activation_epoch } ->
    begin
      match stored_standard, stored_activation, stored_initial, stored_retired with
      | Some standard, Some activation, Some initial, Some retired ->
        validate_stored
          ~activation_epoch
          ~epoch_id
          ~remaining
          ~headroom
          standard
          activation
          initial
          retired
      | None, None, None, None when epoch_id < activation_epoch ->
        Ok {
          initial = remaining;
          remaining;
          retired = Z.zero;
          active = false;
          activated = false;
          writes = [];
        }
      | None, None, None, None when epoch_id = activation_epoch ->
        Result.map
          (fun initial ->
            let retired = Z.sub headroom initial in
            {
              initial;
              remaining = initial;
              retired;
              active = true;
              activated = true;
              writes = [
                standard_key, standard_name;
                activation_key, string_of_int activation_epoch;
                initial_key, Z.to_string initial;
                retired_key, Z.to_string retired;
              ];
            })
          (activation_reserve ~remaining ~headroom)
      | None, None, None, None ->
        Error "emission activation metadata missing"
      | _ ->
        Error "incomplete emission activation metadata"
    end

let cumulative initial elapsed =
  let bounded = max 0 (min duration_epochs elapsed) in
  let k = Z.of_int bounded in
  let duration = Z.of_int duration_epochs in
  let growth = Z.mul growth_weight (Z.mul duration k) in
  let decay = Z.mul decay_weight (Z.mul k k) in
  let weight = Z.sub growth decay in
  let denominator =
    Z.mul weight_denominator (Z.mul duration duration)
  in
  Z.div
    (Z.mul initial weight)
    denominator

let curve_reward ~activation_epoch ~epoch_id ~remaining ~initial =
  let elapsed = epoch_id - activation_epoch in
  if elapsed < 0 then Ok Z.zero
  else
    let issued = cumulative initial elapsed in
    let expected_remaining = Z.sub initial issued in
    if not (Z.equal remaining expected_remaining) then
      Error "emission schedule state mismatch"
    else if elapsed >= duration_epochs then
      Ok Z.zero
    else
      let next_issued = cumulative initial (elapsed + 1) in
      let reward = Z.sub next_issued issued in
      if Z.sign reward < 0 || Z.gt reward remaining then
        Error "emission schedule reward out of bounds"
      else
        Ok reward

let reward ~schedule ~epoch_id binding =
  match schedule with
  | Inactive -> Ok (Some Z.zero)
  | Security_curve { activation_epoch } ->
    Result.map
      (fun value -> Some value)
      (curve_reward
         ~activation_epoch
         ~epoch_id
         ~remaining:binding.remaining
         ~initial:binding.initial)