(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type active = {
  activation_epoch : int;
  parameters : Validator_admission.parameters;
  snapshot_interval : int64;
  evidence_epochs : int64;
}

type t =
  | Inactive
  | Bonded of active

let env_name = "OCTRA_VALIDATOR_ADMISSION_ACTIVATION_EPOCH"
let standard_name = "bonded_stake"
let min_bond = Z.of_int 1_000_000
let max_validators = 64
let activation_delay = 64L
let snapshot_interval = 64L
let evidence_epochs = 86_400L
let unbonding_epochs = Int64.mul evidence_epochs 2L

let parameters = {
  Validator_admission.min_bond;
  max_validators;
  activation_delay;
  unbonding_epochs;
}

let parse_activation raw =
  try
    let epoch = int_of_string raw in
    if epoch < Int64.to_int activation_delay then
      Error "validator admission activation precedes snapshot delay"
    else
      Ok epoch
  with _ ->
    Error "invalid validator admission activation epoch"

let of_env getenv =
  match getenv env_name with
  | None
  | Some "" -> Ok Inactive
  | Some raw ->
    Result.map
      (fun activation_epoch ->
        Bonded {
          activation_epoch;
          parameters;
          snapshot_interval;
          evidence_epochs;
        })
      (parse_activation raw)

let of_env_exn getenv =
  match of_env getenv with
  | Ok value -> value
  | Error error -> failwith error

let activation_epoch = function
  | Inactive -> None
  | Bonded value -> Some value.activation_epoch

let consensus_id = function
  | Inactive -> "inactive"
  | Bonded value ->
    String.concat ":" [
      standard_name;
      string_of_int value.activation_epoch;
      Z.to_string value.parameters.min_bond;
      string_of_int value.parameters.max_validators;
      Int64.to_string value.parameters.activation_delay;
      Int64.to_string value.snapshot_interval;
      Int64.to_string value.parameters.unbonding_epochs;
      Int64.to_string value.evidence_epochs;
    ]

let lifecycle_enabled = function
  | Inactive -> false
  | Bonded _ -> true

let snapshot_activation policy ~source_epoch =
  match policy with
  | Inactive -> None
  | Bonded value ->
    let activation = Int64.of_int value.activation_epoch in
    let first_source =
      Int64.sub activation value.parameters.activation_delay
    in
    if Int64.compare source_epoch first_source < 0 then
      None
    else
      let elapsed = Int64.sub source_epoch first_source in
      if Int64.rem elapsed value.snapshot_interval <> 0L then
        None
      else
        Some (Int64.add source_epoch value.parameters.activation_delay)

let snapshot_at policy ~source_epoch ~cadence ~delay =
  match policy with
  | Inactive -> None
  | Bonded value ->
    let activation = Int64.of_int value.activation_epoch in
    let first_source = Int64.sub activation delay in
    if Int64.compare source_epoch first_source < 0 then
      None
    else
      let elapsed = Int64.sub source_epoch first_source in
      if Int64.rem elapsed cadence <> 0L then
        None
      else
        Some (Int64.add source_epoch delay)