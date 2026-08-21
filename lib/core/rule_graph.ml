(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type mode = Prior | Active

type root_read = Missing | Root of string | Unreadable of string

type fault =
  | Anchor_missing of int
  | Anchor_mismatch of {
      epoch : int;
      expected : string;
      actual : string;
    }
  | Anchor_unreadable of {
      epoch : int;
      reason : string;
    }

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

type t = {
  chain_id : string;
  ready_config_hash : string option;
  circle_activation : activation option;
  wasm_compute_activation : activation option;
  validator_quorum_activation : activation option;
  epoch_time_activation : activation option;
  owner_migration_activation : activation option;
  private_payload_activation : activation option;
  set_fold_activation : activation option;
  validator_ready_activation : activation option;
  set_fold_cap_activation : activation option;
  object_cost_activation : activation option;
  root_at : int -> root_read;
}

let devnet_chain_id = "octra-devnet-9871-cluster"

let devnet_circle_activation = {
  anchor_epoch = 1_293_376;
  anchor_state_root =
    "564797e4554eece839e606a431c3f1251679949c48dea039270dd8c6e706aab9";
  activation_epoch = 1_299_000;
}

let devnet_private_transition_activation = {
  anchor_epoch = 1_325_398;
  anchor_state_root =
    "257254d363a6864cb28461d80b0d40885e561ee3d17a97bfccb3af906a010229";
  activation_epoch = 1_330_000;
}

let devnet_owner_migration_activation = devnet_private_transition_activation

let devnet_wasm_compute_activation = devnet_private_transition_activation

let devnet_private_payload_activation = devnet_private_transition_activation

let devnet_set_fold_activation = {
  anchor_epoch = 1_325_398;
  anchor_state_root =
    "257254d363a6864cb28461d80b0d40885e561ee3d17a97bfccb3af906a010229";
  activation_epoch = 1_334_000;
}

let devnet_validator_transition_activation = {
  anchor_epoch = 1_353_962;
  anchor_state_root =
    "1def67e4c5886e28f648972ab3275901e66def42147b1b8b0d57a7578d19dec4";
  activation_epoch = 1_380_000;
}

let owner_migration_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_owner_migration_activation
  else
    None

let wasm_compute_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_wasm_compute_activation
  else
    None

let private_payload_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_private_payload_activation
  else
    None

let set_fold_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_set_fold_activation
  else
    None

let validator_ready_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_validator_transition_activation
  else
    None

let set_fold_cap_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_validator_transition_activation
  else
    None

let object_cost_activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then
    Some devnet_validator_transition_activation
  else
    None

let validator_quorum_activation_for_chain chain_id : activation option =
  match Octra_consensus.C_quorum_policy.activation_for_chain chain_id with
  | None -> None
  | Some source ->
    Some {
      anchor_epoch = source.anchor_epoch;
      anchor_state_root = source.anchor_state_root;
      activation_epoch = source.activation_epoch;
    }

let epoch_time_activation_for_chain chain_id : activation option =
  match Octra_consensus.C_epoch_time_policy.activation_for_chain chain_id with
  | None -> None
  | Some source ->
    Some {
      anchor_epoch = source.anchor_epoch;
      anchor_state_root = source.anchor_state_root;
      activation_epoch = source.activation_epoch;
    }

let make ~ready_config_hash ~chain_id ~root_at =
  let circle_activation =
    if String.equal chain_id devnet_chain_id then
      Some devnet_circle_activation
    else
      None
  in
  {
    chain_id;
    ready_config_hash;
    circle_activation;
    wasm_compute_activation = wasm_compute_activation_for_chain chain_id;
    validator_quorum_activation =
      validator_quorum_activation_for_chain chain_id;
    epoch_time_activation = epoch_time_activation_for_chain chain_id;
    owner_migration_activation = owner_migration_activation_for_chain chain_id;
    private_payload_activation = private_payload_activation_for_chain chain_id;
    set_fold_activation = set_fold_activation_for_chain chain_id;
    validator_ready_activation = validator_ready_activation_for_chain chain_id;
    set_fold_cap_activation = set_fold_cap_activation_for_chain chain_id;
    object_cost_activation = object_cost_activation_for_chain chain_id;
    root_at;
  }

let create ~chain_id ~root_at =
  make ~ready_config_hash:None ~chain_id ~root_at

let create_ready ~ready_config_hash ~chain_id ~root_at =
  make ~ready_config_hash:(Some ready_config_hash) ~chain_id ~root_at

let circle_activation t = t.circle_activation
let wasm_compute_activation t = t.wasm_compute_activation
let validator_quorum_activation t = t.validator_quorum_activation
let epoch_time_activation t = t.epoch_time_activation
let owner_migration_activation t = t.owner_migration_activation
let private_payload_activation t = t.private_payload_activation
let set_fold_activation t = t.set_fold_activation
let validator_ready_activation t = t.validator_ready_activation
let set_fold_cap_activation t = t.set_fold_cap_activation
let object_cost_activation t = t.object_cost_activation
let ready_config_hash t = t.ready_config_hash

let root_after_floor ~chain_id ~floor_epoch ~epoch =
  if String.equal chain_id devnet_chain_id
     && epoch = devnet_circle_activation.anchor_epoch
     && floor_epoch >= devnet_circle_activation.activation_epoch then
    Some devnet_circle_activation.anchor_state_root
  else
    let activations = [
      validator_quorum_activation_for_chain chain_id;
      epoch_time_activation_for_chain chain_id;
      owner_migration_activation_for_chain chain_id;
      wasm_compute_activation_for_chain chain_id;
      private_payload_activation_for_chain chain_id;
      set_fold_activation_for_chain chain_id;
      validator_ready_activation_for_chain chain_id;
      set_fold_cap_activation_for_chain chain_id;
      object_cost_activation_for_chain chain_id;
    ] in
    List.find_map
      (function
        | Some activation
          when epoch = activation.anchor_epoch
               && floor_epoch >= activation.anchor_epoch ->
          Some activation.anchor_state_root
        | Some _
        | None -> None)
      activations

let verify_anchor t activation =
  match t.root_at activation.anchor_epoch with
  | Missing ->
    Error (Anchor_missing activation.anchor_epoch)
  | Unreadable reason ->
    Error
      (Anchor_unreadable {
         epoch = activation.anchor_epoch;
         reason;
       })
  | Root actual when String.equal actual activation.anchor_state_root ->
    Ok ()
  | Root actual ->
    Error
      (Anchor_mismatch {
         epoch = activation.anchor_epoch;
         expected = activation.anchor_state_root;
         actual;
       })

let mode t activation ~epoch =
  match activation with
  | None -> Ok Prior
  | Some activation when epoch < activation.activation_epoch -> Ok Prior
  | Some activation ->
    Result.map (fun () -> Active) (verify_anchor t activation)

let circle t ~epoch = mode t t.circle_activation ~epoch

let wasm_compute t ~epoch =
  match t.wasm_compute_activation with
  | Some activation -> mode t (Some activation) ~epoch
  | None -> Ok Active

let validator_quorum t ~epoch =
  match t.validator_quorum_activation with
  | Some activation -> mode t (Some activation) ~epoch
  | None ->
    if
      Octra_consensus.C_quorum_policy.active
        ~chain_id:t.chain_id
        ~epoch_id:(Int64.of_int epoch)
    then Ok Active
    else Ok Prior

let epoch_time t ~epoch =
  match t.epoch_time_activation with
  | Some activation -> mode t (Some activation) ~epoch
  | None ->
    match
      Octra_consensus.C_epoch_time_policy.rule_for_epoch
        ~chain_id:t.chain_id
        ~epoch_id:(Int64.of_int epoch)
    with
    | Octra_consensus.Epoch_time.Uniform -> Ok Active
    | Octra_consensus.Epoch_time.Historical -> Ok Prior

let owner_migration t ~epoch =
  match t.owner_migration_activation with
  | Some activation -> mode t (Some activation) ~epoch
  | None -> Ok Prior

let private_payload t ~epoch =
  match t.private_payload_activation with
  | Some activation -> mode t (Some activation) ~epoch
  | None -> Ok Active

let set_fold t ~epoch =
  match t.set_fold_activation with
  | Some activation -> mode t (Some activation) ~epoch
  | None -> Ok Prior

let validator_ready t ~epoch =
  mode t t.validator_ready_activation ~epoch

let set_fold_cap t ~epoch =
  mode t t.set_fold_cap_activation ~epoch

let object_cost t ~epoch =
  mode t t.object_cost_activation ~epoch

let fault_message = function
  | Anchor_missing epoch ->
    Printf.sprintf "rule anchor missing epoch = %d" epoch
  | Anchor_mismatch { epoch; expected; actual } ->
    Printf.sprintf
      "rule anchor mismatch epoch = %d expected = %s actual = %s"
      epoch
      expected
      actual
  | Anchor_unreadable { epoch; reason } ->
    Printf.sprintf
      "rule anchor unreadable epoch = %d reason = %s"
      epoch
      reason