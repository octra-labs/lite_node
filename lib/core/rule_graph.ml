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
  circle_activation : activation option;
  validator_quorum_activation : activation option;
  root_at : int -> root_read;
}

let devnet_chain_id = "octra-devnet-9871-cluster"

let devnet_circle_activation = {
  anchor_epoch = 1_293_376;
  anchor_state_root =
    "564797e4554eece839e606a431c3f1251679949c48dea039270dd8c6e706aab9";
  activation_epoch = 1_299_000;
}

let validator_quorum_activation_for_chain chain_id : activation option =
  match Octra_consensus.C_quorum_policy.activation_for_chain chain_id with
  | None -> None
  | Some source ->
    Some {
      anchor_epoch = source.anchor_epoch;
      anchor_state_root = source.anchor_state_root;
      activation_epoch = source.activation_epoch;
    }

let create ~chain_id ~root_at =
  let circle_activation =
    if String.equal chain_id devnet_chain_id then
      Some devnet_circle_activation
    else
      None
  in
  {
    circle_activation;
    validator_quorum_activation =
      validator_quorum_activation_for_chain chain_id;
    root_at;
  }

let circle_activation t = t.circle_activation
let validator_quorum_activation t = t.validator_quorum_activation

let root_after_floor ~chain_id ~floor_epoch ~epoch =
  if String.equal chain_id devnet_chain_id
     && epoch = devnet_circle_activation.anchor_epoch
     && floor_epoch >= devnet_circle_activation.activation_epoch then
    Some devnet_circle_activation.anchor_state_root
  else
    match validator_quorum_activation_for_chain chain_id with
    | Some activation
      when epoch = activation.anchor_epoch
           && floor_epoch >= activation.anchor_epoch ->
      Some activation.anchor_state_root
    | _ -> None

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

let validator_quorum t ~epoch =
  mode t t.validator_quorum_activation ~epoch

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