(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

let devnet_chain_id = "octra-devnet-9871-cluster"
let mainnet_chain_id = "octra-mainnet"

let devnet_activation = {
  anchor_epoch = 1_311_424;
  anchor_state_root =
    "e70bf274a6590f859d72bd2da171193b99d79e5ff44e78bb91f6cad3cc65fdc6";
  activation_epoch = 1_320_000;
}

let historical_activations = [devnet_chain_id, devnet_activation]

let activation_for_chain chain_id =
  historical_activations
  |> List.find_map (fun (candidate, activation) ->
       if String.equal chain_id candidate then Some activation else None)

let rule_for_epoch ~chain_id ~epoch_id =
  match activation_for_chain chain_id with
  | None when String.equal chain_id mainnet_chain_id -> Epoch_time.Historical
  | None -> Epoch_time.Uniform
  | Some activation ->
    if Int64.compare epoch_id (Int64.of_int activation.activation_epoch) >= 0
    then Epoch_time.Uniform
    else Epoch_time.Historical