(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type activation = {
  anchor_epoch : int;
  anchor_state_root : string;
  activation_epoch : int;
}

let devnet_chain_id = "octra-devnet-9871-cluster"

let devnet_activation = {
  anchor_epoch = 1_306_321;
  anchor_state_root =
    "c19278aec1ff53c4796aea354c9cc7845d8421fc7d6ffe971b5fecea29299a04";
  activation_epoch = 1_306_547;
}

let devnet_weight_epoch = 1_425_168L

let activation_for_chain chain_id =
  if String.equal chain_id devnet_chain_id then Some devnet_activation
  else None

let active ~chain_id ~epoch_id =
  match activation_for_chain chain_id with
  | None -> Int64.compare epoch_id 0L >= 0
  | Some activation ->
    Int64.compare epoch_id (Int64.of_int activation.activation_epoch) >= 0

let count_floor_required ~chain_id ~epoch_id =
  active ~chain_id ~epoch_id
  && (not (String.equal chain_id devnet_chain_id)
      || Int64.compare epoch_id devnet_weight_epoch < 0)

let weight_cap_required ~chain_id ~epoch_id =
  active ~chain_id ~epoch_id

let rewind_allowed ~chain_id ~from_epoch ~to_epoch =
  match activation_for_chain chain_id with
  | None ->
    not
      (Int64.compare from_epoch 0L >= 0
       && Int64.compare to_epoch 0L < 0)
  | Some activation ->
    let threshold = Int64.of_int activation.activation_epoch in
    let crosses value =
      Int64.compare from_epoch value >= 0
      && Int64.compare to_epoch value < 0
    in
    not
      (crosses threshold
       || (String.equal chain_id devnet_chain_id
           && crosses devnet_weight_epoch))