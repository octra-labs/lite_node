(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type action =
  | Request of string list
  | Serve of string list
  | Receive of {
      hash : string;
      tx_json : string;
    }
  | Legacy

type tx_check =
  | Decoded of Octra_core.Transaction.t
  | Decode_error of string
  | Hash_mismatch of {
      advertised : string;
      actual : string;
    }

let plan_inv ?cfg ~has hashes =
  match cfg with
  | None -> Octra_net.P2p_tx_gossip_guard.plan_inv ~has hashes
  | Some cfg -> Octra_net.P2p_tx_gossip_guard.plan_inv ~cfg ~has hashes

let plan_get ?cfg ~has hashes =
  match cfg with
  | None -> Octra_net.P2p_tx_gossip_guard.plan_get ~has hashes
  | Some cfg -> Octra_net.P2p_tx_gossip_guard.plan_get ~cfg ~has hashes

let plan ?cfg ~has payload =
  try
    match Octra_net.P2p_tx_gossip.decode payload with
    | Octra_net.P2p_tx_gossip.Inv hashes -> Request (plan_inv ?cfg ~has hashes)
    | Octra_net.P2p_tx_gossip.Get hashes -> Serve (plan_get ?cfg ~has hashes)
    | Octra_net.P2p_tx_gossip.Tx { hash; tx_json } -> Receive { hash; tx_json }
  with _ -> Legacy

let check_tx ~hash ~tx_json =
  match Octra_core.Tx_payload.decode_admit (Yojson.Safe.from_string tx_json) with
  | Error e -> Decode_error e
  | Ok tx ->
    let actual = Octra_core.Transaction.hash tx in
    if actual <> hash then Hash_mismatch { advertised = hash; actual }
    else Decoded tx