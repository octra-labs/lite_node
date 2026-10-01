(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Transaction
module Rest = Node_rest_facade

type staged = { hash : string; tx : Tx.t }

type network = {
  broadcast : Octra_net.P2p_frame.frame -> unit;
  post : Tx.t -> (unit, Set_post.failure) result Lwt.t;
}

let stage ~bft_mode runtime ledger tx =
  let ( let* ) = Result.bind in
  if tx.Tx.op_type <> Tx.ValidatorReady then Error "validator duty operation required"
  else
    let* tx = Rest.prepare_tx ledger tx |> Result.map_error snd in
    let* hash = Rest.add_tx_to_staging ~relay:false ~bft_mode runtime ledger tx in
    if hash <> Tx.hash tx then Error "validator duty staging hash mismatch"
    else Ok { hash; tx }

let put ~retry post staged =
  Set_post.put ~retry post ~hash:staged.hash staged.tx

let retry ~bft_mode runtime ledger network tx =
  match Rest.add_tx_to_staging ~relay:false ~bft_mode runtime ledger tx with
  | Error reason -> Lwt.return_error (Set_post.Wait reason)
  | Ok hash when hash <> Tx.hash tx ->
    Lwt.return_error (Set_post.Refused "validator duty staging hash mismatch")
  | Ok hash ->
    let payload = Octra_net.P2p_tx_gossip.encode
      (Octra_net.P2p_tx_gossip.Tx {
        hash;
        tx_json = Yojson.Safe.to_string (Tx.to_yojson tx);
      }) in
    network.broadcast Octra_net.P2p_frame.{ msg_type = msg_tx_gossip; payload };
    Lwt.map (function
      | Error (Set_post.Refused reason) -> Error (Set_post.Retry reason)
      | result -> result) (network.post tx)