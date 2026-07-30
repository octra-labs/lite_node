(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type verdict =
  | Accept
  | Invalid_address
  | Invalid_payload
  | Timestamp_drift of float
  | Invalid_signature

let to_valid tx =
  match tx.Octra_core.Transaction.op_type with
  | Octra_core.Transaction.StealthOp ->
    String.equal tx.Octra_core.Transaction.to_ "stealth"
  | Octra_core.Transaction.MultiExec ->
    String.equal tx.Octra_core.Transaction.to_ "multi_exec"
  | _ -> Octra_core.Crypto.Address.is_valid_address tx.Octra_core.Transaction.to_

let addr_valid tx =
  Octra_core.Crypto.Address.is_valid_address tx.Octra_core.Transaction.from
  && to_valid tx

let sig_valid tx = function
  | None -> false
  | Some pk ->
    Octra_core.Crypto.Address.verify_address_pubkey tx.Octra_core.Transaction.from pk
    && Octra_core.Transaction.verify tx pk

let admit ~now ~max_drift ~sender_pk tx =
  if not (addr_valid tx) then Invalid_address
  else
    match
      Tx_view.payload_admission
        ~limits:Tx_view.payload_limits
        tx
    with
    | Error _ -> Invalid_payload
    | Ok () ->
      let drift = Float.abs (tx.Octra_core.Transaction.timestamp -. now) in
      if drift > max_drift then Timestamp_drift drift
      else if not (sig_valid tx sender_pk) then Invalid_signature
      else Accept