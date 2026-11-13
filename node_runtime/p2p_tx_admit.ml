(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type verdict =
  | Accept
  | Invalid_address
  | Invalid_payload
  | Timestamp_drift of float
  | Invalid_signature
  | Payload_unavailable of string
  | Sender_refused of string

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

let sig_valid ~signing tx = function
  | None -> false
  | Some pk ->
    Octra_core.Crypto.Address.verify_address_pubkey tx.Octra_core.Transaction.from pk
    && Octra_core.Tx_signature.verify signing tx pk

let sender_check ?(duty = None) ?(bft_mode = false) ~signing ~now ~max_drift ~sender_pk tx =
  if not (addr_valid tx) then Invalid_address
  else
    match
      Tx_view.payload_size_check
        ~limits:Tx_view.payload_limits
        tx
    with
    | Error _ -> Invalid_payload
    | Ok () ->
      let drift = Float.abs (tx.Octra_core.Transaction.timestamp -. now) in
      if not (Float.is_finite tx.timestamp && Float.is_finite now)
         || drift > max_drift && not (Tx_view.duty_retry ~now ~duty ~bft_mode tx) then
        Timestamp_drift drift
      else if not (sig_valid ~signing tx sender_pk) then Invalid_signature
      else Accept

let admit ?duty ?bft_mode ~signing ~now ~max_drift ~sender_pk tx =
  match sender_check ?duty ?bft_mode ~signing ~now ~max_drift ~sender_pk tx with
  | Accept ->
    let open Lwt.Syntax in
    let* result = Tx_view.payload_check ~limits:Tx_view.payload_limits tx in
    Lwt.return (match result with
      | Error (("pre_verify_busy" | "pre_verify_unavailable"), reason) ->
        Payload_unavailable reason
      | Error _ -> Invalid_payload
      | Ok () -> Accept)
  | verdict -> Lwt.return verdict