(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Rpc = Octra_core.Rpc

let account_of_params params find_account =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    Error e
  | Ok addr ->
    match find_account addr with
    | Some account ->
      Ok (addr, account)
    | None ->
      Error Rpc.sender_not_found

let balance params ~find_account ~pending_nonce =
  match account_of_params params find_account with
  | Error e ->
    Error e
  | Ok (addr, account) ->
    Ok (Rpc_view.balance
      ~addr
      ~account
      ~pending_nonce:(pending_nonce addr account.Ledger.nonce))

let nonce params ~find_account =
  match account_of_params params find_account with
  | Error e ->
    Error e
  | Ok (addr, account) ->
    Ok (Rpc_view.nonce ~addr ~account)

let public_key params ~find_account =
  match account_of_params params find_account with
  | Error e ->
    Error e
  | Ok (addr, account) ->
    match account.Ledger.public_key with
    | Some public_key ->
      Ok (Rpc_view.public_key ~addr ~public_key)
    | None ->
      Error (Rpc.not_found "no public key registered")

let validate_address params =
  match Rpc.require_string params 0 "address" with
  | Error e ->
    Error e
  | Ok raw ->
    Ok (Rpc_view.validate_address
      ~raw
      ~is_valid:(Rpc.sanitize_address raw <> None))

let supply ~true_total ~encrypted ~max_supply ~emission_remaining
    ~retired_supply =
  let display_supply = Z.add true_total encrypted in
  Rpc_view.supply
    ~display_supply
    ~encrypted_supply:encrypted
    ~max_supply
    ~emission_remaining
    ~retired_supply

let total_transactions ~confirmed ~staging =
  Rpc_view.total_transactions ~confirmed ~staging