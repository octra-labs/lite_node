(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Rpc = Octra_core.Rpc
module Staging = Octra_core.Tx_staging
module Store_chaindata = Octra_core.Store_chaindata
module Transaction = Octra_core.Transaction
module Tree = Octra_core.Tree

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result Lwt.t

type 'handler dispatch_adapters = {
  ledger_chaindata_params_read :
    (Ledger.t -> Store_chaindata.t -> params:Yojson.Safe.t -> rpc_result) ->
    'handler;
  chaindata_params_read :
    (Store_chaindata.t -> params:Yojson.Safe.t -> rpc_result) ->
    'handler;
  no_params : (unit -> rpc_result) -> 'handler;
  epoch_current : 'handler;
  epoch_list : 'handler;
  recommended_fee : 'handler;
}

let ok value =
  Lwt.return (Ok value)

let err error =
  Lwt.return (Error error)

let tx_display_fields =
  Tx_view.tx_fields ~decode_message:Text.decode_message_if_hex

let search ledger chaindata ~params =
  match Rpc.require_string params 0 "query" with
  | Error e ->
    err e
  | Ok query ->
    let result =
      Rest_view.search_result_of_query
        ~pending_tx:(fun hash ->
          Option.map tx_display_fields (Staging.find_by_hash hash))
        ~confirmed_tx:(History_read_rpc.confirmed_tx_epoch_with_heal chaindata)
        ~account:(fun addr ->
          Option.map
            (fun account -> account.Ledger.balance, account.Ledger.nonce)
            (Ledger.find_opt ledger addr))
        ~epoch_exists:(fun epoch_id ->
          Option.is_some (Store_chaindata.get_epoch_summary chaindata epoch_id))
        query
    in
    match Rest_view.search_response result with
    | Ok response ->
      ok response
    | Error e ->
      err e

let epoch_current ~tree_ref =
  let tree = !tree_ref in
  ok
    (Rest_view.epoch_current_response
       ~epoch_id:tree.Tree.epoch_id
       ~root_count:(Tree.root_count tree))

let epoch_get chaindata ~params =
  match Rpc.require_int params 0 "epoch_id" with
  | Error e ->
    err e
  | Ok epoch_id ->
    match Rest_view.epoch_get_response
            (Store_chaindata.get_epoch_summary chaindata epoch_id) with
    | Ok response ->
      ok response
    | Error e ->
      err e

let epoch_list ~tree_ref ~params =
  let current_epoch = (!tree_ref).Tree.epoch_id in
  ok (Rest_view.epoch_list_response ~current_epoch params)

let epoch_summaries chaindata ~params =
  ok
    (Rest_view.epoch_summaries_response_of_ids
       ~find_summary:(Store_chaindata.get_epoch_summary chaindata)
       (Rest_view.epoch_summary_ids params))

let staging_view () =
  ok
    (Rest_view.staging_view_response_of_txs
       ~tx_hash:Transaction.hash
       ~tx_fields:tx_display_fields
       (Staging.all ()))

let staging_stats () =
  let txs, by_sender = Staging.get_stats () in
  ok
    (Rest_view.staging_stats_response
       ~tx_count:(List.length txs)
       ~total_ou:!Staging.total_ou
       ~max_ou:Staging.max_ou
       ~by_sender)

let staging_estimate_ou () =
  ok
    (Rest_view.staging_ou_rpc_response_of_txs
       ~tx_ou:(fun tx -> tx.Transaction.ou)
       ~txs:(Staging.all ())
       ~staging_ou:(Staging.staging_total_ou ())
       ~capacity:Staging.max_ou_per_epoch)

let stealth_floor () =
  Transaction.stealth_min_ou_of Sys.getenv_opt

let recommended_fee ~params ~stealth_floor =
  let jitter () =
    Rest_view.recommended_fee_jitter
      ~get_int:Env.int_value
      ~pick:Random.int
  in
  ok
    (Rest_view.recommended_fee_response
       ~params
       ~is_heavy:Tx_view.requires_heavy_preverify
       ~stealth_floor
       ~jitter
       ~staging_ou:(Staging.staging_total_ou ())
       ~capacity:Staging.max_ou_per_epoch
       ~usage_pct:(Staging.staging_usage_pct ())
       ~staging_size:(Staging.staging_size ()))

let recommended_fee_from_env ~params =
  recommended_fee ~params ~stealth_floor:(stealth_floor ())

let dispatch adapters =
  [
    "octra_search", adapters.ledger_chaindata_params_read search;
    "epoch_current", adapters.epoch_current;
    "epoch_get", adapters.chaindata_params_read epoch_get;
    "epoch_list", adapters.epoch_list;
    "epoch_summaries", adapters.chaindata_params_read epoch_summaries;
    "staging_view", adapters.no_params staging_view;
    "staging_stats", adapters.no_params staging_stats;
    "staging_estimateOu", adapters.no_params staging_estimate_ou;
    "octra_recommendedFee", adapters.recommended_fee;
  ]