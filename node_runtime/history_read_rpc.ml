(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc
module Store_chaindata = Octra_core.Store_chaindata
module Staging = Octra_core.Tx_staging
module Transaction = Octra_core.Transaction
module Tx_drop = Octra_core.Tx_drop

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result Lwt.t

type 'handler dispatch_adapters = {
  transaction : 'handler;
  transactions_by_address : 'handler;
  chaindata_params_read :
    (Store_chaindata.t -> params:Yojson.Safe.t -> rpc_result) ->
    'handler;
  chaindata_address_read :
    (Store_chaindata.t -> params:Yojson.Safe.t -> addr:string -> rpc_result) ->
    'handler;
  transactions_by_epoch : 'handler;
}

type tx_epoch_cache_entry = {
  tx_epoch_cache_deadline : float;
  tx_epoch_cache_v : Yojson.Safe.t;
}

let ok value =
  Lwt.return (Ok value)

let err error =
  Lwt.return (Error error)

let tx_epoch_cache : (string, tx_epoch_cache_entry) Hashtbl.t =
  Hashtbl.create 2048

let tx_epoch_cache_max =
  max 16 (Env.int_value "OCTRA_TX_BY_EPOCH_CACHE_MAX" 4096)

let tx_epoch_limit_max =
  max 1 (Env.int_value "OCTRA_TX_BY_EPOCH_LIMIT_MAX" 100)

let tx_epoch_recent_ttl =
  float_of_int (max 1 (Env.int_value "OCTRA_TX_BY_EPOCH_RECENT_TTL" 5))

let tx_epoch_old_ttl =
  float_of_int (max 1 (Env.int_value "OCTRA_TX_BY_EPOCH_OLD_TTL" 3600))

let tx_epoch_cache_get key =
  let now = Unix.gettimeofday () in
  match Hashtbl.find_opt tx_epoch_cache key with
  | Some entry
    when History.epoch_page_cache_live
      ~now
      ~deadline:entry.tx_epoch_cache_deadline ->
    Some entry.tx_epoch_cache_v
  | Some _ ->
    Hashtbl.remove tx_epoch_cache key;
    None
  | None ->
    None

let tx_epoch_cache_put key ~ttl value =
  if Hashtbl.length tx_epoch_cache >= tx_epoch_cache_max then
    Hashtbl.clear tx_epoch_cache;
  let now = Unix.gettimeofday () in
  Hashtbl.replace
    tx_epoch_cache
    key
    {
      tx_epoch_cache_deadline =
        History.epoch_page_cache_deadline ~now ~ttl;
      tx_epoch_cache_v = value;
    }

let tx_lookup_recent_heal_window () =
  Env.int_value "OCTRA_TX_HEAL_RECENT_EPOCHS" 256

let bounded_heal_limit value =
  min 4096 (max 0 value)

let tx_lookup_recent_heal_limit () =
  Env.int_value "OCTRA_TX_HEAL_MAX_RECORDS" 256
  |> bounded_heal_limit

let txid_epoch_heal_recent_window () =
  Env.int_value "OCTRA_TXID_HEAL_RECENT_EPOCHS" 256

let warn_epoch_profile profile =
  let warn_ms =
    float_of_int (Env.int_value "OCTRA_TX_BY_EPOCH_PROFILE_WARN_MS" 1000)
  in
  match History.epoch_profile_warning ~warn_ms profile with
  | Some message ->
    Log.warn "rpc" "%s" message
  | None ->
    ()

let lookup_confirmed_tx_with_heal chaindata txh =
  match Store_chaindata.get_tx_by_hash chaindata txh with
  | Some _ as hit ->
    hit
  | None ->
    let recent_epochs = tx_lookup_recent_heal_window () in
    let max_records = tx_lookup_recent_heal_limit () in
    if recent_epochs <= 0 || max_records <= 0 then None
    else
      match
        Store_chaindata.heal_tx_by_hash_recent
          chaindata
          ~hash:txh
          ~recent_epochs
          ~max_records
      with
      | Some (epoch_id, tx_json) ->
        Log.warn
          "chaindata"
          "auto-healed tx_loc hash = %s epoch = %d recent_epochs = %d max_records = %d"
          (Text.addr_short txh)
          epoch_id
          recent_epochs
          max_records;
        Some (epoch_id, tx_json)
      | None ->
        None

let confirmed_tx_epoch_with_heal chaindata hash =
  Option.map fst (lookup_confirmed_tx_with_heal chaindata hash)

let transaction ~find_drop chaindata ~params =
  match Rpc.require_hash params 0 "hash" with
  | Error e ->
    err e
  | Ok txh ->
    let pending = Staging.find_by_hash txh in
    let confirmed =
      if Option.is_some pending then None
      else lookup_confirmed_tx_with_heal chaindata txh
    in
    let rejected =
      if Option.is_some pending || Option.is_some confirmed then None
      else Store_chaindata.get_rejected_tx chaindata txh
    in
    let dropped =
      if Option.is_some pending || Option.is_some confirmed ||
         Option.is_some rejected
      then None
      else
        match Staging.lookup_dropped txh with
        | Some _ as row -> row
        | None ->
          Option.map
            (fun row ->
               (row.Octra_core.Tx_drop.reason,
                row.detail,
                row.dropped_at,
                row.from_addr,
                row.to_addr,
                row.nonce,
                row.ou,
                row.op_type))
            (find_drop txh)
    in
    match
      Tx_view.transaction_lookup_response
        ~decode_message:Text.decode_message_if_hex
        ~hash:txh
        (Tx_view.transaction_lookup ~pending ~confirmed ~rejected ~dropped)
    with
    | Ok response ->
      ok response
    | Error e ->
      err e

let transactions chaindata ~params =
  let all_epochs =
    Store_chaindata.list_epoch_ids chaindata
    |> List.sort_uniq compare
    |> List.rev
  in
  let result =
    History.transactions_list_result
      ~epoch_status:(Store_chaindata.get_visible_epoch_index_status chaindata)
      ~status_ok:Store_chaindata.epoch_index_status_ok
      ~status_fields:History.epoch_index_status_fields
      ~load_epoch:(Store_chaindata.txs_by_epoch_full chaindata)
      params
      all_epochs
  in
  match History.transactions_list_response result with
  | Ok response ->
    ok response
  | Error e ->
    err e

let recent_transactions chaindata ~params =
  let page =
    History.rpc_page
      ~limit_index:0
      ~offset_index:1
      ~limit_max:100
      ~default_limit:15
      params
  in
  let status =
    Store_chaindata.recent_txs_rows_status
      chaindata
      ~limit:page.limit
      ~offset:page.offset
  in
  match
    History.recent_transactions_result
      ~mask_rows:Tx_view.mask_stealth_rows
      ~page
      ~incomplete:status.incomplete
      ~missing:status.missing
      ~rows:status.rows
  with
  | Ok response ->
    ok response
  | Error e ->
    err e

let address_history_status chaindata addr ~limit ~offset ~profile_tag =
  Lwt.return
    (Store_chaindata.txs_by_addr_rows_status
       ~profile_tag
       chaindata
       addr
       ~limit
       ~offset)

let drop_json (row : Tx_drop.row) =
  `Assoc [
    "hash", `String row.hash;
    "tx_hash", `String row.hash;
    "status", `String "dropped";
    "from", `String row.from_addr;
    "to", `String row.to_addr;
    "to_", `String row.to_addr;
    "nonce", `Int row.nonce;
    "ou", `String (Z.to_string row.ou);
    "op_type", `String (Transaction.op_type_to_string row.op_type);
    "reason", `String row.reason;
    "detail", `String row.detail;
    "dropped_at", `Float row.dropped_at;
    "timestamp", `Float row.dropped_at;
  ]

let transactions_by_address ~drops_by_addr chaindata ~params ~addr =
  let page = History.rpc_page params in
  let open Lwt.Syntax in
  let* status =
    address_history_status
      chaindata
      addr
      ~limit:page.limit
      ~offset:page.offset
      ~profile_tag:"octra_transactionsByAddress"
  in
  let rejected =
    Store_chaindata.rejected_by_addr_rows
      chaindata
      addr
      ~limit:page.limit
      ~offset:page.offset
  in
  let dropped =
    drops_by_addr addr ~limit:page.limit ~offset:page.offset
    |> List.map drop_json
  in
  match
    History.address_transactions_result
      ~mask_rows:Tx_view.mask_stealth_rows
      ~addr
      ~page
      ~total:status.total
      ~incomplete:status.incomplete
      ~missing:status.missing
      ~rows:status.rows
      ~rejected
      ~dropped
  with
  | Ok response ->
    ok response
  | Error e ->
    err e

let token_transfers_by_address chaindata ~params ~addr =
  let req_page = History.rpc_page params in
  let page =
    Store_chaindata.token_txs_by_addr_page
      ~profile_tag:"octra_tokenTransfersByAddress"
      chaindata
      addr
      ~limit:req_page.limit
      ~offset:req_page.offset
  in
  match
    History.token_transactions_result
      ~mask_rows:Tx_view.mask_stealth_rows
      ~addr
      ~page:req_page
      ~total:page.total
      ~has_more:page.has_more
      ~incoming:page.incoming
      ~outgoing:page.outgoing
      ~incomplete:page.incomplete
      ~missing:page.missing
      ~rows:page.rows
  with
  | Ok response ->
    ok response
  | Error e ->
    err e

let rejected_by_address chaindata ~params ~addr =
  let page = History.rpc_page params in
  let total = Store_chaindata.rejected_count_by_addr chaindata addr in
  let rows =
    Store_chaindata.rejected_by_addr_rows
      chaindata
      addr
      ~limit:page.limit
      ~offset:page.offset
  in
  match
    History.rejected_transactions_result
      ~mask_rows:Tx_view.mask_stealth_rows
      ~addr
      ~page
      ~total
      ~rows
  with
  | Ok response ->
    ok response
  | Error e ->
    err e

let transactions_by_epoch chaindata ~params ~current_epoch_id =
  match History.epoch_page_request ~limit_max:tx_epoch_limit_max params with
  | Error e ->
    err e
  | Ok page ->
    let eid = page.History.epoch_page_id in
    let limit = page.History.epoch_page_limit in
    let offset = page.History.epoch_page_offset in
    let header =
      Store_chaindata.get_epoch_header chaindata eid
      |> Option.map (fun header ->
        header.Octra_core.Epochlog.start_txid,
        header.Octra_core.Epochlog.tx_count)
    in
    let cache_plan =
      History.epoch_page_cache_plan
        ~current_epoch_id
        ~recent_ttl:tx_epoch_recent_ttl
        ~old_ttl:tx_epoch_old_ttl
        ~epoch_id:eid
        ~header
        ~limit
        ~offset
    in
    match Option.bind cache_plan (fun (key, _) -> tx_epoch_cache_get key) with
    | Some result ->
      ok result
    | None ->
      let profile_start = Unix.gettimeofday () in
      let status0_start = Unix.gettimeofday () in
      let status0 =
        Store_chaindata.txs_by_epoch_rows_status chaindata eid ~limit ~offset
      in
      let status0_ms = (Unix.gettimeofday () -. status0_start) *. 1000.0 in
      let recent_epochs = txid_epoch_heal_recent_window () in
      let page_status =
        History.epoch_page_status_with_heal
          ~is_incomplete:(fun (status : Store_chaindata.rows_status) ->
            status.incomplete)
          ~recent_epochs
          ~now:Unix.gettimeofday
          ~heal:(fun () ->
            let stats =
              Store_chaindata.heal_epoch_txids_recent
                chaindata
                ~epoch_id:eid
                ~recent_epochs
            in
            stats.checked, stats.repaired, List.length stats.errors)
          ~retry:(fun () ->
            Store_chaindata.txs_by_epoch_rows_status
              chaindata
              eid
              ~limit
              ~offset)
          status0
      in
      if History.epoch_page_heal_should_log page_status then
        Log.warn
          "chaindata"
          "auto-healed txid_loc epoch = %d checked = %d repaired = %d errors = %d"
          eid
          page_status.History.epoch_page_heal_checked
          page_status.History.epoch_page_heal_repaired
          page_status.History.epoch_page_heal_errors;
      let status = page_status.History.epoch_page_status in
      if status.incomplete then
        let total_ms = (Unix.gettimeofday () -. profile_start) *. 1000.0 in
        let profile =
          History.make_epoch_incomplete_page_profile_with_status
            ~epoch_id:eid
            ~current_epoch_id
            ~limit
            ~offset
            ~missing:status.missing
            ~expected:status.total
            ~rows:status.rows
            ~status_ms:status0_ms
            ~page_status
            ~total_ms
        in
        warn_epoch_profile profile;
        err
          (History.epoch_incomplete_status_error
             ~epoch_id:eid
             ~missing:status.missing
             (Store_chaindata.get_visible_epoch_index_status chaindata eid))
      else
        let rejected_start = Unix.gettimeofday () in
        let rejected_rows =
          Store_chaindata.rejected_by_epoch_rows
            chaindata
            eid
            ~limit
            ~offset
        in
        let rejected_ms = (Unix.gettimeofday () -. rejected_start) *. 1000.0 in
        let json_start = Unix.gettimeofday () in
        let result =
          History.epoch_transactions_page_response_of_rows
            ~mask_rows:Tx_view.mask_stealth_rows
            ~epoch_id:eid
            ~expected_confirmed_count:status.total
            ~offset
            ~limit
            ~rows:status.rows
            ~rejected_rows
        in
        let json_ms = (Unix.gettimeofday () -. json_start) *. 1000.0 in
        let total_ms = (Unix.gettimeofday () -. profile_start) *. 1000.0 in
        let profile =
          History.make_epoch_complete_page_profile_with_status
            ~epoch_id:eid
            ~current_epoch_id
            ~limit
            ~offset
            ~expected:status.total
            ~rows:status.rows
            ~rejected_rows
            ~status_ms:status0_ms
            ~page_status
            ~rejected_ms
            ~json_ms
            ~total_ms
        in
        warn_epoch_profile profile;
        Option.iter
          (fun (key, ttl) -> tx_epoch_cache_put key ~ttl result)
          cache_plan;
        ok result

let dispatch adapters =
  [
    "octra_transaction", adapters.transaction;
    "octra_transactions", adapters.chaindata_params_read transactions;
    "octra_recentTransactions", adapters.chaindata_params_read recent_transactions;
    "octra_transactionsByAddress", adapters.transactions_by_address;
    "octra_tokenTransfersByAddress", adapters.chaindata_address_read token_transfers_by_address;
    "octra_rejectedByAddress", adapters.chaindata_address_read rejected_by_address;
    "octra_transactionsByEpoch", adapters.transactions_by_epoch;
  ]