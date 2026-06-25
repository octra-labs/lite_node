(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


module Rpc = Octra_core.Rpc
module Store_chaindata = Octra_core.Store_chaindata
module Staging = Octra_core.Tx_staging
module Transaction = Octra_core.Transaction

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result Lwt.t

type 'handler dispatch_adapters = {
  chaindata_params_read :
    (Store_chaindata.t -> params:Yojson.Safe.t -> rpc_result) ->
    'handler;
  chaindata_address_read :
    (Store_chaindata.t -> params:Yojson.Safe.t -> addr:string -> rpc_result) ->
    'handler;
  transactions_by_epoch : 'handler;
}

type tx_epoch_cache_entry = {
  tx_epoch_cache_t : float;
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

let tx_epoch_cache_key epoch_id limit offset =
  Printf.sprintf "%d:%d:%d" epoch_id limit offset

let tx_epoch_cache_get key ttl =
  match Hashtbl.find_opt tx_epoch_cache key with
  | Some entry when Unix.gettimeofday () -. entry.tx_epoch_cache_t <= ttl ->
    Some entry.tx_epoch_cache_v
  | Some _ ->
    Hashtbl.remove tx_epoch_cache key;
    None
  | None ->
    None

let tx_epoch_cache_put key value =
  if Hashtbl.length tx_epoch_cache >= tx_epoch_cache_max then
    Hashtbl.clear tx_epoch_cache;
  Hashtbl.replace
    tx_epoch_cache
    key
    {
      tx_epoch_cache_t = Unix.gettimeofday ();
      tx_epoch_cache_v = value;
    }

let tx_lookup_recent_heal_window () =
  Env.int_value "OCTRA_TX_HEAL_RECENT_EPOCHS" 256

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
    if recent_epochs <= 0 then None
    else
      match
        Store_chaindata.heal_tx_by_hash_recent
          chaindata
          ~hash:txh
          ~recent_epochs
      with
      | Some (epoch_id, tx_json) ->
        Log.warn
          "chaindata"
          "auto-healed tx_loc hash = %s epoch = %d recent_epochs = %d"
          (Text.addr_short txh)
          epoch_id
          recent_epochs;
        Some (epoch_id, tx_json)
      | None ->
        None

let confirmed_tx_epoch_with_heal chaindata hash =
  Option.map fst (lookup_confirmed_tx_with_heal chaindata hash)

let transaction chaindata ~params =
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
      else Staging.lookup_dropped txh
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

let transactions_by_address chaindata ~params ~addr =
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
    let cache_key = tx_epoch_cache_key eid limit offset in
    let cache_ttl =
      History.epoch_page_cache_ttl
        ~current_epoch_id
        ~recent_ttl:tx_epoch_recent_ttl
        ~old_ttl:tx_epoch_old_ttl
        ~epoch_id:eid
    in
    match tx_epoch_cache_get cache_key cache_ttl with
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
        let rejected_rows = Store_chaindata.rejected_by_epoch_rows chaindata eid in
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
        tx_epoch_cache_put cache_key result;
        ok result

let dispatch adapters =
  [
    "octra_transaction", adapters.chaindata_params_read transaction;
    "octra_transactions", adapters.chaindata_params_read transactions;
    "octra_recentTransactions", adapters.chaindata_params_read recent_transactions;
    "octra_transactionsByAddress", adapters.chaindata_address_read transactions_by_address;
    "octra_tokenTransfersByAddress", adapters.chaindata_address_read token_transfers_by_address;
    "octra_rejectedByAddress", adapters.chaindata_address_read rejected_by_address;
    "octra_transactionsByEpoch", adapters.transactions_by_epoch;
  ]