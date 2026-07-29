(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let recent_txs_of_summary_rows rows =
  List.map (fun row ->
    let open Yojson.Safe.Util in
    let epoch_id =
      try row |> member "epoch" |> to_int
      with _ ->
        try row |> member "epoch_id" |> to_int
        with _ -> 0
    in
    let hash = try row |> member "hash" |> to_string with _ -> "" in
    (epoch_id, hash)
  ) rows

let recent_tx_to_yojson (epoch, hash) =
  `Assoc [
    "epoch", `Int epoch;
    "hash", `String hash;
  ]

let recent_tx_link_to_yojson (epoch, hash) =
  `Assoc [
    "epoch", `Int epoch;
    "hash", `String hash;
    "url", `String ("/tx/" ^ hash);
  ]

let rejected_tx_to_yojson (hash, epoch, _ts) =
  `Assoc [
    "epoch", `Int epoch;
    "hash", `String hash;
  ]

let recent_txs_to_yojson txs =
  List.map recent_tx_to_yojson txs

let recent_txs_to_link_yojson txs =
  List.map recent_tx_link_to_yojson txs

let rejected_txs_to_yojson txs =
  List.map rejected_tx_to_yojson txs

let index_incomplete detail fields =
  Octra_core.Rpc.err 116 "history index incomplete"
    (Some (`Assoc (("detail", `String detail) :: fields)))

let recent_incomplete_error ~offset ~limit ~missing =
  index_incomplete
    "recent transactions view incomplete"
    [
      "offset", `Int offset;
      "limit", `Int limit;
      "missing", `Int missing;
    ]

let address_incomplete_error ~addr ~offset ~limit ~missing =
  index_incomplete
    (Printf.sprintf "address history incomplete for %s" addr)
    [
      "address", `String addr;
      "offset", `Int offset;
      "limit", `Int limit;
      "missing", `Int missing;
    ]

let token_incomplete_error ~addr ~offset ~limit ~missing =
  index_incomplete
    (Printf.sprintf "token transfer history incomplete for %s" addr)
    [
      "address", `String addr;
      "offset", `Int offset;
      "limit", `Int limit;
      "missing", `Int missing;
    ]

let epoch_incomplete_error ~epoch_id fields =
  index_incomplete
    (Printf.sprintf "epoch %d history index incomplete" epoch_id)
    fields

let epoch_index_status_fields status =
  let base = [
    "epoch_id", `Int status.Octra_core.Store_chaindata.epoch_id;
    "expected_start_txid", `String (Int64.to_string status.expected_start_txid);
    "expected_tx_count", `Int status.expected_tx_count;
    "checked", `Int status.checked;
    "missing_epoch_meta", `Bool status.missing_epoch_meta;
    "missing_txid_loc", `Int status.missing_txid_loc;
    "missing_tx_loc", `Int status.missing_tx_loc;
    "missing_addr_refs", `Int status.missing_addr_refs;
    "malformed_records", `Int status.malformed_records;
    "error_count", `Int (List.length status.errors);
  ] in
  match status.errors with
  | first :: _ -> ("first_error", `String first) :: base
  | [] -> base

let epoch_incomplete_status_error ~epoch_id ~missing = function
  | Some status ->
    epoch_incomplete_error ~epoch_id (epoch_index_status_fields status)
  | None ->
    epoch_incomplete_error
      ~epoch_id
      [
        "epoch_id", `Int epoch_id;
        "missing", `Int missing;
      ]

let page_has_more ~offset ~count ~total =
  offset + count < total

let rec take n xs =
  match n, xs with
  | n, _ when n <= 0 -> []
  | _, [] -> []
  | n, x :: rest -> x :: take (n - 1) rest

let transaction_epoch_targets ?epoch_param all_epochs =
  match epoch_param with
  | Some epoch when List.mem epoch all_epochs -> [epoch]
  | Some _ -> []
  | None -> take 5 all_epochs

type transactions_query = {
  transactions_epoch_param : int option;
  transactions_limit : int;
}

type 'a transactions_list_result =
  | Transactions_index_incomplete of int * (string * Yojson.Safe.t) list
  | Transactions_ready of (int * string * 'a) list

let transactions_query params =
  let transactions_epoch_param = Octra_core.Rpc.param_int params 0 in
  let transactions_limit =
    match Octra_core.Rpc.param_int params 1 with
    | Some limit -> min limit 10000
    | None -> 20
  in
  {
    transactions_epoch_param;
    transactions_limit;
  }

let transaction_rows ~load_epoch ~limit target_epochs =
  target_epochs
  |> List.concat_map (fun epoch ->
    load_epoch epoch
    |> List.map (fun (hash, tx_json) -> epoch, hash, tx_json))
  |> take limit

let transactions_list_result
    ~epoch_status
    ~status_ok
    ~status_fields
    ~load_epoch
    params
    all_epochs =
  let query = transactions_query params in
  let target_epochs =
    transaction_epoch_targets
      ?epoch_param:query.transactions_epoch_param
      all_epochs
  in
  match List.find_map
          (fun epoch ->
            match epoch_status epoch with
            | Some status when not (status_ok status) ->
              Some (epoch, status_fields status)
            | _ -> None)
          target_epochs with
  | Some (epoch, fields) ->
    Transactions_index_incomplete (epoch, fields)
  | None ->
    Transactions_ready (transaction_rows
      ~load_epoch
      ~limit:query.transactions_limit
      target_epochs)

type page = {
  limit : int;
  offset : int;
}

type epoch_page_request = {
  epoch_page_id : int;
  epoch_page_limit : int;
  epoch_page_offset : int;
}

type 'a epoch_page_status = {
  epoch_page_status : 'a;
  epoch_page_heal_checked : int;
  epoch_page_heal_repaired : int;
  epoch_page_heal_errors : int;
  epoch_page_heal_ms : float;
  epoch_page_retry_ms : float;
}

let epoch_page_request ~limit_max params =
  match Octra_core.Rpc.require_int params 0 "epoch_id" with
  | Error e -> Error e
  | Ok epoch_page_id ->
    let epoch_page_limit =
      match Octra_core.Rpc.param_int params 1 with
      | Some limit -> max 0 (min limit limit_max)
      | None -> min 50 limit_max
    in
    let epoch_page_offset =
      match Octra_core.Rpc.param_int params 2 with
      | Some offset -> max offset 0
      | None -> 0
    in
    Ok {
      epoch_page_id;
      epoch_page_limit;
      epoch_page_offset;
    }

let epoch_page_cache_ttl ~current_epoch_id ~recent_ttl ~old_ttl ~epoch_id =
  if epoch_id >= current_epoch_id - 2 then recent_ttl
  else old_ttl

let epoch_page_cache_key ~epoch_id ~start_txid ~tx_count ~limit ~offset =
  Printf.sprintf "%d:%Ld:%d:%d:%d"
    epoch_id start_txid tx_count limit offset

let epoch_page_cache_plan ~current_epoch_id ~recent_ttl ~old_ttl ~epoch_id
    ~header ~limit ~offset =
  Option.map
    (fun (start_txid, tx_count) ->
      let key =
        epoch_page_cache_key
          ~epoch_id
          ~start_txid
          ~tx_count
          ~limit
          ~offset
      in
      let ttl =
        epoch_page_cache_ttl
          ~current_epoch_id
          ~recent_ttl
          ~old_ttl
          ~epoch_id
      in
      key, ttl)
    header

let epoch_page_cache_deadline ~now ~ttl =
  now +. ttl

let epoch_page_cache_live ~now ~deadline =
  now <= deadline

let epoch_page_status_without_heal status =
  {
    epoch_page_status = status;
    epoch_page_heal_checked = 0;
    epoch_page_heal_repaired = 0;
    epoch_page_heal_errors = 0;
    epoch_page_heal_ms = 0.0;
    epoch_page_retry_ms = 0.0;
  }

let epoch_page_status_after_heal
    ~status
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~heal_ms
    ~retry_ms =
  {
    epoch_page_status = status;
    epoch_page_heal_checked = heal_checked;
    epoch_page_heal_repaired = heal_repaired;
    epoch_page_heal_errors = heal_errors;
    epoch_page_heal_ms = heal_ms;
    epoch_page_retry_ms = retry_ms;
  }

let epoch_page_heal_should_log status =
  status.epoch_page_heal_repaired > 0 || status.epoch_page_heal_errors > 0

let epoch_page_status_with_heal
    ~is_incomplete
    ~recent_epochs
    ~now
    ~heal
    ~retry
    status0 =
  if is_incomplete status0 then
    let heal_checked, heal_repaired, heal_errors, heal_ms =
      if recent_epochs > 0 then (
        let heal_start = now () in
        let checked, repaired, errors = heal () in
        checked, repaired, errors, (now () -. heal_start) *. 1000.0
      ) else (0, 0, 0, 0.0)
    in
    let retry_start = now () in
    let status = retry () in
    epoch_page_status_after_heal
      ~status
      ~heal_checked
      ~heal_repaired
      ~heal_errors
      ~heal_ms
      ~retry_ms:((now () -. retry_start) *. 1000.0)
  else epoch_page_status_without_heal status0

let rpc_page ?(limit_index = 1) ?(offset_index = 2) ?(limit_max = 500) ?(default_limit = 50) params =
  let limit =
    match Octra_core.Rpc.param_int params limit_index with
    | Some value -> max 0 (min value limit_max)
    | None -> default_limit in
  let offset =
    match Octra_core.Rpc.param_int params offset_index with
    | Some value -> max value 0
    | None -> 0 in
  { limit; offset }

type epoch_profile = {
  epoch_id : int;
  current_epoch_id : int;
  limit : int;
  offset : int;
  incomplete : bool;
  missing : int;
  expected : int;
  rows : int;
  rejected : int;
  has_more : bool;
  status_ms : float;
  heal_ms : float;
  heal_checked : int;
  heal_repaired : int;
  heal_errors : int;
  retry_ms : float;
  rejected_ms : float;
  json_ms : float;
  total_ms : float;
}

let make_epoch_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~incomplete
    ~missing
    ~expected
    ~rows
    ~rejected
    ~has_more
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~rejected_ms
    ~json_ms
    ~total_ms =
  {
    epoch_id;
    current_epoch_id;
    limit;
    offset;
    incomplete;
    missing;
    expected;
    rows;
    rejected;
    has_more;
    status_ms;
    heal_ms;
    heal_checked;
    heal_repaired;
    heal_errors;
    retry_ms;
    rejected_ms;
    json_ms;
    total_ms;
  }

let make_epoch_incomplete_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~missing
    ~expected
    ~rows
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~total_ms =
  make_epoch_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~incomplete:true
    ~missing
    ~expected
    ~rows
    ~rejected:0
    ~has_more:false
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~rejected_ms:0.0
    ~json_ms:0.0
    ~total_ms

let make_epoch_complete_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~expected
    ~rows
    ~rejected
    ~has_more
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~rejected_ms
    ~json_ms
    ~total_ms =
  make_epoch_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~incomplete:false
    ~missing:0
    ~expected
    ~rows
    ~rejected
    ~has_more
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~rejected_ms
    ~json_ms
    ~total_ms

let make_epoch_incomplete_page_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~missing
    ~expected
    ~rows
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~total_ms =
  make_epoch_incomplete_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~missing
    ~expected
    ~rows:(List.length rows)
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~total_ms

let make_epoch_complete_page_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~expected
    ~rows
    ~rejected_rows
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~rejected_ms
    ~json_ms
    ~total_ms =
  let confirmed_count = List.length rows in
  make_epoch_complete_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~expected
    ~rows:confirmed_count
    ~rejected:(List.length rejected_rows)
    ~has_more:(confirmed_count = limit)
    ~status_ms
    ~heal_ms
    ~heal_checked
    ~heal_repaired
    ~heal_errors
    ~retry_ms
    ~rejected_ms
    ~json_ms
    ~total_ms

let make_epoch_incomplete_page_profile_with_status
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~missing
    ~expected
    ~rows
    ~status_ms
    ~page_status
    ~total_ms =
  make_epoch_incomplete_page_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~missing
    ~expected
    ~rows
    ~status_ms
    ~heal_ms:page_status.epoch_page_heal_ms
    ~heal_checked:page_status.epoch_page_heal_checked
    ~heal_repaired:page_status.epoch_page_heal_repaired
    ~heal_errors:page_status.epoch_page_heal_errors
    ~retry_ms:page_status.epoch_page_retry_ms
    ~total_ms

let make_epoch_complete_page_profile_with_status
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~expected
    ~rows
    ~rejected_rows
    ~status_ms
    ~page_status
    ~rejected_ms
    ~json_ms
    ~total_ms =
  make_epoch_complete_page_profile
    ~epoch_id
    ~current_epoch_id
    ~limit
    ~offset
    ~expected
    ~rows
    ~rejected_rows
    ~status_ms
    ~heal_ms:page_status.epoch_page_heal_ms
    ~heal_checked:page_status.epoch_page_heal_checked
    ~heal_repaired:page_status.epoch_page_heal_repaired
    ~heal_errors:page_status.epoch_page_heal_errors
    ~retry_ms:page_status.epoch_page_retry_ms
    ~rejected_ms
    ~json_ms
    ~total_ms

let epoch_profile_should_warn ~warn_ms profile =
  profile.total_ms >= warn_ms

let epoch_profile_log_message profile =
  if profile.incomplete then
    Printf.sprintf
      "tx_by_epoch profile epoch = %d current = %d limit = %d offset = %d cache = miss incomplete = true missing = %d expected = %d rows = %d t_status = %.0fms t_heal = %.0fms heal_checked = %d heal_repaired = %d heal_errors = %d t_retry = %.0fms total = %.0fms"
      profile.epoch_id
      profile.current_epoch_id
      profile.limit
      profile.offset
      profile.missing
      profile.expected
      profile.rows
      profile.status_ms
      profile.heal_ms
      profile.heal_checked
      profile.heal_repaired
      profile.heal_errors
      profile.retry_ms
      profile.total_ms
  else
    Printf.sprintf
      "tx_by_epoch profile epoch = %d current = %d limit = %d offset = %d cache = miss incomplete = false rows = %d expected = %d rejected = %d has_more = %b t_status = %.0fms t_heal = %.0fms heal_checked = %d heal_repaired = %d heal_errors = %d t_retry = %.0fms t_rejected = %.0fms t_json = %.0fms total = %.0fms"
      profile.epoch_id
      profile.current_epoch_id
      profile.limit
      profile.offset
      profile.rows
      profile.expected
      profile.rejected
      profile.has_more
      profile.status_ms
      profile.heal_ms
      profile.heal_checked
      profile.heal_repaired
      profile.heal_errors
      profile.retry_ms
      profile.rejected_ms
      profile.json_ms
      profile.total_ms

type account_profile = {
  account_tag : string;
  account_addr : string;
  account_recent_count : int;
  account_rejected_count : int;
  account_lookup_ms : float;
  account_recent_ms : float;
  account_rejected_ms : float;
  account_json_ms : float;
}

type account_view = {
  account_json : Yojson.Safe.t;
  account_recent_count : int;
  account_rejected_count : int;
}

let make_account_profile
    ~tag
    ~addr
    ~recent_count
    ~rejected_count
    ~lookup_ms
    ~recent_ms
    ~rejected_ms
    ~json_ms =
  {
    account_tag = tag;
    account_addr = addr;
    account_recent_count = recent_count;
    account_rejected_count = rejected_count;
    account_lookup_ms = lookup_ms;
    account_recent_ms = recent_ms;
    account_rejected_ms = rejected_ms;
    account_json_ms = json_ms;
  }

let account_profile_log_message profile =
  Printf.sprintf
    "account_profile tag = %s addr = %s recent_count = %d rejected_count = %d lookup_ms = %.2f recent_ms = %.2f rejected_ms = %.2f json_ms = %.2f total_ms = %.2f"
    profile.account_tag
    profile.account_addr
    profile.account_recent_count
    profile.account_rejected_count
    profile.account_lookup_ms
    profile.account_recent_ms
    profile.account_rejected_ms
    profile.account_json_ms
    (profile.account_lookup_ms
     +. profile.account_recent_ms
     +. profile.account_rejected_ms
     +. profile.account_json_ms)

let encrypted_balance_present = function
  | Some value when value <> "0" -> true
  | _ -> false

let epoch_profile_warning ~warn_ms profile =
  if epoch_profile_should_warn ~warn_ms profile then
    Some (epoch_profile_log_message profile)
  else None

let account_response ~addr ~balance ~balance_raw ~nonce ~has_public_key
    ~has_encrypted_balance ~tx_count ~recent_txs ~rejected_txs =
  `Assoc [
    "address", `String addr;
    "balance", `String balance;
    "balance_raw", `String balance_raw;
    "nonce", `Int nonce;
    "has_public_key", `Bool has_public_key;
    "has_encrypted_balance", `Bool has_encrypted_balance;
    "tx_count", `Int tx_count;
    "recent_txs", `List (recent_txs_to_yojson recent_txs);
    "rejected_txs", `List (rejected_txs_to_yojson rejected_txs);
  ]

let account_view
    ~addr
    ~balance
    ~nonce
    ~has_public_key
    ~encrypted_balance
    ~tx_count
    ~recent_rows
    ~rejected_txs =
  let recent_txs = recent_txs_of_summary_rows recent_rows in
  {
    account_json = account_response
      ~addr
      ~balance:(Octra_core.Denomination.format_balance balance)
      ~balance_raw:(Z.to_string balance)
      ~nonce
      ~has_public_key
      ~has_encrypted_balance:(encrypted_balance_present encrypted_balance)
      ~tx_count
      ~recent_txs
      ~rejected_txs;
    account_recent_count = List.length recent_txs;
    account_rejected_count = List.length rejected_txs;
  }

let account_profile_from_view ~tag ~addr ~view ~t0 ~t1 ~t2 ~t3 ~t4 =
  make_account_profile
    ~tag
    ~addr
    ~recent_count:view.account_recent_count
    ~rejected_count:view.account_rejected_count
    ~lookup_ms:((t1 -. t0) *. 1000.0)
    ~recent_ms:((t2 -. t1) *. 1000.0)
    ~rejected_ms:((t3 -. t2) *. 1000.0)
    ~json_ms:((t4 -. t3) *. 1000.0)

let address_transactions_response ~addr ~total ~offset ~limit ~transactions ~rejected =
  let count = List.length transactions in
  `Assoc [
    "address", `String addr;
    "total", `Int total;
    "count", `Int count;
    "offset", `Int offset;
    "limit", `Int limit;
    "has_more", `Bool (page_has_more ~offset ~count ~total);
    "transactions", `List transactions;
    "rejected", `List rejected;
  ]

let token_transactions_response ~addr ~total ~offset ~limit ~has_more ~incoming ~outgoing
    ~transactions =
  `Assoc [
    "address", `String addr;
    "total", `Int total;
    "count", `Int (List.length transactions);
    "offset", `Int offset;
    "limit", `Int limit;
    "has_more", `Bool has_more;
    "incoming", `Int incoming;
    "outgoing", `Int outgoing;
    "transactions", `List transactions;
  ]

let rejected_response ~addr ~total ~offset ~limit ~rejected =
  let count = List.length rejected in
  `Assoc [
    "address", `String addr;
    "total", `Int total;
    "count", `Int count;
    "offset", `Int offset;
    "limit", `Int limit;
    "has_more", `Bool (page_has_more ~offset ~count ~total);
    "rejected", `List rejected;
  ]

let epoch_transaction_hash_row (epoch, hash, _tx_json) =
  `Assoc [
    "hash", `String hash;
    "epoch", `Int epoch;
  ]

let epoch_transactions_response rows =
  `Assoc [
    "count", `Int (List.length rows);
    "transactions", `List (List.map epoch_transaction_hash_row rows);
  ]

let transactions_list_response = function
  | Transactions_index_incomplete (epoch_id, fields) ->
    Error (epoch_incomplete_error ~epoch_id fields)
  | Transactions_ready rows ->
    Ok (epoch_transactions_response rows)

let recent_transactions_response ~transactions =
  `Assoc [
    "count", `Int (List.length transactions);
    "transactions", `List transactions;
    "rejected", `List [];
  ]

let recent_transactions_result ~mask_rows ~(page : page) ~incomplete ~missing ~rows =
  if incomplete then
    Error (recent_incomplete_error
      ~offset:page.offset
      ~limit:page.limit
      ~missing)
  else
    Ok (recent_transactions_response
      ~transactions:(mask_rows rows))

let address_transactions_result
    ~mask_rows
    ~addr
    ~(page : page)
    ~total
    ~incomplete
    ~missing
    ~rows
    ~rejected =
  if incomplete then
    Error (address_incomplete_error
      ~addr
      ~offset:page.offset
      ~limit:page.limit
      ~missing)
  else
    Ok (address_transactions_response
      ~addr
      ~total
      ~offset:page.offset
      ~limit:page.limit
      ~transactions:(mask_rows rows)
      ~rejected:(mask_rows rejected))

let token_transactions_result
    ~mask_rows
    ~addr
    ~(page : page)
    ~total
    ~has_more
    ~incoming
    ~outgoing
    ~incomplete
    ~missing
    ~rows =
  if incomplete then
    Error (token_incomplete_error
      ~addr
      ~offset:page.offset
      ~limit:page.limit
      ~missing)
  else
    Ok (token_transactions_response
      ~addr
      ~total
      ~offset:page.offset
      ~limit:page.limit
      ~has_more
      ~incoming
      ~outgoing
      ~transactions:(mask_rows rows))

let rejected_transactions_result ~mask_rows ~addr ~(page : page) ~total ~rows =
  Ok (rejected_response
    ~addr
    ~total
    ~offset:page.offset
    ~limit:page.limit
    ~rejected:(mask_rows rows))

let epoch_transactions_page_response ~epoch_id ~expected_confirmed_count ~offset
    ~limit ~transactions ~rejected =
  let confirmed_count = List.length transactions in
  let rejected_count = List.length rejected in
  let count = confirmed_count + rejected_count in
  `Assoc [
    "epoch_id", `Int epoch_id;
    "count", `Int count;
    "confirmed_count", `Int confirmed_count;
    "expected_confirmed_count", `Int expected_confirmed_count;
    "rejected_count", `Int rejected_count;
    "offset", `Int offset;
    "limit", `Int limit;
    "has_more", `Bool (confirmed_count = limit);
    "transactions", `List transactions;
    "rejected", `List rejected;
  ]

let epoch_transactions_page_response_of_rows
    ~mask_rows
    ~epoch_id
    ~expected_confirmed_count
    ~offset
    ~limit
    ~rows
    ~rejected_rows =
  epoch_transactions_page_response
    ~epoch_id
    ~expected_confirmed_count
    ~offset
    ~limit
    ~transactions:(mask_rows rows)
    ~rejected:(mask_rows rejected_rows)

let http_address_summary_response ~addr ~balance ~balance_raw ~nonce ~has_public_key
    ~transaction_count ~recent_txs =
  `Assoc [
    "address", `String addr;
    "balance", `String balance;
    "nonce", `Int nonce;
    "balance_raw", `String balance_raw;
    "has_public_key", `Bool has_public_key;
    "transaction_count", `Int transaction_count;
    "recent_transactions", `List (recent_txs_to_link_yojson recent_txs);
  ]