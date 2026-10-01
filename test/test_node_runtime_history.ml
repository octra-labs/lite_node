(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module H = Octra_node_runtime.History
module HR = Octra_node_runtime.History_read_rpc
module Drop = Octra_core.Tx_drop
module SC = Octra_core.Store_chaindata
module Staging = Octra_core.Tx_staging
module Transaction = Octra_core.Transaction

let fail msg = failwith ("test_node_runtime_history: " ^ msg)

let member name = function
  | `Assoc fields ->
      (match List.assoc_opt name fields with
       | Some value -> value
       | None -> fail ("missing field " ^ name))
  | _ -> fail "expected object"

let string_field name json =
  match member name json with
  | `String value -> value
  | _ -> fail ("expected string field " ^ name)

let int_field name json =
  match member name json with
  | `Int value -> value
  | _ -> fail ("expected int field " ^ name)

let bool_field name json =
  match member name json with
  | `Bool value -> value
  | _ -> fail ("expected bool field " ^ name)

let list_field name json =
  match member name json with
  | `List value -> value
  | _ -> fail ("expected list field " ^ name)

let assert_bool name expected actual =
  if actual <> expected then fail (name ^ " mismatch")

let assert_int name expected actual =
  if actual <> expected then fail (name ^ " mismatch")

let assert_string name expected actual =
  if actual <> expected then fail (name ^ " mismatch")

let test_epoch_profile () =
  let incomplete =
    H.make_epoch_incomplete_profile
      ~epoch_id:9
      ~current_epoch_id:11
      ~limit:50
      ~offset:2
      ~missing:3
      ~expected:10
      ~rows:7
      ~status_ms:12.0
      ~heal_ms:3.0
      ~heal_checked:4
      ~heal_repaired:1
      ~heal_errors:0
      ~retry_ms:6.0
      ~total_ms:999.0
  in
  assert_bool "profile warn below threshold" false
    (H.epoch_profile_should_warn ~warn_ms:1000.0 incomplete);
  begin
    match H.epoch_profile_warning ~warn_ms:1000.0 incomplete with
    | None -> ()
    | Some _ -> fail "profile warning below threshold"
  end;
  assert_string "incomplete profile message"
    "tx_by_epoch profile epoch = 9 current = 11 limit = 50 offset = 2 cache = miss incomplete = true missing = 3 expected = 10 rows = 7 t_status = 12ms t_heal = 3ms heal_checked = 4 heal_repaired = 1 heal_errors = 0 t_retry = 6ms total = 999ms"
    (H.epoch_profile_log_message incomplete);
  let page_incomplete =
    H.make_epoch_incomplete_page_profile
      ~epoch_id:9
      ~current_epoch_id:11
      ~limit:50
      ~offset:2
      ~missing:3
      ~expected:10
      ~rows:[1; 2; 3]
      ~status_ms:12.0
      ~heal_ms:3.0
      ~heal_checked:4
      ~heal_repaired:1
      ~heal_errors:0
      ~retry_ms:6.0
      ~total_ms:999.0
  in
  assert_string "page incomplete profile message"
    "tx_by_epoch profile epoch = 9 current = 11 limit = 50 offset = 2 cache = miss incomplete = true missing = 3 expected = 10 rows = 3 t_status = 12ms t_heal = 3ms heal_checked = 4 heal_repaired = 1 heal_errors = 0 t_retry = 6ms total = 999ms"
    (H.epoch_profile_log_message page_incomplete);
  let page_status =
    H.epoch_page_status_after_heal
      ~status:"retried"
      ~heal_checked:4
      ~heal_repaired:1
      ~heal_errors:0
      ~heal_ms:3.0
      ~retry_ms:6.0
  in
  let page_incomplete_with_status =
    H.make_epoch_incomplete_page_profile_with_status
      ~epoch_id:9
      ~current_epoch_id:11
      ~limit:50
      ~offset:2
      ~missing:3
      ~expected:10
      ~rows:[1; 2; 3]
      ~status_ms:12.0
      ~page_status
      ~total_ms:999.0
  in
  assert_string "page incomplete status profile message"
    "tx_by_epoch profile epoch = 9 current = 11 limit = 50 offset = 2 cache = miss incomplete = true missing = 3 expected = 10 rows = 3 t_status = 12ms t_heal = 3ms heal_checked = 4 heal_repaired = 1 heal_errors = 0 t_retry = 6ms total = 999ms"
    (H.epoch_profile_log_message page_incomplete_with_status);
  let complete =
    H.make_epoch_complete_profile
      ~epoch_id:9
      ~current_epoch_id:12
      ~limit:2
      ~offset:0
      ~expected:5
      ~rows:2
      ~rejected:1
      ~has_more:true
      ~status_ms:8.0
      ~heal_ms:0.0
      ~heal_checked:0
      ~heal_repaired:0
      ~heal_errors:0
      ~retry_ms:0.0
      ~rejected_ms:4.0
      ~json_ms:2.0
      ~total_ms:1000.0
  in
  assert_bool "profile warn at threshold" true
    (H.epoch_profile_should_warn ~warn_ms:1000.0 complete);
  begin
    match H.epoch_profile_warning ~warn_ms:1000.0 complete with
    | Some _ -> ()
    | None -> fail "profile warning missing"
  end;
  assert_string "complete profile message"
    "tx_by_epoch profile epoch = 9 current = 12 limit = 2 offset = 0 cache = miss incomplete = false rows = 2 expected = 5 rejected = 1 has_more = true t_status = 8ms t_heal = 0ms heal_checked = 0 heal_repaired = 0 heal_errors = 0 t_retry = 0ms t_rejected = 4ms t_json = 2ms total = 1000ms"
    (H.epoch_profile_log_message complete);
  let page_complete =
    H.make_epoch_complete_page_profile
      ~epoch_id:10
      ~current_epoch_id:12
      ~limit:2
      ~offset:0
      ~expected:5
      ~rows:[1; 2]
      ~rejected_rows:["r"]
      ~status_ms:8.0
      ~heal_ms:0.0
      ~heal_checked:0
      ~heal_repaired:0
      ~heal_errors:0
      ~retry_ms:0.0
      ~rejected_ms:4.0
      ~json_ms:2.0
      ~total_ms:1000.0
  in
  assert_string "page complete profile message"
    "tx_by_epoch profile epoch = 10 current = 12 limit = 2 offset = 0 cache = miss incomplete = false rows = 2 expected = 5 rejected = 1 has_more = true t_status = 8ms t_heal = 0ms heal_checked = 0 heal_repaired = 0 heal_errors = 0 t_retry = 0ms t_rejected = 4ms t_json = 2ms total = 1000ms"
    (H.epoch_profile_log_message page_complete);
  let page_complete_with_status =
    H.make_epoch_complete_page_profile_with_status
      ~epoch_id:10
      ~current_epoch_id:12
      ~limit:2
      ~offset:0
      ~expected:5
      ~rows:[1; 2]
      ~rejected_rows:["r"]
      ~status_ms:8.0
      ~page_status:(H.epoch_page_status_without_heal "status")
      ~rejected_ms:4.0
      ~json_ms:2.0
      ~total_ms:1000.0
  in
  assert_string "page complete status profile message"
    "tx_by_epoch profile epoch = 10 current = 12 limit = 2 offset = 0 cache = miss incomplete = false rows = 2 expected = 5 rejected = 1 has_more = true t_status = 8ms t_heal = 0ms heal_checked = 0 heal_repaired = 0 heal_errors = 0 t_retry = 0ms t_rejected = 4ms t_json = 2ms total = 1000ms"
    (H.epoch_profile_log_message page_complete_with_status)

let test_recent_rows () =
  let rows = [
    `Assoc ["epoch", `Int 7; "hash", `String "aa"];
    `Assoc ["epoch_id", `Int 8; "hash", `String "bb"];
  ] in
  match H.recent_txs_of_summary_rows rows with
  | [(7, "aa"); (8, "bb")] -> ()
  | _ -> fail "recent row conversion mismatch"

let test_account_response () =
  assert_bool "encrypted balance none" false (H.encrypted_balance_present None);
  assert_bool "encrypted balance zero" false (H.encrypted_balance_present (Some "0"));
  assert_bool "encrypted balance nonzero" true (H.encrypted_balance_present (Some "abc"));
  let json =
    H.account_response
      ~addr:"octabc"
      ~balance:"1.000000"
      ~balance_raw:"1000000"
      ~nonce:9
      ~has_public_key:true
      ~has_encrypted_balance:false
      ~tx_count:2
      ~recent_txs:[(1, "h1")]
      ~rejected_txs:[("r1", 3, 0.0)]
  in
  assert_string "address" "octabc" (string_field "address" json);
  assert_string "balance" "1.000000" (string_field "balance" json);
  assert_string "balance_raw" "1000000" (string_field "balance_raw" json);
  assert_int "nonce" 9 (int_field "nonce" json);
  assert_bool "has_public_key" true (bool_field "has_public_key" json);
  assert_bool "has_encrypted_balance" false (bool_field "has_encrypted_balance" json);
  assert_int "tx_count" 2 (int_field "tx_count" json);
  assert_int "recent_txs length" 1 (List.length (list_field "recent_txs" json));
  assert_int "rejected_txs length" 1 (List.length (list_field "rejected_txs" json));
  let view =
    H.account_view
      ~addr:"octabc"
      ~balance:(Z.of_int 1_000_000)
      ~nonce:9
      ~has_public_key:true
      ~encrypted_balance:(Some "cipher")
      ~tx_count:2
      ~recent_rows:[`Assoc ["epoch", `Int 4; "hash", `String "h4"]]
      ~rejected_txs:[("r1", 3, 0.0)]
  in
  assert_bool "view encrypted" true (bool_field "has_encrypted_balance" view.H.account_json);
  assert_int "view recent count" 1 view.H.account_recent_count;
  assert_int "view rejected count" 1 view.H.account_rejected_count

let test_account_profile () =
  let profile =
    H.make_account_profile
      ~tag:"octra_account"
      ~addr:"octabc"
      ~recent_count:2
      ~rejected_count:1
      ~lookup_ms:1.25
      ~recent_ms:2.5
      ~rejected_ms:3.75
      ~json_ms:4.0
  in
  assert_string "account profile message"
    "account_profile tag = octra_account addr = octabc recent_count = 2 rejected_count = 1 lookup_ms = 1.25 recent_ms = 2.50 rejected_ms = 3.75 json_ms = 4.00 total_ms = 11.50"
    (H.account_profile_log_message profile);
  let view =
    H.account_view
      ~addr:"octabc"
      ~balance:(Z.of_int 1_000_000)
      ~nonce:9
      ~has_public_key:true
      ~encrypted_balance:None
      ~tx_count:1
      ~recent_rows:[`Assoc ["epoch", `Int 4; "hash", `String "h4"]]
      ~rejected_txs:[("r1", 3, 0.0)]
  in
  let profile_from_view =
    H.account_profile_from_view
      ~tag:"octra_account"
      ~addr:"octabc"
      ~view
      ~t0:0.0
      ~t1:0.001
      ~t2:0.003
      ~t3:0.006
      ~t4:0.010
  in
  assert_string "account profile from view"
    "account_profile tag = octra_account addr = octabc recent_count = 1 rejected_count = 1 lookup_ms = 1.00 recent_ms = 2.00 rejected_ms = 3.00 json_ms = 4.00 total_ms = 10.00"
    (H.account_profile_log_message profile_from_view)

let test_page_responses () =
  assert_bool "has more true" true (H.page_has_more ~offset:5 ~count:5 ~total:11);
  assert_bool "has more false" false (H.page_has_more ~offset:5 ~count:5 ~total:10);
  assert_int "target default count" 5
    (List.length (H.transaction_epoch_targets [9; 8; 7; 6; 5; 4]));
  assert_int "target existing" 8
    (match H.transaction_epoch_targets ~epoch_param:8 [9; 8; 7] with
     | [epoch] -> epoch
     | _ -> fail "target existing mismatch");
  assert_int "target missing count" 0
    (List.length (H.transaction_epoch_targets ~epoch_param:10 [9; 8; 7]));
  let query = H.transactions_query (`List [`Int 8; `Int 2]) in
  assert_int "transactions query epoch" 8
    (match query.H.transactions_epoch_param with
     | Some epoch -> epoch
     | None -> fail "missing transactions epoch");
  assert_int "transactions query limit" 2 query.H.transactions_limit;
  let ready =
    H.transactions_list_result
      ~epoch_status:(fun _ -> None)
      ~status_ok:(fun _ -> true)
      ~status_fields:(fun _ -> [])
      ~load_epoch:(fun epoch -> [Printf.sprintf "h%d" epoch, "{}"])
      (`List [`Int 8; `Int 2])
      [9; 8; 7]
  in
  begin
    match ready with
    | H.Transactions_ready [(8, "h8", "{}")] -> ()
    | _ -> fail "transactions ready result mismatch"
  end;
  let incomplete =
    H.transactions_list_result
      ~epoch_status:(fun epoch -> if epoch = 9 then Some "bad" else None)
      ~status_ok:(fun _ -> false)
      ~status_fields:(fun status -> ["status", `String status])
      ~load_epoch:(fun _ -> [])
      (`List [])
      [9; 8; 7]
  in
  begin
    match H.transactions_list_response incomplete with
    | Error e when e.Octra_core.Rpc.code = 116 -> ()
    | Error e -> fail (Printf.sprintf "transactions incomplete code %d" e.Octra_core.Rpc.code)
    | Ok _ -> fail "transactions incomplete accepted"
  end;
  let default_page = H.rpc_page (`List []) in
  assert_int "default page limit" 50 default_page.H.limit;
  assert_int "default page offset" 0 default_page.H.offset;
  let epoch_page =
    match H.epoch_page_request
            ~limit_max:100
            (`List [`Int 9; `Int 120; `Int (-4)]) with
    | Ok page -> page
    | Error e -> fail ("epoch page rejected " ^ e.Octra_core.Rpc.message)
  in
  assert_int "epoch page id" 9 epoch_page.H.epoch_page_id;
  assert_int "epoch page limit" 100 epoch_page.H.epoch_page_limit;
  assert_int "epoch page offset" 0 epoch_page.H.epoch_page_offset;
  if H.epoch_page_cache_ttl
      ~current_epoch_id:11
      ~recent_ttl:5.0
      ~old_ttl:30.0
      ~epoch_id:9 <> 5.0 then
    fail "epoch recent ttl mismatch";
  if H.epoch_page_cache_ttl
      ~current_epoch_id:11
      ~recent_ttl:5.0
      ~old_ttl:30.0
      ~epoch_id:8 <> 30.0 then
    fail "epoch old ttl mismatch";
  let cache_key =
    H.epoch_page_cache_key
      ~epoch_id:9
      ~start_txid:11L
      ~tx_count:2
      ~limit:50
      ~offset:0
  in
  let changed_key =
    H.epoch_page_cache_key
      ~epoch_id:9
      ~start_txid:11L
      ~tx_count:3
      ~limit:50
      ~offset:0
  in
  assert_bool "epoch cache binds tx count" false (cache_key = changed_key);
  let deadline = H.epoch_page_cache_deadline ~now:10.0 ~ttl:5.0 in
  assert_bool "epoch cache live before deadline" true
    (H.epoch_page_cache_live ~now:14.0 ~deadline);
  assert_bool "epoch cache dead after fixed deadline" false
    (H.epoch_page_cache_live ~now:16.0 ~deadline);
  begin
    match
      H.epoch_page_cache_plan
        ~current_epoch_id:11
        ~recent_ttl:5.0
        ~old_ttl:30.0
        ~epoch_id:11
        ~header:None
        ~limit:50
        ~offset:0
    with
    | None -> ()
    | Some _ -> fail "unfinished epoch entered cache"
  end;
  begin
    match
      H.epoch_page_cache_plan
        ~current_epoch_id:11
        ~recent_ttl:5.0
        ~old_ttl:30.0
        ~epoch_id:11
        ~header:(Some (11L, 2))
        ~limit:50
        ~offset:0
    with
    | Some (_, ttl) when ttl = 5.0 -> ()
    | Some _ -> fail "finalized epoch cache ttl mismatch"
    | None -> fail "finalized epoch cache plan missing"
  end;
  let no_heal = H.epoch_page_status_without_heal "status" in
  assert_string "no heal status" "status" no_heal.H.epoch_page_status;
  assert_int "no heal checked" 0 no_heal.H.epoch_page_heal_checked;
  assert_bool "no heal log" false (H.epoch_page_heal_should_log no_heal);
  let healed =
    H.epoch_page_status_after_heal
      ~status:"retried"
      ~heal_checked:5
      ~heal_repaired:1
      ~heal_errors:0
      ~heal_ms:2.0
      ~retry_ms:3.0
  in
  assert_string "healed status" "retried" healed.H.epoch_page_status;
  assert_int "healed repaired" 1 healed.H.epoch_page_heal_repaired;
  assert_bool "healed log" true (H.epoch_page_heal_should_log healed);
  let time = ref 0.0 in
  let tick () =
    time := !time +. 0.001;
    !time
  in
  let healed_by_callback =
    H.epoch_page_status_with_heal
      ~is_incomplete:(fun status -> status = "old")
      ~recent_epochs:4
      ~now:tick
      ~heal:(fun () -> 8, 2, 1)
      ~retry:(fun () -> "new")
      "old"
  in
  assert_string "callback status" "new" healed_by_callback.H.epoch_page_status;
  assert_int "callback checked" 8 healed_by_callback.H.epoch_page_heal_checked;
  assert_int "callback repaired" 2 healed_by_callback.H.epoch_page_heal_repaired;
  assert_int "callback errors" 1 healed_by_callback.H.epoch_page_heal_errors;
  assert_bool "callback log" true (H.epoch_page_heal_should_log healed_by_callback);
  let capped_page = H.rpc_page (`List [`String "oct"; `Int 700; `Int (-4)]) in
  assert_int "capped page limit" 500 capped_page.H.limit;
  assert_int "negative page offset" 0 capped_page.H.offset;
  let custom_page =
    H.rpc_page
      ~limit_index:0
      ~offset_index:1
      ~limit_max:100
      ~default_limit:15
      (`List [`Int 120; `Int 9]) in
  assert_int "custom page limit" 100 custom_page.H.limit;
  assert_int "custom page offset" 9 custom_page.H.offset;
  let txs = [`Assoc ["hash", `String "h1"]] in
  let rejected = [`Assoc ["hash", `String "r1"]] in
  let dropped = [`Assoc ["hash", `String "d1"]] in
  let address_page =
    H.address_transactions_response
      ~addr:"octabc"
      ~total:11
      ~offset:5
      ~limit:5
      ~transactions:txs
      ~rejected
      ~dropped
  in
  assert_int "address count" 1 (int_field "count" address_page);
  assert_bool "address has_more" true (bool_field "has_more" address_page);
  assert_int "address rejected" 1 (List.length (list_field "rejected" address_page));
  assert_int "address dropped" 1 (List.length (list_field "dropped" address_page));
  let recent_result =
    H.recent_transactions_result
      ~mask_rows:(fun rows -> rows)
      ~page:custom_page
      ~incomplete:false
      ~missing:0
      ~rows:txs
  in
  begin
    match recent_result with
    | Ok json -> assert_int "recent result count" 1 (int_field "count" json)
    | Error _ -> fail "recent result rejected"
  end;
  let recent_error =
    H.recent_transactions_result
      ~mask_rows:(fun rows -> rows)
      ~page:custom_page
      ~incomplete:true
      ~missing:4
      ~rows:[]
  in
  begin
    match recent_error with
    | Error e when e.Octra_core.Rpc.code = 116 -> ()
    | Error e -> fail (Printf.sprintf "recent result code %d" e.Octra_core.Rpc.code)
    | Ok _ -> fail "recent incomplete accepted"
  end;
  let mark_rows =
    List.map (function
      | `Assoc fields -> `Assoc (("masked", `Bool true) :: fields)
      | row -> row)
  in
  let address_result =
    H.address_transactions_result
      ~mask_rows:mark_rows
      ~addr:"octabc"
      ~page:custom_page
      ~total:1
      ~incomplete:false
      ~missing:0
      ~rows:[`Assoc ["hash", `String "t1"]]
      ~rejected:[`Assoc ["hash", `String "r1"]]
      ~dropped:[
        `Assoc ["hash", `String "t1"];
        `Assoc ["hash", `String "r1"];
        `Assoc ["hash", `String "d1"];
      ]
  in
  begin
    match address_result with
    | Error _ -> fail "address result rejected"
    | Ok json ->
      match list_field "dropped" json with
      | [row] ->
        assert_string "distinct drop hash" "d1" (string_field "hash" row);
        assert_bool "drop mask" true (bool_field "masked" row)
      | _ -> fail "distinct drop count"
  end;
  let address_error =
    H.address_transactions_result
      ~mask_rows:(fun rows -> rows)
      ~addr:"octabc"
      ~page:custom_page
      ~total:11
      ~incomplete:true
      ~missing:6
      ~rows:[]
      ~rejected:[]
      ~dropped:[]
  in
  begin
    match address_error with
    | Error e when e.Octra_core.Rpc.code = 116 -> ()
    | Error e -> fail (Printf.sprintf "address result code %d" e.Octra_core.Rpc.code)
    | Ok _ -> fail "address incomplete accepted"
  end;
  let token_result =
    H.token_transactions_result
      ~mask_rows:(fun rows -> rows)
      ~addr:"octabc"
      ~page:custom_page
      ~total:2
      ~has_more:false
      ~incoming:1
      ~outgoing:1
      ~incomplete:false
      ~missing:0
      ~rows:txs
  in
  begin
    match token_result with
    | Ok json -> assert_int "token result incoming" 1 (int_field "incoming" json)
    | Error _ -> fail "token result rejected"
  end;
  let rejected_page =
    H.rejected_response
      ~addr:"octabc"
      ~total:6
      ~offset:5
      ~limit:5
      ~rejected
  in
  assert_bool "rejected has_more" false (bool_field "has_more" rejected_page);
  let rejected_result =
    H.rejected_transactions_result
      ~mask_rows:(fun rows -> rows)
      ~addr:"octabc"
      ~page:custom_page
      ~total:6
      ~rows:rejected
  in
  begin
    match rejected_result with
    | Ok json -> assert_int "rejected result count" 1 (int_field "count" json)
    | Error _ -> fail "rejected result failed"
  end

let test_http_summary_response () =
  let json =
    H.http_address_summary_response
      ~addr:"octabc"
      ~balance:"1.000000"
      ~balance_raw:"1000000"
      ~nonce:9
      ~has_public_key:true
      ~transaction_count:1
      ~recent_txs:[(4, "hash4")]
  in
  let recent = list_field "recent_transactions" json in
  match recent with
  | [row] ->
      assert_string "summary url" "/tx/hash4" (string_field "url" row)
  | _ -> fail "summary recent length mismatch"

let test_epoch_transaction_responses () =
  let summary =
    H.epoch_transactions_response [
      (7, "h7", "{}");
      (8, "h8", "{}");
    ]
  in
  assert_int "epoch tx count" 2 (int_field "count" summary);
  begin
    match list_field "transactions" summary with
    | first :: _ ->
        assert_string "epoch tx hash" "h7" (string_field "hash" first);
        assert_int "epoch tx epoch" 7 (int_field "epoch" first)
    | [] -> fail "missing epoch tx row"
  end;
  let recent =
    H.recent_transactions_response
      ~transactions:[`Assoc ["hash", `String "recent"]]
  in
  assert_int "recent count" 1 (int_field "count" recent);
  assert_int "recent rejected empty" 0 (List.length (list_field "rejected" recent));
  let page =
    H.epoch_transactions_page_response
      ~epoch_id:9
      ~expected_confirmed_count:3
      ~offset:0
      ~limit:2
      ~transactions:[
        `Assoc ["hash", `String "a"];
        `Assoc ["hash", `String "b"];
      ]
      ~rejected:[`Assoc ["hash", `String "r"]]
  in
  assert_int "epoch page count" 3 (int_field "count" page);
  assert_int "epoch page confirmed" 2 (int_field "confirmed_count" page);
  assert_int "epoch page rejected" 1 (int_field "rejected_count" page);
  assert_bool "epoch page has more" true (bool_field "has_more" page);
  let page_from_rows =
    H.epoch_transactions_page_response_of_rows
      ~mask_rows:(fun rows -> rows)
      ~epoch_id:9
      ~expected_confirmed_count:3
      ~offset:0
      ~limit:2
      ~rows:[
        `Assoc ["hash", `String "a"];
        `Assoc ["hash", `String "b"];
      ]
      ~rejected_rows:[`Assoc ["hash", `String "r"]]
  in
  assert_int "epoch page rows count" 3 (int_field "count" page_from_rows);
  assert_bool "epoch page rows has more" true (bool_field "has_more" page_from_rows)

let test_index_incomplete () =
  let err = H.index_incomplete "broken" ["missing", `Int 2] in
  assert_int "rpc code" 116 err.Octra_core.Rpc.code;
  assert_string "rpc message" "history index incomplete" err.Octra_core.Rpc.message;
  match err.Octra_core.Rpc.data with
  | Some data ->
      assert_string "detail" "broken" (string_field "detail" data);
      assert_int "missing" 2 (int_field "missing" data)
  | None -> fail "missing rpc error data"

let test_incomplete_epoch_status () =
  let err = H.epoch_incomplete_status_error ~epoch_id:7 ~missing:3 None in
  assert_int "epoch status code" 116 err.Octra_core.Rpc.code;
  match err.Octra_core.Rpc.data with
  | Some data ->
      assert_int "epoch status id" 7 (int_field "epoch_id" data);
      assert_int "epoch status missing" 3 (int_field "missing" data)
  | None -> fail "missing epoch status data"

let test_incomplete_error_helpers () =
  let recent = H.recent_incomplete_error ~offset:3 ~limit:4 ~missing:5 in
  begin
    match recent.Octra_core.Rpc.data with
    | Some data ->
        assert_string "recent detail" "recent transactions view incomplete" (string_field "detail" data);
        assert_int "recent offset" 3 (int_field "offset" data)
    | None -> fail "missing recent error data"
  end;
  let address = H.address_incomplete_error ~addr:"octabc" ~offset:1 ~limit:2 ~missing:3 in
  begin
    match address.Octra_core.Rpc.data with
    | Some data ->
        assert_string "address detail" "address history incomplete for octabc" (string_field "detail" data);
        assert_string "address addr" "octabc" (string_field "address" data)
    | None -> fail "missing address error data"
  end;
  let token = H.token_incomplete_error ~addr:"octabc" ~offset:1 ~limit:2 ~missing:3 in
  begin
    match token.Octra_core.Rpc.data with
    | Some data ->
        assert_string "token detail" "token transfer history incomplete for octabc" (string_field "detail" data)
    | None -> fail "missing token error data"
  end;
  let epoch = H.epoch_incomplete_error ~epoch_id:9 ["missing", `Int 1] in
  begin
    match epoch.Octra_core.Rpc.data with
    | Some data ->
        assert_string "epoch detail" "epoch 9 history index incomplete" (string_field "detail" data)
    | None -> fail "missing epoch error data"
  end

let test_epoch_index_status_fields () =
  let status = {
    Octra_core.Store_chaindata.epoch_id = 7;
    expected_start_txid = 9L;
    expected_tx_count = 3;
    checked = 2;
    missing_epoch_meta = true;
    missing_txid_loc = 1;
    missing_tx_loc = 2;
    missing_addr_refs = 3;
    malformed_records = 4;
    errors = ["first"; "second"];
  } in
  let json = `Assoc (H.epoch_index_status_fields status) in
  assert_string "first error" "first" (string_field "first_error" json);
  assert_int "epoch id" 7 (int_field "epoch_id" json);
  assert_int "error count" 2 (int_field "error_count" json)

let test_heal_limit () =
  assert_int "negative lookup limit" 0 (HR.lookup_record_limit (-1));
  assert_int "default lookup limit" 256 (HR.lookup_record_limit 256);
  assert_int "maximum lookup limit" 4096 (HR.lookup_record_limit 5000)

let test_persisted_drop_lookup () =
  Staging.clear ();
  let data_dir = Test_workspace.unique_dir "history_drop" in
  let hash = String.make 64 'a' in
  let expected =
    Drop.{
      hash;
      from_addr = "octFrom";
      to_addr = "octTo";
      nonce = 7;
      ou = Z.of_int 8;
      op_type = Transaction.ClaimOp;
      reason = "expired";
      detail = "TTL exceeded";
      dropped_at = 9.0;
    }
  in
  let public_hash = String.make 64 'b' in
  let public_drop =
    Drop.{
      expected with
      hash = public_hash;
      op_type = Transaction.ProgramExec;
      dropped_at = 8.0;
    }
  in
  let db = Drop.open_db data_dir in
  begin
    match Drop.save_many db [expected; public_drop] with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  Drop.close db;
  let reopened = Drop.open_db data_dir in
  let chaindata = SC.open_chaindata (Filename.concat data_dir "chaindata") in
  Fun.protect
    ~finally:(fun () ->
      SC.close chaindata;
      Drop.close reopened)
    (fun () ->
      begin
      match
        Lwt_main.run
          (HR.transaction
             ~find_drop:(Drop.find reopened)
             chaindata
             ~params:(`List [`String hash]))
      with
      | Error error -> fail error.Octra_core.Rpc.message
      | Ok json ->
        assert_string "persisted drop status" "dropped" (string_field "status" json);
        assert_string "persisted drop reason" "expired" (string_field "reason" json);
        assert_string "persisted drop detail" "TTL exceeded" (string_field "detail" json);
        assert_string "persisted drop from" "-" (string_field "from" json);
        assert_string "persisted drop to" "-" (string_field "to_" json)
      end;
      match
        Lwt_main.run
          (HR.transactions_by_address
             ~drops_by_addr:(Drop.by_addr reopened)
             chaindata
             ~params:(`List [`String expected.from_addr; `Int 10; `Int 0])
             ~addr:expected.from_addr)
      with
      | Error error -> fail error.Octra_core.Rpc.message
      | Ok json ->
        begin
          match list_field "dropped" json with
          | [row] ->
            assert_string "address drop hash" public_hash (string_field "hash" row);
            assert_string "address drop status" "dropped" (string_field "status" row);
            assert_string "address drop reason" "expired" (string_field "reason" row);
            assert_string "address drop from" expected.from_addr (string_field "from" row);
            assert_string "address drop to" expected.to_addr (string_field "to" row);
            assert_string "address drop to_" expected.to_addr (string_field "to_" row)
          | _ -> fail "address drop row count"
        end;
      SC.begin_batch chaindata;
      SC.save_rejected
        chaindata
        ~hash:public_hash
        ~from_addr:expected.from_addr
        ~to_addr:expected.to_addr
        ~amount:"0"
        ~nonce:expected.nonce
        ~error_type:"claim_failed"
        ~reason:"rejected"
        ~epoch_id:1
        ~ts:10.0;
      SC.commit_batch chaindata;
      begin match Lwt_main.run (HR.transaction
        ~find_drop:(fun _ -> failwith "committed lookup reached local journal")
        chaindata ~params:(`List [`String public_hash])) with
      | Ok json -> assert_string "committed rejection priority" "rejected"
          (string_field "status" json)
      | Error error -> fail error.Octra_core.Rpc.message
      end;
      match
        Lwt_main.run
          (HR.transactions_by_address
             ~drops_by_addr:(Drop.by_addr reopened)
             chaindata
             ~params:(`List [`String expected.from_addr; `Int 10; `Int 0])
             ~addr:expected.from_addr)
      with
      | Error error -> fail error.Octra_core.Rpc.message
      | Ok json ->
        assert_int "superseded drop count" 0 (List.length (list_field "dropped" json)))

let () =
  let root = Test_workspace.unique_dir "drop_read" in
  let store = SC.open_chaindata (Filename.concat root "chaindata") in
  Fun.protect ~finally:(fun () -> SC.close store) (fun () ->
    let result = Lwt_main.run (HR.transactions_by_address
      ~drops_by_addr:(fun _ ~limit:_ ~offset:_ -> failwith "drop read failure")
      store ~params:(`List [`String "octFrom"]) ~addr:"octFrom") in
    match result with
    | Error error -> fail error.Octra_core.Rpc.message
    | Ok json ->
      assert_string "drop read availability" "unavailable" (string_field "dropped_status" json);
      assert_int "drop read rows" 0 (List.length (list_field "dropped" json)));
  let root = Test_workspace.unique_dir "drop_hash" in
  let store = SC.open_chaindata (Filename.concat root "chaindata") in
  Fun.protect ~finally:(fun () -> SC.close store) (fun () ->
    let hash = String.make 64 'b' in
    let result = Lwt_main.run (HR.transaction
      ~find_drop:(fun _ -> failwith "drop read failure")
      store ~params:(`List [`String hash])) in
    match result with
    | Ok _ -> fail "failed local journal became transaction evidence"
    | Error error ->
      assert_int "drop hash availability" 110 error.Octra_core.Rpc.code;
      let data = Option.get error.data in
      assert_string "drop hash status" "unavailable" (string_field "dropped_status" data));
  test_epoch_profile ();
  test_recent_rows ();
  test_account_response ();
  test_account_profile ();
  test_page_responses ();
  test_http_summary_response ();
  test_epoch_transaction_responses ();
  test_index_incomplete ();
  test_incomplete_epoch_status ();
  test_incomplete_error_helpers ();
  test_epoch_index_status_fields ();
  test_heal_limit ();
  test_persisted_drop_lookup ();
  print_endline "status = pass test = node_history"