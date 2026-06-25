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


module Denomination = Octra_core.Denomination
module Head_manifest = Octra_core.Head_manifest
module Staging = Octra_core.Tx_staging
module Transaction = Octra_core.Transaction
module Webhooks = Octra_core.Webhooks

let opt_hex = function
  | None -> `Null
  | Some s -> `String s

let node_root_response ~validator ~epoch =
  `Assoc [
    "node", `String "octra";
    "version", `String "3.0.0";
    "validator", `String validator;
    "epoch", `Int epoch;
    "status", `String "running";
  ]

let error_response ~error_type ~reason =
  `Assoc [
    "status", `String "error";
    "error", `Assoc [
      "type", `String error_type;
      "reason", `String reason;
    ];
  ]

let head_fields = function
  | None ->
    [
      "head_epoch", `Null;
      "state_root", `Null;
      "txid_hi", `Null;
      "irmin_commit", `Null;
    ]
  | Some h ->
    [
      "head_epoch", `Int h.Head_manifest.epoch_id;
      "state_root", `String h.Head_manifest.state_root;
      "txid_hi", `String (Int64.to_string h.Head_manifest.txid_hi);
      "irmin_commit", opt_hex h.Head_manifest.irmin_commit;
    ]

let status_response ~epoch ~validator ~root_count ~timestamp ~total_accounts
    ~total_supply ~encrypted_supply ~active_accounts ~head =
  let display = Z.add total_supply encrypted_supply in
  `Assoc ([
    "current_epoch", `Int epoch;
    "validator", `String validator;
    "root_count", `Int root_count;
    "timestamp", `Float timestamp;
    "total_accounts", `Int total_accounts;
    "total_supply", `String (Denomination.format_balance display);
    "circulating_supply", `String (Denomination.format_balance display);
    "encrypted_supply", `String (Denomination.format_balance encrypted_supply);
    "active_accounts", `Int active_accounts;
    "network_version", `String "v3.0.0-irmin";
    "storage_backend", `String "irmin-pack";
  ] @ head_fields head)

let network_stats_response ~total_accounts ~active_accounts ~total_supply
    ~recent_tx_count ~staging_size ~latest_epochs =
  `Assoc [
    "total_accounts", `Int total_accounts;
    "active_accounts", `Int active_accounts;
    "total_supply", `String (Denomination.format_balance total_supply);
    "total_supply_raw", `String (Z.to_string total_supply);
    "recent_transaction_count", `Int recent_tx_count;
    "staging_size", `Int staging_size;
    "latest_epochs", `List (List.map (fun e -> `Int e) latest_epochs);
    "signature_verification", `String "enabled";
    "address_validation", `String "strict";
    "storage_backend", `String "chaindata-lmdb";
  ]

let int_param name params default =
  match params with
  | `Assoc fields ->
    begin
      match List.assoc_opt name fields with
      | Some (`Int n) -> n
      | _ -> default
    end
  | _ -> default

let epoch_list_response ~current_epoch params =
  let limit =
    match params with
    | `List (`Int l :: _) -> min l 100
    | `Assoc _ -> min (int_param "limit" params 25) 100
    | _ -> 25
  in
  let offset =
    match params with
    | `List (_ :: `Int o :: _) -> o
    | `Assoc _ -> int_param "offset" params 0
    | _ -> 0
  in
  let start = current_epoch - 1 - offset in
  let epochs = List.init (min limit (max 0 (start + 1))) (fun i -> start - i) in
  `Assoc [
    "total", `Int current_epoch;
    "epochs", `List (List.map (fun e -> `Int e) epochs);
    "has_more", `Bool (start - limit >= 0);
  ]

let extract_ints values =
  List.filter_map
    (function
      | `Int i -> Some i
      | _ -> None)
    values

let epoch_summary_ids params =
  let ids =
    match params with
    | `List [`List inner] -> extract_ints inner
    | `List values -> extract_ints values
    | _ -> []
  in
  if List.length ids > 100 then List.filteri (fun i _ -> i < 100) ids
  else ids

let epoch_current_response ~epoch_id ~root_count =
  `Assoc [
    "epoch_id", `Int epoch_id;
    "roots", `Int root_count;
  ]

let epoch_get_response = function
  | Some summary -> Ok summary
  | None -> Error (Octra_core.Rpc.not_found "epoch not found")

let epoch_summaries_response summaries =
  `List summaries

let epoch_summaries_response_of_ids ~find_summary ids =
  ids
  |> List.filter_map find_summary
  |> epoch_summaries_response

let search_pending_tx ~hash ~fields =
  `Assoc [
    "type", `String "transaction";
    "status", `String "pending";
    "hash", `String hash;
    "data", `Assoc fields;
  ]

let search_confirmed_tx ~hash ~epoch =
  `Assoc [
    "type", `String "transaction";
    "status", `String "confirmed";
    "hash", `String hash;
    "epoch", `Int epoch;
  ]

let search_account ~addr ~balance ~nonce =
  `Assoc [
    "type", `String "account";
    "address", `String addr;
    "balance", `String (Denomination.format_balance balance);
    "nonce", `Int nonce;
  ]

let search_epoch ~epoch =
  `Assoc [
    "type", `String "epoch";
    "epoch_id", `Int epoch;
  ]

type search_query =
  | Search_too_short
  | Search_hash of string
  | Search_address of string
  | Search_epoch of int
  | Search_unknown

type search_result =
  | Search_too_short_result
  | Search_pending_tx of string * (string * Yojson.Safe.t) list
  | Search_confirmed_tx of string * int
  | Search_account_result of string * Z.t * int
  | Search_epoch_result of int
  | Search_not_found of string

let search_query raw =
  let query = String.trim raw in
  if String.length query < 3 then Search_too_short
  else
    match Octra_core.Rpc.sanitize_hash query with
    | Some hash -> Search_hash hash
    | None ->
      match Octra_core.Rpc.sanitize_address query with
      | Some addr -> Search_address addr
      | None ->
        try Search_epoch (int_of_string query)
        with _ -> Search_unknown

let search_response = function
  | Search_too_short_result ->
    Error (Octra_core.Rpc.invalid_params "query too short (min 3 chars)")
  | Search_pending_tx (hash, fields) ->
    Ok (search_pending_tx ~hash ~fields)
  | Search_confirmed_tx (hash, epoch) ->
    Ok (search_confirmed_tx ~hash ~epoch)
  | Search_account_result (addr, balance, nonce) ->
    Ok (search_account ~addr ~balance ~nonce)
  | Search_epoch_result epoch ->
    Ok (search_epoch ~epoch)
  | Search_not_found message ->
    Error (Octra_core.Rpc.not_found message)

let search_result_of_query
    ~pending_tx
    ~confirmed_tx
    ~account
    ~epoch_exists
    raw =
  match search_query raw with
  | Search_too_short ->
    Search_too_short_result
  | Search_hash hash ->
    begin
      match pending_tx hash with
      | Some fields -> Search_pending_tx (hash, fields)
      | None ->
        match confirmed_tx hash with
        | Some epoch -> Search_confirmed_tx (hash, epoch)
        | None -> Search_not_found "no match"
    end
  | Search_address addr ->
    begin
      match account addr with
      | Some (balance, nonce) -> Search_account_result (addr, balance, nonce)
      | None -> Search_not_found "address not found"
    end
  | Search_epoch epoch ->
    if epoch_exists epoch then Search_epoch_result epoch
    else Search_not_found "epoch not found"
  | Search_unknown ->
    Search_not_found "no match"

let staging_tx_row ~hash ~fields =
  `Assoc (("hash", `String hash) :: fields)

let staging_view_response ~tx_rows =
  `Assoc [
    "count", `Int (List.length tx_rows);
    "transactions", `List tx_rows;
  ]

let staging_view_response_of_txs ~tx_hash ~tx_fields txs =
  let tx_rows =
    List.map
      (fun tx -> staging_tx_row ~hash:(tx_hash tx) ~fields:(tx_fields tx))
      txs
  in
  staging_view_response ~tx_rows

let staging_removed_response ~tx_hash =
  `Assoc [
    "removed", `Bool true;
    "tx_hash", `String tx_hash;
  ]

let txs_row (epoch, hash, tx_json) =
  `Assoc [
    "hash", `String hash;
    "epoch", `Int epoch;
    "data", `String tx_json;
    "url", `String ("/tx/" ^ hash);
  ]

let txs_response rows =
  `Assoc [
    "total_found", `Int (List.length rows);
    "transactions", `List (List.map txs_row rows);
  ]

let send_tx_accepted_response ~tx_hash ~nonce ~ou_cost ~pending_from_sender ~total_staging_size =
  `Assoc [
    "status", `String "accepted";
    "finality", `String "pending";
    "phase", `String "staging";
    "tx_hash", `String tx_hash;
    "nonce", `Int nonce;
    "ou_cost", `String (Z.to_string ou_cost);
    "message", `String "queued for epoch validation";
    "staging_info", `Assoc [
      "pending_from_sender", `Int pending_from_sender;
      "total_staging_size", `Int total_staging_size;
    ];
  ]

let by_sender_rows by_sender =
  Hashtbl.fold
    (fun addr (count, total) acc ->
      `Assoc [
        "address", `String addr;
        "tx_count", `Int count;
        "total_value", `String (Denomination.format_balance total);
      ] :: acc)
    by_sender
    []

let staging_stats_response ~tx_count ~total_ou ~max_ou ~by_sender =
  `Assoc [
    "total_transactions", `Int tx_count;
    "total_ou", `String (Z.to_string total_ou);
    "max_ou", `String (Z.to_string max_ou);
    "ou_remaining", `String (Z.to_string (Z.sub max_ou total_ou));
    "by_sender", `List (by_sender_rows by_sender);
  ]

let webhook_row id (config : Webhooks.config) =
  `Assoc [
    "id", `String id;
    "url", `String config.Webhooks.url;
    "events", `List (List.map (fun e -> `String e) config.events);
    "active", `Bool config.active;
    "failures", `Int config.failures;
  ]

let webhooks_response webhooks =
  let rows =
    Hashtbl.fold
      (fun id config acc -> webhook_row id config :: acc)
      webhooks
      []
  in
  `Assoc ["webhooks", `List rows]

let webhook_events fields =
  match List.assoc_opt "events" fields with
  | Some (`List values) ->
    Some (
      List.filter_map
        (function
          | `String s -> Some s
          | _ -> None)
        values)
  | _ -> None

let webhook_registered_response ~id =
  `Assoc [
    "id", `String id;
    "status", `String "registered";
  ]

let webhook_unregistered_response ~id =
  `Assoc [
    "status", `String "unregistered";
    "id", `String id;
  ]

let gone_encrypt_balance =
  "REMOVED: Use octra_encryptBalance RPC via signed transaction."

let gone_decrypt_balance =
  "DEPRECATED: Use octra_decryptBalance RPC - private keys must never leave the client."

let gone_view_encrypted_balance =
  "DEPRECATED: Use octra_getEncryptedBalance RPC - decryption happens client-side only."

let gone_register_pvac_pubkey =
  "DEPRECATED: Submit a canonical key_switch transaction."

let gone_private_transactions =
  "DEPRECATED: Private transaction history requires client-side decryption."

let gone_pending_private_transfers =
  "DEPRECATED: Use octra_stealthOutputs RPC + client-side scanning."

let gone_claim_private_transfer =
  "DEPRECATED: Use ClaimOp transaction via octra_sendTransaction RPC - private keys must never leave the client."

let encrypted_cipher_response ~addr ~cipher ~cipher_type =
  `Assoc [
    "address", `String addr;
    "cipher", `String cipher;
    "cipher_type", `String cipher_type;
  ]

let private_transfer_disabled_response =
  `Assoc [
    "status", `String "error";
    "message", `String "method [private_transfer] is not supported in the current client version";
  ]

let gone_legacy_rest =
  "DEPRECATED: REST compatibility endpoint removed; use POST /rpc."

let starts prefix path =
  String.starts_with ~prefix path

let legacy_rest_path ~meth ~path =
  match meth, path with
  | "GET", "/total-txs"
  | "GET", "/circulating-supply"
  | "GET", "/total-supply"
  | "GET", "/max-supply"
  | "GET", "/trees"
  | "GET", "/txs"
  | "GET", "/epochs"
  | "GET", "/epoch/current"
  | "GET", "/network/stats"
  | "GET", "/staging"
  | "GET", "/staging/stats"
  | "GET", "/staging/estimate-ou"
  | "GET", "/webhooks"
  | "GET", "/pending_private_transfers"
  | "POST", "/send-tx"
  | "POST", "/encrypt_balance"
  | "POST", "/decrypt_balance"
  | "POST", "/register_pvac_pubkey"
  | "POST", "/private_transfer"
  | "POST", "/claim_private_transfer"
  | "POST", "/send-batch"
  | "POST", "/deploy-contract"
  | "POST", "/call-contract"
  | "POST", "/contract/call-view"
  | "POST", "/webhooks"
  | "POST", "/validate-address" -> true
  | "DELETE", path when starts "/staging/remove/" path -> true
  | "DELETE", path when starts "/webhooks/" path -> true
  | "GET", path ->
    starts "/balance/" path
    || starts "/public_key/" path
    || starts "/address/" path
    || starts "/rejected/" path
    || starts "/validate-address/" path
    || starts "/tree/" path
    || starts "/tx/" path
    || starts "/epoch/" path
    || starts "/view_encrypted_balance/" path
    || starts "/encrypted_cipher/" path
    || starts "/private_transactions/" path
    || starts "/contract/" path
  | _ -> false

let sorted_ou ou_values =
  List.sort Z.compare ou_values

let percentile n sorted =
  let len = List.length sorted in
  if len = 0 then Z.zero
  else List.nth sorted (min (len - 1) ((n * len) / 100))

let recommended_ou len sorted =
  if len < 10 then "1000"
  else Z.to_string (percentile 60 sorted)

let staging_ou_rpc_response ~ou_values ~staging_ou ~capacity =
  let sorted = sorted_ou ou_values in
  let len = List.length ou_values in
  `Assoc [
    "staging_size", `Int len;
    "p50", `String (Z.to_string (percentile 50 sorted));
    "p75", `String (Z.to_string (percentile 75 sorted));
    "p95", `String (Z.to_string (percentile 95 sorted));
    "recommended", `String (recommended_ou len sorted);
    "staging_ou", `String (Z.to_string staging_ou);
    "epoch_capacity", `String (Z.to_string capacity);
  ]

let staging_ou_rpc_response_of_txs ~tx_ou ~txs ~staging_ou ~capacity =
  staging_ou_rpc_response
    ~ou_values:(List.map tx_ou txs)
    ~staging_ou
    ~capacity

let staging_ou_rest_response ~ou_values ~max_ou ~used_ou ~capacity =
  let sorted = sorted_ou ou_values in
  let len = List.length ou_values in
  `Assoc [
    "current_staging_size", `Int len;
    "max_staging_ou", `String (Z.to_string max_ou);
    "used_ou", `String (Z.to_string used_ou);
    "ou_percentiles", `Assoc [
      "p25", `String (Z.to_string (percentile 25 sorted));
      "p50", `String (Z.to_string (percentile 50 sorted));
      "p75", `String (Z.to_string (percentile 75 sorted));
      "p95", `String (Z.to_string (percentile 95 sorted));
    ];
    "recommended_ou", `String (recommended_ou len sorted);
    "epoch_capacity", `String (Z.to_string capacity);
  ]

let read_nonnegative_int = function
  | `Int n when n >= 0 -> Some n
  | `Intlit s ->
    (try
       let n = int_of_string s in
       if n >= 0 then Some n else None
     with _ -> None)
  | `String s ->
    (try
       let n = int_of_string s in
       if n >= 0 then Some n else None
     with _ -> None)
  | _ -> None

let op_type_of_params params =
  let op_type_str =
    match params with
    | `List [`String s] -> s
    | `List (`String s :: _) -> s
    | _ -> "standard"
  in
  match Transaction.op_type_of_string op_type_str with
  | Ok t -> t
  | Error _ -> Transaction.Standard

let circle_asset_wire_hint params =
  match params with
  | `List [_; (`Int _ | `Intlit _ | `String _ as value)] -> read_nonnegative_int value
  | `List [_; `Assoc fields] ->
    begin
      match List.assoc_opt "asset_wire_bytes" fields with
      | Some value -> read_nonnegative_int value
      | None ->
        begin
          match List.assoc_opt "encrypted_data_len" fields with
          | Some value -> read_nonnegative_int value
          | None -> None
        end
    end
  | _ -> None

let base_tx op_type =
  {
    Transaction.from = "";
    to_ = "";
    amount = Z.zero;
    nonce = 0;
    ou = Z.zero;
    timestamp = 0.;
    signature = "";
    public_key = None;
    message = None;
    encrypted_data = None;
    op_type;
  }

let base_fee op_type wire_hint tx =
  match op_type, wire_hint with
  | (Transaction.CircleAssetPut
    | Transaction.CircleAssetPutEncrypted
    | Transaction.CircleSealedSlotPut
    | Transaction.CircleBalanceCellPut
    | Transaction.CircleRegisterCellPut), Some wire_len ->
    Transaction.circle_asset_min_ou_from_wire_len wire_len
  | _ -> Transaction.ou_cost tx

let usage_fee base_fee usage_pct =
  if usage_pct < 50 then base_fee
  else if usage_pct < 80 then Z.mul base_fee (Z.of_int 2)
  else Z.mul base_fee (Z.of_int 5)

let recommended_fee ~stealth_class ~stealth_floor ~jitter ~usage_recommended =
  if stealth_class then
    let floor_with_usage =
      if Z.lt usage_recommended stealth_floor then stealth_floor
      else usage_recommended
    in
    Z.add floor_with_usage (Z.of_int (jitter ()))
  else usage_recommended

let recommended_fee_jitter ~get_int ~pick =
  let jitter_min = get_int "OCTRA_STEALTH_FEE_JITTER_MIN" 1 in
  let jitter_max = get_int "OCTRA_STEALTH_FEE_JITTER_MAX" 50_000 in
  let jitter_range = max 1 (jitter_max - jitter_min + 1) in
  jitter_min + pick jitter_range

let recommended_fee_response ~params ~is_heavy ~stealth_floor ~jitter ~staging_ou
    ~capacity ~usage_pct ~staging_size =
  let op_type = op_type_of_params params in
  let tx = base_tx op_type in
  let wire_hint = circle_asset_wire_hint params in
  let base_fee = base_fee op_type wire_hint tx in
  let min_fee = Staging.min_relay_fee tx in
  let stealth_class = is_heavy tx in
  let usage_recommended = usage_fee base_fee usage_pct in
  let recommended =
    recommended_fee
      ~stealth_class
      ~stealth_floor
      ~jitter
      ~usage_recommended
  in
  let fast = Z.mul recommended (Z.of_int 2) in
  `Assoc [
    "minimum", `String (Z.to_string min_fee);
    "base_fee", `String (Z.to_string base_fee);
    "recommended", `String (Z.to_string recommended);
    "fast", `String (Z.to_string fast);
    "staging_size", `Int staging_size;
    "staging_ou", `String (Z.to_string staging_ou);
    "epoch_capacity", `String (Z.to_string capacity);
    "usage_pct", `Int usage_pct;
    "stealth_class", `Bool stealth_class;
  ]