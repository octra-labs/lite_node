(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ledger = Octra_core.Ledger
module Rpc = Octra_core.Rpc
module Staging = Octra_core.Tx_staging

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

type 'handler dispatch_adapters = {
  ledger_params_read :
    (Ledger.t -> params:Yojson.Safe.t -> rpc_result Lwt.t) ->
    'handler;
  ledger_params_no_ctx_read :
    (params:Yojson.Safe.t -> rpc_result Lwt.t) ->
    'handler;
  ledger_address_lwt_read :
    (Ledger.t -> addr:string -> rpc_result Lwt.t) ->
    'handler;
  store_address_lwt_read :
    (Octra_core.Store_irmin.t -> addr:string -> rpc_result Lwt.t) ->
    'handler;
  store_params_lwt_read :
    (Octra_core.Store_irmin.t -> params:Yojson.Safe.t -> rpc_result Lwt.t) ->
    'handler;
  store_ledger_address_lwt_read :
    (Octra_core.Store_irmin.t ->
     Ledger.t ->
     params:Yojson.Safe.t ->
     addr:string ->
     rpc_result Lwt.t) ->
    'handler;
  account_lwt_read :
    (addr:string -> account:Ledger.account -> rpc_result Lwt.t) ->
    'handler;
  pvac_migration_status : 'handler;
  account : 'handler;
  supply : 'handler;
  total_transactions : 'handler;
}

let ok value =
  Lwt.return (Ok value)

let account_cipher account =
  Option.value ~default:"0" account.Ledger.encrypted_balance

let public_cipher cipher =
  Octra_core.Crypto.FheBalance.public_cipher cipher

let public_stealth_record output =
  `Assoc [
    "id", `Int output.Octra_core.Ledger_types.id;
    "stealth_tag", `String output.stealth_tag;
    "eph_pub", `String output.eph_pub;
    "enc_amount", `String output.enc_amount;
    "epoch_id", `Int output.epoch_id;
    "tx_hash", `String output.tx_hash;
    "claimed", `Int output.claimed;
    "claim_pub", `String output.claim_pub;
    "delta_cipher_stored", `String (public_cipher output.delta_cipher_stored);
    "amount_hash", `String output.amount_hash;
    "amount_commitment", `String output.amount_commitment;
  ]

let balance ledger ~params =
  Lwt.return
    (Account_rpc.balance
       params
       ~find_account:(Ledger.find_opt ledger)
       ~pending_nonce:Staging.pending_nonce)

let nonce ledger ~params =
  Lwt.return (Account_rpc.nonce params ~find_account:(Ledger.find_opt ledger))

let public_key ledger ~params =
  Lwt.return
    (Account_rpc.public_key params ~find_account:(Ledger.find_opt ledger))

let validate_address ~params =
  Lwt.return (Account_rpc.validate_address params)

let supply_meta = function
  | None -> Ok Z.zero
  | Some raw ->
    try
      let value = Z.of_string raw in
      if Z.sign value < 0 then Error ()
      else Ok value
    with _ ->
      Error ()

let supply store ledger ~encrypted =
  let open Lwt.Syntax in
  let* emission =
    Octra_core.Store_irmin.get_meta store "emission_remaining"
  in
  let* retired =
    Octra_core.Store_irmin.get_meta store
      Octra_core.Emission_schedule.retired_key
  in
  match supply_meta emission, supply_meta retired with
  | Ok emission_remaining, Ok retired_supply ->
    ok
      (Account_rpc.supply
         ~true_total:(Ledger.get_total_supply ledger)
         ~encrypted
         ~max_supply:(Ledger.get_max_supply ())
         ~emission_remaining
         ~retired_supply)
  | _ ->
    Lwt.return_error Rpc.supply_violation

let total_transactions ~confirmed =
  ok
    (Account_rpc.total_transactions
       ~confirmed
       ~staging:(Staging.staging_size ()))

let pvac_pubkey store ~addr =
  let open Lwt.Syntax in
  let* pk_opt = Octra_core.Store_irmin.get_pvac_pubkey store addr in
  ok (Rpc_view.pvac_pubkey ~addr pk_opt)

let pvac_status store ~addr =
  let open Lwt.Syntax in
  let* pk_opt = Octra_core.Store_irmin.get_pvac_pubkey store addr in
  let* canonical_binding = Octra_core.Store_irmin.pvac_is_bound store addr in
  ok
    (Rpc_view.pvac_status
       ~addr
       (Octra_core.Pvac_registry.status_of_blob
          ~canonical_binding
          pk_opt))

let pvac_migration_status
    status_actor
    store
    entitlements
    ~epoch
    ~owner_migration_mode
    ~addr
    ~account =
  let open Lwt.Syntax in
  let cipher = account_cipher account in
  let* key_hash = Octra_core.Store_irmin.get_pvac_hash store addr in
  let* status =
    Pvac_status_actor.query
      status_actor
      ~addr
      ~key_hash
      ~cipher
  in
  match status with
  | Error Pvac_status_actor.Busy
  | Error Pvac_status_actor.Stopped ->
    Lwt.return_error
      (Rpc.err (-32005) "pvac migration status busy" None)
  | Error Pvac_status_actor.Read_failed ->
    Lwt.return_error
      (Rpc.err (-32005) "pvac migration status unavailable" None)
  | Ok status ->
    ok
      (Rpc_view.pvac_migration_status
         ~addr
         ~cipher
         ~epoch
         ~owner_migration_mode
         status
         entitlements)

let encrypted_cipher ~addr ~account =
  ok (Rpc_view.encrypted_cipher ~addr ~cipher:(public_cipher (account_cipher account)))

let encrypted_balance store ledger ~params ~addr =
  match Tx_view.encrypted_balance_auth params ~addr with
  | Error (Tx_view.Rpc_malformed e) ->
    Lwt.return (Error (Rpc.malformed_tx e))
  | Error (Tx_view.Rpc_auth_error e) ->
    Lwt.return (Error (Rpc.err (-32000) e None))
  | Ok () ->
    let open Lwt.Syntax in
    let* pk_opt = Octra_core.Store_irmin.get_pvac_pubkey store addr in
    ok
      (Rpc_view.encrypted_balance
         ~addr
         ~cipher:(Rpc_view.account_encrypted_balance_or_zero (Ledger.find_opt ledger addr))
         ~has_pvac_pubkey:(pk_opt <> None))

let view_pubkey ledger ~addr =
  match Ledger.get_public_key ledger addr with
  | None ->
    ok
      (Rpc_view.account_view_pubkey
         ~addr
         ~view_pubkey:None
         ~reason:(Some "no public key registered"))
  | Some ed25519_pub_b64 ->
    match Octra_core.Crypto.StealthAddress.ed25519_pub_to_view_pubkey ed25519_pub_b64 with
    | None ->
      ok
        (Rpc_view.account_view_pubkey
           ~addr
           ~view_pubkey:None
           ~reason:(Some "conversion failed"))
    | Some view_pub_b64 ->
      ok
        (Rpc_view.account_view_pubkey
           ~addr
           ~view_pubkey:(Some view_pub_b64)
           ~reason:None)

let stealth_from_epoch = function
  | `List (`Int n :: _) ->
    n
  | `List (`String s :: _) ->
    begin
      try int_of_string s with _ -> 0
    end
  | _ ->
    0

let stealth_outputs store ~params =
  let from_epoch = max 0 (stealth_from_epoch params) in
  let open Lwt.Syntax in
  let* page =
    Octra_core.Store_irmin.get_stealth_outputs_page
      store
      ~from_epoch
      ~before_id:None
      ~limit:256
  in
  ok
    (Rpc_view.stealth_outputs_page
       ~from_epoch
       ~before_id:None
       ~outputs:(List.map public_stealth_record page.outputs)
       ~next_before_id:page.next_before_id
       ~has_more:page.has_more
       ~scanned:page.scanned)

let int64_value = function
  | `Int value -> Some (Int64.of_int value)
  | `Intlit value | `String value ->
    begin
      try Some (Int64.of_string value) with _ -> None
    end
  | _ -> None

let stealth_page_params params =
  let from_epoch = max 0 (stealth_from_epoch params) in
  let before_id =
    match Rpc.param_json params 1 with
    | None | Some `Null -> Ok None
    | Some value ->
      begin
        match int64_value value with
        | Some id when Int64.compare id 0L >= 0 -> Ok (Some id)
        | _ -> Error "before_id must be a nonnegative integer or null"
      end
  in
  let limit =
    match Rpc.param_json params 2 with
    | None -> Ok 256
    | Some (`Int value) when value >= 1 && value <= 256 -> Ok value
    | _ -> Error "limit must be between 1 and 256"
  in
  match before_id, limit with
  | Ok before_id, Ok limit -> Ok (from_epoch, before_id, limit)
  | Error reason, _ | _, Error reason -> Error reason

let stealth_outputs_page store ~params =
  match stealth_page_params params with
  | Error reason ->
    Lwt.return (Error (Rpc.invalid_params reason))
  | Ok (from_epoch, before_id, limit) ->
    let open Lwt.Syntax in
    let* page =
      Octra_core.Store_irmin.get_stealth_outputs_page
        store
        ~from_epoch
        ~before_id
        ~limit
    in
    ok
      (Rpc_view.stealth_outputs_page
         ~from_epoch
         ~before_id
         ~outputs:(List.map public_stealth_record page.outputs)
         ~next_before_id:page.next_before_id
         ~has_more:page.has_more
         ~scanned:page.scanned)

let stealth_ids params =
  let parse_id = function
    | `Int id when id >= 0 -> Some id
    | `Intlit value | `String value ->
      begin
        try
          let id = int_of_string value in
          if id >= 0 then Some id else None
        with _ ->
          None
      end
    | _ -> None
  in
  match Rpc.param_json params 0 with
  | Some (`List values) when values <> [] && List.length values <= 64 ->
    let ids = List.filter_map parse_id values in
    if List.length ids = List.length values then Ok ids
    else Error "ids must contain only nonnegative integers"
  | Some (`List []) -> Error "ids must not be empty"
  | Some (`List _) -> Error "ids exceeds limit 64"
  | _ -> Error "ids must be an array"

let stealth_outputs_by_id store ~params =
  match stealth_ids params with
  | Error reason ->
    Lwt.return (Error (Rpc.invalid_params reason))
  | Ok ids ->
    let open Lwt.Syntax in
    let* outputs = Octra_core.Store_irmin.get_stealth_outputs_by_ids store ids in
    ok
      (Rpc_view.stealth_outputs_by_id
         ~requested:(List.length ids)
         ~outputs:(List.map public_stealth_record outputs))

let account chaindata ~params ~profile_enabled ~started_at ~addr ~account =
  let t0 = started_at in
  let t1 = Unix.gettimeofday () in
  let limit =
    match Rpc.param_int params 1 with
    | Some l -> max 0 (min l 50)
    | None -> 10
  in
  let open Lwt.Syntax in
  let* recent_status =
    Lwt.return
      (Octra_core.Store_chaindata.txs_by_addr_rows_status
         ~profile_tag:"octra_account"
         chaindata
         addr
         ~limit
         ~offset:0)
  in
  let t2 = Unix.gettimeofday () in
  let rejected_txs =
    Octra_core.Store_chaindata.get_rejected_txs_by_addr chaindata addr ~limit in
  let t3 = Unix.gettimeofday () in
  let account_view =
    History.account_view
      ~addr
      ~balance:account.Ledger.balance
      ~nonce:account.Ledger.nonce
      ~has_public_key:(Option.is_some account.Ledger.public_key)
      ~encrypted_balance:account.Ledger.encrypted_balance
      ~tx_count:recent_status.total
      ~recent_rows:recent_status.rows
      ~rejected_txs
  in
  let t4 = Unix.gettimeofday () in
  begin
  if profile_enabled then
    let profile =
      History.account_profile_from_view
        ~tag:"octra_account"
        ~addr
        ~view:account_view
        ~t0
        ~t1
        ~t2
        ~t3
        ~t4
    in
    Log.info "rpc" "%s" (History.account_profile_log_message profile)
  end;
  ok account_view.History.account_json

let public_dispatch adapters =
  [
    "octra_balance", adapters.ledger_params_read balance;
    "octra_account", adapters.account;
    "octra_nonce", adapters.ledger_params_read nonce;
    "octra_publicKey", adapters.ledger_params_read public_key;
    "octra_validateAddress", adapters.ledger_params_no_ctx_read validate_address;
    "octra_supply", adapters.supply;
    "octra_totalTransactions", adapters.total_transactions;
  ]

let pvac_dispatch adapters =
  [
    "octra_pvacPubkey", adapters.store_address_lwt_read pvac_pubkey;
    "octra_pvacStatus", adapters.store_address_lwt_read pvac_status;
    "octra_pvacMigrationStatus", adapters.pvac_migration_status;
    "octra_encryptedCipher", adapters.account_lwt_read encrypted_cipher;
    "octra_encryptedBalance", adapters.store_ledger_address_lwt_read encrypted_balance;
    "octra_viewPubkey", adapters.ledger_address_lwt_read view_pubkey;
    "octra_stealthOutputs", adapters.store_params_lwt_read stealth_outputs;
    "octra_stealthOutputsPage", adapters.store_params_lwt_read stealth_outputs_page;
    "octra_stealthOutputsById", adapters.store_params_lwt_read stealth_outputs_by_id;
  ]

let dispatch adapters =
  public_dispatch adapters @ pvac_dispatch adapters