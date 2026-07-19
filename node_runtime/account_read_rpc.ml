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
  account_chaindata_lwt_read :
    (Octra_core.Store_chaindata.t ->
     addr:string ->
     account:Ledger.account ->
     rpc_result Lwt.t) ->
    'handler;
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

let public_output_cipher = function
  | name, `String cipher when name = "delta_cipher_stored" ->
    name, `String (public_cipher cipher)
  | field -> field

let public_stealth_output = function
  | `Assoc fields -> `Assoc (List.map public_output_cipher fields)
  | output -> output

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

let supply ledger ~encrypted =
  ok
    (Account_rpc.supply
       ~true_total:(Ledger.get_total_supply ledger)
       ~encrypted
       ~max_supply:(Ledger.get_max_supply ()))

let total_transactions ~confirmed =
  ok
    (Account_rpc.total_transactions
       ~confirmed
       ~staging:(List.length (Staging.all ())))

let pvac_pubkey store ~addr =
  let open Lwt.Syntax in
  let* pk_opt = Octra_core.Store_irmin.get_pvac_pubkey store addr in
  ok (Rpc_view.pvac_pubkey ~addr pk_opt)

let pvac_status store ~addr =
  let open Lwt.Syntax in
  let* pk_opt = Octra_core.Store_irmin.get_pvac_pubkey store addr in
  ok (Rpc_view.pvac_status ~addr (Octra_core.Pvac_registry.status_of_blob pk_opt))

let pvac_migration_status chaindata ~addr ~account =
  let cipher = account_cipher account in
  let status = Octra_core.Pvac_migration.status_of_cipher cipher in
  let legacy_public_replay =
    if status.needs_legacy_public_replay then
      Some (Octra_core.Store_chaindata.pvac_legacy_public_replay_by_addr chaindata addr ~max_txs:50_000)
    else
      None
  in
  ok (Rpc_view.pvac_migration_status ~addr status legacy_public_replay)

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
  let from_epoch = stealth_from_epoch params in
  let open Lwt.Syntax in
  let* outputs = Octra_core.Store_irmin.get_stealth_outputs_since store from_epoch in
  ok (Rpc_view.stealth_outputs ~from_epoch ~outputs:(List.map public_stealth_output outputs))

let account chaindata ~params ~profile_enabled ~started_at ~addr ~account =
  let t0 = started_at in
  let t1 = Unix.gettimeofday () in
  let limit =
    match Rpc.param_int params 1 with
    | Some l -> min l 50
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
    "octra_pvacMigrationStatus", adapters.account_chaindata_lwt_read pvac_migration_status;
    "octra_encryptedCipher", adapters.account_lwt_read encrypted_cipher;
    "octra_encryptedBalance", adapters.store_ledger_address_lwt_read encrypted_balance;
    "octra_viewPubkey", adapters.ledger_address_lwt_read view_pubkey;
    "octra_stealthOutputs", adapters.store_params_lwt_read stealth_outputs;
  ]

let dispatch adapters =
  public_dispatch adapters @ pvac_dispatch adapters