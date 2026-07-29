(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

type 'handler dispatch_adapters = {
  ledger_params_read :
    (Octra_core.Ledger.t -> params:Yojson.Safe.t -> rpc_result Lwt.t) ->
    'handler;
  ledger_params_no_ctx_read :
    (params:Yojson.Safe.t -> rpc_result Lwt.t) ->
    'handler;
  ledger_address_lwt_read :
    (Octra_core.Ledger.t -> addr:string -> rpc_result Lwt.t) ->
    'handler;
  store_address_lwt_read :
    (Octra_core.Store_irmin.t -> addr:string -> rpc_result Lwt.t) ->
    'handler;
  store_params_lwt_read :
    (Octra_core.Store_irmin.t -> params:Yojson.Safe.t -> rpc_result Lwt.t) ->
    'handler;
  store_ledger_address_lwt_read :
    (Octra_core.Store_irmin.t ->
     Octra_core.Ledger.t ->
     params:Yojson.Safe.t ->
     addr:string ->
     rpc_result Lwt.t) ->
    'handler;
  account_lwt_read :
    (addr:string -> account:Octra_core.Ledger.account -> rpc_result Lwt.t) ->
    'handler;
  pvac_migration_status : 'handler;
  account : 'handler;
  supply : 'handler;
  total_transactions : 'handler;
}

val balance :
  Octra_core.Ledger.t ->
  params:Yojson.Safe.t ->
  rpc_result Lwt.t

val nonce :
  Octra_core.Ledger.t ->
  params:Yojson.Safe.t ->
  rpc_result Lwt.t

val public_key :
  Octra_core.Ledger.t ->
  params:Yojson.Safe.t ->
  rpc_result Lwt.t

val validate_address :
  params:Yojson.Safe.t ->
  rpc_result Lwt.t

val supply :
  Octra_core.Store_irmin.t ->
  Octra_core.Ledger.t ->
  encrypted:Z.t ->
  rpc_result Lwt.t

val total_transactions :
  confirmed:int ->
  rpc_result Lwt.t

val pvac_pubkey :
  Octra_core.Store_irmin.t ->
  addr:string ->
  rpc_result Lwt.t

val pvac_status :
  Octra_core.Store_irmin.t ->
  addr:string ->
  rpc_result Lwt.t

val pvac_migration_status :
  Octra_core.Pvac_migration_entitlement.t ->
  epoch:int ->
  addr:string ->
  account:Octra_core.Ledger.account ->
  rpc_result Lwt.t

val encrypted_cipher :
  addr:string ->
  account:Octra_core.Ledger.account ->
  rpc_result Lwt.t

val encrypted_balance :
  Octra_core.Store_irmin.t ->
  Octra_core.Ledger.t ->
  params:Yojson.Safe.t ->
  addr:string ->
  rpc_result Lwt.t

val view_pubkey :
  Octra_core.Ledger.t ->
  addr:string ->
  rpc_result Lwt.t

val stealth_outputs :
  Octra_core.Store_irmin.t ->
  params:Yojson.Safe.t ->
  rpc_result Lwt.t

val account :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  profile_enabled:bool ->
  started_at:float ->
  addr:string ->
  account:Octra_core.Ledger.account ->
  rpc_result Lwt.t

val public_dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list

val pvac_dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list

val dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list