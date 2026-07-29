(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val with_address :
  Yojson.Safe.t ->
  (string -> (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t) ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val with_address_string :
  Yojson.Safe.t ->
  int ->
  string ->
  (string -> string -> (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t) ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val with_account :
  find_account:('ctx -> string -> 'account option) ->
  Yojson.Safe.t ->
  'ctx ->
  (string -> 'account -> (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t) ->
  (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val store_read :
  store:('ctx -> 'store) ->
  ('store -> 'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val ledger_params_read :
  ledger:('ctx -> 'ledger) ->
  ('ledger -> params:'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val ledger_address_lwt_read :
  ledger:('ctx -> 'ledger) ->
  with_address:('params -> (string -> 'result) -> 'result) ->
  ('ledger -> addr:string -> 'result) ->
  'params ->
  'ctx ->
  'result

val ledger_params_no_ctx_read :
  (params:'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val store_address_lwt_read :
  store:('ctx -> 'store) ->
  with_address:('params -> (string -> 'result) -> 'result) ->
  ('store -> addr:string -> 'result) ->
  'params ->
  'ctx ->
  'result

val store_params_lwt_read :
  store:('ctx -> 'store) ->
  ('store -> params:'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val store_ledger_address_lwt_read :
  store:('ctx -> 'store) ->
  ledger:('ctx -> 'ledger) ->
  with_address:('params -> (string -> 'result) -> 'result) ->
  ('store -> 'ledger -> params:'params -> addr:string -> 'result) ->
  'params ->
  'ctx ->
  'result

val chaindata_params_read :
  chaindata:('ctx -> 'chaindata) ->
  ('chaindata -> params:'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val chaindata_address_read :
  chaindata:('ctx -> 'chaindata) ->
  with_address:('params -> (string -> 'result) -> 'result) ->
  ('chaindata -> params:'params -> addr:string -> 'result) ->
  'params ->
  'ctx ->
  'result

val ledger_chaindata_params_read :
  ledger:('ctx -> 'ledger) ->
  chaindata:('ctx -> 'chaindata) ->
  ('ledger -> 'chaindata -> params:'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val account_lwt_read :
  with_account:('params -> 'ctx -> (string -> 'account -> 'result) -> 'result) ->
  (addr:string -> account:'account -> 'result) ->
  'params ->
  'ctx ->
  'result

val store_label_read :
  store:('ctx -> 'store) ->
  (store:'store -> 'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val chaindata_read :
  chaindata:('ctx -> 'chaindata) ->
  (chaindata:'chaindata -> 'params -> 'result) ->
  'params ->
  'ctx ->
  'result

val epoch_read :
  store:('ctx -> 'store) ->
  current_epoch:('ctx -> int ref) ->
  ('store -> 'params -> current_epoch:int -> 'result) ->
  'params ->
  'ctx ->
  'result

val no_ctx :
  ('params -> 'result) ->
  'params ->
  'ctx ->
  'result

val no_params :
  (unit -> 'result) ->
  'params ->
  'ctx ->
  'result

val json0_read :
  param_json:('params -> int -> 'json option) ->
  (json:'json option -> 'result) ->
  'params ->
  'ctx ->
  'result