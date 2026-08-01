(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

type 'handler dispatch_adapters = {
  transaction : 'handler;
  chaindata_params_read :
    (Octra_core.Store_chaindata.t -> params:Yojson.Safe.t -> rpc_result) ->
    'handler;
  chaindata_address_read :
    (Octra_core.Store_chaindata.t -> params:Yojson.Safe.t -> addr:string -> rpc_result) ->
    'handler;
  transactions_by_epoch : 'handler;
}

val bounded_heal_limit : int -> int

val lookup_confirmed_tx_with_heal :
  Octra_core.Store_chaindata.t ->
  string ->
  (int * string) option

val confirmed_tx_epoch_with_heal :
  Octra_core.Store_chaindata.t ->
  string ->
  int option

val transaction :
  find_drop:(string -> Octra_core.Tx_drop.row option) ->
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  rpc_result

val transactions :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  rpc_result

val recent_transactions :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  rpc_result

val transactions_by_address :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  addr:string ->
  rpc_result

val token_transfers_by_address :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  addr:string ->
  rpc_result

val rejected_by_address :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  addr:string ->
  rpc_result

val transactions_by_epoch :
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  current_epoch_id:int ->
  rpc_result

val dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list