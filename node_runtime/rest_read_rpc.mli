(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

type 'handler dispatch_adapters = {
  ledger_chaindata_params_read :
    (Octra_core.Ledger.t ->
     Octra_core.Store_chaindata.t ->
     params:Yojson.Safe.t ->
     rpc_result) ->
    'handler;
  chain_chaindata_params_read :
    (chain_id:string -> Octra_core.Store_chaindata.t -> params:Yojson.Safe.t -> rpc_result) ->
    'handler;
  no_params : (unit -> rpc_result) -> 'handler;
  epoch_current : 'handler;
  epoch_list : 'handler;
  recommended_fee : 'handler;
}

val search :
  Octra_core.Ledger.t ->
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  rpc_result

val epoch_current :
  tree_ref:Octra_core.Tree.t ref ->
  rpc_result

val epoch_reward_view :
  chain_id:string ->
  epoch_id:int ->
  Yojson.Safe.t ->
  Yojson.Safe.t

val epoch_get :
  chain_id:string ->
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  rpc_result

val epoch_list :
  tree_ref:Octra_core.Tree.t ref ->
  params:Yojson.Safe.t ->
  rpc_result

val epoch_summaries :
  chain_id:string ->
  Octra_core.Store_chaindata.t ->
  params:Yojson.Safe.t ->
  rpc_result

val staging_view :
  unit ->
  rpc_result

val staging_stats :
  unit ->
  rpc_result

val staging_estimate_ou :
  unit ->
  rpc_result

val stealth_floor :
  unit ->
  Z.t

val recommended_fee :
  params:Yojson.Safe.t ->
  stealth_floor:Z.t ->
  rpc_result

val recommended_fee_from_env :
  params:Yojson.Safe.t ->
  rpc_result

val dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list