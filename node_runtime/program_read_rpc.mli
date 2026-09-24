(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

val compile_at :
  chain_id:string -> epoch:int ->
  (compiler:Octra_vm.Program_package.compiler ->
   point_ops:bool -> Yojson.Safe.t -> rpc_result) ->
  Yojson.Safe.t -> rpc_result

type 'handler dispatch_adapters = {
  store_label_read :
    (store:Octra_core.Store_irmin.t -> Yojson.Safe.t -> rpc_result) ->
    'handler;
  store_chaindata_read :
    (store:Octra_core.Store_irmin.t ->
     chaindata:Octra_core.Store_chaindata.t ->
     Yojson.Safe.t ->
     rpc_result) ->
    'handler;
  chaindata_read :
    (chaindata:Octra_core.Store_chaindata.t -> Yojson.Safe.t -> rpc_result) ->
    'handler;
  no_ctx :
    (Yojson.Safe.t -> rpc_result) ->
    'handler;
  json0_read :
    (json:Yojson.Safe.t option -> rpc_result) ->
    'handler;
  compile_read :
    (compiler:Octra_vm.Program_package.compiler ->
     point_ops:bool -> Yojson.Safe.t -> rpc_result) ->
    'handler;
  program_info : 'handler;
  program_list : 'handler;
  program_call : 'handler;
  program_abi : 'handler;
  program_save_abi : 'handler;
  program_tokens_by_address : 'handler;
}

val dispatch :
  'handler dispatch_adapters ->
  'handler Rpc_dispatch.route list