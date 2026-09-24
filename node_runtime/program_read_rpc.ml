(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result Lwt.t

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

let compile_active = ref false

let compile_at ~chain_id ~epoch handler params =
  let point_ops =
    Octra_core.Rule_graph.standard_at ~chain_id ~epoch = Octra_core.Rule_graph.Active
  in
  let compiler = Octra_core.Rule_graph.program_source_at ~chain_id ~epoch
    |> Octra_vm.Program_package.compiler_mode in
  handler ~compiler ~point_ops params

let immediate task =
  match Lwt.state task with
  | Lwt.Return value -> value
  | Lwt.Fail error -> raise error
  | Lwt.Sleep -> failwith "Program compiler returned a pending task"

let compile_rpc handler input =
  if !compile_active then
    Lwt.return_error
      (Octra_core.Rpc.err (-32005) "Program compiler busy" None)
  else begin
    compile_active := true;
    Lwt.catch
      (fun () ->
        Lwt.finalize
          (fun () ->
            Lwt_preemptive.detach
              (fun () -> immediate (handler input))
              ())
          (fun () ->
            compile_active := false;
            Lwt.return_unit))
      (function
        | Out_of_memory as error -> Lwt.fail error
        | Lwt.Canceled as error -> Lwt.fail error
        | Stack_overflow ->
          Lwt.return_error
            (Octra_core.Rpc.err
               (-32000)
               "Program compiler complexity limit exceeded"
               None)
        | _ ->
          Lwt.return_error
            (Octra_core.Rpc.err (-32000) "Program compiler failed" None))
  end

let dispatch adapters =
  let store_label_read = adapters.store_label_read in
  let store_chaindata_read = adapters.store_chaindata_read in
  let chaindata_read = adapters.chaindata_read in
  let no_ctx = adapters.no_ctx in
  Rpc_dispatch.program_routes Rpc_dispatch.{
    program_info = adapters.program_info;
    program_receipt =
      chaindata_read Octra_vm.Contract_rpc.receipt_params;
    program_call = adapters.program_call;
    program_compute_address =
      no_ctx Octra_vm.Contract_rpc.compute_address_params;
    program_list = adapters.program_list;
    program_storage =
      store_label_read Octra_vm.Contract_rpc.contract_storage_params;
    program_storage_dump =
      store_label_read Octra_vm.Contract_rpc.contract_storage_dump_params;
    program_abi = adapters.program_abi;
    program_verify =
      store_chaindata_read Octra_vm.Contract_rpc.verify_params;
    program_save_abi = adapters.program_save_abi;
    program_source =
      store_chaindata_read Octra_vm.Contract_rpc.source_params;
    program_bytecode =
      store_label_read Octra_vm.Contract_rpc.program_bytecode_params;
    program_compile_assembly =
      no_ctx (compile_rpc Octra_vm.Contract_rpc.compile_assembly_params);
    program_compile_aml =
      adapters.compile_read (fun ~compiler ~point_ops params ->
        compile_rpc
          (Octra_vm.Contract_rpc.compile_aml_params ~compiler ~point_ops) params);
    program_compile_aml_multi =
      adapters.compile_read (fun ~compiler ~point_ops params ->
        compile_rpc
          (fun value -> Octra_vm.Contract_rpc.compile_aml_multi_with
            ~compiler ~point_ops ~json:(Octra_core.Rpc.param_json value 0))
          params);
    program_tokens_by_address = adapters.program_tokens_by_address;
  }