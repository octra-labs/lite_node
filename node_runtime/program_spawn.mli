(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

val non_key : string -> bool

val execute :
  t -> journal:Octra_vm.Program_journal.t -> ctx:Octra_vm.Contract_vm.exec_ctx ->
  depth:int -> limit:int -> params:Octra_vm.Contract_vm.v list ->
  Octra_core.Store_irmin.t -> deployer:string -> bytecode_raw:string -> nonce:int ->
  (Octra_vm.Contract_vm.spawn_result, string) result Lwt.t

val run :
  mode:Octra_core.Rule_graph.mode ->
  rules:(unit -> Octra_core.Rule_graph.mode * Octra_core.Rule_graph.mode) ->
  metered:bool -> ctx:Octra_vm.Contract_vm.exec_ctx ->
  make_ctx:(t -> Octra_vm.Contract_vm.exec_ctx) -> tx:Octra_core.Transaction.t ->
  call:Octra_vm.Call_plan.direct_exec ->
  exec:(ctx:Octra_vm.Contract_vm.exec_ctx -> limit:int -> params:Yojson.Safe.t list ->
    Octra_vm.Contract.exec_result Lwt.t) -> Octra_vm.Contract.exec_result Lwt.t