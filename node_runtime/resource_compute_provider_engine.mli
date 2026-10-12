(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type model = Resource_compute_model_cache.Owner.model = {
  snapshots : (int64 * string) list;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  cache_key : string;
  cache_root : string;
  entries : int;
  bytes : int64;
}

type native_result = {
  output_json : string;
  trace_root : string;
  steps : int64;
  operations : int64;
  effort_used : int;
}

type program = {
  circle_id : string;
  code_b64 : string;
  code_hash : string;
  runtime : string;
}

type deps = {
  program :
    epoch_id:int64 ->
    state_root:string ->
    circle_id:string ->
    (program, string) result Lwt.t;
  storage :
    epoch_id:int64 ->
    state_root:string ->
    circle_id:string ->
    ((string * string) list, string) result Lwt.t;
  load_model :
    cancel:string Lwt.t ->
    Resource_compute_provider_rpc.prepare ->
    (model, string) result Lwt.t;
  drop_model : string -> (unit, string) result Lwt.t;
  hold_model : model -> (Octra_core.Circle_wasm_native.lease, string) result;
  self_test : unit -> (Resource_compute_provider_rpc.self_test, string) result Lwt.t;
  execute :
    model:model ->
    program:program ->
    storage:(string * string) list ->
    Resource_compute_provider_rpc.execute ->
    (native_result, string) result Lwt.t;
}

type t

val graph_limits : int64 -> Octra_core.Resource_compute_graph.limits
val model_of_loaded : Resource_compute_provider_rpc.prepare -> Resource_compute_model_cache.loaded -> (model, string) result
val replay_deps : limits:Resource_compute_provider_config.limits -> deps

val measure :
  model:model -> program:program -> storage:(string * string) list ->
  call:Octra_consensus.Resource_compute_protocol.call ->
  Resource_compute_provider_rpc.execute -> (Octra_core.Compute_bill.measured, string) result Lwt.t

val replay :
  model:model -> program:program ->
  (Octra_consensus.Resource_compute_protocol.call * Resource_compute_provider_rpc.execute) list ->
  (Octra_core.Compute_bill.measured list, Octra_core.Circle_wasm_host.exec_error) result Lwt.t

val replay_loaded :
  limits:Resource_compute_provider_config.limits -> deps:deps ->
  program_at:(epoch_id:int64 -> state_root:string -> circle_id:string ->
    (program, Octra_core.Circle_wasm_host.exec_error) result Lwt.t) ->
  load_model:(cancel:string Lwt.t -> Resource_compute_provider_rpc.prepare ->
    (model, Octra_core.Circle_wasm_host.exec_error) result Lwt.t) ->
  (Octra_consensus.Resource_compute_protocol.call * Resource_compute_provider_rpc.execute) list ->
  (Octra_core.Compute_bill.measured list, Octra_core.Circle_wasm_host.exec_error) result Lwt.t

val program_checked :
  (int64 -> string option) -> (int64 -> string option) -> Octra_core.Store_irmin.t ->
  epoch_id:int64 -> state_root:string -> circle_id:string ->
  (program, Octra_core.Circle_wasm_host.exec_error) result Lwt.t
val model_checked :
  Resource_compute_provider_config.limits ->
  (int64 -> string option) -> (int64 -> string option) -> Octra_core.Store_irmin.t ->
  cancel:string Lwt.t -> Resource_compute_provider_rpc.prepare ->
  (model, Octra_core.Circle_wasm_host.exec_error) result Lwt.t

val stop : t -> unit Lwt.t

val native_accelerator :
  Resource_compute_provider_config.accelerator -> (unit, string) result
val native_deps :
  limits:Resource_compute_provider_config.limits ->
  store:Octra_core.Store_irmin.t ->
  state_root_at:(int64 -> string option) ->
  index_root_at:(int64 -> string option) ->
  deps
val native_self_test :
  ?accelerator:Resource_compute_provider_config.accelerator ->
  unit ->
  (Resource_compute_provider_rpc.self_test, string) result Lwt.t
val create :
  limits:Resource_compute_provider_config.limits ->
  deps:deps ->
  t
val self_test :
  t ->
  (Resource_compute_provider_rpc.self_test, string) result Lwt.t
val prepare :
  t ->
  Resource_compute_provider_rpc.prepare ->
  (Resource_compute_provider_rpc.prepared, string) result Lwt.t
val execute :
  t ->
  Resource_compute_provider_rpc.execute ->
  (Resource_compute_provider_rpc.executed, string) result Lwt.t