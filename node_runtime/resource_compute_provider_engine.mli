(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type model = {
  mutable snapshots : (int64 * string) list;
  circle_id : string;
  graph_root : string;
  model_root : string;
  program_root : string;
  executor_root : string;
  cache_key : string;
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
    Resource_compute_provider_rpc.prepare ->
    (model, string) result Lwt.t;
  drop_model : model -> (unit, string) result Lwt.t;
  self_test : unit -> (Resource_compute_provider_rpc.self_test, string) result Lwt.t;
  execute :
    model:model ->
    program:program ->
    storage:(string * string) list ->
    Resource_compute_provider_rpc.execute ->
    (native_result, string) result Lwt.t;
}

type t

val native_deps :
  limits:Resource_compute_provider_config.limits ->
  store:Octra_core.Store_irmin.t ->
  state_root_at:(int64 -> string option) ->
  deps
val native_self_test :
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