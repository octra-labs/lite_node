(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type snapshot = {
  epoch_id : int64;
  state_root : string;
}

type asset = {
  state_root : string;
  circle_id : string;
  path : string;
  content_type : string;
  encoding : string;
  browser_mode : string;
  resource_mode : string;
  blob_hash : string;
  size_bytes : int;
  body : string;
}

type fetch_asset =
  snapshot ->
  max_bytes:int ->
  circle_id:string ->
  path:string ->
  (asset, string) result Lwt.t

type cache_status =
  | Missing
  | Ready of {
      root : string;
      entries : int;
      bytes : int;
    }

type writer = {
  put : key:string -> value:string -> (unit, string) result Lwt.t;
  seal :
    root:string ->
    entries:int ->
    bytes:int ->
    (unit, string) result Lwt.t;
  close : unit -> unit;
}

type sink = {
  status : string -> (cache_status, string) result Lwt.t;
  begin_cache : string -> (writer, string) result Lwt.t;
  drop : string -> (unit, string) result Lwt.t;
}

type loaded = {
  snapshot : snapshot;
  graph : Octra_core.Resource_compute_graph.t;
  cache : Octra_core.Resource_compute_graph.cache_plan;
}

module Owner : sig
  type model = {
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

  type token = { id : string; generation : int64; key : string }
  type plan = {
    token : token;
    request : Resource_compute_provider_rpc.prepare;
    victim : model option;
    shared : bool;
  }
  type preparation = Ready of model | Build of plan
  type state = {
    models : model list;
    leases : token list;
    loading : plan option;
    next : int64;
    limit : int;
    closed : bool;
    seen : string list;
  }

  type _ message =
    | Reserve : Resource_compute_provider_rpc.prepare -> (preparation, string) result message
    | Acquire : Resource_compute_provider_rpc.execute -> ((token * model), string) result message
    | Dropped : token -> bool message
    | Remember : token * model -> (unit, string) result message
    | Check : token -> (unit, string) result message
    | Forget : token -> unit message
    | Lost : model -> unit message
    | Finish : token -> unit message
    | Release : token * bool -> unit message
    | Close : unit message

  val create : int -> state
  val prepared : model -> Resource_compute_provider_rpc.prepare -> bool
  val executable : model -> Resource_compute_provider_rpc.execute -> bool
  val delta : state -> 'reply message -> state * 'reply
end

val asset_of_rpc_json :
  max_bytes:int ->
  state_root:string ->
  Yojson.Safe.t ->
  (asset, string) result
val native_sink : sink
val native_drop : string -> (bool, string) result Lwt.t
val native_hold : Owner.model -> (Octra_core.Circle_wasm_native.lease, string) result
val store_source_of_snapshot : ?state_root:string -> Octra_core.Store_irmin.read_snapshot -> snapshot * fetch_asset

val store_source :
  Octra_core.Store_irmin.t ->
  ((snapshot * fetch_asset), string) result Lwt.t
val store_source_at :
  Octra_core.Store_irmin.t ->
  epoch_id:int64 ->
  state_root:string ->
  index_root:string ->
  ((snapshot * fetch_asset), string) result Lwt.t
val load :
  cancel:string Lwt.t ->
  limits:Octra_core.Resource_compute_graph.limits ->
  snapshot:snapshot ->
  root_circle_id:string ->
  graph_root:string ->
  model_root:string ->
  fetch_asset:fetch_asset ->
  sink:sink ->
  (loaded, string) result Lwt.t

val load_checked :
  cancel:string Lwt.t -> limits:Octra_core.Resource_compute_graph.limits ->
  snapshot:snapshot -> root_circle_id:string -> graph_root:string -> model_root:string ->
  fetch_asset:fetch_asset -> sink:sink ->
  (loaded, Octra_core.Circle_wasm_host.exec_error) result Lwt.t