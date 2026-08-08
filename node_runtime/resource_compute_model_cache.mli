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

type sink = {
  status : string -> (cache_status, string) result Lwt.t;
  begin_cache : string -> (unit, string) result Lwt.t;
  put : cache_key:string -> key:string -> value:string -> (unit, string) result Lwt.t;
  seal :
    cache_key:string ->
    root:string ->
    entries:int ->
    bytes:int ->
    (unit, string) result Lwt.t;
  drop : string -> (unit, string) result Lwt.t;
}

type loaded = {
  snapshot : snapshot;
  graph : Octra_core.Resource_compute_graph.t;
  cache : Octra_core.Resource_compute_graph.cache_plan;
}

val asset_of_rpc_json :
  state_root:string ->
  Yojson.Safe.t ->
  (asset, string) result
val native_sink : sink
val store_source :
  Octra_core.Store_irmin.t ->
  ((snapshot * fetch_asset), string) result Lwt.t
val store_source_at :
  Octra_core.Store_irmin.t ->
  epoch_id:int64 ->
  state_root:string ->
  ((snapshot * fetch_asset), string) result Lwt.t
val load :
  limits:Octra_core.Resource_compute_graph.limits ->
  snapshot:snapshot ->
  root_circle_id:string ->
  fetch_asset:fetch_asset ->
  sink:sink ->
  (loaded, string) result Lwt.t