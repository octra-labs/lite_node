(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = {
  max_graph_bytes : int;
  max_shards : int;
  max_segments : int;
  max_chunks : int;
  max_tensors : int;
  max_segment_bytes : int;
  max_chunk_bytes : int;
  max_shard_bytes : int;
  max_model_bytes : int64;
  max_cache_entries : int;
  max_cache_bytes : int64;
}

type segment = {
  index : int;
  offset : int;
  bytes : int;
  path : string;
  sha256 : string;
}

type chunk = {
  index : int;
  bytes : int;
  sha256 : string;
  source : string;
  stream_offset : int64;
  tensor : string;
  tensor_chunk : int;
  shard : int;
  shard_offset : int;
}

type shard = {
  index : int;
  circle_id : string;
  bytes : int;
  sha256 : string;
  stream_offset : int64;
  chunk_indices : int list;
  segments : segment list;
}

type tensor = {
  name : string;
  source_name : string;
  scale_bits : int64;
  shape : int list;
  chunks : int list;
}

type model = {
  id : string;
  root_domain : string;
  root : string;
  raw_bytes : int64;
  chunks : int;
  tensors : tensor list;
}

type t = {
  format : string;
  root_circle_id : string;
  graph_root : string;
  layout_root : string;
  model : model;
  chunks : chunk array;
  shards : shard list;
}

type cache_source =
  | Inline of string
  | Model_chunk of chunk

type cache_entry = {
  key : string;
  bytes : int;
  sha256 : string;
  source : cache_source;
}

type cache_plan = {
  key : string;
  root : string;
  bytes : int64;
  entries : cache_entry list;
}

val default_limits : limits
val stable_json : Yojson.Safe.t -> string
val parse :
  limits:limits ->
  root_circle_id:string ->
  string ->
  (t, string) result
val shard_manifest_json : t -> shard -> Yojson.Safe.t
val cache_plan :
  limits:limits ->
  state_root:string ->
  t ->
  (cache_plan, string) result