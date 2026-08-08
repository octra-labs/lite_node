(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Graph = Octra_core.Resource_compute_graph

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
  graph : Graph.t;
  cache : Graph.cache_plan;
}

let ( let*? ) value next =
  match value with
  | Ok value -> next value
  | Error _ as error -> error

let result_unit = function
  | Ok _ -> Ok ()
  | Error _ as error -> error

let native_result =
  Result.map_error Octra_core.Circle_wasm_host.error_message

let exact_fields names fields =
  let actual = List.map fst fields |> List.sort String.compare in
  let expected = List.sort String.compare names in
  List.length actual = List.length expected && actual = expected

let field fields name =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error ("circle asset field missing: " ^ name)

let string_field fields name =
  let*? value = field fields name in
  match value with
  | `String value -> Ok value
  | _ -> Error ("circle asset string required: " ^ name)

let int_field fields name =
  let*? value = field fields name in
  match value with
  | `Int value when value >= 0 -> Ok value
  | `Intlit value ->
    begin
      match int_of_string_opt value with
      | Some value when value >= 0 -> Ok value
      | _ -> Error ("circle asset integer required: " ^ name)
    end
  | _ -> Error ("circle asset integer required: " ^ name)

let asset_of_rpc_json ~state_root = function
  | `Assoc fields
    when exact_fields
      [
        "blob_hash";
        "body_b64";
        "browser_mode";
        "canonical_path";
        "circle_id";
        "content_type";
        "encoding";
        "path_key";
        "resource_key";
        "resource_mode";
        "size_bytes";
      ]
      fields ->
    let*? circle_id = string_field fields "circle_id" in
    let*? path = string_field fields "canonical_path" in
    let*? content_type = string_field fields "content_type" in
    let*? encoding = string_field fields "encoding" in
    let*? browser_mode = string_field fields "browser_mode" in
    let*? resource_mode = string_field fields "resource_mode" in
    let*? blob_hash = string_field fields "blob_hash" in
    let*? size_raw = string_field fields "size_bytes" in
    let*? body_b64 = string_field fields "body_b64" in
    let*? size_bytes =
      match int_of_string_opt size_raw with
      | Some value when value >= 0 -> Ok value
      | _ -> Error "circle asset size refused" in
    let*? body =
      match Base64.decode body_b64 with
      | Ok value -> Ok value
      | Error _ -> Error "circle asset base64 refused" in
    let actual_hash = Digestif.SHA256.(to_hex (digest_string body)) in
    if String.length body <> size_bytes then
      Error "circle asset size differs"
    else if actual_hash <> blob_hash then
      Error "circle asset body hash differs"
    else
      Ok {
        state_root;
        circle_id;
        path;
        content_type;
        encoding;
        browser_mode;
        resource_mode;
        blob_hash;
        size_bytes;
        body;
      }
  | _ -> Error "circle asset response differs"

let cache_status_of_json cache_key = function
  | `Assoc fields
    when exact_fields ["cache_key"; "status"] fields ->
    let*? actual_key = string_field fields "cache_key" in
    let*? status = string_field fields "status" in
    if actual_key = cache_key && status = "missing" then Ok Missing
    else Error "resource compute cache status differs"
  | `Assoc fields
    when exact_fields ["bytes"; "cache_key"; "entries"; "root"; "status"] fields ->
    let*? actual_key = string_field fields "cache_key" in
    let*? status = string_field fields "status" in
    let*? root = string_field fields "root" in
    let*? entries = int_field fields "entries" in
    let*? bytes = int_field fields "bytes" in
    if actual_key <> cache_key || status <> "ready" then
      Error "resource compute cache status differs"
    else
      Ok (Ready { root; entries; bytes })
  | _ -> Error "resource compute cache status refused"

let native_sink = {
  status = (fun cache_key ->
    let* result = Octra_core.Circle_wasm_host.compute_storage_status cache_key in
    Lwt.return
      (match result with
       | Error error ->
         Error (Octra_core.Circle_wasm_host.error_message error)
       | Ok json -> cache_status_of_json cache_key json));
  begin_cache = (fun cache_key ->
    let* result = Octra_core.Circle_wasm_host.compute_storage_begin cache_key in
    Lwt.return (result_unit (native_result result)));
  put = (fun ~cache_key ~key ~value ->
    let* result =
      Octra_core.Circle_wasm_host.compute_storage_put ~cache_key ~key ~value in
    Lwt.return (result_unit (native_result result)));
  seal = (fun ~cache_key ~root ~entries ~bytes ->
    let* result =
      Octra_core.Circle_wasm_host.compute_storage_seal
        ~cache_key
        ~root
        ~entries
        ~bytes in
    Lwt.return (result_unit (native_result result)));
  drop = (fun cache_key ->
    let* result = Octra_core.Circle_wasm_host.compute_storage_drop cache_key in
    Lwt.return (result_unit (native_result result)));
}

let asset_of_store (snapshot : snapshot) circle_id path = function
  | Error _ as error -> error
  | Ok value ->
    begin
      match Base64.decode value.Octra_core.Store_irmin.body_b64 with
      | Error _ -> Error "circle asset base64 refused"
      | Ok body ->
        let meta = value.meta in
        Ok {
          state_root = snapshot.state_root;
          circle_id;
          path;
          content_type = meta.Octra_core.Circles.content_type;
          encoding = meta.encoding;
          browser_mode = value.browser_mode;
          resource_mode = value.resource_mode;
          blob_hash = meta.blob_hash;
          size_bytes = Int64.to_int meta.size_bytes;
          body;
        }
    end

let store_source_of_snapshot ?state_root source =
  let snapshot = {
    epoch_id = source.Octra_core.Store_irmin.epoch_id;
    state_root = Option.value ~default:source.state_root state_root;
  } in
  let fetch requested ~circle_id ~path =
    if requested <> snapshot then
      Lwt.return (Error "resource compute snapshot differs")
    else
      let* result =
        Octra_core.Store_irmin.get_circle_public_asset_at
          source
          circle_id
          path in
      Lwt.return (asset_of_store snapshot circle_id path result) in
  snapshot, fetch

let store_source store =
  let* result = Octra_core.Store_irmin.capture_read_snapshot store in
  Lwt.return (Result.map store_source_of_snapshot result)

let store_source_at store ~epoch_id ~state_root =
  let* result =
    Octra_core.Store_irmin.capture_read_snapshot_epoch store ~epoch_id in
  Lwt.return (Result.map (store_source_of_snapshot ~state_root) result)

let verify_asset
    (snapshot : snapshot)
    ~circle_id
    ~path
    ~content_type
    (asset : asset) =
  let actual_hash = Digestif.SHA256.(to_hex (digest_string asset.body)) in
  if asset.state_root <> snapshot.state_root then
    Error "circle asset state root differs"
  else if asset.circle_id <> circle_id || asset.path <> path then
    Error "circle asset identity differs"
  else if asset.content_type <> content_type then
    Error "circle asset content type differs"
  else if asset.encoding <> "identity" then
    Error "circle asset encoding differs"
  else if asset.browser_mode <> "gateway_allowed" then
    Error "circle asset browser mode differs"
  else if asset.resource_mode <> "public_resources" then
    Error "circle asset resource mode differs"
  else if asset.size_bytes <> String.length asset.body then
    Error "circle asset size differs"
  else if asset.blob_hash <> actual_hash then
    Error "circle asset body hash differs"
  else
    Ok asset.body

let fetch_body fetch_asset (snapshot : snapshot) ~circle_id ~path ~content_type =
  let* result = fetch_asset snapshot ~circle_id ~path in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok asset -> Lwt.return (verify_asset snapshot ~circle_id ~path ~content_type asset)

let parse_json name raw =
  try Ok (Yojson.Safe.from_string raw) with _ -> Error (name ^ " JSON refused")

let digest value =
  Digestif.SHA256.(to_hex (digest_string value))

let push_inline sink (plan : Graph.cache_plan) =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | (entry : Graph.cache_entry) :: entries ->
      begin
        match entry.source with
        | Graph.Model_chunk _ -> loop entries
        | Graph.Inline value ->
          let* result = sink.put ~cache_key:plan.Graph.key ~key:entry.key ~value in
          begin
            match result with
            | Ok () -> loop entries
            | Error _ as error -> Lwt.return error
          end
      end in
  loop plan.Graph.entries

let fetch_segment
    fetch_asset
    (snapshot : snapshot)
    (shard : Graph.shard)
    (segment : Graph.segment) =
  let* result =
    fetch_body
      fetch_asset
      snapshot
      ~circle_id:shard.Graph.circle_id
      ~path:segment.Graph.path
      ~content_type:"application/octet-stream" in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok body ->
    if String.length body <> segment.bytes || digest body <> segment.sha256 then
      Lwt.return (Error "resource segment differs")
    else
      Lwt.return (Ok body)

let fetch_shard_body fetch_asset (snapshot : snapshot) (shard : Graph.shard) =
  let rec loop buffer = function
    | [] -> Lwt.return (Ok (Buffer.contents buffer))
    | segment :: segments ->
      let* result = fetch_segment fetch_asset snapshot shard segment in
      begin
        match result with
        | Error _ as error -> Lwt.return error
        | Ok body ->
          Buffer.add_string buffer body;
          loop buffer segments
      end in
  loop (Buffer.create shard.Graph.bytes) shard.segments

let verify_shard_manifest
    fetch_asset
    (snapshot : snapshot)
    (graph : Graph.t)
    (shard : Graph.shard) =
  let* result =
    fetch_body
      fetch_asset
      snapshot
      ~circle_id:shard.Graph.circle_id
      ~path:"/model/shard.json"
      ~content_type:"application/json; charset=utf-8" in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok raw ->
    begin
      match parse_json "resource shard manifest" raw with
      | Error _ as error -> Lwt.return error
      | Ok json ->
        let expected = Graph.shard_manifest_json graph shard in
        if Graph.stable_json json <> Graph.stable_json expected then
          Lwt.return (Error "resource shard manifest differs")
        else
          Lwt.return (Ok ())
    end

let push_shard_chunks
    sink
    (plan : Graph.cache_plan)
    (graph : Graph.t)
    (shard : Graph.shard)
    body =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | index :: indices ->
      let chunk = graph.Graph.chunks.(index) in
      let value = String.sub body chunk.shard_offset chunk.bytes in
      if digest value <> chunk.sha256 then
        Lwt.return (Error "resource chunk differs")
      else
        let key = Printf.sprintf "w:%s:%d" chunk.tensor chunk.tensor_chunk in
        let* result = sink.put ~cache_key:plan.Graph.key ~key ~value in
        begin
          match result with
          | Ok () -> loop indices
          | Error _ as error -> Lwt.return error
        end in
  loop shard.Graph.chunk_indices

let push_shard
    fetch_asset
    sink
    (snapshot : snapshot)
    (plan : Graph.cache_plan)
    (graph : Graph.t)
    (shard : Graph.shard) =
  let* manifest_result = verify_shard_manifest fetch_asset snapshot graph shard in
  match manifest_result with
  | Error _ as error -> Lwt.return error
  | Ok () ->
    let* body_result = fetch_shard_body fetch_asset snapshot shard in
    begin
      match body_result with
      | Error _ as error -> Lwt.return error
      | Ok body ->
        if String.length body <> shard.Graph.bytes || digest body <> shard.sha256 then
          Lwt.return (Error "resource shard differs")
        else
          push_shard_chunks sink plan graph shard body
    end

let push_shards
    fetch_asset
    sink
    (snapshot : snapshot)
    (plan : Graph.cache_plan)
    (graph : Graph.t) =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | shard :: shards ->
      let* result = push_shard fetch_asset sink snapshot plan graph shard in
      begin
        match result with
        | Ok () -> loop shards
        | Error _ as error -> Lwt.return error
      end in
  loop graph.Graph.shards

let cleanup sink cache_key error =
  let* _ = sink.drop cache_key in
  Lwt.return (Error error)

let fill_cache
    fetch_asset
    sink
    (snapshot : snapshot)
    (plan : Graph.cache_plan)
    (graph : Graph.t) =
  let* begin_result = sink.begin_cache plan.Graph.key in
  match begin_result with
  | Error message -> cleanup sink plan.key message
  | Ok () ->
    let* inline_result = push_inline sink plan in
    begin
      match inline_result with
      | Error message -> cleanup sink plan.key message
      | Ok () ->
        let* shard_result = push_shards fetch_asset sink snapshot plan graph in
        begin
          match shard_result with
          | Error message -> cleanup sink plan.key message
          | Ok () ->
            let bytes = Int64.to_int plan.bytes in
            let* seal_result =
              sink.seal
                ~cache_key:plan.key
                ~root:plan.root
                ~entries:(List.length plan.entries)
                ~bytes in
            begin
              match seal_result with
              | Ok () -> Lwt.return (Ok ())
              | Error message -> cleanup sink plan.key message
            end
        end
    end

let load ~limits ~(snapshot : snapshot) ~root_circle_id ~fetch_asset ~sink =
  if snapshot.epoch_id < 0L then
    Lwt.return (Error "resource compute snapshot epoch refused")
  else
    let* graph_result =
      fetch_body
        fetch_asset
        snapshot
        ~circle_id:root_circle_id
        ~path:"/model/resource-graph.json"
        ~content_type:"application/json; charset=utf-8" in
    match graph_result with
    | Error _ as error -> Lwt.return error
    | Ok raw ->
      begin
        match Graph.parse ~limits ~root_circle_id raw with
        | Error _ as error -> Lwt.return error
        | Ok graph ->
          begin
            match Graph.cache_plan ~limits ~state_root:snapshot.state_root graph with
            | Error _ as error -> Lwt.return error
            | Ok cache ->
              let* status_result = sink.status cache.Graph.key in
              begin
                match status_result with
                | Error _ as error -> Lwt.return error
                | Ok (Ready ready)
                  when ready.root = cache.root
                    && ready.entries = List.length cache.entries
                    && Int64.of_int ready.bytes = cache.bytes ->
                  Lwt.return (Ok { snapshot; graph; cache })
                | Ok (Ready _) ->
                  cleanup sink cache.key "resource compute cache identity differs"
                | Ok Missing ->
                  let* result = fill_cache fetch_asset sink snapshot cache graph in
                  begin
                    match result with
                    | Ok () -> Lwt.return (Ok { snapshot; graph; cache })
                    | Error _ as error -> Lwt.return error
                  end
              end
          end
      end