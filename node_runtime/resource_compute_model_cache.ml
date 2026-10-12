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
  graph : Graph.t;
  cache : Graph.cache_plan;
}

module Owner = struct
  module Rpc = Resource_compute_provider_rpc

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

  type token = {
    id : string;
    generation : int64;
    key : string;
  }

  type plan = {
    token : token;
    request : Rpc.prepare;
    victim : model option;
    shared : bool;
  }

  type preparation =
    | Ready of model
    | Build of plan

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
    | Reserve : Rpc.prepare -> (preparation, string) result message
    | Acquire : Rpc.execute -> ((token * model), string) result message
    | Dropped : token -> bool message
    | Remember : token * model -> (unit, string) result message
    | Check : token -> (unit, string) result message
    | Forget : token -> unit message
    | Lost : model -> unit message
    | Finish : token -> unit message
    | Release : token * bool -> unit message
    | Close : unit message

  let create limit = {
    models = [];
    leases = [];
    loading = None;
    next = 0L;
    limit;
    closed = false;
    seen = [];
  }

  let content (model : model) (request : Rpc.prepare) =
    model.circle_id = request.circle_id
    && model.graph_root = request.graph_root
    && model.model_root = request.model_root
    && model.program_root = request.program_root
    && model.executor_root = request.executor_root

  let prepared (model : model) (request : Rpc.prepare) =
    content model request
    && List.mem (request.model_epoch, request.model_state_root) model.snapshots

  let executable (model : model) (request : Rpc.execute) =
    List.mem (request.model_epoch, request.model_state_root) model.snapshots
    && model.circle_id = request.circle_id
    && model.graph_root = request.graph_root
    && model.model_root = request.model_root
    && model.program_root = request.program_root
    && model.executor_root = request.executor_root

  let same (left : token) (right : token) =
    left.id = right.id && left.generation = right.generation && left.key = right.key

  let loading state token =
    match state.loading with
    | Some plan when same plan.token token -> Some plan
    | _ -> None

  let leased state key =
    List.exists (fun token -> token.key = key) state.leases

  let retiring state key =
    match state.loading with
    | Some { victim = Some model; _ } -> model.cache_key = key
    | _ -> false

  let touch state key =
    let selected, rest = List.partition (fun model -> model.cache_key = key) state.models in
    { state with models = selected @ rest }

  let keys state =
    List.map (fun model -> model.cache_key) state.models |> List.sort_uniq String.compare

  let reserve state (request : Rpc.prepare) =
    match List.find_opt (fun model -> prepared model request) state.models with
    | Some model when not (retiring state model.cache_key) ->
      touch state model.cache_key, Ok (Ready model)
    | Some _ -> state, Error "resource compute provider busy"
    | None when Option.is_some state.loading ->
      state, Error "resource compute provider busy"
    | None ->
      let key = Graph.cache_key ~circle_id:request.circle_id ~graph_root:request.graph_root in
      let shared = List.exists (fun model -> model.cache_key = key) state.models in
      let victim =
        if shared || List.length (keys state) < 2 then None
        else List.find_opt (fun model -> not (leased state model.cache_key))
          (List.rev state.models) in
      if not shared && List.length (keys state) >= 2 && victim = None then
        state, Error "resource compute provider busy"
      else
        let id = Rpc.prepare_to_json request |> Yojson.Safe.to_string
          |> fun raw -> Digestif.SHA256.(to_hex (digest_string raw)) in
        let token = { id; generation = state.next; key } in
        let plan = { token; request; victim; shared } in
        { state with loading = Some plan; next = Int64.succ state.next }, Ok (Build plan)

  let acquire state (request : Rpc.execute) =
    match List.find_opt (fun model -> executable model request) state.models with
    | None -> state, Error "resource compute model not prepared"
    | Some model ->
      let id = String.concat "\000" [
        request.request_id;
        Int64.to_string request.epoch_id;
        request.state_root;
        Int64.to_string request.model_epoch;
        request.model_state_root;
      ] |> fun raw -> Digestif.SHA256.(to_hex (digest_string raw)) in
      if List.mem id state.seen then
        state, Error "resource compute execution repeated"
      else if retiring state model.cache_key
        || List.length state.leases >= state.limit
        || List.exists (fun token -> token.id = id) state.leases then
        state, Error "resource compute provider busy"
      else
        let token = { id; generation = state.next; key = model.cache_key } in
        let state = touch state model.cache_key in
        { state with leases = token :: state.leases; next = Int64.succ state.next },
        Ok (token, model)

  let remember state token model =
    match loading state token with
    | None -> state, Error "resource compute preparation changed"
    | Some _ when state.closed -> state, Error "resource compute provider stopped"
    | Some plan when model.cache_key <> token.key
      || not (prepared model plan.request) || model.entries < 0 || model.bytes < 0L
      || String.length model.cache_root <> 64
      || not (String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false)
        model.cache_root) ->
      state, Error "resource compute loaded model differs"
    | Some _ ->
      let other = List.find_opt (fun item -> item.cache_key = model.cache_key) state.models in
      begin match other with
      | Some item when item.circle_id <> model.circle_id || item.graph_root <> model.graph_root
        || item.model_root <> model.model_root || item.entries <> model.entries
        || item.bytes <> model.bytes || item.cache_root <> model.cache_root ->
        state, Error "resource compute cache identity differs"
      | _ ->
        let same_binding item =
          item.cache_key = model.cache_key && item.program_root = model.program_root
          && item.executor_root = model.executor_root in
        let old = List.find_opt same_binding state.models in
        let snapshots = match old with
          | None -> model.snapshots
          | Some item -> model.snapshots @ List.filter
              (fun snapshot -> not (List.mem snapshot model.snapshots)) item.snapshots in
        let model = { model with snapshots = List.filteri (fun index _ -> index < 64) snapshots } in
        let aliases, rest = List.partition (fun item -> item.cache_key = model.cache_key)
          (List.filter (fun item -> not (same_binding item)) state.models) in
        let aliases = List.filteri (fun index _ -> index < 63) aliases in
        { state with models = model :: aliases @ rest }, Ok ()
      end

  let delta : type reply. state -> reply message -> state * reply =
    fun state message ->
      match message with
      | Reserve _ when state.closed -> state, Error "resource compute provider stopped"
      | Acquire _ when state.closed -> state, Error "resource compute provider stopped"
      | Reserve _ when state.next = Int64.max_int ->
        state, Error "resource compute generation exhausted"
      | Acquire _ when state.next = Int64.max_int ->
        state, Error "resource compute generation exhausted"
      | Reserve request -> reserve state request
      | Acquire request -> acquire state request
      | Dropped token ->
        begin match loading state token with
        | Some { victim = Some model; _ } when not (leased state model.cache_key) ->
          { state with models = List.filter
              (fun item -> item.cache_key <> model.cache_key) state.models }, true
        | _ -> state, false
        end
      | Remember (token, model) -> remember state token model
      | Check _ when state.closed -> state, Error "resource compute provider stopped"
      | Check token ->
        if Option.is_some (loading state token) || List.exists (same token) state.leases then
          state, Ok ()
        else state, Error "resource compute operation changed"
      | Finish token ->
        if Option.is_some (loading state token) then { state with loading = None }, ()
        else state, ()
      | Forget token ->
        if List.exists (same token) state.leases then
          {state with models = List.filter (fun model -> model.cache_key <> token.key) state.models}, ()
        else state, ()
      | Lost model ->
        if leased state model.cache_key then state, ()
        else {state with models = List.filter (fun item ->
          item.cache_key <> model.cache_key || item.cache_root <> model.cache_root) state.models}, ()
      | Release (token, spent) ->
        if List.exists (same token) state.leases then
          let seen = if not spent then state.seen else
            token.id :: List.filter ((<>) token.id) state.seen
              |> List.filteri (fun index _ -> index < 128) in
          { state with leases = List.filter (fun item -> not (same token item)) state.leases; seen }, ()
        else state, ()
      | Close -> { state with closed = true }, ()
end

let ( let*? ) value next =
  match value with
  | Ok value -> next value
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

let decode_body ~max_bytes ~size_bytes body_b64 =
  if max_bytes < 0 || size_bytes < 0 || size_bytes > max_bytes then
    Error "circle asset size exceeds limit"
  else
    let encoded = Int64.(mul 4L (div (add (of_int size_bytes) 2L) 3L)) in
    if Int64.of_int (String.length body_b64) > encoded then
      Error "circle asset encoded size exceeds limit"
    else
      match Base64.decode body_b64 with
      | Error _ -> Error "circle asset base64 refused"
      | Ok body when String.length body <> size_bytes -> Error "circle asset size differs"
      | Ok body -> Ok body

let asset_of_rpc_json ~max_bytes ~state_root = function
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
    let*? body = decode_body ~max_bytes ~size_bytes body_b64 in
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

let native_drop cache_key =
  let* result = Octra_core.Circle_wasm_host.compute_storage_drop cache_key in
  Lwt.return (match native_result result with
    | Error _ as error -> error
    | Ok (`Assoc fields) when exact_fields ["cache_key"; "status"] fields ->
      let*? key = string_field fields "cache_key" in
      let*? status = string_field fields "status" in
      if key <> cache_key then Error "resource compute cache drop key differs"
      else (match status with
        | "missing" -> Ok true
        | "busy" -> Ok false
        | _ -> Error "resource compute cache drop status differs")
    | _ -> Error "resource compute cache drop response refused")

let native_hold (model : Owner.model) =
  let module Native = Octra_core.Circle_wasm_native in
  let lease = Native.lease_new () in
  let body = Yojson.Safe.to_string (`Assoc [
    "compute_storage_cache_key", `String model.cache_key;
    "compute_storage_root", `String model.cache_root;
    "compute_storage_entries", `Int model.entries;
    "compute_storage_bytes", `Intlit (Int64.to_string model.bytes);
  ]) in
  match Native.lease_hold lease body with
  | 0 -> Ok lease
  | code ->
    Native.lease_close lease;
    let reason = match code with
      | 1 -> "compute storage cache missing"
      | 2 -> "compute storage cache identity differs"
      | 3 -> "compute storage leases busy"
      | 4 -> "compute storage generation exhausted"
      | 5 -> "compute storage hold input refused"
      | _ -> "compute storage cache unavailable" in
    Error reason

let native_writer cache_key =
  let module Native = Octra_core.Circle_wasm_native in
  let port = Native.build_new () in
  match Native.build_begin port cache_key with
  | 0 ->
    let step action fields =
      let body = Yojson.Safe.to_string (`Assoc (("action", `String action) :: fields)) in
      let* code, raw = Lwt.protected
        (Octra_core.Exec_resource.detach (Native.build_step port) body) in
      Lwt.return (if code = 0 then Ok () else Error raw) in
    Ok {
      put = (fun ~key ~value -> step "compute_storage_put" [
        "compute_storage_key", `String key;
        "compute_storage_value_b64", `String (Base64.encode_exn value);
      ]);
      seal = (fun ~root ~entries ~bytes -> step "compute_storage_seal" [
        "compute_storage_root", `String root;
        "compute_storage_entries", `Int entries;
        "compute_storage_bytes", `Int bytes;
      ]);
      close = (fun () -> Native.build_close port);
    }
  | code ->
    Native.build_close port;
    Error (match code with
      | 1 -> "compute storage builder busy"
      | 2 -> "compute storage builders busy"
      | 3 -> "compute storage generation exhausted"
      | 4 -> "compute storage port used"
      | 5 -> "compute storage key input refused"
      | _ -> "compute storage builder unavailable")

let native_sink = {
  status = (fun cache_key ->
    let* result = Octra_core.Circle_wasm_host.compute_storage_status cache_key in
    Lwt.return
      (match result with
       | Error error ->
         Error (Octra_core.Circle_wasm_host.error_message error)
       | Ok json -> cache_status_of_json cache_key json));
  begin_cache = (fun cache_key -> Lwt.return (native_writer cache_key));
  drop = (fun cache_key ->
    let* result = native_drop cache_key in
    Lwt.return (match result with
      | Ok true -> Ok ()
      | Ok false -> Error "compute storage cache busy"
      | Error _ as error -> error));
}

let asset_of_store ~max_bytes (snapshot : snapshot) circle_id path = function
  | Error _ as error -> error
  | Ok value ->
    let meta = value.Octra_core.Store_irmin.meta in
    let*? body = decode_body ~max_bytes ~size_bytes:(Int64.to_int meta.size_bytes)
      value.body_b64 in
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

let store_source_of_snapshot ?state_root source =
  let snapshot = {
    epoch_id = source.Octra_core.Store_irmin.epoch_id;
    state_root = Option.value ~default:source.state_root state_root;
  } in
  let fetch requested ~max_bytes ~circle_id ~path =
    if requested <> snapshot then
      Lwt.return (Error "resource compute snapshot differs")
    else
      let* result =
        Octra_core.Store_irmin.get_circle_public_asset_at
          ~max_bytes
          source
          circle_id
          path in
      Lwt.return (asset_of_store ~max_bytes snapshot circle_id path result) in
  snapshot, fetch

let store_source store =
  let* result = Octra_core.Store_irmin.capture_read_snapshot store in
  Lwt.return (Result.map store_source_of_snapshot result)

let store_source_at store ~epoch_id ~state_root ~index_root =
  let* result =
    Octra_core.Store_irmin.capture_read_snapshot_at
      ~index_root store ~epoch_id ~state_root in
  Lwt.return (Result.map (store_source_of_snapshot ~state_root) result)

let verify_asset
    (snapshot : snapshot)
    ~max_bytes
    ~circle_id
    ~path
    ~content_type
    (asset : asset) =
  if max_bytes < 0 || asset.size_bytes < 0 || asset.size_bytes > max_bytes
    || String.length asset.body > max_bytes then
    Error "circle asset size exceeds limit"
  else if asset.state_root <> snapshot.state_root then
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
  else if asset.blob_hash <> Digestif.SHA256.(to_hex (digest_string asset.body)) then
    Error "circle asset body hash differs"
  else
    Ok asset.body

let fetch_body fetch_asset (snapshot : snapshot) ~max_bytes ~circle_id ~path ~content_type =
  let* result = fetch_asset snapshot ~max_bytes ~circle_id ~path in
  match result with
  | Error _ as error -> Lwt.return error
  | Ok asset -> Lwt.return (verify_asset snapshot ~max_bytes ~circle_id ~path ~content_type asset)

let parse_json name raw =
  try Ok (Octra_core.Json_tree.read_plain raw)
  with Yojson.Json_error _ -> Error (name ^ " JSON refused")

let digest value =
  Digestif.SHA256.(to_hex (digest_string value))

let push_inline (sink : writer) (plan : Graph.cache_plan) =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | (entry : Graph.cache_entry) :: entries ->
      begin
        match entry.source with
        | Graph.Model_chunk _ -> loop entries
        | Graph.Inline value ->
          let* result = sink.put ~key:entry.key ~value in
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
      ~max_bytes:segment.bytes
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

let verify_shard_manifest
    ~max_bytes
    fetch_asset
    (snapshot : snapshot)
    (graph : Graph.t)
    (shard : Graph.shard) =
  let* result =
    fetch_body
      fetch_asset
      snapshot
      ~max_bytes
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

type shard_stream = {
  body : string;
  offset : int;
  segments : Graph.segment list;
  ctx : Digestif.SHA256.ctx;
  fetched : int;
}

let push_shard_chunks
    fetch_asset
    (sink : writer)
    (snapshot : snapshot)
    (graph : Graph.t)
    (shard : Graph.shard) =
  let rec fill stream value at =
    if at = Bytes.length value then
      Lwt.return (Ok (Bytes.to_string value, stream))
    else if stream.offset < String.length stream.body then
      let count = min (Bytes.length value - at) (String.length stream.body - stream.offset) in
      Bytes.blit_string stream.body stream.offset value at count;
      fill {stream with offset = stream.offset + count} value (at + count)
    else
      match stream.segments with
      | [] -> Lwt.return (Error "resource shard differs")
      | segment :: segments ->
        let* result = fetch_segment fetch_asset snapshot shard segment in
        begin match result with
        | Error _ as error -> Lwt.return error
        | Ok body ->
          let ctx = Digestif.SHA256.feed_string stream.ctx body in
          fill {body; offset = 0; segments; ctx; fetched = stream.fetched + String.length body}
            value at
        end in
  let rec loop stream = function
    | [] ->
      if stream.segments <> [] || stream.offset <> String.length stream.body
        || stream.fetched <> shard.bytes
        || Digestif.SHA256.(to_hex (get stream.ctx)) <> shard.sha256 then
        Lwt.return (Error "resource shard differs")
      else Lwt.return (Ok ())
    | index :: indices ->
      let chunk = graph.Graph.chunks.(index) in
      let* result = fill stream (Bytes.create chunk.bytes) 0 in
      begin match result with
      | Error _ as error -> Lwt.return error
      | Ok (value, stream) ->
        if digest value <> chunk.sha256 then
          Lwt.return (Error "resource chunk differs")
        else
          let key = Printf.sprintf "w:%s:%d" chunk.tensor chunk.tensor_chunk in
          let* result = sink.put ~key ~value in
          begin match result with
          | Ok () -> loop stream indices
          | Error _ as error -> Lwt.return error
          end
      end in
  loop {body = ""; offset = 0; segments = shard.segments;
    ctx = Digestif.SHA256.init (); fetched = 0} shard.chunk_indices

let push_shard
    ~max_bytes
    fetch_asset
    sink
    (snapshot : snapshot)
    (graph : Graph.t)
    (shard : Graph.shard) =
  let* manifest_result = verify_shard_manifest ~max_bytes fetch_asset snapshot graph shard in
  match manifest_result with
  | Error _ as error -> Lwt.return error
  | Ok () -> push_shard_chunks fetch_asset sink snapshot graph shard

let push_shards
    ~max_bytes
    fetch_asset
    sink
    (snapshot : snapshot)
    (graph : Graph.t) =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | shard :: shards ->
      let* result = push_shard ~max_bytes fetch_asset sink snapshot graph shard in
      begin
        match result with
        | Ok () -> loop shards
        | Error _ as error -> Lwt.return error
      end in
  loop graph.Graph.shards

let fill_cache
    ~cancel
    ~max_bytes
    fetch_asset
    sink
    (snapshot : snapshot)
    (plan : Graph.cache_plan)
    (graph : Graph.t) =
  let* begin_result = if Lwt.is_sleeping cancel then sink.begin_cache plan.Graph.key
    else Lwt.map (fun reason -> Error reason) cancel in
  match begin_result with
  | Error message -> Lwt.return (Error message)
  | Ok writer ->
    let closed = ref false in
    let close () = if not !closed then (closed := true; writer.close ()) in
    let running = Lwt.finalize (fun () ->
    let* inline_result = push_inline writer plan in
    begin
      match inline_result with
      | Error message -> Lwt.return (Error message)
      | Ok () ->
        let* shard_result = push_shards ~max_bytes fetch_asset writer snapshot graph in
        begin
          match shard_result with
          | Error message -> Lwt.return (Error message)
          | Ok () ->
            let bytes = Int64.to_int plan.bytes in
            let* seal_result =
              writer.seal
                ~root:plan.root
                ~entries:(List.length plan.entries)
                ~bytes in
            begin
              match seal_result with
              | Ok () -> Lwt.return (Ok ())
              | Error message -> Lwt.return (Error message)
            end
        end
    end) (fun () -> close (); Lwt.return_unit) in
    let cancelled =
      let* reason = Lwt.protected cancel in
      close ();
      Lwt.return (Error reason) in
    Lwt.pick [running; cancelled]

let load ~cancel ~limits ~(snapshot : snapshot) ~root_circle_id ~graph_root ~model_root ~fetch_asset ~sink =
  if not (Lwt.is_sleeping cancel) then
    Lwt.map (fun reason -> Error reason) cancel
  else if snapshot.epoch_id < 0L then
    Lwt.return (Error "resource compute snapshot epoch refused")
  else
    let* graph_result =
      fetch_body
        fetch_asset
        snapshot
        ~max_bytes:limits.Graph.max_graph_bytes
        ~circle_id:root_circle_id
        ~path:"/model/resource-graph.json"
        ~content_type:"application/json; charset=utf-8" in
    match graph_result with
    | Error _ as error -> Lwt.return error
    | Ok raw ->
      begin
        match Graph.parse ~limits ~root_circle_id raw with
        | Error _ as error -> Lwt.return error
        | Ok graph when graph.Graph.graph_root <> graph_root ->
          Lwt.return (Error "resource compute graph root differs")
        | Ok graph when graph.model.root <> model_root ->
          Lwt.return (Error "resource compute model root differs")
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
                  let writer = {put = (fun ~key:_ ~value:_ -> Lwt.return_ok ());
                    seal = (fun ~root:_ ~entries:_ ~bytes:_ -> Lwt.return_ok ());
                    close = (fun () -> ())} in
                  let* checked = push_shards ~max_bytes:limits.Graph.max_graph_bytes
                    fetch_asset writer snapshot graph in
                  Lwt.return (Result.map (fun () -> {snapshot; graph; cache}) checked)
                | Ok (Ready _) ->
                  Lwt.return (Error "resource compute cache identity differs")
                | Ok Missing ->
                  let* result = fill_cache ~cancel ~max_bytes:limits.Graph.max_graph_bytes
                    fetch_asset sink snapshot cache graph in
                  begin
                    match result with
                    | Ok () -> Lwt.return (Ok { snapshot; graph; cache })
                    | Error _ as error -> Lwt.return error
                  end
              end
          end
      end

let load_checked ~cancel ~limits ~snapshot ~root_circle_id ~graph_root ~model_root ~fetch_asset ~sink =
  let module Host = Octra_core.Circle_wasm_host in
  let exception Local of string in
  let local result = let* result = result in match result with
    | Ok value -> Lwt.return_ok value
    | Error reason -> Lwt.fail (Local reason) in
  let sink = {sink with
    status = (fun key -> local (sink.status key));
    begin_cache = (fun key ->
      let* opened = local (sink.begin_cache key) in
      Lwt.return (Result.map (fun writer -> {writer with
        put = (fun ~key ~value -> local (writer.put ~key ~value));
        seal = (fun ~root ~entries ~bytes -> local (writer.seal ~root ~entries ~bytes))}) opened))} in
  Lwt.catch (fun () ->
    let* result = load ~cancel ~limits ~snapshot ~root_circle_id ~graph_root ~model_root ~fetch_asset ~sink in
    Lwt.return (Result.map_error (fun reason ->
      if Lwt.is_sleeping cancel then Host.Rejected reason else Host.Unavailable reason) result))
    (function Local reason -> Lwt.return_error (Host.Unavailable reason) | error -> Lwt.fail error)