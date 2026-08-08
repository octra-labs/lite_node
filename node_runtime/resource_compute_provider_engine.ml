(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

module Cache = Resource_compute_model_cache
module Graph = Octra_core.Resource_compute_graph
module Provider_rpc = Resource_compute_provider_rpc

type program = {
  circle_id : string;
  code_b64 : string;
  code_hash : string;
  runtime : string;
}

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
  load_model : Provider_rpc.prepare -> (model, string) result Lwt.t;
  drop_model : model -> (unit, string) result Lwt.t;
  self_test : unit -> (Provider_rpc.self_test, string) result Lwt.t;
  execute :
    model:model ->
    program:program ->
    storage:(string * string) list ->
    Provider_rpc.execute ->
    (native_result, string) result Lwt.t;
}

type t = {
  limits : Resource_compute_provider_config.limits;
  deps : deps;
  capacity : Circle_view_capacity.t;
  prepare_lock : Lwt_mutex.t;
  mutable models : model list;
}

let ( let*? ) value next =
  match value with
  | Ok value -> next value
  | Error _ as error -> error

let lower_hex_64 value =
  String.length value = 64
  && String.for_all
       (function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
       value

let program_digest code_b64 =
  match Base64.decode code_b64 with
  | Error _ -> Error "resource compute program base64 refused"
  | Ok code -> Ok Digestif.SHA256.(to_hex (digest_string code))

let response_json = function
  | Octra_core.Circle_wasm_codec.Resp_null -> `Null
  | Octra_core.Circle_wasm_codec.Resp_bool value -> `Bool value
  | Octra_core.Circle_wasm_codec.Resp_int value -> `Intlit value
  | Octra_core.Circle_wasm_codec.Resp_string value -> `String value

let self_test_of_json = function
  | Error _ as error -> error
  | Ok json -> Provider_rpc.self_test_of_json json

let host_result =
  Result.map_error Octra_core.Circle_wasm_host.error_message

let native_self_test () =
  let* result = Octra_core.Circle_wasm_host.compute_self_test () in
  Lwt.return (self_test_of_json (host_result result))

let graph_limits memory_bytes =
  let max_cache_bytes =
    Int64.min 536_870_912L (Int64.div memory_bytes 2L) in
  {
    Graph.default_limits with
    max_model_bytes = max_cache_bytes;
    max_cache_bytes;
  }

let model_of_loaded
    (request : Provider_rpc.prepare)
    (loaded : Cache.loaded) =
  let graph = loaded.Cache.graph in
  let cache = loaded.cache in
  if graph.Graph.graph_root <> request.graph_root then
    Error "resource compute graph root differs"
  else if graph.model.root <> request.model_root then
    Error "resource compute model root differs"
  else
    Ok {
      snapshots = [request.model_epoch, request.model_state_root];
      circle_id = request.circle_id;
      graph_root = request.graph_root;
      model_root = request.model_root;
      program_root = request.program_root;
      executor_root = request.executor_root;
      cache_key = cache.Graph.key;
      entries = List.length cache.entries;
      bytes = cache.bytes;
    }

let committed_snapshot state_root_at ~epoch_id ~state_root =
  if not (Text.is_hex_len 64 state_root) then
    Error "resource compute snapshot root refused"
  else if state_root_at epoch_id <> Some (Text.hex_to_string state_root) then
    Error "resource compute snapshot root differs"
  else
    Ok ()

let native_program state_root_at store ~epoch_id ~state_root ~circle_id =
  match committed_snapshot state_root_at ~epoch_id ~state_root with
  | Error _ as error -> Lwt.return error
  | Ok () ->
  let* snapshot_result =
    Octra_core.Store_irmin.capture_read_snapshot_epoch store ~epoch_id in
  match snapshot_result with
  | Error _ as error -> Lwt.return error
  | Ok snapshot ->
    let* program_result =
      Octra_core.Store_irmin.get_circle_program_at snapshot circle_id in
    Lwt.return
      (match program_result with
       | Error _ as error -> error
       | Ok program
         when program.privacy_class <> "public"
           || program.resource_mode <> "public_resources" ->
         Error "resource compute circle is not public"
       | Ok program ->
         Ok {
           circle_id;
           code_b64 = program.code_b64;
           code_hash = program.code_hash;
           runtime = program.runtime;
         })

let native_load_model limits state_root_at store (request : Provider_rpc.prepare) =
  match
    committed_snapshot
      state_root_at
      ~epoch_id:request.model_epoch
      ~state_root:request.model_state_root
  with
  | Error _ as error -> Lwt.return error
  | Ok () ->
  let* source_result =
    Cache.store_source_at
      store
      ~epoch_id:request.model_epoch
      ~state_root:request.model_state_root in
  match source_result with
  | Error _ as error -> Lwt.return error
  | Ok (snapshot, fetch_asset) ->
    if
      snapshot.Cache.epoch_id <> request.model_epoch
      || snapshot.state_root <> request.model_state_root
    then
      Lwt.return (Error "resource compute snapshot differs")
    else
      let* result =
        Cache.load
          ~limits:(graph_limits limits.Resource_compute_provider_config.memory_bytes)
          ~snapshot
          ~root_circle_id:request.circle_id
          ~fetch_asset
          ~sink:Cache.native_sink in
      Lwt.return
        (match result with
         | Error _ as error -> error
         | Ok loaded -> model_of_loaded request loaded)

let native_drop_model model =
  let* result = Octra_core.Circle_wasm_host.compute_storage_drop model.cache_key in
  Lwt.return
    (match result with
     | Ok _ -> Ok ()
     | Error error ->
       Error (Octra_core.Circle_wasm_host.error_message error))

let storage_table entries =
  let table = Hashtbl.create (max 1 (List.length entries)) in
  List.iter (fun (key, value) -> Hashtbl.add table key value) entries;
  table

let params_of_json raw =
  match Yojson.Safe.from_string raw with
  | `List params -> Ok params
  | _ -> Error "resource compute params list required"
  | exception _ -> Error "resource compute params JSON refused"

let session_scope (request : Provider_rpc.execute) =
  Octra_net.Hash_domain.hash_encoded "octra:resource_compute_session" (fun buffer ->
    Octra_net.Oce1.put_string buffer request.circle_id;
    Octra_net.Oce1.put_hash32 buffer (Text.hex_to_raw32_lossy request.model_root);
    Octra_net.Oce1.put_string buffer request.caller;
    Octra_net.Oce1.put_hash32 buffer (Text.hex_to_raw32_lossy request.session_id))
  |> Text.raw_to_hex

let native_execute ~model ~program ~storage (request : Provider_rpc.execute) =
  match params_of_json request.params_json with
  | Error _ as error -> Lwt.return error
  | Ok params ->
    begin
      match Octra_core.Circle_wasm_codec.encode_request ~method_name:request.method_name params with
      | Error _ as error -> Lwt.return error
      | Ok request_bytes ->
        let* result =
          Octra_core.Circle_wasm_host.execute_compute_isolated_with_storage
            ~compute_session_scope:(session_scope request)
            ~compute_storage_cache_key:(Some model.cache_key)
            ~code_b64:program.code_b64
            ~export_name:"octra_query"
            ~request_bytes
            ~storage_tbl:(storage_table storage)
            ~storage_cache_key:None
            ~caller:request.caller
            ~address:request.circle_id
            ~tx_hash:request.request_id
            ~current_epoch:(Int64.to_int request.epoch_id)
            ~hfhe_caps:[]
            ~hfhe_pubkeys:[]
            ~hfhe_active_key:None
            ~hfhe_mode:Octra_core.Circle_hfhe_transcript.Direct
            ~public_reads:[]
            ~fuel_limit:2_000_000_000 in
        Lwt.return
          (match result with
           | Error error ->
             Error (Octra_core.Circle_wasm_host.error_message error)
           | Ok result when not result.Octra_core.Circle_wasm_host.success ->
             Error
               (Option.value
                  ~default:"resource compute execution refused"
                  result.error)
           | Ok result ->
             begin
               match
                 result.response_value,
                 result.compute_trace_root,
                 result.compute_steps,
                 result.compute_operations
               with
               | Some response, Some trace_root, Some steps, Some operations ->
                 Ok {
                   output_json =
                     response
                     |> response_json
                     |> Graph.stable_json;
                   trace_root;
                   steps;
                   operations;
                   effort_used = result.effort_used;
                 }
               | _ -> Error "resource compute trace missing"
             end)
    end

let native_deps ~limits ~store ~state_root_at =
  {
    program = native_program state_root_at store;
    storage = (fun ~epoch_id ~state_root ~circle_id ->
      let* snapshot_result =
          match committed_snapshot state_root_at ~epoch_id ~state_root with
          | Error _ as error -> Lwt.return error
          | Ok () ->
            Octra_core.Store_irmin.capture_read_snapshot_epoch store ~epoch_id in
      let* result =
        match snapshot_result with
        | Error _ as error -> Lwt.return error
        | Ok snapshot ->
          Octra_core.Store_irmin.load_circle_stable_storage_at
            snapshot
            circle_id in
      Lwt.return
        (match result with
         | Error _ as error -> error
         | Ok entries when List.length entries > 2048 ->
           Error "resource compute storage entries exceed limit"
         | Ok entries ->
           let bytes =
             List.fold_left
               (fun total (key, value) ->
                 total + String.length key + String.length value)
               0
               entries in
           if bytes > 2 * 1024 * 1024 then
             Error "resource compute storage exceeds limit"
           else
             Ok entries));
    load_model = native_load_model limits state_root_at store;
    drop_model = native_drop_model;
    self_test = native_self_test;
    execute = native_execute;
  }

let create ~limits ~deps =
  {
    limits;
    deps;
    capacity = Circle_view_capacity.create ~limit:limits.Resource_compute_provider_config.lanes;
    prepare_lock = Lwt_mutex.create ();
    models = [];
  }

let self_test t =
  t.deps.self_test ()

let model_content_matches_prepare (model : model) (request : Provider_rpc.prepare) =
  model.circle_id = request.circle_id
  && model.graph_root = request.graph_root
  && model.model_root = request.model_root
  && model.program_root = request.program_root
  && model.executor_root = request.executor_root

let model_matches_prepare (model : model) (request : Provider_rpc.prepare) =
  model_content_matches_prepare model request
  && List.mem (request.model_epoch, request.model_state_root) model.snapshots

let model_matches_execute (model : model) (request : Provider_rpc.execute) =
  List.mem (request.model_epoch, request.model_state_root) model.snapshots
  && model.circle_id = request.circle_id
  && model.graph_root = request.graph_root
  && model.model_root = request.model_root
  && model.program_root = request.program_root
  && model.executor_root = request.executor_root

let prepared (model : model) = Provider_rpc.{
  cache_key = model.cache_key;
  graph_root = model.graph_root;
  model_root = model.model_root;
  program_root = model.program_root;
  executor_root = model.executor_root;
  entries = model.entries;
  bytes = model.bytes;
}

let touch_model t model =
  t.models <- model :: List.filter (fun value -> value != model) t.models

let remember_snapshot model snapshot =
  if not (List.mem snapshot model.snapshots) then
    model.snapshots <- snapshot :: model.snapshots |> List.filteri (fun index _ -> index < 64)

let find_prepared t request =
  List.find_opt (fun model -> model_matches_prepare model request) t.models

let find_content t request =
  List.find_opt (fun model -> model_content_matches_prepare model request) t.models

let verify_program expected_root (program : program) =
  let*? actual_root = program_digest program.code_b64 in
  if
    program.code_hash <> expected_root
    || actual_root <> expected_root
    || program.runtime <> "wasm_v1"
  then
    Error "resource compute program root differs"
  else
    Ok ()

let evict_if_needed t =
  match List.rev t.models with
  | oldest :: _ when List.length t.models >= 2 ->
    let* result = t.deps.drop_model oldest in
    begin
      match result with
      | Error _ as error -> Lwt.return error
      | Ok () ->
        t.models <- List.filter (fun value -> value != oldest) t.models;
        Lwt.return (Ok ())
    end
  | _ -> Lwt.return (Ok ())

let prepare_locked t request =
  match find_prepared t request with
  | Some model ->
    touch_model t model;
    Lwt.return (Ok (prepared model))
  | None ->
    let* self_test_result = t.deps.self_test () in
    begin
      match self_test_result with
      | Error _ as error -> Lwt.return error
      | Ok test when test.executor_root <> request.Provider_rpc.executor_root ->
        Lwt.return (Error "resource compute executor root differs")
      | Ok _ ->
        let* program_result =
          t.deps.program
            ~epoch_id:request.model_epoch
            ~state_root:request.model_state_root
            ~circle_id:request.circle_id in
                begin
                  match program_result with
                  | Error _ as error -> Lwt.return error
                  | Ok program ->
                    begin
                      match verify_program request.program_root program with
                      | Error _ as error -> Lwt.return error
                      | Ok () ->
                        let content = find_content t request in
                        let* evict_result =
                          match content with
                          | Some _ -> Lwt.return (Ok ())
                          | None -> evict_if_needed t in
                        begin
                          match evict_result with
                          | Error _ as error -> Lwt.return error
                          | Ok () ->
                            let* model_result = t.deps.load_model request in
                            begin
                              match model_result with
                              | Error _ as error -> Lwt.return error
                              | Ok loaded when model_matches_prepare loaded request ->
                                begin
                                  match content with
                                  | Some model when model.cache_key = loaded.cache_key ->
                                    remember_snapshot
                                      model
                                      (request.model_epoch, request.model_state_root);
                                    touch_model t model;
                                    Lwt.return (Ok (prepared model))
                                  | Some _ ->
                                    Lwt.return (Error "resource compute cache key differs")
                                  | None ->
                                    t.models <- loaded :: t.models;
                                    Lwt.return (Ok (prepared loaded))
                                end
                              | Ok _ -> Lwt.return (Error "resource compute loaded model differs")
                            end
                        end
                    end
                end
    end

let prepare t request =
  Lwt_mutex.with_lock t.prepare_lock (fun () -> prepare_locked t request)

let find_model t request =
  List.find_opt (fun model -> model_matches_execute model request) t.models

let execute_ready t model request =
  let output_limit =
    min request.Provider_rpc.max_output_bytes t.limits.max_response_bytes in
  let* program_result =
    t.deps.program
      ~epoch_id:request.epoch_id
      ~state_root:request.state_root
      ~circle_id:request.circle_id in
        begin
          match program_result with
          | Error _ as error -> Lwt.return error
          | Ok program ->
            begin
              match verify_program request.program_root program with
              | Error _ as error -> Lwt.return error
              | Ok () ->
                let* storage_result =
                  t.deps.storage
                    ~epoch_id:request.epoch_id
                    ~state_root:request.state_root
                    ~circle_id:request.circle_id in
                begin
                  match storage_result with
                  | Error _ as error -> Lwt.return error
                  | Ok storage ->
                    let* result =
                      t.deps.execute ~model ~program ~storage request in
                    Lwt.return
                      (match result with
                               | Error _ as error -> error
                               | Ok result ->
                                 let output_bytes = String.length result.output_json in
                                 if output_bytes > output_limit then
                                   Error "resource compute output exceeds requested limit"
                                 else if not (lower_hex_64 result.trace_root) then
                                   Error "resource compute trace root refused"
                                 else if result.steps <= 0L || result.operations <= 0L then
                                   Error "resource compute counters refused"
                                 else
                                   let output_hash =
                                     result.output_json
                                     |> Octra_consensus.Resource_compute_protocol.output_hash
                                     |> Text.raw_to_hex in
                                   Ok Provider_rpc.{
                                     output_json = result.output_json;
                                     output_hash;
                                     trace_root = result.trace_root;
                                     steps = result.steps;
                                     operations = result.operations;
                                     effort_used = result.effort_used;
                                   })
                end
            end
        end

let execute t request =
  if String.length request.Provider_rpc.params_json > t.limits.max_request_bytes then
    Lwt.return (Error "resource compute input exceeds provider limit")
  else
    match find_model t request with
    | None -> Lwt.return (Error "resource compute model not prepared")
    | Some model ->
      touch_model t model;
      Circle_view_capacity.with_slot
        t.capacity
        ~busy:(fun () -> Lwt.return (Error "resource compute provider busy"))
        (fun () -> execute_ready t model request)