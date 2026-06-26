(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


open Lwt.Infix

module Code = Cohttp.Code
module Header = Cohttp.Header
module Ledger = Octra_core.Ledger
module Metrics = Octra_core.Metrics
module Request = Cohttp.Request
module Server = Cohttp_lwt_unix.Server
module State_sync = Octra_bootstrap.State_sync
module Tree = Octra_core.Tree
module WSServer = Octra_core.Ws_server

let cors_headers = Header.of_list [
  "Content-Type", "application/json";
  "Access-Control-Allow-Origin", "*"
]

let octet_stream_headers extra =
  Header.of_list ([
    "Content-Type", "application/octet-stream";
    "Access-Control-Allow-Origin", "*"
  ] @ extra)

let respond_json data =
  Server.respond_string
    ~status:`OK
    ~headers:cors_headers
    ~body:(Yojson.Safe.to_string data)
    ()

let respond_error ?(error_type = "unknown") status msg =
  let json = Rest_view.error_response ~error_type ~reason:msg in
  Server.respond_string
    ~status
    ~headers:cors_headers
    ~body:(Yojson.Safe.to_string json)
    ()

let with_json_body body f =
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  match try Ok (Yojson.Safe.from_string body_str) with e -> Error (Printexc.to_string e) with
  | Error _ ->
      respond_error ~error_type:"malformed_transaction" `Bad_request "invalid JSON"
  | Ok json ->
      f json

let query_param query name =
  match List.assoc_opt name query with
  | Some (v :: _) -> Some v
  | _ -> None

let env_flag name =
  match Sys.getenv_opt name with
  | Some "1" | Some "true" | Some "yes" -> true
  | _ -> false

let state_sync_enabled () =
  env_flag "OCTRA_STATE_SYNC_ENABLE"

let respond_state_sync_disabled () =
  respond_error
    ~error_type:"state_sync_disabled"
    `Forbidden
    "state sync RPC is disabled"

let snapshot_id_of_manifest json =
  match json with
  | `Assoc fields ->
      begin
        match List.assoc_opt "snapshot_id" fields with
        | Some (`String s) -> s
        | _ -> "-"
      end
  | _ -> "-"

let field_string ~default name json =
  match json with
  | `Assoc fields ->
      begin
        match List.assoc_opt name fields with
        | Some (`String s) -> s
        | _ -> default
      end
  | _ -> default

let field_list_len name json =
  match json with
  | `Assoc fields ->
      begin
        match List.assoc_opt name fields with
        | Some (`List xs) -> List.length xs
        | _ -> 0
      end
  | _ -> 0

let handle_manifest ~data_dir ~current_epoch =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    match State_sync.manifest_result ~data_dir ~current_epoch with
    | Error msg ->
        Log.warn "state_sync" "manifest unavailable reason = %s" msg;
        respond_error
          ~error_type:"state_sync_snapshot_unavailable"
          `Service_unavailable
          msg
    | Ok json ->
        let files, bytes = State_sync.manifest_stats json in
        let snapshot_id = snapshot_id_of_manifest json in
        Log.info "state_sync"
          "manifest snapshot = %s files = %d bytes = %s current_epoch = %d"
          snapshot_id files (State_sync.format_bytes bytes) !current_epoch;
        respond_json json

let handle_head ~current_epoch =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    respond_json (State_sync.head_json ~current_epoch)

let handle_client_progress body =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    with_json_body body (fun json ->
      match State_sync.progress_of_yojson json with
      | Error msg ->
          respond_error ~error_type:"state_sync_bad_progress" `Bad_request msg
      | Ok report ->
          Log.info "state_sync" "%s" (State_sync.progress_log_line report);
          respond_json State_sync.progress_accepted_json)

let handle_readiness ~data_dir =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    let path = Filename.concat data_dir "ready_to_vote.json" in
    if not (Sys.file_exists path) then
      respond_json State_sync.readiness_not_ready_json
    else
      try respond_json (Yojson.Safe.from_file path)
      with _ ->
        respond_error
          ~error_type:"state_sync_bad_readiness"
          `Internal_server_error
          "readiness marker is corrupt"

let handle_range ~data_dir ~chaindata query =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    let from_epoch =
      match query_param query "from_epoch" with
      | Some s -> (try Int64.of_string s with _ -> -1L)
      | None -> -1L
    in
    let max_epochs =
      match query_param query "max_epochs" with
      | Some s -> (try int_of_string s with _ -> 16)
      | None -> 16
    in
    if Int64.compare from_epoch 0L < 0 || max_epochs <= 0 then
      respond_error
        ~error_type:"state_sync_bad_range_request"
        `Bad_request
        "invalid from_epoch/max_epochs"
    else
      let json =
        State_sync.range_json
          ~data_dir:(Some data_dir)
          ~chaindata
          ~from_epoch
          ~max_epochs
      in
      let status = field_string ~default:"unknown" "status" json in
      let records = field_list_len "records" json in
      Log.info "state_sync"
        "range from_epoch = %Ld max_epochs = %d status = %s records = %d"
        from_epoch max_epochs status records;
      respond_json json

let int_query_param query name default_value =
  match query_param query name with
  | Some s -> (try int_of_string s with _ -> default_value)
  | None -> default_value

let handle_chunk ~data_dir query =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    let path = Option.value ~default:"" (query_param query "path") in
    let offset = int_query_param query "offset" (-1) in
    let len = int_query_param query "len" (-1) in
    let snapshot_id = query_param query "snapshot" in
    match State_sync.read_chunk ~data_dir ~snapshot_id ~rel:path ~offset ~len with
    | Error msg ->
        respond_error ~error_type:"state_sync_bad_chunk_request" `Bad_request msg
    | Ok (chunk, total_size) ->
        if env_flag "OCTRA_STATE_SYNC_LOG_CHUNKS" then
          Log.info "state_sync"
            "chunk snapshot = %s path = %s offset = %d len = %d file_size = %s"
            (Option.value ~default:"live-legacy" snapshot_id)
            path
            offset
            (String.length chunk)
            (State_sync.format_bytes total_size);
        let sha = Digestif.SHA256.(digest_string chunk |> to_hex) in
        Server.respond_string
          ~status:`OK
          ~headers:(octet_stream_headers [
            "X-Octra-Chunk-Sha256", sha;
            "X-Octra-File-Size", string_of_int total_size;
            "X-Octra-Offset", string_of_int offset;
            "X-Octra-Snapshot", Option.value ~default:"live-legacy" snapshot_id;
          ])
          ~body:chunk
          ()

let handle_options () =
  let headers = Header.of_list [
    "Access-Control-Allow-Origin", "*";
    "Access-Control-Allow-Methods", "GET, POST, DELETE, OPTIONS";
    "Access-Control-Allow-Headers", "Content-Type, Authorization"
  ] in
  Server.respond ~headers ~status:`OK ~body:`Empty ()

let handle
    ~data_dir
    ~ledger
    ~tree_ref
    ~validator
    ~current_epoch
    ~chaindata
    ~encrypted_supply
    req
    body =
  let path = Uri.path (Request.uri req) in
  let query = Uri.query (Request.uri req) in
  match Request.meth req, path with
  | `GET, "/" ->
      let t = !tree_ref in
      respond_json (Rest_view.node_root_response ~validator ~epoch:t.Tree.epoch_id)
  | `GET, "/status" ->
      let t = !tree_ref in
      respond_json (Rest_view.status_response
        ~epoch:t.Tree.epoch_id
        ~validator
        ~root_count:(Tree.root_count t)
        ~timestamp:(Unix.gettimeofday ())
        ~total_accounts:(Ledger.length ledger)
        ~total_supply:(Ledger.get_total_supply ledger)
        ~encrypted_supply:(encrypted_supply ())
        ~active_accounts:(Ledger.active_count ledger)
        ~head:(Octra_core.Head_manifest.get_cached ()))
  | `GET, "/state-sync/v1/manifest" ->
      handle_manifest ~data_dir ~current_epoch
  | `GET, "/state-sync/v1/head" ->
      handle_head ~current_epoch
  | `POST, "/state-sync/v1/client-progress" ->
      handle_client_progress body
  | `GET, "/state-sync/v1/readiness" ->
      handle_readiness ~data_dir
  | `GET, "/state-sync/v1/range" ->
      handle_range ~data_dir ~chaindata query
  | `GET, "/state-sync/v1/chunk" ->
      handle_chunk ~data_dir query
  | `GET, "/metrics" ->
      respond_json (Metrics.get_metrics ())
  | `GET, "/ws/stats" ->
      respond_json (WSServer.get_client_stats ())
  | `OPTIONS, _ ->
      handle_options ()
  | `GET, "/favicon.ico" ->
      Server.respond_string ~status:`Not_found ~body:"" ()
  | meth, path when Rest_view.legacy_rest_path ~meth:(Code.string_of_method meth) ~path ->
      respond_error `Gone Rest_view.gone_legacy_rest
  | _ ->
      respond_error
        ~error_type:"not_found"
        `Not_found
        (Printf.sprintf "no route: %s %s" (Code.string_of_method (Request.meth req)) path)