(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Infix

module Code = Cohttp.Code
module Header = Cohttp.Header
module Ledger = Octra_core.Ledger
module Metrics = Octra_core.Metrics
module Request = Cohttp.Request
module Server = Cohttp_lwt_unix.Server
module State_sync = Octra_bootstrap.State_sync
module Manifest = Octra_bootstrap.State_sync_manifest
module Tree = Octra_core.Tree

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

let respond_error_after ~seconds ?(error_type = "unknown") status msg =
  let json = Rest_view.error_response ~error_type ~reason:msg in
  let headers =
    Header.add cors_headers "Retry-After" (string_of_int seconds)
  in
  Server.respond_string
    ~status
    ~headers
    ~body:(Yojson.Safe.to_string json)
    ()

let client_progress_body_cap = 65_536

let read_body body =
  let stream = Cohttp_lwt.Body.to_stream body in
  let buf = Buffer.create 4096 in
  let rec loop total =
    Lwt_stream.get stream >>= function
    | None -> Lwt.return_ok (Buffer.contents buf)
    | Some chunk ->
      let size = String.length chunk in
      if size > client_progress_body_cap - total then
        Lwt.return_error "request body too large"
      else begin
        Buffer.add_string buf chunk;
        loop (total + size)
      end
  in
  loop 0

let with_json_body body f =
  read_body body >>= function
  | Error reason ->
      respond_error
        ~error_type:"request_too_large"
        `Request_entity_too_large
        reason
  | Ok body_str ->
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
  || Option.fold
       ~none:false
       ~some:(fun value -> String.trim value <> "")
       (Sys.getenv_opt "OCTRA_STATE_SYNC_EXPORTERS")

let manifest_epoch_limit =
  Int64.min 3_000L (Int64.pred Consensus_finality_journal.history_limit)

let snapshot_epoch_state ~current_epoch ~snapshot_epoch =
  let lag = Int64.max 0L (Int64.sub current_epoch snapshot_epoch) in
  if Int64.compare lag manifest_epoch_limit > 0 then `Old_epoch lag
  else `Ready lag

let committed_epoch current_epoch =
  Int64.max 0L (Int64.pred (Int64.of_int !current_epoch))

let snapshot_epoch_reason ~current_epoch ~snapshot_epoch lag =
  Printf.sprintf
    "state sync snapshot is behind live head snapshot_epoch = %Ld head_epoch = %Ld lag = %Ld limit = %Ld"
    snapshot_epoch
    current_epoch
    lag
    manifest_epoch_limit

let respond_state_sync_disabled () =
  respond_error
    ~error_type:"state_sync_disabled"
    `Forbidden
    "state sync RPC is disabled"

type loaded_certificate = Sync_cert.loaded
let manifest_warning_at = ref 0.0
let manifest_warning_seconds = 30.0
let active_chunk_reads = ref 0

let chunk_read_limit raw =
  match Option.bind raw int_of_string_opt with
  | Some value -> min 64 (max 1 value)
  | None -> 8

let max_active_chunk_reads =
  chunk_read_limit (Sys.getenv_opt "OCTRA_STATE_SYNC_MAX_ACTIVE_CHUNK_READS")

type chunk_read_error =
  | Chunk_busy
  | Chunk_corrupt
  | Chunk_unavailable of string

let load_chunk ~data_dir ~snapshot_id ~path ~offset ~size ~sha256 =
  if !active_chunk_reads >= max_active_chunk_reads then
    Lwt.return_error Chunk_busy
  else begin
    incr active_chunk_reads;
    Lwt.finalize
      (fun () ->
        Lwt_preemptive.detach (fun () ->
          try
            match State_sync.read_chunk
              ~data_dir
              ~snapshot_id:(Some snapshot_id)
              ~rel:path
              ~offset
              ~len:size with
            | Error reason -> Error (Chunk_unavailable reason)
            | Ok (payload, _) ->
                let actual = Digestif.SHA256.(digest_string payload |> to_hex) in
                if String.length payload <> size || actual <> sha256 then
                  Error Chunk_corrupt
                else
                  Ok payload
          with _ ->
            Error (Chunk_unavailable "snapshot read failed")
        ) ())
      (fun () ->
        decr active_chunk_reads;
        Lwt.return_unit)
  end

let certificate_path ~data_dir =
  match Sys.getenv_opt "OCTRA_STATE_SYNC_CERT" with
  | Some path when String.trim path <> "" -> String.trim path
  | _ -> Filename.concat data_dir "state_sync/certificate.json"

let configured_certificate_path ~data_dir =
  Ok (certificate_path ~data_dir)

let exporter_set () =
  match Sys.getenv_opt "OCTRA_STATE_SYNC_EXPORTERS" with
  | None -> Error "state sync exporters are not configured"
  | Some value ->
      value
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.filter (fun entry -> entry <> "")
      |> Manifest.validator_set_of_entries

let configured_validator_set () =
  match Sys.getenv_opt "OCTRA_VALIDATORS" with
  | None -> Error "state sync validators are not configured"
  | Some value ->
      value
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.filter (fun entry -> entry <> "")
      |> Manifest.validator_set_of_entries

let hash_hex value =
  if String.length value = 32 then Ok (Manifest.raw_to_hex value)
  else if Manifest.is_lower_hex_64 value then Ok value
  else Error "consensus config hash has invalid encoding"

let configured_config_hash () =
  match Sys.getenv_opt "OCTRA_CONSENSUS_CONFIG_HASH" with
  | Some value -> hash_hex (String.trim value)
  | None -> Error "state sync config hash is not configured"

let certificate_stat left right =
  left.Unix.st_kind = Unix.S_REG && right.Unix.st_kind = Unix.S_REG
  && left.st_dev = right.st_dev && left.st_ino = right.st_ino
  && left.st_size = right.st_size && left.st_mtime = right.st_mtime
  && left.st_ctime = right.st_ctime

let read_certificate path =
  Lwt_preemptive.detach (fun () ->
    try
      let descriptor = Unix.openfile path [Unix.O_RDONLY; Unix.O_NONBLOCK] 0 in
      let input = Unix.in_channel_of_descr descriptor in
      Fun.protect ~finally:(fun () -> close_in_noerr input) (fun () ->
        let before = Unix.fstat descriptor in
        if before.st_kind <> Unix.S_REG then Error "state sync certificate is not a file"
        else if before.st_size > Manifest.manifest_limit then
          Error "state sync certificate exceeds size limit"
        else
          let raw = really_input_string input before.st_size in
          if certificate_stat before (Unix.fstat descriptor)
             && certificate_stat before (Unix.stat path) then Ok raw
          else Error "state sync certificate changed during read")
    with _ -> Error "state sync certificate read failed") ()

let verify_certificate ~cancelled (trust : Sync_cert.trust) raw =
  Lwt_preemptive.detach Manifest.parse_certificate_string raw >>= function
  | Error reason -> Lwt.return_error reason
  | Ok certificate ->
    Manifest.verify_certificate_lwt ~cancelled ~validator_set:trust.validators
      ~exporter_set:trust.exporters certificate >|= function
    | Error reason -> Error reason
    | Ok certificate ->
      if certificate.checkpoint.chain_id <> trust.chain then
        Error "state sync certificate chain mismatch"
      else if certificate.checkpoint.config_hash <> trust.config then
        Error "state sync certificate config mismatch"
      else Ok certificate

let certificate_actor = lazy (
  let clock = Mtime_clock.counter () in
  Sync_cert.create {
    now = (fun () -> Mtime.Span.to_float_ns (Mtime_clock.count clock) /. 1e9);
    read = read_certificate;
    verify = verify_certificate;
  })

let shutdown () =
  if Lazy.is_val certificate_actor then Sync_cert.shutdown (Lazy.force certificate_actor)
  else Lwt.return_unit

let warn_manifest_unavailable reason =
  let now = Unix.gettimeofday () in
  if now -. !manifest_warning_at >= manifest_warning_seconds then begin
    manifest_warning_at := now;
    Log.warn "state_sync" "event = manifest_unavailable reason = %s" reason
  end

let load_certificate_at ~path ~chain_id =
  let configured () =
    match configured_validator_set (), exporter_set (), configured_config_hash () with
    | Ok validators, Ok exporters, Ok config ->
      Ok Sync_cert.{ validators; exporters; config; chain = chain_id }
    | Error reason, _, _ | _, Error reason, _ | _, _, Error reason -> Error (Sync_cert.Invalid reason)
  in
  match configured () with
  | Error reason -> Lwt.return_error reason
  | Ok trust ->
    Sync_cert.load (Lazy.force certificate_actor) ~path trust >|= function
    | Error reason -> Error reason
    | Ok loaded ->
      match configured () with
      | Error reason -> Error reason
      | Ok current when Sync_cert.trust_hash current <> Sync_cert.trust_hash trust ->
        Error (Sync_cert.Invalid "state sync trust changed during verification")
      | Ok _ ->
        if Manifest.fresh ~now:(Int64.of_float (Unix.gettimeofday ())) loaded.certificate then
          Ok loaded
        else Error (Sync_cert.Invalid "state sync certificate is outside its validity window")

let load_certificate ~data_dir ~chain_id ~config_hash:_ ~validator_set:_ =
  match configured_certificate_path ~data_dir with
  | Error reason -> Lwt.return_error (Sync_cert.Invalid reason)
  | Ok path -> load_certificate_at ~path ~chain_id

let load_snapshot_certificate ~data_dir ~chain_id ~snapshot_id =
  let snapshot = State_sync.snapshot_dir data_dir snapshot_id in
  let archived = State_sync.snapshot_certificate_path snapshot in
  let matches (loaded : loaded_certificate) =
    loaded.certificate.Manifest.manifest.snapshot_id = snapshot_id
  in
  load_certificate_at ~path:archived ~chain_id >>= function
  | Ok loaded when matches loaded -> Lwt.return_ok loaded
  | Ok _ -> Lwt.return_error (Sync_cert.Invalid "state sync snapshot certificate mismatch")
  | Error reason when Sync_cert.retryable reason -> Lwt.return_error reason
  | Error archived_error ->
      begin
        match configured_certificate_path ~data_dir with
        | Error _ -> Lwt.return_error archived_error
        | Ok current ->
            load_certificate_at ~path:current ~chain_id >|= function
            | Ok loaded when matches loaded -> Ok loaded
            | Ok _ -> Error (Sync_cert.Invalid "state sync snapshot is not retained")
            | Error reason when Sync_cert.retryable reason -> Error reason
            | Error _ -> Error archived_error
      end

let certificate_error ~error_type status error =
  let reason = Sync_cert.reason error in
  if Sync_cert.retryable error then
    respond_error_after ~seconds:1 ~error_type:"state_sync_busy" `Too_many_requests reason
  else respond_error ~error_type status reason

let respond_certificate (cached : loaded_certificate) =
  Server.respond_string
    ~status:`OK
    ~headers:(Header.of_list [
      "Content-Type", "application/json";
      "Cache-Control", "public, max-age=30";
      "X-Octra-Manifest-Sha256", cached.certificate.Manifest.manifest_hash;
    ])
    ~body:cached.raw
    ()

let handle_manifest ~data_dir ~chain_id ~config_hash ~validator_set ~current_epoch =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    load_certificate ~data_dir ~chain_id ~config_hash ~validator_set >>= function
    | Error reason ->
        warn_manifest_unavailable (Sync_cert.reason reason);
        certificate_error ~error_type:"state_sync_unavailable" `Service_unavailable reason
    | Ok cached ->
        let head_epoch = committed_epoch current_epoch in
        let snapshot_epoch = cached.certificate.Manifest.checkpoint.epoch in
        begin
          match snapshot_epoch_state ~current_epoch:head_epoch ~snapshot_epoch with
          | `Ready _ -> respond_certificate cached
          | `Old_epoch lag ->
              let reason =
                snapshot_epoch_reason ~current_epoch:head_epoch ~snapshot_epoch lag
              in
              warn_manifest_unavailable reason;
              respond_error
                ~error_type:"state_sync_old_epoch"
                `Service_unavailable
                reason
        end

let handle_head ~data_dir ~chain_id ~config_hash ~validator_set ~current_epoch =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    load_certificate ~data_dir ~chain_id ~config_hash ~validator_set >>= function
    | Error reason when Sync_cert.retryable reason ->
      certificate_error ~error_type:"state_sync_unavailable" `Service_unavailable reason
    | result ->
      let snapshot_epoch = Result.to_option result
        |> Option.map (fun cached -> cached.Sync_cert.certificate.Manifest.checkpoint.epoch) in
      let head_epoch = committed_epoch current_epoch in
      let snapshot_status, snapshot_lag =
        match snapshot_epoch with
        | None -> "missing", `Null
        | Some epoch ->
            begin
              match snapshot_epoch_state ~current_epoch:head_epoch ~snapshot_epoch:epoch with
              | `Ready lag -> "ready", `Intlit (Int64.to_string lag)
              | `Old_epoch lag -> "old_epoch", `Intlit (Int64.to_string lag)
            end
      in
      let response =
        match State_sync.head_json ~current_epoch ~snapshot_epoch with
        | `Assoc fields ->
            `Assoc (fields @ [
              "snapshot_status", `String snapshot_status;
              "snapshot_lag", snapshot_lag;
              "snapshot_lag_limit", `Intlit (Int64.to_string manifest_epoch_limit);
            ])
        | other -> other
      in
      respond_json response

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

let handle_range ~ranges ~validator_set query =
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
    let part =
      match query_param query "part" with
      | None -> None
      | Some s -> Some (try int_of_string s with _ -> -1)
    in
    let part_valid =
      match part with
      | None -> true
      | Some index -> index >= 0
    in
    let hash = query_param query "sha256" in
    let hash_valid = match hash with
      | None -> true
      | Some value -> part <> None && String.length value = 64
          && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) value in
    if Int64.compare from_epoch 0L < 0
       || from_epoch > Int64.sub (Int64.of_int max_int) 16L
       || max_epochs <= 0 || not part_valid || not hash_valid then
      respond_error
        ~error_type:"state_sync_bad_range_request"
        `Bad_request
        "invalid range parameters"
    else
      let validator_pubkeys =
        List.map
          (fun validator ->
            validator.Octra_consensus.C_types.address,
            Base64.encode_exn validator.pubkey)
          validator_set.Octra_consensus.C_types.validators
      in
      let validator_policy =
        Octra_core.Validator_policy.of_env_exn Sys.getenv_opt
      in
      Sync_range.load ranges {
        from_epoch; max_epochs; part; hash;
        head = Octra_core.Head_manifest.get_cached ();
        pubkeys = validator_pubkeys;
        activation = Octra_core.Validator_policy.activation_epoch validator_policy;
      } >>= function
      | Ok reply ->
        Log.info "state_sync"
          "range from_epoch = %Ld max_epochs = %d part = %d status = %s records = %d"
          from_epoch max_epochs (Option.value ~default:(-1) part) reply.status reply.records;
        Server.respond_string ~status:`OK ~headers:cors_headers ~body:reply.body ()
      | Error (Sync_range.Invalid reason) ->
        respond_error ~error_type:"state_sync_range_part" `Bad_request reason
      | Error error ->
        let reason = match error with
          | Sync_range.Busy -> "range read busy"
          | Sync_range.Expired -> "range read expired"
          | Sync_range.Stopped -> "range read stopped"
          | Sync_range.Changed -> "range head changed"
          | Sync_range.Missing -> "range response unavailable"
          | Sync_range.Invalid reason -> reason in
        respond_error_after ~seconds:2 ~error_type:"state_sync_range_busy"
          `Service_unavailable reason

let int_query_param query name default_value =
  match query_param query name with
  | Some s -> (try int_of_string s with _ -> default_value)
  | None -> default_value

let handle_chunk ~data_dir ~chain_id ~config_hash:_ ~validator_set:_ query =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    let path = Option.value ~default:"" (query_param query "path") in
    let index = int_query_param query "index" (-1) in
    let sha256 = Option.value ~default:"" (query_param query "sha256") in
    let snapshot_id = Option.value ~default:"" (query_param query "snapshot") in
    let query_valid =
      index >= 0
      && Manifest.is_lower_hex_64 sha256
      && Manifest.valid_id snapshot_id
      && (match Manifest.normalize_path path with
        | Some normalized -> normalized = path
        | None -> false)
    in
    if not query_valid then
      respond_error
        ~error_type:"state_sync_bad_chunk_request"
        `Bad_request
        "invalid chunk request"
    else
    let snapshot = State_sync.snapshot_dir data_dir snapshot_id in
    if not (Sys.file_exists snapshot) then
      respond_error
        ~error_type:"state_sync_snapshot_expired"
        `Gone
        "state sync snapshot expired; restart with the current manifest"
    else
    load_snapshot_certificate ~data_dir ~chain_id ~snapshot_id >>= function
    | Error reason ->
        certificate_error ~error_type:"state_sync_snapshot_unavailable" `Not_found reason
    | Ok cached ->
        let certificate = cached.certificate in
        let body = certificate.Manifest.manifest in
        if snapshot_id <> body.snapshot_id then
          respond_error
                ~error_type:"state_sync_snapshot_mismatch"
            `Bad_request
            "snapshot id mismatch"
        else
          match Manifest.find_chunk body ~path ~index ~sha256 with
          | Error reason ->
              respond_error
                ~error_type:"state_sync_bad_chunk_request"
                `Bad_request
                reason
          | Ok chunk ->
              if Int64.compare chunk.offset (Int64.of_int max_int) > 0 then
                respond_error
                  ~error_type:"state_sync_bad_chunk_request"
                  `Bad_request
                  "chunk offset exceeds platform limit"
              else
                let offset = Int64.to_int chunk.offset in
                load_chunk
                  ~data_dir
                  ~snapshot_id:body.snapshot_id
                  ~path
                  ~offset
                  ~size:chunk.size
                  ~sha256:chunk.sha256 >>= function
                | Error Chunk_busy ->
                    respond_error_after
                      ~seconds:3
                      ~error_type:"state_sync_busy"
                      `Too_many_requests
                      "state sync source is busy"
                | Error (Chunk_unavailable reason) ->
                    respond_error
                      ~error_type:"state_sync_chunk_unavailable"
                      `Service_unavailable
                      reason
                | Error Chunk_corrupt ->
                    Log.error "state_sync"
                      "event = chunk_corrupt snapshot = %s path = %s index = %d"
                      body.snapshot_id
                      path
                      index;
                    respond_error
                      ~error_type:"state_sync_source_corrupt"
                      `Internal_server_error
                      "snapshot chunk failed local integrity check"
                | Ok payload ->
                    begin
                      match Sync_lease.renew ~now:(Unix.gettimeofday ()) snapshot with
                      | Ok () -> ()
                      | Error reason ->
                        Log.warn "state_sync"
                          "event = lease_renew_failed snapshot = %s reason = %s"
                          body.snapshot_id
                          reason
                    end;
                    Server.respond_string
                      ~status:`OK
                      ~headers:(octet_stream_headers [
                        "Cache-Control", "public, max-age=31536000, immutable";
                        "X-Octra-Chunk-Sha256", chunk.sha256;
                        "X-Octra-Manifest-Sha256", certificate.manifest_hash;
                        "X-Octra-Chunk-Index", string_of_int chunk.index;
                        "X-Octra-Snapshot", body.snapshot_id;
                      ])
                      ~body:payload
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
    ~chain_id
    ~config_hash
    ~validator_set
    ~current_epoch
    ~ranges
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
  | `GET, "/state-sync/manifest" ->
      handle_manifest
        ~data_dir
        ~chain_id
        ~config_hash
        ~validator_set
        ~current_epoch
  | `GET, "/state-sync/head" ->
      handle_head ~data_dir ~chain_id ~config_hash ~validator_set ~current_epoch
  | `POST, "/state-sync/client-progress" ->
      handle_client_progress body
  | `GET, "/state-sync/readiness" ->
      handle_readiness ~data_dir
  | `GET, "/state-sync/range" ->
      handle_range ~ranges ~validator_set query
  | `GET, "/state-sync/chunk" ->
      handle_chunk ~data_dir ~chain_id ~config_hash ~validator_set query
  | `GET, "/metrics" ->
      respond_json (Metrics.get_metrics ())
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