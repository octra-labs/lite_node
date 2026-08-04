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

let respond_state_sync_disabled () =
  respond_error
    ~error_type:"state_sync_disabled"
    `Forbidden
    "state sync RPC is disabled"

type loaded_certificate = {
  raw : string;
  certificate : Manifest.certificate;
}

type certificate_cache = {
  path : string;
  inode : int;
  modified : float;
  size : int;
  validator_hash : string;
  checked_at : float;
  outcome : (loaded_certificate, string) result;
}

let certificate_cache = ref None
let negative_cache_seconds = 5.0
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

let read_file_limited path limit =
  let input = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
      let size = in_channel_length input in
      if size > limit then failwith "state sync certificate exceeds size limit";
      really_input_string input size)

let cache_matches cached path stat validator_hash =
  cached.path = path
  && cached.inode = stat.Unix.st_ino
  && cached.modified = stat.Unix.st_mtime
  && cached.size = stat.Unix.st_size
  && cached.validator_hash = validator_hash

let cached_outcome now cached =
  match cached.outcome with
  | Ok loaded ->
      if Manifest.fresh ~now:(Int64.of_float now) loaded.certificate then
        Some (Ok loaded)
      else
        Some (Error "state sync certificate is outside its validity window")
  | Error reason when now -. cached.checked_at <= negative_cache_seconds ->
      Some (Error reason)
  | Error _ -> None

let warn_manifest_unavailable reason =
  let now = Unix.gettimeofday () in
  if now -. !manifest_warning_at >= manifest_warning_seconds then begin
    manifest_warning_at := now;
    Log.warn "state_sync" "event = manifest_unavailable reason = %s" reason
  end

let load_certificate_at ~path ~chain_id =
  match configured_validator_set () with
  | Error _ as error -> error
  | Ok configured_validators ->
      try
        let stat = Unix.stat path in
        if stat.Unix.st_kind <> Unix.S_REG then Error "state sync certificate is not a file"
        else
          let configured_hash =
            Octra_consensus.C_config.validator_set_hash configured_validators
            |> Manifest.raw_to_hex
          in
          match exporter_set (), configured_config_hash () with
          | Error _ as error, _
          | _, (Error _ as error) -> error
          | Ok exporter_set, Ok expected_config ->
              let exporter_hash =
                Octra_consensus.C_config.validator_set_hash exporter_set
                |> Manifest.raw_to_hex
              in
              let cache_hash =
                configured_hash ^ exporter_hash ^ chain_id ^ expected_config
              in
              let now = Unix.gettimeofday () in
              let cached =
                match !certificate_cache with
                | Some cached when cache_matches cached path stat cache_hash ->
                    cached_outcome now cached
                | _ -> None
              in
              begin
                match cached with
                | Some outcome -> outcome
                | None ->
                    let outcome =
                      try
                        let raw = read_file_limited path Manifest.manifest_limit in
                        match Manifest.parse_certificate_string raw with
                        | Error _ as error -> error
                        | Ok certificate ->
                            begin
                              match Manifest.verify_certificate
                                ~validator_set:configured_validators
                                ~exporter_set
                                certificate with
                              | Error _ as error -> error
                              | Ok certificate ->
                                  if certificate.checkpoint.chain_id <> chain_id then
                                    Error "state sync certificate chain mismatch"
                                  else if certificate.checkpoint.config_hash <> expected_config then
                                    Error "state sync certificate config mismatch"
                                  else if not (Manifest.fresh
                                    ~now:(Int64.of_float now)
                                    certificate) then
                                    Error "state sync certificate is outside its validity window"
                                  else
                                    Ok { raw; certificate }
                            end
                      with exn -> Error (Printexc.to_string exn)
                    in
                    certificate_cache := Some {
                      path;
                      inode = stat.Unix.st_ino;
                      modified = stat.Unix.st_mtime;
                      size = stat.Unix.st_size;
                      validator_hash = cache_hash;
                      checked_at = now;
                      outcome;
                    };
                    outcome
              end
      with exn -> Error (Printexc.to_string exn)

let load_certificate ~data_dir ~chain_id ~config_hash:_ ~validator_set:_ =
  match configured_certificate_path ~data_dir with
  | Error _ as error -> error
  | Ok path -> load_certificate_at ~path ~chain_id

let load_snapshot_certificate ~data_dir ~chain_id ~snapshot_id =
  let snapshot = State_sync.snapshot_dir data_dir snapshot_id in
  let archived = State_sync.snapshot_certificate_path snapshot in
  let matches loaded =
    loaded.certificate.Manifest.manifest.snapshot_id = snapshot_id
  in
  match load_certificate_at ~path:archived ~chain_id with
  | Ok loaded when matches loaded -> Ok loaded
  | Ok _ -> Error "state sync snapshot certificate mismatch"
  | Error archived_error ->
      begin
        match configured_certificate_path ~data_dir with
        | Error _ -> Error archived_error
        | Ok current ->
            match load_certificate_at ~path:current ~chain_id with
            | Ok loaded when matches loaded -> Ok loaded
            | Ok _ -> Error "state sync snapshot is not retained"
            | Error _ -> Error archived_error
      end

let respond_certificate cached =
  Server.respond_string
    ~status:`OK
    ~headers:(Header.of_list [
      "Content-Type", "application/json";
      "Cache-Control", "public, max-age=30";
      "X-Octra-Manifest-Sha256", cached.certificate.Manifest.manifest_hash;
    ])
    ~body:cached.raw
    ()

let handle_manifest ~data_dir ~chain_id ~config_hash ~validator_set =
  if not (state_sync_enabled ()) then
    respond_state_sync_disabled ()
  else
    match load_certificate ~data_dir ~chain_id ~config_hash ~validator_set with
    | Error reason ->
        warn_manifest_unavailable reason;
        respond_error
          ~error_type:"state_sync_unavailable"
          `Service_unavailable
          reason
    | Ok cached ->
        respond_certificate cached

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
        | Some (`List values) -> List.length values
        | _ -> 0
      end
  | _ -> 0

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

let handle_range ~data_dir ~chaindata ~chain_id ~validator_set query =
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
      let json =
        State_sync.range_json
          ~chain_id
          ~data_dir:(Some data_dir)
          ~chaindata
          ~reward_source:
            (fun _ header ->
              Consensus_reward_attribution.epoch_source
                ~validator_activation_epoch:
                  (Octra_core.Validator_policy.activation_epoch
                     validator_policy)
                ~validator_pubkeys
                header)
          ~read_finality:
            (fun epoch ->
              match
                Consensus_finality_journal.read_committed_epoch
                  ~chain_id
                  ~epoch:(Int64.of_int epoch)
                  data_dir
              with
              | Consensus_finality_journal.Valid record ->
                Some Octra_consensus.C_codec.{
                  finalize = record.finalize;
                  validator_set = record.validator_set;
                }
              | Consensus_finality_journal.Missing ->
                None
              | Consensus_finality_journal.Invalid reason ->
                Log.error "state_sync"
                  "event = finality_read_failed epoch = %d reason = %s"
                  epoch
                  reason;
                None)
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
    match load_snapshot_certificate ~data_dir ~chain_id ~snapshot_id with
    | Error reason ->
        respond_error
          ~error_type:"state_sync_snapshot_unavailable"
          `Not_found
          reason
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
                    respond_error
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
  | `GET, "/state-sync/manifest" ->
      handle_manifest ~data_dir ~chain_id ~config_hash ~validator_set
  | `GET, "/state-sync/head" ->
      handle_head ~current_epoch
  | `POST, "/state-sync/client-progress" ->
      handle_client_progress body
  | `GET, "/state-sync/readiness" ->
      handle_readiness ~data_dir
  | `GET, "/state-sync/range" ->
      handle_range ~data_dir ~chaindata ~chain_id ~validator_set query
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