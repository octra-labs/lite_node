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


open Lwt.Syntax

type export_info = {
  name : string;
  kind : string;
}

type descriptor = {
  exports : export_info list;
  manifest : Yojson.Safe.t option;
}

type wasm_event = {
  topic : string;
  data : string;
}

type spawn = {
  circle_id : string;
  owner_mode : Circles.spawn_owner;
  spawn_nonce : int;
  payload_json : string;
}

type asset_put = {
  circle_id : string;
  path : string;
  content_type : string;
  encoding : string option;
  body_b64 : string;
}

type encrypted_asset_put = {
  circle_id : string;
  path : string option;
  slot_ref : string option;
  state_ref : string option;
  content_type : string;
  encoding : string option;
  key_id : string;
  plaintext_hash : string;
  padding_class : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  metadata_mode : Circles.metadata_mode option;
  ciphertext_b64 : string;
}

type exec_result = {
  success : bool;
  response_value : Circle_wasm_codec.response option;
  response_status : int;
  storage_tbl : (string, string) Hashtbl.t;
  events : wasm_event list;
  spawns : spawn list;
  assets : asset_put list;
  encrypted_assets : encrypted_asset_put list;
  error : string option;
}

type storage_cache_entry = {
  storage_json : Yojson.Safe.t list;
  mutable seeded_in_native : bool;
}

let storage_payload_cache : (string, storage_cache_entry) Hashtbl.t =
  Hashtbl.create 8

let code_seed_cache : (string, bool) Hashtbl.t =
  Hashtbl.create 8

let code_cache_key code_b64 =
  Digestif.SHA256.(to_hex (digest_string code_b64))

let string_starts_with ~prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len
  && String.sub value 0 prefix_len = prefix

let decode_b64_field name = function
  | `String value ->
    begin
      try
        Ok (Base64.decode_exn value)
      with e ->
        Error
          (Printf.sprintf
             "invalid %s base64: %s"
             name
             (Printexc.to_string e))
    end
  | _ ->
    Error (Printf.sprintf "invalid %s encoding" name)

let read_process_json payload =
  Circle_wasm_hfhe_backend.ensure_registered ();
  let body = Yojson.Safe.to_string payload in
  match Circle_wasm_native.run_json body with
  | Error e ->
    Lwt.return (Error e)
  | Ok raw ->
    begin
      try
        Lwt.return (Ok (Yojson.Safe.from_string raw))
      with e ->
        Lwt.return
          (Error
             (Printf.sprintf
                "invalid native wasm runtime response: %s"
                (Printexc.to_string e)))
    end

let export_info_of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "name" fields, List.assoc_opt "kind" fields with
      | Some (`String name), Some (`String kind) ->
        Ok { name; kind }
      | _ ->
        Error "invalid wasm export descriptor"
    end
  | _ ->
    Error "invalid wasm export descriptor"

let descriptor_of_yojson = function
  | `Assoc fields ->
    let exports =
      match List.assoc_opt "exports" fields with
      | Some (`List values) ->
        let parsed = List.map export_info_of_yojson values in
        begin
          match List.find_opt (function Error _ -> true | Ok _ -> false) parsed with
          | Some (Error e) -> Error e
          | _ ->
            Ok
              (List.filter_map
                 (function
                   | Ok value -> Some value
                   | Error _ -> None)
                 parsed)
        end
      | _ ->
        Ok []
    in
    begin
      match exports with
      | Error e -> Error e
      | Ok exports ->
        let manifest =
          match List.assoc_opt "manifest" fields with
          | Some `Null | None -> None
          | Some json -> Some json in
        Ok { exports; manifest }
    end
  | _ ->
    Error "invalid wasm descriptor"

let make_storage_pairs_json storage_tbl =
  Hashtbl.fold
    (fun key value acc ->
      (`Assoc [
         "key_b64", `String (Base64.encode_exn key);
         "value_b64", `String (Base64.encode_exn value);
       ]) :: acc)
    storage_tbl
    []
  |> List.rev

let cached_storage_entry storage_cache_key storage_tbl =
  match Hashtbl.find_opt storage_payload_cache storage_cache_key with
  | Some entry -> entry
  | None ->
    let entry =
      {
        storage_json = make_storage_pairs_json storage_tbl;
        seeded_in_native = false;
      } in
    Hashtbl.replace storage_payload_cache storage_cache_key entry;
    entry

let decode_storage_tbl = function
  | `List items ->
    let tbl = Hashtbl.create (max 8 (List.length items)) in
    let rec loop = function
      | [] -> Ok tbl
      | (`Assoc fields) :: rest ->
        begin
          match List.assoc_opt "key_b64" fields, List.assoc_opt "value_b64" fields with
          | Some key_json, Some value_json ->
            begin
              match decode_b64_field "storage key" key_json, decode_b64_field "storage value" value_json with
              | Ok key, Ok value ->
                Hashtbl.replace tbl key value;
                loop rest
              | Error e, _
              | _, Error e ->
                Error e
            end
          | _ ->
            Error "invalid wasm storage pair"
        end
      | _ :: _ ->
        Error "invalid wasm storage pair"
    in
    loop items
  | _ ->
    Error "invalid wasm storage table"

let decode_events = function
  | `List items ->
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | (`Assoc fields) :: rest ->
        begin
          match List.assoc_opt "topic_b64" fields, List.assoc_opt "data_b64" fields with
          | Some topic_json, Some data_json ->
            begin
              match decode_b64_field "event topic" topic_json, decode_b64_field "event data" data_json with
              | Ok topic, Ok data ->
                loop ({ topic; data } :: acc) rest
              | Error e, _
              | _, Error e ->
                Error e
            end
          | _ ->
            Error "invalid wasm event"
        end
      | _ :: _ ->
        Error "invalid wasm event"
    in
    loop [] items
  | _ ->
    Error "invalid wasm events"

let decode_spawn = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "circle_id" fields,
        List.assoc_opt "owner_mode" fields,
        List.assoc_opt "spawn_nonce" fields,
        List.assoc_opt "payload_json" fields
      with
      | Some (`String circle_id),
        Some (`String owner_mode_s),
        Some (`Int spawn_nonce),
        Some (`String payload_json) ->
        begin
          match Circles.spawn_owner_of_string owner_mode_s with
          | Ok owner_mode ->
            Ok {
              circle_id;
              owner_mode;
              spawn_nonce;
              payload_json;
            }
          | Error e ->
            Error e
        end
      | _ ->
        Error "invalid wasm spawn effect"
    end
  | _ ->
    Error "invalid wasm spawn effect"

let decode_spawns = function
  | `List items ->
    let parsed = List.map decode_spawn items in
    begin
      match List.find_opt (function Error _ -> true | Ok _ -> false) parsed with
      | Some (Error e) -> Error e
      | _ ->
        Ok
          (List.filter_map
             (function
               | Ok value -> Some value
               | Error _ -> None)
             parsed)
    end
  | `Null ->
    Ok []
  | _ ->
    Error "invalid wasm spawns"

let decode_asset_put = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "circle_id" fields,
        List.assoc_opt "path" fields,
        List.assoc_opt "content_type" fields,
        List.assoc_opt "encoding" fields,
        List.assoc_opt "body_b64" fields
      with
      | Some (`String circle_id),
        Some (`String path),
        Some (`String content_type),
        encoding_json,
        Some (`String body_b64) ->
        let encoding =
          match encoding_json with
          | Some (`String value) -> Some value
          | Some `Null | None -> None
          | _ -> Some "__invalid__"
        in
        if encoding = Some "__invalid__" then
          Error "invalid wasm asset effect"
        else
          Ok {
            circle_id;
            path;
            content_type;
            encoding;
            body_b64;
          }
      | _ ->
        Error "invalid wasm asset effect"
    end
  | _ ->
    Error "invalid wasm asset effect"

let decode_assets = function
  | `List items ->
    let parsed = List.map decode_asset_put items in
    begin
      match List.find_opt (function Error _ -> true | Ok _ -> false) parsed with
      | Some (Error e) -> Error e
      | _ ->
        Ok
          (List.filter_map
             (function
               | Ok value -> Some value
               | Error _ -> None)
             parsed)
    end
  | `Null ->
    Ok []
  | _ ->
    Error "invalid wasm asset effects"

let decode_optional_string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok (Some value)
  | Some `Null | None -> Ok None
  | _ -> Error ("invalid wasm encrypted asset effect")

let decode_optional_i64_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) ->
    begin
      try Ok (Some (Int64.of_string value))
      with _ -> Error ("invalid wasm encrypted asset effect")
    end
  | Some (`Int value) -> Ok (Some (Int64.of_int value))
  | Some (`Intlit value) ->
    begin
      try Ok (Some (Int64.of_string value))
      with _ -> Error ("invalid wasm encrypted asset effect")
    end
  | Some `Null | None -> Ok None
  | _ -> Error ("invalid wasm encrypted asset effect")

let decode_encrypted_asset_put = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "circle_id" fields,
        decode_optional_string_field "path" fields,
        decode_optional_string_field "slot_ref" fields,
        decode_optional_string_field "state_ref" fields,
        List.assoc_opt "content_type" fields,
        decode_optional_string_field "encoding" fields,
        List.assoc_opt "key_id" fields,
        List.assoc_opt "plaintext_hash" fields,
        decode_optional_string_field "padding_class" fields,
        decode_optional_i64_field "activate_after_epoch" fields,
        decode_optional_i64_field "expire_after_epoch" fields,
        decode_optional_string_field "metadata_mode" fields,
        List.assoc_opt "ciphertext_b64" fields
      with
      | Some (`String circle_id),
        Ok path,
        Ok slot_ref,
        Ok state_ref,
        Some (`String content_type),
        Ok encoding,
        Some (`String key_id),
        Some (`String plaintext_hash),
        Ok padding_class,
        Ok activate_after_epoch,
        Ok expire_after_epoch,
        Ok metadata_mode_s,
        Some (`String ciphertext_b64) ->
        let metadata_mode =
          match metadata_mode_s with
          | Some raw_mode ->
            begin
              match Circles.metadata_mode_of_string raw_mode with
              | Ok mode -> Ok (Some mode)
              | Error e -> Error e
            end
          | None -> Ok None
        in
        begin
          match metadata_mode with
          | Ok metadata_mode ->
            Ok {
              circle_id;
              path;
              slot_ref;
              state_ref;
              content_type;
              encoding;
              key_id;
              plaintext_hash;
              padding_class;
              activate_after_epoch;
              expire_after_epoch;
              metadata_mode;
              ciphertext_b64;
            }
          | Error e ->
            Error e
        end
      | _ ->
        Error "invalid wasm encrypted asset effect"
    end
  | _ ->
    Error "invalid wasm encrypted asset effect"

let decode_encrypted_assets = function
  | `List items ->
    let parsed = List.map decode_encrypted_asset_put items in
    begin
      match List.find_opt (function Error _ -> true | Ok _ -> false) parsed with
      | Some (Error e) -> Error e
      | _ ->
        Ok
          (List.filter_map
             (function
               | Ok value -> Some value
               | Error _ -> None)
             parsed)
    end
  | `Null ->
    Ok []
  | _ ->
    Error "invalid wasm encrypted asset effects"

let validate code_b64 =
  let payload =
    `Assoc [
      "action", `String "validate";
      "code_b64", `String code_b64;
    ] in
  let* json_result = read_process_json payload in
  match json_result with
  | Error _ as e ->
    Lwt.return e
  | Ok json ->
    Lwt.return (descriptor_of_yojson json)

let describe code_b64 =
  let payload =
    `Assoc [
      "action", `String "describe";
      "code_b64", `String code_b64;
    ] in
  let* json_result = read_process_json payload in
  match json_result with
  | Error _ as e ->
    Lwt.return e
  | Ok json ->
    Lwt.return (descriptor_of_yojson json)

let execute
    ~code_b64
    ~export_name
    ~request_bytes
    ~storage_tbl
    ~storage_cache_key
    ~caller
    ~address
    ~tx_hash
    ~current_epoch
    ~hfhe_caps
    ~hfhe_pubkeys
    ~hfhe_active_key
    ~is_view =
  let code_key = code_cache_key code_b64 in
  let initial_allow_code_cache_only =
    match Hashtbl.find_opt code_seed_cache code_key with
    | Some true -> true
    | _ -> false in
  let initial_allow_cache_only =
    match storage_cache_key with
    | Some key ->
      begin
        match Hashtbl.find_opt storage_payload_cache key with
        | Some entry -> entry.seeded_in_native
        | None -> false
      end
    | None -> false in
  let rec dispatch ~allow_cache_only ~allow_code_cache_only =
    let storage_entry =
      Option.map (fun key -> cached_storage_entry key storage_tbl) storage_cache_key in
    let use_cache_only =
      allow_cache_only
      && Option.value
           ~default:false
           (Option.map (fun entry -> entry.seeded_in_native) storage_entry) in
    let use_code_cache_only =
      allow_code_cache_only
      && Option.value ~default:false (Hashtbl.find_opt code_seed_cache code_key) in
    let storage_json =
      if use_cache_only then
        `Null
      else
        match storage_entry with
        | Some entry -> `List entry.storage_json
        | None -> `List (make_storage_pairs_json storage_tbl) in
    let storage_cache_key_json =
      if use_cache_only then
        match storage_cache_key with
        | Some key -> `String key
        | None -> `Null
      else
        match storage_json, storage_cache_key with
        | `List [], _ -> `Null
        | _, Some key -> `String key
        | _, None -> `Null in
    let payload =
      `Assoc [
        "action", `String "execute";
        "code_cache_key", `String code_key;
        "code_b64",
        begin
          if use_code_cache_only then `Null else `String code_b64
        end;
        "export_name", `String export_name;
        "request_b64", `String (Base64.encode_exn request_bytes);
        "storage_cache_key", storage_cache_key_json;
        "storage_pairs", storage_json;
        "caller", `String caller;
        "address", `String address;
        "tx_hash", `String tx_hash;
        "current_epoch", `Int current_epoch;
        "hfhe_caps", `List (List.map (fun value -> `String value) hfhe_caps);
        "hfhe_pubkeys",
        `List
          (List.map
             (fun (addr, pubkey_b64) ->
               `Assoc [
                 "addr", `String addr;
                 "pubkey_b64", `String pubkey_b64;
               ])
             hfhe_pubkeys);
        "hfhe_active_key",
        begin
          match hfhe_active_key with
          | Some (key_id, pubkey_b64, seckey_b64) ->
            `Assoc [
              "key_id", `String key_id;
              "pubkey_b64", `String pubkey_b64;
              "seckey_b64", `String seckey_b64;
            ]
          | None ->
            `Null
        end;
        "is_view", `Bool is_view;
      ] in
    let* json_result = read_process_json payload in
    match json_result with
    | Error e
      when use_code_cache_only
           && string_starts_with ~prefix:"missing wasm module cache:" e ->
      dispatch ~allow_cache_only ~allow_code_cache_only:false
    | Error e
      when use_cache_only
           && string_starts_with ~prefix:"missing storage cache:" e ->
      dispatch ~allow_cache_only:false ~allow_code_cache_only
    | Error _ as e ->
      Lwt.return e
    | Ok _ as ok ->
      begin
        if not use_code_cache_only then
          Hashtbl.replace code_seed_cache code_key true;
        match storage_entry with
        | Some entry when not use_cache_only ->
          entry.seeded_in_native <- true
        | _ ->
          ()
      end;
      Lwt.return ok in
  let* json_result =
    dispatch
      ~allow_cache_only:initial_allow_cache_only
      ~allow_code_cache_only:initial_allow_code_cache_only
  in
  match json_result with
  | Error _ as e ->
    Lwt.return e
  | Ok (`Assoc fields) ->
    begin
      match
        List.assoc_opt "success" fields,
        List.assoc_opt "status_code" fields,
        List.assoc_opt "response_b64" fields,
        List.assoc_opt "storage_pairs" fields,
        List.assoc_opt "events" fields
      with
      | Some (`Bool success),
        Some (`Int status_code),
        Some response_json,
        Some storage_json,
        Some events_json ->
        begin
          let storage_decode_result =
            match storage_json with
            | `Null ->
              Ok (Hashtbl.copy storage_tbl)
            | _ ->
              decode_storage_tbl storage_json in
          let spawns_json =
            match List.assoc_opt "spawns" fields with
            | Some value -> value
            | None -> `Null
          in
          let assets_json =
            match List.assoc_opt "assets" fields with
            | Some value -> value
            | None -> `Null
          in
          let encrypted_assets_json =
            match List.assoc_opt "encrypted_assets" fields with
            | Some value -> value
            | None -> `Null
          in
          match
            storage_decode_result,
            decode_events events_json,
            decode_spawns spawns_json,
            decode_assets assets_json,
            decode_encrypted_assets encrypted_assets_json,
            (match response_json with
             | `Null -> Ok None
             | _ ->
               begin
                 match decode_b64_field "wasm response" response_json with
                 | Ok raw ->
                   begin
                     match Circle_wasm_codec.decode_response raw with
                     | Ok value -> Ok (Some value)
                     | Error e -> Error e
                   end
                 | Error e -> Error e
               end)
          with
          | Ok storage_tbl, Ok events, Ok spawns, Ok assets, Ok encrypted_assets, Ok response_value ->
            let error =
              match List.assoc_opt "error" fields with
              | Some (`String msg) -> Some msg
              | _ -> None in
            Lwt.return
              (Ok {
                 success;
                 response_value;
                 response_status = status_code;
                 storage_tbl;
                 events;
                 spawns;
                 assets;
                 encrypted_assets;
                 error;
               })
          | Error e, _, _, _, _, _
          | _, Error e, _, _, _, _
          | _, _, Error e, _, _, _
          | _, _, _, Error e, _, _
          | _, _, _, _, Error e, _
          | _, _, _, _, _, Error e ->
            Lwt.return (Error e)
        end
      | _ ->
        Lwt.return (Error "invalid wasm execution response")
    end
  | Ok _ ->
    Lwt.return (Error "invalid wasm execution response")