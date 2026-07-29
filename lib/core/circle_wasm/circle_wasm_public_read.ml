(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

type declaration = {
  method_name : string;
  circle_id : string;
  canonical_path : string;
  path_key : string;
  offset : int;
  max_bytes : int;
}

type snapshot = {
  snapshot_root : string;
  circle_id : string;
  canonical_path : string;
  offset : int;
  max_bytes : int;
  metadata_json : string;
  body : string;
}

type bundle = {
  snapshots : snapshot list;
  effort_used : int;
}

type policy = {
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  tombstone : bool;
  revoked : bool;
}

let max_reads_per_method = 16
let max_unique_assets = 4
let max_range_bytes = 65_536
let max_total_bytes = 262_144
let max_source_bytes = 1_048_576
let base_effort = 2_000
let byte_effort = 4

let is_hex_char = function
  | '0' .. '9'
  | 'a' .. 'f' -> true
  | _ -> false

let is_hash value =
  String.length value = 64
  && String.for_all is_hex_char value

let int_field name = function
  | `Int value -> Ok value
  | `Intlit value
  | `String value ->
    begin
      try Ok (int_of_string value)
      with _ -> Error ("invalid wasm public read field: " ^ name)
    end
  | _ ->
    Error ("invalid wasm public read field: " ^ name)

let declaration_of_yojson method_name = function
  | `Assoc fields ->
    begin
      match
        List.assoc_opt "circle_id" fields,
        List.assoc_opt "path" fields,
        List.assoc_opt "offset" fields,
        List.assoc_opt "max_bytes" fields
      with
      | Some (`String circle_id), Some (`String path), Some offset_json, Some max_bytes_json ->
        begin
          match
            Circles.path_key_of_raw_path path,
            int_field "offset" offset_json,
            int_field "max_bytes" max_bytes_json
          with
          | Ok (canonical_path, path_key), Ok offset, Ok max_bytes
            when String.equal canonical_path path
                 && Crypto.Address.is_valid_address circle_id
                 && offset >= 0
                 && max_bytes > 0
                 && max_bytes <= max_range_bytes ->
            Ok {
              method_name;
              circle_id;
              canonical_path;
              path_key;
              offset;
              max_bytes;
            }
          | Ok _, Ok _, Ok _ ->
            Error "invalid wasm public read declaration"
          | Error e, _, _
          | _, Error e, _
          | _, _, Error e ->
            Error e
        end
      | _ ->
        Error "invalid wasm public read declaration"
    end
  | _ ->
    Error "invalid wasm public read declaration"

let declarations_of_method = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "name" fields, List.assoc_opt "public_reads" fields with
      | Some (`String method_name), Some (`List entries) ->
        if List.length entries > max_reads_per_method then
          Error "wasm public read count exceeds limit"
        else
          let rec loop acc = function
            | [] ->
              let declarations = List.rev acc in
              let total =
                List.fold_left
                  (fun sum (declaration : declaration) ->
                    sum + declaration.max_bytes)
                  0
                  declarations in
              let unique_assets =
                declarations
                |> List.map
                     (fun (declaration : declaration) ->
                       declaration.circle_id ^ "\000" ^ declaration.canonical_path)
                |> List.sort_uniq String.compare
                |> List.length in
              if total > max_total_bytes || unique_assets > max_unique_assets then
                Error "wasm public read set exceeds limit"
              else
                Ok declarations
            | entry :: rest ->
              begin
                match declaration_of_yojson method_name entry with
                | Ok value -> loop (value :: acc) rest
                | Error e -> Error e
              end
          in
          loop [] entries
      | Some (`String _), None ->
        Ok []
      | _ ->
        Error "invalid wasm method manifest"
    end
  | _ ->
    Error "invalid wasm method manifest"

let declarations_of_manifest = function
  | None -> Ok []
  | Some (`Assoc fields) ->
    begin
      match List.assoc_opt "methods" fields with
      | None -> Ok []
      | Some (`List methods) ->
        let rec loop acc = function
          | [] ->
            let declarations = List.rev acc in
            let method_names =
              List.filter_map
                (function
                  | `Assoc fields ->
                    begin
                      match List.assoc_opt "name" fields with
                      | Some (`String method_name) when method_name <> "" ->
                        Some method_name
                      | _ -> None
                    end
                  | _ -> None)
                methods
            in
            let keys =
              List.map
                (fun declaration ->
                  Printf.sprintf
                    "%s\000%s\000%s\000%d\000%d"
                    declaration.method_name
                    declaration.circle_id
                    declaration.canonical_path
                    declaration.offset
                    declaration.max_bytes)
                declarations
            in
            if List.length method_names <> List.length methods then
              Error "invalid wasm method manifest"
            else if
              List.length method_names
              <> List.length (List.sort_uniq String.compare method_names)
            then
              Error "duplicate wasm method manifest"
            else if List.length keys <> List.length (List.sort_uniq String.compare keys) then
              Error "duplicate wasm public read declaration"
            else
              Ok declarations
          | method_json :: rest ->
            begin
              match declarations_of_method method_json with
              | Ok declarations -> loop (List.rev_append declarations acc) rest
              | Error e -> Error e
            end
        in
        loop [] methods
      | Some _ ->
        Error "invalid wasm methods manifest"
    end
  | Some _ ->
    Error "invalid wasm manifest"

let for_method declarations method_name =
  List.filter
    (fun declaration -> String.equal declaration.method_name method_name)
    declarations

let load_path_policy store circle_id path_key =
  let* activate_after_epoch =
    Circle_policy_store.read_first_inline_value store circle_id [
      Circles.slot_policy_activate_after_key path_key;
      Circles.mailbox_activate_after_key path_key;
    ] in
  let* expire_after_epoch =
    Circle_policy_store.read_first_inline_value store circle_id [
      Circles.slot_policy_expire_after_key path_key;
      Circles.mailbox_expire_after_key path_key;
    ] in
  let* tombstone =
    Circle_policy_store.read_first_inline_value store circle_id [
      Circles.slot_policy_tombstone_key path_key;
      Circles.mailbox_tombstone_key path_key;
    ] in
  let* revoked =
    Circle_policy_store.read_first_inline_value store circle_id [
      Circles.slot_policy_revoked_key path_key;
      Circles.mailbox_revoked_key path_key;
    ] in
  Lwt.return {
    activate_after_epoch = Circle_policy_value.int64_value activate_after_epoch;
    expire_after_epoch = Circle_policy_value.int64_value expire_after_epoch;
    tombstone = Circle_policy_value.bool_value tombstone;
    revoked = Circle_policy_value.bool_value revoked;
  }

let visible_at current_epoch meta policy =
  let activate_after_epoch =
    match policy.activate_after_epoch with
    | Some _ as value -> value
    | None -> meta.Circles.activate_after_epoch in
  let expire_after_epoch =
    match policy.expire_after_epoch with
    | Some _ as value -> value
    | None -> meta.Circles.expire_after_epoch in
  let active =
    match activate_after_epoch with
    | Some epoch -> Int64.compare current_epoch epoch >= 0
    | None -> true
  in
  let unexpired =
    match expire_after_epoch with
    | Some epoch -> Int64.compare current_epoch epoch < 0
    | None -> true
  in
  active && unexpired && not policy.tombstone && not policy.revoked

let metadata_json
    snapshot_root
    (info : Circles.circle_info)
    (meta : Circles.asset_meta)
    offset
    body =
  Yojson.Safe.to_string
    (`Assoc [
      "version", `String "circle_public_read_v1";
      "snapshot_root", `String snapshot_root;
      "circle_id", `String info.Circles.circle_id;
      "circle_version", `String (Int64.to_string info.Circles.version);
      "assets_root", `String info.Circles.assets_root;
      "canonical_path", `String meta.Circles.canonical_path;
      "content_type", `String meta.Circles.content_type;
      "encoding", `String meta.Circles.encoding;
      "size_bytes", `String (Int64.to_string meta.Circles.size_bytes);
      "blob_hash", `String meta.Circles.blob_hash;
      "offset", `Int offset;
      "returned_bytes", `Int (String.length body);
    ])

let load_one store snapshot_root current_epoch (declaration : declaration) =
  let* info_opt = Store_irmin.get_circle_info store declaration.circle_id in
  match info_opt with
  | None ->
    Lwt.return (Error "wasm public read circle not found")
  | Some info
    when info.Circles.privacy_class <> Circles.Public
         || info.Circles.resource_mode <> Circles.Public_resources ->
    Lwt.return (Error "wasm public read requires a public circle")
  | Some info ->
    let* meta_opt =
      Store_irmin.get_circle_asset_meta
        store
        declaration.circle_id
        declaration.path_key in
    begin
      match meta_opt with
      | None ->
        Lwt.return (Error "wasm public read asset not found")
      | Some meta
        when meta.Circles.body_mode <> Circles.Public_resources
             || meta.Circles.locator_mode <> Circles.Path_locator
             || meta.Circles.metadata_mode <> Circles.Metadata_reveal
             || Option.is_some meta.Circles.key_id
             || not (String.equal meta.Circles.canonical_path declaration.canonical_path) ->
        Lwt.return (Error "wasm public read asset is not public")
      | Some meta ->
        let* policy =
          load_path_policy
            store
            declaration.circle_id
            declaration.path_key in
        if not (visible_at current_epoch meta policy) then
          Lwt.return (Error "wasm public read asset is not visible")
        else
          let* body_b64_opt =
            Store_irmin.get_circle_asset_body_b64
              store
              declaration.circle_id
              declaration.path_key in
          begin
            match body_b64_opt with
            | None ->
              Lwt.return (Error "wasm public read body not found")
            | Some body_b64 ->
              begin
                try
                  let raw = Base64.decode_exn body_b64 in
                  let raw_len = String.length raw in
                  if raw_len > max_source_bytes
                     || Int64.compare meta.Circles.size_bytes (Int64.of_int raw_len) <> 0
                     || not (String.equal meta.Circles.blob_hash (Circles.asset_blob_hash raw))
                  then
                    Lwt.return (Error "wasm public read body binding mismatch")
                  else if declaration.offset > raw_len then
                    Lwt.return (Error "wasm public read offset exceeds body")
                  else
                    let body_len =
                      min declaration.max_bytes (raw_len - declaration.offset) in
                    let body = String.sub raw declaration.offset body_len in
                    Lwt.return
                      (Ok {
                         snapshot_root;
                         circle_id = declaration.circle_id;
                         canonical_path = declaration.canonical_path;
                         offset = declaration.offset;
                         max_bytes = declaration.max_bytes;
                         metadata_json =
                           metadata_json
                             snapshot_root
                             info
                             meta
                             declaration.offset
                             body;
                         body;
                       })
                with _ ->
                  Lwt.return (Error "invalid wasm public read body")
              end
          end
    end

let current_snapshot_root store =
  let* batch_root = Store_irmin.get_batch_tree_hash store in
  let* root =
    match batch_root with
    | Some root -> Lwt.return (Some root)
    | None -> Store_irmin.get_head_hash store in
  Lwt.return
    (Option.map
       (fun value ->
         Circles.h256_hex "circle_public_read_snapshot_v1" [value])
       root)

let load store current_epoch (declarations : declaration list) =
  if declarations = [] then
    Lwt.return (Ok { snapshots = []; effort_used = 0 })
  else
    let* snapshot_root_opt = current_snapshot_root store in
    match snapshot_root_opt with
    | None ->
      Lwt.return (Error "wasm public read snapshot root is unavailable")
    | Some snapshot_root when not (is_hash snapshot_root) ->
      Lwt.return (Error "wasm public read requires a canonical snapshot root")
    | Some snapshot_root ->
      let unique_assets =
        declarations
        |> List.map
             (fun (declaration : declaration) ->
               declaration.circle_id ^ "\000" ^ declaration.canonical_path)
        |> List.sort_uniq String.compare
        |> List.length in
      if List.length declarations > max_reads_per_method
         || unique_assets > max_unique_assets
      then
        Lwt.return (Error "wasm public read set exceeds limit")
      else
        let rec loop snapshots total_bytes (pending : declaration list) =
          match pending with
          | [] ->
            Lwt.return
              (Ok {
                 snapshots = List.rev snapshots;
                 effort_used = base_effort + (total_bytes * byte_effort);
               })
          | declaration :: rest ->
            let* snapshot_result =
              load_one store snapshot_root current_epoch declaration in
            begin
              match snapshot_result with
              | Error e -> Lwt.return (Error e)
              | Ok snapshot ->
                let total_bytes = total_bytes + String.length snapshot.body in
                if total_bytes > max_total_bytes then
                  Lwt.return (Error "wasm public read bytes exceed limit")
                else
                  loop (snapshot :: snapshots) total_bytes rest
            end
        in
        loop [] 0 declarations

let yojson_of_snapshot snapshot =
  `Assoc [
    "snapshot_root", `String snapshot.snapshot_root;
    "circle_id", `String snapshot.circle_id;
    "canonical_path", `String snapshot.canonical_path;
    "offset", `Int snapshot.offset;
    "max_bytes", `Int snapshot.max_bytes;
    "metadata_json", `String snapshot.metadata_json;
    "body_b64", `String (Base64.encode_exn snapshot.body);
  ]