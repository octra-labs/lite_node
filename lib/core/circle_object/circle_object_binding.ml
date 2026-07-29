(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  current_state_ref : string option;
  version : int64 option;
  status : string option;
  last_transition_ref : string option;
}

let empty = {
  current_state_ref = None;
  version = None;
  status = None;
  last_transition_ref = None;
}

let base_key object_ref suffix =
  "object_binding:" ^ object_ref ^ ":" ^ suffix

let current_state_ref_key object_ref =
  base_key object_ref "current_state_ref"

let version_key object_ref =
  base_key object_ref "version"

let status_key object_ref =
  base_key object_ref "status"

let last_transition_ref_key object_ref =
  base_key object_ref "last_transition_ref"

let yojson_of_t (binding : t) =
  `Assoc [
    "current_state_ref",
    begin
      match binding.current_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "version",
    begin
      match binding.version with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "status",
    begin
      match binding.status with
      | Some value -> `String value
      | None -> `Null
    end;
    "last_transition_ref",
    begin
      match binding.last_transition_ref with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let write_snapshot storage_tbl object_ref (binding : t) =
  let open Circle_policy_value in
  set_or_clear
    storage_tbl
    (current_state_ref_key object_ref)
    binding.current_state_ref;
  set_or_clear
    storage_tbl
    (version_key object_ref)
    (Option.map Int64.to_string binding.version);
  set_or_clear
    storage_tbl
    (status_key object_ref)
    binding.status;
  set_or_clear
    storage_tbl
    (last_transition_ref_key object_ref)
    binding.last_transition_ref

let materialized (binding : t) =
  Option.is_some binding.current_state_ref
  || Option.is_some binding.version
  || Option.is_some binding.status
  || Option.is_some binding.last_transition_ref

let of_stored_values
    ~current_state_ref_raw
    ~version_raw
    ~status_raw
    ~last_transition_ref_raw =
  let parse_state_ref = function
    | Some value ->
      begin
        match Circles.normalize_state_ref value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  let parse_version = function
    | Some value ->
      begin
        try
          let parsed = Int64.of_string (String.trim value) in
          if Int64.compare parsed 0L < 0 then None else Some parsed
        with _ ->
          None
      end
    | None -> None
  in
  let parse_status = function
    | Some value ->
      begin
        match Circle_private_common.normalize_non_empty "status" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  let parse_transition_ref = function
    | Some value ->
      begin
        match Circle_private_common.normalize_hex64 "last_transition_ref" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  {
    current_state_ref = parse_state_ref current_state_ref_raw;
    version = parse_version version_raw;
    status = parse_status status_raw;
    last_transition_ref = parse_transition_ref last_transition_ref_raw;
  }

let runtime_suffix_allowed = function
  | "current_state_ref"
  | "version"
  | "status"
  | "last_transition_ref" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; object_ref; suffix] when raw_prefix = "object_binding" ->
    begin
      match Circle_private_common.normalize_hex64 "object_ref" object_ref with
      | Error _ ->
        Error ("circle_runtime_invalid_object_binding_key", raw_key, "object_ref")
      | Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_object_binding_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "current_state_ref" ->
              begin
                match Circles.normalize_state_ref value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_binding_value", raw_key, suffix)
              end
            | "version" ->
              begin
                try
                  let parsed = Int64.of_string (String.trim value) in
                  if Int64.compare parsed 0L < 0 then
                    Error ("circle_runtime_invalid_object_binding_value", raw_key, suffix)
                  else
                    Ok ()
                with _ ->
                  Error ("circle_runtime_invalid_object_binding_value", raw_key, suffix)
              end
            | "status" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_binding_value", raw_key, suffix)
              end
            | "last_transition_ref" ->
              begin
                match Circle_private_common.normalize_hex64 suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_binding_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_object_binding_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_object_binding_key", raw_key, "shape")