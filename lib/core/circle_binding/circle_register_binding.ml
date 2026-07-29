(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  current_state_ref : string option;
  version : int64 option;
  status : string option;
  last_workflow_ref : string option;
}

let empty = {
  current_state_ref = None;
  version = None;
  status = None;
  last_workflow_ref = None;
}

let base_key register_ref suffix =
  "register_binding:" ^ register_ref ^ ":" ^ suffix

let current_state_ref_key register_ref =
  base_key register_ref "current_state_ref"

let version_key register_ref =
  base_key register_ref "version"

let status_key register_ref =
  base_key register_ref "status"

let last_workflow_ref_key register_ref =
  base_key register_ref "last_workflow_ref"

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
    "last_workflow_ref",
    begin
      match binding.last_workflow_ref with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let write_snapshot storage_tbl register_ref (binding : t) =
  let open Circle_policy_value in
  set_or_clear
    storage_tbl
    (current_state_ref_key register_ref)
    binding.current_state_ref;
  set_or_clear
    storage_tbl
    (version_key register_ref)
    (Option.map Int64.to_string binding.version);
  set_or_clear
    storage_tbl
    (status_key register_ref)
    binding.status;
  set_or_clear
    storage_tbl
    (last_workflow_ref_key register_ref)
    binding.last_workflow_ref

let materialized (binding : t) =
  Option.is_some binding.current_state_ref
  || Option.is_some binding.version
  || Option.is_some binding.status
  || Option.is_some binding.last_workflow_ref

let of_stored_values
    ~current_state_ref_raw
    ~version_raw
    ~status_raw
    ~last_workflow_ref_raw =
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
  let parse_workflow_ref = function
    | Some value ->
      begin
        match Circle_private_common.normalize_hex64 "last_workflow_ref" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  {
    current_state_ref = parse_state_ref current_state_ref_raw;
    version = parse_version version_raw;
    status = parse_status status_raw;
    last_workflow_ref = parse_workflow_ref last_workflow_ref_raw;
  }

let runtime_suffix_allowed = function
  | "current_state_ref"
  | "version"
  | "status"
  | "last_workflow_ref" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; register_ref; suffix] when raw_prefix = "register_binding" ->
    begin
      match Circle_private_common.normalize_hex64 "register_ref" register_ref with
      | Error _ ->
        Error ("circle_runtime_invalid_register_binding_key", raw_key, "register_ref")
      | Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_register_binding_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "current_state_ref" ->
              begin
                match Circles.normalize_state_ref value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_register_binding_value", raw_key, suffix)
              end
            | "version" ->
              begin
                try
                  let parsed = Int64.of_string (String.trim value) in
                  if Int64.compare parsed 0L < 0 then
                    Error ("circle_runtime_invalid_register_binding_value", raw_key, suffix)
                  else
                    Ok ()
                with _ ->
                  Error ("circle_runtime_invalid_register_binding_value", raw_key, suffix)
              end
            | "status" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_register_binding_value", raw_key, suffix)
              end
            | "last_workflow_ref" ->
              begin
                match Circle_private_common.normalize_hex64 suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_register_binding_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_register_binding_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_register_binding_key", raw_key, "shape")