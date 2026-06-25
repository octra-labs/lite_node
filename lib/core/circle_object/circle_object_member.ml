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


type t = {
  state_ref : string option;
  member_kind : string option;
  state_class : string option;
  codec : string option;
  status : string option;
}

let empty = {
  state_ref = None;
  member_kind = None;
  state_class = None;
  codec = None;
  status = None;
}

let validate_member_ref value =
  let normalized = String.trim value in
  if String.length normalized = 0 then
    Error "member_ref must not be empty"
  else if String.contains normalized ':' then
    Error "member_ref must not contain ':'"
  else
    Ok normalized

let base_key object_ref member_ref suffix =
  "object_member:" ^ object_ref ^ ":" ^ member_ref ^ ":" ^ suffix

let state_ref_key object_ref member_ref =
  base_key object_ref member_ref "state_ref"

let member_kind_key object_ref member_ref =
  base_key object_ref member_ref "member_kind"

let state_class_key object_ref member_ref =
  base_key object_ref member_ref "state_class"

let codec_key object_ref member_ref =
  base_key object_ref member_ref "codec"

let status_key object_ref member_ref =
  base_key object_ref member_ref "status"

let yojson_of_t (member_state : t) =
  `Assoc [
    "state_ref",
    begin
      match member_state.state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "member_kind",
    begin
      match member_state.member_kind with
      | Some value -> `String value
      | None -> `Null
    end;
    "state_class",
    begin
      match member_state.state_class with
      | Some value -> `String value
      | None -> `Null
    end;
    "codec",
    begin
      match member_state.codec with
      | Some value -> `String value
      | None -> `Null
    end;
    "status",
    begin
      match member_state.status with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let write_snapshot storage_tbl object_ref member_ref (member_state : t) =
  let open Circle_policy_value in
  set_or_clear
    storage_tbl
    (state_ref_key object_ref member_ref)
    member_state.state_ref;
  set_or_clear
    storage_tbl
    (member_kind_key object_ref member_ref)
    member_state.member_kind;
  set_or_clear
    storage_tbl
    (state_class_key object_ref member_ref)
    member_state.state_class;
  set_or_clear
    storage_tbl
    (codec_key object_ref member_ref)
    member_state.codec;
  set_or_clear
    storage_tbl
    (status_key object_ref member_ref)
    member_state.status

let materialized (member_state : t) =
  Option.is_some member_state.state_ref
  || Option.is_some member_state.member_kind
  || Option.is_some member_state.state_class
  || Option.is_some member_state.codec
  || Option.is_some member_state.status

let of_stored_values
    ~state_ref_raw
    ~member_kind_raw
    ~state_class_raw
    ~codec_raw
    ~status_raw =
  let parse_state_ref = function
    | Some value ->
      begin
        match Circles.normalize_state_ref value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  let parse_non_empty = function
    | Some value ->
      begin
        match Circle_private_common.normalize_non_empty "field" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  {
    state_ref = parse_state_ref state_ref_raw;
    member_kind = parse_non_empty member_kind_raw;
    state_class = parse_non_empty state_class_raw;
    codec = parse_non_empty codec_raw;
    status = parse_non_empty status_raw;
  }

let runtime_suffix_allowed = function
  | "state_ref"
  | "member_kind"
  | "state_class"
  | "codec"
  | "status" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; object_ref; member_ref; suffix] when raw_prefix = "object_member" ->
    begin
      match
        Circle_private_common.normalize_hex64 "object_ref" object_ref,
        validate_member_ref member_ref
      with
      | Error _, _
      | _, Error _ ->
        Error ("circle_runtime_invalid_object_member_key", raw_key, "shape")
      | Ok _, Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_object_member_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "state_ref" ->
              begin
                match Circles.normalize_state_ref value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_member_value", raw_key, suffix)
              end
            | "member_kind"
            | "state_class"
            | "codec"
            | "status" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_member_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_object_member_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_object_member_key", raw_key, "shape")