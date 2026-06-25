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


type state_class =
  | Opaque_cell
  | Cipher_cell
  | Balance_cell
  | Register_cell
  | Program_object

type hfhe_profile =
  | No_hfhe
  | Ciphertext_v1
  | Balance_cell_v1
  | Register_cell_v1
  | Commit_bound_v1

type t = {
  state_class : state_class;
  codec : string;
  schema_hash : string option;
  subject_addr : string option;
  hfhe_profile : hfhe_profile;
  mutable_state : bool;
}

type put_payload = {
  state_ref : string;
  state_class : state_class option;
  codec : string option;
  schema_hash : string option;
  subject_addr : string option;
  hfhe_profile : hfhe_profile option;
  mutable_state : bool option;
}

let is_hex_char = function
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' ->
    true
  | _ ->
    false

let normalize_hex64 value =
  let normalized =
    String.lowercase_ascii (String.trim value) in
  if String.length normalized = 64 && String.for_all is_hex_char normalized then
    Ok normalized
  else
    Error "state_ref must be a 64-char hex string"

let default = {
  state_class = Opaque_cell;
  codec = "opaque_b64_v1";
  schema_hash = None;
  subject_addr = None;
  hfhe_profile = No_hfhe;
  mutable_state = true;
}

let string_of_state_class = function
  | Opaque_cell -> "opaque_cell"
  | Cipher_cell -> "cipher_cell"
  | Balance_cell -> "balance_cell"
  | Register_cell -> "register_cell"
  | Program_object -> "program_object"

let state_class_of_string = function
  | "opaque_cell" -> Ok Opaque_cell
  | "cipher_cell" -> Ok Cipher_cell
  | "balance_cell" -> Ok Balance_cell
  | "register_cell" -> Ok Register_cell
  | "program_object" -> Ok Program_object
  | other -> Error ("unknown state descriptor class: " ^ other)

let string_of_hfhe_profile = function
  | No_hfhe -> "none"
  | Ciphertext_v1 -> "ciphertext_v1"
  | Balance_cell_v1 -> "balance_cell_v1"
  | Register_cell_v1 -> "register_cell_v1"
  | Commit_bound_v1 -> "commit_bound_v1"

let hfhe_profile_of_string = function
  | "none" -> Ok No_hfhe
  | "ciphertext_v1" -> Ok Ciphertext_v1
  | "balance_cell_v1" -> Ok Balance_cell_v1
  | "register_cell_v1" -> Ok Register_cell_v1
  | "commit_bound_v1" -> Ok Commit_bound_v1
  | other -> Error ("unknown state descriptor hfhe profile: " ^ other)

let base_key path_key suffix =
  "state_descriptor:" ^ path_key ^ ":" ^ suffix

let state_class_key path_key =
  base_key path_key "state_class"

let codec_key path_key =
  base_key path_key "codec"

let schema_hash_key path_key =
  base_key path_key "schema_hash"

let subject_addr_key path_key =
  base_key path_key "subject_addr"

let hfhe_profile_key path_key =
  base_key path_key "hfhe_profile"

let mutable_state_key path_key =
  base_key path_key "mutable_state"

let yojson_of_t (descriptor : t) =
  `Assoc [
    "state_class", `String (string_of_state_class descriptor.state_class);
    "codec", `String descriptor.codec;
    "schema_hash",
    begin
      match descriptor.schema_hash with
      | Some value -> `String value
      | None -> `Null
    end;
    "subject_addr",
    begin
      match descriptor.subject_addr with
      | Some value -> `String value
      | None -> `Null
    end;
    "hfhe_profile", `String (string_of_hfhe_profile descriptor.hfhe_profile);
    "mutable_state", `Bool descriptor.mutable_state;
  ]

let yojson_of_put_payload (payload : put_payload) =
  `Assoc [
    "state_ref", `String payload.state_ref;
    "state_class",
    begin
      match payload.state_class with
      | Some value -> `String (string_of_state_class value)
      | None -> `Null
    end;
    "codec",
    begin
      match payload.codec with
      | Some value -> `String value
      | None -> `Null
    end;
    "schema_hash",
    begin
      match payload.schema_hash with
      | Some value -> `String value
      | None -> `Null
    end;
    "subject_addr",
    begin
      match payload.subject_addr with
      | Some value -> `String value
      | None -> `Null
    end;
    "hfhe_profile",
    begin
      match payload.hfhe_profile with
      | Some value -> `String (string_of_hfhe_profile value)
      | None -> `Null
    end;
    "mutable_state",
    begin
      match payload.mutable_state with
      | Some value -> `Bool value
      | None -> `Null
    end;
  ]

let validate_codec value =
  let normalized = String.trim value in
  if String.length normalized = 0 then
    Error "state descriptor codec must not be empty"
  else
    Ok normalized

let validate_schema_hash = function
  | None -> Ok None
  | Some value ->
    let normalized =
      String.lowercase_ascii (String.trim value) in
    if String.length normalized = 0 then
      Ok None
    else if
      String.length normalized = 64
      && String.for_all is_hex_char normalized
    then
      Ok (Some normalized)
    else
      Error "state descriptor schema_hash must be a 64-char hex string"

let validate_subject_addr = function
  | None -> Ok None
  | Some value ->
    let normalized = String.trim value in
    if String.length normalized = 0 then
      Ok None
    else if Crypto.Address.is_valid_address normalized then
      Ok (Some normalized)
    else
      Error "state descriptor subject_addr must be a valid octra address"

let validate (descriptor : t) =
  match
    validate_codec descriptor.codec,
    validate_schema_hash descriptor.schema_hash,
    validate_subject_addr descriptor.subject_addr
  with
  | Ok codec, Ok schema_hash, Ok subject_addr ->
    Ok {
      descriptor with
      codec;
      schema_hash;
      subject_addr;
    }
  | Error e, _, _
  | _, Error e, _
  | _, _, Error e ->
    Error e

let resolve_put_payload (payload : put_payload) =
  validate {
    state_class = Option.value ~default:default.state_class payload.state_class;
    codec = Option.value ~default:default.codec payload.codec;
    schema_hash = payload.schema_hash;
    subject_addr = payload.subject_addr;
    hfhe_profile = Option.value ~default:default.hfhe_profile payload.hfhe_profile;
    mutable_state = Option.value ~default:default.mutable_state payload.mutable_state;
  }

let read_optional_string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok (Some value)
  | Some `Null
  | None ->
    Ok None
  | _ ->
    Error ("invalid " ^ name)

let read_optional_bool_field name fields =
  match List.assoc_opt name fields with
  | Some (`Bool value) -> Ok (Some value)
  | Some `Null
  | None ->
    Ok None
  | _ ->
    Error ("invalid " ^ name)

let read_optional_enum_field name fields decode =
  match List.assoc_opt name fields with
  | Some (`String value) ->
    decode value |> Result.map Option.some
  | Some `Null
  | None ->
    Ok None
  | _ ->
    Error ("invalid " ^ name)

let put_payload_of_yojson = function
  | `Assoc fields ->
    begin
      match List.assoc_opt "state_ref" fields with
      | Some (`String state_ref) ->
        begin
          match
            read_optional_enum_field "state_class" fields state_class_of_string,
            read_optional_string_field "codec" fields,
            read_optional_string_field "schema_hash" fields,
            read_optional_string_field "subject_addr" fields,
            read_optional_enum_field "hfhe_profile" fields hfhe_profile_of_string,
            read_optional_bool_field "mutable_state" fields
          with
          | Ok state_class, Ok codec, Ok schema_hash, Ok subject_addr,
            Ok hfhe_profile, Ok mutable_state ->
            begin
              match normalize_hex64 state_ref with
              | Ok normalized_state_ref ->
                Ok {
                  state_ref = normalized_state_ref;
                  state_class;
                  codec;
                  schema_hash;
                  subject_addr;
                  hfhe_profile;
                  mutable_state;
                }
              | Error e ->
                Error e
            end
          | Error e, _, _, _, _, _
          | _, Error e, _, _, _, _
          | _, _, Error e, _, _, _
          | _, _, _, Error e, _, _
          | _, _, _, _, Error e, _
          | _, _, _, _, _, Error e ->
            Error e
        end
      | _ ->
        Error "state descriptor payload requires state_ref"
    end
  | _ ->
    Error "state descriptor payload must be an object"

let of_stored_values
    ~state_class_raw
    ~codec_raw
    ~schema_hash_raw
    ~subject_addr_raw
    ~hfhe_profile_raw
    ~mutable_state_raw =
  let descriptor = {
    state_class =
      begin
        match state_class_raw with
        | Some value ->
          begin
            match state_class_of_string (String.trim value) with
            | Ok state_class -> state_class
            | Error _ -> default.state_class
          end
        | None -> default.state_class
      end;
    codec =
      begin
        match codec_raw with
        | Some value when String.length (String.trim value) > 0 -> String.trim value
        | _ -> default.codec
      end;
    schema_hash = schema_hash_raw;
    subject_addr = subject_addr_raw;
    hfhe_profile =
      begin
        match hfhe_profile_raw with
        | Some value ->
          begin
            match hfhe_profile_of_string (String.trim value) with
            | Ok hfhe_profile -> hfhe_profile
            | Error _ -> default.hfhe_profile
          end
        | None -> default.hfhe_profile
      end;
    mutable_state =
      begin
        match mutable_state_raw with
        | Some _ -> Circle_policy_value.bool_value mutable_state_raw
        | None -> default.mutable_state
      end;
  } in
  match validate descriptor with
  | Ok normalized -> normalized
  | Error _ -> default

let write_snapshot storage_tbl path_key payload =
  let open Circle_policy_value in
  set_or_clear
    storage_tbl
    (state_class_key path_key)
    (Option.map string_of_state_class payload.state_class);
  set_or_clear storage_tbl (codec_key path_key) payload.codec;
  set_or_clear storage_tbl (schema_hash_key path_key) payload.schema_hash;
  set_or_clear storage_tbl (subject_addr_key path_key) payload.subject_addr;
  set_or_clear
    storage_tbl
    (hfhe_profile_key path_key)
    (Option.map string_of_hfhe_profile payload.hfhe_profile);
  set_or_clear
    storage_tbl
    (mutable_state_key path_key)
    (Option.map string_of_bool payload.mutable_state)

let runtime_suffix_allowed = function
  | "state_class"
  | "codec"
  | "schema_hash"
  | "subject_addr"
  | "hfhe_profile"
  | "mutable_state" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; path_key; suffix] when raw_prefix = "state_descriptor" ->
    if String.length path_key <> 64 || not (String.for_all is_hex_char path_key) then
      Error ("circle_runtime_invalid_state_descriptor_key", raw_key, "path_key")
    else if not (runtime_suffix_allowed suffix) then
      Error ("circle_runtime_invalid_state_descriptor_key", raw_key, suffix)
    else
      begin
        match suffix with
        | "state_class" ->
          begin
            match state_class_of_string (String.trim value) with
            | Ok _ -> Ok ()
            | Error _ -> Error ("circle_runtime_invalid_state_descriptor_value", raw_key, suffix)
          end
        | "codec" ->
          if String.length (String.trim value) = 0 then
            Error ("circle_runtime_invalid_state_descriptor_value", raw_key, suffix)
          else
            Ok ()
        | "schema_hash" ->
          begin
            match validate_schema_hash (Some value) with
            | Ok _ -> Ok ()
            | Error _ -> Error ("circle_runtime_invalid_state_descriptor_value", raw_key, suffix)
          end
        | "subject_addr" ->
          begin
            match validate_subject_addr (Some value) with
            | Ok _ -> Ok ()
            | Error _ -> Error ("circle_runtime_invalid_state_descriptor_value", raw_key, suffix)
          end
        | "hfhe_profile" ->
          begin
            match hfhe_profile_of_string (String.trim value) with
            | Ok _ -> Ok ()
            | Error _ -> Error ("circle_runtime_invalid_state_descriptor_value", raw_key, suffix)
          end
        | "mutable_state" ->
          let normalized = String.lowercase_ascii (String.trim value) in
          if normalized = "0" || normalized = "1" || normalized = "false" || normalized = "true" || normalized = "no" || normalized = "yes" then
            Ok ()
          else
            Error ("circle_runtime_invalid_state_descriptor_value", raw_key, suffix)
        | _ ->
          Error ("circle_runtime_invalid_state_descriptor_key", raw_key, suffix)
      end
  | _ ->
    Error ("circle_runtime_invalid_state_descriptor_key", raw_key, "shape")