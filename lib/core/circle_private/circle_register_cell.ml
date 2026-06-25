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
  ciphertext_commitment : string option;
  proof_kind : Circle_hfhe_proof.t option;
  proof_receipt_hash : string option;
}

type put_payload = {
  state_ref : string;
  content_type : string option;
  encoding : string option;
  key_id : string;
  plaintext_hash : string;
  padding_class : string option;
  delivery_key_id : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  metadata_mode : Circles.metadata_mode option;
  codec : string option;
  schema_hash : string option;
  subject_addr : string option;
  mutable_state : bool option;
  hfhe_profile : Circle_state_descriptor.hfhe_profile option;
  ciphertext_commitment : string;
  proof_kind : Circle_hfhe_proof.t option;
  proof_receipt_hash : string option;
}

type write_request = {
  state_ref : string;
  content_type : string;
  encoding : string option;
  key_id : string;
  plaintext_hash : string;
  padding_class : string option;
  delivery_key_id : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  metadata_mode : Circles.metadata_mode;
  descriptor : Circle_state_descriptor.t;
  cell : t;
}

let empty = {
  ciphertext_commitment = None;
  proof_kind = None;
  proof_receipt_hash = None;
}

let default_content_type =
  "application/vnd.octra.register-cell+ciphertext"

let base_key path_key suffix =
  "register_cell:" ^ path_key ^ ":" ^ suffix

let ciphertext_commitment_key path_key =
  base_key path_key "ciphertext_commitment"

let proof_kind_key path_key =
  base_key path_key "proof_kind"

let proof_receipt_hash_key path_key =
  base_key path_key "proof_receipt_hash"

let yojson_of_t (cell : t) =
  `Assoc [
    "ciphertext_commitment",
    begin
      match cell.ciphertext_commitment with
      | Some value -> `String value
      | None -> `Null
    end;
    "proof_kind",
    begin
      match cell.proof_kind with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
    "proof_receipt_hash",
    begin
      match cell.proof_receipt_hash with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let yojson_of_put_payload (payload : put_payload) =
  `Assoc [
    "state_ref", `String payload.state_ref;
    "content_type",
    begin
      match payload.content_type with
      | Some value -> `String value
      | None -> `Null
    end;
    "encoding",
    begin
      match payload.encoding with
      | Some value -> `String value
      | None -> `Null
    end;
    "key_id", `String payload.key_id;
    "plaintext_hash", `String payload.plaintext_hash;
    "padding_class",
    begin
      match payload.padding_class with
      | Some value -> `String value
      | None -> `Null
    end;
    "delivery_key_id",
    begin
      match payload.delivery_key_id with
      | Some value -> `String value
      | None -> `Null
    end;
    "activate_after_epoch",
    begin
      match payload.activate_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match payload.expire_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "metadata_mode",
    begin
      match payload.metadata_mode with
      | Some value -> `String (Circles.string_of_metadata_mode value)
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
    "mutable_state",
    begin
      match payload.mutable_state with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "hfhe_profile",
    begin
      match payload.hfhe_profile with
      | Some value ->
        `String (Circle_state_descriptor.string_of_hfhe_profile value)
      | None -> `Null
    end;
    "ciphertext_commitment", `String payload.ciphertext_commitment;
    "proof_kind",
    begin
      match payload.proof_kind with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
    "proof_receipt_hash",
    begin
      match payload.proof_receipt_hash with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let compatible_hfhe_profile = function
  | Circle_state_descriptor.Register_cell_v1
  | Circle_state_descriptor.Ciphertext_v1 ->
    true
  | Circle_state_descriptor.No_hfhe
  | Circle_state_descriptor.Balance_cell_v1
  | Circle_state_descriptor.Commit_bound_v1 ->
    false

let proof_kind_allowed = function
  | None
  | Some Circle_hfhe_proof.Zero_receipt_v1
  | Some Circle_hfhe_proof.Range_v1
  | Some Circle_hfhe_proof.Range_receipt_v1 ->
    true
  | Some Circle_hfhe_proof.No_proof
  | Some Circle_hfhe_proof.Bound_zero_v1
  | Some Circle_hfhe_proof.Bound_zero_receipt_v1 ->
    false

let ensure_descriptor_compatible (descriptor : Circle_state_descriptor.t) =
  if descriptor.state_class <> Circle_state_descriptor.Register_cell then
    Error "register cell requires register_cell descriptor class"
  else if not (compatible_hfhe_profile descriptor.hfhe_profile) then
    Error "register cell requires register_cell_v1 or ciphertext_v1 hfhe profile"
  else
    Ok descriptor

let normalize_cell
    ~ciphertext_commitment
    ~proof_kind
    ~proof_receipt_hash =
  let open Circle_private_common in
  match
    normalize_commitment "ciphertext_commitment" ciphertext_commitment,
    normalize_optional_proof_kind proof_kind,
    normalize_optional_hex64 "proof_receipt_hash" proof_receipt_hash
  with
  | Ok ciphertext_commitment, Ok proof_kind, Ok proof_receipt_hash ->
    if not (proof_kind_allowed proof_kind) then
      Error "register cell proof_kind is not compatible with register cells"
    else
      begin
        match validate_proof_receipt_binding ~proof_kind ~proof_receipt_hash with
        | Ok () ->
          Ok {
            ciphertext_commitment = Some ciphertext_commitment;
            proof_kind;
            proof_receipt_hash;
          }
        | Error e ->
          Error e
      end
  | Error e, _, _
  | _, Error e, _
  | _, _, Error e ->
    Error e

let resolve_put_payload (payload : put_payload) =
  let open Circle_private_common in
  match
    Circles.normalize_state_ref payload.state_ref,
    normalize_optional_content_type default_content_type payload.content_type,
    normalize_non_empty "key_id" payload.key_id,
    normalize_hex64 "plaintext_hash" payload.plaintext_hash,
    normalize_optional_non_empty "padding_class" payload.padding_class,
    normalize_optional_non_empty "delivery_key_id" payload.delivery_key_id,
    normalize_optional_metadata_mode
      Circles.Metadata_opaque
      payload.metadata_mode,
    normalize_cell
      ~ciphertext_commitment:payload.ciphertext_commitment
      ~proof_kind:payload.proof_kind
      ~proof_receipt_hash:payload.proof_receipt_hash,
    validate_epoch_window payload.activate_after_epoch payload.expire_after_epoch
  with
  | Ok state_ref, Ok content_type, Ok key_id, Ok plaintext_hash, Ok padding_class,
    Ok delivery_key_id, Ok metadata_mode, Ok cell, Ok () ->
    begin
      match
        Circle_state_descriptor.resolve_put_payload
          {
            state_ref;
            state_class = Some Circle_state_descriptor.Register_cell;
            codec = payload.codec;
            schema_hash = payload.schema_hash;
            subject_addr = payload.subject_addr;
            hfhe_profile =
              Some
                (Option.value
                   ~default:Circle_state_descriptor.Register_cell_v1
                   payload.hfhe_profile);
            mutable_state = payload.mutable_state;
          }
      with
      | Ok descriptor ->
        begin
          match ensure_descriptor_compatible descriptor with
          | Ok descriptor ->
            Ok {
              state_ref;
              content_type;
              encoding = payload.encoding;
              key_id;
              plaintext_hash;
              padding_class;
              delivery_key_id;
              activate_after_epoch = payload.activate_after_epoch;
              expire_after_epoch = payload.expire_after_epoch;
              metadata_mode;
              descriptor;
              cell;
            }
          | Error e ->
            Error e
        end
      | Error e ->
        Error e
    end
  | Error e, _, _, _, _, _, _, _, _
  | _, Error e, _, _, _, _, _, _, _
  | _, _, Error e, _, _, _, _, _, _
  | _, _, _, Error e, _, _, _, _, _
  | _, _, _, _, Error e, _, _, _, _
  | _, _, _, _, _, Error e, _, _, _
  | _, _, _, _, _, _, Error e, _, _
  | _, _, _, _, _, _, _, Error e, _
  | _, _, _, _, _, _, _, _, Error e ->
    Error e

let descriptor_payload (request : write_request) =
  Circle_private_common.descriptor_payload_of_resolved
    request.state_ref
    request.descriptor

let sealed_payload (request : write_request) =
  {
    Circles.slot_ref = None;
    state_ref = Some request.state_ref;
    content_type = request.content_type;
    encoding = request.encoding;
    key_id = request.key_id;
    plaintext_hash = request.plaintext_hash;
    padding_class = request.padding_class;
    activate_after_epoch = request.activate_after_epoch;
    expire_after_epoch = request.expire_after_epoch;
    metadata_mode = Some request.metadata_mode;
  }

let policy_payload (request : write_request) =
  Circle_private_common.state_policy_payload_of_resolved
    request.state_ref
    request.delivery_key_id
    request.activate_after_epoch
    request.expire_after_epoch

let write_snapshot storage_tbl path_key (cell : t) =
  let open Circle_policy_value in
  set_or_clear
    storage_tbl
    (ciphertext_commitment_key path_key)
    cell.ciphertext_commitment;
  set_or_clear
    storage_tbl
    (proof_kind_key path_key)
    (Option.map Circle_hfhe_proof.string_of_t cell.proof_kind);
  set_or_clear
    storage_tbl
    (proof_receipt_hash_key path_key)
    cell.proof_receipt_hash

let materialized (cell : t) =
  Option.is_some cell.ciphertext_commitment
  || Option.is_some cell.proof_kind
  || Option.is_some cell.proof_receipt_hash

let of_stored_values
    ~ciphertext_commitment_raw
    ~proof_kind_raw
    ~proof_receipt_hash_raw =
  let parse_commitment = function
    | Some value ->
      begin
        match Circle_private_common.normalize_commitment "commitment" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  let parse_proof_kind = function
    | Some value ->
      begin
        match Circle_hfhe_proof.of_string (String.trim value) with
        | Ok proof ->
          begin
            match Circle_private_common.normalize_optional_proof_kind (Some proof) with
            | Ok proof_kind when proof_kind_allowed proof_kind -> proof_kind
            | Ok _ -> None
            | Error _ -> None
          end
        | Error _ -> None
      end
    | None -> None
  in
  let parse_receipt_hash = function
    | Some value ->
      begin
        match
          Circle_private_common.normalize_hex64 "proof_receipt_hash" value
        with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  {
    ciphertext_commitment = parse_commitment ciphertext_commitment_raw;
    proof_kind = parse_proof_kind proof_kind_raw;
    proof_receipt_hash = parse_receipt_hash proof_receipt_hash_raw;
  }

let runtime_suffix_allowed = function
  | "ciphertext_commitment"
  | "proof_kind"
  | "proof_receipt_hash" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; path_key; suffix] when raw_prefix = "register_cell" ->
    if String.length path_key <> 64 || not (String.for_all Circles.is_hex_char path_key) then
      Error ("circle_runtime_invalid_register_cell_key", raw_key, "path_key")
    else if not (runtime_suffix_allowed suffix) then
      Error ("circle_runtime_invalid_register_cell_key", raw_key, suffix)
    else
      begin
        match suffix with
        | "ciphertext_commitment" ->
          begin
            match Circle_private_common.normalize_commitment suffix value with
            | Ok _ -> Ok ()
            | Error _ -> Error ("circle_runtime_invalid_register_cell_value", raw_key, suffix)
          end
        | "proof_kind" ->
          begin
            match Circle_hfhe_proof.of_string (String.trim value) with
            | Ok proof ->
              begin
                match Circle_private_common.normalize_optional_proof_kind (Some proof) with
                | Ok proof_kind when proof_kind_allowed proof_kind -> Ok ()
                | Ok _ -> Error ("circle_runtime_invalid_register_cell_value", raw_key, suffix)
                | Error _ -> Error ("circle_runtime_invalid_register_cell_value", raw_key, suffix)
              end
            | Error _ -> Error ("circle_runtime_invalid_register_cell_value", raw_key, suffix)
          end
        | "proof_receipt_hash" ->
          begin
            match Circle_private_common.normalize_hex64 suffix value with
            | Ok _ -> Ok ()
            | Error _ -> Error ("circle_runtime_invalid_register_cell_value", raw_key, suffix)
          end
        | _ ->
          Error ("circle_runtime_invalid_register_cell_key", raw_key, suffix)
      end
  | _ ->
    Error ("circle_runtime_invalid_register_cell_key", raw_key, "shape")

let put_payload_of_yojson = function
  | `Assoc fields ->
    begin
      match
        Circle_private_common.read_required_string_field "state_ref" fields,
        Circle_private_common.read_optional_string_field "content_type" fields,
        Circle_private_common.read_optional_string_field "encoding" fields,
        Circle_private_common.read_required_string_field "key_id" fields,
        Circle_private_common.read_required_string_field "plaintext_hash" fields,
        Circle_private_common.read_optional_string_field "padding_class" fields,
        Circle_private_common.read_optional_string_field "delivery_key_id" fields,
        Circle_private_common.read_optional_i64_field "activate_after_epoch" fields,
        Circle_private_common.read_optional_i64_field "expire_after_epoch" fields,
        Circle_private_common.read_optional_metadata_mode_field "metadata_mode" fields,
        Circle_private_common.read_optional_string_field "codec" fields,
        Circle_private_common.read_optional_string_field "schema_hash" fields,
        Circle_private_common.read_optional_string_field "subject_addr" fields,
        Circle_private_common.read_optional_bool_field "mutable_state" fields,
        Circle_private_common.read_optional_enum_field
          "hfhe_profile"
          fields
          Circle_state_descriptor.hfhe_profile_of_string,
        Circle_private_common.read_required_string_field "ciphertext_commitment" fields,
        Circle_private_common.read_optional_enum_field
          "proof_kind"
          fields
          Circle_hfhe_proof.of_string,
        Circle_private_common.read_optional_string_field "proof_receipt_hash" fields
      with
      | Ok state_ref, Ok content_type, Ok encoding, Ok key_id, Ok plaintext_hash,
        Ok padding_class, Ok delivery_key_id, Ok activate_after_epoch,
        Ok expire_after_epoch, Ok metadata_mode, Ok codec, Ok schema_hash,
        Ok subject_addr, Ok mutable_state, Ok hfhe_profile,
        Ok ciphertext_commitment, Ok proof_kind, Ok proof_receipt_hash ->
        begin
          match Circles.normalize_state_ref state_ref with
          | Ok normalized_state_ref ->
            Ok {
              state_ref = normalized_state_ref;
              content_type;
              encoding;
              key_id;
              plaintext_hash;
              padding_class;
              delivery_key_id;
              activate_after_epoch;
              expire_after_epoch;
              metadata_mode;
              codec;
              schema_hash;
              subject_addr;
              mutable_state;
              hfhe_profile;
              ciphertext_commitment;
              proof_kind;
              proof_receipt_hash;
            }
          | Error e ->
            Error e
        end
      | Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e, _
      | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error e ->
        Error e
    end
  | _ ->
    Error "register cell payload must be an object"