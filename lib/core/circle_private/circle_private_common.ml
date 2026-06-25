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


let normalize_non_empty field value =
  let normalized = String.trim value in
  if String.length normalized = 0 then
    Error (field ^ " must not be empty")
  else
    Ok normalized

let normalize_optional_non_empty field = function
  | None -> Ok None
  | Some value ->
    normalize_non_empty field value
    |> Result.map Option.some

let normalize_hex64 field value =
  let normalized = String.lowercase_ascii (String.trim value) in
  if String.length normalized = 64 && String.for_all Circles.is_hex_char normalized then
    Ok normalized
  else
    Error (field ^ " must be a 64-char hex string")

let normalize_optional_hex64 field = function
  | None -> Ok None
  | Some value ->
    normalize_hex64 field value
    |> Result.map Option.some

let normalize_commitment field value =
  let normalized = String.trim value in
  if String.length normalized = 0 then
    Error (field ^ " must not be empty")
  else
    match Base64.decode normalized with
    | Ok raw when String.length raw = 32 ->
      Ok normalized
    | Ok _ ->
      Error (field ^ " must decode to 32 bytes")
    | Error _ ->
      Error (field ^ " must be valid base64")

let normalize_optional_content_type default = function
  | None -> Ok default
  | Some value ->
    let normalized = String.trim value in
    if String.length normalized = 0 then
      Ok default
    else
      Ok normalized

let normalize_optional_metadata_mode default = function
  | Some value -> Ok value
  | None -> Ok default

let validate_epoch_window activate_after_epoch expire_after_epoch =
  match activate_after_epoch, expire_after_epoch with
  | Some activate_after_epoch, Some expire_after_epoch ->
    if Int64.compare expire_after_epoch activate_after_epoch <= 0 then
      Error "expire_after_epoch must be greater than activate_after_epoch"
    else
      Ok ()
  | _ ->
    Ok ()

let proof_kind_requires_receipt = function
  | Some Circle_hfhe_proof.Zero_receipt_v1
  | Some Circle_hfhe_proof.Range_receipt_v1
  | Some Circle_hfhe_proof.Bound_zero_receipt_v1 ->
    true
  | Some Circle_hfhe_proof.No_proof
  | Some Circle_hfhe_proof.Range_v1
  | Some Circle_hfhe_proof.Bound_zero_v1
  | None ->
    false

let normalize_optional_proof_kind = function
  | Some Circle_hfhe_proof.No_proof
  | None ->
    Ok None
  | Some value ->
    Ok (Some value)

let validate_proof_receipt_binding ~proof_kind ~proof_receipt_hash =
  if proof_kind_requires_receipt proof_kind then
    begin
      match proof_receipt_hash with
      | Some _ -> Ok ()
      | None -> Error "proof_receipt_hash required for receipt proof kind"
    end
  else
    begin
      match proof_receipt_hash with
      | Some _ -> Error "proof_receipt_hash requires a receipt proof kind"
      | None -> Ok ()
    end

let descriptor_payload_of_resolved state_ref (descriptor : Circle_state_descriptor.t) =
  {
    Circle_state_descriptor.state_ref;
    state_class = Some descriptor.state_class;
    codec = Some descriptor.codec;
    schema_hash = descriptor.schema_hash;
    subject_addr = descriptor.subject_addr;
    hfhe_profile = Some descriptor.hfhe_profile;
    mutable_state = Some descriptor.mutable_state;
  }

let state_policy_payload_of_resolved
    state_ref
    delivery_key_id
    activate_after_epoch
    expire_after_epoch =
  {
    Circles.slot_ref = None;
    state_ref = Some state_ref;
    delivery_key_id;
    activate_after_epoch;
    expire_after_epoch;
    tombstone = false;
    revoked = false;
  }

let read_required_string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok value
  | _ -> Error ("missing " ^ name)

let read_optional_string_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) -> Ok (Some value)
  | Some `Null
  | None ->
    Ok None
  | _ ->
    Error ("invalid " ^ name)

let read_optional_i64_field name fields =
  match List.assoc_opt name fields with
  | Some (`String value) ->
    begin
      try Ok (Some (Int64.of_string value))
      with _ -> Error ("invalid " ^ name)
    end
  | Some (`Int value) ->
    Ok (Some (Int64.of_int value))
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

let read_optional_metadata_mode_field name fields =
  read_optional_enum_field name fields Circles.metadata_mode_of_string