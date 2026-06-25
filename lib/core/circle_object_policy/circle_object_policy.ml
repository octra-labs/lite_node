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
  delivery_key_id : string option;
  activate_after_epoch : int64 option;
  expire_after_epoch : int64 option;
  tombstone : bool option;
  revoked : bool option;
  transition_mode : Circle_object_transition_mode.t option;
  required_proof_kind : Circle_hfhe_proof.t option;
  member_quorum : int64 option;
  allow_detach : bool option;
  allow_root_state_rotation : bool option;
}

let empty = {
  delivery_key_id = None;
  activate_after_epoch = None;
  expire_after_epoch = None;
  tombstone = None;
  revoked = None;
  transition_mode = None;
  required_proof_kind = None;
  member_quorum = None;
  allow_detach = None;
  allow_root_state_rotation = None;
}

let base_key object_ref suffix =
  "object_policy:" ^ object_ref ^ ":" ^ suffix

let delivery_key_id_key object_ref =
  base_key object_ref "delivery_key_id"

let activate_after_epoch_key object_ref =
  base_key object_ref "activate_after_epoch"

let expire_after_epoch_key object_ref =
  base_key object_ref "expire_after_epoch"

let tombstone_key object_ref =
  base_key object_ref "tombstone"

let revoked_key object_ref =
  base_key object_ref "revoked"

let transition_mode_key object_ref =
  base_key object_ref "transition_mode"

let required_proof_kind_key object_ref =
  base_key object_ref "required_proof_kind"

let member_quorum_key object_ref =
  base_key object_ref "member_quorum"

let allow_detach_key object_ref =
  base_key object_ref "allow_detach"

let allow_root_state_rotation_key object_ref =
  base_key object_ref "allow_root_state_rotation"

let yojson_of_t (policy : t) =
  `Assoc [
    "delivery_key_id",
    begin
      match policy.delivery_key_id with
      | Some value -> `String value
      | None -> `Null
    end;
    "activate_after_epoch",
    begin
      match policy.activate_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "expire_after_epoch",
    begin
      match policy.expire_after_epoch with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "tombstone",
    begin
      match policy.tombstone with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "revoked",
    begin
      match policy.revoked with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "transition_mode",
    begin
      match policy.transition_mode with
      | Some value -> `String (Circle_object_transition_mode.string_of_t value)
      | None -> `Null
    end;
    "required_proof_kind",
    begin
      match policy.required_proof_kind with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
    "member_quorum",
    begin
      match policy.member_quorum with
      | Some value -> `String (Int64.to_string value)
      | None -> `Null
    end;
    "allow_detach",
    begin
      match policy.allow_detach with
      | Some value -> `Bool value
      | None -> `Null
    end;
    "allow_root_state_rotation",
    begin
      match policy.allow_root_state_rotation with
      | Some value -> `Bool value
      | None -> `Null
    end;
  ]

let write_snapshot storage_tbl object_ref (policy : t) =
  let open Circle_policy_value in
  set_or_clear
    storage_tbl
    (delivery_key_id_key object_ref)
    policy.delivery_key_id;
  set_or_clear
    storage_tbl
    (activate_after_epoch_key object_ref)
    (Option.map Int64.to_string policy.activate_after_epoch);
  set_or_clear
    storage_tbl
    (expire_after_epoch_key object_ref)
    (Option.map Int64.to_string policy.expire_after_epoch);
  set_or_clear
    storage_tbl
    (tombstone_key object_ref)
    (Option.map string_of_bool policy.tombstone);
  set_or_clear
    storage_tbl
    (revoked_key object_ref)
    (Option.map string_of_bool policy.revoked);
  set_or_clear
    storage_tbl
    (transition_mode_key object_ref)
    (Option.map Circle_object_transition_mode.string_of_t policy.transition_mode);
  set_or_clear
    storage_tbl
    (required_proof_kind_key object_ref)
    (Option.map Circle_hfhe_proof.string_of_t policy.required_proof_kind);
  set_or_clear
    storage_tbl
    (member_quorum_key object_ref)
    (Option.map Int64.to_string policy.member_quorum);
  set_or_clear
    storage_tbl
    (allow_detach_key object_ref)
    (Option.map string_of_bool policy.allow_detach);
  set_or_clear
    storage_tbl
    (allow_root_state_rotation_key object_ref)
    (Option.map string_of_bool policy.allow_root_state_rotation)

let materialized (policy : t) =
  Option.is_some policy.delivery_key_id
  || Option.is_some policy.activate_after_epoch
  || Option.is_some policy.expire_after_epoch
  || Option.is_some policy.tombstone
  || Option.is_some policy.revoked
  || Option.is_some policy.transition_mode
  || Option.is_some policy.required_proof_kind
  || Option.is_some policy.member_quorum
  || Option.is_some policy.allow_detach
  || Option.is_some policy.allow_root_state_rotation

let transition_controls_materialized (policy : t) =
  Option.is_some policy.transition_mode
  && Option.is_some policy.allow_detach
  && Option.is_some policy.allow_root_state_rotation

let parse_i64 = function
  | Some value ->
    begin
      try
        let parsed = Int64.of_string (String.trim value) in
        if Int64.compare parsed 0L < 0 then None else Some parsed
      with _ ->
        None
    end
  | None -> None

let parse_bool = function
  | Some value ->
    let normalized = String.lowercase_ascii (String.trim value) in
    begin
      match normalized with
      | "1"
      | "true"
      | "yes" ->
        Some true
      | "0"
      | "false"
      | "no" ->
        Some false
      | _ ->
        None
    end
  | None ->
    None

let of_stored_values
    ~delivery_key_id_raw
    ~activate_after_epoch_raw
    ~expire_after_epoch_raw
    ~tombstone_raw
    ~revoked_raw
    ~transition_mode_raw
    ~required_proof_kind_raw
    ~member_quorum_raw
    ~allow_detach_raw
    ~allow_root_state_rotation_raw =
  let parse_delivery_key = function
    | Some value ->
      begin
        match Circle_private_common.normalize_non_empty "delivery_key_id" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None ->
      None
  in
  let parse_transition_mode = function
    | Some value ->
      begin
        match Circle_object_transition_mode.of_string (String.trim value) with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None ->
      None
  in
  let parse_required_proof_kind = function
    | Some value ->
      begin
        match Circle_hfhe_proof.of_string (String.trim value) with
        | Ok proof ->
          begin
            match Circle_private_common.normalize_optional_proof_kind (Some proof) with
            | Ok proof_kind -> proof_kind
            | Error _ -> None
          end
        | Error _ -> None
      end
    | None ->
      None
  in
  {
    delivery_key_id = parse_delivery_key delivery_key_id_raw;
    activate_after_epoch = parse_i64 activate_after_epoch_raw;
    expire_after_epoch = parse_i64 expire_after_epoch_raw;
    tombstone = parse_bool tombstone_raw;
    revoked = parse_bool revoked_raw;
    transition_mode = parse_transition_mode transition_mode_raw;
    required_proof_kind = parse_required_proof_kind required_proof_kind_raw;
    member_quorum = parse_i64 member_quorum_raw;
    allow_detach = parse_bool allow_detach_raw;
    allow_root_state_rotation = parse_bool allow_root_state_rotation_raw;
  }

let runtime_suffix_allowed = function
  | "delivery_key_id"
  | "activate_after_epoch"
  | "expire_after_epoch"
  | "tombstone"
  | "revoked"
  | "transition_mode"
  | "required_proof_kind"
  | "member_quorum"
  | "allow_detach"
  | "allow_root_state_rotation" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; object_ref; suffix] when raw_prefix = "object_policy" ->
    begin
      match Circle_private_common.normalize_hex64 "object_ref" object_ref with
      | Error _ ->
        Error ("circle_runtime_invalid_object_policy_key", raw_key, "object_ref")
      | Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_object_policy_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "delivery_key_id" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
              end
            | "activate_after_epoch"
            | "expire_after_epoch"
            | "member_quorum" ->
              begin
                try
                  let parsed = Int64.of_string (String.trim value) in
                  if Int64.compare parsed 0L < 0 then
                    Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
                  else
                    Ok ()
                with _ ->
                  Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
              end
            | "tombstone"
            | "revoked"
            | "allow_detach"
            | "allow_root_state_rotation" ->
              begin
                let normalized = String.lowercase_ascii (String.trim value) in
                match normalized with
                | "1"
                | "0"
                | "true"
                | "false"
                | "yes"
                | "no" ->
                  Ok ()
                | _ ->
                  Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
              end
            | "transition_mode" ->
              begin
                match Circle_object_transition_mode.of_string (String.trim value) with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
              end
            | "required_proof_kind" ->
              begin
                match Circle_hfhe_proof.of_string (String.trim value) with
                | Ok proof ->
                  begin
                    match Circle_private_common.normalize_optional_proof_kind (Some proof) with
                    | Ok _ -> Ok ()
                    | Error _ -> Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
                  end
                | Error _ -> Error ("circle_runtime_invalid_object_policy_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_object_policy_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_object_policy_key", raw_key, "shape")