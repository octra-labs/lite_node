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
  object_ref : string option;
  previous_state_ref : string option;
  next_state_ref : string option;
  touched_members_hash : string option;
  proof_kind : Circle_hfhe_proof.t option;
  proof_receipt_hash : string option;
  status : string option;
  intent_id : string option;
}

let empty = {
  object_ref = None;
  previous_state_ref = None;
  next_state_ref = None;
  touched_members_hash = None;
  proof_kind = None;
  proof_receipt_hash = None;
  status = None;
  intent_id = None;
}

let base_key transition_ref suffix =
  "object_transition:" ^ transition_ref ^ ":" ^ suffix

let object_ref_key transition_ref =
  base_key transition_ref "object_ref"

let previous_state_ref_key transition_ref =
  base_key transition_ref "previous_state_ref"

let next_state_ref_key transition_ref =
  base_key transition_ref "next_state_ref"

let touched_members_hash_key transition_ref =
  base_key transition_ref "touched_members_hash"

let proof_kind_key transition_ref =
  base_key transition_ref "proof_kind"

let proof_receipt_hash_key transition_ref =
  base_key transition_ref "proof_receipt_hash"

let status_key transition_ref =
  base_key transition_ref "status"

let intent_id_key transition_ref =
  base_key transition_ref "intent_id"

let yojson_of_t (transition : t) =
  `Assoc [
    "object_ref",
    begin
      match transition.object_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "previous_state_ref",
    begin
      match transition.previous_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "next_state_ref",
    begin
      match transition.next_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "touched_members_hash",
    begin
      match transition.touched_members_hash with
      | Some value -> `String value
      | None -> `Null
    end;
    "proof_kind",
    begin
      match transition.proof_kind with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
    "proof_receipt_hash",
    begin
      match transition.proof_receipt_hash with
      | Some value -> `String value
      | None -> `Null
    end;
    "status",
    begin
      match transition.status with
      | Some value -> `String value
      | None -> `Null
    end;
    "intent_id",
    begin
      match transition.intent_id with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let write_snapshot storage_tbl transition_ref (transition : t) =
  let open Circle_policy_value in
  set_or_clear storage_tbl (object_ref_key transition_ref) transition.object_ref;
  set_or_clear
    storage_tbl
    (previous_state_ref_key transition_ref)
    transition.previous_state_ref;
  set_or_clear
    storage_tbl
    (next_state_ref_key transition_ref)
    transition.next_state_ref;
  set_or_clear
    storage_tbl
    (touched_members_hash_key transition_ref)
    transition.touched_members_hash;
  set_or_clear
    storage_tbl
    (proof_kind_key transition_ref)
    (Option.map Circle_hfhe_proof.string_of_t transition.proof_kind);
  set_or_clear
    storage_tbl
    (proof_receipt_hash_key transition_ref)
    transition.proof_receipt_hash;
  set_or_clear
    storage_tbl
    (status_key transition_ref)
    transition.status;
  set_or_clear
    storage_tbl
    (intent_id_key transition_ref)
    transition.intent_id

let materialized (transition : t) =
  Option.is_some transition.object_ref
  || Option.is_some transition.previous_state_ref
  || Option.is_some transition.next_state_ref
  || Option.is_some transition.touched_members_hash
  || Option.is_some transition.proof_kind
  || Option.is_some transition.proof_receipt_hash
  || Option.is_some transition.status
  || Option.is_some transition.intent_id

let of_stored_values
    ~object_ref_raw
    ~previous_state_ref_raw
    ~next_state_ref_raw
    ~touched_members_hash_raw
    ~proof_kind_raw
    ~proof_receipt_hash_raw
    ~status_raw
    ~intent_id_raw =
  let parse_hex64 = function
    | Some value ->
      begin
        match Circle_private_common.normalize_hex64 "field" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  let parse_state_ref = function
    | Some value ->
      begin
        match Circles.normalize_state_ref value with
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
            | Ok proof_kind -> proof_kind
            | Error _ -> None
          end
        | Error _ -> None
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
  {
    object_ref = parse_hex64 object_ref_raw;
    previous_state_ref = parse_state_ref previous_state_ref_raw;
    next_state_ref = parse_state_ref next_state_ref_raw;
    touched_members_hash = parse_hex64 touched_members_hash_raw;
    proof_kind = parse_proof_kind proof_kind_raw;
    proof_receipt_hash = parse_hex64 proof_receipt_hash_raw;
    status = parse_status status_raw;
    intent_id = parse_hex64 intent_id_raw;
  }

let runtime_suffix_allowed = function
  | "object_ref"
  | "previous_state_ref"
  | "next_state_ref"
  | "touched_members_hash"
  | "proof_kind"
  | "proof_receipt_hash"
  | "status"
  | "intent_id" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; transition_ref; suffix] when raw_prefix = "object_transition" ->
    begin
      match Circle_private_common.normalize_hex64 "transition_ref" transition_ref with
      | Error _ ->
        Error ("circle_runtime_invalid_object_transition_key", raw_key, "transition_ref")
      | Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_object_transition_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "object_ref"
            | "touched_members_hash"
            | "proof_receipt_hash"
            | "intent_id" ->
              begin
                match Circle_private_common.normalize_hex64 suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_transition_value", raw_key, suffix)
              end
            | "previous_state_ref"
            | "next_state_ref" ->
              begin
                match Circles.normalize_state_ref value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_transition_value", raw_key, suffix)
              end
            | "proof_kind" ->
              begin
                match Circle_hfhe_proof.of_string (String.trim value) with
                | Ok proof ->
                  begin
                    match Circle_private_common.normalize_optional_proof_kind (Some proof) with
                    | Ok _ -> Ok ()
                    | Error _ -> Error ("circle_runtime_invalid_object_transition_value", raw_key, suffix)
                  end
                | Error _ -> Error ("circle_runtime_invalid_object_transition_value", raw_key, suffix)
              end
            | "status" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_object_transition_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_object_transition_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_object_transition_key", raw_key, "shape")