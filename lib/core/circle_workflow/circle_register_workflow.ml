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
  register_ref : string option;
  previous_state_ref : string option;
  next_state_ref : string option;
  workflow_kind : string option;
  proof_kind : Circle_hfhe_proof.t option;
  proof_receipt_hash : string option;
  status : string option;
  intent_id : string option;
}

let empty = {
  register_ref = None;
  previous_state_ref = None;
  next_state_ref = None;
  workflow_kind = None;
  proof_kind = None;
  proof_receipt_hash = None;
  status = None;
  intent_id = None;
}

let base_key workflow_ref suffix =
  "register_workflow:" ^ workflow_ref ^ ":" ^ suffix

let register_ref_key workflow_ref =
  base_key workflow_ref "register_ref"

let previous_state_ref_key workflow_ref =
  base_key workflow_ref "previous_state_ref"

let next_state_ref_key workflow_ref =
  base_key workflow_ref "next_state_ref"

let workflow_kind_key workflow_ref =
  base_key workflow_ref "workflow_kind"

let proof_kind_key workflow_ref =
  base_key workflow_ref "proof_kind"

let proof_receipt_hash_key workflow_ref =
  base_key workflow_ref "proof_receipt_hash"

let status_key workflow_ref =
  base_key workflow_ref "status"

let intent_id_key workflow_ref =
  base_key workflow_ref "intent_id"

let yojson_of_t (workflow : t) =
  `Assoc [
    "register_ref",
    begin
      match workflow.register_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "previous_state_ref",
    begin
      match workflow.previous_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "next_state_ref",
    begin
      match workflow.next_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "workflow_kind",
    begin
      match workflow.workflow_kind with
      | Some value -> `String value
      | None -> `Null
    end;
    "proof_kind",
    begin
      match workflow.proof_kind with
      | Some value -> `String (Circle_hfhe_proof.string_of_t value)
      | None -> `Null
    end;
    "proof_receipt_hash",
    begin
      match workflow.proof_receipt_hash with
      | Some value -> `String value
      | None -> `Null
    end;
    "status",
    begin
      match workflow.status with
      | Some value -> `String value
      | None -> `Null
    end;
    "intent_id",
    begin
      match workflow.intent_id with
      | Some value -> `String value
      | None -> `Null
    end;
  ]

let write_snapshot storage_tbl workflow_ref (workflow : t) =
  let open Circle_policy_value in
  set_or_clear storage_tbl (register_ref_key workflow_ref) workflow.register_ref;
  set_or_clear storage_tbl (previous_state_ref_key workflow_ref) workflow.previous_state_ref;
  set_or_clear storage_tbl (next_state_ref_key workflow_ref) workflow.next_state_ref;
  set_or_clear storage_tbl (workflow_kind_key workflow_ref) workflow.workflow_kind;
  set_or_clear
    storage_tbl
    (proof_kind_key workflow_ref)
    (Option.map Circle_hfhe_proof.string_of_t workflow.proof_kind);
  set_or_clear
    storage_tbl
    (proof_receipt_hash_key workflow_ref)
    workflow.proof_receipt_hash;
  set_or_clear storage_tbl (status_key workflow_ref) workflow.status;
  set_or_clear storage_tbl (intent_id_key workflow_ref) workflow.intent_id

let materialized (workflow : t) =
  Option.is_some workflow.register_ref
  || Option.is_some workflow.previous_state_ref
  || Option.is_some workflow.next_state_ref
  || Option.is_some workflow.workflow_kind
  || Option.is_some workflow.proof_kind
  || Option.is_some workflow.proof_receipt_hash
  || Option.is_some workflow.status
  || Option.is_some workflow.intent_id

let of_stored_values
    ~register_ref_raw
    ~previous_state_ref_raw
    ~next_state_ref_raw
    ~workflow_kind_raw
    ~proof_kind_raw
    ~proof_receipt_hash_raw
    ~status_raw
    ~intent_id_raw =
  let parse_non_empty = function
    | Some value ->
      begin
        match Circle_private_common.normalize_non_empty "field" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
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
  {
    register_ref = parse_hex64 register_ref_raw;
    previous_state_ref = parse_state_ref previous_state_ref_raw;
    next_state_ref = parse_state_ref next_state_ref_raw;
    workflow_kind = parse_non_empty workflow_kind_raw;
    proof_kind = parse_proof_kind proof_kind_raw;
    proof_receipt_hash = parse_hex64 proof_receipt_hash_raw;
    status = parse_non_empty status_raw;
    intent_id = parse_hex64 intent_id_raw;
  }

let runtime_suffix_allowed = function
  | "register_ref"
  | "previous_state_ref"
  | "next_state_ref"
  | "workflow_kind"
  | "proof_kind"
  | "proof_receipt_hash"
  | "status"
  | "intent_id" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; workflow_ref; suffix] when raw_prefix = "register_workflow" ->
    begin
      match Circle_private_common.normalize_hex64 "workflow_ref" workflow_ref with
      | Error _ ->
        Error ("circle_runtime_invalid_register_workflow_key", raw_key, "workflow_ref")
      | Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_register_workflow_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "register_ref"
            | "proof_receipt_hash"
            | "intent_id" ->
              begin
                match Circle_private_common.normalize_hex64 suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_register_workflow_value", raw_key, suffix)
              end
            | "previous_state_ref"
            | "next_state_ref" ->
              begin
                match Circles.normalize_state_ref value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_register_workflow_value", raw_key, suffix)
              end
            | "workflow_kind"
            | "status" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_register_workflow_value", raw_key, suffix)
              end
            | "proof_kind" ->
              begin
                match Circle_hfhe_proof.of_string (String.trim value) with
                | Ok proof ->
                  begin
                    match Circle_private_common.normalize_optional_proof_kind (Some proof) with
                    | Ok _ -> Ok ()
                    | Error _ -> Error ("circle_runtime_invalid_register_workflow_value", raw_key, suffix)
                  end
                | Error _ ->
                  Error ("circle_runtime_invalid_register_workflow_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_register_workflow_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_register_workflow_key", raw_key, "shape")