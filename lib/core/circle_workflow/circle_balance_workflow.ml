(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  flow_kind : string option;
  debit_subject_addr : string option;
  credit_subject_addr : string option;
  debit_state_ref : string option;
  credit_state_ref : string option;
  amount_commitment : string option;
  proof_kind : Circle_hfhe_proof.t option;
  proof_receipt_hash : string option;
  status : string option;
  intent_id : string option;
}

let empty = {
  flow_kind = None;
  debit_subject_addr = None;
  credit_subject_addr = None;
  debit_state_ref = None;
  credit_state_ref = None;
  amount_commitment = None;
  proof_kind = None;
  proof_receipt_hash = None;
  status = None;
  intent_id = None;
}

let base_key workflow_ref suffix =
  "balance_workflow:" ^ workflow_ref ^ ":" ^ suffix

let flow_kind_key workflow_ref =
  base_key workflow_ref "flow_kind"

let debit_subject_addr_key workflow_ref =
  base_key workflow_ref "debit_subject_addr"

let credit_subject_addr_key workflow_ref =
  base_key workflow_ref "credit_subject_addr"

let debit_state_ref_key workflow_ref =
  base_key workflow_ref "debit_state_ref"

let credit_state_ref_key workflow_ref =
  base_key workflow_ref "credit_state_ref"

let amount_commitment_key workflow_ref =
  base_key workflow_ref "amount_commitment"

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
    "flow_kind",
    begin
      match workflow.flow_kind with
      | Some value -> `String value
      | None -> `Null
    end;
    "debit_subject_addr",
    begin
      match workflow.debit_subject_addr with
      | Some value -> `String value
      | None -> `Null
    end;
    "credit_subject_addr",
    begin
      match workflow.credit_subject_addr with
      | Some value -> `String value
      | None -> `Null
    end;
    "debit_state_ref",
    begin
      match workflow.debit_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "credit_state_ref",
    begin
      match workflow.credit_state_ref with
      | Some value -> `String value
      | None -> `Null
    end;
    "amount_commitment",
    begin
      match workflow.amount_commitment with
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
  set_or_clear storage_tbl (flow_kind_key workflow_ref) workflow.flow_kind;
  set_or_clear storage_tbl (debit_subject_addr_key workflow_ref) workflow.debit_subject_addr;
  set_or_clear storage_tbl (credit_subject_addr_key workflow_ref) workflow.credit_subject_addr;
  set_or_clear storage_tbl (debit_state_ref_key workflow_ref) workflow.debit_state_ref;
  set_or_clear storage_tbl (credit_state_ref_key workflow_ref) workflow.credit_state_ref;
  set_or_clear storage_tbl (amount_commitment_key workflow_ref) workflow.amount_commitment;
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
  Option.is_some workflow.flow_kind
  || Option.is_some workflow.debit_subject_addr
  || Option.is_some workflow.credit_subject_addr
  || Option.is_some workflow.debit_state_ref
  || Option.is_some workflow.credit_state_ref
  || Option.is_some workflow.amount_commitment
  || Option.is_some workflow.proof_kind
  || Option.is_some workflow.proof_receipt_hash
  || Option.is_some workflow.status
  || Option.is_some workflow.intent_id

let of_stored_values
    ~flow_kind_raw
    ~debit_subject_addr_raw
    ~credit_subject_addr_raw
    ~debit_state_ref_raw
    ~credit_state_ref_raw
    ~amount_commitment_raw
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
  let parse_addr = function
    | Some value when Crypto.Address.is_valid_address (String.trim value) ->
      Some (String.trim value)
    | Some _ -> None
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
  let parse_commitment = function
    | Some value ->
      begin
        match Circle_private_common.normalize_commitment "amount_commitment" value with
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
  let parse_receipt_hash = function
    | Some value ->
      begin
        match Circle_private_common.normalize_hex64 "proof_receipt_hash" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  let parse_intent_id = function
    | Some value ->
      begin
        match Circle_private_common.normalize_hex64 "intent_id" value with
        | Ok normalized -> Some normalized
        | Error _ -> None
      end
    | None -> None
  in
  {
    flow_kind = parse_non_empty flow_kind_raw;
    debit_subject_addr = parse_addr debit_subject_addr_raw;
    credit_subject_addr = parse_addr credit_subject_addr_raw;
    debit_state_ref = parse_state_ref debit_state_ref_raw;
    credit_state_ref = parse_state_ref credit_state_ref_raw;
    amount_commitment = parse_commitment amount_commitment_raw;
    proof_kind = parse_proof_kind proof_kind_raw;
    proof_receipt_hash = parse_receipt_hash proof_receipt_hash_raw;
    status = parse_non_empty status_raw;
    intent_id = parse_intent_id intent_id_raw;
  }

let runtime_suffix_allowed = function
  | "flow_kind"
  | "debit_subject_addr"
  | "credit_subject_addr"
  | "debit_state_ref"
  | "credit_state_ref"
  | "amount_commitment"
  | "proof_kind"
  | "proof_receipt_hash"
  | "status"
  | "intent_id" ->
    true
  | _ ->
    false

let validate_runtime_key raw_key value =
  match String.split_on_char ':' raw_key with
  | [raw_prefix; workflow_ref; suffix] when raw_prefix = "balance_workflow" ->
    begin
      match Circle_private_common.normalize_hex64 "workflow_ref" workflow_ref with
      | Error _ ->
        Error ("circle_runtime_invalid_balance_workflow_key", raw_key, "workflow_ref")
      | Ok _ ->
        if not (runtime_suffix_allowed suffix) then
          Error ("circle_runtime_invalid_balance_workflow_key", raw_key, suffix)
        else
          begin
            match suffix with
            | "flow_kind"
            | "status" ->
              begin
                match Circle_private_common.normalize_non_empty suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
              end
            | "debit_subject_addr"
            | "credit_subject_addr" ->
              if Crypto.Address.is_valid_address (String.trim value) then
                Ok ()
              else
                Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
            | "debit_state_ref"
            | "credit_state_ref" ->
              begin
                match Circles.normalize_state_ref value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
              end
            | "amount_commitment" ->
              begin
                match Circle_private_common.normalize_commitment suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
              end
            | "proof_kind" ->
              begin
                match Circle_hfhe_proof.of_string (String.trim value) with
                | Ok proof ->
                  begin
                    match Circle_private_common.normalize_optional_proof_kind (Some proof) with
                    | Ok _ -> Ok ()
                    | Error _ -> Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
                  end
                | Error _ ->
                  Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
              end
            | "proof_receipt_hash"
            | "intent_id" ->
              begin
                match Circle_private_common.normalize_hex64 suffix value with
                | Ok _ -> Ok ()
                | Error _ -> Error ("circle_runtime_invalid_balance_workflow_value", raw_key, suffix)
              end
            | _ ->
              Error ("circle_runtime_invalid_balance_workflow_key", raw_key, suffix)
          end
    end
  | _ ->
    Error ("circle_runtime_invalid_balance_workflow_key", raw_key, "shape")