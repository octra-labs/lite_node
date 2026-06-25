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


open Lwt.Syntax

let load_opt store circle_id workflow_ref =
  let* flow_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.flow_kind_key workflow_ref] in
  let* debit_subject_addr_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.debit_subject_addr_key workflow_ref] in
  let* credit_subject_addr_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.credit_subject_addr_key workflow_ref] in
  let* debit_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.debit_state_ref_key workflow_ref] in
  let* credit_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.credit_state_ref_key workflow_ref] in
  let* amount_commitment_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.amount_commitment_key workflow_ref] in
  let* proof_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.proof_kind_key workflow_ref] in
  let* proof_receipt_hash_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.proof_receipt_hash_key workflow_ref] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.status_key workflow_ref] in
  let* intent_id_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_workflow.intent_id_key workflow_ref] in
  let workflow =
    Circle_balance_workflow.of_stored_values
      ~flow_kind_raw
      ~debit_subject_addr_raw
      ~credit_subject_addr_raw
      ~debit_state_ref_raw
      ~credit_state_ref_raw
      ~amount_commitment_raw
      ~proof_kind_raw
      ~proof_receipt_hash_raw
      ~status_raw
      ~intent_id_raw in
  if Circle_balance_workflow.materialized workflow then
    Lwt.return (Some workflow)
  else
    Lwt.return_none

let load store circle_id workflow_ref =
  let* workflow_opt = load_opt store circle_id workflow_ref in
  match workflow_opt with
  | Some workflow -> Lwt.return workflow
  | None -> Lwt.return Circle_balance_workflow.empty