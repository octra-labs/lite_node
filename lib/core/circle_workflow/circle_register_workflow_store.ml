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
  let* register_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.register_ref_key workflow_ref] in
  let* previous_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.previous_state_ref_key workflow_ref] in
  let* next_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.next_state_ref_key workflow_ref] in
  let* workflow_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.workflow_kind_key workflow_ref] in
  let* proof_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.proof_kind_key workflow_ref] in
  let* proof_receipt_hash_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.proof_receipt_hash_key workflow_ref] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.status_key workflow_ref] in
  let* intent_id_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_workflow.intent_id_key workflow_ref] in
  let workflow =
    Circle_register_workflow.of_stored_values
      ~register_ref_raw
      ~previous_state_ref_raw
      ~next_state_ref_raw
      ~workflow_kind_raw
      ~proof_kind_raw
      ~proof_receipt_hash_raw
      ~status_raw
      ~intent_id_raw in
  if Circle_register_workflow.materialized workflow then
    Lwt.return (Some workflow)
  else
    Lwt.return_none

let load store circle_id workflow_ref =
  let* workflow_opt = load_opt store circle_id workflow_ref in
  match workflow_opt with
  | Some workflow -> Lwt.return workflow
  | None -> Lwt.return Circle_register_workflow.empty