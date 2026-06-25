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

let load_opt store circle_id subject_addr =
  let* current_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_binding.current_state_ref_key subject_addr] in
  let* version_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_binding.version_key subject_addr] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_binding.status_key subject_addr] in
  let* last_workflow_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_balance_binding.last_workflow_ref_key subject_addr] in
  let binding =
    Circle_balance_binding.of_stored_values
      ~current_state_ref_raw
      ~version_raw
      ~status_raw
      ~last_workflow_ref_raw in
  if Circle_balance_binding.materialized binding then
    Lwt.return (Some binding)
  else
    Lwt.return_none

let load store circle_id subject_addr =
  let* binding_opt = load_opt store circle_id subject_addr in
  match binding_opt with
  | Some binding -> Lwt.return binding
  | None -> Lwt.return Circle_balance_binding.empty