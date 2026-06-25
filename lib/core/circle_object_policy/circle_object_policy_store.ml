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

let load_opt store circle_id object_ref =
  let* delivery_key_id_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.delivery_key_id_key object_ref] in
  let* activate_after_epoch_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.activate_after_epoch_key object_ref] in
  let* expire_after_epoch_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.expire_after_epoch_key object_ref] in
  let* tombstone_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.tombstone_key object_ref] in
  let* revoked_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.revoked_key object_ref] in
  let* transition_mode_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.transition_mode_key object_ref] in
  let* required_proof_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.required_proof_kind_key object_ref] in
  let* member_quorum_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.member_quorum_key object_ref] in
  let* allow_detach_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.allow_detach_key object_ref] in
  let* allow_root_state_rotation_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_policy.allow_root_state_rotation_key object_ref] in
  let policy =
    Circle_object_policy.of_stored_values
      ~delivery_key_id_raw
      ~activate_after_epoch_raw
      ~expire_after_epoch_raw
      ~tombstone_raw
      ~revoked_raw
      ~transition_mode_raw
      ~required_proof_kind_raw
      ~member_quorum_raw
      ~allow_detach_raw
      ~allow_root_state_rotation_raw in
  if Circle_object_policy.materialized policy then
    Lwt.return (Some policy)
  else
    Lwt.return_none

let load store circle_id object_ref =
  let* policy_opt = load_opt store circle_id object_ref in
  match policy_opt with
  | Some policy -> Lwt.return policy
  | None -> Lwt.return Circle_object_policy.empty