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

let load_opt store circle_id object_ref member_ref =
  let* state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_member.state_ref_key object_ref member_ref] in
  let* member_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_member.member_kind_key object_ref member_ref] in
  let* state_class_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_member.state_class_key object_ref member_ref] in
  let* codec_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_member.codec_key object_ref member_ref] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_member.status_key object_ref member_ref] in
  let member_state =
    Circle_object_member.of_stored_values
      ~state_ref_raw
      ~member_kind_raw
      ~state_class_raw
      ~codec_raw
      ~status_raw in
  if Circle_object_member.materialized member_state then
    Lwt.return (Some member_state)
  else
    Lwt.return_none

let load store circle_id object_ref member_ref =
  let* member_state_opt = load_opt store circle_id object_ref member_ref in
  match member_state_opt with
  | Some member_state -> Lwt.return member_state
  | None -> Lwt.return Circle_object_member.empty