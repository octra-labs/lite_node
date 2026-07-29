(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let load_descriptor_opt store circle_id path_key =
  let open Lwt.Syntax in
  let* state_class_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_state_descriptor.state_class_key path_key] in
  let* codec_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_state_descriptor.codec_key path_key] in
  let* schema_hash_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_state_descriptor.schema_hash_key path_key] in
  let* subject_addr_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_state_descriptor.subject_addr_key path_key] in
  let* hfhe_profile_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_state_descriptor.hfhe_profile_key path_key] in
  let* mutable_state_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_state_descriptor.mutable_state_key path_key] in
  if
    state_class_raw = None
    && codec_raw = None
    && schema_hash_raw = None
    && subject_addr_raw = None
    && hfhe_profile_raw = None
    && mutable_state_raw = None
  then
    Lwt.return_none
  else
    Lwt.return
      (Some
         (Circle_state_descriptor.of_stored_values
            ~state_class_raw
            ~codec_raw
            ~schema_hash_raw
            ~subject_addr_raw
            ~hfhe_profile_raw
            ~mutable_state_raw))

let load_descriptor store circle_id path_key =
  let open Lwt.Syntax in
  let* descriptor_opt = load_descriptor_opt store circle_id path_key in
  match descriptor_opt with
  | Some descriptor -> Lwt.return descriptor
  | None -> Lwt.return Circle_state_descriptor.default