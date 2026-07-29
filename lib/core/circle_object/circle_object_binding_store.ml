(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

let load_opt store circle_id object_ref =
  let* current_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_binding.current_state_ref_key object_ref] in
  let* version_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_binding.version_key object_ref] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_binding.status_key object_ref] in
  let* last_transition_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_binding.last_transition_ref_key object_ref] in
  let binding =
    Circle_object_binding.of_stored_values
      ~current_state_ref_raw
      ~version_raw
      ~status_raw
      ~last_transition_ref_raw in
  if Circle_object_binding.materialized binding then
    Lwt.return (Some binding)
  else
    Lwt.return_none

let load store circle_id object_ref =
  let* binding_opt = load_opt store circle_id object_ref in
  match binding_opt with
  | Some binding -> Lwt.return binding
  | None -> Lwt.return Circle_object_binding.empty