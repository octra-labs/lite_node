(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

let load_opt store circle_id register_ref =
  let* current_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_binding.current_state_ref_key register_ref] in
  let* version_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_binding.version_key register_ref] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_binding.status_key register_ref] in
  let* last_workflow_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_register_binding.last_workflow_ref_key register_ref] in
  let binding =
    Circle_register_binding.of_stored_values
      ~current_state_ref_raw
      ~version_raw
      ~status_raw
      ~last_workflow_ref_raw in
  if Circle_register_binding.materialized binding then
    Lwt.return (Some binding)
  else
    Lwt.return_none

let load store circle_id register_ref =
  let* binding_opt = load_opt store circle_id register_ref in
  match binding_opt with
  | Some binding -> Lwt.return binding
  | None -> Lwt.return Circle_register_binding.empty