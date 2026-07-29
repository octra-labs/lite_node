(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Lwt.Syntax

let load_opt store circle_id transition_ref =
  let* object_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.object_ref_key transition_ref] in
  let* previous_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.previous_state_ref_key transition_ref] in
  let* next_state_ref_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.next_state_ref_key transition_ref] in
  let* touched_members_hash_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.touched_members_hash_key transition_ref] in
  let* proof_kind_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.proof_kind_key transition_ref] in
  let* proof_receipt_hash_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.proof_receipt_hash_key transition_ref] in
  let* status_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.status_key transition_ref] in
  let* intent_id_raw =
    Circle_policy_store.read_first_inline_value
      store
      circle_id
      [Circle_object_transition.intent_id_key transition_ref] in
  let transition =
    Circle_object_transition.of_stored_values
      ~object_ref_raw
      ~previous_state_ref_raw
      ~next_state_ref_raw
      ~touched_members_hash_raw
      ~proof_kind_raw
      ~proof_receipt_hash_raw
      ~status_raw
      ~intent_id_raw in
  if Circle_object_transition.materialized transition then
    Lwt.return (Some transition)
  else
    Lwt.return_none

let load store circle_id transition_ref =
  let* transition_opt = load_opt store circle_id transition_ref in
  match transition_opt with
  | Some transition -> Lwt.return transition
  | None -> Lwt.return Circle_object_transition.empty