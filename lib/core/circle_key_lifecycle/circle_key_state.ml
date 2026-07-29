(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Scheduled
  | Live
  | Expired
  | Revoked
  | Erased

let string_of_t = function
  | Scheduled -> "scheduled"
  | Live -> "live"
  | Expired -> "expired"
  | Revoked -> "revoked"
  | Erased -> "erased"

let classify (policy : Circle_key_policy.t) current_epoch =
  if policy.erased then
    Erased
  else if policy.revoked then
    Revoked
  else
    match policy.expire_after_epoch with
    | Some value when Int64.compare current_epoch value >= 0 ->
      Expired
    | _ ->
      begin
        match policy.activate_after_epoch with
        | Some value when Int64.compare current_epoch value < 0 ->
          Scheduled
        | _ ->
          Live
      end

let live = function
  | Live -> true
  | Scheduled
  | Expired
  | Revoked
  | Erased -> false

let terminal_for_delivery = function
  | Expired
  | Revoked
  | Erased -> true
  | Scheduled
  | Live -> false

let activation_epoch current_epoch (policy : Circle_key_policy.t) =
  match policy.activate_after_epoch with
  | Some value -> value
  | None -> current_epoch

let deliverable_by_deadline ~current_epoch ~deadline_epoch (policy : Circle_key_policy.t) =
  match classify policy current_epoch with
  | Expired
  | Revoked
  | Erased -> false
  | Scheduled
  | Live ->
    let activation_epoch = activation_epoch current_epoch policy in
    let activates_before_deadline =
      Int64.compare activation_epoch deadline_epoch < 0 in
    let survives_activation =
      match policy.expire_after_epoch with
      | Some value ->
        Int64.compare value current_epoch > 0
        && Int64.compare value activation_epoch > 0
      | None ->
        true in
    activates_before_deadline && survives_activation