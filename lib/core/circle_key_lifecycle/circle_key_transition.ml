(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let invalid code message =
  Error (code, message)

let validate_window ~current_epoch ~activate_after_epoch ~expire_after_epoch =
  match activate_after_epoch, expire_after_epoch with
  | Some activate_after_epoch, Some expire_after_epoch
    when Int64.compare expire_after_epoch activate_after_epoch <= 0 ->
    invalid "invalid_circle_key_window" "expire_after_epoch must be greater than activate_after_epoch"
  | None, Some expire_after_epoch
    when Int64.compare expire_after_epoch current_epoch <= 0 ->
    invalid "invalid_circle_key_window" "expire_after_epoch must be in the future"
  | Some activate_after_epoch, Some expire_after_epoch
    when Int64.compare expire_after_epoch current_epoch <= 0
         && Int64.compare activate_after_epoch current_epoch <= 0 ->
    invalid "invalid_circle_key_window" "expire_after_epoch must be in the future"
  | _ ->
    Ok ()

let grant ~current_epoch (policy : Circle_key_policy.t) (payload : Circles.key_grant_payload) =
  if policy.erased then
    invalid "circle_key_erased" "erased keys cannot be granted again"
  else
    match validate_window
      ~current_epoch
      ~activate_after_epoch:payload.activate_after_epoch
      ~expire_after_epoch:payload.expire_after_epoch with
    | Error _ as e ->
      e
    | Ok () ->
      Ok {
        Circle_key_policy.activate_after_epoch = payload.activate_after_epoch;
        expire_after_epoch = payload.expire_after_epoch;
        revoked = false;
        erased = false;
      }

let extend ~current_epoch (policy : Circle_key_policy.t) (payload : Circles.key_extend_payload) =
  if policy.erased then
    invalid "circle_key_erased" "erased keys cannot be extended"
  else if policy.revoked then
    invalid "circle_key_revoked" "revoked keys must be granted again before extension"
  else
    let activate_anchor =
      match policy.activate_after_epoch with
      | Some value -> value
      | None -> current_epoch in
    if Int64.compare payload.expire_after_epoch activate_anchor <= 0 then
      invalid "invalid_circle_key_window" "expire_after_epoch must be greater than activation epoch"
    else if Int64.compare payload.expire_after_epoch current_epoch <= 0 then
      invalid "invalid_circle_key_window" "expire_after_epoch must be in the future"
    else
      Ok {
        policy with
        Circle_key_policy.expire_after_epoch = Some payload.expire_after_epoch;
      }

let revoke (policy : Circle_key_policy.t) =
  if policy.erased then
    invalid "circle_key_erased" "erased keys cannot be revoked again"
  else
    Ok {
      policy with
      Circle_key_policy.revoked = true;
    }

let erase (policy : Circle_key_policy.t) =
  Ok {
    policy with
    Circle_key_policy.revoked = true;
    erased = true;
  }