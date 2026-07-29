(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let outbox_reason_of_state = function
  | Circle_key_state.Expired -> Some Circles.Delivery_key_expired
  | Revoked -> Some Delivery_key_revoked
  | Erased -> Some Delivery_key_erased
  | Scheduled
  | Live -> None

let matches_state state reason =
  match outbox_reason_of_state state with
  | Some expected -> expected = reason || reason = Circles.Delivery_key_inactive
  | None -> false