(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type sent = {
  epoch : int64;
  attempts : int;
  sent_at : int64;
}

type t =
  | Idle
  | Sent of sent

type plan =
  | Wait
  | Rest
  | Send of t

type response_route =
  | Complete_response
  | Finality_response
  | Ignore_legacy_response
  | Reject_response

let max_attempts = 3
let retry_ns = 5_000_000_000L
let rest_ns = 60_000_000_000L
let idle = Idle

let response_route ~finality_request ~complete_valid ~legacy_valid =
  if complete_valid then Complete_response
  else if legacy_valid && finality_request then Finality_response
  else if legacy_valid then Ignore_legacy_response
  else Reject_response

let plan ~now ~epoch = function
  | Idle ->
    Send (Sent { epoch; attempts = 1; sent_at = now })
  | Sent prior when prior.epoch <> epoch ->
    Send (Sent { epoch; attempts = 1; sent_at = now })
  | Sent prior when prior.attempts >= max_attempts
                    && Int64.sub now prior.sent_at < rest_ns ->
    Rest
  | Sent prior when prior.attempts >= max_attempts ->
    Send (Sent { epoch; attempts = 1; sent_at = now })
  | Sent prior when Int64.sub now prior.sent_at < retry_ns ->
    Wait
  | Sent prior ->
    Send (Sent {
      epoch;
      attempts = prior.attempts + 1;
      sent_at = now;
    })