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

type request_scope =
  | Unmarked_request
  | Range_request
  | Finality_request

type response_proof =
  | Complete_proof
  | Legacy_complete_proof
  | Legacy_partial_proof
  | Invalid_proof

type response_route =
  | Complete_response
  | Legacy_range_response
  | Finality_response
  | Ignore_legacy_response
  | Reject_response

let max_attempts = 3
let retry_ns = 5_000_000_000L
let rest_ns = 60_000_000_000L
let idle = Idle

let request_scope ~complete_request ~finality_request =
  if not complete_request then Unmarked_request
  else if finality_request then Finality_request
  else Range_request

let response_proof ~complete_valid ~legacy_valid ~legacy_complete =
  if complete_valid then Complete_proof
  else if legacy_valid && legacy_complete then Legacy_complete_proof
  else if legacy_valid then Legacy_partial_proof
  else Invalid_proof

let response_route request proof =
  match request, proof with
  | (Range_request | Finality_request), Complete_proof -> Complete_response
  | Range_request, Legacy_complete_proof -> Legacy_range_response
  | Finality_request, Legacy_complete_proof -> Finality_response
  | (Range_request | Finality_request), Legacy_partial_proof ->
    Ignore_legacy_response
  | Unmarked_request, _
  | (Range_request | Finality_request), Invalid_proof -> Reject_response

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