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

val max_attempts : int
val retry_ns : int64
val rest_ns : int64
val idle : t
val request_scope :
  complete_request:bool ->
  finality_request:bool ->
  request_scope
val response_proof :
  complete_valid:bool ->
  legacy_valid:bool ->
  legacy_complete:bool ->
  response_proof
val response_route : request_scope -> response_proof -> response_route
val plan : now:int64 -> epoch:int64 -> t -> plan