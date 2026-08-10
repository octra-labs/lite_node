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
  | Exhausted
  | Send of t

type response_route =
  | Complete_response
  | Finality_response
  | Ignore_legacy_response
  | Reject_response

val max_attempts : int
val retry_ns : int64
val idle : t
val response_route :
  finality_request:bool ->
  complete_valid:bool ->
  legacy_valid:bool ->
  response_route
val plan : now:int64 -> epoch:int64 -> t -> plan
