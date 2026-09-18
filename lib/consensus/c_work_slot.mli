(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ('job, 'reply) t

type ('job, 'reply) message =
  | Submit of 'job
  | Finish of int * 'reply
  | Take
  | Close
  | Open

type ('job, 'reply) effect =
  | Run of int * 'job
  | Complete of 'job
  | Deliver of 'job * 'reply

val empty : ('job, 'reply) t
val idle : ('job, 'reply) t -> bool
val ready : ('job, 'reply) t -> bool
val step :
  ('job, 'reply) t ->
  ('job, 'reply) message ->
  ('job, 'reply) t * ('job, 'reply) effect list