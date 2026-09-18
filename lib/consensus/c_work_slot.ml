(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ('job, 'reply) phase =
  | Idle
  | Running of int * 'job
  | Ready of 'job * 'reply
  | Closed

type ('job, 'reply) t = {
  next : int;
  phase : ('job, 'reply) phase;
}

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

let empty = { next = 0; phase = Idle }

let idle t =
  match t.phase with Idle -> true | Running _ | Ready _ | Closed -> false

let ready t =
  match t.phase with Ready _ -> true | Idle | Running _ | Closed -> false

let step t message =
  match message, t.phase with
  | Close, _ -> { t with phase = Closed }, []
  | Open, Closed -> { t with phase = Idle }, []
  | Submit job, Idle when t.next < max_int ->
    { next = t.next + 1; phase = Running (t.next, job) }, [Run (t.next, job)]
  | Finish (id, reply), Running (current, job) when id = current ->
    { t with phase = Ready (job, reply) }, [Complete job]
  | Take, Ready (job, reply) ->
    { t with phase = Idle }, [Deliver (job, reply)]
  | Submit _, (Idle | Running _ | Ready _ | Closed)
  | Finish _, (Idle | Running _ | Ready _ | Closed)
  | Take, (Idle | Running _ | Closed)
  | Open, (Idle | Running _ | Ready _) -> t, []