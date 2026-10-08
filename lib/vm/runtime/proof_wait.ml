(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ticket = {
  request : string;
  generation : string;
}

type state = Waiting of ticket * int64 | Closed

type reason = Cancelled | Deadline

type 'a message = Reply of ticket * int64 * 'a | Cancel | Expire

type 'a effect = Deliver of 'a | Stop of reason

let delta state message =
  match state, message with
  | Waiting (expected, deadline), Reply (ticket, now, value) when expected = ticket ->
    if now < deadline then Closed, [Deliver value]
    else Closed, [Stop Deadline]
  | Waiting _, Reply _ -> state, []
  | Waiting _, Cancel -> Closed, [Stop Cancelled]
  | Waiting _, Expire -> Closed, [Stop Deadline]
  | Closed, _ -> Closed, []