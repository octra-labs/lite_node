(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type resource = Memory | Stack | Host

exception Unavailable of resource
exception Exhausted of string * resource

let protect action input =
  try action input with
  | Out_of_memory -> raise (Unavailable Memory)
  | Stack_overflow -> raise (Unavailable Stack)

let detach action input = Lwt_preemptive.detach (protect action) input

let run ~hash action =
  Lwt.catch (fun () -> protect action ()) (function
    | Unavailable resource -> Lwt.fail (Exhausted (hash, resource))
    | Out_of_memory -> Lwt.fail (Exhausted (hash, Memory))
    | Stack_overflow -> Lwt.fail (Exhausted (hash, Stack))
    | error -> Lwt.fail error)

let name = function Memory -> "memory" | Stack -> "stack" | Host -> "host"