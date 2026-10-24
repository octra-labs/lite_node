(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type resource = Memory | Stack | Host

exception Unavailable of resource
exception Exhausted of string * resource

let protect action input =
  try action input with
  | Out_of_memory -> raise (Unavailable Memory)
  | Stack_overflow -> raise (Unavailable Stack)

let bind work next = Lwt.bind work (protect next)

let catch action handler =
  Lwt.catch (fun () -> protect action ()) (protect handler)

let detach ?(launch = Lwt_preemptive.detach) action input =
  try launch (protect action) input with
  | Out_of_memory -> Lwt.fail (Unavailable Memory)
  | Stack_overflow -> Lwt.fail (Unavailable Stack)
  | _ -> Lwt.fail (Unavailable Host)

let joined ?launch action input =
  let work = detach ?launch action input in
  Lwt.finalize
    (fun () -> Lwt.protected work)
    (fun () -> Lwt.catch
      (fun () -> Lwt.map (fun _ -> ()) (Lwt.protected work))
      (fun _ -> Lwt.return_unit))

let run ~hash action =
  Lwt.catch (fun () -> protect action ()) (function
    | Unavailable resource -> Lwt.fail (Exhausted (hash, resource))
    | Out_of_memory -> Lwt.fail (Exhausted (hash, Memory))
    | Stack_overflow -> Lwt.fail (Exhausted (hash, Stack))
    | error -> Lwt.fail error)

let name = function Memory -> "memory" | Stack -> "stack" | Host -> "host"

let await ?(steps = 10_000_000) work =
  let rec pump n = match Lwt.state work with
    | Lwt.Return value -> value
    | Lwt.Fail error -> raise error
    | Lwt.Sleep ->
      if n > steps then raise (Unavailable Host);
      Lwt_engine.iter false;
      pump (n + 1) in
  pump 0