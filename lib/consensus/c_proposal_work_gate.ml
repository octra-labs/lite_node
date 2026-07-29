(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = Lwt_mutex.t

let create () =
  Lwt_mutex.create ()

let run gate ~relevant work =
  Lwt_mutex.with_lock gate (fun () ->
    if relevant () then work ()
    else Lwt.return_none)