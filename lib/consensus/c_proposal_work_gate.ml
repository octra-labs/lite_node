(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {mutex : Lwt_mutex.t; mutable window : (float * float) option}

let current = Lwt.new_key ()

let window () = Lwt.get current

let start gate ~delay ~duration =
  let now = Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9 in
  gate.window <- Some (now +. delay, duration)

let create () =
  {mutex = Lwt_mutex.create (); window = None}

let run gate ~relevant work =
  let window = gate.window in
  Lwt_mutex.with_lock gate.mutex (fun () ->
    if relevant () then Lwt.with_value current window work
    else Lwt.return_none)