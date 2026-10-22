(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type state = {next : int; active : int list}
type message = Start | Finish of int
type effect = Enter of int | Busy | Leave

let capacity = 2
let initial = {next = 0; active = []}

let step state = function
  | Start when List.length state.active >= capacity || state.next = max_int -> state, Busy
  | Start ->
    {next = state.next + 1; active = state.next :: state.active}, Enter state.next
  | Finish id -> {state with active = List.filter ((<>) id) state.active}, Leave

let owner = ref initial

let send message =
  let state, effect = step !owner message in
  owner := state;
  effect

let run ~busy action =
  match send Start with
  | Busy -> busy ()
  | Enter id ->
    Lwt.finalize action (fun () -> ignore (send (Finish id)); Lwt.return_unit)
  | Leave -> assert false