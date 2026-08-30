(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'a step =
  | Next of 'a
  | Stop

let rec run step state =
  let open Lwt.Syntax in
  let* next = step state in
  match next with
  | Stop -> Lwt.return_unit
  | Next state ->
    let* () = Lwt.pause () in
    run step state