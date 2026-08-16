(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t =
  | Idle
  | Running
  | Requested

type request =
  | Start
  | Join

type completion =
  | Continue
  | Stop

let idle = Idle

let request = function
  | Idle -> Running, Start
  | Running
  | Requested -> Requested, Join

let complete = function
  | Requested -> Running, Continue
  | Running -> Idle, Stop
  | Idle -> Idle, Stop

let fail _ = Idle

let is_idle = function
  | Idle -> true
  | Running
  | Requested -> false

let drain ~step ~fail =
  let open Lwt.Syntax in
  let turn () =
    let* result =
      Lwt.catch
        (fun () ->
          let+ completion = step () in
          Ok completion)
        (fun exn -> Lwt.return (Error exn))
    in
    match result with
    | Error exn ->
      fail exn;
      Lwt.fail exn
    | Ok Stop -> Lwt.return C_loop.Stop
    | Ok Continue -> Lwt.return (C_loop.Next ())
  in
  C_loop.run turn ()