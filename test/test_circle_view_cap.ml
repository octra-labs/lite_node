(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Capacity = Octra_node_runtime.Circle_view_capacity

let fail message =
  failwith ("test_node_runtime_circle_view_capacity: " ^ message)

let test_busy_and_release () =
  let capacity = Capacity.create ~limit:1 in
  let release, wake = Lwt.wait () in
  let first =
    Capacity.with_slot
      capacity
      ~busy:(fun () -> Lwt.return "unexpected_busy")
      (fun () ->
         let open Lwt.Syntax in
         let* () = release in
         Lwt.return "first") in
  if Capacity.active capacity <> 1 then fail "slot was not acquired";
  let second =
    Lwt_main.run
      (Capacity.with_slot
         capacity
         ~busy:(fun () -> Lwt.return "busy")
         (fun () -> Lwt.return "unexpected_run")) in
  if second <> "busy" then fail "full capacity admitted work";
  Lwt.wakeup_later wake ();
  if Lwt_main.run first <> "first" then fail "first work result differs";
  if Capacity.active capacity <> 0 then fail "slot was not released"

let test_timeout_and_cancel () =
  List.iter
    (fun cancel ->
      let capacity = Capacity.create ~limit:1 in
      let release, wake = Lwt.wait () in
      let stopped = ref false in
      let response =
        Capacity.with_slot
          ~timeout:(0.01, fun () -> Lwt.return "expired")
          ~stop:(fun () -> stopped := true)
          capacity
          ~busy:(fun () -> Lwt.return "busy")
          (fun () -> release)
      in
      if cancel then Lwt.cancel response
      else if Lwt_main.run response <> "expired" then fail "timeout response";
      if not !stopped then fail "stop not requested";
      if Capacity.active capacity <> 1 then fail "worker released before completion";
      let next =
        Capacity.with_slot capacity
          ~busy:(fun () -> Lwt.return "busy")
          (fun () -> Lwt.return "unexpected")
      in
      if Lwt_main.run next <> "busy" then fail "active worker not counted";
      Lwt.wakeup wake "finished";
      if Capacity.active capacity <> 0 then fail "completed worker not released")
    [false; true]

let () =
  test_busy_and_release ();
  test_timeout_and_cancel ();
  print_endline "test_node_runtime_circle_view_capacity: ok"