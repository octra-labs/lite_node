(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module V = Octra_node_runtime.Epoch_visibility

let expect name value = if not value then failwith name

let test_busy () =
  let state = V.create () in
  expect "begin failed" (V.begin_apply state = Ok ());
  let calls = ref 0 in
  let result = V.read state (fun () -> incr calls; Lwt.return 7) in
  expect "read waited for writer" (Lwt.state result = Lwt.Return None);
  expect "read reached applying state" (!calls = 0);
  expect "read released writer" (V.is_applying state);
  expect "finish failed" (V.finish_apply state = Ok ());
  expect "read did not reopen" (Lwt_main.run (V.read state (fun () -> Lwt.return 9)) = Some 9)

let test_overlap () =
  let state = V.create () in
  let pending, finish = Lwt.wait () in
  let calls = ref 0 in
  let result = V.read state (fun () -> incr calls; pending) in
  expect "begin failed" (V.begin_apply state = Ok ());
  Lwt.wakeup finish 7;
  expect "overlap waited for writer" (Lwt.state result = Lwt.Return None);
  expect "overlap reread applying state" (!calls = 1);
  expect "finish failed" (V.finish_apply state = Ok ())

let test_generation () =
  let state = V.create () in
  let calls = ref 0 in
  let read () =
    incr calls;
    expect "begin failed" (V.begin_apply state = Ok ());
    expect "finish failed" (V.finish_apply state = Ok ());
    Lwt.return !calls in
  expect "changed generation served" (Lwt_main.run (V.read state read) = None);
  expect "retry count changed" (!calls = 3)

let test_cancel () =
  let state = V.create () in
  let pending, _ = Lwt.task () in
  let result = V.read state (fun () -> pending) in
  Lwt.cancel result;
  expect "read cancellation lost" (Lwt.state pending = Lwt.Fail Lwt.Canceled);
  expect "read cancellation changed writer" (not (V.is_applying state))

let () =
  test_busy ();
  test_overlap ();
  test_generation ();
  test_cancel ();
  print_endline "event = epoch_reads status = passed"