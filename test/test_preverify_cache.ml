(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_node_runtime.Preverify_cache
module Preverify_submit = Octra_node_runtime.Preverify_submit
module Tx_view = Octra_node_runtime.Tx_view

let fail msg =
  failwith ("test_preverify_cache: " ^ msg)

let result ?(keyed = false) ?(math=false) strict sender_enc_snapshot =
  C.{
    delta_ok = true;
    balance_ok = false;
    strict;
    math;
    keyed;
    sender_enc_snapshot;
  }

let expect_state ?(keyed = false) ?(math = false) expected hash =
  if C.state ~keyed ~math hash <> expected then fail "unexpected cache state"

let test_ready_result () =
  let hash = "test-preverify-cache-ready" in
  C.remove hash;
  C.insert_with_cap ~keyed:false hash (Lwt.return (C.Checked (result false "cipher-a")));
  expect_state C.Ready hash;
  begin
    match C.ready_result ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher-a" with
    | Some row when row.C.delta_ok && not row.C.balance_ok -> ()
    | _ -> fail "missing ready result"
  end;
  begin
    match C.ready_result ~keyed:false hash ~strict:true ~sender_enc_snapshot:"cipher-a" with
    | None -> ()
    | Some _ -> fail "accepted wrong proof mode"
  end;
  begin
    match C.ready_result ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher-b" with
    | None -> ()
    | Some _ -> fail "accepted wrong snapshot"
  end;
  C.remove hash;
  expect_state C.Missing hash

let test_math_cache () =
  let hash = "math-cache" in
  C.remove hash;
  let prior, wake = Lwt.wait () in
  if C.start_task ~keyed:false ~math:false hash (fun () -> prior) <> Preverify_submit.Started then
    fail "prior task did not start";
  if C.start_task ~keyed:false ~math:true hash
      (fun () -> Lwt.return (C.Checked (result ~math:true false "cipher")))
     <> Preverify_submit.Started then fail "math task reused prior task";
  Lwt.wakeup wake (C.Checked (result false "cipher"));
  ignore (Lwt_main.run (Lwt.pause ()));
  if Option.is_some (C.ready_result ~keyed:false ~math:false hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior reused math result";
  if Option.is_none (C.ready_result ~keyed:false ~math:true hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior completion replaced math result";
  expect_state C.Missing hash;
  expect_state ~math:true C.Ready hash;
  C.remove hash

let test_key_cache () =
  let hash = "proof-key-cache" in
  C.remove hash;
  let prior, wake = Lwt.wait () in
  if C.start_task ~keyed:false hash (fun () -> prior) <> Preverify_submit.Started then
    fail "prior key task did not start";
  if C.start_task ~keyed:true hash
      (fun () -> Lwt.return (C.Checked (result ~keyed:true false "cipher")))
     <> Preverify_submit.Started then fail "key task reused prior task";
  Lwt.wakeup wake (C.Checked (result false "cipher"));
  ignore (Lwt_main.run (Lwt.pause ()));
  if Option.is_some (C.ready_result ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior reused keyed result";
  if Option.is_none (C.ready_result ~keyed:true hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior completion replaced keyed result";
  if C.gate ~state:(C.state ~keyed:false ~math:false hash) ~defer_count:0 ~max_defer:2
      <> C.Defer_gate {next_count = 1; status = "missing"} then
    fail "wrong key mode passed preverify gate";
  expect_state ~keyed:true C.Ready hash;
  C.remove hash

let test_pending_remove () =
  let hash = "test-preverify-cache-pending" in
  let promise, _wake = Lwt.wait () in
  C.remove hash;
  C.insert_with_cap ~keyed:false hash promise;
  expect_state C.Pending hash;
  C.remove hash;
  expect_state C.Missing hash

let test_unavailable_state () =
  let hash = "test-preverify-cache-unavailable" in
  let reason = Tx_view.Proof_worker_unavailable "worker_missing" in
  C.remove hash;
  C.insert_with_cap ~keyed:false hash (Lwt.return (C.Unavailable reason));
  expect_state (C.Unavailable_state reason) hash;
  begin
    match C.ready_result ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher" with
    | None -> ()
    | Some _ -> fail "unavailable task exposed a ready result"
  end;
  C.remove hash

let test_retain () =
  let keep_hash = "test-preverify-cache-keep" in
  let drop_hash = "test-preverify-cache-drop" in
  C.remove keep_hash;
  C.remove drop_hash;
  C.insert_with_cap ~keyed:false keep_hash (Lwt.return (C.Checked (result false "keep")));
  C.insert_with_cap ~keyed:false drop_hash (Lwt.return (C.Checked (result false "drop")));
  C.retain (fun hash -> String.equal hash keep_hash);
  expect_state C.Ready keep_hash;
  expect_state C.Missing drop_hash;
  C.remove keep_hash

let test_start_task_counter () =
  let hash = "test-preverify-cache-counter" in
  C.remove hash;
  let before = C.pending_count () in
  let promise, wake = Lwt.wait () in
  begin
    match C.start_task ~keyed:false hash (fun () -> promise) with
    | Preverify_submit.Started -> ()
    | _ -> fail "pending task did not start"
  end;
  let after_start = C.pending_count () in
  if after_start <> before + 1 then
    fail
      (Printf.sprintf
         "pending counter did not increment before = %d after = %d"
         before
         after_start);
  Lwt.wakeup wake (C.Checked (result false "task"));
  ignore (Lwt_main.run (Lwt.pause ()));
  if C.pending_count () <> before then fail "pending counter did not decrement";
  C.remove hash

let test_start_task_capacity () =
  let tasks =
    List.init (C.pending_max ()) (fun index ->
      let hash = Printf.sprintf "test-preverify-cache-cap-%d" index in
      let promise, wake = Lwt.wait () in
      C.remove hash;
      begin
        match C.start_task ~keyed:false hash (fun () -> promise) with
        | Preverify_submit.Started -> ()
        | _ -> fail "capacity task did not start"
      end;
      hash, wake)
  in
  let overflow_started = ref false in
  begin
    match
      C.start_task ~keyed:false
        "test-preverify-cache-cap-overflow"
        (fun () ->
          overflow_started := true;
          Lwt.return (C.Checked (result false "overflow")))
    with
    | Preverify_submit.Busy -> ()
    | _ -> fail "capacity overflow was accepted"
  end;
  if !overflow_started then fail "capacity overflow task executed";
  List.iter
    (fun (_, wake) -> Lwt.wakeup wake (C.Checked (result false "done")))
    tasks;
  ignore (Lwt_main.run (Lwt.pause ()));
  List.iter (fun (hash, _) -> C.remove hash) tasks;
  if C.pending_count () <> 0 then fail "capacity tasks did not release"

let expect_gate expected actual =
  if expected <> actual then fail "unexpected preverify cache gate"

let test_gate () =
  expect_gate C.Ready_gate (C.gate ~state:C.Ready ~defer_count:0 ~max_defer:2);
  expect_gate
    (C.Failed_gate "boom")
    (C.gate ~state:(C.Failed "boom") ~defer_count:0 ~max_defer:2);
  expect_gate
    (C.Defer_gate { next_count = 1; status = "missing" })
    (C.gate ~state:C.Missing ~defer_count:0 ~max_defer:2);
  expect_gate
    (C.Defer_gate { next_count = 2; status = "pending" })
    (C.gate ~state:C.Pending ~defer_count:1 ~max_defer:2);
  expect_gate
    (C.Unavailable_gate {
       next_count = 1;
       reason = Tx_view.Proof_worker_unavailable "worker_missing";
     })
    (C.gate
       ~state:(C.Unavailable_state
                 (Tx_view.Proof_worker_unavailable "worker_missing"))
       ~defer_count:0
       ~max_defer:2);
  expect_gate
    (C.Timeout_gate "task not in cache")
    (C.gate ~state:C.Missing ~defer_count:2 ~max_defer:2);
  expect_gate
    (C.Timeout_gate "still running")
    (C.gate ~state:C.Pending ~defer_count:2 ~max_defer:2);
  expect_gate
    (C.Timeout_gate "proof worker unavailable: worker_missing")
    (C.gate
       ~state:(C.Unavailable_state
                 (Tx_view.Proof_worker_unavailable "worker_missing"))
       ~defer_count:2
       ~max_defer:2)

let test_config_limits () =
  Unix.putenv "OCTRA_PREVERIFY_CACHE_TTL" "nan";
  Unix.putenv "OCTRA_PREVERIFY_CACHE_MAX" "0";
  if C.cache_ttl () <> 45. then fail "cache ttl default";
  if C.configured_max_entries () <> 64 then fail "cache max default";
  if C.pending_max () <> 6 then fail "pending max default"

let () =
  test_ready_result ();
  test_math_cache ();
  test_key_cache ();
  test_pending_remove ();
  test_unavailable_state ();
  test_retain ();
  test_start_task_counter ();
  test_start_task_capacity ();
  test_gate ();
  test_config_limits ();
  print_endline "status = pass test = preverify_cache"