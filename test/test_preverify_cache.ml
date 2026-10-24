(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_node_runtime.Preverify_cache
module Preverify_submit = Octra_node_runtime.Preverify_submit
module Tx_view = Octra_node_runtime.Tx_view

let fail msg =
  failwith ("test_preverify_cache: " ^ msg)

let result ?(keyed = false) ?(math = false) strict sender_enc_snapshot =
  C.{
    delta_ok = true;
    balance_ok = false;
    strict;
    math;
    keyed;
    sender_enc_snapshot;
  }

let expect_state ?(keyed = false) ?(math = false) expected hash =
  if C.state ~registered:false ~keyed ~math hash <> expected then fail "unexpected cache state"

let test_ready_result () =
  let hash = "test-preverify-cache-ready" in
  C.remove hash;
  C.insert_with_cap ~registered:false ~keyed:false hash (Lwt.return (C.Checked (result false "cipher-a")));
  expect_state C.Ready hash;
  begin
    match C.ready_result ~registered:false ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher-a" with
    | Some row when row.C.delta_ok && not row.C.balance_ok -> ()
    | _ -> fail "missing ready result"
  end;
  begin
    match C.ready_result ~registered:false ~keyed:false hash ~strict:true ~sender_enc_snapshot:"cipher-a" with
    | None -> ()
    | Some _ -> fail "accepted wrong proof mode"
  end;
  begin
    match C.ready_result ~registered:false ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher-b" with
    | None -> ()
    | Some _ -> fail "accepted wrong snapshot"
  end;
  C.remove hash;
  expect_state C.Missing hash

let test_math_cache () =
  let hash = "math-cache" in
  C.remove hash;
  let prior, wake = Lwt.wait () in
  if C.start_task ~registered:false ~keyed:false ~math:false hash (fun () -> prior) <> Preverify_submit.Started then
    fail "prior task did not start";
  if C.start_task ~registered:false ~keyed:false ~math:true hash
      (fun () -> Lwt.return (C.Checked (result ~math:true false "cipher")))
     <> Preverify_submit.Started then fail "math task reused prior task";
  Lwt.wakeup wake (C.Checked (result false "cipher"));
  ignore (Lwt_main.run (Lwt.pause ()));
  if Option.is_some (C.ready_result ~registered:false ~keyed:false ~math:false hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior reused math result";
  if Option.is_none (C.ready_result ~registered:false ~keyed:false ~math:true hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior completion replaced math result";
  expect_state C.Missing hash;
  expect_state ~math:true C.Ready hash;
  C.remove hash

let test_key_cache () =
  let hash = "proof-key-cache" in
  C.remove hash;
  let prior, wake = Lwt.wait () in
  if C.start_task ~registered:false ~keyed:false hash (fun () -> prior) <> Preverify_submit.Started then
    fail "prior key task did not start";
  if C.start_task ~registered:false ~keyed:true hash
      (fun () -> Lwt.return (C.Checked (result ~keyed:true false "cipher")))
     <> Preverify_submit.Started then fail "key task reused prior task";
  Lwt.wakeup wake (C.Checked (result false "cipher"));
  ignore (Lwt_main.run (Lwt.pause ()));
  if Option.is_some (C.ready_result ~registered:false ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior reused keyed result";
  if Option.is_none (C.ready_result ~registered:false ~keyed:true hash ~strict:false ~sender_enc_snapshot:"cipher") then
    fail "prior completion replaced keyed result";
  if C.gate ~state:(C.state ~registered:false ~keyed:false ~math:false hash) ~defer_count:0 ~max_defer:2
      <> C.Defer_gate {next_count = 1; status = "missing"} then
    fail "wrong key mode passed preverify gate";
  expect_state ~keyed:true C.Ready hash;
  C.remove hash

let test_registered_cache () =
  let hash = "registered-cache" in
  C.remove hash;
  let prior, wake = Lwt.wait () in
  ignore (C.start_task ~registered:false ~keyed:false hash (fun () -> prior));
  let started = C.start_task ~registered:true ~keyed:false hash
    (fun () -> Lwt.return (C.Checked (result false "cipher"))) in
  if started <> Preverify_submit.Started then fail "registered task reused prior";
  Lwt.wakeup wake (C.Checked (result false "cipher"));
  ignore (Lwt_main.run (Lwt.pause ()));
  if C.state ~registered:false ~keyed:false ~math:false hash <> C.Missing then
    fail "registered state reused prior";
  List.iter (fun registered ->
    let found = C.ready_result ~registered ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher" in
    if Option.is_some found <> registered then fail "registered result identity lost") [false; true];
  C.remove hash;
  ignore (C.start_task ~registered:true ~keyed:false hash
    (fun () -> Lwt.fail (Octra_core.Exec_resource.Unavailable Host)));
  List.iter (fun defer_count ->
    let gate = C.gate ~state:(C.state ~registered:true ~keyed:false ~math:false hash)
      ~defer_count ~max_defer:2 in
    if gate <> C.Unavailable_gate {next_count = min defer_count 2;
        reason = Tx_view.Local_resource Host} then
      fail "host fault did not defer") [0; 1; 2; 3; 100];
  C.remove hash

let test_flag_reuse () =
  let bits = [false; true] in
  let flags = List.concat_map (fun registered ->
    List.concat_map (fun keyed -> List.map (fun math -> registered, keyed, math) bits) bits) bits in
  let hash = "flag-reuse" in
  List.iter (fun (registered, keyed, math) ->
    List.iter (fun strict ->
      C.insert_with_cap ~registered ~keyed ~math hash
        (Lwt.return (C.Checked (result ~keyed ~math strict "cipher")));
      List.iter (fun (ask_registered, ask_keyed, ask_math) ->
        let same = (registered, keyed, math) = (ask_registered, ask_keyed, ask_math) in
        if C.state ~registered:ask_registered ~keyed:ask_keyed ~math:ask_math hash
            <> (if same then C.Ready else C.Missing) then fail "cache flag state differs";
        List.iter (fun ask_strict ->
          List.iter (fun cipher ->
            let found = C.ready_result ~registered:ask_registered ~keyed:ask_keyed
              ~math:ask_math hash ~strict:ask_strict ~sender_enc_snapshot:cipher in
            if Option.is_some found <> (same && strict = ask_strict && cipher = "cipher")
            then fail "cache flag result differs") ["cipher"; "other"]) bits) flags;
      C.remove hash) bits;
    List.iter (fun (ask_registered, ask_keyed, ask_math) ->
      List.iter (fun waiting ->
        let task, wake = Lwt.wait () in
        C.insert_with_cap ~registered ~keyed ~math hash
          (if waiting then task else Lwt.return (C.Checked (result ~keyed ~math false "cipher")));
        let called = ref false in
        let started = C.start_task ~registered:ask_registered ~keyed:ask_keyed ~math:ask_math hash
          (fun () -> called := true;
            Lwt.return (C.Checked (result ~keyed:ask_keyed ~math:ask_math false "cipher"))) in
        let same = (registered, keyed, math) = (ask_registered, ask_keyed, ask_math) in
        if started <> (if same then Preverify_submit.Existing else Preverify_submit.Started)
            || !called = same then fail "cache flag task reuse differs";
        Lwt.wakeup wake (C.Checked (result ~keyed ~math false "cipher"));
        if C.state ~registered:ask_registered ~keyed:ask_keyed ~math:ask_math hash <> C.Ready
        then fail "old completion replaced flag result";
        C.remove hash) bits) flags) flags

let test_pending_remove () =
  let hash = "test-preverify-cache-pending" in
  let promise, _wake = Lwt.wait () in
  C.remove hash;
  C.insert_with_cap ~registered:false ~keyed:false hash promise;
  expect_state C.Pending hash;
  C.remove hash;
  expect_state C.Missing hash

let test_unavailable_state () =
  let hash = "test-preverify-cache-unavailable" in
  let reason = Tx_view.Proof_worker_unavailable "worker_missing" in
  C.remove hash;
  C.insert_with_cap ~registered:false ~keyed:false hash (Lwt.return (C.Unavailable reason));
  expect_state (C.Unavailable_state reason) hash;
  begin
    match C.ready_result ~registered:false ~keyed:false hash ~strict:false ~sender_enc_snapshot:"cipher" with
    | None -> ()
    | Some _ -> fail "unavailable task exposed a ready result"
  end;
  C.remove hash

let test_retain () =
  let keep_hash = "test-preverify-cache-keep" in
  let drop_hash = "test-preverify-cache-drop" in
  C.remove keep_hash;
  C.remove drop_hash;
  C.insert_with_cap ~registered:false ~keyed:false keep_hash (Lwt.return (C.Checked (result false "keep")));
  C.insert_with_cap ~registered:false ~keyed:false drop_hash (Lwt.return (C.Checked (result false "drop")));
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
    match C.start_task ~registered:false ~keyed:false hash (fun () -> promise) with
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
        match C.start_task ~registered:false ~keyed:false hash (fun () -> promise) with
        | Preverify_submit.Started -> ()
        | _ -> fail "capacity task did not start"
      end;
      hash, wake)
  in
  let overflow_started = ref false in
  begin
    match
      C.start_task ~registered:false ~keyed:false
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
       next_count = 0;
       reason = Tx_view.Proof_worker_unavailable "worker_missing";
     })
    (C.gate
       ~state:(C.Unavailable_state
                 (Tx_view.Proof_worker_unavailable "worker_missing"))
       ~defer_count:0
       ~max_defer:2);
  expect_gate
    (C.Defer_gate {next_count = 2; status = "missing"})
    (C.gate ~state:C.Missing ~defer_count:2 ~max_defer:2);
  expect_gate
    (C.Defer_gate {next_count = 2; status = "pending"})
    (C.gate ~state:C.Pending ~defer_count:2 ~max_defer:2);
  expect_gate
    (C.Unavailable_gate {next_count = 2; reason = Tx_view.Proof_worker_unavailable "worker_missing"})
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

let test_local_retry () =
  let hash = "local-retry" in
  List.iter (fun outcome ->
    C.remove hash;
    ignore (C.start_task ~registered:true ~keyed:false hash (fun () -> outcome));
    List.iter (fun count ->
      match C.gate ~state:(C.state ~registered:true ~keyed:false ~math:false hash)
          ~defer_count:count ~max_defer:2 with
      | C.Unavailable_gate {next_count; _} when next_count = min count 2 -> ()
      | _ -> fail "local failure did not defer") [0; 2; 3; 100];
    let retry, wake = Lwt.wait () in
    if C.start_task ~registered:true ~keyed:false hash (fun () -> retry) <> Preverify_submit.Started then
      fail "local failure prevented pending retry";
    begin match C.gate ~state:(C.state ~registered:true ~keyed:false ~math:false hash)
        ~defer_count:100 ~max_defer:2 with
    | C.Defer_gate _ -> () | _ -> fail "pending retry became transaction decision"
    end;
    Lwt.wakeup wake (C.Unavailable (Tx_view.Proof_worker_unavailable "retry"));
    ignore (Lwt_main.run (Lwt.pause ()));
    C.prune ~ttl:0. ~max_entries:0 ();
    List.iter (fun state -> match C.gate ~state ~defer_count:100 ~max_defer:2 with
      | C.Defer_gate _ -> () | _ -> fail "evicted or pending local work became rejection")
      [C.state ~registered:true ~keyed:false ~math:false hash; C.Pending];
    let start = C.start_task ~registered:true ~keyed:false hash
      (fun () -> Lwt.return (C.Checked (result false "cipher"))) in
    if start <> Preverify_submit.Started then fail "local failure blocked retry";
    if C.state ~registered:true ~keyed:false ~math:false hash <> C.Ready then fail "retry not ready";
    C.remove hash)
    [Lwt.fail Lwt.Canceled; Lwt.fail (Unix.Unix_error (Unix.EIO, "read", ""));
     Lwt.return (C.Unavailable (Tx_view.Proof_worker_unavailable "missing"))]

let test_task_failures () =
  let module R = Octra_core.Exec_resource in
  List.iter (fun (error, resource) ->
    List.iter (fun delayed ->
      let hash = "task-failure" in
      C.remove hash;
      let before = C.pending_count () in
      let task, wake = Lwt.task () in
      ignore (C.start_task ~registered:false ~keyed:false hash (fun () ->
        if delayed then task else raise error));
      if delayed then begin
        expect_state C.Pending hash;
        Lwt.wakeup_exn wake error
      end;
      expect_state (C.Unavailable_state (Tx_view.Local_resource resource)) hash;
      if C.pending_count () <> before then fail "failed task leaked pending count";
      if C.ready_result ~registered:false ~keyed:false hash ~strict:false
          ~sender_enc_snapshot:"cipher" <> None then fail "failed task exposed result";
      if C.start_task ~registered:false ~keyed:false hash
          (fun () -> Lwt.return (C.Checked (result false "cipher"))) <> Preverify_submit.Started
      then fail "failed task prevented retry";
      expect_state C.Ready hash;
      C.remove hash) [false; true])
    [Failure "read failed", R.Host; Unix.Unix_error (Unix.EIO, "read", ""), R.Host;
     Octra_core.Private_ledger.Worker_retry "worker busy", R.Host;
     R.Unavailable R.Host, R.Host; R.Unavailable R.Memory, R.Memory;
     R.Unavailable R.Stack, R.Stack; Out_of_memory, R.Memory;
     Stack_overflow, R.Stack; Lwt.Canceled, R.Host]

let () =
  test_ready_result ();
  test_math_cache ();
  test_key_cache ();
  test_registered_cache ();
  test_flag_reuse ();
  test_pending_remove ();
  test_unavailable_state ();
  test_retain ();
  test_start_task_counter ();
  test_start_task_capacity ();
  test_gate ();
  test_local_retry ();
  test_task_failures ();
  test_config_limits ();
  print_endline "status = pass test = preverify_cache"