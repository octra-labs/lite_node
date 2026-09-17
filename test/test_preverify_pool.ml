(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Availability = Octra_core.Preverify_availability
module Pool = Octra_node_runtime.Consensus_preverify_pool
module Private_pool = Octra_node_runtime.Consensus_private_preverify
module Transaction = Octra_core.Transaction

let fail name =
  failwith ("preverify_pool: " ^ name)

let expect name condition =
  if not condition then fail name

let tx ?(from = "octFrom") nonce =
  Transaction.{
    from;
    to_ = from;
    amount = Z.zero;
    nonce;
    ou = Z.of_int 3_000;
    timestamp = 1.0;
    signature = "sig";
    public_key = Some "pub";
    message = None;
    op_type = KeySwitch;
    encrypted_data = Some "payload";
  }

let with_op op_type item =
  { item with Transaction.op_type = op_type }

let advance () =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* () = Lwt.pause () in
     Lwt.pause ())

let expect_pending name = function
  | Availability.Pending -> ()
  | Availability.Unmanaged -> fail (name ^ " unmanaged")
  | Availability.Ready _ -> fail (name ^ " ready")
  | Availability.Invalid reason -> fail (name ^ " invalid " ^ reason)

let expect_ready name expected = function
  | Availability.Ready actual -> expect name (actual = expected)
  | Availability.Unmanaged -> fail (name ^ " unmanaged")
  | Availability.Pending -> fail (name ^ " pending")
  | Availability.Invalid reason -> fail (name ^ " invalid " ^ reason)

let expect_invalid name expected = function
  | Availability.Invalid actual -> expect name (actual = expected)
  | Availability.Unmanaged -> fail (name ^ " unmanaged")
  | Availability.Pending -> fail (name ^ " pending")
  | Availability.Ready _ -> fail (name ^ " ready")

let expect_unmanaged name = function
  | Availability.Unmanaged -> ()
  | Availability.Pending -> fail (name ^ " pending")
  | Availability.Ready _ -> fail (name ^ " ready")
  | Availability.Invalid reason -> fail (name ^ " invalid " ^ reason)

let test_duplicate_job_and_ready_binding () =
  let item = tx 1 in
  let calls = ref 0 in
  let pending, resolve = Lwt.wait () in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ -> incr calls; pending);
      bind = (fun _ artifact -> Lwt.return (Pool.Bound (artifact ^ ":bound")));
    }
  in
  Pool.admit pool item |> expect_pending "first admission";
  Pool.admit pool item |> expect_pending "duplicate admission";
  expect "admission returns before verification" (!calls = 0);
  advance ();
  expect "duplicate verification deduplicated" (!calls = 1);
  Lwt_main.run (Pool.observe pool item) |> expect_pending "running item";
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "artifact"));
  advance ();
  Lwt_main.run (Pool.observe pool item)
  |> expect_ready "ready binding" "artifact:bound";
  let stats = Pool.stats pool in
  expect "ready stats" (stats.ready = 1 && stats.pending = 0 && stats.invalid = 0)

let test_validator_joins_running_job () =
  let item = tx 5 in
  let pending, resolve = Lwt.wait () in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ -> incr calls; pending);
      bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool item |> expect_pending "validator admission";
  advance ();
  let waiting = Pool.await pool item in
  expect "validator waits for running job" (Lwt.is_sleeping waiting);
  expect "validator reuses one job" (!calls = 1);
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "prepared"));
  Lwt_main.run waiting |> expect_ready "validator receives prepared" "prepared";
  expect "validator did not duplicate work" (!calls = 1)

let test_private_operation_coverage () =
  let item = tx 10 in
  [ Transaction.EncryptOp;
    Transaction.DecryptOp;
    Transaction.StealthOp;
    Transaction.ClaimOp ]
  |> List.iter (fun op_type ->
    expect
      "private operation admitted"
      (Private_pool.eligible (with_op op_type item)));
  expect
    "key switch remains in its dedicated pool"
    (not (Private_pool.eligible item));
  expect
    "standard transfer is unmanaged"
    (not
       (Private_pool.eligible
          (with_op Transaction.Standard item)))

let test_builder_defers_slow_private_job () =
  let item = with_op Transaction.EncryptOp (tx 11) in
  let job, resolve = Lwt.wait () in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = Private_pool.eligible;
      verify = (fun _priority _ -> incr calls; job);
      bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool item |> expect_pending "private admission";
  advance ();
  Lwt_main.run (Pool.observe pool item)
  |> expect_pending "builder defers slow private job";
  let waiting = Pool.await pool item in
  expect "validator joins slow private job" (Lwt.is_sleeping waiting);
  expect "one private verification started" (!calls = 1);
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "prepared"));
  Lwt_main.run waiting
  |> expect_ready "validator receives private result" "prepared";
  expect "private verification remains unique" (!calls = 1)

let test_cancelled_validator_keeps_shared_job () =
  let item = tx 8 in
  let pending, resolve = Lwt.wait () in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ -> incr calls; pending);
      bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool item |> expect_pending "cancel admission";
  advance ();
  let waiting = Pool.await pool item in
  Lwt.cancel waiting;
  advance ();
  Lwt_main.run (Pool.observe pool item)
  |> expect_pending "cancelled waiter kept job";
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "prepared"));
  advance ();
  Lwt_main.run (Pool.observe pool item)
  |> expect_ready "shared job completed after cancellation" "prepared";
  expect "cancel did not restart job" (!calls = 1)

let test_validator_restarts_stale_artifact () =
  let item = tx 6 in
  let source = ref 1 in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ ->
        incr calls;
        Lwt.return_ok (Pool.Verification_ready !source));
      bind = (fun _ artifact ->
        if artifact = !source then Lwt.return (Pool.Bound artifact)
        else Lwt.return Pool.Source_changed);
    }
  in
  Pool.admit pool item |> expect_pending "stale validator admission";
  advance ();
  source := 2;
  Lwt_main.run (Pool.await pool item)
  |> expect_ready "validator renewed stale artifact" 2;
  expect "validator verified each source once" (!calls = 2)

let test_validator_uses_local_check_after_worker_error () =
  let item = tx 7 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ -> Lwt.return_error "worker unavailable");
      bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool item |> expect_pending "failed validator admission";
  Lwt_main.run (Pool.await pool item)
  |> expect_unmanaged "validator synchronous retry"

let test_required_job_precedes_speculative_queue () =
  let first = tx ~from:"octSlotFirst" 1 in
  let second = tx ~from:"octSlotSecond" 1 in
  let required = tx ~from:"octSlotRequired" 1 in
  let first_job, resolve_first = Lwt.wait () in
  let second_job, resolve_second = Lwt.wait () in
  let required_job, resolve_required = Lwt.wait () in
  let calls = ref [] in
  let pool =
    Pool.create
      ~max_running:1
      ~max_queued:6
      {
        eligible = (fun _ -> true);
        verify = (fun _priority item ->
          calls := !calls @ [item.Transaction.from];
          if item.Transaction.from = first.Transaction.from then first_job
          else if item.Transaction.from = second.Transaction.from then second_job
          else required_job);
        bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
      }
  in
  Pool.admit pool first |> expect_pending "first worker slot";
  Pool.admit pool second |> expect_pending "second worker slot";
  Pool.admit pool required |> expect_pending "required worker slot";
  advance ();
  expect "only one speculative job started"
    (!calls = [first.Transaction.from]);
  let waiting = Pool.await pool required in
  expect "required job waits in local queue" (Lwt.is_sleeping waiting);
  let queued = Pool.stats pool in
  expect "bounded queue reports active and waiting work"
    (queued.running = 1 && queued.queued = 2 && queued.pending = 3);
  Lwt.wakeup resolve_first (Ok (Pool.Verification_ready "first"));
  advance ();
  expect "required job starts before older speculative work"
    (!calls = [first.Transaction.from; required.Transaction.from]);
  Lwt.wakeup resolve_required (Ok (Pool.Verification_ready "required"));
  Lwt_main.run waiting
  |> expect_ready "required validator result" "required";
  advance ();
  expect "speculative work resumes after required result"
    (!calls =
       [
         first.Transaction.from;
         required.Transaction.from;
         second.Transaction.from;
       ]);
  Lwt.wakeup resolve_second (Ok (Pool.Verification_ready "second"));
  advance ();
  Lwt_main.run (Pool.observe pool second)
  |> expect_ready "second job completed" "second"

let test_required_job_displaces_full_speculative_queue () =
  let active = tx ~from:"octQueueActive" 1 in
  let old = tx ~from:"octQueueOld" 1 in
  let recent = tx ~from:"octQueueRecent" 1 in
  let required = tx ~from:"octQueueRequired" 1 in
  let active_job, resolve_active = Lwt.wait () in
  let old_job, _ = Lwt.wait () in
  let recent_job, _ = Lwt.wait () in
  let required_job, resolve_required = Lwt.wait () in
  let calls = ref [] in
  let pool =
    Pool.create
      ~max_running:1
      ~max_queued:2
      {
        eligible = (fun _ -> true);
        verify = (fun _priority item ->
          calls := !calls @ [item.Transaction.from];
          if item.Transaction.from = active.Transaction.from then active_job
          else if item.Transaction.from = old.Transaction.from then old_job
          else if item.Transaction.from = recent.Transaction.from then recent_job
          else required_job);
        bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
      }
  in
  Pool.admit pool active |> expect_pending "active admission";
  Pool.admit pool old |> expect_pending "old queued admission";
  Pool.admit pool recent |> expect_pending "recent queued admission";
  advance ();
  let waiting = Pool.await pool required in
  expect "required job waits after displacement" (Lwt.is_sleeping waiting);
  let queued = Pool.stats pool in
  expect "queue remains bounded after displacement"
    (queued.running = 1 && queued.queued = 2);
  Lwt.wakeup resolve_active (Ok (Pool.Verification_ready "active"));
  advance ();
  expect "required job takes first released slot"
    (!calls = [active.Transaction.from; required.Transaction.from]);
  Lwt.wakeup resolve_required (Ok (Pool.Verification_ready "required"));
  Lwt_main.run waiting
  |> expect_ready "displaced queue required result" "required"

let test_source_change_restarts_verification () =
  let item = tx 2 in
  let source = ref 1 in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ ->
        incr calls;
        Lwt.return_ok (Pool.Verification_ready !source));
      bind = (fun _ artifact ->
        if artifact = !source then Lwt.return (Pool.Bound artifact)
        else Lwt.return Pool.Source_changed);
    }
  in
  Pool.admit pool item |> expect_pending "source admission";
  advance ();
  Lwt_main.run (Pool.observe pool item) |> expect_ready "initial source" 1;
  source := 2;
  Lwt_main.run (Pool.observe pool item) |> expect_pending "changed source";
  advance ();
  Lwt_main.run (Pool.observe pool item) |> expect_ready "renewed source" 2;
  expect "source change verified twice" (!calls = 2)

let test_distinct_wallet_jobs_run_together () =
  let first = tx ~from:"octFirst" 1 in
  let second = tx ~from:"octSecond" 1 in
  let first_job, resolve_first = Lwt.wait () in
  let second_job, resolve_second = Lwt.wait () in
  let started = ref [] in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority item ->
        started := item.Transaction.from :: !started;
        if item.Transaction.from = first.Transaction.from then first_job
        else second_job);
      bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool first |> expect_pending "first wallet admission";
  Pool.admit pool second |> expect_pending "second wallet admission";
  advance ();
  expect "distinct wallet jobs started"
    (List.sort String.compare !started = ["octFirst"; "octSecond"]);
  Lwt.wakeup resolve_first (Ok (Pool.Verification_ready "first"));
  Lwt.wakeup resolve_second (Ok (Pool.Verification_ready "second"));
  advance ();
  Lwt_main.run (Pool.observe pool first) |> expect_ready "first wallet ready" "first";
  Lwt_main.run (Pool.observe pool second)
  |> expect_ready "second wallet ready" "second"

let test_retain_keeps_running_validator_job () =
  let item = tx 3 in
  let job, resolve = Lwt.wait () in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ ->
        incr calls;
        job);
      bind = (fun _ (artifact : string) -> Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool item |> expect_pending "retained job";
  advance ();
  let waiting = Pool.await pool item in
  Pool.retain pool (fun _ -> false);
  expect "retained validator still waits" (Lwt.is_sleeping waiting);
  expect "retain did not restart work" (!calls = 1);
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "prepared"));
  Lwt_main.run waiting
  |> expect_ready "retained validator receives result" "prepared";
  expect "retained job ran once" (!calls = 1)

let test_validator_bounds_source_restarts () =
  let item = tx 9 in
  let calls = ref 0 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ ->
        incr calls;
        Lwt.return_ok (Pool.Verification_ready !calls));
      bind = (fun _ _ -> Lwt.return Pool.Source_changed);
    }
  in
  Pool.admit pool item |> expect_pending "bounded source admission";
  Lwt_main.run (Pool.await pool item)
  |> expect_unmanaged "bounded source local check";
  expect "source retries are finite" (!calls = 2)

let test_invalid_result_is_stable () =
  let item = tx 4 in
  let calls = ref 0 in
  let source = ref 1 in
  let pool =
    Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _priority _ ->
        incr calls;
        if !source = 1 then
          Lwt.return_ok
            (Pool.Verification_rejected (!source, "invalid proof"))
        else
          Lwt.return_ok (Pool.Verification_ready !source));
      bind = (fun _ artifact ->
        if artifact <> !source then Lwt.return Pool.Source_changed
        else if artifact = 1 then
          Lwt.return (Pool.Source_invalid "invalid proof")
        else
          Lwt.return (Pool.Bound artifact));
    }
  in
  Pool.admit pool item |> expect_pending "invalid admission";
  advance ();
  begin
    match Lwt_main.run (Pool.observe pool item) with
    | Availability.Invalid reason -> expect "invalid reason" (reason = "invalid proof")
    | Availability.Unmanaged -> fail "invalid result unmanaged"
    | Availability.Pending -> fail "invalid result pending"
    | Availability.Ready _ -> fail "invalid result ready"
  end;
  Pool.admit pool item |> expect_pending "invalid duplicate";
  Lwt_main.run (Pool.observe pool item)
  |> expect_invalid "invalid duplicate binding" "invalid proof";
  advance ();
  expect "invalid result not repeated" (!calls = 1);
  source := 2;
  Lwt_main.run (Pool.observe pool item) |> expect_pending "invalid source changed";
  advance ();
  Lwt_main.run (Pool.observe pool item) |> expect_ready "changed source ready" 2;
  expect "changed source verified again" (!calls = 2)

let test_artifact_lookup () =
  let item = tx 17 in
  let pending, resolve = Lwt.wait () in
  let pool = Pool.create {
    eligible = (fun _ -> true);
    verify = (fun _ _ -> pending);
    bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
  } in
  expect "missing artifact" (Pool.artifact pool item = None);
  ignore (Pool.admit pool item);
  expect "queued artifact" (Pool.artifact pool item = None);
  advance ();
  expect "running artifact" (Pool.artifact pool item = None);
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "checked"));
  advance ();
  expect "completed artifact" (Pool.artifact pool item = Some "checked");
  expect "different transaction" (Pool.artifact pool (tx 18) = None);
  Pool.retain pool (fun _ -> false);
  expect "removed artifact" (Pool.artifact pool item = None);
  let rejected = Pool.create {
    eligible = (fun _ -> true);
    verify = (fun _ _ ->
      Lwt.return_ok (Pool.Verification_rejected ("invalid", "reason")));
    bind = (fun _ _ -> Lwt.return (Pool.Source_invalid "reason"));
  } in
  ignore (Lwt_main.run (Pool.await rejected item));
  expect "rejected artifact" (Pool.artifact rejected item = None)

let test_collect () =
  let item = tx 19 in
  let calls = ref 0 in
  let pending, resolve = Lwt.wait () in
  let pool = Pool.create {
    eligible = (fun _ -> true);
    verify = (fun _ _ -> incr calls; pending);
    bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
  } in
  expect "missing collection does not launch work"
    (Lwt_main.run (Pool.collect pool [item]) = [] && !calls = 0);
  ignore (Pool.admit pool item);
  advance ();
  expect "instant lookup misses running artifact" (Pool.artifact pool item = None);
  let cancelled = Pool.collect pool [item] in
  Lwt.cancel cancelled;
  expect "collection cancellation preserves job" (Lwt.is_sleeping pending);
  let applied = Pool.collect pool [item; tx 20] in
  let proposal = Pool.await pool item in
  expect "apply joins running proposal check" (Lwt.is_sleeping applied && !calls = 1);
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "checked"));
  expect "apply receives exact verified artifact"
    (Lwt_main.run applied = [Transaction.hash item, "checked"]);
  Lwt_main.run proposal |> expect_ready "proposal shares verification" "checked";
  expect "only one verification" (!calls = 1);
  expect "completed collection reuses artifact"
    (Lwt_main.run (Pool.collect pool [item]) = [Transaction.hash item, "checked"]);
  Pool.retain pool (fun _ -> false);
  expect "removed collection does not restart"
    (Lwt_main.run (Pool.collect pool [item]) = [] && !calls = 1)

let test_collect_queue () =
  let first = tx 21 and second = tx 22 in
  let pending, resolve = Lwt.wait () in
  let calls = ref [] in
  let pool = Pool.create ~max_running:1 {
    eligible = (fun _ -> true);
    verify = (fun priority item ->
      calls := (priority, item.Transaction.nonce) :: !calls;
      if item.nonce = first.nonce then pending
      else Lwt.return_ok (Pool.Verification_ready "second"));
    bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
  } in
  ignore (Pool.admit pool first);
  ignore (Pool.admit pool second);
  advance ();
  let applied = Pool.collect pool [second] in
  expect "collection waits for queued work" (Lwt.is_sleeping applied);
  Lwt.wakeup resolve (Ok (Pool.Verification_ready "first"));
  expect "queued collection result"
    (Lwt_main.run applied = [Transaction.hash second, "second"]);
  expect "queued collection promotes existing work"
    (!calls = [Octra_core.Compute_pool.Required, second.nonce;
               Octra_core.Compute_pool.Speculative, first.nonce])

let test_collect_errors () =
  List.iter (fun result ->
    let item = tx 23 in
    let calls = ref 0 in
    let pending, resolve = Lwt.wait () in
    let pool = Pool.create {
      eligible = (fun _ -> true);
      verify = (fun _ _ -> incr calls; pending);
      bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
    } in
    ignore (Pool.admit pool item);
    advance ();
    let applied = Pool.collect pool [item] in
    Lwt.wakeup resolve result;
    expect "only verified results are collected" (Lwt_main.run applied = []);
    expect "failed collection does not restart" (!calls = 1))
    [Error "unavailable";
     Ok Pool.Verification_stale;
     Ok (Pool.Verification_rejected ("rejected", "reason"))]

let test_collect_batch () =
  let active = tx 25 and older = tx 26 and first = tx 27 and second = tx 28 in
  let active_job, active_done = Lwt.wait () in
  let older_job, older_done = Lwt.wait () in
  let first_job, first_done = Lwt.wait () in
  let second_job, second_done = Lwt.wait () in
  let calls = ref [] in
  let pool = Pool.create ~max_running:1 ~max_queued:3 {
    eligible = (fun _ -> true);
    verify = (fun priority item ->
      calls := !calls @ [priority, item.Transaction.nonce];
      List.assoc item.nonce
        [active.nonce, active_job; older.nonce, older_job;
         first.nonce, first_job; second.nonce, second_job]);
    bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
  } in
  List.iter (fun item -> ignore (Pool.admit pool item)) [active; older; first; second];
  advance ();
  let applied = Pool.collect pool [first; second; tx 29] in
  expect "batch waits for existing work" (Lwt.is_sleeping applied);
  Lwt.wakeup active_done (Ok (Pool.Verification_ready "active"));
  advance ();
  expect "first selected job precedes older work"
    (!calls = [Octra_core.Compute_pool.Speculative, active.nonce;
               Octra_core.Compute_pool.Required, first.nonce]);
  Lwt.wakeup first_done (Ok (Pool.Verification_ready "first"));
  advance ();
  expect "entire batch precedes older work"
    (!calls = [Octra_core.Compute_pool.Speculative, active.nonce;
               Octra_core.Compute_pool.Required, first.nonce;
               Octra_core.Compute_pool.Required, second.nonce]);
  Lwt.wakeup second_done (Ok (Pool.Verification_ready "second"));
  expect "batch preserves input order and skips missing entries"
    (Lwt_main.run applied =
      [Transaction.hash first, "first"; Transaction.hash second, "second"]);
  advance ();
  expect "older work remains scheduled"
    (List.hd (List.rev !calls) = (Octra_core.Compute_pool.Speculative, older.nonce));
  Lwt.wakeup older_done (Ok (Pool.Verification_ready "older"));
  advance ()

let test_collect_exception () =
  let item = tx 24 in
  let pending, resolve = Lwt.wait () in
  let pool = Pool.create {
    eligible = (fun _ -> true);
    verify = (fun _ _ -> pending);
    bind = (fun _ artifact -> Lwt.return (Pool.Bound artifact));
  } in
  ignore (Pool.admit pool item);
  advance ();
  let applied = Pool.collect pool [item] in
  Lwt.wakeup_exn resolve (Failure "planned worker exit");
  expect "worker exit requires normal verification" (Lwt_main.run applied = []);
  expect "worker exit releases running entry" ((Pool.stats pool).running = 0)

let () =
  test_collect_batch ();
  test_collect ();
  test_collect_queue ();
  test_collect_errors ();
  test_collect_exception ();
  test_artifact_lookup ();
  test_duplicate_job_and_ready_binding ();
  test_validator_joins_running_job ();
  test_private_operation_coverage ();
  test_builder_defers_slow_private_job ();
  test_cancelled_validator_keeps_shared_job ();
  test_validator_restarts_stale_artifact ();
  test_validator_uses_local_check_after_worker_error ();
  test_required_job_precedes_speculative_queue ();
  test_required_job_displaces_full_speculative_queue ();
  test_source_change_restarts_verification ();
  test_distinct_wallet_jobs_run_together ();
  test_retain_keeps_running_validator_job ();
  test_validator_bounds_source_restarts ();
  test_invalid_result_is_stable ();
  print_endline "status = pass test = preverify_pool"