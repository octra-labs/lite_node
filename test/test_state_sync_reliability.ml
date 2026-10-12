(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Cycle = Octra_bootstrap.Sync_cycle
module Client = Octra_bootstrap.State_sync_client
module Chain = Octra_node_runtime.Sync_chain
module Http = Octra_node_runtime.State_sync_http
module Mode = Octra_node_runtime.Consensus_mode
module Publish = Octra_node_runtime.Sync_publish

let fail reason =
  failwith ("test_state_sync_reliability: " ^ reason)

let expect_ok = function
  | Ok value -> value
  | Error reason -> fail reason

let expect_effects expected actual =
  if expected <> actual then fail "effects differ"

let test_published_epoch () =
  let policy = expect_ok (Cycle.policy ~interval:10L ~retain:2) in
  let state = Cycle.init ~published:(Some 120L) in
  let state, effects =
    expect_ok (Cycle.step policy state (Cycle.Finalized 137L))
  in
  expect_effects [Cycle.Capture 130L] effects;
  let state, effects =
    expect_ok
      (Cycle.step
         policy
         state
         (Cycle.Completed {
            epoch = 130L;
            outcome = Cycle.Published 137L;
          }))
  in
  expect_effects [Cycle.Retain 2] effects;
  if Cycle.published state <> Some 137L then
    fail "actual published epoch was lost";
  let state, effects =
    expect_ok (Cycle.step policy state (Cycle.Finalized 139L))
  in
  expect_effects [] effects;
  let _, effects =
    expect_ok (Cycle.step policy state (Cycle.Finalized 140L))
  in
  expect_effects [Cycle.Capture 140L] effects

let test_published_before_capture () =
  let policy = expect_ok (Cycle.policy ~interval:10L ~retain:2) in
  let state, _ =
    Cycle.init ~published:None
    |> fun state -> expect_ok (Cycle.step policy state (Cycle.Finalized 137L))
  in
  match
    Cycle.step
      policy
      state
      (Cycle.Completed {
         epoch = 130L;
         outcome = Cycle.Published 129L;
       })
  with
  | Error "sync published epoch precedes capture target" -> ()
  | Error reason -> fail ("publication before capture reason differs: " ^ reason)
  | Ok _ -> fail "publication before capture was accepted"

let test_activation_cycle () =
  let activation = 1_663_000L in
  let before = 1_662_840L in
  let after = Int64.succ activation in
  let event epoch = Publish.cycle_event
    ~chain_id:"octra-devnet-9871-cluster" ~epoch in
  let tick state epoch = expect_ok (Cycle.step Cycle.default state (event epoch)) in
  let complete state epoch outcome =
    Cycle.step Cycle.default state (Cycle.Completed { epoch; outcome }) in
  if event activation <> Cycle.Finalized_after {
    epoch = activation;
    minimum = activation;
  } then fail "publisher omitted activation minimum";
  if event (Int64.pred activation) <> Cycle.Finalized_after {
    epoch = Int64.pred activation;
    minimum = 1_614_500L;
  } then fail "publisher selected future activation";
  let initial = Cycle.init ~published:(Some before) in
  let state, effects = tick initial (Int64.pred activation) in
  expect_effects [] effects;
  let state, effects = tick state activation in
  expect_effects [Cycle.Capture activation] effects;
  if Cycle.running state <> Some activation then fail "activation capture not running";
  let state, effects = tick state after in
  expect_effects [] effects;
  begin match complete state after (Cycle.Published after) with
  | Error "sync completion epoch mismatch" -> ()
  | _ -> fail "different capture completion was accepted"
  end;
  begin match complete state activation (Cycle.Published (Int64.pred activation)) with
  | Error "sync published epoch precedes capture target" -> ()
  | _ -> fail "preactivation publication was accepted"
  end;
  let state, effects = expect_ok (complete state activation Cycle.Failed) in
  expect_effects [] effects;
  if Cycle.published state <> Some before then fail "failed capture lost old image";
  let state, effects = tick state after in
  expect_effects [Cycle.Capture activation] effects;
  let state, effects = expect_ok (complete state activation (Cycle.Published after)) in
  expect_effects [Cycle.Retain 2] effects;
  if Cycle.published state <> Some after || Cycle.running state <> None then
    fail "activation publication did not complete";
  let state, effects = tick state after in
  expect_effects [] effects;
  let _, effects = tick state (Int64.pred activation) in
  expect_effects [] effects;
  let _, effects = tick state 1_663_200L in
  expect_effects [Cycle.Capture 1_663_200L] effects;
  let _, effects = tick (Cycle.init ~published:(Some after)) after in
  expect_effects [] effects;
  let _, effects = tick initial after in
  expect_effects [Cycle.Capture activation] effects;
  let state, _ = tick initial activation in
  let state, _ = expect_ok (complete state activation (Cycle.Published after)) in
  begin match complete state activation (Cycle.Published after) with
  | Error "sync completion has no active capture" -> ()
  | _ -> fail "duplicate capture completion was accepted"
  end

let test_capture_crossing () =
  let policy = expect_ok (Cycle.policy ~interval:10L ~retain:2) in
  let state, _ = expect_ok (Cycle.step policy
    (Cycle.init ~published:(Some 120L)) (Cycle.Finalized 137L)) in
  let event = Cycle.Finalized_after { epoch = 138L; minimum = 138L } in
  let state, effects = expect_ok (Cycle.step policy state event) in
  expect_effects [] effects;
  let state, effects = expect_ok (Cycle.step policy state
    (Cycle.Completed { epoch = 130L; outcome = Cycle.Published 137L })) in
  expect_effects [Cycle.Retain 2] effects;
  let _, effects = expect_ok (Cycle.step policy state event) in
  expect_effects [Cycle.Capture 138L] effects

let test_cycle_minimum () =
  let policy = expect_ok (Cycle.policy ~interval:10L ~retain:2) in
  let initial = Cycle.init ~published:(Some 120L) in
  List.iter (fun (epoch, minimum, reason) ->
    match Cycle.step policy initial (Cycle.Finalized_after { epoch; minimum }) with
    | Error actual when actual = reason -> ()
    | _ -> fail "invalid capture minimum was accepted") [
      -1L, 0L, "finalized epoch is negative";
      137L, -1L, "sync minimum epoch is negative";
      137L, 138L, "sync minimum epoch exceeds finalized head";
    ];
  for epoch = 0 to 150 do
    let epoch = Int64.of_int epoch in
    let legacy = Cycle.step policy initial (Cycle.Finalized epoch) in
    let actual = Cycle.step policy initial
      (Cycle.Finalized_after { epoch; minimum = 0L }) in
    if actual <> legacy then fail "zero minimum changed interval cycle"
  done;
  if Publish.cycle_event ~chain_id:"unconfigured-chain" ~epoch:1_663_001L
     <> Cycle.Finalized 1_663_001L then
    fail "unconfigured chain changed interval cycle";
  if Publish.cycle_event ~chain_id:"octra-devnet-9871-cluster" ~epoch:1_499_999L
     <> Cycle.Finalized 1_499_999L then
    fail "publisher selected activation before first profile"

let mkdir path mode =
  if not (Sys.file_exists path) then Unix.mkdir path mode

let write path value =
  let output = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () -> output_string output value)

let test_read_only_retention () =
  mkdir "runtime_data" 0o750;
  let root =
    Filename.concat
      "runtime_data"
      (Printf.sprintf "state_sync_reliability_%d" (Unix.getpid ()))
  in
  mkdir root 0o750;
  let snapshot_id = String.make 64 'a' in
  let snapshot = Filename.concat root snapshot_id in
  let nested = Filename.concat snapshot "nested" in
  mkdir snapshot 0o750;
  mkdir nested 0o750;
  write (Filename.concat nested "value") "value";
  Unix.chmod nested 0o555;
  Unix.chmod snapshot 0o555;
  begin
    match Publish.remove_snapshots root [snapshot_id] with
    | [] -> ()
    | (_, reason) :: _ -> fail reason
  end;
  if Sys.file_exists snapshot then fail "read-only snapshot remains";
  Unix.rmdir root

let test_manifest_epoch_limit () =
  let limit = Http.manifest_epoch_limit in
  if limit <> 3_000L then fail "snapshot lag allowance differs";
  if limit >= Octra_node_runtime.Consensus_finality_journal.history_limit then
    fail "snapshot exceeds retained finality";
  List.iter (fun lag ->
    let actual = Http.snapshot_epoch_state
      ~current_epoch:10_000L ~snapshot_epoch:(Int64.sub 10_000L lag) in
    let expected = if lag <= 3_000L then `Ready lag else `Old_epoch lag in
    if actual <> expected then fail "snapshot lag decision differs")
    [0L; 360L; 720L; 721L; 1_060L; 2_999L; 3_000L; 3_001L; 4_096L; 5_000L]

let test_staged_retention () =
  Test_workspace.with_dir "sync_archive" (fun root ->
    let key = "OCTRA_STATE_SYNC_SNAPSHOT_DIR" in
    let prior = Sys.getenv_opt key in
    Fun.protect
      ~finally:(fun () -> Unix.putenv key (Option.value ~default:"" prior))
      (fun () ->
        Unix.putenv key root;
        let stage = Filename.concat root (String.make 64 'b' ^ ".next") in
        Unix.mkdir stage 0o750;
        ignore (Octra_bootstrap.Sync_archive.run root (fun archive ->
          Octra_bootstrap.Sync_archive.mark_stage archive (String.make 64 'b'))
          |> expect_ok);
        write (Filename.concat stage "ledger.dat") "unfinished";
        let errors = Publish.retain root ~retain:2 ~current:(fun () -> String.make 64 'c') in
        if errors <> [] then fail "staged retention rejected its own path";
        if Sys.file_exists stage then fail "abandoned stage remains"))

let test_committed_epoch () =
  let current = ref 1_001 in
  if Http.committed_epoch current <> 1_000L then
    fail "committed epoch differs from live head";
  current := 0;
  if Http.committed_epoch current <> 0L then
    fail "genesis committed epoch is negative"

let test_busy_delay () =
  if Client.busy_delay ~now:10. ~deadline:13. 3. <> Some 3. then
    fail "busy delay at deadline was rejected";
  if Client.busy_delay ~now:10. ~deadline:13. 4. <> None then
    fail "busy delay beyond deadline was accepted"

let test_publisher_mode () =
  let base = Mode.of_inputs ~cli_observer:false ~env_mode:(Some "bft") in
  let mode = Mode.publisher base in
  if not mode.consensus_enabled
     || mode.voting_enabled
     || not mode.observer_enabled
     || mode.label <> "publisher" then
    fail "publisher mode does not follow without voting"

let test_bridge_epoch_limit () =
  match
    Chain.bridge_range
      ~head_epoch:200L
      ~after_epoch:100L
      ~before_epoch:150L
      ~activate_epoch:120L
  with
  | Some (120L, 149L) -> ()
  | _ -> fail "bridge range reused the next transition epoch"

let () =
  test_published_epoch ();
  test_published_before_capture ();
  test_activation_cycle ();
  test_capture_crossing ();
  test_cycle_minimum ();
  test_read_only_retention ();
  test_staged_retention ();
  Sync_archive_case.run ();
  test_manifest_epoch_limit ();
  test_committed_epoch ();
  test_busy_delay ();
  test_publisher_mode ();
  test_bridge_epoch_limit ();
  print_endline "status = pass test = state_sync_reliability"