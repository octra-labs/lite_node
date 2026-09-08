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

let test_epoch_regression () =
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
  | Error reason -> fail ("publication regression reason differs: " ^ reason)
  | Ok _ -> fail "publication regression was accepted"

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
  begin
    match
      Http.snapshot_epoch_state
        ~current_epoch:1_000L
        ~snapshot_epoch:(Int64.sub 1_000L limit)
    with
    | `Ready lag when lag = limit -> ()
    | _ -> fail "snapshot at lag limit was rejected"
  end;
  begin
    match
      Http.snapshot_epoch_state
        ~current_epoch:1_000L
        ~snapshot_epoch:(Int64.pred (Int64.sub 1_000L limit))
    with
    | `Old_epoch lag when lag = Int64.succ limit -> ()
    | _ -> fail "old snapshot was accepted"
  end

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
  test_epoch_regression ();
  test_read_only_retention ();
  test_manifest_epoch_limit ();
  test_committed_epoch ();
  test_busy_delay ();
  test_publisher_mode ();
  test_bridge_epoch_limit ();
  print_endline "test_state_sync_reliability: ok"