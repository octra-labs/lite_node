(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_node_runtime.Consensus_tick_plan

let fail msg =
  failwith ("test_tick_plan: " ^ msg)

let expect label cond =
  if not cond then fail label

let expect_plan label expected_action expected_log expected_sleep got =
  expect (label ^ " action") (got.P.action = expected_action);
  expect (label ^ " log") (got.P.log_tick = expected_log);
  expect (label ^ " sleep") (got.P.next_sleep = expected_sleep)

let test_non_consensus () =
  P.plan
    ~consensus_mode:false
    ~epoch_duration:10.
    ~elapsed:5.
    ~finalized_state:P.No_trigger
  |> expect_plan "non-consensus wait" P.Wait true 10.;
  P.plan
    ~consensus_mode:false
    ~epoch_duration:10.
    ~elapsed:10.
    ~finalized_state:P.No_trigger
  |> expect_plan "non-consensus apply" P.Apply true 10.

let test_consensus_idle () =
  P.plan
    ~consensus_mode:true
    ~epoch_duration:10.
    ~elapsed:100.
    ~finalized_state:P.No_trigger
  |> expect_plan "consensus idle" P.Wait false 0.1

let test_consensus_missing_header () =
  P.plan
    ~consensus_mode:true
    ~epoch_duration:10.
    ~elapsed:100.
    ~finalized_state:P.Missing_header
  |> expect_plan "missing header" P.Clear_stale_trigger false 0.1

let test_consensus_ready () =
  P.plan
    ~consensus_mode:true
    ~epoch_duration:10.
    ~elapsed:100.
    ~finalized_state:P.Cached_bundle
  |> expect_plan "cached bundle" P.Apply true 0.1;
  P.plan
    ~consensus_mode:true
    ~epoch_duration:10.
    ~elapsed:100.
    ~finalized_state:P.Empty_bundle
  |> expect_plan "empty bundle" P.Store_empty_bundle_and_apply true 0.1

let test_consensus_ready_deadline () =
  List.iter (fun epoch_duration ->
    List.iter (fun elapsed ->
      P.plan ~consensus_mode:true ~epoch_duration ~elapsed
        ~finalized_state:P.Cached_bundle
      |> expect_plan "certificate applies" P.Apply true 0.1;
      P.plan ~consensus_mode:true ~epoch_duration ~elapsed
        ~finalized_state:P.Empty_bundle
      |> expect_plan "empty certificate applies" P.Store_empty_bundle_and_apply true 0.1;
      P.plan ~consensus_mode:true ~epoch_duration ~elapsed
        ~finalized_state:P.No_trigger
      |> expect_plan "no certificate waits" P.Wait false 0.1)
      [-100.; 0.; 0.01; 11.999; 12.; 100.])
    [10.; 12.; 30.]

let test_consensus_missing_bundle () =
  let got =
    P.plan
      ~consensus_mode:true
      ~epoch_duration:10.
      ~elapsed:100.
      ~finalized_state:(P.Missing_bundle { target_epoch = 12L })
  in
  match got.P.action with
  | P.Queue_missing_bundle q ->
    expect "missing bundle target" (q.target_epoch = 12L);
    expect "missing bundle reason" (q.reason = "finalized_bundle_missing_tick");
    expect "missing bundle no log" (not got.P.log_tick);
    expect "missing bundle sleep" (got.P.next_sleep = 0.1)
  | _ -> fail "missing bundle action"

let test_finalized_state () =
  let module C = Octra_consensus.C_types in
  let header =
    C.{
      proto_version = C.proto_version_current;
      chain_id = "dev";
      epoch_id = 12L;
      prev_state_root = String.make 32 '\x00';
      tx_list_hash = String.make 32 '\x02';
      receipt_root = String.make 32 '\x03';
      proposed_state_root = String.make 32 '\x01';
      parent_commit_hash = Octra_net.Hash_domain.nil_hash;
      creator_addr = "octA";
      txid_hi = 9L;
      ts = 1.;
    }
  in
  let finalize =
    C.{
      chain_id = "dev";
      epoch_id = 12L;
      commit_round = 1;
      header;
      proposal_id = "pid";
      precommits = [];
      parent_commit = None;
    }
  in
  let state =
    P.finalized_state
      ~consensus_mode:true
      ~consensus_finalized:true
      ~current_epoch:12
      ~find_finalized:(fun _ -> Some finalize)
      ~cached_bundle_for_pid:(fun _ -> false)
      ~header_has_empty_bundle:(fun _ -> true)
  in
  expect "empty bundle state" (state.P.state = P.Empty_bundle);
  expect "empty bundle header" (state.P.empty_bundle_header = Some header);
  let missing =
    P.finalized_state
      ~consensus_mode:true
      ~consensus_finalized:true
      ~current_epoch:12
      ~find_finalized:(fun _ -> Some finalize)
      ~cached_bundle_for_pid:(fun _ -> false)
      ~header_has_empty_bundle:(fun _ -> false)
  in
  expect "missing bundle state"
    (missing.P.state = P.Missing_bundle { target_epoch = 12L });
  expect "missing bundle no header" (missing.P.empty_bundle_header = None);
  let idle =
    P.finalized_state
      ~consensus_mode:true
      ~consensus_finalized:false
      ~current_epoch:12
      ~find_finalized:(fun _ -> fail "unexpected lookup")
      ~cached_bundle_for_pid:(fun _ -> false)
      ~header_has_empty_bundle:(fun _ -> false)
  in
  expect "idle no lookup" (idle.P.state = P.No_trigger)

let test_should_apply () =
  expect "apply should apply" (P.should_apply P.Apply);
  expect "empty should apply" (P.should_apply P.Store_empty_bundle_and_apply);
  expect "wait should not apply" (not (P.should_apply P.Wait));
  expect "queue should not apply"
    (not (P.should_apply (P.Queue_missing_bundle {
      target_epoch = 1L;
      reason = "r";
    })))

let test_action_order () =
  let events = ref [] in
  let push event =
    events := event :: !events
  in
  P.apply_action
    ~current_epoch:7
    ~clear_trigger:true
    ~store_empty_bundle:(fun () -> push "store")
    ~queue_missing_bundle:(fun ~target_epoch ~reason ->
      push (Printf.sprintf "queue:%Ld:%s" target_epoch reason))
    ~warn:(fun msg -> push ("warn:" ^ msg))
    ~clear_finalized:(fun () -> push "clear")
    (P.Queue_missing_bundle {
      target_epoch = 9L;
      reason = "missing";
    });
  expect "queue action order"
    (List.rev !events = [
      "queue:9:missing";
      "warn:BFT finalize trigger epoch = 7 deferred = canonical_bundle_missing";
      "clear";
    ]);
  events := [];
  P.apply_action
    ~current_epoch:7
    ~clear_trigger:true
    ~store_empty_bundle:(fun () -> push "store")
    ~queue_missing_bundle:(fun ~target_epoch:_ ~reason:_ -> push "queue")
    ~warn:(fun msg -> push ("warn:" ^ msg))
    ~clear_finalized:(fun () -> push "clear")
    P.Store_empty_bundle_and_apply;
  expect "store action order" (List.rev !events = ["store"; "clear"])

let () =
  test_non_consensus ();
  test_consensus_idle ();
  test_consensus_missing_header ();
  test_consensus_ready ();
  test_consensus_ready_deadline ();
  test_consensus_missing_bundle ();
  test_finalized_state ();
  test_should_apply ();
  test_action_order ();
  print_endline "status = pass test = tick_plan"