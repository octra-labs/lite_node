(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module T = Octra_consensus.Epoch_time
module Policy = Octra_consensus.C_epoch_time_policy

let expect label value =
  if not value then failwith label

let expect_error label value =
  match value with
  | Ok _ -> failwith label
  | Error _ -> ()

let test_conversion () =
  match T.of_seconds 1_700_000_000.123 with
  | Ok value -> expect "millisecond conversion" (value = 1_700_000_000_123L)
  | Error _ -> failwith "millisecond conversion rejected"

let test_check_accepts () =
  match T.check ~now:1_700_000_000.0 ~previous:None ~candidate:1_700_000_001.0 with
  | Ok value -> expect "accepted timestamp" (value = 1_700_000_001_000L)
  | Error _ -> failwith "accepted timestamp rejected"

let test_check_accepts_protocol_interval () =
  match
    T.check
      ~now:1_700_000_010.0
      ~previous:(Some 1_700_000_000_000L)
      ~candidate:1_700_000_010.0
  with
  | Ok value ->
    expect "protocol interval timestamp" (value = 1_700_000_010_000L)
  | Error _ -> failwith "protocol interval timestamp rejected"

let test_check_accepts_schedule_debt () =
  match
    T.check
      ~now:1_700_000_100.0
      ~previous:(Some 1_700_000_000_000L)
      ~candidate:1_700_000_010.0
  with
  | Ok value ->
    expect "schedule debt timestamp" (value = 1_700_000_010_000L)
  | Error _ -> failwith "schedule debt timestamp rejected"

let test_next_delay_before_boundary () =
  expect "next delay before boundary"
    (T.next_delay_ms ~now:1_700_000_005.0 ~previous:1_700_000_000.0
     = 5_000L)

let test_next_delay_at_boundary () =
  expect "next delay at boundary"
    (T.next_delay_ms ~now:1_700_000_010.0 ~previous:1_700_000_000.0
     = 0L)

let test_next_delay_after_boundary () =
  expect "next delay after boundary"
    (T.next_delay_ms ~now:1_700_000_100.0 ~previous:1_700_000_000.0
     = 0L)

let test_check_rejects_drift () =
  expect_error "drift accepted"
    (T.check ~now:1_700_000_000.0 ~previous:None ~candidate:1_700_030_001.0)

let test_check_rejects_short_interval () =
  expect_error "short protocol interval accepted"
    (T.check
       ~now:1_700_000_009.0
       ~previous:(Some 1_700_000_000_000L)
       ~candidate:1_700_000_009.999)

let test_check_rejects_future_time () =
  expect_error "future epoch time accepted"
    (T.check
       ~now:1_700_000_010.0
       ~previous:(Some 1_700_000_005_001L)
       ~candidate:1_700_000_015.001)

let test_check_rejects_backwards () =
  expect_error "backwards timestamp accepted"
    (T.check
       ~now:1_700_000_000.0
       ~previous:(Some 1_700_000_001_000L)
       ~candidate:1_700_000_000.0)

let test_check_rejects_non_finite () =
  expect_error "nan timestamp accepted"
    (T.check ~now:(0. /. 0.) ~previous:None ~candidate:1.0);
  expect_error "infinite timestamp accepted"
    (T.check ~now:1.0 ~previous:None ~candidate:(1. /. 0.))

let test_check_rejects_range () =
  expect_error "negative timestamp accepted"
    (T.check ~now:1.0 ~previous:None ~candidate:(-1.0))

let test_reproposal_accepts_old_monotonic_time () =
  match
    T.check_reproposal
      ~previous:(Some 1_700_000_000_000L)
      ~candidate:1_700_000_010.0
  with
  | Ok value ->
    expect "reproposal timestamp" (value = 1_700_000_010_000L)
  | Error _ -> failwith "old reproposal timestamp rejected"

let test_reproposal_rejects_short_interval () =
  expect_error "short reproposal interval accepted"
    (T.check_reproposal
       ~previous:(Some 1_700_000_000_000L)
       ~candidate:1_700_000_009.999)

let test_reproposal_rejects_backwards_time () =
  expect_error "backwards reproposal timestamp accepted"
    (T.check_reproposal
       ~previous:(Some 1_700_000_001_000L)
       ~candidate:1_700_000_000.0)

let future_candidate = 1_700_000_100.0

let prior_time = Some 1_700_000_000_000L

let test_historical_reproposal_preserves_prior_rule () =
  match
    T.check_proposal
      ~rule:T.Historical
      ~kind:T.Reproposal
      ~now:1_700_000_010.0
      ~previous:prior_time
      ~candidate:future_candidate
  with
  | Ok _ -> ()
  | Error _ -> failwith "historical reproposal rule changed"

let test_historical_fresh_proposal_rejects_future_time () =
  expect_error "historical fresh proposal accepted future time"
    (T.check_proposal
       ~rule:T.Historical
       ~kind:T.Fresh
       ~now:1_700_000_010.0
       ~previous:prior_time
       ~candidate:future_candidate)

let test_uniform_reproposal_rejects_future_time () =
  expect_error "uniform reproposal accepted future time"
    (T.check_proposal
       ~rule:T.Uniform
       ~kind:T.Reproposal
       ~now:1_700_000_010.0
       ~previous:prior_time
       ~candidate:future_candidate)

let test_uniform_reproposal_accepts_observed_time () =
  match
    T.check_proposal
      ~rule:T.Uniform
      ~kind:T.Reproposal
      ~now:1_700_000_010.0
      ~previous:prior_time
      ~candidate:1_700_000_010.0
  with
  | Ok _ -> ()
  | Error _ -> failwith "uniform reproposal rejected observed time"

let test_uniform_early_reproposal_reduces_clock_drift () =
  match
    T.check_proposal
      ~rule:T.Uniform
      ~kind:T.Reproposal
      ~now:1_700_000_002.0
      ~previous:(Some 1_699_999_990_000L)
      ~candidate:1_700_000_004.0
  with
  | Ok _ -> ()
  | Error _ -> failwith "uniform early reproposal rejected reduced drift"

let test_policy_activation_boundary () =
  let activation =
    match Policy.activation_for_chain "octra-devnet-9871-cluster" with
    | Some value -> value
    | None -> failwith "devnet epoch time activation missing"
  in
  expect "prior epoch time rule"
    (Policy.rule_for_epoch
       ~chain_id:"octra-devnet-9871-cluster"
       ~epoch_id:(Int64.of_int (activation.activation_epoch - 1))
     = T.Historical);
  expect "activation epoch time rule"
    (Policy.rule_for_epoch
       ~chain_id:"octra-devnet-9871-cluster"
       ~epoch_id:(Int64.of_int activation.activation_epoch)
     = T.Uniform);
  expect "post activation epoch time rule"
    (Policy.rule_for_epoch
       ~chain_id:"octra-devnet-9871-cluster"
       ~epoch_id:(Int64.of_int (activation.activation_epoch + 1))
     = T.Uniform);
  expect "new chain epoch time rule"
    (Policy.rule_for_epoch ~chain_id:"new-chain" ~epoch_id:0L = T.Uniform);
  expect "mainnet requires anchored epoch time activation"
    (Policy.rule_for_epoch
       ~chain_id:"octra-mainnet"
       ~epoch_id:Int64.max_int
     = T.Historical)

let test_finite_recovery () =
  let now = 1_700_000_000.0 in
  let parent = now +. 86_400.0 in
  let previous = Some (T.of_seconds parent |> Result.get_ok) in
  let candidate = parent +. T.interval_seconds in
  List.iter (fun rule ->
    expect_error "fresh proposal accepted before recovery"
      (T.check_proposal ~rule ~kind:T.Fresh ~now:(parent +. 4.0)
         ~previous ~candidate);
    expect "fresh proposal accepted at future limit"
      (Result.is_ok (T.check_proposal ~rule ~kind:T.Fresh
        ~now:(parent +. 5.0) ~previous ~candidate))) [T.Historical; T.Uniform];
  expect "finite future parent retains a finite delay"
    (T.next_delay_ms ~now ~previous:parent = 86_410_000L);
  expect "recovery admission still precedes cadence"
    (T.next_delay_ms ~now:(parent +. 5.0) ~previous:parent = 5_000L);
  expect "cadence resumes at parent plus interval"
    (T.next_delay_ms ~now:candidate ~previous:parent = 0L);
  expect "cadence remains open after recovery"
    (T.next_delay_ms ~now:(candidate +. 1.0) ~previous:parent = 0L)

let test_live_time_policy () =
  let chain_id = "octra-devnet-9871-cluster" in
  let now = 1_700_000_000.0 in
  let previous = Some (T.of_seconds (now -. 10.0) |> Result.get_ok) in
  let candidate = now +. 86_400.0 in
  List.iter (fun epoch_id ->
    let rule = Policy.rule_for_epoch ~chain_id ~epoch_id in
    List.iter (fun kind ->
      expect_error "active devnet accepted day-ahead proposal"
        (T.check_proposal ~rule ~kind ~now ~previous ~candidate))
      [T.Fresh; T.Reproposal]) [1_320_000L; 1_523_218L];
  expect "historical replay rule preserved"
    (Result.is_ok (T.check_proposal ~rule:T.Historical ~kind:T.Reproposal
      ~now ~previous ~candidate))

let () =
  test_finite_recovery ();
  test_live_time_policy ();
  test_conversion ();
  test_check_accepts ();
  test_check_accepts_protocol_interval ();
  test_check_accepts_schedule_debt ();
  test_next_delay_before_boundary ();
  test_next_delay_at_boundary ();
  test_next_delay_after_boundary ();
  test_check_rejects_drift ();
  test_check_rejects_short_interval ();
  test_check_rejects_future_time ();
  test_check_rejects_backwards ();
  test_check_rejects_non_finite ();
  test_check_rejects_range ();
  test_reproposal_accepts_old_monotonic_time ();
  test_reproposal_rejects_short_interval ();
  test_reproposal_rejects_backwards_time ();
  test_historical_reproposal_preserves_prior_rule ();
  test_historical_fresh_proposal_rejects_future_time ();
  test_uniform_reproposal_rejects_future_time ();
  test_uniform_reproposal_accepts_observed_time ();
  test_uniform_early_reproposal_reduces_clock_drift ();
  test_policy_activation_boundary ()