(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module L = Octra_node_runtime.Consensus_tick_loop

let fail msg =
  failwith ("test_tick_loop: " ^ msg)

let expect label cond =
  if not cond then fail label

let header =
  Octra_consensus.C_types.{
    proto_version = proto_version_current;
    chain_id = "dev";
    epoch_id = 12L;
    prev_state_root = String.make 32 '\x00';
    tx_list_hash = String.make 32 '\x01';
    receipt_root = String.make 32 '\x02';
    proposed_state_root = String.make 32 '\x03';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "octA";
    txid_hi = 9L;
    ts = 1.;
  }

let finalize =
  Octra_consensus.C_types.{
    chain_id = "dev";
    epoch_id = 12L;
    commit_round = 1;
    header;
    proposal_id = "pid";
    precommits = [];
    parent_commit = None;
  }

let deps ?(now = 15.) ?(finalized = true) ?(cached = true) ?(empty = false)
    ?(found = true) ?(apply_ok = true) events =
  let push event =
    events := event :: !events
  in
  L.{
    now = (fun () -> now);
    last_epoch_time = (fun () -> 5.);
    current_epoch = (fun () -> 12);
    consensus_finalized = (fun () -> finalized);
    find_finalized = (fun _ -> if found then Some finalize else None);
    cached_bundle_for_pid = (fun _ -> cached);
    header_has_empty_bundle = (fun _ -> empty);
    store_empty_bundle_for_header = (fun h ->
      push ("store:" ^ Int64.to_string h.Octra_consensus.C_types.epoch_id));
    queue_missing_bundle = (fun ~target_epoch ~reason ->
      push
        (Printf.sprintf
           "queue:%Ld:%s"
           target_epoch
           reason));
    warn = (fun msg -> push ("warn:" ^ msg));
    info = (fun msg -> push ("info:" ^ msg));
    clear_finalized = (fun () -> push "clear");
    apply = (fun ~finalize:applied ~now ~elapsed ->
      expect "apply finalize identity" (applied = Some finalize);
      push (Printf.sprintf "apply:%.0f:%.0f" now elapsed);
      if apply_ok then Lwt.return_unit
      else Lwt.fail_with "apply failed");
    sleep = (fun delay ->
      push (Printf.sprintf "sleep:%.1f" delay);
      Lwt.return_unit);
  }

let test_cached_apply () =
  let events = ref [] in
  let next_sleep =
    Lwt_main.run
      (L.step
         (deps ~now:17. events)
         ~consensus_mode:true)
  in
  expect "cached sleep" (next_sleep = 0.1);
  expect "cached order"
    (List.rev !events = [
      "clear";
      "info:tick epoch = 12 elapsed = 12.00s next_in = 0.00s";
      "apply:17:12";
    ])

let test_cached_without_delay () =
  let events = ref [] in
  let next_sleep =
    Lwt_main.run
      (L.step
         (deps events)
         ~consensus_mode:true)
  in
  expect "cached poll interval" (next_sleep = 0.1);
  expect "certificate applies before local deadline"
    (List.rev !events = [
      "clear";
      "info:tick epoch = 12 elapsed = 10.00s next_in = 0.00s";
      "apply:15:10";
    ])

let test_failed_apply_keeps_journal () =
  let events = ref [] in
  let failed =
    Lwt_main.run
      (Lwt.catch
         (fun () ->
           L.step
             (deps ~now:17. ~apply_ok:false events)
             ~consensus_mode:true
           |> Lwt.map (fun _ -> false))
         (fun _ -> Lwt.return_true))
  in
  expect "failed apply" failed;
  expect "journal retained"
    (List.rev !events = [
      "clear";
      "info:tick epoch = 12 elapsed = 12.00s next_in = 0.00s";
      "apply:17:12";
    ])

let test_missing_bundle () =
  let events = ref [] in
  let next_sleep =
    Lwt_main.run
      (L.step
         (deps ~cached:false ~empty:false events)
         ~consensus_mode:true)
  in
  expect "missing sleep" (next_sleep = 0.1);
  expect "missing order"
    (List.rev !events = [
      "queue:12:finalized_bundle_missing_tick";
      "warn:BFT finalize trigger epoch = 12 deferred = canonical_bundle_missing";
      "clear";
    ])

let test_non_consensus_wait () =
  let events = ref [] in
  let next_sleep =
    Lwt_main.run
      (L.step
         (deps ~now:10. ~finalized:false events)
         ~consensus_mode:false)
  in
  expect "non consensus sleep" (next_sleep = 10.);
  expect "non consensus order"
    (List.rev !events = [
      "info:tick epoch = 12 elapsed = 5.00s next_in = 5.00s";
    ])

let test_node_refs () =
  let events = ref [] in
  let current_epoch = ref 12 in
  let last_epoch_time = ref 5. in
  let consensus_finalized = ref true in
  let state = Octra_node_runtime.Consensus_finality_state.create () in
  let callbacks = Octra_node_runtime.Consensus_finality_state.callbacks state in
  callbacks.store_finalized ~epoch:12 finalize;
  let cache = Octra_node_runtime.Consensus_bundle_cache.create ~cap:4 in
  let bundle = Octra_node_runtime.Consensus_bundle_cache.node_runtime cache in
  bundle.store_empty_proposal
    ~proposal_id:(Octra_consensus.C_hash.proposal_id header);
  let deps =
    L.node_deps
      {
        now = (fun () -> 17.);
        last_epoch_time;
        current_epoch;
        consensus_finalized;
        finality = callbacks;
        bundle;
        queue_missing_bundle = (fun ~target_epoch ~reason ->
          events := Printf.sprintf "queue:%Ld:%s" target_epoch reason :: !events);
        warn = (fun msg -> events := ("warn:" ^ msg) :: !events);
        info = (fun msg -> events := ("info:" ^ msg) :: !events);
        apply = (fun ~finalize:applied ~now ~elapsed ->
          expect "node apply finalize identity" (applied = Some finalize);
          events := Printf.sprintf "apply:%.0f:%.0f" now elapsed :: !events;
          Lwt.return_unit);
        sleep = (fun delay ->
          events := Printf.sprintf "sleep:%.1f" delay :: !events;
          Lwt.return_unit);
      }
  in
  let next_sleep =
    Lwt_main.run
      (L.step
         deps
         ~consensus_mode:true)
  in
  expect "node adapter sleep" (next_sleep = 0.1);
  expect "node adapter cleared" (not !consensus_finalized);
  expect "node adapter order"
    (List.rev !events = [
      "info:tick epoch = 12 elapsed = 12.00s next_in = 0.00s";
      "apply:17:12";
    ])

let () =
  test_cached_apply ();
  test_cached_without_delay ();
  test_failed_apply_keeps_journal ();
  test_missing_bundle ();
  test_non_consensus_wait ();
  test_node_refs ();
  print_endline "status = pass test = tick_loop"