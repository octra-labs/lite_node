(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Actor = Octra_node_runtime.Set_actor
module C_types = Octra_consensus.C_types

let expect label condition =
  if not condition then failwith ("set_actor: " ^ label)

let vote epoch =
  C_types.{
    chain_id = "actor-test";
    epoch_id = epoch;
    round = 0;
    vote_type = Precommit;
    proposal_id = String.make 32 '\x11';
    validator = "octA";
    signature = String.make 64 '\x22';
  }

let parent epoch =
  let validator = C_types.{ address = "octA"; pubkey = String.make 32 '\x33' } in
  let validator_set = C_types.make_validator_set [validator] in
  let header = C_types.{
    proto_version = proto_version_current;
    chain_id = "actor-test";
    epoch_id = epoch;
    prev_state_root = String.make 32 '\x44';
    tx_list_hash = String.make 32 '\x55';
    receipt_root = String.make 32 '\x66';
    proposed_state_root = String.make 32 '\x77';
    parent_commit_hash = Octra_net.Hash_domain.nil_hash;
    creator_addr = "octA";
    txid_hi = 0L;
    ts = 0.;
  } in
  C_types.{
    validator_set;
    certificate = {
      chain_id = "actor-test";
      epoch_id = epoch;
      commit_round = 0;
      header;
      proposal_id = String.make 32 '\x11';
      precommits = [];
    };
  }

let settle () = Lwt_unix.sleep 0.02

let unread ~epoch:_ = Lwt.return_ok Octra_core.Set_fold.{ marked = []; pulse = None }

let check_flow () =
  let sample = ref Actor.{ epoch = 100L; active = false; bonded = Ok true } in
  let sent = ref [] in
  let actor =
    Actor.create Actor.{
      sample = (fun () -> !sample);
      read = unread;
      peers = (fun () -> 1);
      send = (fun ~epoch action ->
        expect "send keeps sampled epoch" (epoch = !sample.epoch);
        sent := action :: !sent;
        Lwt.return_ok ());
      warn = (fun _ _ -> ());
    }
  in
  expect "first notice accepted"
    (Actor.notify actor ~epoch:100L None = Actor.Accepted);
  Lwt_main.run (settle ());
  expect "inactive validator emits pulse"
    (match !sent with Actor.Pulse :: _ -> true | _ -> false);
  let first_count = List.length !sent in
  ignore (Actor.notify actor ~epoch:101L None);
  Lwt_main.run (settle ());
  expect "pulse interval enforced" (List.length !sent = first_count);
  let event = vote 101L, parent 101L in
  sample := { !sample with epoch = 102L; active = true };
  ignore (Actor.notify actor ~epoch:102L (Some event));
  Lwt_main.run (settle ());
  expect "appeal waits two epochs" (List.length !sent = first_count);
  sample := { !sample with epoch = 103L; bonded = Ok false };
  ignore (Actor.notify actor ~epoch:103L None);
  Lwt_main.run (settle ());
  expect "unbonded validator emits nothing" (List.length !sent = first_count);
  sample := { !sample with epoch = 104L; bonded = Ok true };
  ignore (Actor.notify actor ~epoch:104L None);
  Lwt_main.run (settle ());
  expect "appeal emitted when ready"
    (match !sent with Actor.Appeal _ :: _ -> true | _ -> false);
  let stats = Lwt_main.run (Actor.stats actor) in
  expect "actor records sends" (Int64.compare stats.Actor.sent 2L = 0);
  Lwt_main.run (Actor.shutdown actor);
  expect "stopped actor refuses notice"
    (Actor.notify actor ~epoch:105L None = Actor.Stopped)

let check_overload () =
  let wait, wake = Lwt.wait () in
  let events = ref [] in
  let actor =
    Actor.create ~observe:(fun entry -> events := entry :: !events) Actor.{
      sample = (fun () -> { epoch = 1L; active = false; bonded = Ok true });
      read = unread;
      peers = (fun () -> 1);
      send = (fun ~epoch:_ _ -> wait);
      warn = (fun _ _ -> ());
    }
  in
  ignore (Actor.notify actor ~epoch:1L None);
  Lwt_main.run (Lwt_unix.sleep 0.01);
  for index = 1 to Actor.stream_capacity do
    expect "stream accepts finite capacity"
      (Actor.notify actor ~epoch:(Int64.of_int (index + 1))
         (Some (vote (Int64.of_int index), parent (Int64.of_int index)))
       = Actor.Accepted)
  done;
  events := [];
  expect "stream reports overload"
    (Actor.notify actor ~epoch:99L (Some (vote 98L, parent 98L)) = Actor.Busy);
  expect "overload preserves available proof evidence"
    (List.map (fun (entry : Actor.observation) -> entry.stage) !events
     = [Actor.Overload; Actor.Available]);
  Lwt.wakeup_later wake (Ok ());
  Lwt_main.run (Lwt_unix.sleep 0.02);
  Lwt_main.run (Actor.shutdown actor)

let check_wake_merge () =
  let hold, release = Lwt.wait () in
  let reads = ref 0 in
  let sends = ref 0 in
  let actor = Actor.create Actor.{
    sample = (fun () -> { epoch = 100L; active = false; bonded = Ok true });
    read = (fun ~epoch:_ ->
      incr reads;
      if !reads = 1 then hold else unread ~epoch:100L);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sends; Lwt.return_ok ());
    warn = (fun _ _ -> ());
  } in
  ignore (Actor.wake actor ~head:99);
  Lwt_main.run (Lwt.pause ());
  for head = 99 to 99 + Actor.stream_capacity - 1 do
    expect "repeated wake accepted" (Actor.wake actor ~head = Actor.Accepted)
  done;
  Lwt.wakeup release (Error "head not committed");
  Lwt_main.run (settle ());
  expect "one queued retry after read refusal" (!reads = 2 && !sends = 1);
  Lwt_main.run (Actor.shutdown actor)

let check_effect_failure () =
  let fail = ref true in
  let sent = ref 0 in
  let warnings = ref 0 in
  let sample = ref Actor.{ epoch = 1L; active = false; bonded = Ok true } in
  let actor =
    Actor.create Actor.{
      sample = (fun () -> !sample);
      read = unread;
      peers = (fun () -> 1);
      send = (fun ~epoch:_ _ ->
        if !fail then Lwt.fail (Failure "planned send failure")
        else begin
          incr sent;
          Lwt.return_ok ()
        end);
      warn = (fun _ _ -> incr warnings);
    }
  in
  ignore (Actor.notify actor ~epoch:1L None);
  Lwt_main.run (settle ());
  expect "effect failure is visible" (!warnings = 1);
  fail := false;
  sample := { !sample with epoch = 2L };
  ignore (Actor.notify actor ~epoch:2L None);
  Lwt_main.run (settle ());
  expect "actor survives effect failure" (!sent = 1);
  Lwt_main.run (Actor.shutdown actor)

let check_phase () =
  let cases = [
    100L, 100L, Some 99L, false, None;
    100L, 100L, Some 99L, true, Some Actor.Finalized;
    100L, 101L, Some 100L, false, Some Actor.Moved;
    100L, 99L, Some 98L, false, Some Actor.Moved;
    100L, 100L, Some 98L, false, Some Actor.Uncommitted;
    100L, 100L, Some 100L, false, Some Actor.Uncommitted;
    100L, 100L, None, false, Some Actor.Uncommitted;
    0L, 0L, Some (-1L), false, Some Actor.Uncommitted;
    Int64.min_int, Int64.min_int, Some Int64.max_int, false, Some Actor.Uncommitted;
    Int64.max_int, Int64.max_int, Some (Int64.pred Int64.max_int), false, None;
  ] in
  List.iter (fun (epoch, current, head, finalized, expected) ->
    let point = Actor.{ epoch = current; head; finalized } in
    expect "send phase" (match Actor.plan ~epoch point, expected with
      | Ok value, None -> Some value = head
      | Error reason, Some error -> reason = error
      | _ -> false)) cases

let check_send_resume () =
  List.iter (fun appeal ->
    let point = ref Actor.{
      epoch = 100L;
      head = Some 99L;
      finalized = true;
    } in
    let sent = ref [] in
    let warnings = ref [] in
    let actor = Actor.create Actor.{
      sample = (fun () -> { epoch = !point.epoch; active = appeal; bonded = Ok true });
      read = unread;
      peers = (fun () -> 1);
      send = (fun ~epoch action ->
        match Actor.plan ~epoch !point with
        | Error reason -> Lwt.return_error (Actor.Send, Actor.reason reason)
        | Ok head -> sent := (action, head) :: !sent; Lwt.return_ok ());
      warn = (fun _ reason -> warnings := reason :: !warnings);
    } in
    let event = if appeal then Some (vote 97L, parent 97L) else None in
    ignore (Actor.notify actor ~epoch:100L event);
    Lwt_main.run (settle ());
    let stats = Lwt_main.run (Actor.stats actor) in
    expect "closed phase retains action"
      (!sent = [] && stats.sent = 0L && stats.appeals = if appeal then 1 else 0);
    expect "closed phase reports reason"
      (!warnings = ["validator duty epoch is already finalized"]);
    point := Actor.{ epoch = 101L; head = Some 100L; finalized = false };
    ignore (Actor.wake actor ~head:100);
    Lwt_main.run (settle ());
    let stats = Lwt_main.run (Actor.stats actor) in
    expect "committed head resumes action"
      (stats.sent = 1L && stats.appeals = if appeal then 1 else 0);
    expect "resumed action is preserved" (match appeal, !sent with
      | true, [Actor.Appeal proof, head] -> proof.vote.epoch_id = 97L && head = 100L
      | false, [Actor.Pulse, head] -> head = 100L
      | _ -> false);
    Lwt_main.run (Actor.shutdown actor)) [false; true]

let check_ack () =
  let sample = ref Actor.{ epoch = 102L; active = true; bonded = Ok true } in
  let receipt = ref Octra_core.Set_fold.{ marked = []; pulse = None } in
  let sent = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok !receipt);
    peers = (fun () -> 1);
    send = (fun ~epoch action -> sent := (epoch, action) :: !sent; Lwt.return_ok ());
    warn = (fun _ reason -> failwith reason);
  } in
  let notify epoch event =
    sample := { !sample with epoch };
    ignore (Actor.notify actor ~epoch event);
    Lwt_main.run (settle ())
  in
  notify 102L (Some (vote 100L, parent 100L));
  expect "local admission keeps proof" ((Lwt_main.run (Actor.stats actor)).appeals = 1);
  notify 102L (Some (vote 101L, parent 101L));
  expect "one submission per epoch" (List.length !sent = 1);
  notify 103L None;
  expect "unconfirmed proof retried first"
    (match !sent with (103L, Actor.Appeal proof) :: _ -> proof.vote.epoch_id = 100L | _ -> false);
  receipt := { !receipt with marked = [100L] };
  notify 104L None;
  expect "confirmed proof advances queue"
    (match !sent with (104L, Actor.Appeal proof) :: _ -> proof.vote.epoch_id = 101L | _ -> false);
  expect "only unconfirmed proof retained" ((Lwt_main.run (Actor.stats actor)).appeals = 1);
  receipt := { !receipt with marked = [100L; 101L] };
  notify 105L None;
  expect "committed marks acknowledge proofs" ((Lwt_main.run (Actor.stats actor)).appeals = 0);
  notify 106L (Some (vote 100L, parent 100L));
  expect "duplicate confirmed proof not resent" (List.length !sent = 3);
  notify 117L (Some (vote 100L, parent 100L));
  expect "expired proof not retried" ((Lwt_main.run (Actor.stats actor)).appeals = 0);
  Lwt_main.run (Actor.shutdown actor)

let check_proof_trace () =
  let sample = ref Actor.{ epoch = 102L; active = true; bonded = Ok true } in
  let receipt = ref Octra_core.Set_fold.{ marked = []; pulse = None } in
  let events = ref [] in
  let sends = ref 0 in
  let actor = Actor.create ~observe:(fun entry -> events := entry :: !events) Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok !receipt);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sends; Lwt.return_ok ());
    warn = (fun _ reason -> failwith reason);
  } in
  let notify epoch proof =
    sample := { !sample with epoch };
    ignore (Actor.notify actor ~epoch proof);
    Lwt_main.run (settle ())
  in
  let count stage epoch =
    List.filter (fun (entry : Actor.observation) ->
      entry.stage = stage && entry.proof.vote.epoch_id = epoch) !events
    |> List.length
  in
  notify 102L (Some (vote 100L, parent 100L));
  notify 102L (Some (vote 100L, parent 100L));
  expect "availability repeats share one proof identity"
    (List.map (fun (entry : Actor.observation) -> Actor.proof_key entry.proof) !events
     |> List.sort_uniq String.compare |> List.length = 1);
  expect "send success is not a committed mark"
    (count Actor.Available 100L = 2 && count Actor.Attempt 100L = 1
     && count Actor.Marked 100L = 0);
  receipt := { !receipt with marked = [100L] };
  notify 103L None;
  expect "committed receipt acknowledges retained proof" (count Actor.Marked 100L = 1);
  expect "certificate acknowledgment does not require another send" (!sends = 1);
  notify 103L (Some (vote 101L, parent 101L));
  notify 118L None;
  expect "expired without observed mark is not success"
    (count Actor.Expired 101L = 1 && count Actor.Marked 101L = 0);
  Lwt_main.run (Actor.shutdown actor);
  expect "closed actor refuses proof"
    (Actor.notify actor ~epoch:119L (Some (vote 118L, parent 118L)) = Actor.Stopped);
  expect "closed refusal retains proof identity"
    (count Actor.Available 118L = 1 && count Actor.Closed 118L = 1)

let check_last_receipt result =
  let sample = ref Actor.{epoch = 116L; active = true; bonded = Ok true} in
  let reply = ref (Ok Octra_core.Set_fold.{marked = []; pulse = None}) in
  let events = ref [] in
  let sends = ref 0 in
  let move = ref false in
  let raises = ref false in
  let actor = Actor.create ~observe:(fun event -> events := event :: !events) Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch ->
      if !move then sample := {!sample with epoch = Int64.succ epoch};
      if !raises then Lwt.fail (Failure "read unavailable") else Lwt.return !reply);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sends; Lwt.return_ok ());
    warn = (fun fault _ -> expect "receipt error classified" (fault = Actor.Receipt));
  } in
  let notify epoch proof =
    sample := {!sample with epoch};
    ignore (Actor.notify actor ~epoch proof);
    Lwt_main.run (settle ()) in
  let count stage = List.filter (fun (event : Actor.observation) ->
    event.stage = stage && event.proof.vote.epoch_id = 100L) !events |> List.length in
  notify 116L (Some (vote 100L, parent 100L));
  expect "appeal sent in last valid epoch" (!sends = 1);
  reply := if result = `Unread then Error "head not committed"
    else Ok {marked = [100L]; pulse = None};
  move := result = `Moved;
  raises := result = `Raised;
  sample := {!sample with bonded = Ok false};
  notify 117L None;
  expect "final window receipt is credited when observed"
    (count Actor.Marked = if result = `Marked || result = `Moved then 1 else 0);
  expect "only unavailable reads remain unknown"
    (count Actor.Unread = if result = `Marked || result = `Moved then 0 else 1);
  expect "unavailable receipt is not a missed mark" (count Actor.Expired = 0);
  expect "expired proof never resent" (!sends = 1);
  expect "expiry clears queue even without receipt"
    ((Lwt_main.run (Actor.stats actor)).appeals = 0);
  Lwt_main.run (Actor.shutdown actor)

let check_read_rollover (first, next, result, expected) =
  let start = Int64.min first 116L in
  let sample = ref Actor.{epoch = start; active = true; bonded = Ok true} in
  let reading = ref false in
  let reply = ref result in
  let events = ref [] in
  let sends = ref 0 in
  let actor = Actor.create ~observe:(fun event -> events := event :: !events) Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ ->
      if not !reading then unread ~epoch:start
      else begin
        sample := {!sample with epoch = Int64.max (!sample).epoch next};
        match !reply with
        | `Error -> Lwt.return_error "read unavailable"
        | `Raised -> Lwt.fail (Failure "read unavailable")
        | `Marked | `Empty -> Lwt.return_ok Octra_core.Set_fold.{
            marked = if !reply = `Marked then [100L] else []; pulse = None }
      end);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sends; Lwt.return_ok ());
    warn = (fun fault _ -> expect "read error classified" (fault = Actor.Receipt));
  } in
  ignore (Actor.notify actor ~epoch:start (Some (vote 100L, parent 100L)));
  Lwt_main.run (settle ());
  expect "live proof queued before read" ((Lwt_main.run (Actor.stats actor)).appeals = 1);
  reading := true;
  sample := {!sample with epoch = first};
  ignore (Actor.notify actor ~epoch:first None);
  Lwt_main.run (settle ());
  let outcomes () = List.filter_map (fun (event : Actor.observation) ->
    match event.stage with
    | Actor.Marked | Actor.Expired | Actor.Unread | Actor.Capacity -> Some event.stage
    | _ -> None) !events in
  expect "post-read receipt clears the queue" ((Lwt_main.run (Actor.stats actor)).appeals = 0);
  expect "receipt epoch determines the expiry outcome" (outcomes () = [expected]);
  expect "epoch rollover cannot send another duty" (!sends = 1);
  reply := `Error;
  sample := {!sample with epoch = Int64.max next 117L};
  ignore (Actor.notify actor ~epoch:(!sample).epoch None);
  Lwt_main.run (settle ());
  expect "later read failure cannot change the outcome" (outcomes () = [expected]);
  expect "settled proof cannot be resent" (!sends = 1);
  Lwt_main.run (Actor.shutdown actor)

let check_expiry_queue () =
  let epoch = ref 116L in
  let fail = ref true in
  let events = ref [] in
  let sent = ref [] in
  let actor = Actor.create ~observe:(fun event -> events := event :: !events) Actor.{
    sample = (fun () -> {epoch = !epoch; active = true; bonded = Ok true});
    read = (fun ~epoch:_ -> if !fail then Lwt.return_error "read unavailable"
      else Lwt.return_ok Octra_core.Set_fold.{marked = []; pulse = None});
    peers = (fun () -> 1);
    send = (fun ~epoch:_ action -> sent := action :: !sent; Lwt.return_ok ());
    warn = (fun fault _ -> expect "receipt failure classified" (fault = Actor.Receipt));
  } in
  let notify proof =
    ignore (Actor.notify actor ~epoch:!epoch proof);
    Lwt_main.run (settle ()) in
  for index = 0 to Actor.appeal_capacity - 1 do
    let height = Int64.of_int (100 + index / 2) in
    notify (Some ({(vote height) with round = index mod 2}, parent height))
  done;
  notify (Some (vote 116L, parent 116L));
  let lost = List.filter (fun (event : Actor.observation) ->
    event.stage = Actor.Capacity) !events in
  expect "shortest remaining lifetime evicted"
    (List.length lost = 1 && (List.hd lost).proof.vote.epoch_id = 100L);
  for round = 0 to Actor.appeal_capacity do
    notify (Some ({(vote 90L) with round}, parent 90L))
  done;
  expect "expired arrivals cannot displace live appeals"
    (List.filter (fun (event : Actor.observation) -> event.stage = Actor.Capacity)
      !events |> List.length = 1);
  expect "live queue remains full" ((Lwt_main.run (Actor.stats actor)).appeals = Actor.appeal_capacity);
  epoch := 132L;
  notify None;
  expect "failed read removes expired queue entries"
    ((Lwt_main.run (Actor.stats actor)).appeals = 1);
  fail := false;
  notify None;
  expect "newest proof survives overload and read failure"
    (match !sent with [Actor.Appeal proof] -> proof.vote.epoch_id = 116L | _ -> false);
  epoch := 133L;
  fail := true;
  notify None;
  expect "last retained proof expires on failed read"
    ((Lwt_main.run (Actor.stats actor)).appeals = 0);
  Lwt_main.run (Actor.shutdown actor)

let check_last_absence () =
  List.iter (fun marked ->
    let events = ref [] in
    let actor = Actor.create ~observe:(fun event -> events := event :: !events) Actor.{
      sample = (fun () -> {epoch = 117L; active = true; bonded = Ok true});
      read = (fun ~epoch:_ -> Lwt.return_ok Octra_core.Set_fold.{marked; pulse = None});
      peers = (fun () -> 1);
      send = (fun ~epoch:_ _ -> failwith "expired proof sent");
      warn = (fun _ reason -> failwith reason);
    } in
    ignore (Actor.notify actor ~epoch:117L (Some (vote 100L, parent 100L)));
    Lwt_main.run (settle ());
    expect "receipt settles late proof before expiration"
      (List.rev_map (fun (event : Actor.observation) -> event.stage) !events
       = [Actor.Available; if marked = [] then Actor.Expired else Actor.Marked]);
    expect "settled proof removed" ((Lwt_main.run (Actor.stats actor)).appeals = 0);
    Lwt_main.run (Actor.shutdown actor)
  ) [[]; [100L]]

let check_trace_failure () =
  let warnings = ref 0 in
  let sent = ref 0 in
  let actor = Actor.create ~observe:(fun _ -> failwith "trace unavailable") Actor.{
    sample = (fun () -> {epoch = 102L; active = true; bonded = Ok true});
    read = unread;
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sent; Lwt.return_ok ());
    warn = (fun fault _ ->
      expect "trace failure classified" (fault = Actor.Internal);
      incr warnings);
  } in
  expect "trace failure does not refuse notice"
    (Actor.notify actor ~epoch:102L (Some (vote 100L, parent 100L)) = Actor.Accepted);
  Lwt_main.run (settle ());
  expect "trace failure preserves delivery" (!sent = 1 && !warnings = 2);
  Lwt_main.run (Actor.shutdown actor)

let check_trace_capacity () =
  let events = ref [] in
  let epoch = ref 102L in
  let actor = Actor.create ~observe:(fun entry -> events := entry :: !events) Actor.{
    sample = (fun () -> {epoch = !epoch; active = false; bonded = Ok false});
    read = unread;
    peers = (fun () -> 0);
    send = (fun ~epoch:_ _ -> failwith "unexpected inactive send");
    warn = (fun _ reason -> failwith reason);
  } in
  for round = 0 to Actor.appeal_capacity do
    let vote = { (vote 100L) with round } in
    ignore (Actor.notify actor ~epoch:102L (Some (vote, parent 100L)));
    Lwt_main.run (settle ())
  done;
  let count stage =
    List.filter (fun (entry : Actor.observation) -> entry.stage = stage) !events
    |> List.length
  in
  expect "capacity removal is observable"
    (count Actor.Capacity = 1 && count Actor.Available = Actor.appeal_capacity + 1);
  expect "capacity trace does not change retention"
    ((Lwt_main.run (Actor.stats actor)).appeals = Actor.appeal_capacity);
  epoch := 117L;
  ignore (Actor.notify actor ~epoch:117L None);
  Lwt_main.run (settle ());
  expect "expiry accounts for remaining retained proofs"
    (count Actor.Expired = Actor.appeal_capacity && count Actor.Marked = 0);
  Lwt_main.run (Actor.shutdown actor)

let check_pulse_ack () =
  let sample = ref Actor.{ epoch = 100L; active = false; bonded = Ok true } in
  let receipt = ref Octra_core.Set_fold.{ marked = []; pulse = None } in
  let sent = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok !receipt);
    peers = (fun () -> 1);
    send = (fun ~epoch action -> sent := (epoch, action) :: !sent; Lwt.return_ok ());
    warn = (fun _ reason -> failwith reason);
  } in
  let tick epoch =
    sample := { !sample with epoch };
    ignore (Actor.wake actor ~head:(Int64.to_int epoch - 1));
    Lwt_main.run (settle ())
  in
  tick 100L;
  tick 100L;
  tick 101L;
  expect "unconfirmed pulse retried once per epoch" (List.length !sent = 2);
  receipt := { !receipt with pulse = Some 101L };
  tick 102L;
  tick 104L;
  expect "confirmed pulse starts interval" (List.length !sent = 2);
  tick 105L;
  expect "next pulse due from committed epoch" (List.length !sent = 3);
  Lwt_main.run (Actor.shutdown actor)

let check_shadow_appeal () =
  let sample = ref Actor.{ epoch = 102L; active = false; bonded = Ok true } in
  let receipt = ref Octra_core.Set_fold.{ marked = []; pulse = Some 100L } in
  let sent = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok !receipt);
    peers = (fun () -> 1);
    send = (fun ~epoch action -> sent := (epoch, action) :: !sent; Lwt.return_ok ());
    warn = (fun _ reason -> failwith reason);
  } in
  let tick epoch proof =
    sample := { !sample with epoch };
    ignore (Actor.notify actor ~epoch proof);
    Lwt_main.run (settle ())
  in
  tick 102L (Some (vote 100L, parent 100L));
  tick 103L None;
  expect "shadow does not spend pulse interval on appeals" (!sent = []);
  expect "shadow keeps unexpired proof"
    ((Lwt_main.run (Actor.stats actor)).appeals = 1);
  tick 104L None;
  expect "shadow emits only due pulse" (!sent = [104L, Actor.Pulse]);
  receipt := { !receipt with pulse = Some 104L };
  sample := { !sample with active = true };
  tick 105L None;
  expect "admission resumes retained appeal"
    (match !sent with
     | (105L, Actor.Appeal proof) :: [104L, Actor.Pulse] ->
       proof.vote.epoch_id = 100L
     | _ -> false);
  receipt := { !receipt with marked = [100L] };
  tick 106L None;
  expect "admitted proof acknowledged"
    (List.length !sent = 2 && (Lwt_main.run (Actor.stats actor)).appeals = 0);
  sample := { !sample with active = false };
  tick 107L (Some (vote 103L, parent 103L));
  expect "second exclusion stops appeal traffic" (List.length !sent = 2);
  tick 108L None;
  expect "second exclusion resumes pulse schedule"
    (match !sent with (108L, Actor.Pulse) :: _ -> true | _ -> false);
  receipt := { !receipt with pulse = Some 119L };
  tick 120L None;
  expect "shadow prunes expired proof"
    (List.length !sent = 3 && (Lwt_main.run (Actor.stats actor)).appeals = 0);
  sample := { !sample with active = true };
  tick 121L None;
  expect "readmission cannot resend expired proof" (List.length !sent = 3);
  Lwt_main.run (Actor.shutdown actor)

let check_membership_read () =
  List.iter (fun (before, active, bonded, expected) ->
    let sample = ref Actor.{ epoch = 102L; active = before; bonded = Ok true } in
    let sent = ref [] in
    let actor = Actor.create Actor.{
      sample = (fun () -> !sample);
      read = (fun ~epoch:_ ->
        sample := { !sample with active; bonded = Ok bonded };
        Lwt.return_ok Octra_core.Set_fold.{ marked = []; pulse = Some 100L });
      peers = (fun () -> 1);
      send = (fun ~epoch:_ action -> sent := action :: !sent; Lwt.return_ok ());
      warn = (fun _ reason -> failwith reason);
    } in
    ignore (Actor.notify actor ~epoch:102L (Some (vote 100L, parent 100L)));
    Lwt_main.run (settle ());
    expect "membership after read selects action"
      (match !sent with
       | [Actor.Appeal proof] -> expected && proof.vote.epoch_id = 100L
       | [] -> not expected
       | _ -> false);
    expect "membership change preserves evidence"
      ((Lwt_main.run (Actor.stats actor)).appeals = 1);
    Lwt_main.run (Actor.shutdown actor))
    [true, false, true, false;
     false, true, true, true;
     true, true, false, false]

let check_read () =
  let sample = ref Actor.{ epoch = 102L; active = true; bonded = Ok true } in
  let mode = ref 0 in
  let sent = ref 0 in
  let warnings = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ ->
      if !mode = 0 then Lwt.return_error "committed read unavailable"
      else if !mode = 1 then begin
        sample := { !sample with epoch = 103L };
        Lwt.return_ok Octra_core.Set_fold.{ marked = []; pulse = None }
      end else unread ~epoch:!sample.epoch);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sent; Lwt.return_ok ());
    warn = (fun _ reason -> warnings := reason :: !warnings);
  } in
  let tick () =
    ignore (Actor.notify actor ~epoch:!sample.epoch (Some (vote 100L, parent 100L)));
    Lwt_main.run (settle ())
  in
  tick ();
  expect "read failure retains proof"
    (!sent = 0 && (Lwt_main.run (Actor.stats actor)).appeals = 1);
  expect "read failure reported" (!warnings = ["committed read unavailable"]);
  mode := 1;
  tick ();
  expect "changed head retains unmarked proof"
    (!sent = 0 && (Lwt_main.run (Actor.stats actor)).appeals = 1);
  mode := 2;
  tick ();
  expect "fresh read resumes proof" (!sent = 1);
  sample := { !sample with epoch = 117L };
  tick ();
  expect "challenge expires after last valid epoch"
    (!sent = 1 && (Lwt_main.run (Actor.stats actor)).appeals = 0);
  Lwt_main.run (Actor.shutdown actor)

let check_post_head () =
  let module Post = Octra_node_runtime.Set_post in
  let module Tx = Octra_core.Transaction in
  let clock = ref 0. in
  let live = ref ["a"; "b"; "c"] in
  let landed = ref [] in
  let calls = ref [] in
  let replies = ref [] in
  let timers = ref [] in
  let warnings = ref [] in
  let tx name = Tx.{
    from = "sender"; to_ = "sender"; amount = Z.zero; nonce = 1;
    ou = Z.of_int 1_000; timestamp = 0.; signature = name;
    public_key = None; message = None; op_type = ValidatorReady;
    encrypted_data = None;
  } in
  let post = Post.create Post.{
    now = (fun () -> !clock);
    wait = (fun delay ->
      let promise, reply = Lwt.wait () in
      timers := (delay, reply) :: !timers;
      promise);
    staged = (fun hash -> List.mem hash !live);
    landed = (fun tx -> List.mem tx.Tx.signature !landed);
    post = (fun tx ->
      let promise, reply = Lwt.wait () in
      calls := tx.Tx.signature :: !calls;
      replies := (tx.signature, reply) :: !replies;
      promise);
    warn = (fun reason -> warnings := reason :: !warnings);
  } in
  let reply name result =
    Lwt.wakeup (List.assoc name !replies) result;
    Lwt_main.run (Lwt.pause ())
  in
  Post.put post ~hash:"a" (tx "a");
  reply "a" (Error (Post.Retry "unavailable"));
  expect "failed post arms one timer" (List.length !timers = 1);
  clock := 1.;
  Post.put post ~hash:"b" (tx "b");
  Post.put post ~hash:"c" (tx "c");
  expect "posts never overlap" (!calls = ["b"; "a"]);
  landed := ["b"];
  reply "b" (Error (Post.Refused "old reply"));
  expect "old reply cannot clear next head" (!calls = ["c"; "b"; "a"]);
  expect "old reply not reported for next head" (!warnings = ["unavailable"]);
  live := [];
  reply "c" (Error (Post.Retry "expired reply"));
  Post.tick post;
  expect "expired post not retried" (!calls = ["c"; "b"; "a"]);
  expect "expired post does not arm new timer" (List.length !timers = 1);
  Post.stop post;
  List.iter (fun (_, reply) -> Lwt.wakeup reply ()) !timers;
  Lwt_main.run (Lwt.pause ());
  expect "closed post timer is inert" (!calls = ["c"; "b"; "a"])

let check_post_retry () =
  let module Post = Octra_node_runtime.Set_post in
  let module Tx = Octra_core.Transaction in
  let clock = ref 0. in
  let live = ref true in
  let calls = ref [] in
  let timers = ref [] in
  let tx = Tx.{
    from = "sender"; to_ = "sender"; amount = Z.zero; nonce = 7;
    ou = Z.of_int 1_000; timestamp = 0.; signature = "signed";
    public_key = None; message = None; op_type = ValidatorReady;
    encrypted_data = None;
  } in
  let post = Post.create Post.{
    now = (fun () -> !clock);
    wait = (fun delay ->
      let promise, reply = Lwt.wait () in
      timers := (!clock +. delay, reply) :: !timers;
      promise);
    staged = (fun _ -> !live);
    landed = (fun _ -> false);
    post = (fun value ->
      calls := value :: !calls;
      Lwt.return_error (Post.Retry "transport unavailable"));
    warn = (fun _ -> ());
  } in
  let flush () = Lwt_main.run (Lwt.pause ()) in
  let fire at =
    match List.sort (fun (a, _) (b, _) -> Float.compare a b) !timers with
    | [] -> failwith "set_actor: retry timer missing"
    | (due, reply) :: rest ->
      timers := rest;
      expect "retry deadline" (Float.equal due at);
      clock := due;
      Lwt.wakeup reply ();
      flush ()
  in
  Post.put post ~hash:"same" tx;
  flush ();
  fire 1.;
  expect "transient refusal retries same transaction"
    (List.length !calls = 2 && List.for_all (( = ) tx) !calls);
  fire 3.;
  expect "two quick retries keep signed bytes"
    (List.length !calls = 3 && List.for_all (( = ) tx) !calls);
  clock := 9.;
  Post.tick post;
  Post.put post ~hash:"same" tx;
  flush ();
  expect "same item cannot reset retry pace" (List.length !calls = 3);
  fire 33.;
  expect "long round keeps retrying" (List.length !calls = 4);
  Post.put post ~hash:"next" { tx with signature = "next" };
  flush ();
  expect "next head starts without old delay" (List.length !calls = 5);
  fire 34.;
  fire 36.;
  expect "new head has its own quick retries" (List.length !calls = 7);
  fire 63.;
  expect "old timer cannot send next item" (List.length !calls = 7);
  fire 66.;
  expect "old timer cannot clear next timer" (List.length !calls = 8);
  live := false;
  fire 96.;
  expect "expired item cannot retry" (List.length !calls = 8 && !timers = []);
  Post.stop post

let check_post_cancel () =
  let module Post = Octra_node_runtime.Set_post in
  let module Tx = Octra_core.Transaction in
  let clock = ref 0. in
  let landed = ref false in
  let calls = ref [] in
  let timers = ref [] in
  let warnings = ref [] in
  let tx name = Tx.{
    from = "sender"; to_ = "sender"; amount = Z.zero; nonce = 7;
    ou = Z.of_int 1_000; timestamp = 0.; signature = name;
    public_key = None; message = None; op_type = ValidatorReady;
    encrypted_data = None;
  } in
  let post = Post.create Post.{
    now = (fun () -> !clock);
    wait = (fun _ ->
      let promise, _ = Lwt.task () in
      timers := promise :: !timers;
      promise);
    staged = (fun _ -> true);
    landed = (fun _ -> !landed);
    post = (fun value ->
      calls := value.Tx.signature :: !calls;
      match value.signature with
      | "c" -> failwith "transport exception"
      | "d" -> Lwt.return_ok ()
      | _ -> Lwt.return_error (Post.Retry "transport unavailable"));
    warn = (fun value -> warnings := value :: !warnings);
  } in
  let flush () = Lwt_main.run (Lwt.pause ()) in
  let put name =
    Post.put post ~hash:name (tx name);
    flush ()
  in
  let cancelled promise =
    match Lwt.state promise with
    | Lwt.Fail Lwt.Canceled -> true
    | _ -> false
  in
  put "a";
  let first = List.hd !timers in
  clock := 0.5;
  Post.tick post;
  expect "tick keeps one timer" (List.length !timers = 1);
  put "b";
  expect "replacement cancels owned wait" (cancelled first);
  landed := true;
  Post.tick post;
  flush ();
  expect "confirmed item cancels retry" (List.for_all cancelled !timers);
  landed := false;
  put "c";
  expect "post exception leaves retry available" (Lwt.is_sleeping (List.hd !timers));
  put "d";
  expect "accepted item has no retry wait" (List.for_all cancelled !timers);
  clock := 40.;
  Post.tick post;
  expect "accepted item not posted again" (!calls = ["d"; "c"; "b"; "a"]);
  put "e";
  Post.stop post;
  put "f";
  expect "stop cancels wait and prevents posting"
    (List.for_all cancelled !timers && !calls = ["e"; "d"; "c"; "b"; "a"]);
  expect "cancellation is not a transport failure" (List.length !warnings = 4)

let check_post_failure () =
  let module Post = Octra_node_runtime.Set_post in
  let retry = function Post.Retry _ -> true | _ -> false in
  let wait = function Post.Wait _ -> true | _ -> false in
  let refused = function Post.Refused _ -> true | _ -> false in
  let rpc code data = Post.rpc_failure (`Assoc ["code", `Int code; "data", `String data]) in
  List.iter (fun code -> expect "transient HTTP retry" (retry (Post.http_failure code)))
    [408; 429; 500; 502; 503; 504];
  List.iter (fun code -> expect "permanent HTTP refusal" (refused (Post.http_failure code)))
    [301; 400; 401; 403; 404];
  List.iter (fun code -> expect "RPC capacity retry" (retry (rpc code "")))
    [104; 107; 110; 113; -32005];
  List.iter (fun data -> expect "RPC verification retry" (retry (rpc 105 data)))
    ["pre_verify_busy"; "pre_verify_busy reason = queue full";
     "pre_verify_unavailable reason = worker offline"];
  expect "unknown verification error is not retried"
    (refused (rpc 105 "pre_verify_unavailable_bad_proof"));
  List.iter (fun code -> expect "RPC queue conflict waits" (wait (rpc code ""))) [100; 103; 106];
  expect "previous RPC duplicate waits" (wait (rpc 105 "duplicate nonce (fee rate bump < 10%)"));
  let expired = rpc 105 "validator ready head has expired" in
  expect "expired head is not retried" (refused expired);
  expect "unknown RPC refusal is not retried" (refused (rpc 999 "unknown"));
  let clock = ref 0. in
  let calls = ref 0 in
  let timers = ref [] in
  let failure = ref expired in
  let tx = Octra_core.Transaction.{
    from = "sender"; to_ = "sender"; amount = Z.zero; nonce = 7;
    ou = Z.of_int 1_000; timestamp = 0.; signature = "signed";
    public_key = None; message = None; op_type = ValidatorReady;
    encrypted_data = None;
  } in
  let post = Post.create Post.{
    now = (fun () -> !clock);
    wait = (fun delay ->
      let promise, reply = Lwt.wait () in
      timers := (delay, reply) :: !timers;
      promise);
    staged = (fun _ -> true);
    landed = (fun _ -> false);
    post = (fun _ -> incr calls; Lwt.return_error !failure);
    warn = (fun _ -> ());
  } in
  let flush () = Lwt_main.run (Lwt.pause ()) in
  Post.put post ~hash:"expired" tx;
  flush ();
  Post.tick post;
  expect "terminal reply stops RPC posting" (!calls = 1 && !timers = []);
  failure := rpc 106 "";
  Post.put post ~hash:"next" tx;
  flush ();
  let delay, reply = List.hd !timers in
  expect "duplicate does not get quick retries" (Float.equal delay 30.);
  timers := [];
  clock := delay;
  failure := expired;
  Lwt.wakeup reply ();
  flush ();
  expect "queue retry can stop on expiry" (!calls = 3 && !timers = []);
  Post.stop post

let check_post_recovery () =
  let module Post = Octra_node_runtime.Set_post in
  let failures = [
    104, "insufficient balance";
    113, "fee too low";
    105, "pre_verify_unavailable reason = worker offline";
  ] in
  List.iter (fun (code, data) ->
    let clock = ref 0. in
    let timer = ref None in
    let sent = ref [] in
    let tx = Octra_core.Transaction.{
      from = "sender"; to_ = "sender"; amount = Z.zero; nonce = 7;
      ou = Z.of_int 1_000; timestamp = 0.; signature = "signed";
      public_key = None; message = None; op_type = ValidatorReady;
      encrypted_data = None;
    } in
    let post = Post.create Post.{
      now = (fun () -> !clock);
      wait = (fun delay ->
        let promise, reply = Lwt.task () in
        timer := Some (delay, reply);
        promise);
      staged = (fun _ -> true);
      landed = (fun _ -> false);
      post = (fun value ->
        sent := value :: !sent;
        if List.length !sent = 1 then
          Lwt.return_error (Post.rpc_failure
            (`Assoc ["code", `Int code; "data", `String data]))
        else Lwt.return_ok ());
      warn = (fun _ -> ());
    } in
    Post.put post ~hash:"same" tx;
    Lwt_main.run (Lwt.pause ());
    let delay, reply = Option.get !timer in
    expect "temporary RPC refusal gets quick retry" (delay = 1.);
    clock := delay;
    Lwt.wakeup reply ();
    Lwt_main.run (Lwt.pause ());
    expect "retry preserves signed transaction" (!sent = [tx; tx]);
    clock := 100.;
    Post.tick post;
    expect "accepted retry finishes posting" (List.length !sent = 2);
    Post.stop post)
    failures

let check_quiet_cycle () =
  let module Fold = Octra_core.Set_fold in
  let sample = ref Actor.{ epoch = 100L; active = false; bonded = Ok true } in
  let state = ref Fold.empty in
  let sent = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok (Fold.receipt ~address:"octA" !state));
    peers = (fun () -> 1);
    send = (fun ~epoch action ->
      sent := (epoch, action) :: !sent;
      match action with
      | Actor.Pulse ->
        state := Fold.note_pulse Fold.standard ~epoch ~active:false
          ~address:"octA" !state |> Result.get_ok;
        Lwt.return_ok ()
      | Actor.Appeal _ -> Lwt.return_ok ());
    warn = (fun _ reason -> failwith reason);
  } in
  let tick epoch event =
    sample := { !sample with epoch = Int64.of_int epoch };
    ignore (Actor.notify actor ~epoch:!sample.epoch event);
    Lwt_main.run (settle ())
  in
  for epoch = 100 to 164 do tick epoch None done;
  expect "rejoin needs seventeen confirmed pulses, not one per epoch"
    (List.length !sent = 17);
  expect "pulse schedule satisfies unchanged admission rule"
    (Fold.allows Fold.standard ~start:0L ~source:164L ~address:"octA" !state);
  sample := { !sample with active = true };
  for epoch = 165 to 324 do tick epoch None done;
  expect "active validator is quiet for 160 epochs" (List.length !sent = 17);
  tick 325 (Some (vote 323L, parent 323L));
  expect "active validator retains useful appeal"
    (match !sent with (_, Actor.Appeal proof) :: _ -> proof.vote.epoch_id = 323L
     | _ -> false);
  sample := { !sample with bonded = Ok false };
  let count = List.length !sent in
  tick 326 None;
  expect "exited validator is quiet even with pending proof" (List.length !sent = count);
  Lwt_main.run (Actor.shutdown actor)

let check_delivery_load loss =
  let module Post = Octra_node_runtime.Set_post in
  let module Pool = Octra_core.Tx_staging in
  let module Fold = Octra_core.Set_fold in
  let module Tx = Octra_core.Transaction in
  let module Ready = Octra_core.Validator_registry in
  let mode = Octra_core.Rule_graph.Active in
  let cfg = Fold.participating in
  let epoch = ref 100 in
  let clock = ref 0. in
  let live = List.init 36 (fun index -> Printf.sprintf "octLive%04d" index) in
  let addresses = List.init 764 (fun index -> Printf.sprintf "octQueue%04d" index) in
  let state = ref (Fold.note_set cfg ~epoch:100L ~active:live Fold.empty |> Result.get_ok) in
  let nonces = Hashtbl.create 764 in
  let signed = Hashtbl.create 764 in
  let lost = Hashtbl.create 256 in
  let confirmed address = Option.value ~default:0 (Hashtbl.find_opt nonces address) in
  let lookup address = Some (Z.of_int 1_000_000, confirmed address) in
  let eligible tx =
    if Pool.duty_expired ~mode ~head:(Some (Int64.of_int (!epoch - 1))) tx then
      Post.Expired else Post.Eligible in
  Pool.clear ();
  let nodes = List.map (fun address ->
    let stage ~current tx =
      if not (current ()) then Lwt.return_error (Post.Wait "generation changed")
      else if loss && (Ready.ready_payload_of_message tx.Tx.message |> Result.get_ok).head_epoch = 119L then begin
        Hashtbl.replace lost tx.from ();
        Lwt.return_ok ()
      end
      else if Option.is_some (Pool.find_by_hash (Tx.hash tx)) then Lwt.return_ok ()
      else Lwt.return (Pool.add_smart ~lookup tx
        |> Result.map (fun _ -> ()) |> Result.map_error (fun error -> Post.Wait error)) in
    let retry = Post.{retain = true; eligible; post = stage} in
    let post = Post.create Post.{
      now = (fun () -> !clock);
      wait = (fun _ -> fst (Lwt.task ()));
      staged = (fun hash -> Option.is_some (Pool.find_by_hash hash));
      landed = (fun tx -> confirmed tx.Tx.from >= tx.nonce);
      post = (fun _ -> failwith "delivery lost signed ownership");
      warn = (fun reason -> failwith reason);
    } in
    let actor = Actor.create Actor.{
      sample = (fun () -> {epoch = Int64.of_int !epoch; active = false; bonded = Ok true});
      read = (fun ~epoch:_ -> Lwt.return_ok (Fold.receipt ~address !state));
      peers = (fun () -> 1);
      send = (fun ~epoch action ->
        expect "waiting participant sends pulses" (action = Actor.Pulse);
        if not (Post.pending post) then begin
          let tx = Tx.{
            from = address; to_ = address; amount = Z.zero; nonce = confirmed address + 1;
            ou = Z.of_int 1_000; timestamp = !clock; signature = "queue-test";
            public_key = Some "key"; op_type = ValidatorReady; encrypted_data = None;
            message = Some (Yojson.Safe.to_string (`Assoc [
              "consensus_pubkey", `String "key";
              "head_epoch", `String (Int64.to_string (Int64.pred epoch));
              "head_proposal_id", `String (String.make 64 'a');
              "state_root", `String (String.make 64 'b');
            ]));
          } in
          let count = Option.value ~default:0 (Hashtbl.find_opt signed address) in
          Hashtbl.replace signed address (count + 1);
          Post.put ~retry post ~hash:(Tx.hash tx) tx
        end;
        Lwt.return_ok ());
      warn = (fun _ reason -> failwith reason);
    } in
    address, actor, post) addresses in
  Fun.protect ~finally:(fun () ->
    List.iter (fun (_, actor, post) ->
      Post.stop post;
      Lwt_main.run (Actor.shutdown actor)) nodes;
    Pool.clear ()) (fun () ->
    for step = 100 to 202 do
      epoch := step;
      clock := float_of_int ((step - 100) * 10);
      ignore (Pool.expire_duty ~mode ~head:(Some (Int64.of_int (step - 1))) ());
      List.iter (fun (_, actor, post) ->
        Post.tick post;
        ignore (Actor.notify actor ~epoch:(Int64.of_int step) None)) nodes;
      Lwt_main.run (settle ());
      expect "one queued duty per waiting participant" (Pool.staging_size () <= 764);
      List.iter (fun address ->
        expect "no queued nonce ratchet"
          (List.length (Pool.sender_entries address) <= 1)) addresses;
      let selected = Pool.ready_epoch_txs ~capacity:(Z.of_int 256_000)
        ~confirmed_nonce:(fun address -> Some (confirmed address))
        ~accept:(fun tx -> eligible tx = Post.Eligible
          && not (loss && step = 123 && Hashtbl.mem lost tx.Tx.from)) in
      expect "capacity limits each epoch to two hundred fifty six" (List.length selected <= 256);
      List.iter (fun tx ->
        let ready = Ready.ready_payload_of_message tx.Tx.message |> Result.get_ok in
        let execution = Int64.of_int step in
        expect "selected duty remains inside delivery window"
          (Octra_core.Validator_ready_policy.delivery ~epoch:execution ~head:ready.head_epoch);
        state := Fold.note_pulse ~credit:(Int64.succ ready.head_epoch) cfg
          ~epoch:execution ~active:false ~address:tx.from !state |> Result.get_ok;
        Hashtbl.replace nonces tx.from tx.nonce) selected;
      Pool.remove_processed (List.map Tx.hash selected);
      state := Fold.to_string !state |> Fold.of_string |> Result.get_ok;
      if step >= 103 then begin
        let open Yojson.Safe.Util in
        let entries = Fold.to_yojson !state |> member "members" |> to_list in
        List.iter (fun item ->
          let phase = member "phase" item in
          if member "kind" phase = `String "shadow" then
            let first = member "pulse" phase |> member "first" |> to_string |> Int64.of_string in
            expect "delivery and loss never reset an established series" (first <= 102L)) entries
      end
    done;
    expect "all waiting participants retain admission progress"
      (List.for_all (fun address -> Fold.allows cfg ~start:0L ~source:202L ~address !state) addresses);
    expect "capacity does not cause repeated resigning"
      (List.for_all (fun address ->
        let count = Hashtbl.find signed address in
        let extra = if Hashtbl.mem lost address then 1 else 0 in
        count <= 27 + extra && count <= confirmed address + 1 + extra) addresses);
    expect "loss scenario actually drops a wave" ((Hashtbl.length lost > 0) = loss);
    Printf.printf "event = duty_pipeline waiting = 764 active_records = 36 capacity = 256 epochs = 103 lost = %d status = pass\n%!"
      (Hashtbl.length lost))

let () =
  List.iter check_read_rollover [
    110L, 111L, `Marked, Actor.Marked;
    116L, 117L, `Error, Actor.Unread;
    116L, 117L, `Raised, Actor.Unread;
    116L, 117L, `Marked, Actor.Marked;
    117L, 118L, `Marked, Actor.Marked;
    116L, 117L, `Empty, Actor.Unread;
    116L, 120L, `Empty, Actor.Unread;
    117L, 118L, `Empty, Actor.Expired;
  ];
  check_expiry_queue ();
  List.iter check_last_receipt [`Marked; `Unread; `Moved; `Raised];
  check_last_absence ();
  check_proof_trace ();
  check_trace_failure ();
  check_trace_capacity ();
  check_delivery_load false;
  check_delivery_load true;
  check_membership_read ();
  check_shadow_appeal ();
  check_quiet_cycle ();
  check_post_failure ();
  check_post_recovery ();
  check_post_retry ();
  check_post_cancel ();
  check_post_head ();
  check_ack ();
  check_pulse_ack ();
  check_read ();
  check_phase ();
  check_send_resume ();
  check_flow ();
  check_effect_failure ();
  check_overload ();
  check_wake_merge ();
  Printf.printf "status = pass test = set_actor\n%!"