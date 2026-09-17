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
  let sample = ref Actor.{ epoch = 100L; active = false; bonded = true } in
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
      warn = (fun _ -> ());
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
  sample := { !sample with epoch = 103L; bonded = false };
  ignore (Actor.notify actor ~epoch:103L None);
  Lwt_main.run (settle ());
  expect "unbonded validator emits nothing" (List.length !sent = first_count);
  sample := { !sample with epoch = 104L; bonded = true };
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
  let actor =
    Actor.create Actor.{
      sample = (fun () -> { epoch = 1L; active = false; bonded = true });
      read = unread;
      peers = (fun () -> 1);
      send = (fun ~epoch:_ _ -> wait);
      warn = (fun _ -> ());
    }
  in
  ignore (Actor.notify actor ~epoch:1L None);
  Lwt_main.run (Lwt_unix.sleep 0.01);
  for index = 1 to Actor.stream_capacity do
    expect "stream accepts finite capacity"
      (Actor.notify actor ~epoch:(Int64.of_int (index + 1)) None
       = Actor.Accepted)
  done;
  expect "stream reports overload"
    (Actor.notify actor ~epoch:99L None = Actor.Busy);
  Lwt.wakeup_later wake (Ok ());
  Lwt_main.run (Lwt_unix.sleep 0.02);
  Lwt_main.run (Actor.shutdown actor)

let check_effect_failure () =
  let fail = ref true in
  let sent = ref 0 in
  let warnings = ref 0 in
  let sample = ref Actor.{ epoch = 1L; active = false; bonded = true } in
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
      warn = (fun _ -> incr warnings);
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
      sample = (fun () -> { epoch = !point.epoch; active = appeal; bonded = true });
      read = unread;
      peers = (fun () -> 1);
      send = (fun ~epoch action ->
        match Actor.plan ~epoch !point with
        | Error reason -> Lwt.return_error (Actor.reason reason)
        | Ok head -> sent := (action, head) :: !sent; Lwt.return_ok ());
      warn = (fun reason -> warnings := reason :: !warnings);
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
  let sample = ref Actor.{ epoch = 102L; active = true; bonded = true } in
  let receipt = ref Octra_core.Set_fold.{ marked = []; pulse = None } in
  let sent = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok !receipt);
    peers = (fun () -> 1);
    send = (fun ~epoch action -> sent := (epoch, action) :: !sent; Lwt.return_ok ());
    warn = (fun reason -> failwith reason);
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

let check_pulse_ack () =
  let sample = ref Actor.{ epoch = 100L; active = false; bonded = true } in
  let receipt = ref Octra_core.Set_fold.{ marked = []; pulse = None } in
  let sent = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ -> Lwt.return_ok !receipt);
    peers = (fun () -> 1);
    send = (fun ~epoch action -> sent := (epoch, action) :: !sent; Lwt.return_ok ());
    warn = (fun reason -> failwith reason);
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

let check_read () =
  let sample = ref Actor.{ epoch = 102L; active = true; bonded = true } in
  let mode = ref 0 in
  let sent = ref 0 in
  let warnings = ref [] in
  let actor = Actor.create Actor.{
    sample = (fun () -> !sample);
    read = (fun ~epoch:_ ->
      if !mode = 0 then Lwt.return_error "committed read unavailable"
      else if !mode = 1 then begin
        sample := { !sample with epoch = 103L };
        Lwt.return_ok Octra_core.Set_fold.{ marked = [100L]; pulse = None }
      end else unread ~epoch:!sample.epoch);
    peers = (fun () -> 1);
    send = (fun ~epoch:_ _ -> incr sent; Lwt.return_ok ());
    warn = (fun reason -> warnings := reason :: !warnings);
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
  expect "changed head retains proof"
    (!sent = 0 && (Lwt_main.run (Actor.stats actor)).appeals = 1);
  mode := 2;
  tick ();
  expect "fresh read resumes proof" (!sent = 1);
  sample := { !sample with epoch = 117L };
  tick ();
  expect "challenge expires after last valid epoch"
    (!sent = 1 && (Lwt_main.run (Actor.stats actor)).appeals = 0);
  Lwt_main.run (Actor.shutdown actor)

let () =
  check_ack ();
  check_pulse_ack ();
  check_read ();
  check_phase ();
  check_send_resume ();
  check_flow ();
  check_effect_failure ();
  check_overload ();
  Printf.printf "status = pass test = set_actor\n%!"