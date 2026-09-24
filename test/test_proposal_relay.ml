(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Relay = Octra_consensus.C_relay

let expect label value =
  if not value then failwith ("test_proposal_relay: " ^ label)

let item key = Relay.{ key; value = key; expires = 2. }
let offer key = Relay.Offer (0, 0., item key)

let check_capacity () =
  let state, effects = Relay.step Relay.empty (offer "first") in
  expect "first send starts once" (List.length effects = 1);
  let serial = fst (Option.get state.active) in
  let state, effects = Relay.step state (offer "first") in
  expect "active duplicate is ignored" (effects = [] && state.queue = []);
  let state = List.fold_left (fun state i ->
    fst (Relay.step state (offer (string_of_int i)))) state (List.init 1000 Fun.id) in
  expect "waiting queue is finite" (List.length state.queue = Relay.capacity);
  expect "dedup keys have finite capacity" (List.length state.seen <= 64);
  let state, effects = Relay.step state (Relay.Finished (serial, 0.5, Relay.Sent)) in
  expect "completion starts next send"
    (List.length effects = 1 && Option.map fst state.active <> Some serial);
  let state, _ = Relay.step state (Relay.Advance (1, 0.6)) in
  expect "new generation discards old work" (state.active = None && state.queue = []);
  let same, effects = Relay.step state (Relay.Finished (serial, 0.7, Relay.Sent)) in
  expect "old completion cannot resume sending" (same = state && effects = []);
  let same, effects = Relay.step state (offer "old") in
  expect "old generation cannot submit" (same = state && effects = []);
  let state, _ = Relay.step state Relay.Close in
  let same, effects = Relay.step state (Relay.Offer (2, 0., item "closed")) in
  expect "closed relay refuses work" (same = state && effects = []);
  let state, _ = Relay.step state (Relay.Open 2) in
  let state, effects = Relay.step state (Relay.Offer (2, 0., item "new")) in
  expect "restart has a new request id"
    (Option.map fst state.active <> Some serial && List.length effects = 1)

let check_expiry () =
  let state, _ = Relay.step Relay.empty (offer "first") in
  let serial = fst (Option.get state.active) in
  let state, _ = Relay.step state (offer "second") in
  let state, effects = Relay.step state (Relay.Finished (serial, 2., Relay.Expired)) in
  expect "active and waiting expiry are reported"
    (state.queue = [] && effects = [Relay.Dropped "expired"; Relay.Dropped "expired"]);
  let state, effects = Relay.step state
    (Relay.Offer (0, 2., { (item "second") with expires = 4. })) in
  expect "unsent work can be offered again" (List.length effects = 1);
  let serial = fst (Option.get state.active) in
  let state, _ = Relay.step state (Relay.Finished (serial, 2.5, Relay.Sent)) in
  let same, effects = Relay.step state
    (Relay.Offer (0, 2.5, { (item "second") with expires = 4.5 })) in
  expect "completed work remains deduplicated" (same = state && effects = []);
  let state, effects = Relay.step state (Relay.Offer (0, 2., item "third")) in
  expect "expired admission is explicit" (effects = [Relay.Dropped "expired"]);
  expect "expired admission does not occupy slot" (state.active = None)

let check_failure () =
  let state, _ = Relay.step Relay.empty (offer "first") in
  let serial = fst (Option.get state.active) in
  let state, effects = Relay.step state
    (Relay.Finished (serial, 0.1, Relay.Failed "peer closed")) in
  expect "send failure is reported" (effects = [Relay.Dropped "peer closed"]);
  let state, effects = Relay.step state (offer "first") in
  expect "failed send can be retried" (List.length effects = 1);
  let same, effects = Relay.step state (Relay.Finished (serial, 0.2, Relay.Sent)) in
  expect "late success cannot mark retry complete" (same = state && effects = []);
  let state, _ = Relay.step state (Relay.Offer (0, 3., item "expired")) in
  expect "expired offers never populate dedup" (not (List.mem "expired" state.seen))

let check_history () =
  let state = List.fold_left (fun state index ->
    let state, _ = Relay.step state (offer (string_of_int index)) in
    let serial = fst (Option.get state.active) in
    fst (Relay.step state (Relay.Finished (serial, 0., Relay.Sent))))
    Relay.empty (List.init 1000 Fun.id)
  in
  expect "completed history has finite capacity" (List.length state.seen = 64)

let check_runtime () =
  let clock = ref 0. in
  let sent = ref [] in
  let pending = ref [] in
  let timers = ref [] in
  let warnings = ref [] in
  let actor = Relay.create
    ~now:(fun () -> !clock)
    ~send:(fun value ->
      sent := value :: !sent;
      let promise, reply = Lwt.task () in
      pending := (promise, reply) :: !pending;
      promise)
    ~wait:(fun _ ->
      let promise, reply = Lwt.task () in
      timers := reply :: !timers;
      promise)
    ~warn:(fun reason -> warnings := reason :: !warnings)
  in
  let flush () = Lwt_main.run (Lwt.pause ()) in
  Relay.offer actor ~generation:0 ~key:"a" "a";
  expect "submission does not wait for peer" (!sent = []);
  flush ();
  Relay.offer actor ~generation:0 ~key:"a" "a";
  Relay.offer actor ~generation:0 ~key:"b" "b";
  flush ();
  expect "one peer send at a time" (!sent = ["a"]);
  let first, _ = List.hd !pending in
  Relay.progress actor ~generation:1;
  flush ();
  expect "generation change cancels pending send"
    (match Lwt.state first with Lwt.Fail Lwt.Canceled -> true | _ -> false);
  Relay.offer actor ~generation:1 ~key:"c" "c";
  flush ();
  let current, _ = List.hd !pending in
  Relay.close actor;
  Relay.open_ actor ~generation:1;
  Relay.offer actor ~generation:1 ~key:"d" "d";
  flush ();
  expect "close cancels old send"
    (match Lwt.state current with Lwt.Fail Lwt.Canceled -> true | _ -> false);
  expect "restart does not send old queue" (!sent = ["d"; "c"; "a"]);
  let blocked, _ = List.hd !pending in
  clock := 3.;
  Lwt.wakeup (List.hd !timers) ();
  flush ();
  expect "deadline cancels blocked peer"
    (match Lwt.state blocked with Lwt.Fail Lwt.Canceled -> true | _ -> false);
  expect "active deadline is reported" (!warnings = ["expired"]);
  Relay.offer actor ~generation:1 ~key:"e" "e";
  Relay.offer actor ~generation:1 ~key:"f" "f";
  flush ();
  expect "deadline releases slot" (!sent = ["e"; "d"; "c"; "a"]);
  Lwt.wakeup_exn (snd (List.hd !pending)) (Failure "peer disconnected");
  flush ();
  expect "peer exception does not wedge relay" (!sent = ["f"; "e"; "d"; "c"; "a"]);
  Relay.close actor

let () =
  check_capacity ();
  check_expiry ();
  check_failure ();
  check_history ();
  check_runtime ();
  Printf.printf "status = pass test = proposal_relay\n%!"