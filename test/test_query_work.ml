(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

open Octra_consensus
module Q = C_query_work

let check name value = if not value then failwith name
let start ?network ?(validator = false) ?(cost = 4) now peer state =
  Q.step state (Q.Start {now; peer; network = Option.value network ~default:peer; validator; cost})
let finish state id = fst (Q.step state (Q.Finish (Option.get id)))

let test_network () =
  let key host = Q.network (Unix.ADDR_INET (Unix.inet_addr_of_string host, 19000)) in
  check "IPv4 subnet differs" (key "192.0.2.1" = key "192.0.2.200");
  check "IPv4 subnets share credit" (key "192.0.2.1" <> key "192.0.3.1");
  check "mapped IPv4 escaped credit" (key "192.0.2.1" = key "::ffff:192.0.2.1");
  check "IPv6 subnet differs" (key "2001:db8:1:2::1" = key "2001:db8:1:2::20");
  check "IPv6 subnets share credit" (key "2001:db8:1:2::1" <> key "2001:db8:1:3::1")

let test_capacity () =
  let a, id = start 0L "a" (Q.create 0L) in
  check "repeated identity got a slot" (snd (start 0L "a" a) = None);
  let b, _ = start 0L "b" a in
  check "public capacity exceeded" (snd (start 0L "c" b) = None);
  let c, _ = start ~validator:true 0L "v" b in
  let d, _ = start ~validator:true 0L "w" c in
  check "validator capacity exceeded" (snd (start ~validator:true 0L "x" d) = None);
  check "capacity differs" (List.length d.Q.active = 4);
  let e = finish d id in
  check "completion lost other jobs" (List.length e.Q.active = 3);
  check "late completion removed another job" (finish e id = e)

let test_credit () =
  let state, id = start ~cost:16 0L "a" (Q.create 0L) in
  let state = finish state id in
  check "reconnect reset credit" (snd (start ~cost:1 0L "a" state) = None);
  check "other tag escaped credit" (snd (start ~cost:16 0L "a" state) = None);
  check "new identity escaped global credit" (snd (start ~cost:1 0L "b" state) = None);
  check "negative clock refilled" (snd (start ~cost:1 (-1L) "a" state) = None);
  check "fractional credit rounded up" (snd (start ~cost:1 999_999_999L "a" state) = None);
  check "credit failed to refill" (snd (start ~cost:1 1_000_000_000L "a" state) <> None);
  check "public debt consumed validator reserve"
    (snd (start ~validator:true ~cost:16 0L "v" state) <> None);
  let rec fill index state = if index = 256 then state else
    let now = Int64.mul (Int64.of_int index) 100_000_000L in
    let state, id = start ~validator:true ~cost:1 now (string_of_int index) state in
    check "honest paced request refused" (id <> None);
    fill (index + 1) (finish state id) in
  let full = fill 0 (Q.create 0L) in
  check "tracking capacity exceeded" (snd (start ~validator:true 26_000_000_000L "new" full) = None);
  check "tracking exhausted other lane" (snd (start 26_000_000_000L "observer" full) <> None);
  check "expired tracking never released" (snd (start 90_000_000_000L "new" full) <> None)

let test_prepared () =
  List.iter (fun validator ->
    let prepare state id = fst (Q.step state (Q.Prepared {
      id = Option.get id; bytes = Q.reply_limit; cost = 4})) in
    let state, a = start ~validator 0L "a" (Q.create 0L) in
    let state = prepare state a in
    let state, b = start ~validator 0L "b" state in
    let state = prepare state b in
    let state, c = start ~validator 0L "c" state in
    check "two writers captured compute capacity" (c <> None);
    check "writing identity was reused" (snd (start ~validator 0L "a" state) = None);
    let state = prepare state c in
    let state, d = start ~validator 0L "d" state in
    check "fourth reply had no reservation" (d <> None);
    check "reply bytes exceeded lane" (Q.lane_bytes state validator = Q.lane_limit);
    check "fifth reply escaped byte cap" (snd (start ~validator 10_000_000_000L "e" state) = None);
    let bad, ready = Q.step state (Q.Prepared {id = Option.get d; bytes = Q.reply_limit + 1; cost = 1}) in
    check "oversized reply accepted" (ready = None && bad = state);
    let next, ready = Q.step state (Q.Prepared {id = Option.get d; bytes = 1; cost = 1}) in
    check "reply preparation failed" (ready <> None);
    check "duplicate preparation refunded twice"
      (fst (Q.step next (Q.Prepared {id = Option.get d; bytes = 1; cost = 1})) = next);
    check "small reply did not release byte reservation" (Q.lane_bytes next validator = 3 * Q.reply_limit + 1))
    [false; true]

let test_wait_turn () =
  let state, id = start ~network:"shared" ~cost:16 0L "a" (Q.create 0L) in
  let state = finish state id in
  let state, id = start ~network:"shared" 0L "reader" state in
  check "empty group granted credit" (id = None);
  check "reader did not retain turn" (List.length state.waiting = 1);
  let again, id = start ~network:"shared" ~cost:1 1_000_000_000L "a" state in
  check "new request overtook reader" (id = None);
  let renewed, _ = start ~network:"shared" 1_500_000_000L "reader" again in
  check "retry did not renew reservation" ((List.hd renewed.waiting).until = 17_500_000_000L);
  check "other lane blocked by turn"
    (snd (start ~network:"shared" ~validator:true 2_000_000_000L "v" renewed) <> None);
  let renewed, _ = start ~network:"shared" 3_000_000_000L "reader" renewed in
  let served, id = start ~network:"shared" 4_000_000_000L "reader" renewed in
  check "first reader did not receive replenished credit" (id <> None);
  check "served turn retained" (not (List.exists (fun (w : Q.waiting) -> w.who = "reader") served.waiting));
  let surplus, id = start ~network:"shared" ~cost:1 6_000_000_000L "new" renewed in
  check "absent reader consumed surplus" (id <> None && surplus.waiting <> []);
  let expired, id = start ~network:"shared" 19_000_000_000L "new" renewed in
  check "absent reader blocked expiry" (id <> None && expired.waiting = []);
  let filled = List.fold_left (fun state i ->
    fst (start ~network:"shared" 0L (string_of_int i) state)) state (List.init 300 Fun.id) in
  check "waiting capacity exceeded" (List.length filled.waiting = 2)

let test_fair_share () =
  List.iter (fun (validator, shared) ->
    let send now peer network cost state =
      let state, id = start ~validator ~network ~cost now peer state in
      match id with None -> state, false | Some _ -> finish state id, true in
    let rec run tick state served =
      if tick = 2400 then served else
      let now = Int64.mul (Int64.of_int tick) 250_000_000L in
      let group name = if shared then "shared" else name in
      let state, _ = send now "a" (group "a") 1 state in
      let state, _ = send now "b" (group "b") 1 state in
      let state, accepted = send now "reader" (group "reader") 4 state in
      run (tick + 1) state (served + if tick >= 160 && accepted then 1 else 0) in
    let served = run 0 (Q.create 0L) 0 in
    let minimum = if shared then (if validator then 320 else 80) else (if validator then 500 else 130) in
    check "two readers starved honest catchup" (served >= minimum);
    Printf.printf "event = query_share validator = %b shared = %b served = %d\n" validator shared served;
    let state, _ = send 0L "a" "net" 16 (Q.create 0L) in
    let state, _ = if validator then send 1_000_000_000L "b" "net" 16 state else state, false in
    check "network credit did not follow authentication"
      (snd (send 1_000_000_000L "c" "net" 16 state) = validator))
    [false, false; true, false; false, true; true, true]

let test_many_readers () =
  List.iter (fun validator -> List.iter (fun count -> List.iter (fun cost ->
    let send now peer cost state =
      let next, id = start ~validator ~cost now peer state in
      match id with None -> next, 0 | Some _ -> finish next id, 1 in
    let rec loop tick state served =
      if tick = 2400 then served else
      let now = Int64.mul (Int64.of_int tick) 250_000_000L in
      let state = List.fold_left (fun state peer ->
        fst (send now ("busy" ^ string_of_int peer) 1 state)) state (List.init count Fun.id) in
      let state, got = send now "reader" cost state in
      loop (tick + 1) state (served + if tick >= 160 then got else 0) in
    let served = loop 0 (Q.create 0L) 0 in
    check "multiple networks starved reader" (served >= 20);
    Printf.printf "event = query_many validator = %b networks = %d cost = %d served = %d\n"
      validator count cost served) [4; 16]) [4; 8]) [false; true]

let test_lane_tracking () =
  let rec loop n state =
    if n = 600 then state else
    let now = Int64.mul (Int64.of_int n) 80_000_000L in
    let state, id = start ~validator:true ~network:(string_of_int n) ~cost:1
      now ("moving" ^ string_of_int (n mod 4)) state in
    check "address change exhausted tracking" (id <> None);
    loop (n + 1) (finish state id) in
  let state = loop 0 (Q.create 0L) in
  check "validator retained multiple address budgets" (Q.Peers.cardinal state.networks = 4);
  check "validator traffic exhausted public tracking" (snd (start 48_000_000_000L "reader" state) <> None);
  check "validator traffic exhausted other identity"
    (snd (start ~validator:true 48_000_000_000L "other" state) <> None)

let test_retry_cadence () =
  List.iter (fun seconds -> List.iter (fun validator ->
    let interval = seconds * 4 in
    let send now who cost state =
      let state, id = start ~validator ~cost now who state in
      match id with None -> state, 0 | Some _ -> finish state id, 1 in
    let rec run tick state served last longest =
      if tick = 2400 then served, longest else
      let now = Int64.mul (Int64.of_int tick) 250_000_000L in
      let state = List.fold_left (fun state n ->
        fst (send now ("busy" ^ string_of_int n) 1 state)) state (List.init 4 Fun.id) in
      let state, got = if tick mod interval = 0 then send now "reader" 4 state else state, 0 in
      let last = if got = 1 then tick else last in
      run (tick + 1) state (served + got) last (max longest (tick - last)) in
    let served, longest = run 0 (Q.create 0L) 0 0 0 in
    check "ordinary retry cadence lost turn" (served >= 2400 / (interval * 3));
    check "ordinary retry cadence starved" (longest <= interval * 3);
    Printf.printf "event = query_retry validator = %b interval = %d served = %d gap = %d\n"
      validator seconds served longest) [false; true]) [2; 8; 10]

let test_write () =
  let open Lwt.Syntax in
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Observer
    local ~peer_id:"a" ~addr:"local" ~direction:N.Inbound in
  let frame = F.{msg_type = msg_bundle_response; payload = "reply"} in
  let wait, wake = Lwt.wait () in
  let sending = C_driver.send_query ~wait:(fun () -> wait) conn frame in
  check "enqueue released service" (Lwt.is_sleeping sending);
  check "missing write receipt" (Option.is_some conn.service_write);
  Lwt.wakeup wake ();
  Lwt_main.run (let* () = Lwt.pause () in
    check "slow reader closed connection" conn.connected;
    check "slow reader dropped response" (Lwt.is_sleeping sending && Option.is_some conn.service_write);
    let writer = N.write_loop conn in
    let* () = sending in
    check "slow write count differs" (conn.msg_count_out = 1);
    check "completed slow write kept receipt" (conn.service_write = None);
    let* () = N.close conn in
    let* () = writer in
    Lwt_unix.close remote);
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Observer
    local ~peer_id:"b" ~addr:"local" ~direction:N.Inbound in
  Lwt_main.run (let writer = N.write_loop conn in
    let* () = N.send_service conn frame in
    check "write count differs" (conn.msg_count_out = 1);
    check "completed write kept receipt" (conn.service_write = None);
    let* () = N.close conn in let* () = writer in Lwt_unix.close remote)

let test_service_wait () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Observer
    local ~peer_id:"waiting" ~addr:"local" ~direction:N.Inbound in
  Lwt_main.run (N.send conn F.{msg_type = msg_tx_gossip; payload = "notice"});
  let clock, wake = Lwt.wait () in
  let waiting = N.await_service ~wait:(fun () -> clock) conn in
  check "service skipped occupied writer" (Lwt.is_sleeping waiting && conn.service_pending);
  check "second service wait allocated" (not (Lwt_main.run (N.await_service conn)));
  Lwt.wakeup wake ();
  check "service wait exceeded deadline" (not (Lwt_main.run waiting));
  check "service wait closed ordinary traffic" (conn.connected && not conn.service_pending);
  let clock, wake = Lwt.wait () in
  let sending = N.send_service ~wait:(fun () -> clock) conn
    F.{msg_type = msg_bundle_response; payload = "reply"} in
  check "busy writer dropped response" (Lwt.is_sleeping sending);
  Lwt.wakeup wake ();
  Lwt_main.run sending;
  check "busy writer deadline closed connection" conn.connected;
  let never, _ = Lwt.wait () in
  let sending = N.send_service ~wait:(fun () -> never) conn
    F.{msg_type = msg_bundle_response; payload = "reply"} in
  Lwt_main.run (let open Lwt.Syntax in
    let writer = N.write_loop conn in
    let* () = sending in
    check "writer race lost response" (conn.msg_count_out = 2);
    let* () = N.close conn in writer);
  Lwt_main.run (let open Lwt.Syntax in let* () = N.close conn in Lwt_unix.close remote)

let () =
  test_network ();
  test_capacity ();
  test_fair_share ();
  test_credit ();
  test_retry_cadence ();
  test_prepared ();
  test_wait_turn ();
  test_many_readers ();
  test_lane_tracking ();
  test_write ();
  test_service_wait ();
  print_endline "status = pass suite = query_work"