(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module C = Octra_core
module F = C.Proof_frame
module S = C.Proof_serve
module W = C.Proof_warm
module L = C.Proof_link
module K = C.Proof_keeper
module P = C.Pvac_verify_protocol

let need reason value = if not value then failwith reason

let pipe_signal () =
  List.iter (fun pending ->
    let child = Unix.fork () in
    if child = 0 then begin
      Sys.set_signal Sys.sigpipe Sys.Signal_default;
      ignore (Thread.sigmask Unix.SIG_UNBLOCK [Sys.sigpipe]);
      let input, output = Unix.pipe ~cloexec:true () in
      Unix.close input;
      if pending then begin
        ignore (Thread.sigmask Unix.SIG_BLOCK [Sys.sigpipe]);
        Unix.kill (Unix.getpid ()) Sys.sigpipe
      end;
      let mask = Thread.sigmask Unix.SIG_BLOCK [] in
      let refused = try F.write output "x"; false
        with Unix.Unix_error (Unix.EPIPE, _, _) -> true in
      let offset = ref 0 in
      let worker = C.Pvac_verify_worker.write_available output "x" offset in
      let intact = mask = Thread.sigmask Unix.SIG_BLOCK []
        && List.mem Sys.sigpipe (Unix.sigpending ()) = pending in
      Unix.close output;
      Unix._exit (if refused && intact && worker = Error "worker_input_closed" && !offset = 0 then 0 else 2)
    end;
    need "closed pipe killed writer" (snd (Unix.waitpid [] child) = Unix.WEXITED 0)) [false; true]

let decisions () =
  let configured = C.Pvac_verify_worker.prepared_config in
  need "prepared memory floor refused"
    (configured ~workers:1 ~memory:K.min_memory_mb = Ok ());
  need "prepared low memory accepted"
    (configured ~workers:1 ~memory:(K.min_memory_mb - 1) = Error "memory_limit");
  need "prepared parallel workers accepted"
    (configured ~workers:2 ~memory:K.min_memory_mb = Error "worker_count");
  let state, effects = W.delta W.empty (W.Tick 0.) in
  need "warm start missing" (effects = [W.Spawn 1]);
  need "wrong warm generation accepted" (W.delta state (W.Warmed 2) = (state, []));
  let state, _ = W.delta state (W.Warmed 1) in
  let state, effects = W.delta state (W.Borrow 5) in
  need "warm lease missing" (effects = [W.Granted (1, 5)]);
  need "parallel lease accepted" (W.delta state (W.Borrow 6) = (state, [W.Denied]));
  need "wrong reply accepted" (W.delta state (W.Returned (1, 4, true)) = (state, []));
  let state, _ = W.delta state (W.Check (1, 5, 20.)) in
  need "recheck lease expired" ((fst (W.delta state (W.Tick 21.))).phase = W.Absent);
  need "recheck lease accepted" (W.delta state (W.Borrow 6) = (state, [W.Denied]));
  List.iter (fun result ->
    let next, effects = W.delta state (W.Settled (1, 5, result)) in
    need "confirmation retained lease" (next.phase = W.Ready 1 && effects = []
      && next.expires = Float.infinity && next.proofs = state.proofs);
    need "other confirmation changed lease" (W.delta state (W.Settled (2, 5, result)) = (state, []));
    need "other request changed lease" (W.delta state (W.Settled (1, 6, result)) = (state, []));
    let expired, _ = W.delta state (W.Tick 21.) in
    need "expired confirmation revived lease" (W.delta expired (W.Settled (1, 5, result)) = (expired, [])))
    [W.Agreed; W.Unfinished];
  let state, effects = W.delta state (W.Settled (1, 5, W.Disagreed)) in
  need "disagreement continued" (state.phase = W.Closed && effects = [W.Retire]);
  need "closed session revived" (W.delta state (W.Tick 1000.) = (state, []));
  let checking = {W.empty with phase = W.Rechecking (3, 7); generation = 3;
    expires = 20.; proofs = [String.make 64 'a']} in
  List.iter (fun result ->
    let next, effects = W.poll checking 21. (Some (3, 7, result)) in
    need "queued confirmation expired" (next.phase = W.Ready 3 && effects = []
      && next.proofs = checking.proofs)) [W.Agreed; W.Unfinished];
  let next, effects = W.poll checking 21. (Some (3, 7, W.Disagreed)) in
  need "queued disagreement expired" (next.phase = W.Closed && next.proofs = []
    && effects = [W.Retire]);
  let next, effects = W.poll checking 21. None in
  need "missing confirmation retained" (next.phase = W.Absent && effects = [W.Retire]);
  List.iter (fun token -> need "other confirmation delayed expiry"
    (W.poll checking 21. (Some token) = (next, effects)))
    [2, 7, W.Agreed; 3, 8, W.Disagreed];
  let rec failures state count =
    if count = 3 then state else
      let state, _ = W.delta state (W.Tick 0.) in
      let state, _ = W.delta state (W.Failed state.generation) in
      failures state (count + 1) in
  let state, effects = W.delta (failures W.empty 0) (W.Tick 1.) in
  need "restart window exceeded" (state.phase = W.Paused 600. && effects = []);
  let state, _ = W.delta {W.empty with total = 16} (W.Tick 1000.) in
  need "restart count exceeded" (state.phase = W.Dormant 1600.);
  need "partial input reused" (S.delta (S.Running (1, false)) (S.Cancel 1) = (S.Closed, [S.Kill false]));
  need "loaded input discarded" (S.delta (S.Running (1, true)) (S.Cancel 1) = (S.Idle 1, [S.Kill true]));
  need "duplicate command accepted" (S.delta (S.Idle 4) (S.Begin 4) = (S.Closed, [S.Quit]));
  need "wrong abort accepted" (S.delta (S.Running (4, true)) (S.Cancel 3) = (S.Closed, [S.Kill false; S.Quit]));
  List.iter (fun message -> need "command encoding differs"
    (F.command (F.command_bytes message) = message)) [F.Check (1, 32, 1024); F.Abort 1; F.Stop];
  List.iter (fun raw ->
    let refused = try ignore (F.command raw); false with Failure _ -> true in
    need "invalid command accepted" refused)
    [F.pack [1; 0; 12; 1024]; F.pack [1; 1; P.max_request_bytes + 1; 1024]; F.pack [2; 1; 1; 0]];
  List.iter (fun value -> need "proof reply encoding differs" (F.reply (F.reply_bytes value) = value))
    [F.Answer (1, 0, 0, "x"); F.Answer (1, 1, 0, "x"); F.Answer (1, 1, 1, "x")];
  List.iter (fun raw ->
    let refused = try ignore (F.reply raw); false with Failure _ -> true in
    need "invalid equation count accepted" refused)
    [F.pack [7; 1; 0; 1]; F.pack [7; 1; 1; 2]; F.pack [7; 1; 1]; F.pack [3; 1; 1]]

let recovery_controls () =
  need "warming waiter cancelled parent"
    (K.waiting_result (W.Warming 17) ~control:L.Continue ~now:9. ~deadline:10. = None);
  need "warming cancellation missing"
    (K.waiting_result (W.Warming 17) ~control:L.Cancel ~now:9. ~deadline:10. = Some K.Cancelled);
  need "warming expiry missing"
    (K.waiting_result (W.Warming 17) ~control:L.Continue ~now:10. ~deadline:10. = Some K.Timed_out);
  need "warming cancellation lost priority"
    (K.waiting_result (W.Warming 17) ~control:L.Cancel ~now:11. ~deadline:10. = Some K.Cancelled);
  List.iter (fun phase ->
    need "waiter bypassed lease cleanup"
      (K.waiting_result phase ~control:L.Cancel ~now:11. ~deadline:10. = None))
    [W.Absent; W.Ready 17; W.Lent (17, 21); W.Rechecking (17, 21);
     W.Paused 10.; W.Dormant 10.; W.Closed];
  List.iter (fun phase ->
    need "terminal waiter cancellation differs"
      (K.waiting_result ~retiring:true phase ~control:L.Cancel ~now:11. ~deadline:10. = Some K.Cancelled);
    need "terminal waiter expiry differs"
      (K.waiting_result ~retiring:true phase ~control:L.Continue ~now:10. ~deadline:10. = Some K.Timed_out);
    need "terminal waiter retained"
      (K.waiting_result ~retiring:true phase ~control:L.Continue ~now:9. ~deadline:10. = Some (K.Recheck None)))
    [W.Warming 17; W.Ready 17; W.Dormant 10.; W.Closed];
  List.iter (fun phase -> need "terminal waiter bypassed child reap"
    (K.waiting_result ~retiring:true phase ~control:L.Cancel ~now:11. ~deadline:10. = None))
    [W.Lent (17, 21); W.Rechecking (17, 21)];
  let hash = String.make 64 'b' in
  let cached = String.make 64 'a' in
  let state = {W.empty with phase = W.Dormant 600.; generation = 16; total = 16;
    starts = [599.; 598.]; proofs = [cached]} in
  let ticket = W.{generation = 16; id = 20; hash; deadline = 610.} in
  List.iter (fun message -> need "dormant changed without demand" (W.delta state message = (state, [])))
    [W.Tick 600.; W.Tick 9000.; W.Failed 16; W.Failed 15; W.Warmed 16;
     W.Settled (16, 20, W.Agreed); W.Settled (16, 20, W.Unfinished); W.Settled (16, 20, W.Disagreed)];
  List.iter (fun (now, ticket) ->
    need "invalid recovery demand accepted" (W.delta state (W.Demand (now, ticket)) = (state, [W.Denied])))
    [599., ticket; 610., ticket; 600., {ticket with generation = 15};
     600., {ticket with id = 0}; 600., {ticket with hash = cached};
     600., {ticket with hash = "bad"}; 600., {ticket with deadline = Float.infinity}];
  let waiting, effects = W.delta state (W.Demand (600., ticket)) in
  need "recovery ticket missing" (waiting.recovery = Some ticket && effects = [W.Recovery ticket]);
  need "parallel recovery accepted"
    (W.delta waiting (W.Demand (601., {ticket with id = 21})) = (waiting, [W.Denied]));
  need "duplicate demand extended lease"
    (W.delta waiting (W.Demand (601., {ticket with deadline = 620.})) = (waiting, [W.Denied]));
  List.iter (fun other -> need "wrong recovery changed state"
    (W.delta waiting (W.Recovered (601., other, W.Cold_accepted)) = (waiting, [])))
    [{ticket with generation = 17}; {ticket with id = 21}; {ticket with hash = cached};
     {ticket with deadline = 611.}];
  List.iter (fun result ->
    let next, effects = W.delta waiting (W.Recovered (601., ticket, result)) in
    need "unsuccessful cold recovery restarted" (next = state && effects = []))
    [W.Cold_rejected; W.Cold_unfinished];
  let expired, effects = W.poll ~recovered:(ticket, W.Cold_accepted) waiting 610. None in
  need "expired recovery restarted" (expired = state && effects = []);
  let warming, effects = W.poll ~recovered:(ticket, W.Cold_accepted) waiting 601. None in
  need "cold recovery gave full burst" (warming.phase = W.Warming 17 && warming.total = 16
    && warming.recovery = None && warming.starts = [601.; 599.; 598.] && effects = [W.Spawn 17]);
  need "recovery replay started worker"
    (W.delta warming (W.Recovered (602., ticket, W.Cold_accepted)) = (warming, []));
  let limited = {waiting with starts = [599.; 598.; 597.]} in
  let next, effects = W.delta limited (W.Recovered (601., ticket, W.Cold_accepted)) in
  need "cold recovery bypassed rate limit"
    (next.phase = state.phase && next.starts = limited.starts && next.recovery = None && effects = []);
  let ready, _ = W.delta warming (W.Warmed 17) in
  let lent, _ = W.delta ready (W.Borrow 21) in
  let reused, _ = W.delta lent (W.Cached (17, 21, cached)) in
  need "cached proof reset burst" (reused.total = 16 && reused.starts = warming.starts);
  let repeated, _ = W.delta lent (W.Verified (17, 21, cached)) in
  need "remembered hash reset burst" (repeated.total = 16 && repeated.starts = warming.starts);
  let returned, _ = W.delta lent (W.Returned (17, 21, true)) in
  need "returned lease reset burst" (returned.total = 16 && returned.starts = warming.starts);
  List.iter (fun message -> need "wrong fresh proof reset burst" (W.delta lent message = (lent, [])))
    [W.Verified (16, 21, hash); W.Verified (17, 22, hash); W.Verified (17, 21, "bad")];
  let fresh, _ = W.delta lent (W.Verified (17, 21, hash)) in
  need "fresh proof did not reset burst" (fresh.total = 0 && fresh.starts = warming.starts
    && List.mem hash fresh.proofs);
  let failed, _ = W.delta fresh (W.Failed 17) in
  let paused, effects = W.delta failed (W.Tick 602.) in
  need "fresh reset erased rate limit" (paused.phase = W.Paused 1198. && effects = []);
  let failed, _ = W.delta reused (W.Failed 17) in
  let dormant, effects = W.delta failed (W.Tick 602.) in
  need "probe failure restarted automatically" (dormant.phase = W.Dormant 1202. && effects = []);
  need "probe cooldown restarted automatically" (W.delta dormant (W.Tick 9000.) = (dormant, []));
  let next_ticket = {ticket with generation = 17; id = 22; deadline = 1210.} in
  let next, effects = W.delta dormant (W.Demand (1203., next_ticket)) in
  need "new cold request for same hash refused" (effects = [W.Recovery next_ticket]);
  let next, effects = W.delta next (W.Recovered (1204., next_ticket, W.Cold_accepted)) in
  need "new cold work for same hash lost probe" (next.phase = W.Warming 18 && next.total = 16
    && effects = [W.Spawn 18]);
  let checking, _ = W.delta lent (W.Check (17, 21, 610.)) in
  List.iter (fun result ->
    let next, _ = W.delta checking (W.Settled (17, 21, result)) in
    need "settlement reset burst" (next.total = 16 && next.starts = warming.starts)) [W.Agreed; W.Unfinished];
  let closed, effects = W.poll ~recovered:(ticket, W.Cold_accepted) checking 611.
    (Some (17, 21, W.Disagreed)) in
  need "recovery overrode disagreement" (closed.phase = W.Closed && closed.proofs = []
    && closed.recovery = None && effects = [W.Retire]);
  let stopped, _ = W.delta waiting W.Stop in
  List.iter (fun state ->
    need "closed actor accepted demand" (W.delta state (W.Demand (9000., {ticket with deadline = 9010.}))
      = (state, [W.Denied]));
    need "closed actor accepted cold result" (W.delta state (W.Recovered (601., ticket, W.Cold_accepted))
      = (state, []))) [closed; stopped]

let remembered () =
  let start = {W.empty with phase = W.Ready 7; generation = 7} in
  let hash value = P.hash_bytes (string_of_int value) in
  let rec fill state index =
    if index = 200 then state else
    let state, _ = W.delta state (W.Borrow index) in
    let next, _ = W.delta state (W.Verified (7, index, hash index)) in
    need "proof capacity exceeded" (List.length next.proofs <= W.proof_cap);
    need "proof not retained" (List.mem (hash index) next.proofs);
    need "wrong verification changed proofs" (W.delta state (W.Verified (8, index, hash index)) = (state, []));
    need "wrong request changed proofs" (W.delta state (W.Verified (7, index + 1, hash index)) = (state, []));
    fill next (index + 1) in
  let state = fill start 0 in
  need "proof eviction differs" (List.length state.proofs = 64
    && not (List.mem (hash 135) state.proofs) && List.mem (hash 136) state.proofs);
  let lent, _ = W.delta {state with total = 16} (W.Borrow 201) in
  let next, _ = W.delta lent (W.Verified (7, 201, hash 135)) in
  need "evicted proof fresh work did not reset" (next.total = 0 && List.length next.proofs = 64);
  let state, _ = W.delta state (W.Borrow 201) in
  let next, _ = W.delta state (W.Verified (7, 201, hash 136)) in
  need "proof duplicate increased memory" (List.length next.proofs = 64
    && List.hd next.proofs = hash 136);
  need "invalid digest retained" (W.delta state (W.Verified (7, 201, "x")) = (state, []));
  let closed, _ = W.delta next W.Stop in
  need "closed proofs retained" (closed.proofs = []);
  let first = P.Proof_key (P.Math (P.Groth16 {key = "a"; proof = "b"; inputs = "c"})) in
  let others = [P.Math (P.Groth16 {key = "a"; proof = "b"; inputs = "c"});
    P.Proof_key (P.Groth16 {key = "a"; proof = "b"; inputs = "c"});
    P.Proof_key (P.Math (P.Groth16 {key = "a"; proof = "b"; inputs = "d"}))] in
  List.iter (fun other -> need "proof digest lost rules or input"
    (P.request_hash first <> P.request_hash other)) others

let wait_ready owner =
  let deadline = F.now () +. 120. in
  let rec loop () = match K.phase owner with
    | W.Ready _ -> ()
    | W.Closed -> failwith "proof actor closed"
    | _ when F.now () >= deadline -> failwith "proof actor not ready"
    | _ -> Thread.delay 0.01; loop () in
  loop ()

let process worker =
  let stop = Atomic.make false in
  let link = L.spawn worker in
  let parent = link.pid in
  Fun.protect ~finally:(fun () -> L.retire link) (fun () ->
    let peak = Atomic.make 0 in
    let samples = Atomic.make 0 in
    let sampling = Atomic.make true in
    let observer = Thread.create (fun () ->
      while Atomic.get sampling do
        let memory = F.rss parent in
        if memory >= 0 then begin
          Atomic.set peak (max (Atomic.get peak) memory);
          Atomic.incr samples
        end;
        Thread.delay 0.005
      done) () in
    let ready = Fun.protect ~finally:(fun () -> Atomic.set sampling false; Thread.join observer)
      (fun () -> L.ready ~stop link) in
    need "proof parent not ready" ready;
    need "proof startup sampling absent" (Atomic.get samples > 0 && Atomic.get peak > 0);
    need "proof startup reserve exceeded" (Atomic.get peak <= K.reserve_mb);
    Printf.printf "event = proof_start sampled_peak_mb = %d samples = %d reserve_mb = %d\n%!"
      (Atomic.get peak) (Atomic.get samples) K.reserve_mb;
    need "parent not counted" (List.mem parent (F.children (Unix.getpid ())));
    let resident = F.rss parent in
    need "parent memory missing" (resident > 0 && resident < K.reserve_mb);
    Printf.printf "event = proof_parent rss_mb = %d reserve_mb = %d\n%!" resident K.reserve_mb;
    let raw = P.request_bytes P.Ping in
    List.iter (fun id ->
      match L.run ~stop ~control:(Atomic.make L.Continue) ~deadline:(F.now () +. 5.)
        ~memory:1024 ~id link raw with
      | L.Checked (reply, _, _) -> need "ping refused" (reply.accepted && reply.request_hash = P.hash_bytes raw)
      | _ -> failwith "persistent check failed") [1; 2; 3; 4];
    need "parent memory retained input" (F.rss parent <= resident + 8);
    need "cancel ignored" (L.run ~stop ~control:(Atomic.make L.Cancel) ~deadline:(F.now () +. 5.)
      ~memory:1024 ~id:5 link raw = L.Cancelled));
  let reaped = try ignore (Unix.waitpid [Unix.WNOHANG] parent); false
    with Unix.Unix_error (Unix.ECHILD, _, _) -> true in
  need "proof parent not reaped" reaped;
  let link = L.spawn worker in
  Fun.protect ~finally:(fun () -> L.retire link) (fun () ->
    need "proof memory parent not ready" (L.ready ~stop link);
    need "process memory ignored" (L.run ~stop ~control:(Atomic.make L.Continue)
      ~deadline:(F.now () +. 5.) ~memory:64 ~id:1 link (P.request_bytes P.Ping) = L.Retry));
  let owner = K.create ~memory:K.min_memory_mb worker in
  Fun.protect ~finally:(fun () -> K.stop owner) (fun () ->
    wait_ready owner;
    let check request = K.run owner ~control:(Atomic.make L.Continue) ~deadline:(F.now () +. 5.) request in
    (match check P.Ping with K.Accepted reply -> need "actor ping refused" reply.accepted
    | _ -> failwith "actor reply missing");
    let invalid = P.Groth16 {key = ""; proof = ""; inputs = ""} in
    (match check invalid with
    | K.Recheck (Some token) ->
      need "recheck state absent" (K.phase owner = W.Rechecking (token.generation, token.id));
      K.settle owner token W.Agreed;
      wait_ready owner;
      need "duplicate settle changed state" (K.phase owner = W.Ready token.generation)
    | _ -> failwith "negative proof not rechecked"));
  need "actor survived stop" (K.phase owner = W.Closed)

let partial_cancel worker =
  let link = L.spawn worker in
  let stop = Atomic.make false in
  Fun.protect ~finally:(fun () -> L.retire link) (fun () ->
    need "cancel parent not ready" (L.ready ~stop link);
    F.write link.input (F.command_bytes (F.Check (11, P.max_request_bytes, 1024)));
    let receive () =
      let deadline = F.now () +. 5. in
      let rec loop parser =
        need "cancel reply timed out" (F.now () < deadline);
        let ready, _, _ = Unix.select [link.output] [] [] 0.01 in
        if ready = [] then loop parser else
        let parser, reply = F.receive link.output parser in
        match reply with Some reply -> reply | None -> loop parser in
      loop (F.reader ()) in
    let child = match receive () with F.Started (11, pid) -> pid
      | _ -> failwith "cancel child missing" in
    need "checking child not counted" (F.children link.pid = [child]);
    if Sys.file_exists "/proc/self/oom_score_adj" then begin
      let priority pid = In_channel.with_open_text (Printf.sprintf "/proc/%d/oom_score_adj" pid)
        (fun channel -> In_channel.input_line channel |> Option.get |> int_of_string) in
      need "oom control lacks parent contrast" (priority link.pid < 1000);
      let expires = F.now () +. 2. in
      let rec await () =
        if priority child = 1000 then ()
        else if F.now () >= expires then failwith "proof child oom priority missing"
        else (Thread.delay 0.005; await ()) in
      await ();
      Printf.printf "event = proof_oom parent = %d child = %d\n%!" (priority link.pid) (priority child)
    end;
    F.write link.input (F.command_bytes (F.Abort 11));
    need "partial cancel not acknowledged" (receive () = F.Cancelled (11, false));
    need "cancelled child survived" (try Unix.kill child 0; false
      with Unix.Unix_error (Unix.ESRCH, _, _) -> true))

let fake () =
  F.shed ();
  let await_file path =
    let deadline = F.now () +. 30. in
    while not (Sys.file_exists path) do
      if F.now () >= deadline then Unix._exit 2;
      Unix.sleepf 0.005
    done in
  let mark path = Out_channel.with_open_text path (fun stream -> output_string stream "ready") in
  (match Sys.getenv_opt "OCTRA_SESSION_READY_FILE" with
  | Some path when path <> "" ->
    await_file path;
    if Sys.getenv_opt "OCTRA_SESSION_TERMINAL" = Some "1" then begin
      F.write Unix.stdout (F.pack [24]);
      mark (path ^ ".prefix");
      await_file (path ^ ".body");
      F.write Unix.stdout (F.reply_bytes (F.Fault 1));
      mark (path ^ ".sent");
      await_file (path ^ ".exit");
      Unix._exit 0
    end
  | _ -> ());
  F.send Unix.stdout F.Ready;
  let rec loop () = match F.command (F.read Unix.stdin 32) with
  | F.Check (id, length, _) ->
    F.send Unix.stdout (F.Started (id, Unix.getpid ()));
    let raw = F.read Unix.stderr length in
    let mode = Sys.getenv "OCTRA_SESSION_CASE" in
    let crash = match P.request_of_string raw with
      | Ok (P.Groth16 {key = "crash"; _}) -> true | _ -> false in
    if mode = "crash" || (mode = "recovery" && crash) then Unix._exit 2;
    if mode = "frame" then F.write Unix.stdout (F.pack [F.limit + 1])
    else
      F.send Unix.stdout (F.Answer ((if mode = "id" then id + 1 else id),
        (if List.mem mode ["refused"; "conflict"; "recovery"] then 1 else 0),
        (if List.mem mode ["refused"; "conflict"; "count"] then 1 else 0),
        P.response_bytes P.{request_hash = (if mode = "hash" then String.make 64 '0' else P.hash_bytes raw);
          accepted = not (List.mem mode ["refused"; "recheck"]);
          reason = if List.mem mode ["refused"; "recheck"] then "proof rejected" else ""}));
    loop ()
  | F.Stop -> ()
  | _ -> Unix._exit 2 in
  loop ()

let protocol_faults () =
  List.iter (fun mode ->
    Unix.putenv "OCTRA_SESSION_CASE" mode;
    let link = L.spawn Sys.executable_name in
    let stop = Atomic.make false in
    Fun.protect ~finally:(fun () -> L.retire link) (fun () ->
      need "fault worker not ready" (L.ready ~stop link);
      need ("invalid reply accepted: " ^ mode)
        (L.run ~stop ~control:(Atomic.make L.Continue) ~deadline:(F.now () +. 5.)
          ~memory:1024 ~id:1 link (P.request_bytes P.Ping) = L.Retry)))
    ["hash"; "id"; "frame"; "crash"; "conflict"; "count"];
  Unix.putenv "OCTRA_SESSION_CASE" "refused";
  let owner = K.create ~memory:K.min_memory_mb Sys.executable_name in
  Fun.protect ~finally:(fun () -> K.stop owner) (fun () ->
    wait_ready owner;
    List.iter (fun _ ->
      match K.run owner ~control:(Atomic.make L.Continue) ~deadline:(F.now () +. 5.) P.Ping with
      | K.Accepted reply -> need "refused equation accepted" (not reply.accepted)
      | _ -> failwith "refused equation repeated") [1; 2];
    let stats = K.stats owner in
    need "refused equation was retained" (stats.refused = 2 && stats.reused = 0 && stats.accepted = 0))

let cold_memory () =
  let initial = F.rss (Unix.getpid ()) in
  need "cold RSS unavailable" (initial > 0);
  let bytes = Bytes.make ((max 80 (initial + 32) - initial) * 1024 * 1024) '\001' in
  let resident = F.rss (Unix.getpid ()) in
  need "cold allocation absent" (resident > 64);
  Out_channel.with_open_gen [Open_wronly; Open_append; Open_text] 0o600
    (Sys.getenv "OCTRA_SESSION_COLD_FILE")
    (fun stream -> Printf.fprintf stream "\n%d\n" resident);
  Unix.sleepf 0.5;
  ignore (Sys.opaque_identity bytes)

let cold_sample () =
  let raw = C.Fhe_calc.read_request () in
  let request = P.request_of_string raw |> Result.get_ok in
  let channel = open_out (Sys.getenv "OCTRA_SESSION_COLD_FILE") in
  Fun.protect ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel (string_of_int (Unix.getpid ())));
  match Sys.getenv "OCTRA_SESSION_COLD" with
  | "cancel" -> Unix.sleepf 10.
  | "failed" -> Unix._exit 2
  | mode ->
    if mode = "memory" then cold_memory ();
    P.response_bytes P.{request_hash = P.request_hash request;
      accepted = mode = "disagree" || mode = "memory";
      reason = if mode = "disagree" || mode = "memory" then "" else "proof rejected"}
    |> print_endline

let cold_recovery () = Test_workspace.with_dir "proof_recovery" (fun path ->
  let module Worker = C.Pvac_verify_worker in
  let settings = ["OCTRA_SESSION_CASE", "recovery"; "OCTRA_SESSION_COLD", "disagree";
    "OCTRA_SESSION_READY_FILE", "";
    "OCTRA_SESSION_TERMINAL", "";
    "OCTRA_SESSION_COLD_FILE", Filename.concat path "child"] in
  let saved = List.map (fun (key, _) -> key, Option.value ~default:"" (Sys.getenv_opt key)) settings in
  List.iter (fun (key, value) -> Unix.putenv key value) settings;
  Fun.protect ~finally:(fun () -> List.iter (fun (key, value) -> Unix.putenv key value) saved) (fun () ->
    let clock = Atomic.make (F.now ()) in
    let waiter = Atomic.make None in
    let entered = Atomic.make false in
    let barrier = Atomic.make 0 in
    let paused = Atomic.make false in
    let resume = Atomic.make false in
    let owner = K.create ~now:(fun () ->
      let now = Atomic.get clock in
      if Atomic.get waiter = Some (Thread.id (Thread.self ())) then Atomic.set entered true
      else if Atomic.get barrier > 0 then begin
        let remaining = Atomic.get barrier - 1 in
        Atomic.set barrier remaining;
        if remaining = 0 then begin
          Atomic.set paused true;
          let expires = F.now () +. 5. in
          while not (Atomic.get resume) do
            need "terminal clock handshake timed out" (F.now () < expires);
            Thread.delay 0.001
          done
        end
      end;
      now) ~memory:K.min_memory_mb Sys.executable_name in
    Fun.protect ~finally:(fun () -> K.stop owner) (fun () ->
      let await condition =
        let expires = F.now () +. 5. in
        let rec loop () = if condition () then () else
          (need "recovery actor did not advance" (F.now () < expires); Thread.delay 0.005; loop ()) in
        loop () in
      let ready () = await (fun () -> match K.phase owner with W.Ready _ -> true | _ -> false) in
      let dormant () =
        await (fun () -> match K.phase owner with W.Dormant _ -> true | _ -> false);
        match K.phase owner with W.Dormant until -> until | _ -> assert false in
      let request key = P.Groth16 {key; proof = "proof"; inputs = "inputs"} in
      let check request = K.run owner ~control:(Atomic.make L.Continue)
        ~deadline:(Atomic.get clock +. 5.) request in
      let accepted request = match check request with
        | K.Accepted reply -> need "recovery proof refused" reply.accepted
        | _ -> failwith "recovery proof missing" in
      let ticket request =
        let result = ref None in
        await (fun () -> match check request with
          | K.Recovery ticket -> result := Some ticket; true
          | K.Recheck None -> false
          | _ -> failwith "recovery ticket outcome differs");
        Option.get !result in
      let cached = request "cached" in
      ready ();
      accepted cached;
      for index = 0 to 16 do
        accepted cached;
        Atomic.set clock (Atomic.get clock +. 601.);
        need "failed parent did not request cold proof" (check (request "crash") = K.Recheck None);
        if index < 16 then ready ()
      done;
      let until = dormant () in
      need "dormant parent retained" ((K.stats owner).parent = None);
      accepted cached;
      need "cached proof was reverified" ((K.stats owner).prepared = 1);
      need "cooldown accepted request" (check (request "early") = K.Recheck None);
      Atomic.set clock (until +. 1.);
      Thread.delay 0.05;
      need "cooldown restarted parent" (K.phase owner = W.Dormant until);
      let cell = P.Circle_cell {pubkey = "key"; cipher = "cipher"; ciphertext_commitment = "commit";
        proof_kind = P.Circle_none; proof = ""; amount_commitment = ""; strict = true} in
      List.iter (fun request -> need "non-proof issued recovery ticket" (check request = K.Recheck None))
        [P.Ping; P.Proof_key (P.Math P.Ping); cell; P.Math (P.Proof_key cell)];
      let proof = request "cold" in
      List.iter (fun mode ->
        let issued = ticket proof in
        need "parallel recovery ticket accepted" (check (request "parallel") = K.Recheck None);
        Unix.putenv "OCTRA_SESSION_COLD" mode;
        let outcome = Worker.run_recovery owner ~control:(Atomic.make Worker.Continue)
          ~memory:(K.min_memory_mb - K.reserve_mb) Sys.executable_name proof issued in
        need "unsuccessful cold result changed" (match mode, outcome with
          | "agree", Worker.Completed reply -> not reply.accepted
          | "failed", Worker.Failed _ -> true | _ -> false)) ["agree"; "failed"];
      let issued = ticket proof in
      let outcome = Worker.run_recovery owner ~control:(Atomic.make Worker.Cancel)
        ~memory:(K.min_memory_mb - K.reserve_mb) Sys.executable_name proof issued in
      need "cancelled recovery accepted" (outcome = Worker.Unavailable "worker_cancelled");
      let issued = ticket proof in
      Atomic.set clock issued.deadline;
      K.recover owner issued W.Cold_accepted;
      let issued = ticket proof in
      K.recover owner {issued with hash = String.make 64 '0'} W.Cold_accepted;
      Unix.putenv "OCTRA_SESSION_COLD" "disagree";
      let run issued = Worker.run_recovery owner ~control:(Atomic.make Worker.Continue)
        ~memory:(K.min_memory_mb - K.reserve_mb) Sys.executable_name proof issued in
      let wait_probe mode issued =
        let terminal = match mode with `Cancel | `Expire -> false | _ -> true in
        let rec attempt remaining (issued : K.recovery) =
          let gate = Filename.concat path ("ready_" ^ string_of_int issued.id) in
          let mark path = Out_channel.with_open_text path (fun stream -> output_string stream "ready") in
          let release () =
            Atomic.set resume true;
            Atomic.set barrier 0;
            List.iter mark [gate; gate ^ ".body"; gate ^ ".exit"];
            Unix.putenv "OCTRA_SESSION_READY_FILE" "";
            Unix.putenv "OCTRA_SESSION_TERMINAL" "" in
          Unix.putenv "OCTRA_SESSION_READY_FILE" gate;
          Unix.putenv "OCTRA_SESSION_TERMINAL" (if terminal then "1" else "");
          let thread = ref None in
          let captured = Fun.protect ~finally:(fun () ->
            release (); Option.iter Thread.join !thread; Atomic.set waiter None) (fun () ->
            need "cold recovery result missing"
              (match run issued with Worker.Completed reply -> reply.accepted | _ -> false);
            let before = K.stats owner in
            let result = Atomic.make None in
            let control = Atomic.make L.Continue in
            let deadline = Atomic.get clock +. 2. in
            Atomic.set entered false;
            thread := Some (Thread.create (fun () ->
              Atomic.set waiter (Some (Thread.id (Thread.self ())));
              Atomic.set result (Some (try Ok (K.run owner ~control ~deadline cached)
                with error -> Error error))) ());
            await (fun () -> Atomic.get entered || Atomic.get result <> None);
            await (fun () -> K.phase owner = W.Warming (issued.generation + 1)
              && (K.stats owner).parent <> None);
            if not (Atomic.get entered) then begin
              need "warming request returned unexpected result" (Atomic.get result = Some (Ok (K.Recheck None)));
              false
            end else begin
              let parent = (K.stats owner).parent in
              if terminal then begin
                mark gate;
                await (fun () -> Sys.file_exists (gate ^ ".prefix"));
                Atomic.set paused false;
                Atomic.set resume false;
                Atomic.set barrier 2;
                await (fun () -> Atomic.get paused)
              end;
              (match mode with
              | `Cancel | `Fail_cancel -> Atomic.set control L.Cancel
              | `Expire | `Fail_expire -> Atomic.set clock deadline
              | `Fail_open -> ()
              | `Stop -> K.request_stop owner);
              if terminal then begin
                mark (gate ^ ".body");
                await (fun () -> Sys.file_exists (gate ^ ".sent"));
                Atomic.set resume true
              end;
              let expires = F.now () +. 1. in
              let rec wait () = match Atomic.get result with
                | Some result -> result
                | None ->
                  need (if terminal then "terminal warming waiter retained until retire"
                    else "warming retained cancelled or expired waiter") (F.now () < expires);
                  Thread.delay 0.005; wait () in
              let result = wait () in
              need (if terminal then "terminal warming waiter outcome differs" else "warming waiter outcome differs")
                (result = Ok (match mode with
                  | `Cancel | `Fail_cancel | `Stop -> K.Cancelled
                  | `Expire | `Fail_expire -> K.Timed_out
                  | `Fail_open -> K.Recheck None));
              let after = K.stats owner in
              need (if terminal then "terminal waiter waited for parent reap" else "waiter interrupted independent preparation")
                (K.phase owner = (if mode = `Stop then W.Closed
                  else if terminal then W.Absent else W.Warming (issued.generation + 1)) && after.parent = parent);
              need "waiting cached proof was dispatched"
                (after.accepted = before.accepted && after.prepared = before.prepared && after.reused = before.reused);
              Unix.kill (Option.get parent) 0;
              release ();
              if terminal then begin
                await (fun () -> (K.stats owner).parent = None);
                need "terminal parent not reaped" (try ignore (Unix.waitpid [Unix.WNOHANG] (Option.get parent)); false
                  with Unix.Unix_error (Unix.ECHILD, _, _) -> true)
              end else begin
                ready ();
                need "waiter replaced preparation parent" ((K.stats owner).parent = parent);
                accepted cached
              end;
              true
            end) in
          if captured then issued else begin
            need "recovery waiter interleaving not reached" (remaining > 1);
            if not terminal then begin
              ready ();
              need "missed probe did not fail" (check (request "crash") = K.Recheck None)
            end;
            Atomic.set clock (dormant () +. 1.);
            attempt (remaining - 1) (ticket proof)
          end in
        attempt 8 issued in
      let issued = wait_probe `Cancel issued in
      need "recovery generation differs" (K.phase owner = W.Ready (issued.generation + 1));
      K.recover owner issued W.Cold_accepted;
      accepted cached;
      need "recovery reverified cached proof" ((K.stats owner).prepared = 1);
      need "probe did not fail" (check (request "crash") = K.Recheck None);
      let until = dormant () in
      Atomic.set clock (until +. 1.);
      Thread.delay 0.05;
      need "probe granted automatic burst" (K.phase owner = W.Dormant until);
      let issued = ticket proof in
      ignore (wait_probe `Expire issued);
      accepted (request "fresh");
      need "fresh warm work absent" ((K.stats owner).prepared = 2);
      need "fresh parent did not fail" (check (request "crash") = K.Recheck None);
      ready ();
      for _index = 1 to 16 do
        Atomic.set clock (Atomic.get clock +. 601.);
        need "terminal setup parent did not fail" (check (request "crash") = K.Recheck None);
        await (fun () -> match K.phase owner with W.Ready _ | W.Dormant _ -> true | _ -> false)
      done;
      List.iter (fun mode ->
        Atomic.set clock (dormant () +. 1.);
        ignore (wait_probe mode (ticket proof))) [`Fail_cancel; `Fail_expire; `Fail_open; `Stop])))

let cold_route path ~memory ~mode ~accepted =
  let module Worker = C.Pvac_verify_worker in
  Unix.putenv "OCTRA_PVAC_VERIFY_MAX_RSS_MB" (string_of_int memory);
  Unix.putenv "OCTRA_SESSION_COLD" mode;
  if Sys.file_exists path then Unix.unlink path;
  let result = Worker.run_unmanaged P.Ping in
  need "cold route not executed" (Sys.file_exists path);
  need "cold memory outcome differs" (match result with
    | Worker.Completed reply -> accepted && reply.accepted
        && reply.request_hash = P.request_hash P.Ping
    | Worker.Memory_exceeded -> not accepted
    | _ -> false)

let cold_rss path = In_channel.with_open_text path (fun stream ->
  ignore (input_line stream);
  input_line stream |> int_of_string)

let memory_case path warm =
  let module Worker = C.Pvac_verify_worker in
  let memory = if warm then K.min_memory_mb else K.min_memory_mb - 1 in
  List.iter (fun (key, value) -> Unix.putenv key value)
    ["OCTRA_SESSION_CASE", "refused"; "OCTRA_SESSION_COLD", "disagree";
     "OCTRA_SESSION_COLD_FILE", path; "OCTRA_SESSION_READY_FILE", "";
     "OCTRA_SESSION_TERMINAL", ""; "OCTRA_PVAC_VERIFY_WORKER", Sys.executable_name;
     "OCTRA_PVAC_VERIFY_MAX_RSS_MB", string_of_int memory;
     "OCTRA_PVAC_VERIFY_TIMEOUT_SEC", "5"];
  if not warm then need "small shared session created"
    (match K.shared ~memory Sys.executable_name with
    | None -> true | Some owner -> K.stop owner; false);
  Worker.start_prepared ();
  let owner = K.current Sys.executable_name in
  need "memory selected wrong session" (Option.is_some owner = warm);
  Fun.protect ~finally:(fun () -> Option.iter K.stop owner) (fun () ->
    if warm then begin
      let owner = Option.get owner in
      wait_ready owner;
      need "minimum memory skipped warm route"
        (match Worker.run_unmanaged P.Ping with
        | Worker.Completed reply -> not reply.accepted
        | _ -> false);
      need "minimum memory used cold route" (not (Sys.file_exists path));
      need "warm refusal absent" ((K.stats owner).refused = 1)
    end;
    cold_route path ~memory:(K.min_memory_mb - 1) ~mode:"disagree" ~accepted:true;
    cold_route path ~memory:(K.min_memory_mb - 1) ~mode:"memory" ~accepted:true;
    let resident = cold_rss path in
    if warm then begin
      cold_route path ~memory:(K.reserve_mb + max 64 (resident - 16))
        ~mode:"memory" ~accepted:false;
      cold_route path ~memory:(K.reserve_mb + resident + 16) ~mode:"memory" ~accepted:true;
      need "small memory borrowed warm session" ((K.stats (Option.get owner)).refused = 1)
    end else
      cold_route path ~memory:(resident + 16) ~mode:"memory" ~accepted:true)

let memory_selection () = Test_workspace.with_dir "proof_memory" (fun path ->
  List.iter (fun warm ->
    let child = Unix.fork () in
    if child = 0 then begin
      (try memory_case (Filename.concat path (if warm then "warm" else "cold")) warm
        with error -> prerr_endline (Printexc.to_string error); Unix._exit 2);
      Unix._exit 0
    end;
    need "memory route control failed" (snd (Unix.waitpid [] child) = Unix.WEXITED 0))
    [false; true])

let cold_settlement () =
  let module Worker = C.Pvac_verify_worker in
  let settings = ["OCTRA_SESSION_CASE", "recheck";
    "OCTRA_PVAC_VERIFY_WORKER", Sys.executable_name;
    "OCTRA_PVAC_VERIFY_MAX_RSS_MB", string_of_int K.min_memory_mb;
    "OCTRA_PVAC_VERIFY_TIMEOUT_SEC", "5"] in
  let saved = List.map (fun (key, _) -> key, Option.value ~default:"" (Sys.getenv_opt key))
    (settings @ ["OCTRA_SESSION_COLD", ""; "OCTRA_SESSION_COLD_FILE", ""]) in
  List.iter (fun (key, value) -> Unix.putenv key value) settings;
  Fun.protect ~finally:(fun () -> List.iter (fun (key, value) -> Unix.putenv key value) saved) (fun () ->
    let owner = K.shared ~memory:K.min_memory_mb Sys.executable_name |> Option.get in
    Fun.protect ~finally:(fun () -> K.stop owner) (fun () ->
      wait_ready owner;
      let parent = (K.stats owner).parent in
      List.iter (fun mode -> Test_workspace.with_dir "proof_settle" (fun path ->
        let marker = Filename.concat path "child" in
        Unix.putenv "OCTRA_SESSION_COLD" mode;
        Unix.putenv "OCTRA_SESSION_COLD_FILE" marker;
        let control = Atomic.make Worker.Continue in
        let observer = if mode <> "cancel" then None else Some (Thread.create (fun () ->
          let expires = F.now () +. 5. in
          let rec wait () =
            if Sys.file_exists marker || F.now () >= expires then Atomic.set control Worker.Cancel
            else (Thread.delay 0.005; wait ()) in wait ()) ()) in
        let result = Fun.protect ~finally:(fun () -> Option.iter Thread.join observer)
          (fun () -> Worker.run_unmanaged ~control P.Ping) in
        need "cold confirmation absent" (Sys.file_exists marker);
        let channel = open_in marker in
        let child = Fun.protect ~finally:(fun () -> close_in channel)
          (fun () -> input_line channel |> int_of_string) in
        need "cold child not reaped" (try ignore (Unix.waitpid [Unix.WNOHANG] child); false
          with Unix.Unix_error (Unix.ECHILD, _, _) -> true);
        need "cold outcome changed" (match mode, result with
          | "cancel", Worker.Unavailable "worker_cancelled" | "failed", Worker.Failed _ -> true
          | "agree", Worker.Completed reply -> not reply.accepted
          | "disagree", Worker.Completed reply -> reply.accepted
          | _ -> false);
        if mode = "disagree" then begin
          let expires = F.now () +. 5. in
          let rec wait () = if K.phase owner = W.Closed then () else
            (need "completed disagreement retained worker" (F.now () < expires);
              Thread.delay 0.005; wait ()) in wait ()
        end else begin
          wait_ready owner;
          need "incomplete confirmation restarted parent" ((K.stats owner).parent = parent);
          need "incomplete confirmation cached proof" ((K.stats owner).reused = 0)
        end)) ["cancel"; "failed"; "agree"; "disagree"];
      need "cold confirmations skipped warm path" ((K.stats owner).rechecked = 4)))

let () =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--proof-session" then fake ()
  else if Sys.getenv_opt "OCTRA_SESSION_COLD" |> Option.fold ~none:false ~some:((<>) "") then cold_sample ()
  else begin
    pipe_signal ();
    decisions ();
    recovery_controls ();
    remembered ();
    need "worker path missing" (Array.length Sys.argv = 2);
    process Sys.argv.(1);
    partial_cancel Sys.argv.(1);
    protocol_faults ();
    cold_recovery ();
    memory_selection ();
    cold_settlement ();
    print_endline "event = proof_session status = passed"
  end