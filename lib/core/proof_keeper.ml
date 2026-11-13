(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module W = Proof_warm
module L = Proof_link
module F = Proof_frame
module P = Pvac_verify_protocol

type token = {generation : int; id : int}
type recovery = W.recovery_ticket
type outcome = Accepted of P.response | Recheck of token option | Recovery of recovery | Cancelled | Timed_out
type job = {id : int; raw : string; control : L.control Atomic.t; started : float; deadline : float;
  proof : bool; result : outcome option Atomic.t}
type stats = {parent : int option; accepted : int; prepared : int; rechecked : int; reused : int; refused : int}
type t = {program : string; memory : int; stamp : Unix.stats; lock : Mutex.t; now : unit -> float;
  stop : bool Atomic.t; mutable phase : W.phase; mutable next : int;
  mutable pending : job option; mutable settled : (token * W.confirmation) option;
  mutable recovery : recovery option; mutable recovered : (recovery * W.recovery_result) option;
  mutable thread : Thread.t option; mutable stats : stats}

let reserve_mb = L.reserve_mb
let min_memory_mb = 4 * reserve_mb
let locked owner action =
  Mutex.lock owner.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock owner.lock) action

let same_file left right =
  left.Unix.st_dev = right.Unix.st_dev && left.st_ino = right.st_ino
  && left.st_size = right.st_size && left.st_mtime = right.st_mtime
  && left.st_ctime = right.st_ctime

let unchanged owner =
  try same_file owner.stamp (Unix.stat owner.program) with _ -> false

let rec proof_request = function
  | P.Math request | P.Proof_key request -> proof_request request
  | P.Ping | P.Circle_cell {proof_kind = P.Circle_none; _} -> false
  | P.Groth16 _ | P.Encrypt _ | P.Claim _ | P.Key_switch_claim _ | P.Stored_key_claim _
  | P.Historical_migration_claim _ | P.Range _ | P.Zero _ | P.Range_bound _ | P.Circle_cell _ -> true

let waiting_result ?(retiring = false) phase ~control ~now ~deadline = match phase with
  | W.Lent _ | W.Rechecking _ -> None
  | _ when retiring -> Some (if control = L.Cancel then Cancelled
      else if now >= deadline then Timed_out else Recheck None)
  | W.Warming _ when control = L.Cancel -> Some Cancelled
  | W.Warming _ when now >= deadline -> Some Timed_out
  | _ -> None

let serve owner =
  let session = ref None in
  let retire () =
    let prior = !session in
    session := None;
    Option.iter L.retire prior;
    locked owner (fun () -> owner.stats <- {owner.stats with parent = None}) in
  let complete_waiting phase retiring =
    match phase, owner.pending with
    | (W.Lent _ | W.Rechecking _), _ | _, None -> ()
    | _, Some job ->
      let control = if Atomic.get owner.stop then L.Cancel else Atomic.get job.control in
      match waiting_result ~retiring phase ~control ~now:(owner.now ()) ~deadline:job.deadline with
      | None -> ()
      | Some outcome -> owner.pending <- None; Atomic.set job.result (Some outcome) in
  let publish state effects =
    let phase = owner.phase in
    owner.phase <- state.W.phase;
    owner.recovery <- state.W.recovery;
    complete_waiting phase (List.mem W.Retire effects) in
  let complete job outcome = locked owner (fun () ->
    owner.pending <- None;
    Atomic.set job.result (Some outcome)) in
  let change state message =
    let state, effects = W.delta state message in
    locked owner (fun () -> publish state effects);
    List.iter (function W.Retire -> retire () | _ -> ()) effects;
    state, effects in
  let rec loop state =
    if Atomic.get owner.stop || not (unchanged owner) then ignore (change state W.Stop)
    else
      let state, effects = locked owner (fun () ->
        let settled = Option.map (fun (token, result) -> token.generation, token.id, result)
          owner.settled in
        let recovered = owner.recovered in
        owner.settled <- None;
        owner.recovered <- None;
        let state, effects = W.poll ?recovered state (owner.now ()) settled in
        publish state effects;
        state, effects) in
      List.iter (function W.Retire -> retire () | _ -> ()) effects;
      match effects with
      | [W.Spawn generation] ->
        let ready = try
          let link = L.spawn owner.program in
          session := Some link;
          locked owner (fun () -> owner.stats <- {owner.stats with parent = Some link.pid});
          let deadline = F.now () +. 120. in
          let rec warm parser =
            locked owner (fun () -> complete_waiting owner.phase false);
            match L.ready_step ~stop:owner.stop ~deadline link parser with
            | L.Waiting parser -> warm parser
            | L.Prepared -> unchanged owner
            | L.Unprepared -> false in
          warm (F.reader ())
        with _ -> false in
        let message = if Atomic.get owner.stop then W.Stop
          else if ready then W.Warmed generation else W.Failed generation in
        let state, _ = change state message in
        loop state
      | _ when state.phase = W.Closed -> ()
      | _ ->
        match locked owner (fun () -> owner.pending) with
          | Some job when (match state.phase with W.Dormant _ -> true | _ -> false) ->
            let hash = P.hash_bytes job.raw in
            let now = owner.now () in
            if Atomic.get job.control = L.Cancel || Atomic.get owner.stop then
              (complete job Cancelled; loop state)
            else if now >= job.deadline then (complete job Timed_out; loop state)
            else if List.mem hash state.proofs then begin
              locked owner (fun () -> owner.stats <- {owner.stats with
                accepted = owner.stats.accepted + 1; reused = owner.stats.reused + 1});
              complete job (Accepted P.{request_hash = hash; accepted = true; reason = ""});
              loop state
            end else if not job.proof then (complete job (Recheck None); loop state)
            else
              let ticket = W.{generation = state.generation; id = job.id; hash; deadline = job.deadline} in
              let state, effects = change state (W.Demand (job.started, ticket)) in
              complete job (match effects with [W.Recovery ticket] -> Recovery ticket | _ -> Recheck None);
              loop state
          | Some job ->
            let state, effects = change state (W.Borrow job.id) in
            (match effects, !session with
            | [W.Granted (generation, id)], Some link ->
              let hash = P.hash_bytes job.raw in
              let reused = List.mem hash state.proofs in
              let result =
                if Atomic.get job.control = L.Cancel || Atomic.get owner.stop then L.Cancelled
                else if owner.now () >= job.deadline then L.Timed_out
                else if reused then L.Checked (P.{request_hash = hash; accepted = true; reason = ""}, 0, 0)
                else L.run ~stop:owner.stop ~control:job.control ~deadline:job.deadline
                  ~memory:owner.memory ~id link job.raw in
              let message, outcome = match result with
                | L.Checked (response, sums, _) when response.accepted ->
                  locked owner (fun () -> owner.stats <- {owner.stats with
                    accepted = owner.stats.accepted + 1;
                    prepared = owner.stats.prepared + (if sums > 0 then 1 else 0);
                    reused = owner.stats.reused + (if reused then 1 else 0)});
                  (if reused then W.Cached (generation, id, hash)
                   else if sums > 0 && job.proof && response.reason = "" then W.Verified (generation, id, hash)
                   else W.Returned (generation, id, true)), Accepted response
                | L.Checked (response, _, rejected) when rejected > 0 ->
                  locked owner (fun () -> owner.stats <- {owner.stats with refused = owner.stats.refused + 1});
                  W.Returned (generation, id, true), Accepted response
                | L.Checked _ ->
                  locked owner (fun () -> owner.stats <- {owner.stats with rechecked = owner.stats.rechecked + 1});
                  W.Check (generation, id, job.deadline), Recheck (Some {generation; id})
                | L.Cancelled -> W.Returned (generation, id, link.live), Cancelled
                | L.Timed_out -> W.Returned (generation, id, link.live), Timed_out
                | L.Retry -> W.Returned (generation, id, false), Recheck None in
              let state, _ = change state message in
              complete job outcome;
              loop state
            | _ -> complete job (Recheck None); loop state)
          | None ->
            let state = match !session with
              | Some link when F.rss link.pid < 0 || F.rss link.pid > reserve_mb ->
                fst (change state (W.Failed state.generation))
              | _ -> state in
            Thread.delay 0.02;
            loop state in
  Fun.protect ~finally:(fun () ->
    Fun.protect ~finally:(fun () ->
      (try retire () with _ -> ());
      locked owner (fun () ->
        Option.iter (fun job -> Atomic.set job.result (Some
          (if Atomic.get owner.stop || Atomic.get job.control = L.Cancel then Cancelled else Recheck None))) owner.pending;
        owner.pending <- None))
      (fun () -> locked owner (fun () ->
        let phase = owner.phase in
        owner.phase <- W.Closed;
        owner.recovery <- None;
        owner.recovered <- None;
        complete_waiting phase true)))
    (fun () -> try loop W.empty with error ->
      Octra_log.warn "proof" "event = proof_session_failed reason = %s" (Printexc.to_string error))

let create ?(now = F.now) ~memory program =
  if memory < min_memory_mb || memory > 32768 then invalid_arg "proof session memory";
  let owner = {program; memory; stamp = Unix.stat program; lock = Mutex.create (); now;
    stop = Atomic.make false; phase = W.Absent; next = 0; pending = None; settled = None; thread = None;
    recovery = None; recovered = None;
    stats = {parent = None; accepted = 0; prepared = 0; rechecked = 0; reused = 0; refused = 0}} in
  owner.thread <- Some (Thread.create serve owner);
  owner

let request_stop owner = Atomic.set owner.stop true

let stop owner =
  request_stop owner;
  Option.iter Thread.join owner.thread

let phase owner = locked owner (fun () -> owner.phase)
let stats owner = locked owner (fun () -> owner.stats)

let run owner ~control ~deadline request =
  let job = locked owner (fun () ->
    if Atomic.get owner.stop || owner.pending <> None || owner.next = max_int
        || not (Float.is_finite deadline)
        || (match owner.phase with W.Ready _ | W.Dormant _ -> false | _ -> true) then None
    else begin
      owner.next <- owner.next + 1;
      let started = owner.now () in
      let job = {id = owner.next; raw = P.request_bytes request; control; started; deadline;
        proof = proof_request request; result = Atomic.make None} in
      owner.pending <- Some job;
      Some job
    end) in
  match job with
  | None -> Recheck None
  | Some job ->
    let rec wait () = match Atomic.get job.result with
      | Some result -> result
      | None -> Thread.delay 0.002; wait () in
    wait ()

let settle owner token agreed = locked owner (fun () ->
  if owner.phase = W.Rechecking (token.generation, token.id) && owner.settled = None then
    owner.settled <- Some (token, agreed))

let recover owner ticket result = locked owner (fun () ->
  if not (Atomic.get owner.stop) && owner.recovery = Some ticket && owner.recovered = None
      && (match owner.phase with W.Dormant _ -> true | _ -> false) then
    owner.recovered <- Some (ticket, result))

let shared_lock = Mutex.create ()
let shared_owner = ref None

let current program =
  Mutex.lock shared_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock shared_lock) (fun () ->
    match !shared_owner with Some owner when owner.program = program -> Some owner | _ -> None)

let footprint () =
  Mutex.lock shared_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock shared_lock) (fun () ->
    match !shared_owner with
    | Some owner when phase owner <> W.Closed -> reserve_mb
    | _ -> 0)

let processes () =
  Mutex.lock shared_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock shared_lock) (fun () ->
    match !shared_owner with Some owner -> Option.to_list (stats owner).parent | None -> [])

let shared ~memory program =
  Mutex.lock shared_lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock shared_lock) (fun () ->
    match !shared_owner with
    | Some owner when owner.program = program && owner.memory = memory -> Some owner
    | Some _ -> None
    | None ->
      try
        let owner = create ~memory program in
        shared_owner := Some owner;
        at_exit (fun () -> stop owner);
        Some owner
      with _ -> None)