(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type priority =
  | Required
  | Speculative

type waiter_state =
  | Waiting
  | Granted
  | Claimed
  | Cancelled

type async_gate = {
  ready : unit Lwt.t;
  wake : unit Lwt.u;
}

type waiter_kind =
  | Async of async_gate
  | Sync of Condition.t

type waiter = {
  id : int;
  priority : priority;
  kind : waiter_kind;
  mutable state : waiter_state;
}

type t = {
  capacity : int;
  required_limit : int;
  speculative_limit : int;
  required_burst : int;
  mutex : Mutex.t;
  mutable active : int;
  mutable next_id : int;
  mutable required_streak : int;
  mutable required_waiters : waiter list;
  mutable speculative_waiters : waiter list;
}

type stats = {
  active : int;
  required_waiting : int;
  speculative_waiting : int;
  required_streak : int;
}

let create
    ~capacity
    ~required_limit
    ~speculative_limit
    ~required_burst
    () =
  if capacity < 1 then invalid_arg "compute pool capacity must be positive";
  if required_limit < 1 then
    invalid_arg "compute pool required limit must be positive";
  if speculative_limit < 1 then
    invalid_arg "compute pool speculative limit must be positive";
  if required_burst < 1 then
    invalid_arg "compute pool required burst must be positive";
  {
    capacity;
    required_limit;
    speculative_limit;
    required_burst;
    mutex = Mutex.create ();
    active = 0;
    next_id = 0;
    required_streak = 0;
    required_waiters = [];
    speculative_waiters = [];
  }

let capacity (t : t) =
  t.capacity

let queue (t : t) = function
  | Required -> t.required_waiters
  | Speculative -> t.speculative_waiters

let set_queue (t : t) priority waiters =
  match priority with
  | Required -> t.required_waiters <- waiters
  | Speculative -> t.speculative_waiters <- waiters

let limit (t : t) = function
  | Required -> t.required_limit
  | Speculative -> t.speculative_limit

let enqueue_locked (t : t) waiter =
  let waiters = queue t waiter.priority in
  if List.length waiters >= limit t waiter.priority then false
  else begin
    set_queue t waiter.priority (waiters @ [waiter]);
    true
  end

let remove_waiter_locked (t : t) waiter =
  let keep current = current.id <> waiter.id in
  set_queue t waiter.priority (List.filter keep (queue t waiter.priority))

let take_locked (t : t) priority =
  match queue t priority with
  | [] -> None
  | waiter :: rest ->
    set_queue t priority rest;
    Some waiter

let next_priority (t : t) =
  match t.required_waiters, t.speculative_waiters with
  | [], [] -> None
  | _ :: _, [] -> Some Required
  | [], _ :: _ -> Some Speculative
  | _ :: _, _ :: _ when t.required_streak >= t.required_burst ->
    Some Speculative
  | _ :: _, _ :: _ -> Some Required

let note_grant (t : t) = function
  | Required when t.speculative_waiters = [] -> t.required_streak <- 0
  | Required -> t.required_streak <- t.required_streak + 1
  | Speculative -> t.required_streak <- 0

let rec dispatch_locked (t : t) wakes =
  if t.active >= t.capacity then List.rev wakes
  else
    match next_priority t with
    | None -> List.rev wakes
    | Some priority ->
      begin
        match take_locked t priority with
        | None -> dispatch_locked t wakes
        | Some waiter when waiter.state <> Waiting ->
          dispatch_locked t wakes
        | Some waiter ->
          waiter.state <- Granted;
          t.active <- t.active + 1;
          note_grant t priority;
          begin
            match waiter.kind with
            | Sync condition ->
              Condition.signal condition;
              dispatch_locked t wakes
            | Async _ ->
              dispatch_locked t (waiter :: wakes)
          end
      end

let wake_async waiter =
  match waiter.kind with
  | Async gate when waiter.state = Granted && Lwt.is_sleeping gate.ready ->
    begin
      try Lwt.wakeup gate.wake ()
      with Invalid_argument _ -> ()
    end
  | Async _
  | Sync _ -> ()

let deliver_main waiters =
  List.iter wake_async waiters

let deliver_from_thread waiters =
  match waiters with
  | [] -> ()
  | _ ->
    Lwt_preemptive.run_in_main_dont_wait
      (fun () ->
         deliver_main waiters;
         Lwt.return_unit)
      (fun _ -> ())

let release_locked (t : t) waiter =
  match waiter.state with
  | Granted
  | Claimed ->
    waiter.state <- Cancelled;
    if t.active > 0 then t.active <- t.active - 1;
    dispatch_locked t []
  | Waiting
  | Cancelled -> []

let release_main (t : t) waiter =
  Mutex.lock t.mutex;
  let wakes = release_locked t waiter in
  Mutex.unlock t.mutex;
  deliver_main wakes

let release_from_thread (t : t) waiter =
  Mutex.lock t.mutex;
  let wakes = release_locked t waiter in
  Mutex.unlock t.mutex;
  deliver_from_thread wakes

let cancel_async (t : t) waiter =
  Mutex.lock t.mutex;
  let wakes =
    match waiter.state with
    | Waiting ->
      remove_waiter_locked t waiter;
      waiter.state <- Cancelled;
      dispatch_locked t []
    | Granted -> release_locked t waiter
    | Claimed
    | Cancelled -> []
  in
  Mutex.unlock t.mutex;
  deliver_main wakes

let claim (t : t) waiter =
  Mutex.lock t.mutex;
  let claimed =
    match waiter.state with
    | Granted ->
      waiter.state <- Claimed;
      true
    | Waiting
    | Claimed
    | Cancelled -> false
  in
  Mutex.unlock t.mutex;
  claimed

let fresh_waiter (t : t) priority kind =
  let waiter = {
    id = t.next_id;
    priority;
    kind;
    state = Waiting;
  } in
  t.next_id <- t.next_id + 1;
  waiter

let acquire_async (t : t) priority =
  let ready, wake = Lwt.task () in
  Mutex.lock t.mutex;
  let waiter = fresh_waiter t priority (Async { ready; wake }) in
  let accepted = enqueue_locked t waiter in
  let wakes = if accepted then dispatch_locked t [] else [] in
  Mutex.unlock t.mutex;
  if not accepted then None
  else begin
    Lwt.on_cancel ready (fun () -> cancel_async t waiter);
    deliver_main wakes;
    Some (waiter, ready)
  end

let run_async (t : t) priority task =
  match acquire_async t priority with
  | None -> Lwt.return_none
  | Some (waiter, ready) ->
    let open Lwt.Syntax in
    let* () = ready in
    if not (claim t waiter) then Lwt.return_none
    else
      let job =
        try task ()
        with exn -> Lwt.fail exn
      in
      Lwt.on_any
        job
        (fun _ -> release_main t waiter)
        (fun _ -> release_main t waiter);
      let* value = Lwt.protected job in
      Lwt.return_some value

let resolve_threaded t waiter promise resolver outcome =
  Mutex.lock t.mutex;
  let wakes = release_locked t waiter in
  Mutex.unlock t.mutex;
  Lwt_preemptive.run_in_main_dont_wait
    (fun () ->
       deliver_main wakes;
       if Lwt.is_sleeping promise then begin
         match outcome with
         | Ok value -> Lwt.wakeup resolver value
         | Error exn -> Lwt.wakeup_exn resolver exn
       end;
       Lwt.return_unit)
    (fun _ -> ())

let run_threaded (t : t) priority task value =
  match acquire_async t priority with
  | None -> Lwt.return_none
  | Some (waiter, ready) ->
    let open Lwt.Syntax in
    let* () = ready in
    if not (claim t waiter) then Lwt.return_none
    else
      let result, resolver = Lwt.task () in
      let start () =
        let outcome =
          try Ok (task value)
          with exn -> Error exn
        in
        resolve_threaded t waiter result resolver outcome
      in
      begin
        try ignore (Thread.create start ())
        with exn ->
          release_main t waiter;
          Lwt.wakeup_exn resolver exn
      end;
      let* value = Lwt.protected result in
      Lwt.return_some value

let acquire_sync (t : t) priority =
  let condition = Condition.create () in
  Mutex.lock t.mutex;
  let waiter = fresh_waiter t priority (Sync condition) in
  if not (enqueue_locked t waiter) then begin
    Mutex.unlock t.mutex;
    None
  end else begin
    let wakes = dispatch_locked t [] in
    deliver_from_thread wakes;
    while waiter.state = Waiting do
      Condition.wait condition t.mutex
    done;
    let claimed =
      match waiter.state with
      | Granted ->
        waiter.state <- Claimed;
        true
      | Waiting
      | Claimed
      | Cancelled -> false
    in
    Mutex.unlock t.mutex;
    if claimed then Some waiter else None
  end

let run_sync (t : t) priority task =
  match acquire_sync t priority with
  | None -> None
  | Some waiter ->
    Some (Fun.protect ~finally:(fun () -> release_from_thread t waiter) task)

let try_acquire_sync (t : t) priority =
  let condition = Condition.create () in
  Mutex.lock t.mutex;
  let waiter = fresh_waiter t priority (Sync condition) in
  let accepted = enqueue_locked t waiter in
  let wakes = if accepted then dispatch_locked t [] else [] in
  let claimed =
    match waiter.state with
    | Granted ->
      waiter.state <- Claimed;
      true
    | Waiting ->
      remove_waiter_locked t waiter;
      waiter.state <- Cancelled;
      false
    | Claimed
    | Cancelled -> false
  in
  Mutex.unlock t.mutex;
  deliver_from_thread wakes;
  if claimed then Some waiter else None

let try_run_sync (t : t) priority task =
  match try_acquire_sync t priority with
  | None -> None
  | Some waiter ->
    Some (Fun.protect ~finally:(fun () -> release_from_thread t waiter) task)

let stats (t : t) =
  Mutex.lock t.mutex;
  let value = {
    active = t.active;
    required_waiting = List.length t.required_waiters;
    speculative_waiting = List.length t.speculative_waiters;
    required_streak = t.required_streak;
  } in
  Mutex.unlock t.mutex;
  value