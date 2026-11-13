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

type reservation_state = Available | Borrowed | Closing | Released
type reservation_message = Borrow | Settle | Close
type reservation_effect = Grant | Deny | Free

let reservation_step state message = match state, message with
  | Available, Borrow -> Borrowed, [Grant]
  | _, Borrow -> state, [Deny]
  | Borrowed, Settle -> Available, []
  | Closing, Settle | Available, Close -> Released, [Free]
  | Borrowed, Close -> Closing, []
  | (Available | Released), Settle | (Closing | Released), Close -> state, []

type waiter = {
  id : int;
  priority : priority;
  kind : waiter_kind;
  bytes : int option;
  mutable charge : Compute_memory.ticket option;
  reservation : reservation option;
  mutable state : waiter_state;
}

and reservation = {
  owner : t;
  size : int;
  ticket : Compute_memory.ticket;
  mutable usage : reservation_state;
}

and t = {
  capacity : int;
  required_limit : int;
  reserved_limit : int;
  mutable reservations : int;
  speculative_limit : int;
  required_burst : int;
  memory : Compute_memory.t;
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
    ?(memory = Compute_memory.shared)
    ?(reserved_limit = 0)
    ~capacity
    ~required_limit
    ~speculative_limit
    ~required_burst
    () =
  if capacity < 1 then invalid_arg "compute pool capacity must be positive";
  if required_limit < 1 then
    invalid_arg "compute pool required limit must be positive";
  if reserved_limit < 0 || reserved_limit > required_limit then
    invalid_arg "compute pool reserve differs";
  if speculative_limit < 1 then
    invalid_arg "compute pool speculative limit must be positive";
  if required_burst < 1 then
    invalid_arg "compute pool required burst must be positive";
  {
    capacity;
    required_limit;
    reserved_limit;
    reservations = 0;
    speculative_limit;
    required_burst;
    memory;
    mutex = Mutex.create ();
    active = 0;
    next_id = 0;
    required_streak = 0;
    required_waiters = [];
    speculative_waiters = [];
  }

let capacity (t : t) =
  t.capacity

let locked (t : t) action =
  Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock t.mutex) action

let reserve ~memory ~bytes (t : t) = locked t (fun () ->
  if t.reservations >= t.reserved_limit then None
  else match Compute_memory.reserve memory ~speculative:false bytes with
  | None -> None
  | Some ticket ->
    t.reservations <- t.reservations + 1;
    Some {owner = t; size = bytes; ticket; usage = Available})

let change_reservation (reservation : reservation) message =
  let state, effects = reservation_step reservation.usage message in
  reservation.usage <- state;
  List.iter (function
    | Free ->
      reservation.owner.reservations <- reservation.owner.reservations - 1;
      Compute_memory.release reservation.ticket
    | Grant | Deny -> ()) effects;
  effects

let close_reservation (reservation : reservation) = locked reservation.owner (fun () ->
  ignore (change_reservation reservation Close))

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
  let room = match waiter.reservation with
    | Some _ -> waiter.priority = Required && List.length waiters < t.required_limit
    | None when waiter.priority = Required ->
      List.fold_left (fun count (entry : waiter) ->
        count + if Option.is_none entry.reservation then 1 else 0) 0 waiters
        < t.required_limit - t.reserved_limit
    | None -> List.length waiters < limit t waiter.priority in
  if not room then false
  else begin
    let next = waiters @ [waiter] in
    let accepted = match waiter.reservation, waiter.bytes with
      | Some reservation, Some bytes when reservation.owner == t
          && bytes >= 0 && bytes <= reservation.size ->
        change_reservation reservation Borrow = [Grant]
      | Some _, _ -> false
      | None, None -> true
      | None, Some bytes ->
        waiter.charge <- Compute_memory.reserve t.memory
          ~speculative:(waiter.priority = Speculative) bytes;
        Option.is_some waiter.charge in
    if accepted then set_queue t waiter.priority next;
    accepted
  end

let release_memory (waiter : waiter) =
  Option.iter Compute_memory.release waiter.charge;
  waiter.charge <- None;
  Option.iter (fun reservation -> ignore (change_reservation reservation Settle)) waiter.reservation

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
    release_memory waiter;
    if t.active > 0 then t.active <- t.active - 1;
    dispatch_locked t []
  | Waiting
  | Cancelled -> []

let release_main (t : t) waiter =
  let wakes = locked t (fun () -> release_locked t waiter) in
  deliver_main wakes

let release_from_thread (t : t) waiter =
  let wakes = locked t (fun () -> release_locked t waiter) in
  deliver_from_thread wakes

let cancel_async (t : t) waiter =
  let wakes = locked t (fun () ->
    match waiter.state with
    | Waiting ->
      remove_waiter_locked t waiter;
      waiter.state <- Cancelled;
      release_memory waiter;
      dispatch_locked t []
    | Granted -> release_locked t waiter
    | Claimed
    | Cancelled -> []) in
  deliver_main wakes

let claim (t : t) waiter =
  locked t (fun () ->
    match waiter.state with
    | Granted ->
      waiter.state <- Claimed;
      true
    | Waiting
    | Claimed
    | Cancelled -> false)

let fresh_waiter ?reservation (t : t) bytes priority kind =
  let waiter = {
    id = t.next_id;
    priority;
    kind;
    bytes;
    charge = None;
    reservation;
    state = Waiting;
  } in
  t.next_id <- t.next_id + 1;
  waiter

let acquire_async ?reservation (t : t) bytes priority =
  let ready, wake = Lwt.task () in
  let waiter, accepted, wakes = locked t (fun () ->
    let waiter = fresh_waiter ?reservation t bytes priority (Async { ready; wake }) in
    let accepted = enqueue_locked t waiter in
    let wakes = if accepted then dispatch_locked t [] else [] in
    waiter, accepted, wakes) in
  if not accepted then None
  else begin
    Lwt.on_cancel ready (fun () -> cancel_async t waiter);
    deliver_main wakes;
    Some (waiter, ready)
  end

let move_input (t : t) waiter memory = locked t (fun () ->
  match waiter.state, waiter.charge, waiter.bytes with
  | Claimed, Some charge, Some bytes ->
    if memory == t.memory then true else
    begin match Compute_memory.reserve memory ~speculative:(waiter.priority = Speculative) bytes with
    | None -> false
    | Some ticket ->
      waiter.charge <- Some ticket;
      Compute_memory.release charge;
      true
    end
  | _ -> false)

let run_async ?bytes ?reservation ?running_memory (t : t) priority task =
  if Option.is_some running_memory && (Option.is_none bytes || Option.is_some reservation) then
    Lwt.return_none
  else
  match acquire_async ?reservation t bytes priority with
  | None -> Lwt.return_none
  | Some (waiter, ready) ->
    let open Lwt.Syntax in
    let* () = ready in
    if not (claim t waiter) then Lwt.return_none
    else
      let job =
        try
          if Option.fold ~none:true ~some:(move_input t waiter) running_memory then
            Lwt.map Option.some (task ())
          else Lwt.return_none
        with exn -> Lwt.fail exn
      in
      let settled = Lwt.finalize (fun () -> job)
        (fun () -> release_main t waiter; Lwt.return_unit) in
      Lwt.protected settled

let resolve_threaded t waiter promise resolver outcome =
  let wakes = locked t (fun () -> release_locked t waiter) in
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

let run_threaded ?bytes (t : t) priority task value =
  match acquire_async t bytes priority with
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

let acquire_sync (t : t) bytes priority =
  let condition = Condition.create () in
  locked t (fun () ->
  let waiter = fresh_waiter t bytes priority (Sync condition) in
  if not (enqueue_locked t waiter) then None
  else begin
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
    if claimed then Some waiter else None
  end)

let run_sync ?bytes (t : t) priority task =
  match acquire_sync t bytes priority with
  | None -> None
  | Some waiter ->
    Some (Fun.protect ~finally:(fun () -> release_from_thread t waiter) task)

let try_acquire_sync (t : t) bytes priority =
  let condition = Condition.create () in
  let claimed, waiter, wakes = locked t (fun () ->
  let waiter = fresh_waiter t bytes priority (Sync condition) in
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
      release_memory waiter;
      false
    | Claimed
    | Cancelled -> false
  in
  claimed, waiter, wakes) in
  deliver_from_thread wakes;
  if claimed then Some waiter else None

let try_run_sync ?bytes (t : t) priority task =
  match try_acquire_sync t bytes priority with
  | None -> None
  | Some waiter ->
    Some (Fun.protect ~finally:(fun () -> release_from_thread t waiter) task)

let stats (t : t) =
  locked t (fun () -> {
    active = t.active;
    required_waiting = List.length t.required_waiters;
    speculative_waiting = List.length t.speculative_waiters;
    required_streak = t.required_streak;
  })