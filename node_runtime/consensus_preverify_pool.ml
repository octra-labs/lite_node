(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Availability = Octra_core.Preverify_availability
module Scheduler = Octra_core.Compute_pool
module Transaction = Octra_core.Transaction

type 'prepared binding =
  | Bound of 'prepared
  | Source_changed
  | Source_invalid of string

type 'artifact verification =
  | Verification_ready of 'artifact
  | Verification_rejected of 'artifact * string
  | Verification_stale

type ('artifact, 'prepared) deps = {
  eligible : Transaction.t -> bool;
  verify :
    Scheduler.priority ->
    Transaction.t ->
    ('artifact verification, string) result Lwt.t;
  bind : Transaction.t -> 'artifact -> 'prepared binding Lwt.t;
}

type priority = Scheduler.priority =
  | Required
  | Speculative

type queued = {
  tx : Transaction.t;
  hash : string;
  order : int;
  priority : priority;
  activated : unit Lwt.t;
  activate : unit Lwt.u;
}

type 'artifact running = {
  job : ('artifact verification, string) result Lwt.t;
  started_at : float;
}

type 'artifact completion =
  | Verified of 'artifact
  | Rejected of 'artifact * string

type 'artifact completed = {
  completion : 'artifact completion;
  completed_at : float;
}

type 'artifact entry =
  | Queued of queued
  | Running of 'artifact running
  | Complete of 'artifact completed

type ('artifact, 'prepared) t = {
  deps : ('artifact, 'prepared) deps;
  now : unit -> float;
  max_running : int;
  max_queued : int;
  max_complete : int;
  mutable next_order : int;
  entries : (string, 'artifact entry) Hashtbl.t;
}

type stats = {
  pending : int;
  queued : int;
  running : int;
  ready : int;
  invalid : int;
}

let create
    ?(now = fun () ->
      Int64.to_float (Mtime_clock.elapsed_ns ()) /. 1e9)
    ?(max_running = max_int)
    ?(max_queued =
      Octra_core.Resource_lanes.preverify_queue_limit
        Octra_core.Resource_lanes.Fhe)
    ?(max_complete =
      Octra_core.Resource_lanes.preverify_queue_limit
        Octra_core.Resource_lanes.Fhe)
    deps =
  if max_running < 1 then invalid_arg "preverify max_running must be positive";
  if max_queued < 1 then invalid_arg "preverify max_queued must be positive";
  if max_complete < 1 then invalid_arg "preverify max_complete must be positive";
  {
    deps;
    now;
    max_running;
    max_queued;
    max_complete;
    next_order = 0;
    entries = Hashtbl.create 32;
  }

let short hash =
  String.sub hash 0 (min 12 (String.length hash))

let log_lookup tx status =
  let tx_hash = Transaction.hash tx in
  Octra_log.info "consensus"
    "event = preverify_lookup tx = %s op = %s status = %s"
    (short tx_hash)
    (Transaction.op_type_to_string tx.Transaction.op_type)
    status

let wake_queued queued =
  if Lwt.is_sleeping queued.activated then
    Lwt.wakeup_later queued.activate ()

let remove t hashes =
  List.iter
    (fun hash ->
       begin
         match Hashtbl.find_opt t.entries hash with
         | Some (Queued queued) -> wake_queued queued
         | Some (Running _)
         | Some (Complete _)
         | None -> ()
       end;
       Hashtbl.remove t.entries hash)
    hashes

let running_count t =
  Hashtbl.fold
    (fun _ entry count ->
       match entry with
       | Running _ -> count + 1
       | Queued _
       | Complete _ -> count)
    t.entries
    0

let queued_count t =
  Hashtbl.fold
    (fun _ entry count ->
       match entry with
       | Queued _ -> count + 1
       | Running _
       | Complete _ -> count)
    t.entries
    0

let priority_rank = function
  | Required -> 0
  | Speculative -> 1

let queued_before left right =
  let priority_order =
    compare (priority_rank left.priority) (priority_rank right.priority)
  in
  if priority_order <> 0 then priority_order < 0
  else if left.order <> right.order then left.order < right.order
  else String.compare left.hash right.hash < 0

let next_queued t =
  Hashtbl.fold
    (fun _ entry selected ->
       match entry, selected with
       | Queued queued, None -> Some queued
       | Queued queued, Some current when queued_before queued current ->
         Some queued
       | Queued _, Some _
       | Running _, _
       | Complete _, _ -> selected)
    t.entries
    None

let complete_entries t =
  Hashtbl.fold
    (fun hash entry entries ->
       match entry with
       | Complete completed -> (hash, completed.completed_at) :: entries
       | Queued _
       | Running _ -> entries)
    t.entries
    []
  |> List.sort (fun (left_hash, left_time) (right_hash, right_time) ->
       let time_order = compare left_time right_time in
       if time_order <> 0 then time_order
       else String.compare left_hash right_hash)

let trim_complete t =
  let entries = complete_entries t in
  let excess = List.length entries - t.max_complete in
  if excess > 0 then
    entries
    |> List.filteri (fun index _ -> index < excess)
    |> List.map fst
    |> remove t

let apply_result t tx result =
  let tx_hash = Transaction.hash tx in
  begin
    match result with
    | Ok (Verification_ready artifact) ->
      Hashtbl.replace t.entries tx_hash
        (Complete {
           completion = Verified artifact;
           completed_at = t.now ();
         })
    | Ok (Verification_rejected (artifact, reason)) ->
      Hashtbl.replace t.entries tx_hash
        (Complete {
           completion = Rejected (artifact, reason);
           completed_at = t.now ();
         })
    | Ok Verification_stale
    | Error _ -> remove t [tx_hash]
  end;
  trim_complete t

let rec schedule t =
  if running_count t < t.max_running then
    match next_queued t with
    | None -> ()
    | Some queued ->
      start t queued;
      schedule t

and start t queued =
  match Hashtbl.find_opt t.entries queued.hash with
  | Some (Queued current) when current.activated == queued.activated ->
    let job =
      let open Lwt.Syntax in
      let* () = Lwt.pause () in
      try t.deps.verify queued.priority queued.tx with exn -> Lwt.fail exn
    in
    let running = {
      job;
      started_at = t.now ();
    } in
    Hashtbl.replace t.entries queued.hash (Running running);
    wake_queued queued;
    Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
           let open Lwt.Syntax in
           let* result = job in
           complete t queued.tx running result;
           Lwt.return_unit)
        (fun exn ->
           fail t queued.tx running exn;
           Lwt.return_unit))
  | Some (Queued _)
  | Some (Running _)
  | Some (Complete _)
  | None -> ()

and complete t tx running result =
  let tx_hash = Transaction.hash tx in
  begin
    match Hashtbl.find_opt t.entries tx_hash with
    | Some (Running current) when current.job == running.job ->
      apply_result t tx result;
      let elapsed_ms = int_of_float ((t.now () -. running.started_at) *. 1000.) in
      begin
        match result with
        | Ok (Verification_ready _) ->
          Octra_log.info "consensus"
            "event = preverify_job tx = %s op = %s status = ready elapsed_ms = %d"
            (short tx_hash)
            (Transaction.op_type_to_string tx.Transaction.op_type)
            elapsed_ms
        | Ok (Verification_rejected (_, reason)) ->
          Octra_log.info "consensus"
            "event = preverify_job tx = %s op = %s status = invalid elapsed_ms = %d reason = %s"
            (short tx_hash)
            (Transaction.op_type_to_string tx.Transaction.op_type)
            elapsed_ms
            reason
        | Ok Verification_stale ->
          Octra_log.info "consensus"
            "event = preverify_job tx = %s op = %s status = stale elapsed_ms = %d"
            (short tx_hash)
            (Transaction.op_type_to_string tx.Transaction.op_type)
            elapsed_ms
        | Error reason ->
          Octra_log.error "consensus"
            "event = preverify_job tx = %s op = %s status = failed elapsed_ms = %d reason = %s"
            (short tx_hash)
            (Transaction.op_type_to_string tx.Transaction.op_type)
            elapsed_ms
            reason
      end
    | Some (Queued _)
    | Some (Running _)
    | Some (Complete _)
    | None -> ()
  end;
  schedule t

and fail t tx running exn =
  let tx_hash = Transaction.hash tx in
  begin
    match Hashtbl.find_opt t.entries tx_hash with
    | Some (Running current) when current.job == running.job ->
      remove t [tx_hash];
      Octra_log.error "consensus"
        "event = preverify_job tx = %s op = %s status = failed reason = %s"
        (short tx_hash)
        (Transaction.op_type_to_string tx.Transaction.op_type)
        (Printexc.to_string exn)
    | Some (Queued _)
    | Some (Running _)
    | Some (Complete _)
    | None -> ()
  end;
  schedule t

let oldest_speculative t =
  Hashtbl.fold
    (fun _ entry selected ->
       match entry, selected with
       | Queued queued, _ when queued.priority = Required -> selected
       | Queued queued, None -> Some queued
       | Queued queued, Some current when queued.order < current.order ->
         Some queued
       | Queued _, Some _
       | Running _, _
       | Complete _, _ -> selected)
    t.entries
    None

let make_room_for_required t =
  if queued_count t < t.max_queued then true
  else
    match oldest_speculative t with
    | None -> false
    | Some queued ->
      remove t [queued.hash];
      Octra_log.info "consensus"
        "event = preverify_queue tx = %s status = displaced"
        (short queued.hash);
      true

let enqueue t priority tx =
  let has_room =
    match priority with
    | Speculative -> queued_count t < t.max_queued
    | Required -> make_room_for_required t
  in
  if not has_room then false
  else
    let hash = Transaction.hash tx in
    let activated, activate = Lwt.wait () in
    let queued = {
      tx;
      hash;
      order = t.next_order;
      priority;
      activated;
      activate;
    } in
    t.next_order <- t.next_order + 1;
    Hashtbl.replace t.entries hash (Queued queued);
    schedule t;
    true

let admit_with_priority t priority tx =
  if not (t.deps.eligible tx) then Availability.Unmanaged
  else
    let tx_hash = Transaction.hash tx in
    begin
      match Hashtbl.find_opt t.entries tx_hash with
      | Some (Queued _)
      | Some (Running _)
      | Some (Complete _) -> ()
      | None -> ignore (enqueue t priority tx)
    end;
    Availability.Pending

let admit t tx =
  admit_with_priority t Speculative tx

let restart t priority tx =
  let tx_hash = Transaction.hash tx in
  remove t [tx_hash];
  ignore (admit_with_priority t priority tx)

let bind_complete t tx completion =
  let tx_hash = Transaction.hash tx in
  let artifact =
    match completion with
    | Verified artifact
    | Rejected (artifact, _) -> artifact
  in
  let open Lwt.Syntax in
  let* binding = t.deps.bind tx artifact in
  match binding with
  | Bound prepared ->
    log_lookup tx "ready";
    Lwt.return (Bound prepared)
  | Source_changed ->
    log_lookup tx "stale";
    Lwt.return Source_changed
  | Source_invalid reason ->
    log_lookup tx "invalid";
    Hashtbl.replace t.entries tx_hash
      (Complete {
         completion = Rejected (artifact, reason);
         completed_at = t.now ();
       });
    trim_complete t;
    Lwt.return (Source_invalid reason)

let observe_binding t tx binding =
  match binding with
  | Bound prepared -> Lwt.return (Availability.Ready prepared)
  | Source_changed ->
    restart t Speculative tx;
    Lwt.return Availability.Pending
  | Source_invalid reason -> Lwt.return (Availability.Invalid reason)

let observe t tx =
  if not (t.deps.eligible tx) then Lwt.return Availability.Unmanaged
  else
    let tx_hash = Transaction.hash tx in
    match Hashtbl.find_opt t.entries tx_hash with
    | None ->
      log_lookup tx "missing";
      ignore (admit t tx);
      Lwt.return Availability.Pending
    | Some (Queued _) ->
      log_lookup tx "queued";
      Lwt.return Availability.Pending
    | Some (Running _) ->
      log_lookup tx "pending";
      Lwt.return Availability.Pending
    | Some (Complete completed) ->
      let open Lwt.Syntax in
      let* binding = bind_complete t tx completed.completion in
      observe_binding t tx binding

let max_source_restarts = 1

let promote_required t queued =
  match Hashtbl.find_opt t.entries queued.hash with
  | Some (Queued current) when current.activated == queued.activated ->
    let promoted = { current with priority = Required } in
    Hashtbl.replace t.entries queued.hash (Queued promoted);
    promoted
  | Some (Queued current) -> current
  | Some (Running _)
  | Some (Complete _)
  | None -> queued

let rec await_with_restarts t tx source_restarts =
  if not (t.deps.eligible tx) then Lwt.return Availability.Unmanaged
  else
    let tx_hash = Transaction.hash tx in
    match Hashtbl.find_opt t.entries tx_hash with
    | None ->
      log_lookup tx "missing_wait";
      ignore (admit_with_priority t Required tx);
      begin
        match Hashtbl.find_opt t.entries tx_hash with
        | None ->
          log_lookup tx "synchronous_retry";
          Lwt.return Availability.Unmanaged
        | Some _ -> await_with_restarts t tx source_restarts
      end
    | Some (Queued queued) ->
      let queued = promote_required t queued in
      schedule t;
      log_lookup tx "queued_wait";
      let open Lwt.Syntax in
      let* () = Lwt.protected queued.activated in
      await_with_restarts t tx source_restarts
    | Some (Complete completed) ->
      let open Lwt.Syntax in
      let* binding = bind_complete t tx completed.completion in
      await_binding t tx source_restarts binding
    | Some (Running running) ->
      log_lookup tx "waiting";
      Lwt.catch
        (fun () ->
           let open Lwt.Syntax in
           let* result = Lwt.protected running.job in
           complete t tx running result;
           await_result t tx source_restarts result)
        (function
         | Lwt.Canceled -> Lwt.fail Lwt.Canceled
         | exn ->
           fail t tx running exn;
           log_lookup tx "synchronous_retry";
           Lwt.return Availability.Unmanaged)

and await_binding t tx source_restarts = function
  | Bound prepared -> Lwt.return (Availability.Ready prepared)
  | Source_invalid reason -> Lwt.return (Availability.Invalid reason)
  | Source_changed when source_restarts < max_source_restarts ->
    restart t Required tx;
    await_with_restarts t tx (source_restarts + 1)
  | Source_changed ->
    let tx_hash = Transaction.hash tx in
    remove t [tx_hash];
    log_lookup tx "source_change_limit";
    Lwt.return Availability.Unmanaged

and await_result t tx source_restarts = function
  | Error _ ->
    log_lookup tx "synchronous_retry";
    Lwt.return Availability.Unmanaged
  | Ok Verification_stale ->
    await_binding t tx source_restarts Source_changed
  | Ok (Verification_ready artifact) ->
    let open Lwt.Syntax in
    let* binding = bind_complete t tx (Verified artifact) in
    await_binding t tx source_restarts binding
  | Ok (Verification_rejected (artifact, reason)) ->
    let open Lwt.Syntax in
    let* binding = bind_complete t tx (Rejected (artifact, reason)) in
    await_binding t tx source_restarts binding

let await t tx =
  await_with_restarts t tx 0

let retain t keep =
  let removed =
    Hashtbl.fold
      (fun hash entry hashes ->
         match entry with
         | Queued queued when queued.priority = Required -> hashes
         | Queued _ -> if keep hash then hashes else hash :: hashes
         | Running _ -> hashes
         | Complete _ -> if keep hash then hashes else hash :: hashes)
      t.entries
      []
  in
  remove t removed

let stats t =
  Hashtbl.fold
    (fun _ entry stats ->
       match entry with
       | Queued _ ->
         {
           stats with
           pending = stats.pending + 1;
           queued = stats.queued + 1;
         }
       | Running _ ->
         {
           stats with
           pending = stats.pending + 1;
           running = stats.running + 1;
         }
       | Complete { completion = Verified _; _ } ->
         { stats with ready = stats.ready + 1 }
       | Complete { completion = Rejected _; _ } ->
         { stats with invalid = stats.invalid + 1 })
    t.entries
    { pending = 0; queued = 0; running = 0; ready = 0; invalid = 0 }