(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Ids = Map.Make (Int64)
module Parts = Octra_bootstrap.Range_part

type query = {
  from_epoch : int64;
  max_epochs : int;
  part : int option;
  hash : string option;
  head : Octra_core.Head_manifest.t option;
  pubkeys : (string * string) list;
  activation : int option;
}

type loaded = {
  body : string;
  status : string;
  records : int;
  encoded : Parts.encoded option;
}

type error = Busy | Expired | Stopped | Changed | Missing | Invalid of string
type reply = (loaded, error) result
type listener = Waiting | Released

type cache = {
  from_epoch : int64;
  max_epochs : int;
  encoded : Parts.encoded;
  status : string;
  records : int;
  expires : float;
}

type job = {
  id : int64;
  generation : int64;
  query : query;
  deadline : float;
  listener : listener;
  cached : cache option;
}

type state = {
  open_ : bool;
  generation : int64;
  last_id : int64;
  active : job option;
  queued : job list;
  cache : cache option;
}

type message =
  | Ask of int64 * query * float
  | Completed of int64 * int64 * float * reply
  | Cancel of int64
  | Tick of float
  | Stop

type effect = Run of job | Interrupt of int64 | Reply of int64 * reply

let capacity = 4
let lifetime = 15.
let retention = 60.

let empty = {
  open_ = true;
  generation = 0L;
  last_id = 0L;
  active = None;
  queued = [];
  cache = None;
}

let expire now state =
  let cache = Option.bind state.cache (fun entry ->
    if now < entry.expires then Some entry else None) in
  let state = { state with cache } in
  let expired, queued = List.partition (fun job -> now >= job.deadline) state.queued in
  let effects = List.map (fun job -> Reply (job.id, Error Expired)) expired in
  match state.active with
  | Some job when job.listener = Waiting && now >= job.deadline ->
    { state with queued; active = Some { job with listener = Released } },
    Interrupt job.id :: Reply (job.id, Error Expired) :: effects
  | _ -> { state with queued }, effects

let advance state =
  match state.active, state.queued with
  | None, job :: rest ->
    let cached = Option.bind state.cache (fun entry ->
      if job.query.part <> None && entry.from_epoch = job.query.from_epoch
        && entry.max_epochs = job.query.max_epochs
        && (job.query.hash = None || job.query.hash = Parts.digest entry.encoded)
      then Some entry else None) in
    let job = { job with cached } in
    { state with active = Some job; queued = rest }, [Run job]
  | _ -> state, []

let delta state = function
  | Ask (id, query, now) ->
    if id <= state.last_id then state, []
    else if not state.open_ then
      { state with last_id = id }, [Reply (id, Error Stopped)]
    else
      let state, expired = expire now { state with last_id = id } in
      let jobs = Option.to_list state.active @ state.queued in
      if List.length jobs >= capacity || List.exists (fun job -> job.query = query) jobs then
        state, expired @ [Reply (id, Error Busy)]
      else
        let job = { id; generation = state.generation; query;
          deadline = now +. lifetime; listener = Waiting; cached = None } in
        let state, effects = advance { state with queued = state.queued @ [job] } in
        state, expired @ effects
  | Completed (id, generation, now, reply) ->
    begin match state.active with
    | Some job when state.open_ && job.id = id && job.generation = generation
        && generation = state.generation ->
      let state, expired = expire now state in
      let job = Option.get state.active in
      let cache = match reply, job.listener with
        | Ok { encoded = Some encoded; status; records; _ }, Waiting
            when Parts.digest encoded <> None ->
          Some { from_epoch = job.query.from_epoch; max_epochs = job.query.max_epochs;
            encoded; status; records; expires = now +. retention }
        | _ -> state.cache in
      let state, effects = advance { state with active = None; cache } in
      let replies = if job.listener = Waiting then [Reply (id, reply)] else [] in
      state, expired @ replies @ effects
    | _ -> state, []
    end
  | Cancel id ->
    let active = Option.map (fun job ->
      if job.id = id then { job with listener = Released } else job) state.active in
    { state with active; queued = List.filter (fun job -> job.id <> id) state.queued },
    [Interrupt id]
  | Tick now -> expire now state
  | Stop ->
    if not state.open_ then state, []
    else
      let jobs = Option.to_list state.active @ state.queued in
      { empty with open_ = false; generation = Int64.succ state.generation;
        last_id = state.last_id },
      List.concat_map (fun job -> Interrupt job.id ::
        if job.listener = Waiting then [Reply (job.id, Error Stopped)] else []) jobs

type deps = {
  now : unit -> float;
  read : cancelled:(unit -> bool) -> query -> reply Lwt.t;
}

type work = {
  id : int64;
  interrupted : bool ref;
  finished : unit Lwt.t;
}

type t = {
  deps : deps;
  mutable state : state;
  mutable serial : int64;
  mutable replies : reply Lwt.u Ids.t;
  mutable work : work option;
}

let rec dispatch t message =
  let state, effects = delta t.state message in
  t.state <- state;
  List.iter (execute t) effects

and execute t = function
  | Reply (id, value) ->
    begin match Ids.find_opt id t.replies with
    | None -> ()
    | Some resolver ->
      t.replies <- Ids.remove id t.replies;
      Lwt.wakeup_later resolver value
    end
  | Interrupt id ->
    Option.iter (fun work -> if work.id = id then work.interrupted := true) t.work
  | Run job ->
    let interrupted = ref false in
    let finished, finish = Lwt.wait () in
    t.work <- Some { id = job.id; interrupted; finished };
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      Lwt.finalize (fun () ->
        let* reply = Lwt.catch
          (fun () -> match job.cached with
            | Some entry -> Lwt_preemptive.detach (fun () ->
                Parts.render ?index:job.query.part entry.encoded
                |> Result.map (fun body -> {
                  body; status = entry.status; records = entry.records; encoded = None;
                })
                |> Result.map_error (fun reason -> Invalid reason)) ()
            | None when job.query.hash <> None -> Lwt.return_error Missing
            | None -> t.deps.read ~cancelled:(fun () -> !interrupted) job.query)
          (fun _ -> Lwt.return_error (Invalid "range read failed")) in
        dispatch t (Completed (job.id, job.generation, t.deps.now (), reply));
        Lwt.return_unit) (fun () ->
        if Option.fold ~none:false ~some:(fun work -> work.finished == finished) t.work then
          t.work <- None;
        Lwt.wakeup_later finish ();
        Lwt.return_unit))

let create deps = { deps; state = empty; serial = 0L; replies = Ids.empty; work = None }

let load t query =
  if t.serial = Int64.max_int then Lwt.return_error (Invalid "range request id exhausted")
  else
    let id = Int64.succ t.serial in
    t.serial <- id;
    let promise, resolver = Lwt.task () in
    t.replies <- Ids.add id resolver t.replies;
    Lwt.on_cancel promise (fun () ->
      t.replies <- Ids.remove id t.replies;
      dispatch t (Cancel id));
    dispatch t (Ask (id, query, t.deps.now ()));
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt.catch (fun () -> Lwt.pick [
        Lwt.map (fun _ -> ()) (Lwt.protected promise);
        Lwt_unix.sleep lifetime;
      ]) (fun _ -> Lwt.return_unit) in
      dispatch t (Tick (t.deps.now ()));
      Lwt.return_unit);
    promise

let shutdown t =
  let pending = Option.map (fun work -> work.finished) t.work in
  dispatch t Stop;
  Option.value ~default:Lwt.return_unit (Option.map Lwt.protected pending)

let pending state = List.length state.queued + if state.active = None then 0 else 1