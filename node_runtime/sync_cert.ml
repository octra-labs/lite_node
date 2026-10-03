(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Manifest = Octra_bootstrap.State_sync_manifest
module Ids = Map.Make (Int64)

type bytes = {
  raw : string;
  digest : string;
}

type trust = {
  validators : Octra_consensus.C_types.validator_set;
  exporters : Octra_consensus.C_types.validator_set;
  chain : string;
  config : string;
}

type query = {
  path : string;
  trust : trust;
  trust_hash : string;
}

type loaded = {
  raw : string;
  certificate : Manifest.certificate;
}

type error = Busy | Expired | Stopped | Invalid of string
type reply = (loaded, error) result
type listener = Waiting | Timed_out | Cancelled

type job = {
  id : int64;
  generation : int64;
  query : query;
  deadline : float;
  listener : listener;
}

type entry = {
  digest : string;
  trust_hash : string;
  checked_at : float;
  verdict : (unit, string) result;
}

type active =
  | Reading of job
  | Checking of job * string * string
  | Parsing of job * string

type state = {
  open_ : bool;
  generation : int64;
  last_id : int64;
  active : active option;
  queued : job list;
  entries : entry list;
}

type message =
  | Ask of int64 * query * float
  | Read of int64 * int64 * float * (bytes, string) result
  | Checked of int64 * int64 * float * (Manifest.certificate, string) result
  | Parsed of int64 * int64 * float * (Manifest.certificate, string) result
  | Cancel of int64
  | Tick of float
  | Stop

type effect =
  | Read_file of job
  | Verify of job * string
  | Parse of job * string
  | Interrupt of int64
  | Reply of int64 * reply

let capacity = 8
let cache_capacity = 64
let lifetime = 15.
let negative_seconds = 5.

let empty = {
  open_ = true;
  generation = 0L;
  last_id = 0L;
  active = None;
  queued = [];
  entries = [];
}

let job = function
  | Reading job | Checking (job, _, _) | Parsing (job, _) -> job

let with_listener listener active =
  let next = { (job active) with listener } in
  match active with
  | Reading _ -> Reading next
  | Checking (_, raw, digest) -> Checking (next, raw, digest)
  | Parsing (_, raw) -> Parsing (next, raw)

let reason = function
  | Busy -> "state sync verification busy"
  | Expired -> "state sync verification expired"
  | Stopped -> "state sync verification stopped"
  | Invalid reason -> reason

let retryable = function
  | Busy | Expired | Stopped -> true
  | Invalid _ -> false

let failure id reason = Reply (id, Error reason)

let rec advance now state =
  match state.active, state.queued with
  | Some _, _ | None, [] -> state, []
  | None, first :: rest ->
    let state = { state with queued = rest } in
    if now >= first.deadline then
      let state, effects = advance now state in
      state, failure first.id Expired :: effects
    else
      { state with active = Some (Reading first) }, [Read_file first]

let complete now state job reply =
  let state, effects = advance now { state with active = None } in
  state, if job.listener <> Waiting then effects else Reply (job.id, reply) :: effects

let expire now state =
  let expired, queued = List.partition (fun job -> now >= job.deadline) state.queued in
  let effects = List.map (fun job -> failure job.id Expired) expired in
  match state.active with
  | Some active when (job active).listener = Waiting && now >= (job active).deadline ->
    { state with queued; active = Some (with_listener Timed_out active) },
    failure (job active).id Expired :: effects
  | _ -> { state with queued }, effects

let delta state message =
  match message with
  | Ask (id, query, now) ->
    if not state.open_ then state, [failure id Stopped]
    else if id <= state.last_id then state, []
    else
      let state, expired = expire now { state with last_id = id } in
      let size = List.length state.queued + if state.active = None then 0 else 1 in
      if size >= capacity then state, expired @ [failure id Busy]
      else
        let next = { id; generation = state.generation; query;
          deadline = now +. lifetime; listener = Waiting } in
        let state, effects = advance now { state with queued = state.queued @ [next] } in
        state, expired @ effects
  | Read (id, generation, now, result) ->
    begin match state.active with
    | Some (Reading current) when state.open_ && current.id = id
        && current.generation = generation && generation = state.generation ->
      let state, expired = expire now state in
      let current = job (Option.get state.active) in
      let state, effects =
        if current.listener <> Waiting then complete now state current (Error Expired)
        else match result with
        | Error reason -> complete now state current (Error (Invalid reason))
        | Ok { raw; digest } ->
          let matches entry = entry.digest = digest && entry.trust_hash = current.query.trust_hash in
          let cached = List.find_opt (fun entry -> matches entry &&
            (Result.is_ok entry.verdict || now -. entry.checked_at <= negative_seconds)) state.entries in
          match cached with
          | Some entry ->
            let entries = entry :: List.filter (fun row -> not (matches row)) state.entries in
            begin match entry.verdict with
            | Error reason -> complete now { state with entries } current (Error (Invalid reason))
            | Ok () ->
              { state with entries; active = Some (Parsing (current, raw)) }, [Parse (current, raw)]
            end
          | None ->
            { state with active = Some (Checking (current, raw, digest)) }, [Verify (current, raw)]
      in
      state, expired @ effects
    | _ -> state, []
    end
  | Checked (id, generation, now, result) ->
    begin match state.active with
    | Some (Checking (current, raw, digest)) when state.open_ && current.id = id
        && current.generation = generation && generation = state.generation ->
      let state, expired = expire now state in
      let current = job (Option.get state.active) in
      let reply = Result.map (fun certificate -> { raw; certificate }) result |> Result.map_error (fun e -> Invalid e) in
      let entries = if current.listener = Cancelled then state.entries else
        let entry = { digest; trust_hash = current.query.trust_hash; checked_at = now;
          verdict = Result.map (fun _ -> ()) result } in
        entry :: List.filter (fun row -> row.digest <> digest || row.trust_hash <> entry.trust_hash) state.entries
        |> List.filteri (fun index _ -> index < cache_capacity) in
      let state, effects = complete now { state with entries } current reply in
      state, expired @ effects
    | _ -> state, []
    end
  | Parsed (id, generation, now, result) ->
    begin match state.active with
    | Some (Parsing (current, raw)) when state.open_ && current.id = id
        && current.generation = generation && generation = state.generation ->
      let state, expired = expire now state in
      let current = job (Option.get state.active) in
      let reply = Result.map (fun certificate -> { raw; certificate }) result
        |> Result.map_error (fun e -> Invalid e) in
      let state, effects = complete now state current reply in
      state, expired @ effects
    | _ -> state, []
    end
  | Cancel id ->
    let active = Option.map (fun active ->
      if (job active).id = id then with_listener Cancelled active else active) state.active in
    { state with active; queued = List.filter (fun job -> job.id <> id) state.queued }, [Interrupt id]
  | Tick now -> expire now state
  | Stop ->
    let jobs = Option.fold ~none:state.queued ~some:(fun active -> job active :: state.queued) state.active in
    { empty with open_ = false; generation = Int64.succ state.generation; last_id = state.last_id },
    List.concat_map (fun job -> Interrupt job.id ::
      if job.listener = Waiting then [failure job.id Stopped] else []) jobs

type deps = {
  now : unit -> float;
  read : string -> (string, string) result Lwt.t;
  verify : cancelled:(unit -> bool) -> trust -> string -> (Manifest.certificate, string) result Lwt.t;
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

let launch t (job : job) run =
  let interrupted = ref false in
  let finished, finish = Lwt.wait () in
  t.work <- Some { id = job.id; interrupted; finished };
  Lwt.async (fun () ->
    Lwt.finalize (fun () -> run (fun () -> !interrupted)) (fun () ->
      if Option.fold ~none:false ~some:(fun work -> work.finished == finished) t.work then
        t.work <- None;
      Lwt.wakeup_later finish ();
      Lwt.return_unit))

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
  | Read_file job ->
    launch t job (fun cancelled ->
      let open Lwt.Syntax in
      let* value = Lwt.catch (fun () ->
        let* raw = t.deps.read job.query.path in
        if cancelled () then Lwt.return_error "state sync request cancelled"
        else Lwt_preemptive.detach (Result.map (fun raw ->
            { raw; digest = Digestif.SHA256.(digest_string raw |> to_hex) })) raw)
        (fun _ -> Lwt.return_error "state sync certificate read failed") in
      dispatch t (Read (job.id, job.generation, t.deps.now (), value));
      Lwt.return_unit)
  | Verify (job, raw) ->
    launch t job (fun cancelled ->
      let open Lwt.Syntax in
      let* value = Lwt.catch (fun () -> t.deps.verify ~cancelled job.query.trust raw)
        (fun _ -> Lwt.return_error "state sync certificate verification failed") in
      dispatch t (Checked (job.id, job.generation, t.deps.now (), value));
      Lwt.return_unit)
  | Parse (job, raw) ->
    launch t job (fun _ ->
      let open Lwt.Syntax in
      let* value = Lwt.catch
        (fun () -> Lwt_preemptive.detach Manifest.parse_certificate_string raw)
        (fun _ -> Lwt.return_error "state sync certificate parse failed") in
      dispatch t (Parsed (job.id, job.generation, t.deps.now (), value));
      Lwt.return_unit)
  | Interrupt id ->
    Option.iter (fun work -> if work.id = id then work.interrupted := true) t.work

let create deps = { deps; state = empty; serial = 0L; replies = Ids.empty; work = None }

type stats = {
  active : int;
  queued : int;
  cached : int;
  generation : int64;
}

let stats (state : state) = {
  active = if state.active = None then 0 else 1;
  queued = List.length state.queued;
  cached = List.length state.entries;
  generation = state.generation;
}

let trust_hash trust =
  Yojson.Safe.to_string (`List [
    `String (Manifest.set_hash trust.validators);
    `String (Manifest.set_hash trust.exporters);
    `String trust.chain; `String trust.config;
  ]) |> fun raw -> Digestif.SHA256.(digest_string raw |> to_hex)

let load t ~path trust =
  if t.serial = Int64.max_int then Lwt.return_error (Invalid "state sync request id exhausted")
  else
    let id = Int64.succ t.serial in
    t.serial <- id;
    let promise, resolver = Lwt.task () in
    t.replies <- Ids.add id resolver t.replies;
    Lwt.on_cancel promise (fun () ->
      t.replies <- Ids.remove id t.replies;
      dispatch t (Cancel id));
    dispatch t (Ask (id, { path; trust; trust_hash = trust_hash trust }, t.deps.now ()));
    Lwt.async (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt.pick [
        (Lwt.protected promise |> Lwt.map (fun _ -> ()));
        Lwt_unix.sleep lifetime;
      ] |> fun work -> Lwt.catch (fun () -> work) (fun _ -> Lwt.return_unit) in
      dispatch t (Tick (t.deps.now ()));
      Lwt.return_unit);
    promise

let shutdown t =
  let pending = Option.map (fun work -> work.finished) t.work in
  dispatch t Stop;
  match pending with
  | None -> Lwt.return_unit
  | Some finished -> Lwt.protected finished