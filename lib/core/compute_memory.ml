(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Entries = Map.Make (Int)

type stats = {bytes : int; speculative : int; jobs : int}
type state = {
  limit : int;
  speculative_limit : int;
  slots : int;
  next : int;
  usage : stats;
  entries : (int * bool) Entries.t;
}
type message = Take of int * bool | Release of int
type effect = Held of int | Refused | Released
type t = {lock : Mutex.t; mutable state : state}
type ticket = {owner : t; id : int}

let add left right =
  if left < 0 || right < 0 || left > max_int - right then max_int
  else left + right

let strings values = List.fold_left (fun size value -> add size (String.length value)) 0 values

let delta state = function
  | Take (bytes, speculative) ->
    if bytes < 0 || bytes > state.limit - state.usage.bytes
        || state.usage.jobs >= state.slots || state.next = max_int
        || (speculative && bytes > state.speculative_limit - state.usage.speculative) then
      state, [Refused]
    else
      let id = state.next in
      let usage = {bytes = state.usage.bytes + bytes;
        speculative = state.usage.speculative + (if speculative then bytes else 0);
        jobs = state.usage.jobs + 1} in
      {state with next = id + 1; usage;
        entries = Entries.add id (bytes, speculative) state.entries}, [Held id]
  | Release id ->
    match Entries.find_opt id state.entries with
    | None -> state, [Released]
    | Some (bytes, speculative) ->
      let usage = {bytes = state.usage.bytes - bytes;
        speculative = state.usage.speculative - (if speculative then bytes else 0);
        jobs = state.usage.jobs - 1} in
      {state with usage; entries = Entries.remove id state.entries}, [Released]

let create ~limit ~speculative_limit ~slots =
  if limit < 1 || speculative_limit < 0 || speculative_limit > limit || slots < 1 then
    invalid_arg "compute memory limits";
  {lock = Mutex.create (); state = {limit; speculative_limit; slots; next = 0;
    usage = {bytes = 0; speculative = 0; jobs = 0}; entries = Entries.empty}}

let locked owner action =
  Mutex.lock owner.lock;
  Fun.protect ~finally:(fun () -> Mutex.unlock owner.lock) action

let reserve owner ~speculative bytes = locked owner (fun () ->
  let state, effects = delta owner.state (Take (bytes, speculative)) in
  let result = match effects with [Held id] -> Some {owner; id} | _ -> None in
  owner.state <- state;
  result)

let release ticket = locked ticket.owner (fun () ->
  let state, _ = delta ticket.owner.state (Release ticket.id) in
  ticket.owner.state <- state)

let stats owner = locked owner (fun () -> owner.state.usage)

let limit_of_getenv getenv =
  let megabytes = match Option.bind (getenv "OCTRA_COMPUTE_QUEUE_MB") int_of_string_opt with
    | Some value when value >= 64 && value <= 4096 -> value
    | _ -> 512 in
  megabytes * 1_048_576

let shared =
  let limit = limit_of_getenv Sys.getenv_opt in
  create ~limit ~speculative_limit:(limit / 4) ~slots:256