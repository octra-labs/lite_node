(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type phase = Absent | Warming of int | Ready of int | Lent of int * int
  | Rechecking of int * int | Paused of float | Dormant of float | Closed
type recovery_ticket = {generation : int; id : int; hash : string; deadline : float}
type recovery_result = Cold_accepted | Cold_rejected | Cold_unfinished
type state = {phase : phase; generation : int; starts : float list; total : int; expires : float;
  proofs : string list; recovery : recovery_ticket option}
type confirmation = Agreed | Disagreed | Unfinished
type message = Tick of float | Warmed of int | Borrow of int
  | Returned of int * int * bool | Check of int * int * float
  | Settled of int * int * confirmation | Failed of int | Stop
  | Verified of int * int * string | Cached of int * int * string
  | Demand of float * recovery_ticket | Recovered of float * recovery_ticket * recovery_result
type effect = Spawn of int | Retire | Granted of int * int | Recovery of recovery_ticket | Denied

let empty = {phase = Absent; generation = 0; starts = []; total = 0; expires = Float.infinity;
  proofs = []; recovery = None}

let proof_cap = 64

let valid_hash hash = String.length hash = 64
  && String.for_all (function '0'..'9' | 'a'..'f' -> true | _ -> false) hash

let recent now starts = List.filter (fun time -> now -. time < 600.) starts

let remember hash proofs =
  let rec take count = function
    | _ when count = 0 -> []
    | [] -> []
    | value :: rest -> value :: take (count - 1) rest in
  hash :: take (proof_cap - 1) (List.filter ((<>) hash) proofs)

let delta state = function
  | Stop -> {state with phase = Closed; proofs = []; recovery = None},
    (if state.phase = Closed then [] else [Retire])
  | Tick now when Option.fold ~none:false ~some:(fun ticket -> now >= ticket.deadline) state.recovery ->
    {state with recovery = None}, []
  | Tick now when now >= state.expires && (match state.phase with Rechecking _ -> true | _ -> false) ->
    {state with phase = Absent; expires = Float.infinity}, [Retire]
  | Tick now when state.phase = Absent || (match state.phase with Paused until -> now >= until | _ -> false) ->
    let starts = recent now state.starts in
    if state.total >= 16 || state.generation = max_int then
      {state with phase = Dormant (now +. 600.); starts; recovery = None}, []
    else if List.length starts >= 3 then
      {state with phase = Paused (List.hd (List.rev starts) +. 600.); starts}, []
    else
      let generation = state.generation + 1 in
      {state with phase = Warming generation; generation; starts = now :: starts; total = state.total + 1;
        expires = Float.infinity}, [Spawn generation]
  | Warmed generation when state.phase = Warming generation ->
    {state with phase = Ready generation}, []
  | Borrow id ->
    (match state.phase with
    | Ready generation -> {state with phase = Lent (generation, id)}, [Granted (generation, id)]
    | _ -> state, [Denied])
  | Returned (generation, id, true) when state.phase = Lent (generation, id) ->
    {state with phase = Ready generation}, []
  | (Verified (generation, id, hash) | Cached (generation, id, hash)) as message
      when state.phase = Lent (generation, id) && valid_hash hash
        && (match message with Cached _ -> List.mem hash state.proofs | _ -> true) ->
    let total = match message with
      | Verified _ when not (List.mem hash state.proofs) -> 0
      | _ -> state.total in
    {state with phase = Ready generation; total; proofs = remember hash state.proofs}, []
  | Check (generation, id, expires) when state.phase = Lent (generation, id) ->
    {state with phase = Rechecking (generation, id); expires}, []
  | Settled (generation, id, (Agreed | Unfinished)) when state.phase = Rechecking (generation, id) ->
    {state with phase = Ready generation; expires = Float.infinity}, []
  | Settled (generation, id, Disagreed) when state.phase = Rechecking (generation, id) ->
    {state with phase = Closed; proofs = []; recovery = None}, [Retire]
  | Returned (generation, id, false) when state.phase = Lent (generation, id) ->
    {state with phase = Absent}, [Retire]
  | Failed generation when generation = state.generation && (match state.phase with
      | Warming found | Ready found | Lent (found, _) | Rechecking (found, _) -> found = generation
      | Absent | Paused _ | Dormant _ | Closed -> false) ->
    {state with phase = Absent}, [Retire]
  | Demand (now, ticket) when (match state.phase with Dormant until -> now >= until | _ -> false)
      && state.recovery = None && ticket.generation = state.generation && ticket.id > 0
      && Float.is_finite now && Float.is_finite ticket.deadline && now < ticket.deadline
      && valid_hash ticket.hash && not (List.mem ticket.hash state.proofs) ->
    {state with recovery = Some ticket}, [Recovery ticket]
  | Demand _ -> state, [Denied]
  | Recovered (now, ticket, result) when state.recovery = Some ticket
      && (match state.phase with Dormant _ -> true | _ -> false) ->
    let starts = recent now state.starts in
    let state = {state with recovery = None; starts} in
    if result = Cold_accepted && Float.is_finite now && now < ticket.deadline
        && (match state.phase with Dormant until -> now >= until | _ -> false)
        && ticket.generation = state.generation && state.generation < max_int
        && List.length starts < 3 then
      let generation = state.generation + 1 in
      {state with phase = Warming generation; generation; total = 16;
        starts = now :: starts; expires = Float.infinity}, [Spawn generation]
    else state, []
  | _ -> state, []

let poll ?recovered state now settled =
  let state, effects = match settled with
    | None -> state, []
    | Some (generation, id, result) -> delta state (Settled (generation, id, result)) in
  let state, next = delta state (Tick now) in
  let state, last = match recovered with
    | None -> state, []
    | Some (ticket, result) -> delta state (Recovered (now, ticket, result)) in
  state, effects @ next @ last