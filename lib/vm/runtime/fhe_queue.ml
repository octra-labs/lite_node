(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type ticket = Proof_wait.ticket

type entry = {
  ticket : ticket;
  deadline : int64;
  abandoned : bool;
  urgent : bool;
}

type state = {
  active : entry option;
  pending : entry list;
  closed : bool;
}

type reason = Busy | Duplicate | Deadline | Cancelled | Stopped

type message = Submit of entry | Complete of ticket | Cancel of ticket | Tick | Stop

type effect = Start of ticket | Fail of ticket * reason | Retire of ticket | Refuse of reason

let capacity = 8
let view_limit = 2
let empty = {active = None; pending = []; closed = false}

let rec enqueue entry = function
  | [] -> [entry]
  | first :: rest when not entry.urgent || first.urgent -> first :: enqueue entry rest
  | pending -> entry :: pending

let expire now state =
  let pending, effects = List.fold_right (fun entry (kept, effects) ->
    if now >= entry.deadline then
      kept, Fail (entry.ticket, Deadline) :: Retire entry.ticket :: effects
    else entry :: kept, effects) state.pending ([], []) in
  match state.active with
  | Some entry when not entry.abandoned && now >= entry.deadline ->
    {state with active = Some {entry with abandoned = true}; pending},
    Fail (entry.ticket, Deadline) :: effects
  | _ -> {state with pending}, effects

let dispatch state =
  match state.active, state.pending with
  | None, entry :: rest when not state.closed ->
    {state with active = Some entry; pending = rest}, [Start entry.ticket]
  | _ -> state, []

let delta state (now, message) =
  let state, expired = expire now state in
  let next, effects = match message with
    | Submit entry ->
      let present item = item.ticket = entry.ticket in
      if state.closed then state, [Refuse Stopped]
      else if entry.deadline <= now then state, [Refuse Deadline]
      else if Option.fold ~none:false ~some:present state.active
          || List.exists present state.pending then state, [Refuse Duplicate]
      else if List.length state.pending >= capacity then state, [Refuse Busy]
      else if not entry.urgent && List.length
          (List.filter (fun entry -> not entry.urgent) state.pending) >= view_limit then
        state, [Refuse Busy]
      else
        let active, effects = match state.active with
          | Some work when entry.urgent && not work.urgent && not work.abandoned ->
            Some {work with abandoned = true}, [Fail (work.ticket, Cancelled)]
          | active -> active, [] in
        {state with active; pending = enqueue {entry with abandoned = false} state.pending}, effects
    | Complete ticket ->
      begin match state.active with
      | Some entry when entry.ticket = ticket ->
        {state with active = None}, [Retire ticket]
      | _ -> state, []
      end
    | Cancel ticket ->
      begin match state.active with
      | Some entry when entry.ticket = ticket && not entry.abandoned ->
        {state with active = Some {entry with abandoned = true}}, [Fail (ticket, Cancelled)]
      | _ ->
        if List.exists (fun entry -> entry.ticket = ticket) state.pending then
          {state with pending = List.filter (fun entry -> entry.ticket <> ticket) state.pending},
          [Fail (ticket, Cancelled); Retire ticket]
        else state, []
      end
    | Tick -> state, []
    | Stop ->
      let effects = List.concat_map (fun entry ->
        [Fail (entry.ticket, Stopped); Retire entry.ticket]) state.pending in
      let active, effects = match state.active with
        | Some entry when not entry.abandoned ->
          Some {entry with abandoned = true}, Fail (entry.ticket, Stopped) :: effects
        | active -> active, effects in
      {active; pending = []; closed = true}, effects
  in
  let next, started = dispatch next in
  next, expired @ effects @ started