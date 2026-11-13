(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type scope = { height : int64; generation : int }

type token = { scope : scope; life : int }

type query = { token : token; round : int; stamp : int option }

type t = {
  scope : scope;
  life : int;
  queue : query list;
  active : (int * query) option;
  serial : int;
  last : int64 option;
  notices : int;
  closed : bool;
}

type message =
  | Advance of scope * int64
  | Offer of query
  | Prioritize of query
  | Take of int64
  | Complete of int * int64
  | Begin_notice of int64
  | End_notice of int64
  | Open of scope * int64
  | Close of int64

type refusal = Closed | Other_scope | Full

type effect =
  | Send of int * query
  | Cancel of int
  | Refused of refusal
  | Displaced of query

let capacity = C_engine.round_history_limit
let interval = 1_100_000_000L

let create scope = {
  scope; life = 0; queue = []; active = None; serial = 0; last = None;
  notices = 0; closed = false;
}

let token (state : t) : token = { scope = state.scope; life = state.life }

let pending state = not state.closed && state.queue <> []

let notice state now =
  { state with last = Some (Option.fold ~none:now ~some:(max now) state.last) }

let clear state now =
  let effect = Option.map (fun (serial, _) -> Cancel serial) state.active in
  let state = if Option.is_some state.active then notice state now else state in
  { state with queue = []; active = None }, effect

let advance state scope now =
  if state.scope = scope then state, None
  else
    let state, effect = clear state now in
    { state with scope }, effect

let offer ~priority state query =
  let same kept = kept.token = query.token && kept.round = query.round in
  let older kept = match kept.stamp, query.stamp with
    | Some before, Some after -> after < before
    | Some _, None -> true
    | None, _ -> false in
  if state.closed then state, Some (Refused Closed)
  else if query.token <> token state then state, Some (Refused Other_scope)
  else match List.find_opt same state.queue with
    | Some kept when older kept -> state, None
    | Some _ ->
      let queue = if priority then query :: List.filter (fun kept -> not (same kept)) state.queue
        else List.map (fun kept -> if same kept then query else kept) state.queue in
      {state with queue}, None
    | None when Option.fold ~none:false ~some:(fun (_, active) -> active = query) state.active ->
      state, None
    | None ->
      if List.length state.queue + (if Option.is_some state.active then 1 else 0) >= capacity then
        match priority, List.rev state.queue with
        | true, removed :: rest ->
          {state with queue = query :: List.rev rest}, Some (Displaced removed)
        | _ -> state, Some (Refused Full)
      else {state with queue = if priority then query :: state.queue else state.queue @ [query]}, None

let step state = function
  | Advance (scope, now) -> advance state scope now
  | Open (scope, now) ->
    let state, effect = advance state scope now in
    { state with closed = false }, effect
  | Close now ->
    let state, effect = clear state now in
    { state with closed = true; life = state.life + 1 }, effect
  | Begin_notice now ->
    { (notice state now) with notices = state.notices + 1 }, None
  | End_notice now ->
    { (notice state now) with notices = max 0 (state.notices - 1) }, None
  | Complete (serial, now) ->
    (match state.active with
     | Some (current, _) when current = serial ->
       { (notice state now) with active = None }, None
     | Some _ | None -> state, None)
  | Offer query -> offer ~priority:false state query
  | Prioritize query -> offer ~priority:true state query
  | Take now ->
    let due = Option.fold ~none:true ~some:(fun prior ->
      now >= prior && Int64.sub now prior >= interval) state.last in
    match state.queue, state.active with
    | query :: rest, None when not state.closed && state.notices = 0 && due ->
      let serial = state.serial + 1 in
      { (notice state now) with queue = rest; active = Some (serial, query); serial },
      Some (Send (serial, query))
    | _ -> state, None