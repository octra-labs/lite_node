(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type 'a item = {
  key : string;
  value : 'a;
  expires : float;
}

type 'a state = {
  generation : int;
  serial : int;
  active : (int * 'a item) option;
  queue : 'a item list;
  seen : string list;
  closed : bool;
}

type outcome = Sent | Expired | Failed of string | Cancelled

type 'a message =
  | Offer of int * float * 'a item
  | Finished of int * float * outcome
  | Advance of int * float
  | Close
  | Open of int

type 'a effect =
  | Send of int * 'a item
  | Cancel
  | Dropped of string

let capacity = 4
let lifetime = 2.

let empty = {
  generation = 0;
  serial = 0;
  active = None;
  queue = [];
  seen = [];
  closed = false;
}

let rec recent count = function
  | _ when count = 0 -> []
  | [] -> []
  | key :: rest -> key :: recent (count - 1) rest

let expire state now =
  let expired, queue = List.partition (fun item -> item.expires <= now) state.queue in
  { state with queue }, List.map (fun _ -> Dropped "expired") expired

let start state now =
  let state, effects = expire state now in
  let queue = state.queue in
  match state.active, queue with
  | None, item :: rest when not state.closed ->
    let serial = state.serial + 1 in
    { state with serial; active = Some (serial, item); queue = rest },
    effects @ [Send (serial, item)]
  | _ -> state, effects

let known state key =
  List.mem key state.seen
  || List.exists (fun item -> item.key = key) state.queue
  || Option.fold ~none:false
       ~some:(fun (_, item) -> item.key = key) state.active

let advance state generation =
  if generation <= state.generation then state, []
  else
    { state with generation; active = None; queue = []; seen = [] },
    (if Option.is_some state.active then [Cancel] else [])

let step state = function
  | Close ->
    { state with closed = true; active = None; queue = []; seen = [] }, [Cancel]
  | Open generation ->
    { state with generation; closed = false; active = None; queue = []; seen = [] },
    [Cancel]
  | Advance (generation, now) ->
    let state, effects = advance state generation in
    let state, sends = start state now in
    state, effects @ sends
  | Finished (serial, now, outcome) ->
    (match state.active with
     | Some (current, item) when current = serial ->
       let seen, effects = match outcome with
         | Sent -> recent 64 (item.key :: state.seen), []
         | Expired -> state.seen, [Dropped "expired"]
         | Failed reason -> state.seen, [Dropped reason]
         | Cancelled -> state.seen, []
       in
       let state, sends = start { state with active = None; seen } now in
       state, effects @ sends
     | Some _ | None -> state, [])
  | Offer (generation, now, item) ->
    if state.closed || generation < state.generation then state, []
    else
      let state, effects = advance state generation in
      let state, expired = expire state now in
      let effects = effects @ expired in
      if known state item.key then state, effects
      else if item.expires <= now then state, effects @ [Dropped "expired"]
      else if List.length state.queue >= capacity then state, effects @ [Dropped "full"]
      else
        let state = {
          state with
          queue = state.queue @ [item];
        } in
        let state, sends = start state now in
        state, effects @ sends

type 'a t = {
  mutable state : 'a state;
  mutable task : outcome Lwt.t option;
  now : unit -> float;
  send : 'a -> unit Lwt.t;
  wait : float -> unit Lwt.t;
  warn : string -> unit;
}

let create ~now ~send ~wait ~warn = {
  state = empty;
  task = None;
  now;
  send;
  wait;
  warn;
}

let rec dispatch t message =
  let state, effects = step t.state message in
  t.state <- state;
  List.iter (perform t) effects

and perform t = function
  | Cancel ->
    let task = t.task in
    t.task <- None;
    Option.iter Lwt.cancel task
  | Dropped reason -> t.warn reason
  | Send (serial, item) ->
    let open Lwt.Syntax in
    let task =
      let* () = Lwt.pause () in
      let remaining = item.expires -. t.now () in
      if remaining <= 0. then Lwt.return Expired
      else Lwt.pick [
        (let* () = t.send item.value in Lwt.return Sent);
        (let* () = t.wait remaining in Lwt.return Expired);
      ]
    in
    t.task <- Some task;
    Lwt.async (fun () ->
      let* outcome = Lwt.catch
        (fun () -> task)
        (function
          | Lwt.Canceled -> Lwt.return Cancelled
          | exn -> Lwt.return (Failed (Printexc.to_string exn)))
      in
      (match t.state.active with
       | Some (current, _) when current = serial -> t.task <- None
       | Some _ | None -> ());
      dispatch t (Finished (serial, t.now (), outcome));
      Lwt.return_unit)

let offer t ~generation ~key value =
  let now = t.now () in
  dispatch t (Offer (generation, now, { key; value; expires = now +. lifetime }))

let progress t ~generation =
  dispatch t (Advance (generation, t.now ()))

let close t = dispatch t Close
let open_ t ~generation = dispatch t (Open generation)