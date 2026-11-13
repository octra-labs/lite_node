(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epochs = Map.Make (Int64)
module Readers = Map.Make (Int)

type lease = { epoch : int64; reader : int }
type 'a reader = Shared | Fresh of 'a list
type 'a slot = { readers : 'a reader Readers.t; replies : 'a list; capacity : int }
type 'a t = { next : int; count : int; slots : 'a slot Epochs.t }

let empty = { next = 0; count = 0; slots = Epochs.empty }

let join ?(fresh = false) ?(capacity = 256) ~epoch t =
  if capacity < 0 then Error "root query reply capacity is negative"
  else if t.next = max_int then Error "root query reader limit reached"
  else if t.count >= 64 then Error "root query reader capacity reached"
  else if not (Epochs.mem epoch t.slots) && Epochs.cardinal t.slots >= 64 then
    Error "root query epoch capacity reached"
  else
    let lease = { epoch; reader = t.next } in
    let slot = match Epochs.find_opt epoch t.slots with
      | Some slot -> slot
      | None -> { readers = Readers.empty; replies = []; capacity } in
    let reader = if fresh then Fresh [] else Shared in
    let slot = { slot with readers = Readers.add lease.reader reader slot.readers;
      capacity = max capacity slot.capacity } in
    Ok ({ next = t.next + 1; count = t.count + 1;
      slots = Epochs.add epoch slot t.slots }, lease)

let listening ~epoch t = Epochs.mem epoch t.slots

let add ~epoch ~same reply t =
  match Epochs.find_opt epoch t.slots with
  | None -> t
  | Some slot ->
    let collect replies =
      if List.length replies >= slot.capacity || List.exists (same reply) replies then replies
      else reply :: replies in
    let readers = Readers.map (function
      | Shared -> Shared
      | Fresh replies -> Fresh (collect replies)) slot.readers in
    let slot = { slot with readers; replies = collect slot.replies } in
    { t with slots = Epochs.add epoch slot t.slots }

let read lease t =
  match Epochs.find_opt lease.epoch t.slots with
  | None -> []
  | Some slot ->
    match Readers.find_opt lease.reader slot.readers with
    | Some Shared -> slot.replies
    | Some (Fresh replies) -> replies
    | None -> []

let leave lease t =
  match Epochs.find_opt lease.epoch t.slots with
  | None -> t
  | Some slot when not (Readers.mem lease.reader slot.readers) -> t
  | Some slot ->
    let readers = Readers.remove lease.reader slot.readers in
    let slots = if Readers.is_empty readers then Epochs.remove lease.epoch t.slots
      else Epochs.add lease.epoch {slot with readers} t.slots in
    { t with count = t.count - 1; slots }