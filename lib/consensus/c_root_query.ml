(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Epochs = Map.Make (Int64)
module Readers = Set.Make (Int)

type lease = { epoch : int64; reader : int }
type 'a slot = { readers : Readers.t; replies : 'a list }
type 'a t = { next : int; slots : 'a slot Epochs.t }

let empty = { next = 0; slots = Epochs.empty }

let join ~epoch t =
  if t.next = max_int then Error "root query reader limit reached"
  else
    let lease = { epoch; reader = t.next } in
    let slot = match Epochs.find_opt epoch t.slots with
      | Some slot -> slot
      | None -> { readers = Readers.empty; replies = [] } in
    let slot = { slot with readers = Readers.add lease.reader slot.readers } in
    Ok ({ next = t.next + 1; slots = Epochs.add epoch slot t.slots }, lease)

let listening ~epoch t = Epochs.mem epoch t.slots

let add ~epoch ~same reply t =
  match Epochs.find_opt epoch t.slots with
  | None -> t
  | Some slot when List.exists (same reply) slot.replies -> t
  | Some slot ->
    let slot = { slot with replies = reply :: slot.replies } in
    { t with slots = Epochs.add epoch slot t.slots }

let read lease t =
  match Epochs.find_opt lease.epoch t.slots with
  | Some slot when Readers.mem lease.reader slot.readers -> slot.replies
  | None | Some _ -> []

let leave lease t =
  match Epochs.find_opt lease.epoch t.slots with
  | None -> t
  | Some slot ->
    let readers = Readers.remove lease.reader slot.readers in
    let slots = if Readers.is_empty readers then Epochs.remove lease.epoch t.slots
      else Epochs.add lease.epoch {slot with readers} t.slots in
    { t with slots }