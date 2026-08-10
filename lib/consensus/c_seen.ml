(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  capacity : int;
  ids : (string, unit) Hashtbl.t;
  order : string Queue.t;
}

let create ~capacity =
  if capacity <= 0 then invalid_arg "consensus seen capacity";
  {
    capacity;
    ids = Hashtbl.create (min capacity 256);
    order = Queue.create ();
  }

let evict_oldest t =
  let id = Queue.take t.order in
  Hashtbl.remove t.ids id

let remember t id =
  if Hashtbl.mem t.ids id then false
  else begin
    if Hashtbl.length t.ids >= t.capacity then evict_oldest t;
    Hashtbl.add t.ids id ();
    Queue.add id t.order;
    true
  end

let known t id = Hashtbl.mem t.ids id

let length t = Hashtbl.length t.ids