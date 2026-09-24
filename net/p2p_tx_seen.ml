(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Keys = Map.Make (String)
module Times = Set.Make (struct
  type t = float * string
  let compare (left, key) (right, other) =
    let order = Float.compare left right in
    if order = 0 then String.compare key other else order
end)

type t = {
  keys : float Keys.t;
  times : Times.t;
  last : float;
}

let capacity = 4096
let lifetime = 1.0
let empty = { keys = Keys.empty; times = Times.empty; last = neg_infinity }
let size t = Keys.cardinal t.keys

let recent t ~now key =
  Float.is_finite now && now >= t.last
  && match Keys.find_opt key t.keys with
    | Some stamp -> now -. stamp < lifetime
    | None -> false

let remove t (stamp, key) =
  { t with keys = Keys.remove key t.keys; times = Times.remove (stamp, key) t.times }

let rec prune t now =
  match Times.min_elt_opt t.times with
  | Some ((stamp, _) as entry) when now -. stamp >= lifetime ->
    prune (remove t entry) now
  | Some _ | None -> t

let step t ~now key =
  if not (Float.is_finite now) then empty, true
  else
    let t = if now < t.last then empty else prune t now in
    let t = { t with last = now } in
    if Keys.mem key t.keys then t, false
    else
      let t =
        if size t < capacity then t
        else remove t (Times.min_elt t.times)
      in
      { keys = Keys.add key now t.keys;
        times = Times.add (now, key) t.times;
        last = now }, true