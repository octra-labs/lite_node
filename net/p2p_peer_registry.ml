(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t = {
  rows : (string, P2p_peer_record.t) Hashtbl.t;
  ttl_s : float;
  max_per_bucket : int;
}

type add_result =
  | Added
  | Refreshed
  | Rejected of string

let create ?(ttl_s = 300.0) ?(max_per_bucket = 4) () =
  { rows = Hashtbl.create 32; ttl_s; max_per_bucket }

let clear t =
  Hashtbl.clear t.rows

let live ~now t r =
  let age = now -. Int64.to_float r.P2p_peer_record.ts in
  age >= -30.0 && age <= t.ttl_s

let prune ~now t =
  let dead =
    Hashtbl.fold
      (fun id r acc -> if live ~now t r then acc else id :: acc)
      t.rows
      []
  in
  List.iter (Hashtbl.remove t.rows) dead

let values ~now t =
  prune ~now t;
  Hashtbl.fold (fun _ r acc -> r :: acc) t.rows []
  |> List.sort (fun a b -> String.compare a.P2p_peer_record.node_id b.P2p_peer_record.node_id)

let add ~now t r =
  prune ~now t;
  if not (live ~now t r) then Rejected "ttl"
  else
    let records =
      Hashtbl.fold
        (fun id row acc -> if id = r.P2p_peer_record.node_id then acc else row :: acc)
        t.rows
        []
    in
    if not (P2p_peer_record.allow_bucket ~max_per_bucket:t.max_per_bucket records r) then
      Rejected "eclipse_bucket"
    else
      let existed = Hashtbl.mem t.rows r.node_id in
      Hashtbl.replace t.rows r.node_id r;
      if existed then Refreshed else Added