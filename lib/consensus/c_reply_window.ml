(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Peers = Set.Make (String)

type t = {
  deadline : int64;
  requested : Peers.t;
  consumed : Peers.t;
}

type decision =
  | Read of t
  | Expired
  | Repeated
  | Unexpected_peer

let create ~deadline peers = {
  deadline;
  requested = Peers.of_list peers;
  consumed = Peers.empty;
}

let extend peers window =
  {window with requested = List.fold_left
    (fun set peer -> Peers.add peer set) window.requested peers}

let take ~now ~peer window =
  if Int64.compare now window.deadline >= 0 then Expired
  else if not (Peers.mem peer window.requested) then Unexpected_peer
  else if Peers.mem peer window.consumed then Repeated
  else Read {window with consumed = Peers.add peer window.consumed}