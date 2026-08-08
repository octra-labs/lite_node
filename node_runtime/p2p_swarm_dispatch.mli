(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type tx_deps = {
  guard : Octra_net.P2p_tx_gossip_guard.t;
  has_tx : string -> bool;
  find_tx : string -> Octra_core.Transaction.t option;
  sender_pk : Octra_core.Transaction.t -> string option;
  add_tx : Octra_core.Transaction.t -> (string, string) result;
  send_payload : string -> unit;
  broadcast_payload : string -> unit;
  report_bad_peer : string -> unit;
  close_peer : unit -> unit;
  now : unit -> float;
  max_drift : float;
}

type deps = {
  observer : bool;
  peer_id : string;
  tx : tx_deps;
  on_consensus : unit -> unit;
  on_resource_compute : unit -> unit;
}

val handle_frame :
  deps ->
  Octra_net.P2p_frame.frame ->
  unit