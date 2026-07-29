(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type io = {
  guard : Octra_net.P2p_tx_gossip_guard.t;
  payload : string;
  peer_id : string;
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

val handle_tx : io -> Octra_core.Transaction.t -> unit

val handle_legacy : io -> unit

val handle_plan : io -> unit

val handle : io -> unit