(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type deps = {
  p2p_port : int;
  rpc : unit -> unit Lwt.t;
  services : (unit -> unit Lwt.t) list;
  observer : bool;
  follow : bool;
  tick_loop : unit -> unit Lwt.t;
  swarm : Octra_net.P2p_swarm.t option;
  guard : Octra_net.P2p_tx_gossip_guard.t;
  find_tx : string -> Octra_core.Transaction.t option;
  find_account : string -> Octra_core.Ledger.account option;
  add_tx : Octra_core.Transaction.t -> (string, string) result;
  now : unit -> float;
  max_drift : float;
  driver_ref : Octra_consensus.C_driver.t option ref;
  resource_compute : Resource_compute_service.t option;
  close_chaindata : unit -> unit;
  exit_fatal : unit -> unit;
}

val run :
  deps ->
  unit