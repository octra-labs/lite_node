(*
Octra Labs 2026

Lite node, for internal use only (pre-release build 0x1067dzc2)

Include at startup:
- compiler
- env-constructor
- binary-proto consensus for updates
- PVAC (optimized version, build 0f24dd-2025)
- libp2p
- gRPC (version 9738fdy44-2025)
*)


type tx_callbacks = {
  guard : Octra_net.P2p_tx_gossip_guard.t;
  has_tx : string -> bool;
  find_tx : string -> Octra_core.Transaction.t option;
  sender_pk : Octra_core.Transaction.t -> string option;
  add_tx : Octra_core.Transaction.t -> (string, string) result;
  now : unit -> float;
  max_drift : float;
}

type transport = {
  send_tx_gossip : string -> unit;
  broadcast_tx_gossip : string -> unit;
  report_bad_peer : string -> unit;
  close_peer : unit -> unit;
}

type deps = {
  observer : bool;
  tx : tx_callbacks;
  on_consensus : Octra_net.P2p_conn.t -> Octra_net.P2p_frame.frame -> unit;
}

type node_deps = {
  observer : bool;
  guard : Octra_net.P2p_tx_gossip_guard.t;
  find_tx : string -> Octra_core.Transaction.t option;
  find_account : string -> Octra_core.Ledger.account option;
  add_tx : Octra_core.Transaction.t -> (string, string) result;
  now : unit -> float;
  max_drift : float;
  driver_ref : Octra_consensus.C_driver.t option ref;
}

val node_tx_callbacks :
  node_deps ->
  tx_callbacks

val node_lifecycle_deps :
  node_deps ->
  deps

val handle_frame_with_transport :
  observer:bool ->
  peer_id:string ->
  tx:tx_callbacks ->
  transport:transport ->
  on_consensus:(unit -> unit) ->
  Octra_net.P2p_frame.frame ->
  unit

val start :
  deps ->
  Octra_net.P2p_swarm.t ->
  unit Lwt.t

val node_task :
  swarm:Octra_net.P2p_swarm.t option ->
  node_deps ->
  unit Lwt.t option