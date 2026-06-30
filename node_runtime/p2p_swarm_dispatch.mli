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
}

val handle_frame :
  deps ->
  Octra_net.P2p_frame.frame ->
  unit