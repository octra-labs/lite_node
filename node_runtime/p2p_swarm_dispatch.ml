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

let log_recv frame =
  Log.trace "p2p" "recv msg_type = %s len = %d"
    (Text.msg_hex frame.Octra_net.P2p_frame.msg_type)
    (Text.frame_len frame)

let handle_legacy deps frame =
  match P2p_epoch_broadcast.observer_event
          ~observer:deps.observer frame.Octra_net.P2p_frame.payload with
  | Ok P2p_epoch_broadcast.Silent -> ()
  | Ok (P2p_epoch_broadcast.Ignored row) ->
    Log.trace "shadow"
      "legacy_epoch_broadcast = ignored epoch = %d txs = %d root = %s producer = %s"
      row.epoch
      row.tx_count
      row.root
      row.producer
  | Error e ->
    Log.warn "consensus"
      "epoch_broadcast = decode_failed reason = %s"
      e

let handle_tx deps frame =
  P2p_tx_handler.handle P2p_tx_handler.{
    guard = deps.tx.guard;
    payload = frame.Octra_net.P2p_frame.payload;
    peer_id = deps.peer_id;
    has_tx = deps.tx.has_tx;
    find_tx = deps.tx.find_tx;
    sender_pk = deps.tx.sender_pk;
    add_tx = deps.tx.add_tx;
    send_payload = deps.tx.send_payload;
    broadcast_payload = deps.tx.broadcast_payload;
    report_bad_peer = deps.tx.report_bad_peer;
    close_peer = deps.tx.close_peer;
    now = deps.tx.now;
    max_drift = deps.tx.max_drift;
  }

let handle_unknown frame t =
  Log.info "consensus" "p2p_msg = unknown msg_type = %s len = %d"
    (Text.msg_hex t)
    (Text.frame_len frame)

let handle_frame deps frame =
  log_recv frame;
  match P2p_message.classify frame.Octra_net.P2p_frame.msg_type with
  | P2p_message.Legacy_epoch -> handle_legacy deps frame
  | P2p_message.Tx_gossip -> handle_tx deps frame
  | P2p_message.Consensus -> deps.on_consensus ()
  | P2p_message.Unknown t -> handle_unknown frame t