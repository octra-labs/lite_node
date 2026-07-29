(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

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

let frame_tx_gossip payload =
  Octra_net.P2p_frame.{
    msg_type = msg_tx_gossip;
    payload;
  }

let transport_of_peer swarm conn =
  {
    send_tx_gossip = (fun payload ->
      Lwt.async (fun () ->
        Octra_net.P2p_conn.send conn (frame_tx_gossip payload)));
    broadcast_tx_gossip = (fun payload ->
      Lwt.async (fun () ->
        Octra_net.P2p_swarm.broadcast_except swarm
          ~except:conn.Octra_net.P2p_conn.peer_id
          (frame_tx_gossip payload)));
    report_bad_peer = (fun reason ->
      Octra_net.P2p_swarm.report_bad_peer swarm conn ~reason);
    close_peer = (fun () ->
      Lwt.async (fun () -> Octra_net.P2p_conn.close conn));
  }

let dispatch_tx_deps
    (tx : tx_callbacks)
    (transport : transport) : P2p_swarm_dispatch.tx_deps =
  P2p_swarm_dispatch.{
    guard = tx.guard;
    has_tx = tx.has_tx;
    find_tx = tx.find_tx;
    sender_pk = tx.sender_pk;
    add_tx = tx.add_tx;
    send_payload = transport.send_tx_gossip;
    broadcast_payload = transport.broadcast_tx_gossip;
    report_bad_peer = transport.report_bad_peer;
    close_peer = transport.close_peer;
    now = tx.now;
    max_drift = tx.max_drift;
  }

let handle_frame_with_transport
    ~observer
    ~peer_id
    ~(tx : tx_callbacks)
    ~(transport : transport)
    ~on_consensus
    frame =
  P2p_swarm_dispatch.handle_frame
    {
      observer;
      peer_id;
      tx = dispatch_tx_deps tx transport;
      on_consensus;
    }
    frame

let handle_frame (deps : deps) swarm conn frame =
  handle_frame_with_transport
    ~observer:deps.observer
    ~peer_id:conn.Octra_net.P2p_conn.peer_id
    ~tx:deps.tx
    ~transport:(transport_of_peer swarm conn)
    ~on_consensus:(fun () -> deps.on_consensus conn frame)
    frame;
  Lwt.return_unit

let start deps swarm =
  Octra_net.P2p_swarm.start swarm ~on_message:(handle_frame deps swarm)

let node_tx_callbacks (deps : node_deps) =
  {
    guard = deps.guard;
    has_tx = (fun hash -> deps.find_tx hash <> None);
    find_tx = deps.find_tx;
    sender_pk = (fun tx ->
      Tx_sender_key.resolve
        ~find_account:deps.find_account
        tx);
    add_tx = deps.add_tx;
    now = deps.now;
    max_drift = deps.max_drift;
  }

let forward_consensus driver_ref conn frame =
  match !driver_ref with
  | Some driver ->
    Lwt.async (fun () ->
      Octra_consensus.C_driver.on_p2p_message driver conn frame)
  | None -> ()

let node_lifecycle_deps (deps : node_deps) =
  {
    observer = deps.observer;
    tx = node_tx_callbacks deps;
    on_consensus = forward_consensus deps.driver_ref;
  }

let node_task ~swarm deps =
  Option.map (start (node_lifecycle_deps deps)) swarm