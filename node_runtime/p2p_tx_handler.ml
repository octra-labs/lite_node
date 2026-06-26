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

let send_msg io msg =
  io.send_payload (Octra_net.P2p_tx_gossip.encode msg)

let broadcast_inv io hash =
  io.broadcast_payload
    (Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Inv [hash]))

let handle_tx io tx =
  let tx_hash = Octra_core.Transaction.hash tx in
  if io.has_tx tx_hash then ()
  else
    let now = io.now () in
    let sender_pk = io.sender_pk tx in
    let h12 = Text.hash_short tx_hash in
    match P2p_tx_admit.admit ~now ~max_drift:io.max_drift ~sender_pk tx with
    | P2p_tx_admit.Invalid_address ->
      Log.warn "p2p" "tx_gossip rejected: invalid_address hash = %s" h12
    | P2p_tx_admit.Timestamp_drift drift ->
      Log.warn "p2p" "tx_gossip rejected: timestamp_drift hash = %s drift = %.0fs" h12 drift
    | P2p_tx_admit.Invalid_signature ->
      Log.warn "p2p" "tx_gossip rejected: invalid_signature hash = %s from = %s"
        h12
        (Text.addr14 tx.Octra_core.Transaction.from)
    | P2p_tx_admit.Accept ->
      match io.add_tx tx with
      | Ok tx_hash -> broadcast_inv io tx_hash
      | Error e ->
        Log.warn "p2p" "tx_gossip rejected: admission hash = %s reason = %s" h12 e

let handle_legacy io =
  let c = Octra_net.Oce1.make_cursor io.payload in
  let tx_json = Octra_net.Oce1.get_string c in
  match Octra_core.Transaction.of_yojson (Yojson.Safe.from_string tx_json) with
  | Ok tx -> handle_tx io tx
  | Error e -> Log.warn "p2p" "tx_gossip decode failed: %s" e

let handle_plan io =
  match P2p_tx_plan.plan ~has:io.has_tx io.payload with
  | P2p_tx_plan.Request missing ->
    if missing <> [] then send_msg io (Octra_net.P2p_tx_gossip.Get missing)
  | P2p_tx_plan.Serve hits ->
    List.iter
      (fun hash ->
        match io.find_tx hash with
        | None -> ()
        | Some tx ->
          let tx_json = Yojson.Safe.to_string (Octra_core.Transaction.to_yojson tx) in
          send_msg io (Octra_net.P2p_tx_gossip.Tx { hash; tx_json }))
      hits
  | P2p_tx_plan.Receive { hash; tx_json } ->
    begin
      match P2p_tx_plan.check_tx ~hash ~tx_json with
      | P2p_tx_plan.Decode_error e ->
        Log.warn "p2p" "tx_gossip tx decode failed: %s" e
      | P2p_tx_plan.Hash_mismatch got ->
        Log.warn "p2p" "tx_gossip rejected: tx_hash_mismatch advertised = %s actual = %s"
          (Text.hash_short got.advertised)
          (Text.hash_short got.actual)
      | P2p_tx_plan.Decoded tx -> handle_tx io tx
    end
  | P2p_tx_plan.Legacy -> handle_legacy io

let handle io =
  match Octra_net.P2p_tx_gossip_guard.admit io.guard
    ~now:(io.now ())
    ~peer:io.peer_id
    ~bytes:(String.length io.payload) with
  | Octra_net.P2p_tx_gossip_guard.Drop reason ->
    Log.warn "p2p" "tx_gossip peer dropped peer = %s reason = %s"
      (Text.peer_short io.peer_id)
      reason;
    io.report_bad_peer reason;
    io.close_peer ()
  | Octra_net.P2p_tx_gossip_guard.Accept ->
    try handle_plan io
    with _ ->
      try handle_legacy io
      with exn ->
        Log.warn "p2p" "tx_gossip exception: %s" (Printexc.to_string exn)