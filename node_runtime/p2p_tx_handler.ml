(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type io = {
  signing : unit -> Octra_core.Tx_signature.t;
  guard : Octra_net.P2p_tx_gossip_guard.t;
  payload : string;
  peer_id : string;
  has_tx : string -> bool;
  find_tx : string -> Octra_core.Transaction.t option;
  sender_pk : Octra_core.Transaction.t -> string option;
  find_account : string -> Octra_core.Ledger.account option;
  add_tx : Octra_core.Transaction.t -> (string, string) result;
  send_payload : string -> unit;
  send_batch : (unit -> string option) list -> unit;
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

let receive_tx ?duty ?(bft_mode = false) io tx =
  let open Lwt.Syntax in
  let tx_hash = Octra_core.Transaction.hash tx in
  if io.has_tx tx_hash then Lwt.return_unit
  else
    let now = io.now () in
    if not (Octra_net.P2p_tx_gossip_guard.remember io.guard ~now tx_hash) then Lwt.return_unit
    else
    let sender_pk = io.sender_pk tx in
    let h12 = Text.hash_short tx_hash in
    let account_check () = Tx_view.payload_account_check ~bft_mode
      ~bft_window:(max 1 (Env.int_value "OCTRA_BFT_PENDING_NONCE_WINDOW" 32))
      ~min_ou:(Octra_core.Transaction.stealth_min_ou_of Sys.getenv_opt)
      ~min_relay_fee:(Octra_core.Tx_staging.min_relay_fee tx)
      ~account:(io.find_account tx.Octra_core.Transaction.from) tx in
    let* verdict = match account_check () with
      | Error (code, _) -> Lwt.return (P2p_tx_admit.Sender_refused code)
      | Ok () -> P2p_tx_admit.admit ?duty ~bft_mode
        ~signing:(io.signing ()) ~now ~max_drift:io.max_drift ~sender_pk tx in
    let verdict = match verdict with
      | P2p_tx_admit.Accept ->
        (match account_check () with
        | Error (code, _) -> P2p_tx_admit.Sender_refused code
        | Ok () -> P2p_tx_admit.sender_check ?duty ~bft_mode ~signing:(io.signing ())
          ~now:(io.now ()) ~max_drift:io.max_drift ~sender_pk:(io.sender_pk tx) tx)
      | verdict -> verdict in
    begin match verdict with
    | P2p_tx_admit.Invalid_address ->
      Log.warn "p2p" "tx_gossip rejected: invalid_address hash = %s" h12
    | P2p_tx_admit.Invalid_payload ->
      Log.warn "p2p" "tx_gossip rejected: invalid_payload hash = %s" h12
    | P2p_tx_admit.Timestamp_drift drift ->
      Log.warn "p2p" "tx_gossip rejected: timestamp_drift hash = %s drift = %.0fs" h12 drift
    | P2p_tx_admit.Invalid_signature ->
      io.report_bad_peer "invalid_signature";
      Log.warn "p2p" "tx_gossip rejected: invalid_signature hash = %s from = %s"
        h12
        (Text.addr14 tx.Octra_core.Transaction.from)
    | P2p_tx_admit.Payload_unavailable reason ->
      Log.warn "p2p" "event = tx_parse_deferred hash = %s reason = %s" h12 reason
    | P2p_tx_admit.Sender_refused reason ->
      Log.warn "p2p" "event = tx_sender_refused hash = %s reason = %s" h12 reason
    | P2p_tx_admit.Accept ->
      if not (io.has_tx tx_hash) then
      match io.add_tx tx with
      | Ok tx_hash -> broadcast_inv io tx_hash
      | Error e ->
        Log.warn "p2p" "event = tx_queue_refused hash = %s reason = %s" h12 e
    end;
    Lwt.return_unit

let handle_tx ?duty ?bft_mode io tx =
  let received_hash = Octra_core.Transaction.hash tx in
  let now = io.now () in
  if io.has_tx received_hash
     || Octra_net.P2p_tx_gossip_guard.recent io.guard ~now received_hash then Lwt.return_unit
  else
    match Octra_core.Tx_envelope.normalize ~sender_pk:(io.sender_pk tx) tx with
    | Error (code, reason) ->
      ignore (Octra_net.P2p_tx_gossip_guard.remember io.guard ~now received_hash);
      Log.warn "p2p" "event = tx_envelope_refused code = %s reason = %s" code reason;
      Lwt.return_unit
    | Ok tx ->
      if received_hash <> Octra_core.Transaction.hash tx then
        ignore (Octra_net.P2p_tx_gossip_guard.remember io.guard ~now received_hash);
      receive_tx ?duty ?bft_mode io tx

let handle_legacy ?duty ?bft_mode io =
  let decoded =
    try
      let c = Octra_net.Oce1.make_cursor io.payload in
      let tx_json = Octra_net.Oce1.get_string c in
      Tx_view.decode_network_tx (Octra_core.Json_tree.read tx_json)
    with
    | (Out_of_memory | Stack_overflow | Lwt.Canceled) as error -> raise error
    | error -> Error (Printexc.to_string error) in
  match decoded with
  | Ok tx -> handle_tx ?duty ?bft_mode io tx
  | Error e ->
    Log.warn "p2p" "tx_gossip decode failed: %s" e;
    Lwt.return_unit

let refused_key io hash = "refused:" ^ io.peer_id ^ ":" ^ hash

let handle_plan ?duty ?bft_mode io =
  match P2p_tx_plan.plan ~has:io.has_tx io.payload with
  | P2p_tx_plan.Request missing ->
    let now = io.now () in
    let missing = List.filter (fun hash ->
      not (Octra_net.P2p_tx_gossip_guard.recent io.guard ~now hash)
      && not (Octra_net.P2p_tx_gossip_guard.recent io.guard ~now (refused_key io hash))) missing in
    if missing <> [] then send_msg io (Octra_net.P2p_tx_gossip.Get missing);
    Lwt.return_unit
  | P2p_tx_plan.Serve hits ->
    let find = io.find_tx in
    io.send_batch (List.map
      (fun hash () ->
        match find hash with
        | None -> None
        | Some tx ->
          let tx_json = Yojson.Safe.to_string (Octra_core.Transaction.to_yojson tx) in
          if String.length tx_json > Octra_net.P2p_tx_gossip.max_tx_json then begin
            Log.warn "p2p" "event = tx_reply_refused hash = %s reason = json_size bytes = %d"
              (Text.hash_short hash) (String.length tx_json);
            None
          end else
            Some (Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx {hash; tx_json})))
      hits);
    Lwt.return_unit
  | P2p_tx_plan.Receive { hash; tx_json } ->
    let now = io.now () in
    if io.has_tx hash || Octra_net.P2p_tx_gossip_guard.recent io.guard ~now hash
       || Octra_net.P2p_tx_gossip_guard.recent io.guard ~now (refused_key io hash) then Lwt.return_unit
    else
    begin
      match P2p_tx_plan.check_tx ~hash ~tx_json with
      | P2p_tx_plan.Decode_error e ->
        ignore (Octra_net.P2p_tx_gossip_guard.remember io.guard ~now (refused_key io hash));
        Log.warn "p2p" "tx_gossip tx decode failed: %s" e;
        Lwt.return_unit
      | P2p_tx_plan.Hash_mismatch got ->
        ignore (Octra_net.P2p_tx_gossip_guard.remember io.guard ~now (refused_key io hash));
        Log.warn "p2p" "tx_gossip rejected: tx_hash_mismatch advertised = %s actual = %s"
          (Text.hash_short got.advertised)
          (Text.hash_short got.actual);
        Lwt.return_unit
      | P2p_tx_plan.Decoded tx -> handle_tx ?duty ?bft_mode io tx
    end
  | P2p_tx_plan.Legacy -> handle_legacy ?duty ?bft_mode io

let handle ?duty ?bft_mode io =
  match Octra_net.P2p_tx_gossip_guard.admit io.guard
    ~now:(io.now ())
    ~peer:io.peer_id
    ~bytes:(String.length io.payload) with
  | Octra_net.P2p_tx_gossip_guard.Drop reason ->
    Log.warn "p2p" "tx_gossip peer dropped peer = %s reason = %s"
      (Text.peer_short io.peer_id)
      reason;
    io.report_bad_peer reason;
    io.close_peer ();
    Lwt.return_unit
  | Octra_net.P2p_tx_gossip_guard.Accept ->
    Lwt.catch
      (fun () -> handle_plan ?duty ?bft_mode io)
      (function
        | Lwt.Canceled as error -> Lwt.fail error
        | exn ->
          Log.warn "p2p" "tx_gossip exception: %s" (Printexc.to_string exn);
          Lwt.return_unit)