(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let fail msg = failwith ("test_node_runtime_p2p_tx_handler: " ^ msg)

let h c =
  String.make 64 c

let assert_true msg v =
  if not v then fail msg

let key () =
  let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let priv_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets priv) in
  let pub_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets pub) in
  let addr = Octra_core.Crypto.Address.address_from_pubkey pub_b64 in
  addr, priv_b64, pub_b64

let tx ~from ~to_ ~timestamp =
  Octra_core.Transaction.{
    from;
    to_;
    amount = Z.of_int 1;
    nonce = 1;
    ou = Z.of_int 1;
    timestamp;
    signature = "";
    public_key = None;
    message = None;
    op_type = Standard;
    encrypted_data = None;
  }

let signed ~priv tx =
  Octra_core.Transaction.sign_with_privkey tx priv

let io ?(payload="") ?(has_tx=fun _ -> false) ?(find_tx=fun _ -> None)
    ?(sender_pk=fun _ -> None) ?(add_tx=fun _ -> Error "reject")
    ?(max_drift=300.0) () =
  let sent = ref [] in
  let broadcast = ref [] in
  let reported = ref [] in
  let closed = ref false in
  let io = Octra_node_runtime.P2p_tx_handler.{
    guard = Octra_net.P2p_tx_gossip_guard.create ();
    payload;
    peer_id = "peer1234567890";
    has_tx;
    find_tx;
    sender_pk;
    add_tx;
    send_payload = (fun payload -> sent := payload :: !sent);
    broadcast_payload = (fun payload -> broadcast := payload :: !broadcast);
    report_bad_peer = (fun reason -> reported := reason :: !reported);
    close_peer = (fun () -> closed := true);
    now = (fun () -> 1000.0);
    max_drift;
  } in
  io, sent, broadcast, reported, closed

let test_inv_sends_get () =
  let a = h 'a' in
  let b = h 'b' in
  let payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Inv [a; b]) in
  let io, sent, _, _, _ =
    io ~payload ~has_tx:(fun hash -> hash = b) ()
  in
  Octra_node_runtime.P2p_tx_handler.handle io;
  match !sent with
  | [payload] ->
    begin
      match Octra_net.P2p_tx_gossip.decode payload with
      | Octra_net.P2p_tx_gossip.Get [hash] -> assert_true "requested a" (hash = a)
      | _ -> fail "unexpected get payload"
    end
  | _ -> fail "expected one get payload"

let test_get_sends_tx () =
  let from, priv, _pub = key () in
  let to_, _, _ = key () in
  let tx = tx ~from ~to_ ~timestamp:1000.0 |> signed ~priv in
  let hash = Octra_core.Transaction.hash tx in
  let payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Get [hash]) in
  let io, sent, _, _, _ =
    io ~payload ~has_tx:(fun h -> h = hash) ~find_tx:(fun h -> if h = hash then Some tx else None) ()
  in
  Octra_node_runtime.P2p_tx_handler.handle io;
  match !sent with
  | [payload] ->
    begin
      match Octra_net.P2p_tx_gossip.decode payload with
      | Octra_net.P2p_tx_gossip.Tx got -> assert_true "served hash" (got.hash = hash)
      | _ -> fail "unexpected tx payload"
    end
  | _ -> fail "expected one tx payload"

let test_receive_broadcasts_inv () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let tx = tx ~from ~to_ ~timestamp:1000.0 |> signed ~priv in
  let hash = Octra_core.Transaction.hash tx in
  let tx_json = Yojson.Safe.to_string (Octra_core.Transaction.to_yojson tx) in
  let payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx { hash; tx_json }) in
  let io, _, broadcast, _, _ =
    io
      ~payload
      ~sender_pk:(fun _ -> Some pub)
      ~add_tx:(fun _ -> Ok hash)
      ()
  in
  Octra_node_runtime.P2p_tx_handler.handle io;
  match !broadcast with
  | [payload] ->
    begin
      match Octra_net.P2p_tx_gossip.decode payload with
      | Octra_net.P2p_tx_gossip.Inv [got] -> assert_true "broadcast hash" (got = hash)
      | _ -> fail "unexpected broadcast payload"
    end
  | _ -> fail "expected one broadcast payload"

let test_drop_reports_and_closes () =
  let payload = String.make (Octra_net.P2p_tx_gossip_guard.default.max_payload_bytes + 1) 'x' in
  let io, _, _, reported, closed = io ~payload () in
  Octra_node_runtime.P2p_tx_handler.handle io;
  assert_true "reported drop" (!reported = ["payload_too_large"]);
  assert_true "closed peer" !closed

let test_seen_window () =
  let module Seen = Octra_net.P2p_tx_seen in
  let seen, first = Seen.step Seen.empty ~now:10. "one" in
  assert_true "first message" first;
  let seen, repeat = Seen.step seen ~now:10.5 "one" in
  assert_true "same message" (not repeat);
  let seen, retry = Seen.step seen ~now:11. "one" in
  assert_true "retry after window" retry;
  let _, rewind = Seen.step seen ~now:9. "one" in
  assert_true "clock moved back" rewind;
  let reset, finite = Seen.step seen ~now:nan "one" in
  assert_true "invalid clock resets" (finite && Seen.size reset = 0);
  let full = List.init (Seen.capacity + 1) string_of_int
    |> List.fold_left (fun seen key -> fst (Seen.step seen ~now:12. key)) Seen.empty
  in
  assert_true "finite entries" (Seen.size full = Seen.capacity);
  let expired, fresh = Seen.step full ~now:13. "next" in
  assert_true "old entries expire" (fresh && Seen.size expired = 1)

let test_repeat_admission () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let tx = tx ~from ~to_ ~timestamp:1000.0 |> signed ~priv in
  let hash = Octra_core.Transaction.hash tx in
  let checks = ref 0 in
  let calls = ref 0 in
  let clock = ref 1000.0 in
  let accept = ref false in
  let io, _, broadcast, _, _ = io
    ~sender_pk:(fun _ -> incr checks; Some pub)
    ~add_tx:(fun _ -> incr calls; if !accept then Ok hash else Error "queue changed") ()
  in
  let io = { io with now = (fun () -> !clock) } in
  let handle peer tx =
    let tx_json = Yojson.Safe.to_string (Octra_core.Transaction.to_yojson tx) in
    let payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx { hash; tx_json }) in
    Octra_node_runtime.P2p_tx_handler.handle { io with peer_id = peer; payload }
  in
  handle "peer-a" tx;
  handle "peer-b" tx;
  assert_true "repeat skipped before signature" (!checks = 1 && !calls = 1);
  assert_true "rejected message not relayed" (!broadcast = []);
  clock := 1000.5;
  handle "peer-c" tx;
  clock := 1001.0;
  accept := true;
  handle "peer-b" tx;
  assert_true "state may change during retry" (!checks = 2 && !calls = 2);
  assert_true "accepted retry relayed" (List.length !broadcast = 1)

let test_signature_identity () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let tx = tx ~from ~to_ ~timestamp:1000.0 |> signed ~priv in
  let hash = Octra_core.Transaction.hash tx in
  let calls = ref 0 in
  let io, _, broadcast, _, _ = io
    ~sender_pk:(fun _ -> Some pub)
    ~add_tx:(fun _ -> incr calls; Ok hash) ()
  in
  let bad = { tx with signature = Base64.encode_exn (String.make 64 '\x00') } in
  assert_true "signature changes transaction hash" (Octra_core.Transaction.hash bad <> hash);
  Octra_node_runtime.P2p_tx_handler.handle_tx io bad;
  Octra_node_runtime.P2p_tx_handler.handle_tx io tx;
  assert_true "signature belongs to cache key" (!calls = 1 && List.length !broadcast = 1);
  let next = { tx with public_key = Some pub } in
  assert_true "public key changes transaction hash" (Octra_core.Transaction.hash next <> hash);
  Octra_node_runtime.P2p_tx_handler.handle_tx io next;
  assert_true "public key belongs to cache key" (!calls = 2);
  let raised = { tx with ou = Z.mul tx.ou (Z.of_int 2) } |> signed ~priv in
  assert_true "fee changes transaction hash" (Octra_core.Transaction.hash raised <> hash);
  Octra_node_runtime.P2p_tx_handler.handle_tx io raised;
  assert_true "fee belongs to cache key" (!calls = 3)

let () =
  Mirage_crypto_rng_unix.use_default ();
  test_inv_sends_get ();
  test_get_sends_tx ();
  test_receive_broadcasts_inv ();
  test_drop_reports_and_closes ();
  test_seen_window ();
  test_repeat_admission ();
  test_signature_identity ();
  print_endline "node runtime p2p tx handler tests passed"