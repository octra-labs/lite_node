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
    ?(find_account = fun _ -> None)
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
    find_account;
    add_tx;
    send_payload = (fun payload -> sent := payload :: !sent);
    send_batch = (List.iter (fun build -> Option.iter (fun payload -> sent := payload :: !sent) (build ())));
    broadcast_payload = (fun payload -> broadcast := payload :: !broadcast);
    report_bad_peer = (fun reason -> reported := reason :: !reported);
    close_peer = (fun () -> closed := true);
    now = (fun () -> 1000.0);
    max_drift;
    signing = (fun () -> Octra_core.Tx_signature.Historical);
  } in
  io, sent, broadcast, reported, closed

let test_inv_sends_get () =
  let a = h 'a' in
  let b = h 'b' in
  let payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Inv [a; b]) in
  let io, sent, _, _, _ =
    io ~payload ~has_tx:(fun hash -> hash = b) ()
  in
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle io);
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
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle io);
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
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle io);
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
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle io);
  assert_true "reported drop" (!reported = ["payload_too_large"]);
  assert_true "closed peer" !closed

let test_network_numbers () =
  let module T = Octra_core.Transaction in
  let module G = Octra_net.P2p_tx_gossip in
  let module Plan = Octra_node_runtime.P2p_tx_plan in
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let item = tx ~from ~to_ ~timestamp:1000. |> signed ~priv in
  let hash = T.hash item in
  let fields = match T.to_yojson item with `Assoc fields -> fields | _ -> assert false in
  let payloads json = [G.encode (G.Tx {hash; tx_json = json});
    Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer json)] in
  let rejected json =
    assert_true "invalid number reached hash comparison"
      (match Plan.check_tx ~hash ~tx_json:json with Plan.Decode_error _ -> true | _ -> false);
    List.iter (fun payload ->
      let source, _, broadcast, reported, closed = io ~payload
        ~sender_pk:(fun _ -> fail "invalid number reached signature check")
        ~add_tx:(fun _ -> fail "invalid number reached staging") () in
      Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle source);
      assert_true "local decode refusal penalized relay" (!reported = []);
      assert_true "invalid number relayed or closed peer" (!broadcast = [] && not !closed))
      (payloads json) in
  List.iter (fun field ->
    let raw = String.make (T.network_number_chars + 1) '1' in
    List.iter (fun json -> rejected (Yojson.Safe.to_string json)) [
      `Assoc ((field, `String raw) :: List.remove_assoc field fields);
      `Assoc (fields @ [field, `String raw]);
      `Assoc ((field, `String "not-a-number") :: List.remove_assoc field fields)])
    ["amount"; "ou"];
  rejected "{";
  List.iter (fun raw ->
    let json = `Assoc (("amount", `String raw) :: ("ou", `String raw)
      :: List.remove_assoc "ou" (List.remove_assoc "amount" fields)) |> Yojson.Safe.to_string in
    assert_true "numeric spelling changed signed hash"
      (match Plan.check_tx ~hash ~tx_json:json with Plan.Decoded got -> got = item | _ -> false);
    List.iter (fun payload ->
      let source, _, broadcast, reported, _ = io ~payload
        ~sender_pk:(fun _ -> Some pub) ~add_tx:(fun _ -> Ok hash) () in
      Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle source);
      assert_true "valid numeric spelling refused" (List.length !broadcast = 1 && !reported = []))
      (payloads json)) ["+0b1"; "0x1"; "0o1"; String.make (T.network_number_chars - 1) '0' ^ "1"];
  let tx_json = Yojson.Safe.to_string (T.to_yojson item) in
  let payload = G.encode (G.Tx {hash = h 'f'; tx_json}) in
  let source, _, broadcast, reported, _ = io ~payload
    ~sender_pk:(fun _ -> fail "hash mismatch reached signature check") () in
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle source);
  assert_true "hash mismatch penalized relay" (!reported = [] && !broadcast = [])

let test_prior_relay () =
  let module T = Octra_core.Transaction in
  let module G = Octra_net.P2p_tx_gossip in
  let module Guard = Octra_net.P2p_tx_gossip_guard in
  let module Handler = Octra_node_runtime.P2p_tx_handler in
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let clock = ref 1000. in
  let source, sent, broadcast, reported, closed = io
    ~sender_pk:(fun _ -> Some pub) ~add_tx:(fun item -> Ok (T.hash item)) () in
  let source = {source with Handler.now = (fun () -> !clock)} in
  let invoke peer payload = Lwt_main.run (Handler.handle {source with peer_id = peer; payload}) in
  List.iter (fun nonce ->
    let item = {(tx ~from ~to_ ~timestamp:1000.) with T.nonce = nonce; public_key = Some pub;
      op_type = DecryptOp; amount = Z.of_string (String.make (T.network_number_chars + 1) '1')}
      |> signed ~priv in
    let hash = T.hash item in
    let json = T.to_yojson item in
    assert_true "prior relay case no longer decodes" (T.of_yojson json = Ok item);
    assert_true "prior relay case is not signed" (T.verify item pub);
    let tx_json = Yojson.Safe.to_string json in
    invoke "prior-a" (G.encode (G.Tx {hash; tx_json}));
    assert_true "rejected advertised hash poisoned shared cache"
      (not (Guard.recent source.guard ~now:!clock hash));
    invoke "prior-a" (G.encode (G.Inv [hash]));
    assert_true "rejected relay hash requested again" (!sent = []);
    invoke "prior-b" (G.encode (G.Inv [hash]));
    assert_true "rejected relay suppressed other peer"
      (match !sent with [payload] -> G.decode payload = G.Get [hash] | _ -> false);
    sent := [];
    clock := !clock +. 0.5;
    invoke "prior-a" (G.encode (G.Tx {hash; tx_json}));
    invoke "prior-b" (G.encode (G.Tx {hash; tx_json = "{"}));
    assert_true "prior relay punished or disconnected" (!reported = [] && not !closed);
    assert_true "prior relay admitted rejected transaction" (!broadcast = []);
    clock := !clock +. 0.5;
    invoke "prior-a" (G.encode (G.Inv [hash]));
    assert_true "rejected relay hash never retried"
      (match !sent with [payload] -> G.decode payload = G.Get [hash] | _ -> false);
    sent := []) [1; 2; 3];
  let item = {(tx ~from ~to_ ~timestamp:1000.) with T.public_key = Some pub} |> signed ~priv in
  let hash = T.hash item in
  invoke "prior-a" (G.encode (G.Tx {hash; tx_json = Yojson.Safe.to_string (T.to_yojson item)}));
  assert_true "prior relay lost valid transactions"
    (match !broadcast with [payload] -> G.decode payload = G.Inv [hash] | _ -> false);
  assert_true "prior relay accumulated punishment" (!reported = [] && not !closed)

let test_payload_relay () =
  let module T = Octra_core.Transaction in
  let module G = Octra_net.P2p_tx_gossip in
  let module V = Octra_node_runtime.Tx_view in
  let module H = Octra_node_runtime.P2p_tx_handler in
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  List.iter (fun legacy ->
    let clock = ref 1000. in
    let added = ref [] in
    let source, sent, broadcast, reported, closed = io
      ~sender_pk:(fun _ -> Some pub)
      ~add_tx:(fun item -> added := item :: !added; Ok (T.hash item)) () in
    let source = {source with H.now = (fun () -> !clock)} in
    let invoke payload = Lwt_main.run (H.handle {source with payload}) in
    List.iter (fun nonce ->
      let item = {(tx ~from ~to_ ~timestamp:1000.) with
        T.op_type = ProgramExec; nonce; amount = Z.zero; ou = Z.of_int 10000;
        public_key = Some pub; message = Some "[\"echo\",\"<b>value</b>\"]"}
        |> signed ~priv in
      assert_true "relay case signature invalid" (T.verify item pub);
      assert_true "relay case exceeds payload size"
        (V.payload_size_check ~limits:V.payload_limits item = Ok ());
      assert_true "relay case passed local parameter check" (not (V.call_params_ok item));
      assert_true "relay case did not reach payload refusal"
        (Lwt_main.run (Octra_node_runtime.P2p_tx_admit.admit
          ~signing:Octra_core.Tx_signature.Historical ~now:!clock ~max_drift:300.
          ~sender_pk:(Some pub) item) = Octra_node_runtime.P2p_tx_admit.Invalid_payload);
      let hash = T.hash item in
      let tx_json = Yojson.Safe.to_string (T.to_yojson item) in
      let payload = if legacy then Octra_net.Oce1.encode (fun buffer ->
          Octra_net.Oce1.put_string buffer tx_json)
        else G.encode (G.Tx {hash; tx_json}) in
      invoke payload;
      assert_true "local payload refusal punished relay" (!reported = [] && not !closed);
      assert_true "local payload refusal reached queue" (!added = [] && !broadcast = []);
      invoke (G.encode (G.Inv [hash]));
      assert_true "refused payload requested again" (!sent = []);
      clock := !clock +. 1.;
      invoke (G.encode (G.Inv [hash]));
      assert_true "refused payload never retried"
        (List.map G.decode !sent = [G.Get [hash]]);
      sent := []) [1; 2; 3];
    let item = tx ~from ~to_ ~timestamp:!clock |> signed ~priv in
    invoke (G.encode (G.Tx {hash = T.hash item;
      tx_json = Yojson.Safe.to_string (T.to_yojson item)}));
    let expected = Octra_core.Tx_envelope.normalize ~sender_pk:(Some pub) item |> Result.get_ok in
    assert_true "relay lost ordinary transaction"
      (!added = [expected] && List.length !broadcast = 1 && !reported = [] && not !closed))
    [false; true]

let test_relay_poison () =
  let module T = Octra_core.Transaction in
  let module G = Octra_net.P2p_tx_gossip in
  let module Handler = Octra_node_runtime.P2p_tx_handler in
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let item = {(tx ~from ~to_ ~timestamp:1000.) with T.public_key = Some pub} |> signed ~priv in
  let hash = T.hash item in
  let valid = T.to_yojson item in
  let raw = Yojson.Safe.to_string valid in
  let padded = match valid with
    | `Assoc fields -> `Assoc (("amount", `String (String.make T.network_number_chars '0' ^ "1"))
        :: List.remove_assoc "amount" fields) |> Yojson.Safe.to_string
    | _ -> assert false in
  List.iter (fun refused ->
    List.iter (fun legacy ->
      let staged = ref 0 in
      let source, sent, broadcast, reported, closed = io
        ~sender_pk:(fun _ -> Some pub)
        ~add_tx:(fun value -> incr staged; Ok (T.hash value)) () in
      let invoke peer payload = Lwt_main.run (Handler.handle {source with peer_id = peer; payload}) in
      invoke "prior-a" (G.encode (G.Tx {hash; tx_json = refused}));
      assert_true "refused response reached staging" (!staged = 0 && !broadcast = []);
      invoke "prior-a" (G.encode (G.Inv [hash]));
      assert_true "refused response source was requested again" (!sent = []);
      invoke "prior-b" (G.encode (G.Inv [hash]));
      assert_true "unverified hash suppressed honest inventory"
        (match !sent with [payload] -> G.decode payload = G.Get [hash] | _ -> false);
      let payload = if legacy then Octra_net.Oce1.encode (fun buffer ->
          Octra_net.Oce1.put_string buffer raw)
        else G.encode (G.Tx {hash; tx_json = raw}) in
      invoke "prior-b" payload;
      assert_true "unverified hash suppressed valid relay"
        (!staged = 1 && List.length !broadcast = 1);
      assert_true "refused response punished relay" (!reported = [] && not !closed)) [false; true])
    ["{"; padded; Yojson.Safe.to_string (T.to_yojson {item with nonce = 2})]

let test_program_precheck () =
  let module T = Octra_core.Transaction in
  let module V = Octra_node_runtime.Tx_view in
  let module Memory = Octra_core.Compute_memory in
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let item = {(tx ~from ~to_ ~timestamp:1000.) with T.op_type = ProgramDeploy;
    amount = Z.zero; nonce = 2; ou = Z.of_int 1_000_000; encrypted_data = Some "not-a-package"} in
  Octra_core.Tx_staging.clear ();
  List.iter (fun (balance, nonce, fee, expected) ->
    let item = {item with T.ou = fee} |> signed ~priv in
    let account = Option.map (fun balance -> Octra_core.Ledger.{empty_account with
      balance; nonce; public_key = Some pub}) balance in
    assert_true "Program sender check differs"
      (match V.payload_account_check ~bft_mode:false ~bft_window:32 ~min_ou:Z.one
        ~min_relay_fee:(Octra_core.Tx_staging.min_relay_fee item) ~account item, expected with
      | Error (code, _), Some expected -> code = expected
      | Ok (), None -> true
      | _ -> false);
    let before = Memory.stats Memory.shared in
    let ticket = if expected = None then Some (Memory.reserve Memory.shared ~speculative:true
      (Memory.limit_of_getenv Sys.getenv_opt / 4) |> Option.get) else None in
    Fun.protect ~finally:(fun () -> Option.iter Memory.release ticket) (fun () ->
      let hash = T.hash item in
      let tx_json = Yojson.Safe.to_string (T.to_yojson item) in
      List.iter (fun payload ->
        let source, _, broadcast, reported, closed = io ~payload
          ~sender_pk:(fun _ -> Some pub) ~find_account:(fun _ -> account)
          ~add_tx:(fun _ -> fail "Program refusal reached staging") () in
        Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle source);
        assert_true "Program refusal relayed or punished peer"
          (!broadcast = [] && !reported = [] && not !closed))
        [Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx {hash; tx_json});
         Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer tx_json)]);
    assert_true "Program parse leaked memory" (Memory.stats Memory.shared = before))
    [None, 0, item.ou, Some "sender_not_found";
     Some Z.zero, 0, item.ou, Some "insufficient_balance";
     Some (Z.of_int 10_000_000), 2, item.ou, Some "invalid_nonce";
     Some (Z.of_int 10_000_000), 0, Z.one, Some "malformed_transaction";
     Some (Z.of_int 10_000_000), 0, item.ou, None]

let test_seen_window () =
  let module Seen = Octra_net.P2p_tx_seen in
  let seen, first = Seen.step Seen.empty ~now:10. "one" in
  assert_true "first message" first;
  assert_true "recent read finds checked hash" (Seen.recent seen ~now:10.5 "one");
  assert_true "recent read does not invent hashes" (not (Seen.recent seen ~now:10.5 "two"));
  assert_true "recent read expires" (not (Seen.recent seen ~now:11. "one"));
  assert_true "recent read rejects invalid clock" (not (Seen.recent seen ~now:nan "one"));
  assert_true "recent read rejects clock rewind" (not (Seen.recent seen ~now:9. "one"));
  assert_true "recent read does not consume retry"
    (Seen.recent seen ~now:10.9 "one" && Seen.size seen = 1);
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

let test_repeat_check () =
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
    Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle { io with peer_id = peer; payload })
  in
  handle "peer-a" tx;
  handle "peer-b" tx;
  assert_true "repeat skipped before signature" (!checks = 3 && !calls = 1);
  assert_true "rejected message not relayed" (!broadcast = []);
  clock := 1000.5;
  handle "peer-c" tx;
  clock := 1001.0;
  accept := true;
  handle "peer-b" tx;
  assert_true "state may change during retry" (!checks = 6 && !calls = 2);
  assert_true "accepted retry relayed" (List.length !broadcast = 1)

let test_signature_identity () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let tx = tx ~from ~to_ ~timestamp:1000.0 |> signed ~priv in
  let hash = Octra_core.Transaction.hash tx in
  let calls = ref 0 in
  let io, _, broadcast, reported, _ = io
    ~sender_pk:(fun _ -> Some pub)
    ~add_tx:(fun _ -> incr calls; Ok hash) ()
  in
  let bad = { tx with signature = Base64.encode_exn (String.make 64 '\x00') } in
  assert_true "signature changes transaction hash" (Octra_core.Transaction.hash bad <> hash);
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle_tx io bad);
  assert_true "invalid signature penalty was removed" (!reported = ["invalid_signature"]);
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle_tx io tx);
  assert_true "signature belongs to cache key" (!calls = 1 && List.length !broadcast = 1);
  let next = { tx with public_key = Some pub } in
  assert_true "public key changes transaction hash" (Octra_core.Transaction.hash next <> hash);
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle_tx io next);
  assert_true "equivalent public key shares queue identity" (!calls = 1);
  let raised = { tx with ou = Z.mul tx.ou (Z.of_int 2) } |> signed ~priv in
  assert_true "fee changes transaction hash" (Octra_core.Transaction.hash raised <> hash);
  Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle_tx io raised);
  assert_true "signed fee change has another queue identity" (!calls = 2)

let test_inv_repeat () =
  let module Gossip = Octra_net.P2p_tx_gossip in
  let module Handler = Octra_node_runtime.P2p_tx_handler in
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let item = tx ~from ~to_ ~timestamp:1000.0 |> signed ~priv in
  let hash = Octra_core.Transaction.hash item in
  let clock = ref 1000.0 in
  let base, sent, broadcast, _, _ = io ~sender_pk:(fun _ -> Some pub) () in
  let base = { base with now = (fun () -> !clock) } in
  Lwt_main.run (Handler.handle_tx base item);
  assert_true "rejected transaction not announced" (!broadcast = []);
  let announce hashes =
    Lwt_main.run (Handler.handle { base with payload = Gossip.encode (Gossip.Inv hashes) })
  in
  announce [hash];
  assert_true "recently checked hash not downloaded again" (!sent = []);
  let raised = { item with ou = Z.of_int 2 } |> signed ~priv in
  let raised_hash = Octra_core.Transaction.hash raised in
  announce [hash; raised_hash];
  assert_true "fee replacement still requested"
    (List.map Gossip.decode !sent = [Gossip.Get [raised_hash]]);
  sent := [];
  clock := 1000.9;
  announce [hash];
  assert_true "announcements do not extend hold" (!sent = []);
  clock := 1001.0;
  announce [hash];
  assert_true "transient refusal can be retried"
    (List.map Gossip.decode !sent = [Gossip.Get [hash]]);
  sent := [];
  Lwt_main.run (Handler.handle_tx base item);
  let serving = { base with
    has_tx = (fun key -> key = hash);
    find_tx = (fun key -> if key = hash then Some item else None);
    payload = Gossip.encode (Gossip.Get [hash]);
  } in
  Lwt_main.run (Handler.handle serving);
  assert_true "seen hash remains available for serving"
    (match List.map Gossip.decode !sent with
     | [Gossip.Tx value] -> value.hash = hash
     | _ -> false)

let test_duty_wire_retry () =
  let module Tx = Octra_core.Transaction in
  let module Handler = Octra_node_runtime.P2p_tx_handler in
  let module Gossip = Octra_net.P2p_tx_gossip in
  let module Rule = Octra_core.Rule_graph in
  let from, priv, pub = key () in
  let item = {
    (tx ~from ~to_:from ~timestamp:1000.) with
    amount = Z.zero; ou = Z.of_int 1000; public_key = Some pub;
    op_type = Tx.ValidatorReady;
    message = Some (Yojson.Safe.to_string (`Assoc [
      "consensus_pubkey", `String pub; "head_epoch", `String "100";
      "state_root", `String (String.make 64 'b');
      "head_proposal_id", `String (String.make 64 'a');
    ]));
  } |> signed ~priv in
  let hash = Tx.hash item in
  let tx_json = Yojson.Safe.to_string (Tx.to_yojson item) in
  let legacy = Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer tx_json) in
  List.iter (fun payload ->
    let clock = ref 1000. in
    let present = ref false in
    let calls = ref [] in
    let base, _, broadcast, reported, closed = io ~payload
      ~has_tx:(fun value -> !present && value = hash)
      ~sender_pk:(fun _ -> Some pub)
      ~add_tx:(fun value ->
        present := true;
        calls := Yojson.Safe.to_string (Tx.to_yojson value) :: !calls;
        Ok (Tx.hash value)) () in
    let base = { base with now = (fun () -> !clock) } in
    Lwt_main.run (Handler.handle ~duty:(Some (100L, Rule.Active)) ~bft_mode:true base);
    present := false;
    clock := 1601.;
    Lwt_main.run (Handler.handle ~duty:(Some (102L, Rule.Active)) ~bft_mode:true base);
    assert_true "lost duty retries after pool TTL with exact bytes" (!calls = [tx_json; tx_json]);
    assert_true "accepted retry is announced" (List.length !broadcast = 2);
    present := false;
    clock := 1632.;
    Lwt_main.run (Handler.handle ~duty:(Some (103L, Rule.Active)) ~bft_mode:true base);
    assert_true "expired reference cannot bypass timestamp age" (List.length !calls = 2);
    clock := 1663.;
    Lwt_main.run (Handler.handle ~duty:(Some (100L, Rule.Prior)) ~bft_mode:true base);
    assert_true "Prior never inherits active exception" (List.length !calls = 2);
    assert_true "admission refusal does not punish peer" (!reported = [] && not !closed))
    [Gossip.encode (Gossip.Tx { hash; tx_json }); legacy]

let test_nested_message () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let item = { (tx ~from ~to_ ~timestamp:1000.0) with public_key = Some pub }
    |> signed ~priv in
  let hash = Octra_core.Transaction.hash item in
  let encoded = Yojson.Safe.to_string (Octra_core.Transaction.to_yojson item) in
  let nested = String.make 20_000 '[' ^ "0" ^ String.make 20_000 ']' in
  let tx_json = String.sub encoded 0 (String.length encoded - 1)
    ^ ",\"extra\":" ^ nested ^ "}" in
  List.iter (fun modern ->
    List.iter (fun broken ->
      let tx_json = if broken then tx_json ^ "?" else tx_json in
      let payload = if modern then
        Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx {hash; tx_json})
        else Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer tx_json) in
      let admitted = ref 0 in
      let io, _, _, _, _ = io ~payload ~sender_pk:(fun _ -> Some pub)
        ~add_tx:(fun actual ->
          assert_true "nested field changed signed transaction" (actual = item);
          incr admitted; Ok hash) () in
      Lwt_main.run (Octra_node_runtime.P2p_tx_handler.handle io);
      assert_true (Printf.sprintf "nested gossip acceptance differs modern = %b broken = %b count = %d"
        modern broken !admitted) (!admitted = if broken then 0 else 1)
    ) [false; true]
  ) [false; true];
  List.iter (fun kind ->
    assert_true "nested control payload accepted"
      (Result.is_error (Octra_core.Bft_control_check.validate_message kind (Some nested))))
    Octra_core.Transaction.[ValidatorBond; ValidatorReady; ValidatorSetUpdate; ValidatorEvidence];
  List.iter (fun op_type -> assert_true "nested private payload accepted"
    (not (Octra_node_runtime.Tx_view.encrypted_payload_ok
      {item with op_type; encrypted_data = Some nested})))
    Octra_core.Transaction.[EncryptOp; DecryptOp; StealthOp; ClaimOp; KeySwitch]

let test_parse_wait () =
  let module Pool = Octra_core.Proof_pool in
  let module Memory = Octra_core.Compute_memory in
  let module Tx = Octra_core.Transaction in
  let public, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\046') in
  let from, priv, pub = key () in
  let body = Yojson.Safe.to_string (`Assoc [
    "new_pubkey", `String (Pvac_ffi.serialize_pubkey public |> Bytes.to_string |> Base64.encode_exn);
    "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
  ]) in
  let item = {(tx ~from ~to_:from ~timestamp:1000.) with
    Tx.op_type = KeySwitch; amount = Z.zero; nonce = 2; ou = Z.of_int 1_000_000;
    encrypted_data = Some body} |> signed ~priv in
  let tx_json = Yojson.Safe.to_string (Tx.to_yojson item) in
  let hash = Tx.hash item in
  let payloads = [Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx {hash; tx_json});
    Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer tx_json)] in
  List.iter (fun payload -> List.iter (fun action ->
    Octra_core.Tx_staging.clear ();
    let clock = ref 1000. in
    let staged = ref 0 in
    let changed = ref false in
    let account () =
      if action = "missing" || action = "late_missing" && !changed then None
      else Some Octra_core.Ledger.{empty_account with
        balance = (if action = "zero" || action = "late_balance" && !changed then Z.zero else Z.of_int 10_000_000);
        nonce = (if action = "used" || action = "late_nonce" && !changed then 2 else 0);
        public_key = Some pub} in
    if action = "pending" || action = "bump" then begin
      let pending = if action = "bump" then item else
        {item with Tx.nonce = 1; op_type = Standard; encrypted_data = None;
          ou = Z.of_int 1000; amount = Z.of_int 8_999_001} in
      Octra_core.Tx_staging.add_smart ~lookup:(fun _ -> Some (Z.of_int 10_000_000, 0)) pending
      |> Result.get_ok |> ignore
    end;
    let source, _, broadcast, reported, closed = io ~payload
      ~find_account:(fun _ -> account ())
      ~sender_pk:(fun _ -> Some pub) ~add_tx:(fun _ -> incr staged; Ok hash) () in
    let source = {source with now = (fun () -> !clock)} in
    let before = Memory.stats Memory.shared in
    let ticket = if action = "busy" then
      Some (Memory.reserve Memory.shared ~speculative:true
        (Memory.limit_of_getenv Sys.getenv_opt / 4) |> Option.get) else None in
    let mutex = Mutex.create () and condition = Condition.create () in
    let released = ref false in
    let blockers = List.init Pool.capacity (fun _ -> Pool.try_run ~bytes:0 (fun () ->
      Mutex.lock mutex;
      while not !released do Condition.wait condition mutex done;
      Mutex.unlock mutex)) in
    let release () =
      Mutex.lock mutex;
      released := true;
      Condition.broadcast condition;
      Mutex.unlock mutex in
    Lwt_main.run (Lwt.finalize (fun () ->
      let open Lwt.Syntax in
      let work = Octra_node_runtime.P2p_tx_handler.handle ~bft_mode:true source in
      let early = List.mem action ["busy"; "missing"; "zero"; "used"; "pending"; "bump"] in
      assert_true "P2P parse queue wait differs" (Lwt.is_sleeping work = not early);
      assert_true "P2P staged before parse" (!staged = 0 && !broadcast = []);
      if action = "cancel" then Lwt.cancel work;
      if action = "clock" then clock := 1401.;
      changed := true;
      release ();
      let* () = Lwt_list.iter_s (fun job -> Lwt.map (fun _ -> ()) job) blockers in
      let* cancelled = Lwt.catch (fun () -> Lwt.map (fun () -> false) work)
        (function Lwt.Canceled -> Lwt.return_true | error -> Lwt.fail error) in
      assert_true "P2P swallowed cancellation" (cancelled = (action = "cancel"));
      let expected = if action = "accept" then 1 else 0 in
      assert_true "P2P late stage or relay" (!staged = expected && List.length !broadcast = expected);
      assert_true "local parse refusal punished peer" (!reported = [] && not !closed);
      Lwt.return_unit)
      (fun () ->
        Option.iter Memory.release ticket;
        release ();
        Lwt_list.iter_s (fun job -> Lwt.map (fun _ -> ()) job) blockers));
    assert_true "P2P parse leaked reservation" (Memory.stats Memory.shared = before);
    Octra_core.Tx_staging.clear ())
    ["busy"; "cancel"; "clock"; "accept"; "missing"; "zero"; "used";
     "pending"; "bump"; "late_missing"; "late_balance"; "late_nonce"]) payloads

let direct_ops = Octra_core.Transaction.[
  Standard; PrivateOp; ContractDeploy; ContractCall; ProgramExec; MultiExec; ContractUpgrade;
  CircleDeploy; CircleProgramUpdate; CircleAssetPut; CircleAssetPutEncrypted;
  CircleSealedSlotPut; CircleSlotPolicyPut; CircleStateDescriptorPut; CircleBalanceCellPut;
  CircleRegisterCellPut; CircleTransportPolicyPut; CircleHfhePolicyPut; CircleKeyPolicyPut;
  CircleKeyGrant; CircleKeyExtend; CircleKeyRevoke; CircleKeyErase; CircleOutboxOpen;
  CircleRelayClaim; CircleRelayCancel; CircleIngressCommit; CircleCall;
  ValidatorSetUpdate; ValidatorReady; ValidatorBond; ValidatorExit; ValidatorWithdraw;
  ValidatorEvidence; RecryptOp; Op01Burn; ComputeFund; ComputeReturn; ComputeSettle]

let test_parse_kinds () =
  let module P = Octra_core.Tx_payload in
  let module T = Octra_core.Transaction in
  let module G = Octra_net.P2p_tx_gossip in
  let expected = function
    | T.EncryptOp | T.DecryptOp | T.KeySwitch | T.StealthOp | T.ClaimOp -> P.Encrypted
    | T.ProgramDeploy -> P.Program
    | T.Standard | T.PrivateOp | T.ContractDeploy | T.ContractCall | T.ProgramExec
    | T.MultiExec | T.ContractUpgrade | T.CircleDeploy | T.CircleProgramUpdate
    | T.CircleAssetPut | T.CircleAssetPutEncrypted | T.CircleSealedSlotPut
    | T.CircleSlotPolicyPut | T.CircleStateDescriptorPut | T.CircleBalanceCellPut
    | T.CircleRegisterCellPut | T.CircleTransportPolicyPut | T.CircleHfhePolicyPut
    | T.CircleKeyPolicyPut | T.CircleKeyGrant | T.CircleKeyExtend | T.CircleKeyRevoke
    | T.CircleKeyErase | T.CircleOutboxOpen | T.CircleRelayClaim | T.CircleRelayCancel
    | T.CircleIngressCommit | T.CircleCall | T.ValidatorSetUpdate | T.ValidatorReady
    | T.ValidatorBond | T.ValidatorExit | T.ValidatorWithdraw | T.ValidatorEvidence
    | T.RecryptOp | T.Op01Burn | T.ComputeFund | T.ComputeReturn | T.ComputeSettle -> P.Direct in
  let check kind op =
    let name = T.op_type_to_string op in
    assert_true "parse operation roundtrip differs" (T.op_type_of_string name = Ok op);
    assert_true "core parse kind differs" (P.parse_kind op = kind);
    assert_true "wire parse kind differs" (G.parse_kind name = Some kind);
    let json = Yojson.Safe.to_string (`Assoc ["op_type", `String name]) in
    let payload = G.encode (G.Tx {hash = h 'a'; tx_json = json}) in
    let legacy = Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer json) in
    List.iter (fun payload -> assert_true "parse lane differs"
      (G.is_light payload = (kind = P.Direct))) [payload; legacy] in
  let ops = direct_ops @ T.[EncryptOp; DecryptOp; KeySwitch; StealthOp; ClaimOp; ProgramDeploy] in
  assert_true "parse enum coverage differs" (List.length ops = 45
    && List.length (List.sort_uniq compare ops) = 45);
  List.iter (fun op -> check (expected op) op) ops;
  assert_true "unknown parse operation accepted" (G.parse_kind "unknown" = None)

let test_gossip_control () =
  let module G = Octra_net.P2p_tx_gossip in
  let max = G.encode (G.Inv (List.init G.max_hashes (fun _ -> h 'a'))) in
  assert_true "control maximum size differs" (String.length max = 34820);
  List.iter (fun payload -> assert_true "valid control refused" (G.is_control payload))
    [max; G.encode (G.Inv []); G.encode (G.Get [h 'b'])];
  List.iter (fun payload -> assert_true "invalid control bypassed parse slot" (not (G.is_control payload)))
    [""; max ^ "x"; String.sub max 0 (String.length max - 1);
     G.encode (G.Get [h 'b']) ^ "x";
     "\001\001\002\001";
     "\001\002\000\001\000\000\000\064" ^ String.make 64 'z';
     G.encode (G.Tx {hash = h 'a'; tx_json = "{}"});
     Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer "{}")];
  let wire json = G.encode (G.Tx {hash = h 'a'; tx_json = json}) in
  List.iter (fun json -> assert_true "light transaction delayed" (G.is_light (wire json)))
    ["{}"; "{\"op_type\":\"standard\"}"; "{\"op_type\":\"validator_ready\"}"];
  List.iter (fun json -> assert_true "heavy transaction bypassed queue" (not (G.is_light (wire json))))
    ["{\"op_type\":\"key_switch\"}"; "{\"op_type\":\"encrypt\"}";
     "{\"op_type\":\"program_deploy\"}"; "{\"op_type\":\"\\u006bey_switch\"}";
     "{\"op_type\":\"standard\",\"op_type\":\"key_switch\"}";
     "{\"op_type\":null}"; "{\"op_type\":\"unknown\"}";
     Yojson.Safe.to_string (`Assoc ["op_type", `String "validator_bond";
       "message", `String (String.make 16_385 'x')]);
     String.make 16_385 ' '; "["; "null"]

let test_input_queue () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:"queue" ~addr:"local" ~direction:N.Inbound in
  let blocked, _ = Lwt.task () in
  let frame value = F.{msg_type = msg_tx_gossip; payload = value} in
  let handle _ _ = blocked in
  let push value = N.receive_tx conn handle (frame value) in
  assert_true "initial parse refused" (push "active");
  List.iter (fun index -> assert_true "queued parse refused" (push (string_of_int index)))
    (List.init N.tx_input_count Fun.id);
  let bytes = conn.tx_input_bytes in
  assert_true "duplicate filled queue" (push "0" && conn.tx_input_bytes = bytes
    && List.length conn.tx_inputs = N.tx_input_count);
  assert_true "queue count overflow accepted" (not (push "overflow"));
  assert_true "queue byte overflow accepted" (not (push (String.make (N.tx_input_limit + 1) 'x')));
  assert_true "queue overflow unrecorded" (conn.tx_refused = 2);
  Lwt_main.run (let open Lwt.Syntax in let* () = N.close conn in Lwt_unix.close remote);
  assert_true "closed connection retained queue" (conn.tx_inputs = []
    && conn.tx_input_bytes = 0 && conn.tx_read = None && conn.tx_digest = None);
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:"byte-queue" ~addr:"local" ~direction:N.Inbound in
  let blocked, _ = Lwt.task () in
  let push value = N.receive_tx conn (fun _ _ -> blocked) (frame value) in
  assert_true "byte queue initial parse refused" (push "active");
  assert_true "active duplicate changed queue" (push "active" && conn.tx_inputs = []);
  assert_true "empty queue ignored byte limit"
    (not (push (String.make (N.tx_input_limit + 1) 'b')) && conn.tx_inputs = []);
  assert_true "byte queue refused a small input" (push "small");
  Lwt_main.run (let open Lwt.Syntax in let* () = N.close conn in Lwt_unix.close remote);
  assert_true "byte queue retained data" (conn.tx_inputs = [] && conn.tx_input_bytes = 0)

let test_read_dispatch () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let module Life = Octra_node_runtime.P2p_swarm_lifecycle in
  let module Pool = Octra_core.Proof_pool in
  let module Memory = Octra_core.Compute_memory in
  let module Tx = Octra_core.Transaction in
  let public, _ = Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (Bytes.make 32 '\047') in
  let from, priv, pub = key () in
  let body = Yojson.Safe.to_string (`Assoc [
    "new_pubkey", `String (Pvac_ffi.serialize_pubkey public |> Bytes.to_string |> Base64.encode_exn);
    "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
  ]) in
  let item nonce = {(tx ~from ~to_:from ~timestamp:1000.) with
    Tx.nonce; op_type = KeySwitch; amount = Z.zero; ou = Z.of_int 1_000_000;
    encrypted_data = Some body} |> signed ~priv in
  let frame nonce = let tx = item nonce in F.{msg_type = msg_tx_gossip;
    payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx {
      hash = Tx.hash tx; tx_json = Yojson.Safe.to_string (Tx.to_yojson tx)})} in
  let expected = Octra_core.Tx_envelope.normalize ~sender_pk:(Some pub) (item 1)
    |> Result.get_ok in
  let second = Octra_core.Tx_envelope.normalize ~sender_pk:(Some pub) (item 2)
    |> Result.get_ok in
  let available = tx ~from ~to_:from ~timestamp:1000. |> signed ~priv in
  let to_, _, _ = key () in
  let plain = {(tx ~from ~to_ ~timestamp:1000.) with Tx.nonce = 3} |> signed ~priv
    |> Octra_core.Tx_envelope.normalize ~sender_pk:(Some pub) |> Result.get_ok in
  let plain_frame = F.{msg_type = msg_tx_gossip;
    payload = Octra_net.P2p_tx_gossip.encode (Octra_net.P2p_tx_gossip.Tx {
      hash = Tx.hash plain; tx_json = Yojson.Safe.to_string (Tx.to_yojson plain)})} in
  let available_hash = Tx.hash available and missing = h 'd' in
  let control msg = F.{msg_type = msg_tx_gossip; payload = Octra_net.P2p_tx_gossip.encode msg} in
  List.iter (fun action ->
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator local
      ~peer_id:"reader" ~addr:"local" ~direction:N.Inbound in
    let staged = ref [] in
    let source, sent, broadcast, reported, closed = io ~sender_pk:(fun _ -> Some pub)
      ~find_account:(fun _ -> Some Octra_core.Ledger.{empty_account with
        balance = Z.of_int 10_000_000; public_key = Some pub})
      ~has_tx:(String.equal available_hash)
      ~find_tx:(fun hash -> if hash = available_hash then Some available else None)
      ~add_tx:(fun tx -> staged := tx :: !staged; Ok (Tx.hash tx)) () in
    let received, wake = Lwt.wait () in
    let callbacks = Life.{guard = source.guard; has_tx = source.has_tx; find_tx = source.find_tx;
      sender_pk = source.sender_pk; find_account = source.find_account;
      add_tx = source.add_tx; now = source.now;
      max_drift = source.max_drift; signing = source.signing} in
    let transport = Life.{send_tx_gossip = source.send_payload; send_batch = source.send_batch;
      broadcast_tx_gossip = source.broadcast_payload; report_bad_peer = source.report_bad_peer;
      close_peer = source.close_peer} in
    let before = Memory.stats Memory.shared in
    let mutex = Mutex.create () and condition = Condition.create () in
    let released = ref false in
    let blockers = List.init Pool.capacity (fun _ -> Pool.try_run ~bytes:0 (fun () ->
      Mutex.lock mutex;
      while not !released do Condition.wait condition mutex done;
      Mutex.unlock mutex)) in
    let release () = Mutex.lock mutex; released := true;
      Condition.broadcast condition; Mutex.unlock mutex in
    let handler conn frame =
      N.until_closed conn (Life.handle_frame_with_transport ~observer:false ~peer_id:conn.N.peer_id
        ~tx:callbacks ~transport ~on_consensus:(fun () -> Lwt.wakeup_later wake ())
        ~on_resource_compute:(fun () -> fail "unexpected resource frame") frame)
      |> Lwt.map (fun _ -> ()) in
    let reader = N.read_loop ~tx_idle_s:(if action = "expire" then 0.25 else 10.)
      conn ~on_message:handler in
    Lwt_main.run (Lwt.finalize (fun () ->
      let open Lwt.Syntax in
      let* () = Lwt.catch
        (fun () -> Lwt_unix.with_timeout 2. (fun () ->
          let* () = F.write_frame remote (frame 1) in
          let* () = F.write_frame remote (frame 2) in
          let* () = F.write_frame remote plain_frame in
          let* () = F.write_frame remote (control (Octra_net.P2p_tx_gossip.Inv [missing])) in
          let* () = F.write_frame remote (control (Octra_net.P2p_tx_gossip.Get [available_hash])) in
          let* () = F.write_frame remote F.{msg_type = msg_cons_vote; payload = "vote"} in
          Lwt.protected received))
        (function Lwt_unix.Timeout -> Lwt.fail_with "consensus frame waited for payload"
          | error -> Lwt.fail error) in
      assert_true "reader delayed light input" (!staged = [plain] && List.length !broadcast = 1);
      assert_true "reader dropped transaction control during parse"
        (List.rev_map Octra_net.P2p_tx_gossip.decode !sent = [
          Octra_net.P2p_tx_gossip.Get [missing];
          Octra_net.P2p_tx_gossip.Tx {hash = available_hash;
            tx_json = Yojson.Safe.to_string (Tx.to_yojson available)}]);
      assert_true "connection accepted multiple parse jobs"
        ((Memory.stats Memory.shared).bytes - before.bytes
          = Option.get (Octra_node_runtime.Tx_view.payload_parse_bytes expected));
      assert_true "connection lost queued input"
        (List.length conn.tx_inputs = 1 && conn.tx_input_bytes = String.length (frame 2).payload);
      let work = Option.get conn.tx_read in
      let* () = match action with
        | "disconnect" -> Lwt_unix.shutdown remote Lwt_unix.SHUTDOWN_ALL; reader
        | "expire" -> Lwt_unix.with_timeout 2. (fun () -> Lwt.protected work)
        | _ -> Lwt.return_unit in
      if action <> "accept" then
        assert_true "closed or expired parse retained slot" (conn.tx_read = None);
      release ();
      let* () = Lwt_list.iter_s (fun job -> Lwt.map (fun _ -> ()) job) blockers in
      let* () = Lwt_unix.with_timeout 5. (fun () -> Lwt.protected work) in
      assert_true "reader staged cancelled parse" (!staged = if action = "accept" then [second; expected; plain] else [plain]);
      assert_true "reader relayed cancelled parse"
        (List.length !broadcast = if action = "accept" then 3 else 1);
      assert_true "reader punished local congestion" (!reported = [] && not !closed);
      assert_true "parse timeout closed connection" (conn.connected = (action <> "disconnect"));
      Lwt.return_unit)
      (fun () ->
        let open Lwt.Syntax in
        release ();
        let* () = N.close conn in
        let* () = reader in
        let* () = Lwt_list.iter_s (fun job -> Lwt.map (fun _ -> ()) job) blockers in
        Lwt_unix.close remote));
    assert_true "reader leaked parse reservation" (Memory.stats Memory.shared = before))
    ["accept"; "disconnect"; "expire"]

let test_mixed_receive () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let module G = Octra_net.P2p_tx_gossip in
  let module T = Octra_core.Transaction in
  let from, _, _ = key () and to_, _, _ = key () in
  let base = tx ~from ~to_ ~timestamp:1000. in
  let heavy = {base with T.op_type = KeySwitch; encrypted_data = Some "{}"} in
  let required = T.[ValidatorBond; ValidatorEvidence; ValidatorExit; ValidatorWithdraw;
    ContractCall; ProgramExec; CircleCall] in
  let mixed = required @ direct_ops in
  let records = heavy :: List.init 63 (fun index ->
    {base with T.op_type = List.nth mixed (index mod List.length mixed);
      nonce = index + 2}) |> List.map (fun tx -> T.hash tx, tx) in
  assert_true "mixed Get reply count differs" (List.length records = 64);
  List.iter (fun op -> assert_true "mixed Get omitted required operation"
    (List.exists (fun (_, tx) -> tx.T.op_type = op) (List.tl records))) required;
  let hashes = List.map fst records and blocked_hash = fst (List.hd records) in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let connection peer fd = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator fd
    ~peer_id:peer ~addr:"local" ~direction:N.Inbound in
  let sender = connection "mixed-sender" local and receiver = connection "mixed-receiver" remote in
  let blocked, release = Lwt.wait () and complete, wake = Lwt.wait () in
  let received = ref [] and sending = ref Lwt.return_unit in
  let source, _, _, reported, closed = io ~payload:(G.encode (G.Get hashes))
    ~has_tx:(fun hash -> List.mem_assoc hash records)
    ~find_tx:(fun hash -> List.assoc_opt hash records) () in
  let source = {source with Octra_node_runtime.P2p_tx_handler.send_batch = (fun builders ->
    sending := N.send_batch sender ~msg_type:F.msg_tx_gossip builders)} in
  let reader = N.read_loop receiver ~on_message:(fun _ frame ->
    if frame.F.msg_type = F.msg_cons_vote then begin
      if Lwt.is_sleeping complete then Lwt.wakeup_later wake ();
      Lwt.return_unit
    end else match G.decode frame.payload with
    | G.Tx {hash; _} ->
      received := hash :: !received;
      if hash = blocked_hash then Lwt.protected blocked else Lwt.return_unit
    | G.Inv _ | G.Get _ -> Lwt.fail_with "mixed reply contains transaction control") in
  let writer = N.write_loop sender in
  Lwt_main.run (Lwt.finalize (fun () ->
    let open Lwt.Syntax in
    let* () = Lwt_unix.with_timeout 2. (fun () ->
      let* () = Octra_node_runtime.P2p_tx_handler.handle_plan source in
      let* () = !sending in
      let* () = N.send sender F.{msg_type = msg_cons_vote; payload = "mixed-complete"} in
      Lwt.protected complete) in
    assert_true "mixed Get lost or delayed replies" (List.rev !received = hashes);
    assert_true "mixed Get did not retain heavy work" (Lwt.is_sleeping blocked && receiver.tx_read <> None);
    assert_true "mixed Get filled heavy queue"
      (receiver.tx_inputs = [] && receiver.tx_input_bytes = 0 && receiver.tx_refused = 0);
    assert_true "mixed Get changed queue limits" (N.tx_input_count = 8 && N.tx_input_limit = 8 * 1024 * 1024);
    assert_true "mixed Get punished peer" (!reported = [] && not !closed && receiver.connected);
    let work = Option.get receiver.tx_read in
    Lwt.wakeup release ();
    let* () = work in
    assert_true "mixed Get retained finished work" (receiver.tx_read = None && receiver.tx_digest = None);
    Lwt.return_unit)
    (fun () ->
      let open Lwt.Syntax in
      if Lwt.is_sleeping blocked then Lwt.wakeup release ();
      let* () = N.close sender in
      let* () = N.close receiver in
      Lwt.join [writer; reader; !sending]))

let test_get_transport () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let module G = Octra_net.P2p_tx_gossip in
  let from, _, _ = key () and to_, _, _ = key () in
  let item = tx ~from ~to_ ~timestamp:1000. in
  let records = List.init 64 (fun index ->
    let item = {item with Octra_core.Transaction.nonce = index + 1} in
    Octra_core.Transaction.hash item, item) in
  let hashes = List.map fst records in
  let reads = ref 0 in
  let source, _, _, _, _ = io ~payload:(G.encode (G.Get hashes))
    ~has_tx:(fun hash -> List.mem_assoc hash records)
    ~find_tx:(fun hash -> incr reads; List.assoc_opt hash records) () in
  let run draining =
    reads := 0;
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Observer local
      ~peer_id:"reader" ~addr:"local" ~direction:N.Inbound in
    let deadline, wake = Lwt.wait () in
    let pending = ref [] in
    let source = {source with Octra_node_runtime.P2p_tx_handler.send_batch = (fun builders ->
      let job = N.send_batch ~wait:(fun () -> Lwt.protected deadline) conn
        ~msg_type:F.msg_tx_gossip builders in
      pending := job :: !pending)} in
    let handle () = Octra_node_runtime.P2p_tx_handler.handle_plan source in
    if not draining then begin
      for _ = 1 to 1000 do Lwt_main.run (handle ()) done;
      assert_true "Get eagerly serialized a blocked batch" (!reads = 1);
      assert_true "Get retained excess frames" (List.length conn.tx_writes = 1);
      assert_true "Get retained excess batches" (conn.tx_batches = 4);
      assert_true "Get exceeded byte cap" (conn.tx_bytes <= N.tx_limit);
      Lwt.wakeup wake ();
      Lwt_main.run (let open Lwt.Syntax in
        let* () = Lwt.pause () in
        assert_true "transaction deadline closed peer" conn.connected;
        assert_true "transaction deadline removed queued frame" (conn.tx_bytes > 0);
        let* () = N.close conn in
        let* () = Lwt.join !pending in
        assert_true "closed Get retained bytes" (conn.tx_bytes = 0 && conn.tx_writes = []);
        assert_true "closed Get retained batches" (conn.tx_batches = 0);
        assert_true "closed Get retained queue" (Lwt_mvar.is_empty conn.write_queue);
        Lwt_unix.close remote)
    end else Lwt_main.run (let open Lwt.Syntax in
      let notice = F.{msg_type = msg_tx_gossip; payload = G.encode (G.Inv hashes)} in
      let sent = N.send_tx ~wait:(fun () -> Lwt.protected deadline) conn notice in
      let* () = handle () in
      let* () = handle () in
      assert_true "Get built replies before pending transaction write" (!reads = 0);
      let writer = N.write_loop conn in
      let* prior = F.read_frame remote in
      assert_true "pending transaction frame changed" (prior.payload = notice.payload);
      let* () = Lwt_list.iter_s (fun hash ->
        let* frame = F.read_frame remote in
        let expected = G.encode (G.Tx {hash; tx_json = Yojson.Safe.to_string
          (Octra_core.Transaction.to_yojson (List.assoc hash records))}) in
        assert_true "honest Get wire changed" (frame.payload = expected);
        Lwt.return_unit) (hashes @ hashes) in
      let* () = Lwt.join !pending in
      let* () = sent in
      assert_true "honest batch lost replies" (!reads = 128 && conn.tx_batches = 0);
      assert_true "honest batch retained bytes" (conn.tx_bytes = 0 && conn.tx_batches = 0);
      let* () = N.close conn in let* () = writer in Lwt_unix.close remote)
  in
  run false;
  run true

let test_get_errors () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  let module G = Octra_net.P2p_tx_gossip in
  let from, _, _ = key () and to_, _, _ = key () in
  let item = tx ~from ~to_ ~timestamp:1000. in
  let large = {item with Octra_core.Transaction.op_type = ProgramDeploy;
    message = Some (String.make 5_030_000 'a')} in
  let records = List.map (fun tx -> Octra_core.Transaction.hash tx, tx) [large; item] in
  let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator local
    ~peer_id:"reader" ~addr:"local" ~direction:N.Inbound in
  let source, _, _, _, _ = io ~payload:(G.encode (G.Get (List.map fst records)))
    ~has_tx:(fun hash -> List.mem_assoc hash records)
    ~find_tx:(fun hash -> List.assoc_opt hash records) () in
  let work = ref Lwt.return_unit in
  let source = {source with Octra_node_runtime.P2p_tx_handler.send_batch = (fun builders ->
    work := N.send_batch conn ~msg_type:F.msg_tx_gossip
      ((fun () -> invalid_arg "local encoder error") :: builders))} in
  Lwt_main.run (let open Lwt.Syntax in
    let notice = F.{msg_type = msg_cons_vote; payload = "consensus"} in
    let* () = N.send conn notice in
    let* () = Octra_node_runtime.P2p_tx_handler.handle_plan source in
    assert_true "local encoder failure closed peer" conn.connected;
    let writer = N.write_loop conn in
    let* prior = F.read_frame remote in
    assert_true "encoder failure lost consensus queue" (prior = notice);
    let* reply = F.read_frame remote in
    assert_true "encoder failure lost next reply" (reply.payload = G.encode
      (G.Tx {hash = Octra_core.Transaction.hash item;
        tx_json = Yojson.Safe.to_string (Octra_core.Transaction.to_yojson item)}));
    let* () = !work in
    assert_true "encoder failure disconnected peer" conn.connected;
    let* () = N.close conn in let* () = writer in Lwt_unix.close remote)

let test_tx_deadline () =
  let module N = Octra_net.P2p_conn in
  let module F = Octra_net.P2p_frame in
  List.iter (fun occupied ->
    let local, remote = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let conn = N.create ~peer_class:Octra_net.P2p_frame_budget.Validator local
      ~peer_id:"reader" ~addr:"local" ~direction:N.Inbound in
    let clock, wake = Lwt.wait () in
    Lwt_main.run (let open Lwt.Syntax in
      let vote = F.{msg_type = msg_cons_vote; payload = "consensus"} in
      let tx = F.{msg_type = msg_tx_gossip; payload = "transaction"} in
      let* () = if occupied then N.send conn vote else Lwt.return_unit in
      let work = N.send_tx ~wait:(fun () -> Lwt.protected clock) conn tx in
      Lwt.wakeup wake ();
      let* () = Lwt.pause () in
      assert_true "queue deadline closed consensus transport" conn.connected;
      assert_true "queue deadline removed transferred transaction"
        (Lwt.is_sleeping work = not occupied);
      let writer = N.write_loop conn in
      let* frame = F.read_frame remote in
      assert_true "queue deadline changed frame" (frame = if occupied then vote else tx);
      let* () = work in
      assert_true "queue deadline leaked bytes" (conn.tx_bytes = 0);
      let* () = N.close conn in let* () = writer in Lwt_unix.close remote)) [false; true]

let () =
  Mirage_crypto_rng_unix.use_default ();
  test_inv_sends_get ();
  test_get_sends_tx ();
  test_get_transport ();
  test_get_errors ();
  test_tx_deadline ();
  test_receive_broadcasts_inv ();
  test_drop_reports_and_closes ();
  test_network_numbers ();
  test_relay_poison ();
  test_prior_relay ();
  test_payload_relay ();
  test_program_precheck ();
  test_seen_window ();
  test_repeat_check ();
  test_signature_identity ();
  test_inv_repeat ();
  test_duty_wire_retry ();
  test_nested_message ();
  test_parse_wait ();
  test_parse_kinds ();
  test_gossip_control ();
  test_input_queue ();
  test_read_dispatch ();
  test_mixed_receive ();
  print_endline "status = pass test = p2p_tx_handler"