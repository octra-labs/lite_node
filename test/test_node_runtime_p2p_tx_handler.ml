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
  assert_true "equivalent public key shares queue identity" (!calls = 1);
  let raised = { tx with ou = Z.mul tx.ou (Z.of_int 2) } |> signed ~priv in
  assert_true "fee changes transaction hash" (Octra_core.Transaction.hash raised <> hash);
  Octra_node_runtime.P2p_tx_handler.handle_tx io raised;
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
  Handler.handle_tx base item;
  assert_true "rejected transaction not announced" (!broadcast = []);
  let announce hashes =
    Handler.handle { base with payload = Gossip.encode (Gossip.Inv hashes) }
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
  Handler.handle_tx base item;
  let serving = { base with
    has_tx = (fun key -> key = hash);
    find_tx = (fun key -> if key = hash then Some item else None);
    payload = Gossip.encode (Gossip.Get [hash]);
  } in
  Handler.handle serving;
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
    Handler.handle ~duty:(Some (100L, Rule.Active)) ~bft_mode:true base;
    present := false;
    clock := 1601.;
    Handler.handle ~duty:(Some (102L, Rule.Active)) ~bft_mode:true base;
    assert_true "lost duty retries after pool TTL with exact bytes" (!calls = [tx_json; tx_json]);
    assert_true "accepted retry is announced" (List.length !broadcast = 2);
    present := false;
    clock := 1632.;
    Handler.handle ~duty:(Some (103L, Rule.Active)) ~bft_mode:true base;
    assert_true "expired reference cannot bypass timestamp age" (List.length !calls = 2);
    clock := 1663.;
    Handler.handle ~duty:(Some (100L, Rule.Prior)) ~bft_mode:true base;
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
      Octra_node_runtime.P2p_tx_handler.handle io;
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
      for _ = 1 to 1000 do handle () done;
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
      handle ();
      handle ();
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
    Octra_node_runtime.P2p_tx_handler.handle_plan source;
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
  test_seen_window ();
  test_repeat_check ();
  test_signature_identity ();
  test_inv_repeat ();
  test_duty_wire_retry ();
  test_nested_message ();
  print_endline "status = pass test = p2p_tx_handler"