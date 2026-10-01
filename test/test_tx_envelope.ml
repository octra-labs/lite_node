(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx = Octra_core.Transaction
module Ledger = Octra_core.Ledger
module Store = Octra_core.Store_irmin
module Pool = Octra_core.Tx_staging
module Rest = Octra_node_runtime.Node_rest_facade
module Gossip = Octra_net.P2p_tx_gossip
module Handler = Octra_node_runtime.P2p_tx_handler

let expect label value = if not value then failwith label

let key () =
  let secret, public = Mirage_crypto_ec.Ed25519.generate () in
  let secret = Mirage_crypto_ec.Ed25519.priv_to_octets secret |> Base64.encode_exn in
  let public = Mirage_crypto_ec.Ed25519.pub_to_octets public |> Base64.encode_exn in
  Octra_core.Crypto.Address.address_from_pubkey public, secret, public

let signed () =
  let from, secret, public = key () in
  let to_, _, _ = key () in
  Tx.sign_with_privkey Tx.{
    from; to_; amount = Z.one; nonce = 1; ou = Z.of_int 10_000;
    timestamp = Unix.gettimeofday (); signature = ""; public_key = Some public;
    message = None; op_type = Standard; encrypted_data = None;
  } secret

let aliases text index count =
  let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let code = String.index alphabet text.[index] in
  expect "zero pad bits" (code mod count = 0);
  List.init count (fun offset ->
    let bytes = Bytes.of_string text in
    Bytes.set bytes index alphabet.[code + offset];
    Bytes.to_string bytes)

let sig_forms tx =
  aliases tx.Tx.signature 85 16
  |> List.map (fun signature -> {tx with Tx.signature})

let key_forms tx =
  let public = Option.get tx.Tx.public_key in
  let _, _, other = key () in
  let forms = None :: Some "" :: Some "invalid" :: Some other ::
    List.map Option.some (aliases public 42 4) in
  List.map (fun public_key -> {tx with Tx.public_key}) forms

let test_normalize () =
  let module Envelope = Octra_core.Tx_envelope in
  let tx = signed () in
  let normalize = Envelope.normalize ~sender_pk:tx.public_key in
  expect "wallet envelope remains byte stable" (normalize tx = Ok tx);
  List.iter (fun input ->
    let result = normalize input |> Result.get_ok in
    expect "normalized envelope is unique" (result = tx);
    expect "normalization is idempotent" (normalize result = Ok result);
    expect "signature payload is preserved"
      (Tx.serialize_for_signing input = Tx.serialize_for_signing result))
    (sig_forms tx @ key_forms tx)

let test_bond_key () =
  let module Envelope = Octra_core.Tx_envelope in
  let tx = { (signed ()) with Tx.op_type = Tx.ValidatorBond } in
  let normalize = Envelope.normalize ~sender_pk:tx.public_key in
  expect "bond envelope preserved" (normalize tx = Ok tx);
  let public = Option.get tx.public_key in
  List.iter (fun carried ->
    expect "bond pad bits normalized"
      (normalize {tx with public_key = Some carried} = Ok tx)) (aliases public 42 4);
  let _, _, other = key () in
  List.iter (fun public_key ->
    expect "bond execution key is not repaired"
      (Result.is_error (normalize {tx with public_key})))
    [None; Some ""; Some "invalid"; Some other]

let scalar_forms tx =
  let order = Z.(add (shift_left one 252)
    (of_string "27742317777372353535851937790883648493")) in
  let raw = Base64.decode_exn tx.Tx.signature in
  let scalar = Z.of_bits (String.sub raw 32 32) in
  List.map (fun value ->
    let bytes = Z.to_bits value in
    let bytes = String.init 32 (fun index ->
      if index < String.length bytes then bytes.[index] else '\000') in
    {tx with signature = Base64.encode_exn (String.sub raw 0 32 ^ bytes)})
    [order; Z.add order scalar; Z.pred (Z.shift_left Z.one 256)]

let test_scalar () =
  let tx = signed () in
  List.iter (fun input ->
    expect "scalar overflow refused"
      (Octra_core.Tx_envelope.normalize ~sender_pk:tx.public_key input
       = Error ("invalid_signature", "signature scalar is out of range")))
    (scalar_forms tx)

let with_ledger ~known tx action =
  Test_workspace.with_dir "tx_envelope" (fun dir ->
    let store = Lwt_main.run (Store.open_store (Filename.concat dir "store")) in
    Fun.protect ~finally:(fun () -> Pool.clear (); Lwt_main.run (Store.close store))
      (fun () ->
        let ledger = Ledger.create store in
        let added =
          if known then Ledger.add_account_with_pubkey ledger tx.Tx.from
            (Z.of_int 1_000_000) (Option.get tx.public_key)
          else Ledger.add_account ledger tx.from (Z.of_int 1_000_000) in
        ignore (Result.get_ok added);
        Pool.clear ();
        action ledger))

let runtime checked = Rest.{
  swarm_ref = ref None; duty_head = (fun () -> None);
  preverify_admit = (fun tx -> checked := tx :: !checked; Ok ());
  save_drops = ignore; find_drop = (fun _ -> None);
  drops_by_addr = (fun _ ~limit:_ ~offset:_ -> []);
}

let test_hash_forms () =
  let tx = signed () in
  let public = Option.get tx.public_key in
  let forms = sig_forms tx in
  expect "sixteen text hashes"
    (List.length (List.sort_uniq String.compare (List.map Tx.hash forms)) = 16);
  List.iter (fun form ->
    expect "same signature bytes"
      (Base64.decode_exn form.Tx.signature = Base64.decode_exn tx.signature);
    expect "same signed body" (Tx.serialize_for_signing form = Tx.serialize_for_signing tx);
    expect "valid signature variant" (Tx.verify form public)) forms

let check_rpc ~known forms =
  let tx = signed () in
  let hash = Tx.hash tx in
  with_ledger ~known tx (fun ledger ->
    List.iter (fun input ->
      Pool.clear ();
      let checked = ref [] in
      let result = Rest.validate_and_submit_tx (runtime checked) ledger input in
      expect "RPC returns normalized identity" (result = Ok hash);
      expect "queue stores normalized envelope" (Pool.find_by_hash hash = Some tx);
      expect "preverify sees normalized envelope" (!checked = [tx]);
      expect "normalization preserves signed body"
        (Tx.serialize_for_signing tx = Tx.serialize_for_signing input)) (forms tx))

let test_rpc_signatures () = check_rpc ~known:true sig_forms
let test_rpc_keys () = check_rpc ~known:true key_forms

let test_rpc_first_key () =
  check_rpc ~known:false (fun tx ->
    aliases (Option.get tx.Tx.public_key) 42 4
    |> List.map (fun public -> {tx with public_key = Some public}))

let test_nonce_identity () =
  let tx = signed () in
  let input = List.nth (sig_forms tx) 1 in
  expect "variants have different text hashes" (Tx.hash tx <> Tx.hash input);
  with_ledger ~known:true tx (fun ledger ->
    let transfer input = Ledger.transfer ledger ~from:input.Tx.from ~to_:input.to_
      ~amount:input.amount ~fee:Z.zero input.nonce in
    expect "first transfer succeeds" (Result.is_ok (transfer tx));
    let before = Ledger.find ledger tx.to_ in
    expect "variant cannot spend same nonce" (Result.is_error (transfer input));
    expect "recipient credited only once" ((Ledger.find ledger tx.to_).balance = before.balance);
    expect "one consumed nonce" ((Ledger.find ledger tx.from).nonce = 1))

let test_rpc_refusals () =
  let tx = signed () in
  let _, _, other = key () in
  List.iter (fun known ->
    with_ledger ~known tx (fun ledger ->
      let inputs = [
        {tx with signature = ""}; {tx with signature = String.make 89 'A'};
        {tx with public_key = Some (String.make 45 'A')};
        {tx with amount = Z.of_int 2};
        {tx with signature = Base64.encode_exn (String.make 64 '\000')};
      ] @ scalar_forms tx @ if known then [] else [
        {tx with public_key = None}; {tx with public_key = Some other};
      ] in
      List.iter (fun input ->
        let checked = ref [] in
        expect "invalid envelope refused"
          (Result.is_error (Rest.validate_and_submit_tx (runtime checked) ledger input));
        expect "refusal has no queue effects" (Pool.staging_size () = 0 && !checked = [])) inputs))
    [false; true]

let wire legacy tx =
  let tx_json = Tx.to_yojson tx |> Yojson.Safe.to_string in
  if legacy then Octra_net.Oce1.encode (fun buffer -> Octra_net.Oce1.put_string buffer tx_json)
  else Gossip.encode (Gossip.Tx {hash = Tx.hash tx; tx_json})

let peer_io ledger tx seen sent = Handler.{
  guard = Octra_net.P2p_tx_gossip_guard.create (); payload = ""; peer_id = "envelope-peer";
  has_tx = (fun hash -> Pool.find_by_hash hash <> None);
  find_tx = Pool.find_by_hash;
  sender_pk = Octra_node_runtime.Tx_sender_key.resolve ~find_account:(Ledger.find_opt ledger);
  add_tx = (fun input -> seen := input :: !seen;
    Rest.add_tx_to_staging ~relay:false (runtime (ref [])) ledger input);
  send_payload = (fun payload -> sent := payload :: !sent);
  broadcast_payload = (fun payload -> sent := payload :: !sent);
  report_bad_peer = (fun _ -> failwith "unexpected peer penalty");
  close_peer = (fun () -> failwith "unexpected peer close");
  now = (fun () -> tx.Tx.timestamp); max_drift = 300.;
}

let check_p2p legacy =
  let tx = signed () in
  with_ledger ~known:true tx (fun ledger ->
    let seen, sent = ref [], ref [] in
    let io = peer_io ledger tx seen sent in
    let forms = List.rev (sig_forms tx) @ key_forms tx in
    List.iter (fun input -> Handler.handle {io with payload = wire legacy input}) forms;
    let hash = Tx.hash tx in
    expect "P2P adds one normalized envelope" (!seen = [tx]);
    expect "P2P stores one normalized hash"
      (Pool.staging_size () = 1 && Pool.find_by_hash hash = Some tx);
    expect "P2P advertises one normalized hash"
      (List.map Gossip.decode !sent = [Gossip.Inv [hash]]))

let test_p2p_tx () = check_p2p false
let test_p2p_legacy () = check_p2p true

let test_wire_hash () =
  let tx = signed () in
  let input = List.nth (sig_forms tx) 1 in
  with_ledger ~known:true tx (fun ledger ->
    let seen, sent = ref [], ref [] in
    let io = peer_io ledger tx seen sent in
    let payload = Gossip.encode (Gossip.Tx {
      hash = Tx.hash tx; tx_json = Tx.to_yojson input |> Yojson.Safe.to_string;
    }) in
    Handler.handle {io with payload};
    expect "wire identity checked before normalization" (!seen = [] && !sent = []);
    let old_hash = Tx.hash input in
    let serve = {io with
      payload = Gossip.encode (Gossip.Get [old_hash]);
      has_tx = (fun hash -> hash = old_hash);
      find_tx = (fun hash -> if hash = old_hash then Some input else None);
    } in
    Handler.handle serve;
    expect "serving prior identity preserves bytes"
      (List.map Gossip.decode !sent = [Gossip.Tx {
        hash = old_hash; tx_json = Tx.to_yojson input |> Yojson.Safe.to_string;
      }]))

let test_rpc_routes () =
  let module Submit = Octra_node_runtime.Submit_rpc in
  let tx = signed () in
  let input = List.nth (sig_forms tx) 1 in
  let json = Tx.to_yojson input in
  with_ledger ~known:true tx (fun ledger ->
    let validate = Rest.validate_and_submit_tx (runtime (ref [])) ledger in
    let routes = [
      (fun () -> Submit.submit ~validate (`List [json]));
      (fun () -> Submit.private_transfer ~validate (`List [json]));
      (fun () -> Submit.submit_batch ~validate (`List [`List [json]]));
    ] in
    let rec hashes = function
      | `Assoc fields -> List.concat_map (function
        | "tx_hash", `String hash -> [hash]
        | _, value -> hashes value) fields
      | `List values -> List.concat_map hashes values
      | _ -> [] in
    List.iter (fun route ->
      Pool.clear ();
      let response = Lwt_main.run (route ()) |> Result.get_ok in
      expect "RPC route returns queued hash" (hashes response = [Tx.hash tx])) routes)

let () =
  Mirage_crypto_rng_unix.use_default ();
  let failed = ref 0 in
  List.iter (fun (name, run) ->
    try run (); Printf.printf "event = test name = %s status = passed\n%!" name
    with exn -> incr failed; Printf.eprintf "event = test name = %s status = failed error = %s\n%!"
      name (Printexc.to_string exn)) [
    "normalize", test_normalize;
    "bond_key", test_bond_key;
    "scalar", test_scalar;
    "hash_forms", test_hash_forms;
    "rpc_signatures", test_rpc_signatures;
    "rpc_keys", test_rpc_keys;
    "rpc_first_key", test_rpc_first_key;
    "nonce_identity", test_nonce_identity;
    "rpc_refusals", test_rpc_refusals;
    "p2p_tx", test_p2p_tx;
    "p2p_legacy", test_p2p_legacy;
    "wire_hash", test_wire_hash;
    "rpc_routes", test_rpc_routes;
  ];
  if !failed <> 0 then exit 1