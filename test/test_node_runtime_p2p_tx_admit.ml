(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let fail msg = failwith ("test_node_runtime_p2p_tx_admit: " ^ msg)

let assert_verdict msg expected actual =
  if expected <> actual then fail msg

let assert_true msg v =
  if not v then fail msg

let key () =
  let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
  let priv_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets priv) in
  let pub_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets pub) in
  let addr = Octra_core.Crypto.Address.address_from_pubkey pub_b64 in
  addr, priv_b64, pub_b64

let tx ~from ~to_ ~timestamp ~op_type =
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
    op_type;
    encrypted_data = None;
  }

let signed ~priv tx =
  Octra_core.Transaction.sign_with_privkey tx priv

let test_accepts_standard () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let tx =
    tx ~from ~to_ ~timestamp:1000.0 ~op_type:Octra_core.Transaction.Standard
    |> signed ~priv
  in
  assert_verdict "standard accepted"
    Octra_node_runtime.P2p_tx_admit.Accept
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       tx)

let test_special_targets () =
  let from, priv, pub = key () in
  let stealth =
    tx ~from ~to_:"stealth" ~timestamp:1000.0 ~op_type:Octra_core.Transaction.StealthOp
    |> signed ~priv
  in
  let multi =
    tx ~from ~to_:"multi_exec" ~timestamp:1000.0 ~op_type:Octra_core.Transaction.MultiExec
    |> signed ~priv
  in
  assert_true "stealth target valid" (Octra_node_runtime.P2p_tx_admit.to_valid stealth);
  assert_true "multi target valid" (Octra_node_runtime.P2p_tx_admit.to_valid multi);
  assert_verdict "stealth without payload rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       stealth);
  assert_verdict "multi accepted"
    Octra_node_runtime.P2p_tx_admit.Accept
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       multi)

let test_rejects_invalid_address () =
  let from, priv, pub = key () in
  let short_pubkey =
    Base64.encode_exn
      (String.make 31 '\x00' ^ String.make 1 '\x1a')
  in
  let padded =
    Octra_core.Crypto.Address.address_from_pubkey short_pubkey
  in
  assert_true "short base58 address padded"
    (String.equal
       padded
       "oct1zHDhuhZ9kBpPku5KstyRbZ7t54ZTk6xNz15dwQyHZAK");
  assert_true "padded address has canonical width" (String.length padded = 47);
  let malformed =
    tx ~from ~to_:"bad" ~timestamp:1000.0 ~op_type:Octra_core.Transaction.Standard
    |> signed ~priv
  in
  assert_verdict "invalid address"
    Octra_node_runtime.P2p_tx_admit.Invalid_address
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       malformed);
  let recipient, _, _ = key () in
  assert_true "derived address has canonical width" (String.length recipient = 47);
  let short = String.sub recipient 0 46 in
  let long = recipient ^ "1" in
  assert_true "short address rejected"
    (not (Octra_core.Crypto.Address.is_valid_address short));
  assert_true "long address rejected"
    (not (Octra_core.Crypto.Address.is_valid_address long));
  let reject_recipient recipient =
    tx ~from ~to_:recipient ~timestamp:1000.0 ~op_type:Octra_core.Transaction.Standard
    |> signed ~priv
    |> Octra_node_runtime.P2p_tx_admit.admit
         ~now:1001.0
         ~max_drift:300.0
         ~sender_pk:(Some pub)
  in
  assert_verdict "short recipient rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_address
    (reject_recipient short);
  assert_verdict "long recipient rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_address
    (reject_recipient long)

let test_rejects_timestamp () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let tx =
    tx ~from ~to_ ~timestamp:1000.0 ~op_type:Octra_core.Transaction.Standard
    |> signed ~priv
  in
  assert_verdict "timestamp drift"
    (Octra_node_runtime.P2p_tx_admit.Timestamp_drift 401.0)
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1401.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       tx)

let test_rejects_signature () =
  let from, priv, _pub = key () in
  let to_, _, wrong_pub = key () in
  let tx =
    tx ~from ~to_ ~timestamp:1000.0 ~op_type:Octra_core.Transaction.Standard
    |> signed ~priv
  in
  assert_verdict "missing signature key"
    Octra_node_runtime.P2p_tx_admit.Invalid_signature
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:None
       tx);
  assert_verdict "wrong signature key"
    Octra_node_runtime.P2p_tx_admit.Invalid_signature
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some wrong_pub)
       tx)

let test_reject_invalid_claim_secret () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let claim secret =
    {
      (tx
         ~from
         ~to_
         ~timestamp:1000.0
         ~op_type:Octra_core.Transaction.ClaimOp) with
      encrypted_data =
        Some
          (Yojson.Safe.to_string
             (`Assoc ["claim_secret", `String secret]));
    }
    |> signed ~priv
  in
  assert_verdict "non-hex claim secret rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       (claim (String.make 64 'z')));
  assert_verdict "missing claim payload rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       (tx
          ~from
          ~to_
          ~timestamp:1000.0
          ~op_type:Octra_core.Transaction.ClaimOp
        |> signed ~priv));
  assert_verdict "incomplete claim payload rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       (claim (String.make 64 'a')))

let test_reject_shared_payload () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let signed_tx op_type ?message ?encrypted_data () =
    {
      (tx ~from ~to_ ~timestamp:1000.0 ~op_type) with
      message;
      encrypted_data;
    }
    |> signed ~priv
  in
  let admit tx =
    Octra_node_runtime.P2p_tx_admit.admit
      ~now:1001.0
      ~max_drift:300.0
      ~sender_pk:(Some pub)
      tx
  in
  assert_verdict "oversized standard message rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (admit
       (signed_tx
          Octra_core.Transaction.Standard
          ~message:(String.make 257 'a')
          ()));
  assert_verdict "key switch without payload rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (admit (signed_tx Octra_core.Transaction.KeySwitch ()));
  assert_verdict "unsafe call params rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (admit
       (signed_tx
          Octra_core.Transaction.ProgramExec
          ~message:"[\"<script>\"]"
          ()))

let test_signature_before_payload () =
  let from, priv, pub = key () in
  let value =
    tx ~from ~to_:from ~timestamp:1000.0 ~op_type:Octra_core.Transaction.EncryptOp
  in
  let admit value =
    Octra_node_runtime.P2p_tx_admit.admit ~now:1001.0 ~max_drift:300.0
      ~sender_pk:(Some pub) value
  in
  assert_verdict "signature precedes proof parsing"
    Octra_node_runtime.P2p_tx_admit.Invalid_signature (admit value);
  assert_verdict "signed payload still validated"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload (admit (signed ~priv value))

let test_program_package_shape () =
  let from, priv, pub = key () in
  let to_, _, _ = key () in
  let compiled =
    match
      Octra_vm.Program_package.compile
        ~main:"main.aml"
        ~sources:[
          Octra_vm.Program_package.{
            path = "main.aml";
            body = "program P2p { fn value(): int { return 1 } }";
          };
        ]
    with
    | Ok value -> value
    | Error error ->
      fail (Octra_vm.Program_package.error_message error)
  in
  let program payload =
    {
      (tx
         ~from
         ~to_
         ~timestamp:1000.0
         ~op_type:Octra_core.Transaction.ProgramDeploy)
      with
      encrypted_data = Some payload;
    }
    |> signed ~priv
  in
  assert_verdict
    "malformed Program package rejected"
    Octra_node_runtime.P2p_tx_admit.Invalid_payload
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       (program "invalid"));
  assert_verdict
    "canonical Program package accepted"
    Octra_node_runtime.P2p_tx_admit.Accept
    (Octra_node_runtime.P2p_tx_admit.admit
       ~now:1001.0
       ~max_drift:300.0
       ~sender_pk:(Some pub)
       (program (Base64.encode_exn compiled.package)))

let test_duty_retry () =
  let module Admit = Octra_node_runtime.P2p_tx_admit in
  let module View = Octra_node_runtime.Tx_view in
  let module Tx = Octra_core.Transaction in
  let module Rule = Octra_core.Rule_graph in
  let from, priv, pub = key () in
  let message proposal = Yojson.Safe.to_string (`Assoc ([
    "consensus_pubkey", `String pub;
    "head_epoch", `String "100";
    "state_root", `String (String.make 64 'b');
  ] @ proposal)) in
  let item = {
    (tx ~from ~to_:from ~timestamp:1. ~op_type:Tx.ValidatorReady) with
    amount = Z.zero; ou = Z.of_int 1000; public_key = Some pub;
    message = Some (message ["head_proposal_id", `String (String.make 64 'a')]);
  } |> signed ~priv in
  let original = Tx.to_yojson item in
  let admit ?(bft_mode = true) duty value =
    Admit.admit ~duty ~bft_mode ~now:1000. ~max_drift:300. ~sender_pk:(Some pub) value in
  let rest ?(bft_mode = true) duty value =
    View.pre_route_admission ~duty ~bft_mode ~now:1000. ~max_timestamp_drift:300.
      ~observer_rpc_mode:false value in
  List.iter (fun head ->
    let duty = Some (head, Rule.Active) in
    assert_verdict "active delayed P2P retry" Admit.Accept (admit duty item);
    assert_true "active delayed REST retry" (rest duty item = Ok ());
    assert_true "retry leaves signed transaction unchanged" (Tx.to_yojson item = original))
    [100L; 101L; 102L];
  let rejects ?(bft_mode = true) label duty value =
    assert_true (label ^ ": P2P timestamp rejected")
      (match admit ~bft_mode duty value with Admit.Timestamp_drift _ -> true | _ -> false);
    assert_true (label ^ ": REST timestamp rejected")
      (Result.is_error (rest ~bft_mode duty value))
  in
  List.iter (fun (label, duty) -> rejects label duty item)
    ["missing duty head", None;
     "Prior mode", Some (100L, Rule.Prior);
     "future reference", Some (99L, Rule.Active);
     "expired reference", Some (103L, Rule.Active)];
  let duty = Some (102L, Rule.Active) in
  rejects ~bft_mode:false "non-BFT mode" duty item;
  rejects "ordinary transaction" duty
    ({ item with op_type = Tx.Standard; message = None } |> signed ~priv);
  rejects "missing proposal ID" duty
    ({ item with message = Some (message []) } |> signed ~priv);
  let malformed = { item with message = Some "invalid" } |> signed ~priv in
  assert_true "malformed ready fails payload precheck"
    (Result.is_error (View.payload_size_admission ~limits:View.payload_limits malformed));
  assert_verdict "malformed ready rejected before P2P timestamp check"
    Admit.Invalid_payload (admit duty malformed);
  assert_true "malformed ready cannot claim retry"
    (not (View.duty_retry ~now:1000. ~duty ~bft_mode:true malformed));
  assert_true "malformed ready rejected by REST timestamp check"
    (Result.is_error (rest duty malformed));
  rejects "future timestamp beyond tolerance" duty
    ({ item with timestamp = 1401. } |> signed ~priv);
  List.iter (fun (label, timestamp) ->
    rejects label duty { item with timestamp };
    assert_true (label ^ ": timestamp cannot claim retry")
      (not (View.duty_retry ~now:1000. ~duty ~bft_mode:true { item with timestamp })))
    ["NaN", nan; "positive infinity", infinity; "negative infinity", neg_infinity];
  List.iter (fun (label, now) ->
    assert_true (label ^ ": clock cannot claim retry")
      (not (View.duty_retry ~now ~duty ~bft_mode:true item));
    assert_true (label ^ ": P2P clock rejected")
      (match Admit.admit ~duty ~bft_mode:true ~now ~max_drift:300.
        ~sender_pk:(Some pub) item with Admit.Timestamp_drift _ -> true | _ -> false);
    assert_true (label ^ ": REST clock rejected")
      (Result.is_error (View.pre_route_admission ~duty ~bft_mode:true ~now
        ~max_timestamp_drift:300. ~observer_rpc_mode:false item)))
    ["NaN", nan; "positive infinity", infinity];
  let near_future = { item with timestamp = 1200. } |> signed ~priv in
  assert_verdict "existing future tolerance retained" Admit.Accept (admit duty near_future);
  assert_true "future tolerance is not a duty exemption"
    (not (View.duty_retry ~now:1000. ~duty ~bft_mode:true near_future));
  let bad = { item with signature = Base64.encode_exn (String.make 64 '\000') } in
  assert_verdict "old duty still requires valid signature" Admit.Invalid_signature (admit duty bad);
  assert_true "REST signature check remains separate and mandatory"
    (rest duty bad = Ok ()
     && Result.is_error (View.signature_admission ~account_public_key:(Some pub) bad));
  assert_verdict "old duty still requires sender key" Admit.Invalid_signature
    (Admit.admit ~duty ~bft_mode:true ~now:1000. ~max_drift:300. ~sender_pk:None item);
  assert_true "old caller has no implicit duty exception"
    (match Admit.admit ~now:1000. ~max_drift:300. ~sender_pk:(Some pub) item with
     | Admit.Timestamp_drift _ -> true | _ -> false)

let () =
  Mirage_crypto_rng_unix.use_default ();
  test_accepts_standard ();
  test_special_targets ();
  test_rejects_invalid_address ();
  test_rejects_timestamp ();
  test_rejects_signature ();
  test_reject_invalid_claim_secret ();
  test_reject_shared_payload ();
  test_signature_before_payload ();
  test_program_package_shape ();
  test_duty_retry ();
  print_endline "node runtime p2p tx admit tests passed"