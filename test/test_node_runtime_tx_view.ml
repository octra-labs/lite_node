(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Tx_view = Octra_node_runtime.Tx_view
module Preverify_cache = Octra_node_runtime.Preverify_cache
module Preverify_submit = Octra_node_runtime.Preverify_submit
module Peer_auth = Octra_core.Peer_auth
module Transaction = Octra_core.Transaction
module FheBalance = Octra_core.Crypto.FheBalance

let fail msg =
  failwith ("test_node_runtime_tx_view: " ^ msg)

let member name = function
  | `Assoc fields ->
    (match List.assoc_opt name fields with
     | Some value -> value
     | None -> fail ("missing field " ^ name))
  | _ -> fail "expected object"

let string_field name json =
  match member name json with
  | `String value -> value
  | _ -> fail ("expected string field " ^ name)

let int_field name json =
  match member name json with
  | `Int value -> value
  | _ -> fail ("expected int field " ^ name)

let bool_field name json =
  match member name json with
  | `Bool value -> value
  | _ -> fail ("expected bool field " ^ name)

let list_field name json =
  match member name json with
  | `List value -> value
  | _ -> fail ("expected list field " ^ name)

let sample_tx ?(amount = Z.of_int 1_000_000) ?(nonce = 7) ?(ou = Z.of_int 9) ?encrypted_data op_type =
  {
    Transaction.from = "octfrom";
    to_ = "octto";
    amount;
    nonce;
    ou;
    timestamp = 12.5;
    signature = "sig";
    public_key = None;
    message = Some "6869";
    encrypted_data;
    op_type;
  }

let signed_sample_tx () =
  let priv =
    match Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 '\042') with
    | Ok value -> value
    | Error _ -> fail "invalid test key"
  in
  let pub = Mirage_crypto_ec.Ed25519.pub_of_priv priv in
  let priv_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets priv) in
  let pub_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets pub) in
  let addr = Octra_core.Crypto.Address.address_from_pubkey pub_b64 in
  let unsigned =
    { (sample_tx Transaction.Standard) with
      Transaction.from = addr;
      signature = "";
      public_key = Some pub_b64;
    }
  in
  (Transaction.sign_with_privkey unsigned priv_b64, pub_b64)

let test_masking () =
  let stealth = sample_tx Transaction.StealthOp in
  let claim = sample_tx Transaction.ClaimOp in
  if Tx_view.display_from stealth <> "-" then fail "stealth from not masked";
  if Tx_view.display_to stealth <> "octto" then fail "stealth to should remain visible";
  if Tx_view.display_from claim <> "-" then fail "claim from not masked";
  if Tx_view.display_to claim <> "-" then fail "claim to not masked";
  let row =
    `Assoc [
      "op_type", `String "claim";
      "from", `String "alice";
      "to", `String "bob";
      "to_", `String "bob";
      "hash", `String "h";
    ]
  in
  let masked = Tx_view.mask_stealth_row row in
  if string_field "from" masked <> "-" then fail "row from not masked";
  if string_field "to" masked <> "-" then fail "row to not masked";
  if string_field "to_" masked <> "-" then fail "row to_ not masked";
  if string_field "hash" masked <> "h" then fail "row hash changed"

let test_tx_fields () =
  let tx = sample_tx Transaction.Standard in
  let fields = Tx_view.tx_fields ~decode_message:(fun s -> if s = "6869" then "hi" else s) tx in
  let json = `Assoc fields in
  if string_field "from" json <> "octfrom" then fail "from mismatch";
  if string_field "to" json <> "octto" then fail "to mismatch";
  if string_field "amount" json = "" then fail "amount empty";
  if string_field "amount_raw" json <> "1000000" then fail "amount raw mismatch";
  if string_field "message" json <> "hi" then fail "message mismatch"

let test_staging_error () =
  let cases = [
    "duplicate transaction", ("duplicate_transaction", "tx already in staging");
    "duplicate nonce", ("duplicate_transaction", "duplicate nonce");
    "duplicate nonce (fee rate bump < 10%)", ("duplicate_transaction", "duplicate nonce (fee rate bump < 10%)");
    "Duplicate Nonce (fee rate bump < 10%)", ("duplicate_transaction", "Duplicate Nonce (fee rate bump < 10%)");
    "duplicate non", ("internal_error", "duplicate non");
    "nonce too low (already used)", ("invalid_nonce", "nonce already used");
    "nonce too far ahead", ("nonce_too_far", "nonce too far ahead");
    "fee too low", ("fee_too_low", "fee too low");
    "staging full hard cap", ("staging_full", "staging full hard cap");
    "transaction too large", ("tx_too_large", "transaction too large");
    "unknown", ("internal_error", "unknown");
  ] in
  List.iter
    (fun (msg, expected) ->
      if Tx_view.staging_error msg <> expected then fail ("staging error mismatch: " ^ msg))
    cases

let test_status_json () =
  let fields = Tx_view.tx_fields ~decode_message:(fun s -> s) (sample_tx Transaction.Standard) in
  let pending = Tx_view.rpc_pending ~hash:"h1" ~fields in
  if string_field "status" pending <> "pending" then fail "rpc pending status mismatch";
  if string_field "tx_hash" pending <> "h1" then fail "rpc pending hash mismatch";
  let confirmed = Tx_view.rest_confirmed ~hash:"h2" ~epoch:9 ~tx_json:"{}" ~fields in
  if string_field "status" confirmed <> "confirmed" then fail "rest confirmed status mismatch";
  if int_field "epoch" confirmed <> 9 then fail "rest confirmed epoch mismatch";
  let rejected =
    Tx_view.rest_rejected
      ~hash:"h3"
      ~from_addr:"a"
      ~to_addr:"b"
      ~amount:"1"
      ~nonce:3
      ~err_type:"bad"
      ~reason:"reason"
      ~epoch:4
      ~rejected_at:5.0
  in
  if string_field "status" rejected <> "rejected" then fail "rejected status mismatch";
  if string_field "source" rejected <> "rejected_txs" then fail "rejected source mismatch";
  let dropped =
    Tx_view.rpc_dropped
      ~hash:"h4"
      ~reason:"drop"
      ~detail:"detail"
      ~dropped_at:1.0
      ~from_addr:"a"
      ~to_addr:"b"
      ~nonce:2
      ~ou:(Z.of_int 8)
      ~op_type:Transaction.Standard
  in
  if string_field "status" dropped <> "dropped" then fail "dropped status mismatch";
  if string_field "ou" dropped <> "8" then fail "dropped ou mismatch";
  let private_drop =
    Tx_view.rpc_dropped
      ~hash:"h5"
      ~reason:"drop"
      ~detail:"detail"
      ~dropped_at:1.0
      ~from_addr:"alice"
      ~to_addr:"bob"
      ~nonce:3
      ~ou:(Z.of_int 9)
      ~op_type:Transaction.ClaimOp
  in
  if string_field "from" private_drop <> "-" then fail "dropped claim from not masked";
  if string_field "to_" private_drop <> "-" then fail "dropped claim to not masked"

let test_submit_rpc_helpers () =
  let tx = sample_tx Transaction.Standard in
  let tx_json = Transaction.to_yojson tx in
  let decoded =
    match Tx_view.submit_params (`List [tx_json]) with
    | Ok value -> value
    | Error e -> fail ("submit params error " ^ e.Octra_core.Rpc.message)
  in
  if decoded.Transaction.nonce <> tx.Transaction.nonce then fail "submit decoded nonce";
  begin
    match Tx_view.submit_params (`List []) with
    | Error e when e.Octra_core.Rpc.code = -32602 -> ()
    | Error e -> fail (Printf.sprintf "submit missing code %d" e.Octra_core.Rpc.code)
    | Ok _ -> fail "submit accepted missing tx"
  end;
  let accepted = Tx_view.submit_accepted ~tx_hash:"h1" tx in
  if string_field "status" accepted <> "accepted" then fail "submit helper status";
  let actual_ou = string_field "ou_cost" accepted in
  let expected_ou = Z.to_string (Transaction.ou_cost tx) in
  if actual_ou <> expected_ou then
    fail (Printf.sprintf "submit helper ou actual %s expected %s" actual_ou expected_ou);
  let unsupported = Tx_view.submit_rpc_error "unsupported_operation" "blocked" in
  if unsupported.Octra_core.Rpc.code <> 116 then fail "submit unsupported code";
  let observer = Tx_view.submit_rpc_error "read_only_observer" "observer node is read-only" in
  if observer.Octra_core.Rpc.code <> 117 then fail "submit observer code";
  List.iter (fun reason ->
    let kind, detail = Tx_view.staging_error reason in
    let error = Tx_view.submit_rpc_error kind detail in
    let response = Octra_core.Rpc.response_json (Error_ (error, `Int 1)) in
    match response with
    | `Assoc fields ->
      begin match List.assoc "error" fields with
      | `Assoc fields when List.assoc "code" fields = `Int 106
                           && List.assoc "data" fields = `String detail -> ()
      | _ -> fail "duplicate RPC lost staging reason"
      end
    | _ -> fail "duplicate RPC response shape")
    ["duplicate transaction"; "duplicate nonce (fee rate bump < 10%)"];
  List.iter (fun kind ->
    let error = Tx_view.submit_rpc_error kind "capacity unavailable" in
    if error.Octra_core.Rpc.code <> 110 then fail "temporary verifier RPC code";
    match Octra_core.Rpc.response_json (Error_ (error, `Int 1)) with
    | `Assoc fields ->
      begin match Octra_node_runtime.Set_post.rpc_failure (List.assoc "error" fields) with
      | Retry _ -> ()
      | _ -> fail "temporary verifier RPC retry classification"
      end
    | _ -> fail "RPC error response shape")
    ["pre_verify_busy"; "pre_verify_unavailable"];
  let batch_params =
    match Tx_view.submit_batch_params (`List [`List [tx_json]]) with
    | Ok value -> value
    | Error e -> fail ("submit batch params error " ^ e.Octra_core.Rpc.message)
  in
  if List.length batch_params <> 1 then fail "submit batch params count";
  let batch_tx =
    match Tx_view.decode_submit_batch_tx tx_json with
    | Ok value -> value
    | Error e -> fail ("submit batch decode error " ^ e)
  in
  let batch_ok = Tx_view.submit_batch_row batch_tx (Ok "h2") in
  if string_field "status" batch_ok <> "accepted" then fail "submit batch row ok";
  let batch_bad = Tx_view.submit_batch_row batch_tx (Error ("bad", "rejected")) in
  if string_field "status" batch_bad <> "rejected" then fail "submit batch row reject";
  let batch_decode_bad = Tx_view.submit_batch_decode_error "invalid tx" in
  if string_field "status" batch_decode_bad <> "error" then fail "submit batch decode row"

let lookup_response hash lookup =
  match Tx_view.transaction_lookup_response ~decode_message:(fun s -> s) ~hash lookup with
  | Ok json -> json
  | Error e -> fail ("transaction lookup error " ^ e.Octra_core.Rpc.message)

let test_transaction_lookup_response () =
  let tx = sample_tx Transaction.Standard in
  let selected =
    Tx_view.transaction_lookup
      ~pending:(Some tx)
      ~confirmed:(Some (9, "{}"))
      ~rejected:None
      ~dropped:None
  in
  begin
    match selected with
    | Tx_view.Lookup_pending selected_tx when selected_tx.Transaction.nonce = tx.Transaction.nonce -> ()
    | _ -> fail "lookup precedence pending"
  end;
  let pending = lookup_response "h1" (Tx_view.Lookup_pending tx) in
  if string_field "status" pending <> "pending" then fail "lookup pending status";
  if string_field "tx_hash" pending <> "h1" then fail "lookup pending hash";
  let tx_json = Yojson.Safe.to_string (Transaction.to_yojson tx) in
  let confirmed = lookup_response "h2" (Tx_view.Lookup_confirmed (9, tx_json)) in
  if string_field "status" confirmed <> "confirmed" then fail "lookup confirmed status";
  if int_field "epoch" confirmed <> 9 then fail "lookup confirmed epoch";
  let rejected =
    lookup_response "h3" (Tx_view.Lookup_rejected Tx_view.{
      rejected_from = "octfrom";
      rejected_to = "octto";
      rejected_amount = "1";
      rejected_nonce = 4;
      rejected_type = "bad";
      rejected_reason = "reason";
      rejected_epoch = 5;
      rejected_at = 6.0;
    })
  in
  if string_field "status" rejected <> "rejected" then fail "lookup rejected status";
  if string_field "source" rejected <> "rejected_txs" then fail "lookup rejected source";
  let dropped =
    lookup_response "h4" (Tx_view.Lookup_dropped Tx_view.{
      dropped_reason = "drop";
      dropped_detail = "detail";
      dropped_at = 1.0;
      dropped_from = "octfrom";
      dropped_to = "octto";
      dropped_nonce = 7;
      dropped_ou = Z.of_int 8;
      dropped_op = Transaction.Standard;
    })
  in
  if string_field "status" dropped <> "dropped" then fail "lookup dropped status";
  if string_field "ou" dropped <> "8" then fail "lookup dropped ou";
  begin
    match Tx_view.transaction_lookup_response ~decode_message:(fun s -> s) ~hash:"h5" Tx_view.Lookup_missing with
    | Error e when e.Octra_core.Rpc.code = 112 -> ()
    | Error e -> fail (Printf.sprintf "lookup missing code %d" e.Octra_core.Rpc.code)
    | Ok _ -> fail "lookup missing accepted"
  end

let test_staging_json () =
  let tx = sample_tx Transaction.EncryptOp in
  let row = Tx_view.staging_row ~decode_message:(fun s -> if s = "6869" then "hi" else s) tx in
  if string_field "stage_status" row <> "awaiting_epoch" then fail "staging status mismatch";
  if string_field "priority" row = "" then fail "priority empty";
  if string_field "message" row <> "hi" then fail "staging message mismatch";
  if bool_field "has_encrypted_data" row then fail "unexpected encrypted data marker";
  let response = Tx_view.staging_response ~decode_message:(fun s -> s) [tx] in
  if int_field "count" response <> 1 then fail "staging response count mismatch";
  if List.length (list_field "staged_transactions" response) <> 1 then fail "staging list mismatch"

let test_staging_submit_effects () =
  let tx_hash = String.make 64 'a' in
  let effects =
    Tx_view.staging_submit_effects
      ~relay:true
      ~peer_count:2
      ~total_txs:3
      ~total_ou:(Z.of_int 10)
      ~max_ou:(Z.of_int 7)
      ~tx_hash
  in
  if effects.Tx_view.total_txs <> 3 then fail "staging effects total";
  if not (Z.equal effects.total_ou (Z.of_int 10)) then fail "staging effects ou";
  if not (Z.equal effects.max_ou (Z.of_int 7)) then fail "staging effects max ou";
  begin
    match effects.relay_payload with
    | Some payload when String.length payload > 0 -> ()
    | _ -> fail "staging effects relay payload missing"
  end;
  let no_peer =
    Tx_view.staging_submit_effects
      ~relay:true
      ~peer_count:0
      ~total_txs:0
      ~total_ou:Z.zero
      ~max_ou:Z.zero
      ~tx_hash
  in
  if no_peer.Tx_view.relay_payload <> None then fail "staging effects no-peer relay";
  let no_relay =
    Tx_view.staging_submit_effects
      ~relay:false
      ~peer_count:2
      ~total_txs:0
      ~total_ou:Z.zero
      ~max_ou:Z.zero
      ~tx_hash
  in
  if no_relay.Tx_view.relay_payload <> None then fail "staging effects relay disabled"

let expect_ok name = function
  | Ok () -> ()
  | Error e -> fail (name ^ " rejected: " ^ e)

let expect_error name expected = function
  | Error e when e = expected -> ()
  | Error e -> fail (name ^ " error mismatch: " ^ e)
  | Ok () -> fail (name ^ " accepted")

let expect_payload_ok name = function
  | Ok () -> ()
  | Error (kind, reason) -> fail (name ^ " rejected: " ^ kind ^ " " ^ reason)

let expect_payload_error name expected = function
  | Error e when e = expected -> ()
  | Error (kind, reason) -> fail (name ^ " error mismatch: " ^ kind ^ " " ^ reason)
  | Ok () -> fail (name ^ " accepted")

let route_from = "octBnxVM2DQxLH93H3xBxAVRjHpyrTcM87FMnEFZ5t8RaXr"
let route_to = "oct7xCozDD9JEsbeVpo5C7HXp2BJbKqfmNUHmDDCCTtWcGb"

let route_tx ?(from_addr = route_from) ?(to_addr = route_to) op_type =
  { (sample_tx op_type) with
    Transaction.from = from_addr;
    to_ = to_addr;
  }

let test_route_admission () =
  expect_payload_ok "standard route address"
    (Tx_view.address_route_admission (route_tx Transaction.Standard));
  expect_payload_ok "stealth sentinel route"
    (Tx_view.address_route_admission (route_tx ~to_addr:"stealth" Transaction.StealthOp));
  expect_payload_ok "multi exec sentinel route"
    (Tx_view.address_route_admission (route_tx ~to_addr:"multi_exec" Transaction.MultiExec));
  expect_payload_error "invalid route address"
    ("invalid_address", "malformed sender or recipient address")
    (Tx_view.address_route_admission (route_tx ~to_addr:"bad" Transaction.Standard));
  expect_payload_error "short noncanonical route address"
    ("invalid_address", "malformed sender or recipient address")
    (Tx_view.address_route_admission
       (route_tx
          ~to_addr:(String.sub route_to 0 (String.length route_to - 1))
          Transaction.Standard));
  expect_payload_error "long noncanonical route address"
    ("invalid_address", "malformed sender or recipient address")
    (Tx_view.address_route_admission
       (route_tx ~to_addr:(route_to ^ "1") Transaction.Standard));
  expect_payload_error "bad stealth recipient"
    ("invalid_address", "malformed sender or recipient address")
    (Tx_view.address_route_admission (route_tx Transaction.StealthOp));
  expect_payload_ok "standard non-self"
    (Tx_view.self_route_admission (route_tx Transaction.Standard));
  expect_payload_error "standard self"
    ("self_transfer", "sender and recipient are the same")
    (Tx_view.self_route_admission (route_tx ~to_addr:route_from Transaction.Standard));
  expect_payload_error "encrypt non-self"
    ("self_only_operation", "encrypt operation must target sender")
    (Tx_view.self_route_admission (route_tx Transaction.EncryptOp));
  expect_payload_ok "encrypt self"
    (Tx_view.self_route_admission (route_tx ~to_addr:route_from Transaction.EncryptOp));
  expect_payload_error "private self"
    ("self_transfer", "private transfer must target different address")
    (Tx_view.self_route_admission (route_tx ~to_addr:route_from Transaction.PrivateOp));
  expect_payload_error "claim non-self"
    ("self_only_operation", "claim operation must target sender")
    (Tx_view.self_route_admission (route_tx Transaction.ClaimOp));
  expect_payload_error "op01 burn non-self"
    ("self_only_operation", "op01_burn must target sender (self-tx)")
    (Tx_view.self_route_admission (route_tx Transaction.Op01Burn))

let test_admission_semantics () =
  if not (Tx_view.requires_heavy_preverify (sample_tx Transaction.StealthOp)) then
    fail "stealth not classified heavy";
  if not (Tx_view.requires_heavy_preverify (sample_tx Transaction.EncryptOp)) then
    fail "encrypt not classified heavy";
  if Tx_view.requires_heavy_preverify (sample_tx Transaction.Standard) then
    fail "standard classified heavy";
  let low_stealth = sample_tx Transaction.StealthOp in
  let high_encrypt = sample_tx ~ou:(Z.of_int 100) Transaction.EncryptOp in
  let low_standard = sample_tx Transaction.Standard in
  let victims =
    Tx_view.low_fee_heavy_hashes
      ~min_ou:(Z.of_int 100)
      [low_stealth; high_encrypt; low_standard]
  in
  if victims <> [Transaction.hash low_stealth] then
    fail "low fee heavy sweep mismatch";
  expect_error "heavy low fee"
    "fee too low: stealth requires ou >= 100, got 9"
    (Tx_view.admission_fee_check ~min_ou:(Z.of_int 100) (sample_tx Transaction.StealthOp));
  expect_ok "heavy sufficient fee"
    (Tx_view.admission_fee_check ~min_ou:(Z.of_int 9) (sample_tx Transaction.StealthOp));
  expect_ok "standard fee bypass"
    (Tx_view.admission_fee_check ~min_ou:(Z.of_int 100) (sample_tx Transaction.Standard));
  expect_error "negative amount"
    "amount must not be negative"
    (Tx_view.semantic_check (sample_tx ~amount:(Z.of_int (-1)) Transaction.ProgramExec));
  expect_error "negative ou"
    "ou must not be negative"
    (Tx_view.semantic_check (sample_tx ~ou:(Z.of_int (-1)) Transaction.Standard));
  expect_error "standard zero"
    "amount must be positive"
    (Tx_view.semantic_check (sample_tx ~amount:Z.zero Transaction.Standard));
  expect_error "zero-only op amount"
    "amount must be zero for this operation"
    (Tx_view.semantic_check (sample_tx Transaction.ContractDeploy));
  expect_ok "program exec payable"
    (Tx_view.semantic_check (sample_tx Transaction.ProgramExec));
  expect_error "multi exec amount"
    "amount must be zero for multi_exec"
    (Tx_view.semantic_check (sample_tx Transaction.MultiExec));
  expect_error "encrypt zero"
    "amount must be positive"
    (Tx_view.semantic_check (sample_tx ~amount:Z.zero Transaction.EncryptOp));
  expect_error "stealth amount"
    "amount must be zero for stealth transfer"
    (Tx_view.semantic_check (sample_tx Transaction.StealthOp));
  expect_error "key switch amount"
    "amount must be zero for key_switch"
    (Tx_view.semantic_check (sample_tx Transaction.KeySwitch));
  expect_error "op01 burn zero"
    "amount must be positive for op01_burn"
    (Tx_view.semantic_check (sample_tx ~amount:Z.zero Transaction.Op01Burn));
  expect_ok "staging submit ok"
    (Tx_view.staging_submit_admission
       ~min_ou:(Z.of_int 100)
       ~min_relay_fee:(Z.of_int 9)
       (sample_tx ~ou:(Z.of_int 1_000) Transaction.Standard));
  expect_error "staging heavy fee"
    "fee too low: stealth requires ou >= 100, got 9"
    (Tx_view.staging_submit_admission
       ~min_ou:(Z.of_int 100)
       ~min_relay_fee:(Z.of_int 9)
       (sample_tx Transaction.StealthOp));
  expect_error "staging min relay"
    "fee too low (min: 1000)"
    (Tx_view.staging_submit_admission
       ~min_ou:(Z.of_int 100)
       ~min_relay_fee:(Z.of_int 10)
       (sample_tx Transaction.Standard));
  let program =
    sample_tx
      ~amount:Z.zero
      ~ou:(Z.of_int 200_000)
      ~encrypted_data:(String.make 4_096 'x')
      Transaction.ProgramDeploy
  in
  expect_error "staging Program fee floor"
    "fee too low (min: 204000)"
    (Tx_view.staging_submit_admission
       ~min_ou:Z.one
       ~min_relay_fee:Z.one
       program);
  expect_ok "staging Program fee floor accepted"
    (Tx_view.staging_submit_admission
       ~min_ou:Z.one
       ~min_relay_fee:Z.one
       { program with Transaction.ou = Z.of_int 204_000 });
  expect_error "staging semantic"
    "amount must not be negative"
    (Tx_view.staging_submit_admission
       ~min_ou:(Z.of_int 100)
       ~min_relay_fee:(Z.of_int 9)
       (sample_tx ~amount:(Z.of_int (-1)) Transaction.ProgramExec))

let test_staging_remove_auth () =
  let sender = "octBnxVM2DQxLH93H3xBxAVRjHpyrTcM87FMnEFZ5t8RaXr" in
  let priv_b64 = "plv3RpuPxlopIP2cAwF0PLPA29S2flKL8J9DX/x8/ls=" in
  let pub_b64 = "GZqVq3wLRzWIshIoGhIKWLMe4RRowpD2u5eL93JJmuc=" in
  let tx_hash = "445566778899aabbccddeeff00112233445566778899aabbccddeeff00112233" in
  let msg = Tx_view.staging_remove_message tx_hash in
  let signature_b64 = Peer_auth.sign msg priv_b64 in
  let req =
    match Tx_view.staging_remove_params (`List [
      `String tx_hash;
      `String signature_b64;
      `String pub_b64;
    ]) with
    | Ok value -> value
    | Error e -> fail ("valid staging remove params rejected: " ^ e.Octra_core.Rpc.message)
  in
  if req.Tx_view.staging_remove_hash <> tx_hash then fail "staging remove hash mismatch";
  if req.Tx_view.staging_remove_signature_b64 <> signature_b64 then
    fail "staging remove signature mismatch";
  if req.Tx_view.staging_remove_pubkey_b64 <> pub_b64 then
    fail "staging remove pubkey mismatch";
  begin
    match Tx_view.staging_remove_params (`List []) with
    | Error e when e.Octra_core.Rpc.code = -32602 -> ()
    | Error e -> fail (Printf.sprintf "staging remove missing code %d" e.Octra_core.Rpc.code)
    | Ok _ -> fail "staging remove accepted missing hash"
  end;
  begin
    match Tx_view.staging_remove_auth
            ~sender
            ~tx_hash
            ~signature_b64
            ~pubkey_b64:pub_b64 with
    | Ok () -> ()
    | Error e -> fail ("valid staging remove auth rejected: " ^ e)
  end;
  begin
    match Tx_view.staging_remove_auth
            ~sender
            ~tx_hash
            ~signature_b64:(Peer_auth.sign (msg ^ ":other") priv_b64)
            ~pubkey_b64:pub_b64 with
    | Error "signature verification failed" -> ()
    | Error e -> fail ("wrong signature error changed: " ^ e)
    | Ok () -> fail "wrong staging remove signature accepted"
  end;
  begin
    match Tx_view.staging_remove_auth
            ~sender:"oct11111111111111111111111111111111111111111111"
            ~tx_hash
            ~signature_b64
            ~pubkey_b64:pub_b64 with
    | Error "pubkey does not match tx sender" -> ()
    | Error e -> fail ("wrong sender error changed: " ^ e)
    | Ok () -> fail "wrong staging remove sender accepted"
  end;
  begin
    match Tx_view.staging_remove_request_auth ~sender req with
    | Ok () -> ()
    | Error e -> fail ("valid staging remove request rejected: " ^ e)
  end

let test_pubkey_registration_auth () =
  let addr = "octBnxVM2DQxLH93H3xBxAVRjHpyrTcM87FMnEFZ5t8RaXr" in
  let priv_b64 = "plv3RpuPxlopIP2cAwF0PLPA29S2flKL8J9DX/x8/ls=" in
  let pub_b64 = "GZqVq3wLRzWIshIoGhIKWLMe4RRowpD2u5eL93JJmuc=" in
  let msg = Tx_view.public_key_registration_message addr in
  let signature_b64 = Peer_auth.sign msg priv_b64 in
  begin
    match Tx_view.public_key_registration_auth
            ~addr
            ~pubkey_b64:pub_b64
            ~signature_b64 with
    | Ok () -> ()
    | Error e -> fail ("valid public key registration rejected: " ^ e)
  end;
  begin
    match Tx_view.public_key_registration_auth
            ~addr
            ~pubkey_b64:pub_b64
            ~signature_b64:(Peer_auth.sign (msg ^ ":bad") priv_b64) with
    | Error "signature verification failed" -> ()
    | Error e -> fail ("wrong registration signature error changed: " ^ e)
    | Ok () -> fail "wrong registration signature accepted"
  end;
  begin
    match Tx_view.public_key_registration_auth
            ~addr:"oct11111111111111111111111111111111111111111111"
            ~pubkey_b64:pub_b64
            ~signature_b64 with
    | Error "public key does not match address" -> ()
    | Error e -> fail ("wrong registration address error changed: " ^ e)
    | Ok () -> fail "wrong registration address accepted"
  end;
  begin
    match Tx_view.public_key_registration_auth
            ~addr
            ~pubkey_b64:"bad"
            ~signature_b64 with
    | Error "invalid key or signature length" -> ()
    | Error e -> fail ("bad registration length error changed: " ^ e)
    | Ok () -> fail "bad registration length accepted"
  end

let test_encrypted_balance_auth () =
  let addr = "octBnxVM2DQxLH93H3xBxAVRjHpyrTcM87FMnEFZ5t8RaXr" in
  let priv_b64 = "plv3RpuPxlopIP2cAwF0PLPA29S2flKL8J9DX/x8/ls=" in
  let pub_b64 = "GZqVq3wLRzWIshIoGhIKWLMe4RRowpD2u5eL93JJmuc=" in
  let msg = Tx_view.encrypted_balance_message addr in
  let signature_b64 = Peer_auth.sign msg priv_b64 in
  let params = `List [`String addr; `String signature_b64; `String pub_b64] in
  begin
    match Tx_view.encrypted_balance_auth params ~addr with
    | Ok () -> ()
    | Error _ -> fail "valid encrypted balance auth rejected"
  end;
  begin
    match Tx_view.encrypted_balance_auth (`List [`String addr]) ~addr with
    | Error (Tx_view.Rpc_malformed "encryptedBalance requires [address, signature, pubkey]") -> ()
    | Error _ -> fail "encrypted balance missing args error changed"
    | Ok () -> fail "encrypted balance missing args accepted"
  end;
  begin
    let wrong_params =
      `List [`String addr; `String (Peer_auth.sign (msg ^ ":bad") priv_b64); `String pub_b64] in
    match Tx_view.encrypted_balance_auth wrong_params ~addr with
    | Error (Tx_view.Rpc_auth_error "signature verification failed") -> ()
    | Error _ -> fail "encrypted balance wrong signature error changed"
    | Ok () -> fail "encrypted balance wrong signature accepted"
  end;
  begin
    match Tx_view.encrypted_balance_auth params ~addr:"oct11111111111111111111111111111111111111111111" with
    | Error (Tx_view.Rpc_auth_error "pubkey does not match address") -> ()
    | Error _ -> fail "encrypted balance wrong address error changed"
    | Ok () -> fail "encrypted balance wrong address accepted"
  end

let test_standard_fields () =
  let fields =
    Tx_view.standard_fields
      ~decode_message:(fun s -> if s = "6869" then "hi" else s)
      ~from_addr:"fa"
      ~to_addr:"ta"
      ~amount_raw:"1000000"
      ~nonce:5
      ~ou:"7"
      ~timestamp:8.0
      ~message:(Some "6869")
  in
  let json = `Assoc fields in
  if string_field "op_type" json <> "standard" then fail "standard op mismatch";
  if string_field "from" json <> "fa" then fail "standard from mismatch";
  if string_field "amount_raw" json <> "1000000" then fail "standard amount mismatch";
  if string_field "message" json <> "hi" then fail "standard message mismatch"

let test_encrypted_payload_admission () =
  if Tx_view.encrypted_payload_ok (sample_tx Transaction.EncryptOp) then
    fail "encrypt without payload admitted";
  if Tx_view.encrypted_payload_ok (sample_tx ~encrypted_data:"{}" Transaction.EncryptOp) then
    fail "malformed encrypt admitted";
  if Tx_view.encrypted_payload_ok (sample_tx Transaction.KeySwitch) then
    fail "key switch without payload admitted";
  if Tx_view.encrypted_payload_ok (sample_tx ~encrypted_data:"{}" Transaction.KeySwitch) then
    fail "malformed key switch admitted";
  if not (Tx_view.encrypted_payload_ok (sample_tx Transaction.PrivateOp)) then
    fail "disabled private op changed";
  if not (Tx_view.encrypted_payload_ok (sample_tx Transaction.RecryptOp)) then
    fail "disabled recrypt op changed";
  if not (Tx_view.encrypted_payload_ok (sample_tx Transaction.Standard)) then
    fail "standard tx rejected"

let test_call_params_admission () =
  let harmless =
    { (sample_tx Transaction.ProgramExec) with
      Transaction.message = Some "[\"hello\",\"2 < 3\"]";
    }
  in
  if not (Tx_view.call_params_ok harmless) then fail "harmless program params rejected";
  let malicious =
    { (sample_tx Transaction.ProgramExec) with
      Transaction.message = Some "[\"<script>alert(1)</script>\"]";
    }
  in
  if Tx_view.call_params_ok malicious then fail "malicious program params admitted";
  let nested =
    { (sample_tx Transaction.CircleCall) with
      Transaction.message = Some "[{\"value\":\"<script>\"}]";
    }
  in
  if not (Tx_view.call_params_ok nested) then fail "legacy nested params behavior changed";
  let malformed =
    { (sample_tx Transaction.MultiExec) with
      Transaction.message = Some "[";
    }
  in
  if not (Tx_view.call_params_ok malformed) then fail "malformed params behavior changed";
  let standard =
    { (sample_tx Transaction.Standard) with
      Transaction.message = Some "[\"<script>\"]";
    }
  in
  if not (Tx_view.call_params_ok standard) then fail "standard params behavior changed"

let payload_limits = Tx_view.{
  encrypted_data_len = 5;
  message_len = 4;
  message_zkp_len = 8;
  message_validator_len = 10;
  message_program_len = 100;
}

let test_payload_admission () =
  expect_payload_ok "standard payload"
    (Tx_view.payload_admission ~limits:payload_limits (sample_tx Transaction.Standard));
  let message_too_large =
    { (sample_tx Transaction.Standard) with
      Transaction.message = Some "12345";
    }
  in
  expect_payload_error "message too large"
    ("malformed_transaction", "encrypted_data or message exceeds size limit")
    (Tx_view.payload_admission ~limits:payload_limits message_too_large);
  expect_payload_error "encrypted data too large"
    ("malformed_transaction", "encrypted_data or message exceeds size limit")
    (Tx_view.payload_admission
       ~limits:payload_limits
       (sample_tx ~encrypted_data:"123456" Transaction.Standard));
  expect_payload_error "encrypt missing payload"
    ("malformed_transaction", "malformed or oversized encrypted_data")
    (Tx_view.payload_admission ~limits:payload_limits (sample_tx Transaction.EncryptOp));
  let malicious =
    { (sample_tx Transaction.ProgramExec) with
      Transaction.message = Some "[\"<script>alert(1)</script>\"]";
    }
  in
  expect_payload_error "program params html"
    ("malformed_transaction", "contract call params contain invalid characters")
    (Tx_view.payload_admission ~limits:payload_limits malicious)

let test_deep_message () =
  let deep = String.make 250_000 '[' ^ "0" ^ String.make 250_000 ']' in
  List.iter (fun op ->
    let tx = { (sample_tx op) with Transaction.message = Some deep } in
    if not (Tx_view.call_params_ok tx) then fail "deep message refused";
    if op <> Transaction.ProgramDeploy then
      expect_payload_ok "deep message"
        (Tx_view.payload_admission ~limits:{ payload_limits with
          message_program_len = 10_000_000 } tx))
    [Transaction.ContractCall; Transaction.ProgramExec; Transaction.MultiExec;
     Transaction.ContractDeploy; Transaction.ProgramDeploy; Transaction.CircleCall]

let pre_route_ok tx =
  Tx_view.pre_route_admission
    ~now:12.5
    ~max_timestamp_drift:1.0
    ~observer_rpc_mode:false
    ~bft_mode:false
    tx

let test_pre_route_admission () =
  expect_payload_ok "pre route standard"
    (pre_route_ok (sample_tx Transaction.Standard));
  expect_payload_error "pre route timestamp"
    ("malformed_transaction", "timestamp drift 8s exceeds 1s limit")
    (Tx_view.pre_route_admission
       ~now:20.5
       ~max_timestamp_drift:1.0
       ~observer_rpc_mode:false
       ~bft_mode:false
       (sample_tx Transaction.Standard));
  expect_payload_error "pre route observer"
    ("read_only_observer", "observer node is read-only")
    (Tx_view.pre_route_admission
       ~now:12.5
       ~max_timestamp_drift:1.0
       ~observer_rpc_mode:true
       ~bft_mode:false
       (sample_tx Transaction.Standard));
  expect_payload_error "pre route bft op"
    ("unsupported_operation", Transaction.bft_reject_reason Transaction.EncryptOp)
    (Tx_view.pre_route_admission
       ~now:12.5
       ~max_timestamp_drift:1.0
       ~observer_rpc_mode:false
       ~bft_mode:true
       (sample_tx Transaction.EncryptOp))

let test_bft_op_admission () =
  expect_payload_ok "bft standard"
    (Tx_view.bft_op_admission
       ~bft_mode:true
       (sample_tx Transaction.Standard));
  expect_payload_error "bft private blocked"
    ("unsupported_operation", Transaction.bft_reject_reason Transaction.StealthOp)
    (Tx_view.bft_op_admission
       ~bft_mode:true
       (sample_tx Transaction.StealthOp));
  expect_payload_ok "single private unchanged"
    (Tx_view.bft_op_admission
       ~bft_mode:false
       (sample_tx Transaction.StealthOp))

let test_signature_admission () =
  let signed_tx, pub_b64 = signed_sample_tx () in
  expect_payload_ok "signature valid"
    (Tx_view.signature_admission ~account_public_key:None signed_tx);
  let wire_pubkey_ignored =
    { signed_tx with Transaction.public_key = Some "not-base64"; }
  in
  expect_payload_ok "signature account pubkey priority"
    (Tx_view.signature_admission
       ~account_public_key:(Some pub_b64)
       wire_pubkey_ignored);
  expect_payload_error "signature missing pubkey"
    ("invalid_signature", "no public key available")
    (Tx_view.signature_admission
       ~account_public_key:None
       (sample_tx Transaction.Standard));
  let wrong_addr =
    { signed_tx with Transaction.from = "octwrong"; }
  in
  expect_payload_error "signature address mismatch"
    ("invalid_address", "address does not match public key")
    (Tx_view.signature_admission ~account_public_key:None wrong_addr);
  let wrong_signature =
    { signed_tx with Transaction.signature = Base64.encode_exn (String.make 64 '\000'); }
  in
  expect_payload_error "signature invalid"
    ("invalid_signature", "signature verification failed")
    (Tx_view.signature_admission ~account_public_key:None wrong_signature)

let test_sender_admission () =
  expect_payload_ok "sender present"
    (Tx_view.sender_admission
       ~sender_exists:true
       (sample_tx Transaction.Standard));
  expect_payload_error "sender ou"
    ("malformed_transaction", "OU must be greater than zero")
    (Tx_view.sender_admission
       ~sender_exists:true
       (sample_tx ~ou:Z.zero Transaction.Standard));
  expect_payload_error "sender missing"
    ("sender_not_found", "sender account does not exist")
    (Tx_view.sender_admission
       ~sender_exists:false
       (sample_tx Transaction.Standard))

let submit_pre_signature_ok tx =
  Tx_view.submit_pre_signature_admission
    ~now:12.5
    ~max_timestamp_drift:1.0
    ~observer_rpc_mode:false
    ~bft_mode:false
    ~limits:payload_limits
    ~sender_exists:true
    tx

let test_submit_pre_signature () =
  expect_payload_ok "submit pre signature standard"
    (submit_pre_signature_ok (route_tx Transaction.Standard));
  let semantic_bad =
    { (route_tx Transaction.Standard) with
      Transaction.amount = Z.zero;
    }
  in
  expect_payload_error "submit pre signature semantic"
    ("malformed_transaction", "amount must be positive")
    (submit_pre_signature_ok semantic_bad);
  expect_payload_error "submit pre signature missing sender"
    ("sender_not_found", "sender account does not exist")
    (Tx_view.submit_pre_signature_admission
       ~now:12.5
       ~max_timestamp_drift:1.0
       ~observer_rpc_mode:false
       ~bft_mode:false
       ~limits:payload_limits
       ~sender_exists:false
       (route_tx Transaction.Standard));
  expect_payload_error "submit pre signature route first"
    ("invalid_address", "malformed sender or recipient address")
    (submit_pre_signature_ok (route_tx ~to_addr:"bad" Transaction.Standard))

let valid_stealth_payload =
  `Assoc [
    "version", `Int 5;
    "delta_cipher", `String "cipher";
    "commitment", `String "commitment";
    "range_proof_delta", `String "range_delta";
    "range_proof_balance", `String "range_balance";
    "eph_pub", `String "eph";
    "stealth_tag", `String "tag";
    "enc_amount", `String "amount";
    "claim_pub", `String "claim";
    "amount_commitment", `String "amount_commitment";
    "send_zero_proof", `String "zero";
  ]
  |> Yojson.Safe.to_string

let stealth_payload_with_cipher delta_cipher =
  `Assoc [
    "version", `Int 5;
    "delta_cipher", `String delta_cipher;
    "commitment", `String (Base64.encode_exn (String.make 32 '\001'));
    "range_proof_delta", `String "rp_v1|YQ==";
    "range_proof_balance", `String "rp_v1|Yg==";
    "eph_pub", `String "eph";
    "stealth_tag", `String (String.make 32 '1');
    "enc_amount", `String "amount";
    "claim_pub", `String (String.make 64 '2');
    "amount_commitment", `String (Base64.encode_exn (String.make 32 '\003'));
    "send_zero_proof", `String "";
  ]
  |> Yojson.Safe.to_string

let test_stealth_delta_layer_limit () =
  let wallet_seed = Base64.encode_exn (String.make 32 '\017') in
  let pk, sk = FheBalance.derive_pvac_keys wallet_seed in
  let scalar =
    Pvac_ffi.enc_zero_seeded pk sk (Bytes.make 32 '\019')
    |> FheBalance.encode_cipher
  in
  let layered =
    Pvac_ffi.ct_mul_seeded
      pk
      (FheBalance.decode_cipher scalar |> Result.get_ok)
      (FheBalance.decode_cipher scalar |> Result.get_ok)
      (Bytes.make 32 '\023')
    |> FheBalance.encode_cipher
  in
  let layered_tx =
    sample_tx
      ~encrypted_data:(stealth_payload_with_cipher layered)
      Transaction.StealthOp
  in
  let layered_payload =
    match Tx_view.preverify_stealth_payload layered_tx with
    | Some payload -> payload
    | None -> fail "layered stealth sample did not parse"
  in
  let pubkey_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey pk) in
  begin
    match
      Lwt_main.run
        (Tx_view.preverify_stealth_ranges ~math:false
           ~strict:false
           ~pubkey_blob
           ~sender_enc:scalar
           layered_payload)
    with
    | Error (Tx_view.Preverify_invalid _) -> ()
    | Error (Tx_view.Preverify_unavailable reason) ->
      fail
        ("layered stealth preverify unavailable: "
         ^ Tx_view.preverify_unavailable_message reason)
    | Ok _ -> fail "layered stealth cipher reached subtraction"
  end;
  expect_payload_error "layered stealth admission"
    ("malformed_transaction", "malformed or oversized encrypted_data")
    (Tx_view.payload_admission ~limits:Tx_view.payload_limits layered_tx);
  expect_payload_ok "wrapped scalar stealth admission"
    (Tx_view.payload_admission
       ~limits:Tx_view.payload_limits
       (sample_tx
          ~encrypted_data:(stealth_payload_with_cipher scalar)
          Transaction.StealthOp))

let test_preverify_stealth_payload () =
  begin
    match Tx_view.preverify_stealth_payload
            (sample_tx ~encrypted_data:valid_stealth_payload Transaction.Standard) with
    | None -> ()
    | Some _ -> fail "standard tx scheduled stealth preverify"
  end;
  begin
    match Tx_view.preverify_stealth_payload
            (sample_tx ~encrypted_data:"{" Transaction.StealthOp) with
    | None -> ()
    | Some _ -> fail "malformed stealth payload scheduled preverify"
  end;
  match Tx_view.preverify_stealth_payload
          (sample_tx ~encrypted_data:valid_stealth_payload Transaction.StealthOp) with
  | Some payload ->
    if payload.Octra_core.Crypto.PrivateTransferV4.version <> 5 then
      fail "stealth preverify payload version changed";
    let seed = Base64.encode_exn (String.make 32 '\017') in
    let pk, sk = FheBalance.derive_pvac_keys seed in
    let pubkey_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey pk) in
    let cipher =
      Pvac_ffi.enc_zero_seeded pk sk (Bytes.make 32 '\019')
      |> FheBalance.encode_cipher in
    let payload = {
      payload with
      Octra_core.Crypto.PrivateTransferV4.delta_cipher = cipher;
    } in
    begin
      match
        Lwt_main.run
          (Tx_view.preverify_stealth_ranges ~math:false
           ~strict:false
             ~pubkey_blob
             ~sender_enc:cipher
             payload)
      with
      | Ok (false, false) -> ()
      | Ok _ -> fail "stealth preverify accepted bogus ranges"
      | Error (Tx_view.Preverify_invalid reason) ->
        fail ("stealth preverify rejected valid pubkey: " ^ reason)
      | Error (Tx_view.Preverify_unavailable reason) ->
        fail
          ("stealth preverify unavailable: "
           ^ Tx_view.preverify_unavailable_message reason)
    end
  | None -> fail "valid stealth payload did not schedule preverify"

let test_preverify_saturation () =
  let cache_key = "saturated-preverify" in
  Preverify_cache.remove cache_key;
  let mutex = Mutex.create () in
  let condition = Condition.create () in
  let released = ref false in
  let block () =
    Mutex.lock mutex;
    while not !released do
      Condition.wait condition mutex
    done;
    Mutex.unlock mutex
  in
  let occupied =
    Octra_core.Proof_pool.capacity
    + Octra_core.Resource_lanes.preverify_speculative_queue_limit
  in
  let blockers =
    List.init occupied (fun _ ->
      Octra_core.Proof_pool.try_run
        ~priority:Octra_core.Compute_pool.Speculative
        block)
  in
  let completed, resolve_completed = Lwt.task () in
  let compute () =
    let open Lwt.Syntax in
    let* value = Tx_view.run_preverify_compute (fun () -> 42) in
    match value with
    | Error reason -> Lwt.return (Preverify_submit.Unavailable reason)
    | Ok value ->
      let result =
        Preverify_submit.{
          delta_ok = true;
          balance_ok = true;
          sender_enc_snapshot = string_of_int value;
          strict = false;
          math = false;
        }
      in
      if Lwt.is_sleeping completed then Lwt.wakeup resolve_completed result;
      Lwt.return (Preverify_submit.Checked result)
  in
  begin
    match Preverify_cache.start_task cache_key compute with
    | Preverify_submit.Started -> ()
    | _ -> fail "saturated preverify task was not admitted"
  end;
  begin
    match Preverify_cache.state cache_key with
    | Preverify_cache.Pending -> ()
    | Preverify_cache.Missing -> fail "saturated preverify task is missing"
    | Preverify_cache.Ready -> fail "saturated preverify completed before capacity release"
    | Preverify_cache.Unavailable_state reason ->
      fail
        ("saturated preverify unavailable: "
         ^ Tx_view.preverify_unavailable_message reason)
    | Preverify_cache.Failed reason -> fail ("saturated preverify failed: " ^ reason)
  end;
  begin
    match
      Preverify_cache.gate
        ~state:(Preverify_cache.state cache_key)
        ~defer_count:0
        ~max_defer:2
    with
    | Preverify_cache.Defer_gate { next_count = 1; status = "pending" } -> ()
    | _ -> fail "saturated preverify did not defer"
  end;
  Mutex.lock mutex;
  released := true;
  Condition.broadcast condition;
  Mutex.unlock mutex;
  let result =
    Lwt_main.run
      (let open Lwt.Syntax in
       let* blocker_results = Lwt.all blockers in
       if List.exists Option.is_none blocker_results then
         fail "proof pool rejected a reserved saturation slot";
       completed)
  in
  if result.Preverify_submit.sender_enc_snapshot <> "42" then
    fail "saturated preverify result changed";
  begin
    match Preverify_cache.state cache_key with
    | Preverify_cache.Ready -> ()
    | _ -> fail "saturated preverify did not become ready"
  end;
  Preverify_cache.remove cache_key

let test_preverify_submit_result () =
  let ok =
    Preverify_submit.result_of_ranges
      ~math:false
      ~strict:false
      ~sender_enc_snapshot:"cipher-a"
      (Ok (true, false)) in
  begin
    match ok with
    | Preverify_submit.Checked result ->
      if not result.Preverify_submit.delta_ok then fail "preverify submit delta";
      if result.Preverify_submit.balance_ok then fail "preverify submit balance";
      if result.Preverify_submit.sender_enc_snapshot <> "cipher-a" then
        fail "preverify submit snapshot"
    | Preverify_submit.Unavailable _ -> fail "preverify submit became unavailable"
  end;
  let bad =
    Preverify_submit.result_of_ranges
      ~math:false
      ~strict:false
      ~sender_enc_snapshot:"cipher-b"
      (Error (Tx_view.Preverify_invalid "bad pubkey")) in
  begin
    match bad with
    | Preverify_submit.Checked result ->
      if result.Preverify_submit.delta_ok then fail "preverify submit error delta";
      if result.Preverify_submit.balance_ok then fail "preverify submit error balance";
      if result.Preverify_submit.sender_enc_snapshot <> "cipher-b" then
        fail "preverify submit error snapshot"
    | Preverify_submit.Unavailable _ -> fail "invalid pubkey became unavailable"
  end;
  match
    Preverify_submit.result_of_ranges
      ~math:false
      ~strict:false
      ~sender_enc_snapshot:"cipher-c"
      (Error
         (Tx_view.Preverify_unavailable
            (Tx_view.Proof_worker_unavailable "worker_missing")))
  with
  | Preverify_submit.Unavailable
      (Tx_view.Proof_worker_unavailable "worker_missing") -> ()
  | _ -> fail "preverify unavailable classification"

let test_preverify_tx_launch () =
  let launches = ref 0 in
  let inserts = ref [] in
  let sender_snapshots = ref [] in
  let launcher : Preverify_submit.launcher = {
    get_pvac_pubkey = (fun _ -> Lwt.return (Some "pk"));
    sender_enc = (fun addr -> "cipher:" ^ addr);
    start_task = (fun ~math:_ hash f ->
      incr launches;
      inserts := hash :: !inserts;
      ignore (f ());
      Preverify_submit.Started);
    strict = (fun () -> false);
    math = (fun () -> false);
    verify_ranges = (fun ~math:_ ~strict:_ ~pubkey_blob:_ ~sender_enc ptd ->
      sender_snapshots := sender_enc :: !sender_snapshots;
      Lwt.return (Ok (ptd.Octra_core.Crypto.PrivateTransferV4.version = 5, false)));
    now = (fun () -> 1.0);
  } in
  begin
    match
      Preverify_submit.launch_for_tx
        launcher
        ~tx_hash:"standard-hash"
        (sample_tx Transaction.Standard)
    with
    | Preverify_submit.Unmanaged -> ()
    | _ -> fail "standard tx managed by stealth preverify"
  end;
  if !launches <> 0 then fail "standard tx launched preverify";
  begin
    match
      Preverify_submit.launch_for_tx
        launcher
        ~tx_hash:"stealth-hash"
        (sample_tx ~encrypted_data:valid_stealth_payload Transaction.StealthOp)
    with
    | Preverify_submit.Started -> ()
    | _ -> fail "stealth tx did not start preverify"
  end;
  if !launches <> 1 then fail "stealth tx did not launch preverify";
  if !inserts <> ["stealth-hash"] then fail "stealth preverify insert hash mismatch";
  if !sender_snapshots <> ["cipher:octfrom"] then fail "stealth preverify sender snapshot mismatch";
  let busy_launcher = {
    launcher with
    Preverify_submit.start_task = (fun ~math:_ _ _ -> Preverify_submit.Busy);
  } in
  match
    Preverify_submit.launch_for_tx
      busy_launcher
      ~tx_hash:"busy-hash"
      (sample_tx ~encrypted_data:valid_stealth_payload Transaction.StealthOp)
  with
  | Preverify_submit.Busy -> ()
  | _ -> fail "busy stealth preverify was accepted"

let test_preverify_cache_plans () =
  let entry cache_key cache_ts cache_pending =
    Preverify_submit.{ cache_key; cache_ts; cache_pending }
  in
  let prune =
    Preverify_submit.cache_prune_plan
      ~now:100.0
      ~ttl:10.0
      ~max_entries:2
      [
        entry "old" 80.0 false;
        entry "fresh" 95.0 false;
        entry "stale-pending" 50.0 true;
        entry "newest" 99.0 false;
      ]
  in
  if prune.Preverify_submit.prune_expired <> ["old"] then
    fail "preverify cache expired plan mismatch";
  if prune.prune_overflow <> [] then fail "preverify cache unexpected overflow";
  let overflow =
    Preverify_submit.cache_prune_plan
      ~now:100.0
      ~ttl:100.0
      ~max_entries:2
      [
        entry "oldest" 90.0 false;
        entry "older" 91.0 false;
        entry "pending" 92.0 true;
        entry "newer" 93.0 false;
      ]
  in
  if overflow.Preverify_submit.prune_overflow <> ["oldest"] then
    fail "preverify cache overflow plan mismatch";
  let hard =
    Preverify_submit.hard_cap_plan
      ~max_entries:3
      [
        entry "oldest" 90.0 false;
        entry "older" 91.0 false;
        entry "pending" 92.0 true;
        entry "newer" 93.0 false;
      ]
  in
  if hard.Preverify_submit.hard_cap_requested_drop <> 2 then
    fail "preverify hard cap requested mismatch";
  if hard.hard_cap_drop <> ["oldest"; "older"] then
    fail "preverify hard cap drop mismatch";
  let none =
    Preverify_submit.hard_cap_plan
      ~max_entries:4
      [entry "a" 1.0 false; entry "b" 2.0 false]
  in
  if none.Preverify_submit.hard_cap_drop <> [] then fail "preverify hard cap unexpected drop"

let post_signature_ok tx =
  Tx_view.post_signature_admission
    ~preverify_has_capacity:true
    ~preverify_pending:0
    ~preverify_pending_max:2
    ~bft_mode:false
    ~confirmed_nonce:7
    ~bft_window:32
    tx

let test_post_signature_admission () =
  expect_payload_ok "post signature standard"
    (post_signature_ok (sample_tx Transaction.Standard));
  expect_payload_error "post signature recrypt payload"
    ("malformed_transaction", "FHE op requires encrypted_data with cipher + proofs")
    (post_signature_ok (sample_tx Transaction.RecryptOp));
  expect_payload_error "post signature stealth busy"
    ("pre_verify_busy", "stealth pre-verify busy (2/2), retry shortly")
    (Tx_view.post_signature_admission
       ~preverify_has_capacity:false
       ~preverify_pending:2
       ~preverify_pending_max:2
       ~bft_mode:false
       ~confirmed_nonce:7
       ~bft_window:32
       (sample_tx ~encrypted_data:"payload" Transaction.StealthOp));
  expect_payload_error "post signature bft nonce"
    ("nonce_too_far", "nonce too far ahead for BFT admission window (confirmed=7 window=2)")
    (Tx_view.post_signature_admission
       ~preverify_has_capacity:true
       ~preverify_pending:0
       ~preverify_pending_max:2
       ~bft_mode:true
       ~confirmed_nonce:7
       ~bft_window:2
       (sample_tx ~nonce:10 Transaction.Standard))

let submit_signature_ok ?account_public_key tx =
  Tx_view.submit_signature_admission
    ~account_public_key
    ~preverify_has_capacity:true
    ~preverify_pending:0
    ~preverify_pending_max:2
    ~bft_mode:false
    ~confirmed_nonce:7
    ~bft_window:32
    tx

let test_submit_signature_admission () =
  let signed_tx, pub_b64 = signed_sample_tx () in
  expect_payload_ok "submit signature valid"
    (submit_signature_ok signed_tx);
  expect_payload_ok "submit signature account pubkey"
    (submit_signature_ok ~account_public_key:pub_b64 signed_tx);
  expect_payload_error "submit signature before fhe payload"
    ("invalid_signature", "no public key available")
    (submit_signature_ok (sample_tx Transaction.RecryptOp));
  expect_payload_error "submit signature before proof parsing"
    ("invalid_signature", "no public key available")
    (submit_signature_ok (sample_tx ~encrypted_data:"{}" Transaction.EncryptOp));
  let proof_limit = Octra_core.Pvac_verify_policy.max_proof_encoded_bytes in
  if Octra_core.Pvac_verify_policy.proof_allowed (String.make (proof_limit + 1) 'a') then
    fail "proof size limit missing"

let () =
  test_masking ();
  test_tx_fields ();
  test_staging_error ();
  test_status_json ();
  test_submit_rpc_helpers ();
  test_transaction_lookup_response ();
  test_staging_json ();
  test_staging_submit_effects ();
  test_route_admission ();
  test_admission_semantics ();
  test_staging_remove_auth ();
  test_pubkey_registration_auth ();
  test_encrypted_balance_auth ();
  test_standard_fields ();
  test_encrypted_payload_admission ();
  test_call_params_admission ();
  test_payload_admission ();
  test_deep_message ();
  test_pre_route_admission ();
  test_bft_op_admission ();
  test_signature_admission ();
  test_sender_admission ();
  test_submit_pre_signature ();
  test_stealth_delta_layer_limit ();
  test_preverify_stealth_payload ();
  test_preverify_saturation ();
  test_preverify_submit_result ();
  test_preverify_tx_launch ();
  test_preverify_cache_plans ();
  test_post_signature_admission ();
  test_submit_signature_admission ();
  print_endline "node_runtime_tx_view tests passed"