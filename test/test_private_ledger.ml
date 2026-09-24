(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Octra_core.Private_ledger
module T = Octra_core.Transaction
module FB = Octra_core.Crypto.FheBalance
module SA = Octra_core.Crypto.StealthAddress
module W = Octra_core.Preverify_worker

let fail msg =
  failwith ("test_private_ledger: " ^ msg)

let ok msg cond =
  if not cond then fail msg

let run_responsive name task =
  let open Lwt.Syntax in
  let started = Unix.gettimeofday () in
  let probe =
    let* () = Lwt_unix.sleep 0.01 in
    Lwt.return (Unix.gettimeofday () -. started)
  in
  let result, delay = Lwt_main.run (Lwt.both task probe) in
  ok name (delay < 0.5);
  result

let tx ?encrypted_data ?(op=T.EncryptOp) ?(amount=Z.of_int 1) ?(ou=Z.of_int 3_000) () =
  T.{
    from = "octFrom";
    to_ = "octFrom";
    amount;
    nonce = 1;
    ou;
    timestamp = 1.0;
    signature = "sig";
    public_key = Some "pub";
    message = None;
    op_type = op;
    encrypted_data;
  }

let ledger name =
  if not (Sys.file_exists "runtime_data") then Unix.mkdir "runtime_data" 0o755;
  let path =
    Filename.concat
      "runtime_data"
      ("test_private_ledger_" ^ name ^ "_" ^ string_of_int (Unix.getpid ()))
  in
  let store = Lwt_main.run (Octra_core.Store_irmin.open_store path) in
  Octra_core.Ledger.create store

let seed ch =
  Bytes.make 32 ch

let pvac ledger balance =
  let params = Pvac_ffi.default_params () in
  let pk, sk = Pvac_ffi.keygen_from_seed params (seed '\001') in
  let pk_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey pk) in
  (match Octra_core.Ledger.add_account ledger "octFrom" balance with
  | Ok () -> ()
  | Error e -> fail e);
  Lwt_main.run (Octra_core.Ledger.set_pvac_pubkey ledger "octFrom" pk_blob);
  pk, sk

let b64 bytes =
  Base64.encode_exn (Bytes.to_string bytes)

let hex_string raw =
  let buf = Buffer.create (String.length raw * 2) in
  String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) raw;
  Buffer.contents buf

let payload pk sk amount seed_ch =
  let amount_i64 = Z.to_int64 amount in
  let blind = seed seed_ch in
  let ct = Pvac_ffi.enc_value_seeded pk sk amount_i64 (seed (Char.chr (Char.code seed_ch + 1))) in
  let cipher = FB.encode_cipher ct in
  let commitment = b64 (Pvac_ffi.pedersen_commit_amount amount_i64 blind) in
  let proof = FB.encode_zero_proof (Pvac_ffi.make_zero_proof_bound pk sk ct amount_i64 blind) in
  cipher, commitment, proof, b64 blind

let payload_json ?range_proof_balance (cipher, amount_commitment, zero_proof, blinding) =
  let fields = [
    "cipher", `String cipher;
    "amount_commitment", `String amount_commitment;
    "zero_proof", `String zero_proof;
    "blinding", `String blinding;
  ] in
  let fields = match range_proof_balance with
    | None -> fields
    | Some proof -> ("range_proof_balance", `String proof) :: fields
  in
  Some (Yojson.Safe.to_string (`Assoc fields))

let key_switch_json pk_blob =
  Some (Yojson.Safe.to_string (`Assoc [
    "new_pubkey", `String (Base64.encode_exn pk_blob);
    "aes_kat", `String (Octra_core.Pvac_registry.expected_kat ());
  ]))

let claim_secret =
  String.make 32 '\077'

let claim_pub =
  hex_string (SA.compute_claim_pub claim_secret "octFrom")

let claim_json output_id =
  Some (Yojson.Safe.to_string (`Assoc [
    "version", `Int 5;
    "output_id", `Int output_id;
    "claim_cipher", `String "hfhe_v1|legacy";
    "commitment", `String "legacy";
    "claim_secret", `String (hex_string claim_secret);
    "zero_proof", `String "zkzp_v2|legacy";
  ]))

let claim_output ledger amount amount_commitment amount_hash =
  Lwt_main.run (Octra_core.Ledger.create_stealth_output ledger
    ~stealth_tag:"00112233445566778899aabbccddeeff"
    ~eph_pub:"test_eph"
    ~enc_amount:"test_enc"
    ~amount
    ~epoch_id:1
    ~tx_hash:"claim_test_hash"
    ~sender_addr:"octFrom"
    ~claim_pub
    ~amount_hash
    ~amount_commitment
    ())
  |> function
  | Ok id -> Int64.to_int id
  | Error e -> fail e

let expect_error tag user_reason result =
  match Lwt_main.run result with
  | Ok _ -> fail ("expected error " ^ tag)
  | Error e ->
    ok ("tag " ^ tag) (String.equal e.P.tag tag);
    ok ("user reason " ^ user_reason) (String.equal e.user_reason user_reason)

let test_encrypt_missing () =
  P.encrypt_plan ~strict:true ~field_policy:P.Unique_fields (ledger "encrypt_missing") (tx ())
  |> expect_error "malformed_transaction" "encrypt: malformed encrypted_data"

let test_decrypt_invalid_amount () =
  P.decrypt_plan ~strict:true
    ~field_policy:P.Unique_fields
    (ledger "decrypt_invalid_amount")
    (tx ~op:T.DecryptOp ~encrypted_data:"{}" ~amount:Z.zero ())
  |> expect_error "invalid_amount" "decrypt amount must be positive"

let test_key_switch_bad_json () =
  P.key_switch_plan ~strict:true
    ~field_policy:P.Unique_fields
    (ledger "key_switch_bad_json")
    (tx ~op:T.KeySwitch ~encrypted_data:"{" ())
  |> expect_error "key_switch_rejected" "encrypted_data must be JSON with new_pubkey and aes_kat"

let test_private_version_wrong_type () =
  let payload = Yojson.Safe.to_string (`Assoc ["version", `Bool true]) in
  List.iter
    (fun field_policy ->
      let suffix =
        match field_policy with
        | P.First_field -> "first"
        | P.Unique_fields -> "unique"
      in
      P.stealth_plan
        ~field_policy
        (ledger ("stealth_wrong_version_type_" ^ suffix))
        (tx ~op:T.StealthOp ~encrypted_data:payload ())
      |> expect_error
           "version_rejected"
           "only version 5 stealth transfers are accepted";
      P.claim_plan ~strict:true
        ~field_policy
        (ledger ("claim_wrong_version_type_" ^ suffix))
        (tx ~op:T.ClaimOp ~encrypted_data:payload ())
      |> expect_error
           "version_rejected"
           "only version 5 (private) claim operations are accepted")
    [P.First_field; P.Unique_fields]

let test_balance_payload_field_policy () =
  let payload =
    Yojson.Safe.to_string
      (`Assoc [
        "cipher", `String "cipher";
        "cipher", `Bool true;
        "amount_commitment", `String "commitment";
        "zero_proof", `String "proof";
        "blinding", `String "blinding";
      ])
  in
  let store = ledger "balance_payload_policy" in
  let first_encrypt =
    Lwt_main.run
      (P.encrypt_plan ~strict:true
         ~field_policy:P.First_field
         store
         (tx ~encrypted_data:payload ()))
  in
  let unique_encrypt =
    Lwt_main.run
      (P.encrypt_plan ~strict:true
         ~field_policy:P.Unique_fields
         store
         (tx ~encrypted_data:payload ()))
  in
  let first_decrypt =
    Lwt_main.run
      (P.decrypt_plan ~strict:true
         ~field_policy:P.First_field
         store
         (tx ~op:T.DecryptOp ~encrypted_data:payload ()))
  in
  let unique_decrypt =
    Lwt_main.run
      (P.decrypt_plan ~strict:true
         ~field_policy:P.Unique_fields
         store
         (tx ~op:T.DecryptOp ~encrypted_data:payload ()))
  in
  begin
    match first_encrypt, unique_encrypt, first_decrypt, unique_decrypt with
    | Error first_encrypt,
      Error unique_encrypt,
      Error first_decrypt,
      Error unique_decrypt ->
      ok "prior encrypt retained first field"
        (not (String.equal first_encrypt.P.tag "malformed_transaction"));
      ok "active encrypt rejected duplicate"
        (String.equal unique_encrypt.P.tag "malformed_transaction");
      ok "prior decrypt retained first field"
        (not (String.equal first_decrypt.P.tag "malformed_transaction"));
      ok "active decrypt rejected duplicate"
        (String.equal unique_decrypt.P.tag "malformed_transaction")
    | _ -> fail "balance payload policy was not deterministic"
  end

let test_key_switch_field_policy () =
  let migration_payload =
    Yojson.Safe.to_string
      (`Assoc [
        "new_pubkey", `String "key";
        "aes_kat", `String "kat";
        "migration_mode", `String "historical_owner_proof";
        "migration_mode", `String "standard";
      ])
  in
  let migration_tx =
    tx ~op:T.KeySwitch ~encrypted_data:migration_payload ()
  in
  ok "first field migration retained"
    (P.key_switch_requests_historical_owner_proof
       ~field_policy:P.First_field
       migration_tx);
  ok "duplicate migration rejected"
    (not
       (P.key_switch_requests_historical_owner_proof
          ~field_policy:P.Unique_fields
          migration_tx));
  let reset_payload =
    Yojson.Safe.to_string
      (`Assoc [
        "new_pubkey", `String "key";
        "aes_kat", `String "kat";
        "legacy_zero_reset", `Bool true;
        "legacy_zero_reset", `Bool false;
      ])
  in
  let reset_tx = tx ~op:T.KeySwitch ~encrypted_data:reset_payload () in
  let store = ledger "key_switch_duplicate_policy" in
  let prior =
    Lwt_main.run
      (P.key_switch_plan ~strict:false ~field_policy:P.First_field store reset_tx)
  in
  let active =
    Lwt_main.run
      (P.key_switch_plan ~strict:true ~field_policy:P.Unique_fields store reset_tx)
  in
  begin
    match prior, active with
    | Error prior, Error active ->
      ok "prior decoder retained first field"
        (not (String.equal prior.P.reason "duplicate legacy_zero_reset"));
      ok "active decoder rejected duplicate"
        (String.equal active.P.reason "duplicate legacy_zero_reset")
    | _ -> fail "duplicate key switch policy did not reject deterministically"
  end

let test_key_switch_wrong_types_are_values () =
  let store = ledger "key_switch_wrong_types" in
  let base = [
    "new_pubkey", `String "key";
    "aes_kat", `String "kat";
  ] in
  let string_fields = [
    "new_pubkey";
    "aes_kat";
    "old_bound_pubkey";
    "old_bound_cipher";
    "source_cipher_hash";
    "new_cipher";
    "old_zero_proof";
    "new_zero_proof";
    "amount_commitment";
    "amount_blinding";
    "migration_mode";
  ] in
  let replace field value =
    if List.exists (fun (name, _) -> String.equal name field) base then
      List.map
        (fun (name, current) ->
          if String.equal name field then name, value else name, current)
        base
    else
      (field, value) :: base
  in
  let reject policy fields =
    let payload = Yojson.Safe.to_string (`Assoc fields) in
    P.key_switch_plan ~strict:true
      ~field_policy:policy
      store
      (tx ~op:T.KeySwitch ~encrypted_data:payload ())
    |> expect_error
         "key_switch_rejected"
         "encrypted_data must be JSON with new_pubkey and aes_kat"
  in
  List.iter
    (fun field ->
      reject P.First_field (replace field (`Bool true));
      reject P.Unique_fields (replace field (`Bool true)))
    string_fields;
  List.iter
    (fun field ->
      reject P.First_field ((field, `String "true") :: base);
      reject P.Unique_fields ((field, `String "true") :: base))
    [
      "legacy_ct_migration";
      "legacy_public_migration";
      "legacy_commitment_migration";
      "legacy_zero_reset";
    ];
  reject P.First_field [];
  reject P.Unique_fields [];
  List.iter
    (fun policy ->
      P.key_switch_plan ~strict:true
        ~field_policy:policy
        store
        (tx ~op:T.KeySwitch ~encrypted_data:"[]" ())
      |> expect_error
           "key_switch_rejected"
           "encrypted_data must be JSON with new_pubkey and aes_kat")
    [P.First_field; P.Unique_fields]

let test_key_switch_artifact_binds_field_policy () =
  let store = ledger "key_switch_artifact_policy" in
  let _, _ = pvac store (Z.of_int 100) in
  let params = Pvac_ffi.default_params () in
  let new_pk, _ = Pvac_ffi.keygen_from_seed params (seed '\021') in
  let new_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey new_pk) in
  let encrypted_data = key_switch_json new_blob in
  let operation =
    tx ~op:T.KeySwitch ?encrypted_data ~amount:Z.zero ~ou:Z.one ()
  in
  let artifact =
    match
      Lwt_main.run
        (P.preverify_key_switch_artifact ~strict:true
           ~field_policy:P.First_field
           store
           operation)
    with
    | Ok artifact -> artifact
    | Error failure -> fail failure.P.reason
  in
  match
    Lwt_main.run
      (P.bind_key_switch_artifact ~strict:true
         ~field_policy:P.Unique_fields
         store
         operation
         artifact)
  with
  | P.Key_switch_source_changed -> ()
  | _ -> fail "key switch artifact crossed field policy"

let test_key_cache () =
  let state = ledger "key_cache" in
  let pk, _ = pvac state (Z.of_int 100) in
  Lwt_main.run (Octra_core.Ledger.flush_dirty_lwt state);
  let blob = Bytes.to_string (Pvac_ffi.serialize_pubkey pk) in
  let operation =
    tx ~op:T.KeySwitch ?encrypted_data:(key_switch_json blob) ()
  in
  let check ?(strict = true) ?(fields = P.Unique_fields) operation =
    Lwt_main.run (P.key_switch_plan ~field_policy:fields ~strict state operation)
  in
  let plan = function
    | Ok value -> value
    | Error error -> fail error.P.reason
  in
  let digest value = P.hash_prepared (P.Prepared_key_switch value) in
  let first = plan (check operation) in
  let current value =
    Lwt_main.run
      (P.prepared_current ~field_policy:P.Unique_fields state operation
         (P.Prepared_key_switch value))
  in
  ok "plan keeps complete key" (first.old_pubkey = Some blob);
  ok "prepared key matches" (current first);
  ok "missing prepared key differs" (not (current { first with old_pubkey = None }));
  ok "key metadata preserves receipt"
    (String.equal (digest first) (digest { first with old_pubkey = None }));
  let prepared = plan (Lwt_main.run
    (P.prepare_key_switch_plan ~field_policy:P.First_field state operation)) in
  ok "prepared key captured" (prepared.old_pubkey = first.old_pubkey);
  ok "prepared receipt preserved" (String.equal (digest first) (digest prepared));
  let again = plan (check operation) in
  ok "checked plan reused" (first == again);
  let prior = plan (check ~strict:false operation) in
  let prior_again = plan (check ~strict:false operation) in
  ok "prior still verifies" (prior != prior_again);
  ok "plan mode parity" (String.equal (digest first) (digest prior));
  let fields = plan (check ~fields:P.First_field operation) in
  ok "field policy isolated" (first != fields);
  let changed = plan (check { operation with nonce = 2 }) in
  ok "transaction isolated" (first != changed);
  let root = Lwt_main.run (Octra_core.Ledger.hash state) in
  begin
    match Octra_core.Ledger.update_enc_balance state "octFrom" "invalid" with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  ok "dirty account leaves store root unchanged"
    (String.equal root (Lwt_main.run (Octra_core.Ledger.hash state)));
  begin
    match check operation with
    | Error _ -> ()
    | Ok _ -> fail "plan reused after cipher change"
  end;
  begin
    match Octra_core.Ledger.update_enc_balance state "octFrom" "0" with
    | Ok () -> ()
    | Error reason -> fail reason
  end;
  let restored = plan (check operation) in
  ok "restored source parity" (String.equal (digest first) (digest restored));
  let changed_pk, _ =
    Pvac_ffi.keygen_from_seed (Pvac_ffi.default_params ()) (seed '\042')
  in
  let changed_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey changed_pk) in
  Lwt_main.run (Octra_core.Store_irmin.begin_epoch_batch state.store);
  Lwt_main.run (Octra_core.Ledger.set_pvac_pubkey state "octFrom" changed_blob);
  ok "batch leaves store root unchanged"
    (String.equal root (Lwt_main.run (Octra_core.Ledger.hash state)));
  let changed_key = plan (check operation) in
  ok "changed key differs" (not (current first));
  ok "short key alias differs"
    (not (current { first with old_key_hash = changed_key.old_key_hash }));
  ok "new prepared key matches" (current changed_key);
  ok "batch key change checked" (restored != changed_key);
  ok "batch key plan follows source"
    (String.equal changed_key.P.old_key_hash
       (Octra_core.Pvac_registry.key_hash changed_blob));
  Octra_core.Store_irmin.abort_epoch_batch state.store;
  let rolled = plan (check operation) in
  ok "restored key matches" (current first && not (current changed_key));
  ok "rollback plan follows source"
    (String.equal (digest first) (digest rolled));
  let artifact =
    match
      Lwt_main.run
        (P.preverify_key_switch_artifact ~field_policy:P.Unique_fields
           ~strict:true state operation)
    with
    | Ok value -> value
    | Error error -> fail error.P.reason
  in
  let verified =
    match
      Lwt_main.run
        (P.bind_key_switch_artifact ~field_policy:P.Unique_fields
           ~strict:true state operation artifact)
    with
    | P.Key_switch_bound (P.Prepared_key_switch value) -> value
    | _ -> fail "local plan not retained"
  in
  ok "binding preserves cache" (rolled == plan (check operation));
  ok "binding preserves result" (String.equal (digest verified) (digest rolled));
  List.iter
    (fun nonce -> ignore (plan (check { operation with nonce })))
    (List.init 33 (fun offset -> offset + 3));
  let after = plan (check operation) in
  ok "eviction causes new check" (verified != after);
  ok "eviction preserves result" (String.equal (digest verified) (digest after))

let test_stealth_missing () =
  P.stealth_plan
    ~field_policy:P.Unique_fields
    (ledger "stealth_missing")
    (tx ~op:T.StealthOp ())
  |> expect_error "missing_encrypted_data" "encrypted_data required for stealth transfer"

let test_claim_not_self () =
  let tx = {
    (tx ~op:T.ClaimOp ~encrypted_data:"{}" ()) with
    T.to_ = "octOther";
  } in
  P.claim_plan ~strict:true ~field_policy:P.Unique_fields (ledger "claim_not_self") tx
  |> expect_error "claim_not_self" "claim operation must target sender (from == to)"

let test_claim_missing () =
  P.claim_plan ~strict:true
    ~field_policy:P.Unique_fields
    (ledger "claim_missing")
    (tx ~op:T.ClaimOp ())
  |> expect_error "missing_encrypted_data" "encrypted_data required for claim"

let test_apply_encrypt () =
  let ledger = ledger "apply_encrypt" in
  let pk, sk = pvac ledger (Z.of_int 1000) in
  let encrypted_data = payload_json (payload pk sk (Z.of_int 10) '\003') in
  match run_responsive "encrypt proof blocked event loop"
    (P.apply_encrypt ~strict:true
       ~field_policy:P.Unique_fields
       ledger
       (tx ?encrypted_data ~amount:(Z.of_int 10) ~ou:Z.one ())) with
  | Error e -> fail ("apply_encrypt rejected: " ^ e.P.reason)
  | Ok plan ->
    ok "encrypt changed cipher" (not (String.equal plan.current_cipher plan.next_cipher));
    let acc = Octra_core.Ledger.find ledger "octFrom" in
    ok "encrypt public balance" (Z.equal acc.balance (Z.of_int 989));
    ok "encrypt nonce" (acc.nonce = 1);
    match acc.encrypted_balance with
    | None -> fail "encrypt missing encrypted balance"
    | Some cipher ->
      match FB.get_balance pk sk cipher with
      | Error e -> fail e
      | Ok balance -> ok "encrypt encrypted balance" (Z.equal balance (Z.of_int 10))

let test_apply_decrypt () =
  let ledger = ledger "apply_decrypt" in
  let pk, sk = pvac ledger (Z.of_int 1000) in
  let current_ct = Pvac_ffi.enc_value_seeded pk sk 20L (seed '\011') in
  let delta_payload = payload pk sk (Z.of_int 5) '\013' in
  let delta_ct = match FB.decode_cipher (let c, _, _, _ = delta_payload in c) with
    | Ok ct -> ct
    | Error e -> fail e
  in
  let next_ct = Pvac_ffi.ct_sub pk current_ct delta_ct in
  let next_range =
    FB.encode_bound_range_proof
      (Pvac_ffi.make_zero_proof_bound_range pk sk next_ct 15L (seed '\014'))
  in
  ignore (Octra_core.Ledger.update_enc_balance ledger "octFrom" (FB.encode_cipher current_ct));
  let encrypted_data = payload_json ~range_proof_balance:next_range delta_payload in
  match run_responsive "decrypt proof blocked event loop"
    (P.apply_decrypt ~strict:true
       ~field_policy:P.Unique_fields
       ledger
       (tx ~op:T.DecryptOp ?encrypted_data ~amount:(Z.of_int 5) ~ou:Z.one ())) with
  | Error e -> fail ("apply_decrypt rejected: " ^ e.P.reason)
  | Ok plan ->
    ok "decrypt changed cipher" (not (String.equal plan.current_cipher plan.next_cipher));
    let acc = Octra_core.Ledger.find ledger "octFrom" in
    ok "decrypt public balance" (Z.equal acc.balance (Z.of_int 1004));
    ok "decrypt nonce" (acc.nonce = 1);
    match acc.encrypted_balance with
    | None -> fail "decrypt missing encrypted balance"
    | Some cipher ->
      match FB.get_balance pk sk cipher with
      | Error e -> fail e
      | Ok balance -> ok "decrypt encrypted balance" (Z.equal balance (Z.of_int 15))

let six_layer_cipher pk sk =
  let first = Pvac_ffi.enc_value_seeded pk sk 1L (seed '\031') in
  let second = Pvac_ffi.enc_value_seeded pk sk 2L (seed '\032') in
  let third = Pvac_ffi.enc_value_seeded pk sk 3L (seed '\033') in
  Pvac_ffi.ct_add pk (Pvac_ffi.ct_add pk first second) third

let test_private_ops_require_refresh () =
  let ledger = ledger "private_refresh" in
  let pk, sk = pvac ledger (Z.of_int 1000) in
  let six = six_layer_cipher pk sk |> FB.encode_cipher in
  ok "six base layers allowed" (FB.check_private_input six = Ok ());
  let fourth = Pvac_ffi.enc_value_seeded pk sk 4L (seed '\034') in
  let current =
    match FB.decode_cipher six with
    | Error e -> fail e
    | Ok cipher -> Pvac_ffi.ct_add pk cipher fourth |> FB.encode_cipher
  in
  ignore (Octra_core.Ledger.update_enc_balance ledger "octFrom" current);
  let encrypted_data =
    payload_json ("hfhe_v1|bad", "bad", "bad", "bad")
  in
  P.encrypt_plan ~strict:true
    ~field_policy:P.Unique_fields
    ledger
    (tx ?encrypted_data ~amount:Z.one ())
  |> expect_error
      "encrypt_balance_failed"
      "encrypt: encrypted balance compact refresh required (8 base layers)";
  P.decrypt_plan ~strict:true ~field_policy:P.Unique_fields ledger
    (tx ~op:T.DecryptOp ?encrypted_data ~amount:Z.one ())
  |> expect_error
      "decrypt_cipher_failed"
      "decrypt: encrypted balance compact refresh required (8 base layers)";
  let expect_preverify op =
    match
      Lwt_main.run
        (W.run ~strict:true
           ~field_policy:P.Unique_fields
           ~ledger
           (tx ~op ?encrypted_data ~amount:Z.one ()))
    with
    | W.Skip reason ->
      ok "preverify requires refresh"
        (String.equal reason
          "encrypted balance compact refresh required (8 base layers)")
    | W.Defer reason -> fail ("preverify deferred layered balance " ^ reason)
    | W.Ready _ -> fail "preverify accepted layered encrypted balance"
  in
  expect_preverify T.EncryptOp;
  expect_preverify T.DecryptOp

let test_claim_rejects_unmarked_output () =
  let ledger = ledger "claim_unmarked" in
  let _, _ = pvac ledger (Z.of_int 1000) in
  let amount_commitment = Base64.encode_exn (String.make 32 '\001') in
  let output_id = claim_output ledger (Z.of_int 7) amount_commitment "" in
  let encrypted_data = claim_json output_id in
  P.claim_plan ~strict:true
    ~field_policy:P.Unique_fields
    ledger
    (tx ~op:T.ClaimOp ?encrypted_data ())
  |> expect_error "legacy_stealth_output" "stealth output predates key-bound PVAC send verification; legacy output migration is required";
  match
    Lwt_main.run
      (W.run ~strict:true
         ~field_policy:P.Unique_fields
         ~ledger
         (tx ~op:T.ClaimOp ?encrypted_data ()))
  with
  | W.Skip reason -> ok "preverify legacy output" (String.equal reason "legacy_stealth_output")
  | W.Defer reason -> fail ("preverify deferred legacy output " ^ reason)
  | W.Ready _ -> fail "preverify accepted legacy output"

let test_apply_key_switch_debit_failure () =
  let ledger = ledger "key_switch_debit_failure" in
  match
    Lwt_main.run
      (P.apply_key_switch ~strict:true
         ~field_policy:P.Unique_fields
         ledger
         (tx ~op:T.KeySwitch ~encrypted_data:"{}" ~amount:Z.zero ~ou:Z.one ()))
  with
  | P.Key_switch_rejected r ->
    ok "key switch debit no consume" (not r.consume_nonce);
    ok "key switch debit tag" (r.failure.P.tag = "key_switch_rejected")
  | P.Key_switch_applied _ -> fail "key switch debit failure accepted"

let test_apply_key_switch_bad_payload_preserves () =
  let ledger = ledger "key_switch_bad_payload_apply" in
  (match Octra_core.Ledger.add_account ledger "octFrom" (Z.of_int 100) with
  | Ok () -> ()
  | Error e -> fail e);
  match
    Lwt_main.run
      (P.apply_key_switch ~strict:true
         ~field_policy:P.Unique_fields
         ledger
         (tx ~op:T.KeySwitch ~encrypted_data:"{" ~amount:Z.zero ~ou:Z.one ()))
  with
  | P.Key_switch_rejected r ->
    let acc = Octra_core.Ledger.find ledger "octFrom" in
    ok "key switch bad no consume" (not r.consume_nonce);
    ok "key switch bad balance" (Z.equal acc.balance (Z.of_int 100));
    ok "key switch bad nonce" (acc.nonce = 0)
  | P.Key_switch_applied _ -> fail "key switch bad payload accepted"

let test_apply_key_switch_locked_balance () =
  let ledger = ledger "key_switch_locked_balance" in
  let pk, _ = pvac ledger (Z.of_int 100) in
  ignore (Octra_core.Ledger.update_enc_balance ledger "octFrom" "hfhe_v1|old");
  let params = Pvac_ffi.default_params () in
  let new_pk, _ = Pvac_ffi.keygen_from_seed params (seed '\021') in
  let new_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey new_pk) in
  let encrypted_data = key_switch_json new_blob in
  match
    Lwt_main.run
      (P.apply_key_switch ~strict:true
         ~field_policy:P.Unique_fields
         ledger
         (tx ~op:T.KeySwitch ?encrypted_data ~amount:Z.zero ~ou:Z.one ()))
  with
  | P.Key_switch_rejected r ->
    ok "key switch locked no consume" (not r.consume_nonce);
    ok "key switch locked tag" (String.equal r.failure.P.tag "key_switch_rejected");
    ok "key switch locked reason"
      (String.length r.failure.P.reason > 0);
    let acc = Octra_core.Ledger.find ledger "octFrom" in
    ok "key switch locked balance" (Z.equal acc.balance (Z.of_int 100));
    ok "key switch locked preserves encrypted" (acc.encrypted_balance = Some "hfhe_v1|old");
    ignore pk
  | P.Key_switch_applied _ -> fail "key switch with encrypted balance accepted"

let test_apply_key_switch_success () =
  let ledger = ledger "key_switch_success" in
  let pk, _ = pvac ledger (Z.of_int 100) in
  let params = Pvac_ffi.default_params () in
  let new_pk, _ = Pvac_ffi.keygen_from_seed params (seed '\021') in
  let new_blob = Bytes.to_string (Pvac_ffi.serialize_pubkey new_pk) in
  let encrypted_data = key_switch_json new_blob in
  match
    Lwt_main.run
      (P.apply_key_switch ~strict:true
         ~field_policy:P.Unique_fields
         ledger
         (tx ~op:T.KeySwitch ?encrypted_data ~amount:Z.zero ~ou:Z.one ()))
  with
  | P.Key_switch_rejected r -> fail ("key switch success rejected: " ^ r.failure.P.reason)
  | P.Key_switch_applied r ->
    ok "key switch old hash" (String.length r.old_key_hash = 16);
    ok "key switch new hash" (String.length r.new_key_hash = 16);
    let acc = Octra_core.Ledger.find ledger "octFrom" in
    ok "key switch balance" (Z.equal acc.balance (Z.of_int 99));
    ok "key switch nonce" (acc.nonce = 1);
    ok "key switch leaves empty encrypted state" (acc.encrypted_balance = None);
    ok "key switch kat" (Octra_core.Ledger.get_pvac_kat ledger "octFrom" = Some (Octra_core.Pvac_registry.expected_kat ()));
    match Lwt_main.run (Octra_core.Ledger.get_pvac_pubkey ledger "octFrom") with
    | None -> fail "key switch missing new key"
    | Some stored ->
      match Octra_core.Pvac_registry.canonicalize_blob new_blob with
      | Error e -> fail e
      | Ok canonical ->
        ok "key switch stored new key" (String.equal stored canonical);
        ignore pk

let test_json_tree () =
  let store = ledger "json_tree" in
  Fun.protect ~finally:(fun () ->
    Lwt_main.run (Octra_core.Store_irmin.close store.Octra_core.Ledger.store))
    (fun () ->
      let deep = String.make 250_000 '[' ^ "0" ^ String.make 250_000 ']' in
      let payload padding = "{\"cipher\":false,\"padding\":" ^ padding ^ "}" in
      List.iter (fun field_policy ->
        let encrypt raw = Lwt_main.run (P.encrypt_plan ~strict:true ~field_policy
          store (tx ~encrypted_data:raw ())) in
        let key raw = Lwt_main.run (P.key_switch_plan ~strict:true ~field_policy
          store (tx ~op:T.KeySwitch ~encrypted_data:raw ())) in
        let same name left right = match left, right with
          | Error a, Error b ->
            ok name (a.P.tag = b.P.tag && a.reason = b.reason && a.user_reason = b.user_reason)
          | _ -> fail (name ^ " accepted invalid fields") in
        same "encrypt ignores padding" (encrypt (payload "0")) (encrypt (payload deep));
        same "key switch ignores padding" (key (payload "0")) (key (payload deep));
        same "encrypt malformed input" (encrypt "{") (encrypt ("[" ^ deep));
        same "key switch malformed input" (key "{") (key ("[" ^ deep)))
        [P.First_field; P.Unique_fields])

let () =
  test_json_tree ();
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "--json" then
    print_endline "status = pass test = private_json"
  else begin
  test_encrypt_missing ();
  test_decrypt_invalid_amount ();
  test_key_switch_bad_json ();
  test_private_version_wrong_type ();
  test_balance_payload_field_policy ();
  test_key_switch_field_policy ();
  test_key_switch_wrong_types_are_values ();
  test_key_switch_artifact_binds_field_policy ();
  test_key_cache ();
  test_stealth_missing ();
  test_claim_not_self ();
  test_claim_missing ();
  test_apply_encrypt ();
  test_apply_decrypt ();
  test_private_ops_require_refresh ();
  test_claim_rejects_unmarked_output ();
  test_apply_key_switch_debit_failure ();
  test_apply_key_switch_bad_payload_preserves ();
  test_apply_key_switch_locked_balance ();
  test_apply_key_switch_success ();
  print_endline "test_private_ledger: ok"
  end