(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module P = Pvac_ffi
module C = Octra_core
module F = C.Crypto.FheBalance
module K = C.Pvac_registry
module L = C.Ledger
module S = C.Store_irmin
module T = C.Transaction
module PL = C.Private_ledger
module W = C.Pvac_verify_worker
module V = C.Pvac_verify_protocol
module D = C.Pvac_verify_direct

let run = Lwt_main.run
let check reason value = if not value then failwith reason
let b64 = Base64.encode_exn
let seed value = Bytes.make 32 (Char.chr value)
let raw value = Bytes.to_string value
let json fields = Yojson.Safe.to_string (`Assoc fields)
let key value = P.serialize_pubkey value |> raw
let fail error = failwith error.PL.reason
let get = function Ok value -> value | Error error -> fail error

let signing = seed 91 |> raw
let public_key =
  let value = Mirage_crypto_ec.Ed25519.priv_of_octets signing |> Result.get_ok in
  Mirage_crypto_ec.Ed25519.pub_of_priv value
  |> Mirage_crypto_ec.Ed25519.pub_to_octets |> b64
let address = C.Crypto.Address.address_from_pubkey public_key

let tx ?(nonce = 1) ?(amount = Z.zero) op_type fields =
  T.sign_with_privkey T.{from = address; to_ = address; amount; nonce;
    ou = Z.one; timestamp = 1.; signature = ""; public_key = Some public_key;
    message = None; op_type; encrypted_data = Some (json fields)} (b64 signing)

let read path =
  let input = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in input) (fun () ->
    let values = Array.init 14 (fun _ -> really_input_string input (input_binary_int input)) in
    check "extra key bytes" (pos_in input = in_channel_length input);
    values)

let with_ledger name f =
  Test_workspace.with_dir name (fun path ->
    let store = run (S.open_store (Filename.concat path "store")) in
    Fun.protect ~finally:(fun () -> run (S.close store)) (fun () ->
      let ledger = L.create store in
      L.add_account_with_pubkey ledger address (Z.of_int 1000) public_key |> Result.get_ok;
      f ledger))

let state ledger =
  let account = L.find ledger address in
  account.balance, account.nonce, account.encrypted_balance,
  run (L.get_pvac_pubkey ledger address), L.get_pvac_kat ledger address

let fields blob = ["new_pubkey", `String (b64 blob);
  "aes_kat", `String (K.expected_kat ())]

let owner_fields pk sk =
  let blind = K.owner_blinding address in
  let cipher = P.enc_value_seeded pk sk 0L (seed 109) in
  let proof =
    P.make_zero_proof_bound ~keyed:true ~math:true pk sk cipher 0L blind
    |> F.encode_zero_proof in
  ["owner_cipher", `String (F.encode_cipher cipher);
   "owner_proof", `String proof]

let refused_series = function
  | Error error -> check "wrong series refusal"
      (error.PL.reason = "pvac public key powers are not cyclic")
  | Ok _ -> failwith "invalid key series accepted"

let owner_checks samples =
  with_ledger "owner_checks" (fun ledger ->
    let module PW = C.Preverify_worker in
    let module PC = C.Preverify_commit in
    let module PT = C.Private_transition in
    let module E = C.Epoch_exec in
    let operation = tx T.KeySwitch (fields samples.(0)) in
    let before = state ledger in
    let old = run (PL.prepare_key_switch_plan ~keyed:false ~math:true
      ~field_policy:PL.Unique_fields ledger operation) |> get in
    let prepared = run (PL.prepare_key_switch_plan ~keyed:true ~math:true
      ~field_policy:PL.Unique_fields ledger operation) in
    let root = run (L.hash ledger) |> PW.state_hash in
    let source = run (PW.source_binding ledger root operation) |> Result.get_ok in
    let receipt = match PW.receipt operation source (PL.Prepared_key_switch old) with
      | PW.Ready receipt -> receipt
      | _ -> failwith "receipt construction failed" in
    let gate = PC.create [receipt] in
    check "receipt validation failed"
      (run (PC.check_bound ledger gate [operation]) = Ok ());
    let transition = PT.create ~preverify:(Some gate)
      ~legacy_replay:(fun ~epoch:_ ~address:_ ~cipher:_ ->
        failwith "unexpected history read")
      ~ledger ~epoch_id:1 ~owner_migration_mode:C.Rule_graph.Active
      ~cipher_mode:C.Rule_graph.Prior ~proof_mode:C.Rule_graph.Prior
      ~keyed:true ~math:true ~field_policy:PL.Unique_fields
      ~result_policy:C.Private_result_policy.Recoverable
      ~limits:PT.{max_fhe = 1; max_stealth = 1} in
    let env = E.{chain_id = "octra-owner-check"; epoch_id = 1;
      proposer_addr = address; validator_addrs = []; validator_pubkeys = [];
      epoch_ts = 1.; prev_state_root = String.make 64 '0';
      ready_state_root_at = None; ready_max_lag = 0} in
    let applied = run (PT.process transition
      ~backend:(E.make_live_backend ~math:true ledger.L.store ledger) ~env operation) in
    let missing = "key ownership proof is required" in
    let prepared_ok = match prepared with
      | Error error -> error.PL.reason = missing
      | Ok _ -> false in
    let receipt_ok = applied = Error ("key_switch_rejected", missing) in
    Printf.printf "test = owner_prepare refused = %b\n%!" prepared_ok;
    Printf.printf "test = owner_receipt refused = %b\n%!" receipt_ok;
    check "owner preparation accepted missing proof" prepared_ok;
    check "receipt accepted missing owner proof" receipt_ok;
    check "owner checks changed state" (state ledger = before))

let registration samples =
  let packed = samples.(4) in
  let invalid = P.deserialize_pubkey (Bytes.of_string packed) in
  check "invalid series accepted by FFI" (not (P.pubkey_powers_ok invalid));
  List.iter (fun blob ->
    check "history key refused" (Result.is_ok (K.key_blob ~keyed:false blob));
    check "active invalid key accepted" (K.key_blob ~keyed:true blob =
      Error "pvac public key powers are not cyclic");
    with_ledger "key_series_entry" (fun ledger ->
      let operation = tx T.KeySwitch (fields blob) in
      check "registration signature" (T.verify operation public_key);
      let before = state ledger in
      let prepare keyed = run (PL.prepare_key_switch_plan ~keyed ~math:true
        ~field_policy:PL.Unique_fields ledger operation) in
      let verify keyed = run (PL.key_switch_plan ~keyed ~math:true
        ~field_policy:PL.Unique_fields ~strict:true ledger operation) in
      ignore (get (prepare false));
      ignore (get (verify false));
      let artifact = run (PL.preverify_key_switch_artifact ~keyed:false ~math:true
        ~field_policy:PL.Unique_fields ~strict:true ledger operation) |> get in
      let reused = run (PL.bind_key_switch_artifact ~keyed:true ~math:true
        ~field_policy:PL.Unique_fields ~strict:true ledger operation artifact) in
      check "prior artifact reused" (reused = PL.Key_switch_source_changed);
      refused_series (prepare true);
      refused_series (verify true);
      let applied = run (PL.apply_key_switch ~keyed:true ~math:true
        ~field_policy:PL.Unique_fields ~strict:true ledger operation) in
      check "invalid registration applied" (match applied with
        | PL.Key_switch_rejected {failure; consume_nonce = false} ->
          failure.reason = "pvac public key powers are not cyclic"
        | _ -> false);
      check "registration changed state" (state ledger = before)))
    (List.map (Array.get samples) [4; 5; 7; 8; 9; 10; 11; 12; 13]);
  List.iter (fun index ->
    check "honest key refused" (Result.is_ok (K.key_blob ~keyed:true samples.(index)))) [0; 2; 6];
  Printf.printf "status = pass test = key_series_entry\n%!"

let worker expected request =
  match run (W.classified_result ~keyed:true ~math:true ~priority:W.Required request) with
  | Ok () -> check "invalid proof accepted" expected
  | Error (W.Proof_rejected _) -> check "honest proof refused" (not expected)
  | Error _ -> failwith "proof worker failed technically"

let stored_key samples =
  let pubkey = samples.(4) in
  let pk = P.deserialize_pubkey (Bytes.of_string samples.(0)) in
  let sk = P.deserialize_seckey (Bytes.of_string samples.(1)) in
  let cipher = P.enc_value_seeded pk sk 20L (seed 93) |> F.encode_cipher in
  let point = F.decode_cipher cipher |> Result.get_ok in
  let commitment = P.commit_ct pk point |> raw |> b64 in
  let cell = V.{pubkey; cipher; ciphertext_commitment = commitment;
    proof_kind = Circle_none; proof = ""; amount_commitment = "";
    strict = true} in
  let requests = [
    V.Encrypt {pubkey; cipher; amount = Z.of_int 20; proof = "";
      commitment = ""; blinding = ""; strict = true};
    V.Claim {pubkey; cipher; proof = ""; commitment = ""; strict = true};
    V.Key_switch_claim {pubkey; cipher; proof = ""; commitment = "";
      strict = true};
    V.Range {pubkey; cipher; proof = ""; strict = true};
    V.Zero {pubkey; cipher; proof = ""};
    V.Range_bound {pubkey; cipher; proof = ""; commitment = "";
      strict = true};
    V.Circle_cell cell] in
  List.iter (fun request ->
    check "stored key accepted" (D.execute ~keyed:true ~math:true request =
      Error "pvac public key powers are not cyclic");
    check "historical key changed" (D.execute ~keyed:false ~math:true request <>
      Error "pvac public key powers are not cyclic")) requests;
  Printf.printf "status = pass test = key_series_stored\n%!"

let composition samples =
  let load index = P.deserialize_pubkey (Bytes.of_string samples.(index)),
    P.deserialize_seckey (Bytes.of_string samples.(index + 1)) in
  let old_key, old_secret = load 0 and new_key, new_secret = load 2 in
  let owner = owner_fields new_key new_secret in
  let other_owner = owner_fields old_key old_secret in
  let old_cipher = P.enc_value_seeded old_key old_secret 20L (seed 93) in
  let new_cipher = P.enc_value_seeded new_key new_secret 20L (seed 94) in
  List.iter (fun cipher -> check "expected two base layers" (P.cipher_base_layers cipher = 2))
    [old_cipher; new_cipher];
  let blind = seed 95 in
  let commitment = P.pedersen_commit_amount 20L blind |> raw |> b64 in
  let proof keyed pk sk cipher =
    P.make_zero_proof_bound ~keyed ~math:true pk sk cipher 20L blind |> F.encode_zero_proof in
  let old_proof = proof true old_key old_secret old_cipher in
  let new_proof = proof true new_key new_secret new_cipher in
  let prior_old = proof false old_key old_secret old_cipher in
  let prior_new = proof false new_key new_secret new_cipher in
  with_ledger "key_owner_cache" (fun ledger ->
    let operation = tx T.KeySwitch (fields samples.(2) @ other_owner) in
    let before = state ledger in
    ignore (get (run (PL.prepare_key_switch_plan ~keyed:true ~math:true
      ~field_policy:PL.Unique_fields ~cap:false ledger operation)));
    check "prepared owner bypassed" (match
      run (PL.apply_key_switch ~keyed:true ~math:true
        ~field_policy:PL.Unique_fields ~strict:false ledger operation) with
      | PL.Key_switch_rejected {failure; consume_nonce = false} ->
        failure.tag = "key_switch_rejected"
      | _ -> false);
    check "prepared owner changed state" (state ledger = before));
  with_ledger "key_series_flow" (fun ledger ->
    run (L.set_pvac_pubkey ledger address samples.(0));
    L.set_pvac_kat ledger address (K.expected_kat ());
    L.update_enc_balance ledger address (F.encode_cipher old_cipher) |> Result.get_ok;
    let before = state ledger in
    let migration first second = fields samples.(2) @ owner @ [
      "new_cipher", `String (F.encode_cipher new_cipher);
      "source_cipher_hash", `String Digestif.SHA256.(digest_string (F.encode_cipher old_cipher) |> to_hex);
      "old_zero_proof", `String first; "new_zero_proof", `String second;
      "amount_commitment", `String commitment] in
    let apply first second =
      let operation = tx T.KeySwitch (migration first second) in
      check "switch signature" (T.verify operation public_key);
      run (PL.apply_key_switch ~keyed:true ~math:true ~cipher_mode:C.Rule_graph.Active
        ~field_policy:PL.Unique_fields ~strict:true ledger operation) in
    let wrong_owner =
      tx T.KeySwitch (fields samples.(2) @ other_owner @ [
        "new_cipher", `String (F.encode_cipher new_cipher);
        "source_cipher_hash", `String Digestif.SHA256.(digest_string (F.encode_cipher old_cipher) |> to_hex);
        "old_zero_proof", `String old_proof;
        "new_zero_proof", `String new_proof;
        "amount_commitment", `String commitment]) in
    check "other key owner accepted" (match
      run (PL.apply_key_switch ~keyed:true ~math:true ~cipher_mode:C.Rule_graph.Active
        ~field_policy:PL.Unique_fields ~strict:true ledger wrong_owner) with
      | PL.Key_switch_rejected {failure; consume_nonce = false} ->
        failure.tag = "key_switch_rejected"
      | _ -> false);
    check "other key owner changed state" (state ledger = before);
    let foreign_owner =
      try Some (owner_fields new_key old_secret)
      with (Out_of_memory | Stack_overflow) as failure -> raise failure
         | Failure _ -> None in
    Option.iter (fun foreign ->
      let operation = tx T.KeySwitch (fields samples.(2) @ foreign) in
      check "foreign secret accepted" (match
        run (PL.apply_key_switch ~keyed:true ~math:true
          ~cipher_mode:C.Rule_graph.Active ~field_policy:PL.Unique_fields
          ~strict:true ledger operation) with
        | PL.Key_switch_rejected {failure; consume_nonce = false} ->
          failure.tag = "key_switch_rejected"
        | _ -> false);
      check "foreign secret changed state" (state ledger = before)) foreign_owner;
    let missing_owner = tx T.KeySwitch (fields samples.(2)) in
    check "missing owner accepted" (match
      run (PL.apply_key_switch ~keyed:true ~math:true ~cipher_mode:C.Rule_graph.Active
        ~field_policy:PL.Unique_fields ~strict:true ledger missing_owner) with
      | PL.Key_switch_rejected {failure; consume_nonce = false} ->
        failure.reason = "key ownership proof is required"
      | _ -> false);
    check "missing owner changed state" (state ledger = before);
    List.iter (fun (first, second) ->
      check "prior switch proof accepted" (match apply first second with
        | PL.Key_switch_rejected {failure; consume_nonce = false} ->
          failure.tag = "key_switch_rejected"
        | _ -> false);
      check "refused switch changed state" (state ledger = before))
      [prior_old, new_proof; old_proof, prior_new];
    check "honest switch refused" (match apply old_proof new_proof with
      | PL.Key_switch_applied {migrated_cipher = true; _} -> true | _ -> false);
    let stored = run (L.get_pvac_pubkey ledger address) |> Option.get in
    check "wrong registered key" (stored = key new_key);
    let account = L.find ledger address in
    check "switch debit" (account.nonce = 1 && account.balance = Z.of_int 999);
    check "switch cipher" (account.encrypted_balance = Some (F.encode_cipher new_cipher));
    check "switch value" (P.dec_value new_key new_secret new_cipher = 20L);
    let range keyed = P.make_zero_proof_bound_range ~keyed ~math:true
      new_key new_secret new_cipher 20L blind |> F.encode_bound_range_proof in
    let current_range = range true and prior_range = range false in
    List.iter (fun (pubkey, expected) ->
      worker expected (V.Claim {pubkey; cipher = F.encode_cipher new_cipher;
        proof = new_proof; commitment; strict = true});
      worker expected (V.Range_bound {pubkey; cipher = F.encode_cipher new_cipher;
        proof = current_range; commitment; strict = true}))
      [stored, true; samples.(4), false; samples.(6), false];
    worker false (V.Range_bound {pubkey = stored; cipher = F.encode_cipher new_cipher;
      proof = prior_range; commitment; strict = true});
    let delta = P.enc_value_seeded new_key new_secret 5L (seed 96) in
    let remainder = P.ct_sub new_key new_cipher delta in
    let delta_blind = seed 97 and rest_blind = seed 98 in
    let delta_proof keyed = P.make_zero_proof_bound ~keyed ~math:true
      new_key new_secret delta 5L delta_blind |> F.encode_zero_proof in
    let rest_proof keyed = P.make_zero_proof_bound_range ~keyed ~math:true
      new_key new_secret remainder 15L rest_blind |> F.encode_bound_range_proof in
    let current_delta = delta_proof true and prior_delta = delta_proof false in
    let current_rest = rest_proof true and prior_rest = rest_proof false in
    let decrypt first second =
      let fields = ["cipher", `String (F.encode_cipher delta);
        "zero_proof", `String first; "range_proof_balance", `String second;
        "amount_commitment", `String (P.pedersen_commit_amount 5L delta_blind |> raw |> b64);
        "blinding", `String (raw delta_blind |> b64)] in
      let operation = tx ~nonce:2 ~amount:(Z.of_int 5) T.DecryptOp fields in
      check "decrypt signature" (T.verify operation public_key);
      run (PL.apply_decrypt ~keyed:true ~math:true ~field_policy:PL.Unique_fields
        ~strict:true ledger operation) in
    run (L.set_pvac_pubkey ledger address samples.(4));
    let invalid_before = state ledger in
    check "stored series decrypt accepted" (match decrypt current_delta current_rest with
      | Error error -> error.PL.reason =
        "decrypt: pvac public key powers are not cyclic"
      | Ok _ -> false);
    check "stored series changed state" (state ledger = invalid_before);
    run (L.set_pvac_pubkey ledger address stored);
    let before = state ledger in
    List.iter (fun (first, second, tag) ->
      check "prior decrypt proof accepted" (match decrypt first second with
        | Error error -> error.PL.tag = tag | Ok _ -> false);
      check "refused decrypt changed state" (state ledger = before))
      [prior_delta, current_rest, "bad_zero_proof";
       current_delta, prior_rest, "bad_range_proof_balance"];
    ignore (get (decrypt current_delta current_rest));
    let account = L.find ledger address in
    check "decrypt debit" (account.nonce = 2 && account.balance = Z.of_int 1003);
    check "decrypt value" (F.get_balance new_key new_secret
      (Option.get account.encrypted_balance) = Ok (Z.of_int 15));
    check "decrypt kept key" (run (L.get_pvac_pubkey ledger address) = Some stored));
  Printf.printf "status = pass test = key_series_flow\n%!"

let stored_switch samples =
  let old_key = P.deserialize_pubkey (Bytes.of_string samples.(4)) in
  let old_secret = P.deserialize_seckey (Bytes.of_string samples.(3)) in
  let new_key = P.deserialize_pubkey (Bytes.of_string samples.(0)) in
  let new_secret = P.deserialize_seckey (Bytes.of_string samples.(1)) in
  let old_cipher = P.enc_value_seeded old_key old_secret 20L (seed 101) in
  let new_cipher = P.enc_value_seeded new_key new_secret 20L (seed 102) in
  check "stored key did not open" (P.dec_value old_key old_secret old_cipher = 20L);
  let blind = seed 103 in
  let commitment = P.pedersen_commit_amount 20L blind |> raw |> b64 in
  let old_proof keyed = P.make_zero_proof_bound ~keyed ~math:true
    old_key old_secret old_cipher 20L blind |> F.encode_zero_proof in
  let new_proof = P.make_zero_proof_bound ~keyed:true ~math:true
    new_key new_secret new_cipher 20L blind |> F.encode_zero_proof in
  let current_proof = old_proof true in
  let prior_proof = old_proof false in
  let old_blob = samples.(4) in
  let cipher = F.encode_cipher old_cipher in
  let stored proof commitment = V.Stored_key_claim {
    pubkey = old_blob; cipher; proof; commitment; strict = true} in
  worker true (stored current_proof commitment);
  worker false (stored prior_proof commitment);
  worker false (stored current_proof
    (P.pedersen_commit_amount 10_000L blind |> raw |> b64));
  worker false (V.Key_switch_claim {pubkey = old_blob; cipher;
    proof = current_proof; commitment; strict = true});
  check "prior stored claim accepted" (D.execute ~keyed:false ~math:true
    (stored current_proof commitment) =
      Error "stored key claim requires full key proof");
  with_ledger "stored_key_switch" (fun ledger ->
    run (L.set_pvac_pubkey ledger address old_blob);
    L.set_pvac_kat ledger address (K.expected_kat ());
    L.update_enc_balance ledger address cipher |> Result.get_ok;
    let before = state ledger in
    let withdraw = tx ~amount:(Z.of_int 10_000) T.DecryptOp [
      "cipher", `String cipher;
      "zero_proof", `String current_proof;
      "range_proof_balance", `String "rp_v1|AA==";
      "amount_commitment", `String commitment;
      "blinding", `String (b64 (raw blind))] in
    check "stored key decrypt accepted" (match
      run (PL.apply_decrypt ~keyed:true ~math:true
        ~field_policy:PL.Unique_fields ~strict:true ledger withdraw) with
      | Error failure -> failure.PL.tag = "bad_zero_proof"
      | Ok _ -> false);
    check "stored key decrypt changed state" (state ledger = before);
    let migration old_proof = fields samples.(0) @ owner_fields new_key new_secret @ [
      "new_cipher", `String (F.encode_cipher new_cipher);
      "source_cipher_hash", `String Digestif.SHA256.(digest_string cipher |> to_hex);
      "old_zero_proof", `String old_proof;
      "new_zero_proof", `String new_proof;
      "amount_commitment", `String commitment] in
    let apply proof = run (PL.apply_key_switch ~keyed:true ~math:true
      ~cipher_mode:C.Rule_graph.Active ~field_policy:PL.Unique_fields
      ~strict:true ledger (tx T.KeySwitch (migration proof))) in
    check "old transcript accepted" (match apply prior_proof with
      | PL.Key_switch_rejected {consume_nonce = false; _} -> true
      | _ -> false);
    check "old transcript changed state" (state ledger = before);
    check "stored key switch refused" (match apply current_proof with
      | PL.Key_switch_applied {migrated_cipher = true; _} -> true
      | _ -> false);
    check "stored key not replaced" (run (L.get_pvac_pubkey ledger address) =
      Some (key new_key));
    let account = L.find ledger address in
    check "stored cipher not replaced" (account.encrypted_balance =
      Some (F.encode_cipher new_cipher));
    check "stored value changed" (P.dec_value new_key new_secret new_cipher = 20L));
  Printf.printf "status = pass test = stored_key_switch\n%!"

let () =
  check "sample path missing" (Array.length Sys.argv = 2 ||
    (Array.length Sys.argv = 3 &&
      List.mem Sys.argv.(2) ["--entry"; "--stored"]));
  let samples = read Sys.argv.(1) in
  if Array.length Sys.argv = 2 || Sys.argv.(2) = "--entry" then begin
    owner_checks samples;
    registration samples;
    stored_key samples
  end;
  if Array.length Sys.argv = 2 then begin
    composition samples;
    stored_switch samples
  end else if Sys.argv.(2) = "--stored" then
    stored_switch samples