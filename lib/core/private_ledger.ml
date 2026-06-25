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


module FB = Crypto.FheBalance
module PR = Pvac_registry
module PM = Pvac_migration
module PT = Crypto.PrivateTransferV4
module SC = Crypto.StealthClaimV5
module SA = Crypto.StealthAddress
module T = Transaction

type kat =
  | Kat_missing
  | Kat_current
  | Kat_mismatch

type failure = {
  tag : string;
  reason : string;
  user_reason : string;
}

type balance_plan = {
  current_cipher : string;
  next_cipher : string;
}

type key_switch_plan = {
  old_key_hash : string;
  new_key_hash : string;
  new_pubkey : string;
  new_cipher : string option;
}

type key_switch_apply = {
  old_key_hash : string;
  new_key_hash : string;
  migrated_cipher : bool;
}

type key_switch_rejection = {
  failure : failure;
  consume_nonce : bool;
}

type key_switch_outcome =
  | Key_switch_applied of key_switch_apply
  | Key_switch_rejected of key_switch_rejection

type stealth_plan = {
  stealth_current_cipher : string;
  stealth_next_cipher : string;
  stealth_delta_cipher : string;
  stealth_range_proof_delta : string;
  stealth_range_proof_balance : string;
  stealth_tag : string;
  stealth_eph_pub : string;
  stealth_enc_amount : string;
  stealth_claim_pub : string;
  stealth_amount_commitment : string;
}

type stealth_range = {
  delta_ok : bool;
  balance_ok : bool;
}

type claim_plan = {
  claim_output_id : int;
  claim_cipher : string;
}

let key_bound_stealth_output_marker = "pvac_key_bound_output_v1"

let stealth_output_is_key_bound (so : Ledger_types.stealth_output) =
  String.equal so.Ledger_types.amount_hash key_bound_stealth_output_marker

type encrypt_payload = {
  cipher : string;
  amount_commitment : string;
  zero_proof : string;
  blinding : string;
}

type decrypt_payload = {
  cipher : string;
  amount_commitment : string;
  zero_proof : string;
  blinding : string;
  range_proof_balance : string option;
}

type key_switch_payload = {
  new_pubkey_b64 : string;
  aes_kat : string;
  old_bound_pubkey_b64 : string option;
  old_bound_cipher : string option;
  new_cipher : string option;
  old_zero_proof : string option;
  new_zero_proof : string option;
  amount_commitment : string option;
  amount_blinding : string option;
  legacy_ct_migration : bool;
  legacy_public_migration : bool;
  legacy_commitment_migration : bool;
}

let error ?user_reason tag reason =
  let user_reason = match user_reason with
    | Some s -> s
    | None -> reason
  in
  Error { tag; reason; user_reason }

let claim_gate claimer (so : Ledger_types.stealth_output) (claim : SC.t) =
  if so.Ledger_types.claimed <> 0 then
    error "already_claimed"
      (Printf.sprintf "stealth output %d already claimed" claim.SC.output_id)
  else if String.length so.Ledger_types.claim_pub = 0 then
    error "invalid_claim_secret" "claim ownership verification failed: you are not the intended recipient"
  else if String.length claim.SC.claim_secret = 0 then
    error "invalid_claim_secret" "claim ownership verification failed: you are not the intended recipient"
  else if not (SA.verify_claim_secret
    ~claim_secret_hex:claim.SC.claim_secret
    ~claimer_addr:claimer
    ~stored_claim_pub_hex:so.Ledger_types.claim_pub) then
    error "invalid_claim_secret" "claim ownership verification failed: you are not the intended recipient"
  else if String.length so.Ledger_types.amount_commitment = 0 then
    error "no_amount_commitment" "stealth output has no stored amount_commitment (not a V5 output)"
  else if not (stealth_output_is_key_bound so) then
    error "legacy_stealth_output"
      "stealth output predates key-bound PVAC send verification; legacy output migration is required"
  else Ok ()

let field json name =
  match Yojson.Safe.Util.(member name json |> to_string_option) with
  | Some s -> Ok s
  | None -> Error ("missing " ^ name)

let json_payload op raw =
  match raw with
  | None -> error "malformed_transaction" (op ^ ": missing encrypted_data") ~user_reason:(op ^ ": malformed encrypted_data")
  | Some s ->
    try Ok (Yojson.Safe.from_string s)
    with e ->
      error "malformed_transaction"
        (op ^ ": cannot parse encrypted_data: " ^ Printexc.to_string e)
        ~user_reason:(op ^ ": malformed encrypted_data")

let safe_json raw =
  match raw with
  | None -> Error "missing"
  | Some s ->
    try Ok (Yojson.Safe.from_string s)
    with _ -> Error "parse"

let parse_encrypt raw =
  match json_payload "encrypt" raw with
  | Error e -> Error e
  | Ok json ->
    match field json "cipher",
      field json "amount_commitment",
      field json "zero_proof",
      field json "blinding" with
    | Ok cipher, Ok amount_commitment, Ok zero_proof, Ok blinding ->
      Ok { cipher; amount_commitment; zero_proof; blinding }
    | Error e, _, _, _
    | _, Error e, _, _
    | _, _, Error e, _
    | _, _, _, Error e ->
      error "malformed_transaction"
        ("encrypt: cannot parse encrypted_data: " ^ e)
        ~user_reason:"encrypt: malformed encrypted_data"

let parse_decrypt raw =
  match json_payload "decrypt" raw with
  | Error e -> Error e
  | Ok json ->
    match field json "cipher",
      field json "amount_commitment",
      field json "zero_proof",
      field json "blinding" with
    | Ok cipher, Ok amount_commitment, Ok zero_proof, Ok blinding ->
      let range_proof_balance =
        Yojson.Safe.Util.(member "range_proof_balance" json |> to_string_option)
      in
      Ok { cipher; amount_commitment; zero_proof; blinding; range_proof_balance }
    | Error e, _, _, _
    | _, Error e, _, _
    | _, _, Error e, _
    | _, _, _, Error e ->
      error "malformed_transaction"
        ("decrypt: cannot parse encrypted_data: " ^ e)
        ~user_reason:"decrypt: malformed encrypted_data"

let parse_key_switch raw =
  match json_payload "key_switch" raw with
  | Error e ->
    error "key_switch_rejected" e.reason ~user_reason:"encrypted_data must be JSON with new_pubkey and aes_kat"
  | Ok json ->
    match field json "new_pubkey", field json "aes_kat" with
    | Ok new_pubkey_b64, Ok aes_kat ->
      Ok {
        new_pubkey_b64;
        aes_kat;
        old_bound_pubkey_b64 = Yojson.Safe.Util.(member "old_bound_pubkey" json |> to_string_option);
        old_bound_cipher = Yojson.Safe.Util.(member "old_bound_cipher" json |> to_string_option);
        new_cipher = Yojson.Safe.Util.(member "new_cipher" json |> to_string_option);
        old_zero_proof = Yojson.Safe.Util.(member "old_zero_proof" json |> to_string_option);
        new_zero_proof = Yojson.Safe.Util.(member "new_zero_proof" json |> to_string_option);
        amount_commitment = Yojson.Safe.Util.(member "amount_commitment" json |> to_string_option);
        amount_blinding = Yojson.Safe.Util.(member "amount_blinding" json |> to_string_option);
        legacy_ct_migration =
          Yojson.Safe.Util.(member "legacy_ct_migration" json |> to_bool_option) = Some true;
        legacy_public_migration =
          Yojson.Safe.Util.(member "legacy_public_migration" json |> to_bool_option) = Some true;
        legacy_commitment_migration =
          Yojson.Safe.Util.(member "legacy_commitment_migration" json |> to_bool_option) = Some true;
      }
    | Error e, _
    | _, Error e ->
      error "key_switch_rejected"
        ("encrypted_data must be JSON with new_pubkey and aes_kat: " ^ e)
        ~user_reason:"encrypted_data must be JSON with new_pubkey and aes_kat"

let key_switch_requests_legacy_public_migration tx =
  match parse_key_switch tx.T.encrypted_data with
  | Ok payload -> payload.legacy_public_migration
  | Error _ -> false

let key_switch_requests_legacy_audit tx =
  match parse_key_switch tx.T.encrypted_data with
  | Ok payload ->
    payload.legacy_public_migration ||
    payload.legacy_ct_migration ||
    payload.legacy_commitment_migration
  | Error _ -> false

let version json =
  match Yojson.Safe.Util.(member "version" json |> to_int_option) with
  | Some v -> v
  | None -> 0

let parse_stealth raw =
  match safe_json raw with
  | Error "missing" -> error "missing_encrypted_data" "encrypted_data required for stealth transfer"
  | Error _ -> error "version_rejected" "only version 5 stealth transfers are accepted"
  | Ok json ->
    if version json <> 5 then
      error "version_rejected" "only version 5 stealth transfers are accepted"
    else
      match PT.of_json json with
      | Error e -> error "invalid_stealth_data" ("cannot parse V5 payload: " ^ e)
      | Ok ptd ->
        if String.length ptd.PT.claim_pub <> 64 then
          error "invalid_claim_pub" "claim_pub must be 64 hex characters"
        else
          Ok ptd

let parse_claim raw =
  match safe_json raw with
  | Error "missing" -> error "missing_encrypted_data" "encrypted_data required for claim"
  | Error _ -> error "version_rejected" "only version 5 (private) claim operations are accepted"
  | Ok json ->
    if version json <> 5 then
      error "version_rejected" "only version 5 (private) claim operations are accepted"
    else
      match SC.of_json json with
      | Error e -> error "invalid_claim_data" ("cannot parse V5 claim payload: " ^ e)
      | Ok claim -> Ok claim

let kat_state ledger addr =
  let expected = PR.expected_kat () in
  match Ledger.get_pvac_kat ledger addr with
  | None -> Kat_missing
  | Some stored when stored = expected -> Kat_current
  | Some _ -> Kat_mismatch

let backfill_kat ledger addr =
  Ledger.set_pvac_kat ledger addr (PR.expected_kat ())

let current_cipher ledger addr tag =
  match Ledger.find_opt ledger addr with
  | None -> error tag "account not found"
  | Some acc -> Ok (Option.value ~default:"0" acc.Ledger.encrypted_balance)

let load_pk ledger addr tag op =
  let open Lwt.Syntax in
  let* pk_opt = Ledger.get_pvac_pubkey ledger addr in
  match pk_opt with
  | None ->
    Lwt.return
      (error tag
        (op ^ ": no pvac pubkey registered")
        ~user_reason:(op ^ ": bound zero proof failed: no pvac pubkey registered"))
  | Some blob ->
    match FB.load_pubkey_result blob with
    | Error e ->
      Lwt.return
        (error tag
          (op ^ ": " ^ e)
          ~user_reason:(op ^ ": bound zero proof failed: " ^ e))
    | Ok pk -> Lwt.return (Ok pk)

let load_pk_plain ledger addr missing_tag missing_reason bad_tag =
  let open Lwt.Syntax in
  let* pk_opt = Ledger.get_pvac_pubkey ledger addr in
  match pk_opt with
  | None -> Lwt.return (error missing_tag missing_reason)
  | Some blob ->
    match FB.load_pubkey_result blob with
    | Error e -> Lwt.return (error bad_tag e)
    | Ok pk -> Lwt.return (Ok pk)

let encrypt_plan ledger tx =
  let open Lwt.Syntax in
  match parse_encrypt tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok payload ->
    let* pk_result = load_pk ledger tx.T.from "bad_zero_proof" "encrypt" in
    match pk_result with
    | Error e -> Lwt.return (Error e)
    | Ok pk ->
      match FB.verify_encrypt_proof pk payload.cipher tx.T.amount payload.zero_proof payload.amount_commitment payload.blinding with
      | Error e ->
        Lwt.return
          (error "bad_zero_proof"
            ("encrypt: " ^ e)
            ~user_reason:("encrypt: bound zero proof failed: " ^ e))
      | Ok () ->
        match current_cipher ledger tx.T.from "encrypt_balance_failed" with
        | Error e -> Lwt.return (Error e)
        | Ok current ->
          match FB.decode_cipher payload.cipher with
          | Error e -> Lwt.return (error "encrypt_balance_failed" e)
          | Ok delta ->
            match FB.deposit_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
            | Error e -> Lwt.return (error "encrypt_balance_failed" e)
            | Ok next_cipher -> Lwt.return (Ok { current_cipher = current; next_cipher })

let max_decrypt_amount = Z.of_string "1000000000000"

let decrypt_plan ledger tx =
  let open Lwt.Syntax in
  if Z.gt tx.T.amount max_decrypt_amount then
    Lwt.return
      (error "amount_too_large" "decrypt amount exceeds 1000000 OCT limit")
  else if Z.leq tx.T.amount Z.zero then
    Lwt.return
      (error "invalid_amount" "decrypt amount must be positive")
  else
    match parse_decrypt tx.T.encrypted_data with
    | Error e -> Lwt.return (Error e)
    | Ok payload ->
      let* pk_result = load_pk ledger tx.T.from "bad_zero_proof" "decrypt" in
      match pk_result with
      | Error e -> Lwt.return (Error e)
      | Ok pk ->
        match FB.verify_encrypt_proof pk payload.cipher tx.T.amount payload.zero_proof payload.amount_commitment payload.blinding with
        | Error e ->
          Lwt.return
            (error "bad_zero_proof"
              ("decrypt: " ^ e)
              ~user_reason:("decrypt: bound zero proof failed: " ^ e))
        | Ok () ->
          match current_cipher ledger tx.T.from "decrypt_cipher_failed" with
          | Error e -> Lwt.return (Error e)
          | Ok current ->
            match FB.decode_cipher payload.cipher with
            | Error e ->
              Lwt.return
                (error "decrypt_cipher_failed" ("cannot decode delta cipher: " ^ e))
            | Ok delta ->
              match FB.withdraw_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
              | Error e ->
                Lwt.return
                  (error "decrypt_cipher_failed" ("encrypted balance subtract failed: " ^ e))
              | Ok next_cipher ->
                match payload.range_proof_balance with
                | None ->
                  Lwt.return
                    (error "bad_range_proof_balance"
                      "decrypt: range_proof_balance required: proves remaining encrypted balance >= 0"
                      ~user_reason:"decrypt: range_proof_balance required: proves remaining encrypted balance >= 0")
                | Some proof ->
                  if FB.verify_range pk next_cipher proof then
                    Lwt.return (Ok { current_cipher = current; next_cipher })
                  else
                    Lwt.return
                      (error "bad_range_proof_balance"
                        "decrypt: range_proof_balance verification failed: remaining balance may be negative"
                        ~user_reason:"decrypt: range_proof_balance verification failed: remaining balance may be negative")

let apply_encrypt ledger tx =
  let open Lwt.Syntax in
  let* plan = encrypt_plan ledger tx in
  match plan with
  | Error e -> Lwt.return (Error e)
  | Ok plan ->
    let total_cost = Z.add tx.T.amount tx.T.ou in
    match Ledger.debit ledger tx.T.from total_cost tx.T.nonce with
    | Error err -> Lwt.return (error "insufficient_balance" err)
    | Ok () ->
      (match Ledger.update_enc_balance ledger tx.T.from plan.next_cipher with
      | Ok () -> Lwt.return (Ok plan)
      | Error e -> failwith (Printf.sprintf "encrypt update_enc_balance: %s" e))

let apply_decrypt ledger tx =
  let open Lwt.Syntax in
  let* plan = decrypt_plan ledger tx in
  match plan with
  | Error e -> Lwt.return (Error e)
  | Ok plan ->
    match Ledger.debit ledger tx.T.from tx.T.ou tx.T.nonce with
    | Error err -> Lwt.return (error "insufficient_balance" err)
    | Ok () ->
      match Ledger.credit ledger tx.T.from tx.T.amount with
      | Error err -> Lwt.return (error "supply_violation" err)
      | Ok () ->
        (match Ledger.update_enc_balance ledger tx.T.from plan.next_cipher with
        | Ok () -> Lwt.return (Ok plan)
        | Error e -> failwith (Printf.sprintf "decrypt update_enc_balance: %s" e))

let legacy_public_replay_amount = function
  | Some replay when replay.Pvac_legacy_public_replay.can_public_migrate ->
    (match replay.public_net with
    | Some amount when Z.sign amount > 0 -> Ok amount
    | Some _ -> Error "legacy public replay produced an empty balance"
    | None -> Error "legacy public replay did not produce a public balance")
  | Some replay -> Error replay.reason
  | None -> Error "legacy public migration requires node public-history replay"

let legacy_audit_allows_witness = function
  | Some replay ->
    (match replay.Pvac_legacy_public_replay.audit_class with
    | Pvac_legacy_public_replay.Poisoned -> Error replay.reason
    | Public_clean
    | Hidden_witness -> Ok ())
  | None -> Error "legacy ciphertext migration requires node history audit"

let legacy_audit_commitment = function
  | Some replay ->
    (match replay.Pvac_legacy_public_replay.audit_class, replay.commitment_net with
    | Pvac_legacy_public_replay.Poisoned, _ -> Error replay.reason
    | _, Some commitment -> Ok commitment
    | _, None -> Error "legacy commitment migration requires reconstructable commitment history")
  | None -> Error "legacy commitment migration requires node history audit"

let verify_legacy_public_migration new_loaded amount payload =
  match payload.new_cipher, payload.new_zero_proof, payload.amount_commitment, payload.amount_blinding with
  | Some new_cipher, Some new_zero_proof, Some amount_commitment, Some amount_blinding ->
    (match PM.classify_cipher new_cipher with
    | PM.V3 ->
      FB.verify_encrypt_proof new_loaded new_cipher amount new_zero_proof amount_commitment amount_blinding
    | _ -> Error "new encrypted balance must be v3 key-bound")
  | _ ->
    Error "legacy public migration requires new_cipher, new_zero_proof, amount_commitment and amount_blinding"

let legacy_ct_old_pubkey_blob old_blob payload =
  let candidate =
    match payload.old_bound_pubkey_b64 with
    | None -> Ok old_blob
    | Some encoded ->
      (match PR.decode_b64 encoded with
      | Error e -> Error e
      | Ok raw -> PR.canonicalize_blob raw)
  in
  match candidate with
  | Error e -> Error ("old bound pubkey decode failed: " ^ e)
  | Ok bound_blob ->
    (match FB.pubkey_is_key_bound_extension old_blob bound_blob with
    | Error e -> Error ("old bound pubkey extension check failed: " ^ e)
    | Ok false -> Error "old bound pubkey is not an extension of the registered key"
    | Ok true -> Ok bound_blob)

let verify_legacy_ct_migration old_loaded new_loaded current_cipher payload =
  match payload.old_bound_cipher, payload.new_cipher, payload.old_zero_proof, payload.new_zero_proof, payload.amount_commitment with
  | Some old_bound_cipher, Some new_cipher, Some old_zero_proof, Some new_zero_proof, Some amount_commitment ->
    (match FB.cipher_is_key_bound_extension current_cipher old_bound_cipher with
    | Error e -> Error ("old bound cipher extension check failed: " ^ e)
    | Ok false -> Error "old bound cipher is not an extension of the ledger cipher"
    | Ok true ->
      (match PM.classify_cipher old_bound_cipher, PM.classify_cipher new_cipher with
      | PM.V3, PM.V3 ->
        (match FB.verify_claim_amount_v5 old_loaded old_bound_cipher old_zero_proof amount_commitment with
        | Error e -> Error ("old encrypted balance proof failed: " ^ e)
        | Ok () ->
          (match FB.verify_claim_amount_v5 new_loaded new_cipher new_zero_proof amount_commitment with
          | Error e -> Error ("new encrypted balance proof failed: " ^ e)
          | Ok () -> Ok ()))
      | _ -> Error "legacy ciphertext migration requires key-bound old and new ciphers"))
  | _ ->
    Error "legacy ciphertext migration requires old_bound_cipher, new_cipher, old_zero_proof, new_zero_proof and amount_commitment"

let verify_legacy_commitment_migration new_loaded commitment payload =
  match payload.new_cipher, payload.new_zero_proof with
  | Some new_cipher, Some new_zero_proof ->
    (match PM.classify_cipher new_cipher with
    | PM.V3 ->
      FB.verify_claim_amount_v5 new_loaded new_cipher new_zero_proof commitment
    | _ -> Error "legacy commitment migration requires a v3 key-bound new cipher")
  | _ ->
    Error "legacy commitment migration requires new_cipher and new_zero_proof"

let key_switch_plan ?legacy_public_replay ledger tx =
  let open Lwt.Syntax in
  match parse_key_switch tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok payload ->
    if payload.aes_kat <> PR.expected_kat () then
      Lwt.return
        (error "key_switch_rejected" "AES KAT mismatch - incompatible key")
    else
      match PR.decode_b64 payload.new_pubkey_b64 with
      | Error e ->
        Lwt.return (error "key_switch_rejected" e)
      | Ok new_raw ->
        match PR.canonicalize_blob new_raw with
        | Error e ->
          Lwt.return (error "key_switch_rejected" e)
        | Ok new_pubkey ->
          let* old_pk = Ledger.get_pvac_pubkey ledger tx.T.from in
          let old_key_hash = match old_pk with
            | Some blob -> PR.key_hash blob
            | None -> "none"
          in
          let new_key_hash = PR.key_hash new_pubkey in
          let current = current_cipher ledger tx.T.from "key_switch_rejected" in
          match current with
          | Error e -> Lwt.return (Error e)
          | Ok current_cipher ->
            let migration_status = PM.status_of_cipher current_cipher in
            if migration_status.can_key_switch then
              Lwt.return (Ok { old_key_hash; new_key_hash; new_pubkey; new_cipher = None })
            else if migration_status.needs_legacy_public_replay then
              if payload.legacy_commitment_migration then
                (match legacy_audit_commitment legacy_public_replay with
                | Error e -> Lwt.return (error "key_switch_rejected" e)
                | Ok commitment ->
                  (match FB.load_pubkey_result new_pubkey with
                  | Error e -> Lwt.return (error "key_switch_rejected" ("new pubkey decode failed: " ^ e))
                  | Ok new_loaded ->
                    match verify_legacy_commitment_migration new_loaded commitment payload with
                    | Error e -> Lwt.return (error "key_switch_rejected" ("legacy commitment migration failed: " ^ e))
                    | Ok () ->
                      (match payload.new_cipher with
                      | Some new_cipher -> Lwt.return (Ok { old_key_hash; new_key_hash; new_pubkey; new_cipher = Some new_cipher })
                      | None -> Lwt.return (error "key_switch_rejected" "legacy commitment migration lost new cipher"))))
              else if payload.legacy_ct_migration then
                (match legacy_audit_allows_witness legacy_public_replay with
                | Error e -> Lwt.return (error "key_switch_rejected" e)
                | Ok () ->
                (match old_pk with
                | None -> Lwt.return (error "key_switch_rejected" "legacy ciphertext migration requires old registered pvac key")
                | Some old_blob ->
                  (match legacy_ct_old_pubkey_blob old_blob payload with
                  | Error e -> Lwt.return (error "key_switch_rejected" e)
                  | Ok old_bound_blob ->
                  (match FB.load_pubkey_result old_bound_blob, FB.load_pubkey_result new_pubkey with
                  | Ok old_loaded, Ok new_loaded ->
                    (match verify_legacy_ct_migration old_loaded new_loaded current_cipher payload with
                    | Error e -> Lwt.return (error "key_switch_rejected" ("legacy ciphertext migration failed: " ^ e))
                    | Ok () ->
                      (match payload.new_cipher with
                      | Some new_cipher -> Lwt.return (Ok { old_key_hash; new_key_hash; new_pubkey; new_cipher = Some new_cipher })
                      | None -> Lwt.return (error "key_switch_rejected" "legacy ciphertext migration lost new cipher")))
                  | Error e, _ -> Lwt.return (error "key_switch_rejected" ("old pubkey decode failed: " ^ e))
                  | _, Error e -> Lwt.return (error "key_switch_rejected" ("new pubkey decode failed: " ^ e))))))
              else if payload.legacy_public_migration then
                match legacy_public_replay_amount legacy_public_replay with
                | Error e -> Lwt.return (error "key_switch_rejected" e)
                | Ok amount ->
                  (match FB.load_pubkey_result new_pubkey with
                  | Error e -> Lwt.return (error "key_switch_rejected" ("new pubkey decode failed: " ^ e))
                  | Ok new_loaded ->
                    match verify_legacy_public_migration new_loaded amount payload with
                    | Error e -> Lwt.return (error "key_switch_rejected" ("legacy public migration failed: " ^ e))
                    | Ok () ->
                      (match payload.new_cipher with
                      | Some new_cipher -> Lwt.return (Ok { old_key_hash; new_key_hash; new_pubkey; new_cipher = Some new_cipher })
                      | None -> Lwt.return (error "key_switch_rejected" "legacy public migration lost new cipher")))
              else
                Lwt.return (error "key_switch_rejected" migration_status.reason)
            else if not migration_status.can_v3_migrate then
              Lwt.return (error "key_switch_rejected" migration_status.reason)
            else
              match old_pk, payload.new_cipher, payload.old_zero_proof, payload.new_zero_proof, payload.amount_commitment with
              | Some old_blob, Some new_cipher, Some old_zero_proof, Some new_zero_proof, Some amount_commitment ->
                (match FB.load_pubkey_result old_blob, FB.load_pubkey_result new_pubkey with
                | Ok old_loaded, Ok new_loaded ->
                  (match FB.verify_claim_amount_v5 old_loaded current_cipher old_zero_proof amount_commitment with
                  | Error e -> Lwt.return (error "key_switch_rejected" ("old encrypted balance proof failed: " ^ e))
                  | Ok () ->
                    (match FB.verify_claim_amount_v5 new_loaded new_cipher new_zero_proof amount_commitment with
                    | Error e -> Lwt.return (error "key_switch_rejected" ("new encrypted balance proof failed: " ^ e))
                    | Ok () -> Lwt.return (Ok { old_key_hash; new_key_hash; new_pubkey; new_cipher = Some new_cipher })))
                | Error e, _ -> Lwt.return (error "key_switch_rejected" ("old pubkey decode failed: " ^ e))
                | _, Error e -> Lwt.return (error "key_switch_rejected" ("new pubkey decode failed: " ^ e)))
              | _ ->
                Lwt.return
                  (error "key_switch_rejected"
                    "encrypted balance migration requires new_cipher, old_zero_proof, new_zero_proof and amount_commitment")

let apply_key_switch ?legacy_public_replay ledger tx =
  let open Lwt.Syntax in
  let* plan = key_switch_plan ?legacy_public_replay ledger tx in
  match plan with
  | Error failure ->
    Lwt.return (Key_switch_rejected { failure; consume_nonce = false })
  | Ok plan ->
    match Ledger.debit ledger tx.T.from tx.T.ou tx.T.nonce with
    | Error reason ->
      let failure = { tag = "key_switch_failed"; reason; user_reason = reason } in
      Lwt.return (Key_switch_rejected { failure; consume_nonce = false })
    | Ok () ->
      let* () = Ledger.delete_pvac_pubkey ledger tx.T.from in
      let* () = Ledger.set_pvac_pubkey ledger tx.T.from plan.new_pubkey in
      let migrated_cipher = match plan.new_cipher with
        | None -> false
        | Some cipher ->
          (match Ledger.update_enc_balance ledger tx.T.from cipher with
          | Ok () -> true
          | Error e -> failwith (Printf.sprintf "key_switch update_enc_balance: %s" e))
      in
      Ledger.set_pvac_kat ledger tx.T.from (PR.expected_kat ());
      Lwt.return
        (Key_switch_applied {
          old_key_hash = plan.old_key_hash;
          new_key_hash = plan.new_key_hash;
          migrated_cipher;
        })

let stealth_plan ledger tx =
  let open Lwt.Syntax in
  match parse_stealth tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok ptd ->
    let* pk_result =
      load_pk_plain ledger tx.T.from
        "no_pvac_pubkey"
        "sender has no registered PVAC pubkey"
        "bad_pvac_pubkey"
    in
    match pk_result with
    | Error e -> Lwt.return (Error e)
    | Ok pk ->
      if not (FB.verify_commitment pk ptd.PT.delta_cipher ptd.PT.commitment) then
        Lwt.return (error "bad_commitment" "delta cipher commitment verification failed")
      else
        match current_cipher ledger tx.T.from "encrypted_balance_update_failed" with
        | Error e -> Lwt.return (Error e)
        | Ok current ->
          match FB.decode_cipher ptd.PT.delta_cipher with
          | Error e ->
            Lwt.return
              (error "bad_delta_cipher" ("cannot decode delta cipher: " ^ e))
          | Ok delta ->
            match FB.withdraw_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
            | Error e ->
              Lwt.return
                (error "encrypted_balance_update_failed" ("sender encrypted balance update: " ^ e))
            | Ok next_cipher ->
              Lwt.return
                (Ok {
                  stealth_current_cipher = current;
                  stealth_next_cipher = next_cipher;
                  stealth_delta_cipher = ptd.PT.delta_cipher;
                  stealth_range_proof_delta = ptd.PT.range_proof_delta;
                  stealth_range_proof_balance = ptd.PT.range_proof_balance;
                  stealth_tag = ptd.PT.stealth_tag;
                  stealth_eph_pub = ptd.PT.eph_pub;
                  stealth_enc_amount = ptd.PT.enc_amount;
                  stealth_claim_pub = ptd.PT.claim_pub;
                  stealth_amount_commitment = ptd.PT.amount_commitment;
                })

let stealth_inline_range ledger tx plan =
  let open Lwt.Syntax in
  let* pk_result =
    load_pk_plain ledger tx.T.from
      "no_pvac_pubkey"
      "sender has no registered PVAC pubkey"
      "bad_pvac_pubkey"
  in
  match pk_result with
  | Error e -> Lwt.return (Error e)
  | Ok pk ->
    let delta_ok =
      FB.verify_range pk plan.stealth_delta_cipher plan.stealth_range_proof_delta
    in
    let balance_ok =
      FB.verify_range pk plan.stealth_next_cipher plan.stealth_range_proof_balance
    in
    Lwt.return (Ok { delta_ok; balance_ok })

let stealth_accept_range range =
  if not range.delta_ok then
    error "bad_range_proof_delta" "delta range proof failed: delta must be non-negative"
  else if not range.balance_ok then
    error "bad_range_proof_balance" "balance range proof failed: sender may have insufficient balance"
  else
    Ok ()

let stealth_binding ledger tx plan =
  let open Lwt.Syntax in
  match parse_stealth tx.T.encrypted_data with
    | Error e -> Lwt.return (Error e)
    | Ok ptd ->
      let* pk_result =
        load_pk_plain ledger tx.T.from
          "no_pvac_pubkey"
          "sender has no registered PVAC pubkey"
          "bad_pvac_pubkey"
      in
      match pk_result with
      | Error e -> Lwt.return (Error e)
      | Ok pk ->
        if String.length ptd.PT.send_zero_proof = 0 then
          Lwt.return
            (error "bad_send_binding"
              "send_zero_proof does not bind delta_cipher to amount_commitment: send_zero_proof required but missing")
        else
          match FB.verify_claim_amount_v5 pk plan.stealth_delta_cipher ptd.PT.send_zero_proof plan.stealth_amount_commitment with
          | Ok () -> Lwt.return (Ok ())
          | Error e ->
            Lwt.return
              (error "bad_send_binding"
                ("send_zero_proof does not bind delta_cipher to amount_commitment: " ^ e))

let claim_plan ledger tx =
  let open Lwt.Syntax in
  if not (String.equal tx.T.from tx.T.to_) then
    Lwt.return (error "claim_not_self" "claim operation must target sender (from == to)")
  else
    match parse_claim tx.T.encrypted_data with
    | Error e -> Lwt.return (Error e)
    | Ok claim ->
      let* so_opt = Ledger.get_stealth_output_by_id ledger claim.SC.output_id in
      match so_opt with
      | None ->
        Lwt.return
          (error "output_not_found"
            (Printf.sprintf "stealth output %d not found" claim.SC.output_id))
      | Some so ->
        match claim_gate tx.T.from so claim with
        | Error e -> Lwt.return (Error e)
        | Ok () ->
          let* pk_result =
            load_pk_plain ledger tx.T.from
              "no_pvac_pubkey"
              "claimant has no registered PVAC pubkey"
              "bad_pvac_pubkey"
          in
          match pk_result with
          | Error e -> Lwt.return (Error e)
          | Ok pk ->
            if not (FB.verify_commitment pk claim.SC.claim_cipher claim.SC.commitment) then
              Lwt.return (error "bad_commitment" "claim cipher commitment verification failed")
            else
              match FB.verify_claim_amount_v5 pk claim.SC.claim_cipher claim.SC.zero_proof so.Ledger_types.amount_commitment with
              | Error e ->
                Lwt.return
                  (error "bad_zero_proof" ("bound zero proof verification failed: " ^ e))
              | Ok () ->
                Lwt.return (Ok { claim_output_id = claim.SC.output_id; claim_cipher = claim.SC.claim_cipher })

let claim_balance_plan ledger tx plan =
  let open Lwt.Syntax in
  let* pk_result =
    load_pk_plain ledger tx.T.from
      "no_pvac_pubkey"
      "claimant has no registered PVAC pubkey"
      "bad_pvac_pubkey"
  in
  match pk_result with
  | Error e -> Lwt.return (Error e)
  | Ok pk ->
    match current_cipher ledger tx.T.from "encrypted_balance_update_failed" with
    | Error e -> Lwt.return (Error e)
    | Ok current ->
      match FB.decode_cipher plan.claim_cipher with
      | Error e ->
        Lwt.return
          (error "bad_claim_cipher" ("cannot decode claim cipher: " ^ e))
      | Ok delta ->
        match FB.deposit_with_pubkey pk ~current_cipher:(Some current) ~delta_cipher:delta with
        | Error e ->
          Lwt.return
            (error "encrypted_balance_update_failed" ("claimant encrypted balance update: " ^ e))
        | Ok next_cipher ->
          Lwt.return (Ok { current_cipher = current; next_cipher })