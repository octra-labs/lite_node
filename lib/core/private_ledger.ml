(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module FB = Crypto.FheBalance
module PR = Pvac_registry
module PM = Pvac_migration
module PT = Crypto.PrivateTransferV4
module SC = Crypto.StealthClaimV5
module SA = Crypto.StealthAddress
module T = Transaction
module VW = Pvac_verify_worker

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
  source_cipher : string;
}

type key_switch_source = {
  source_cipher : string;
  source_key_hash : string;
}

type key_switch_snapshot = {
  snapshot_cipher : string;
  snapshot_pubkey : string option;
}

type key_switch_artifact =
  | Verified_key_switch of {
      artifact_tx_hash : string;
      artifact_plan : key_switch_plan;
    }
  | Rejected_key_switch of {
      artifact_tx_hash : string;
      artifact_source : key_switch_source;
      artifact_failure : failure;
    }

type private_op =
  | Private_encrypt
  | Private_decrypt
  | Private_stealth
  | Private_claim

type private_account =
  | Private_account_missing
  | Private_account of string

type private_claim =
  | Private_claim_none
  | Private_claim_invalid
  | Private_claim_missing of int
  | Private_claim_output of int * string

type private_source = {
  source_tx_hash : string;
  source_op : private_op;
  source_account : private_account;
  source_key_hash : string option;
  source_claim : private_claim;
  source_policy : Private_result_policy.t;
  source_verifier : string;
}

let key_switch_cache_cap = 32

let key_switch_cache : (string, key_switch_plan) Hashtbl.t =
  Hashtbl.create key_switch_cache_cap

let key_switch_cache_key ledger tx =
  let open Lwt.Syntax in
  let* root = Ledger.hash ledger in
  Lwt.return (T.hash tx ^ ":" ^ root)

let remember_key_switch_plan key plan =
  if
    not (Hashtbl.mem key_switch_cache key)
    && Hashtbl.length key_switch_cache >= key_switch_cache_cap
  then
    Hashtbl.reset key_switch_cache;
  Hashtbl.replace key_switch_cache key plan

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

type prepared =
  | Prepared_encrypt of balance_plan
  | Prepared_decrypt of balance_plan
  | Prepared_key_switch of key_switch_plan
  | Prepared_stealth of stealth_plan
  | Prepared_claim of claim_plan * balance_plan

type private_rejection = {
  private_error : failure;
  private_preverify_reason : string;
}

type private_artifact =
  | Verified_private of {
      private_source : private_source;
      private_prepared : prepared;
    }
  | Rejected_private of {
      private_source : private_source;
      private_rejection : private_rejection;
    }

type private_binding =
  | Private_bound of prepared
  | Private_source_changed
  | Private_artifact_invalid of private_rejection

type key_switch_binding =
  | Key_switch_bound of prepared
  | Key_switch_source_changed
  | Key_switch_artifact_invalid of failure

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
  source_cipher_hash : string option;
  new_cipher : string option;
  old_zero_proof : string option;
  new_zero_proof : string option;
  amount_commitment : string option;
  amount_blinding : string option;
  legacy_ct_migration : bool;
  legacy_public_migration : bool;
  legacy_commitment_migration : bool;
  legacy_zero_reset : bool;
}

let key_switch_migration_count payload =
  [
    payload.legacy_ct_migration;
    payload.legacy_public_migration;
    payload.legacy_commitment_migration;
    payload.legacy_zero_reset;
  ]
  |> List.fold_left (fun count enabled -> if enabled then count + 1 else count) 0

let error ?user_reason tag reason =
  let user_reason = match user_reason with
    | Some s -> s
    | None -> reason
  in
  Error { tag; reason; user_reason }

let key_switch_worker_retry_tag = "key_switch_worker_retry"

let private_worker_retry_tag = "private_worker_retry"

let key_switch_worker_error prefix worker_failure =
  let tag =
    match worker_failure with
    | VW.Proof_rejected _ -> "key_switch_rejected"
    | VW.Worker_busy
    | VW.Worker_unavailable _
    | VW.Worker_timed_out
    | VW.Worker_memory_exceeded
    | VW.Worker_failed _ -> key_switch_worker_retry_tag
  in
  error tag (prefix ^ VW.verification_failure_message worker_failure)

let key_switch_failure_retryable failure =
  String.equal failure.tag key_switch_worker_retry_tag

let private_failure_retryable failure =
  String.equal failure.tag private_worker_retry_tag

let private_reject ?preverify_reason private_error =
  {
    private_error;
    private_preverify_reason =
      Option.value preverify_reason ~default:private_error.reason;
  }

let private_worker_error tag prefix user_prefix worker_failure =
  match worker_failure with
  | VW.Proof_rejected reason ->
    error tag (prefix ^ reason) ~user_reason:(user_prefix ^ reason)
  | VW.Worker_busy
  | VW.Worker_unavailable _
  | VW.Worker_timed_out
  | VW.Worker_memory_exceeded
  | VW.Worker_failed _ ->
    error private_worker_retry_tag
      (prefix ^ VW.verification_failure_message worker_failure)

let digest value =
  Digestif.SHA256.digest_string value
  |> Digestif.SHA256.to_hex

let prepared_hash tag fields =
  String.concat "\000" (tag :: List.map digest fields)
  |> digest

let hash_prepared = function
  | Prepared_encrypt plan ->
    prepared_hash
      "octra:private_transition:encrypt"
      [plan.current_cipher; plan.next_cipher]
  | Prepared_decrypt plan ->
    prepared_hash
      "octra:private_transition:decrypt"
      [plan.current_cipher; plan.next_cipher]
  | Prepared_key_switch plan ->
    prepared_hash
      "octra:private_transition:key_switch"
      [
        plan.old_key_hash;
        plan.new_key_hash;
        plan.new_pubkey;
        Option.value ~default:"" plan.new_cipher;
        plan.source_cipher;
      ]
  | Prepared_stealth plan ->
    prepared_hash
      "octra:private_transition:stealth"
      [
        plan.stealth_current_cipher;
        plan.stealth_next_cipher;
        plan.stealth_delta_cipher;
        plan.stealth_tag;
        plan.stealth_eph_pub;
        plan.stealth_enc_amount;
        plan.stealth_claim_pub;
        plan.stealth_amount_commitment;
      ]
  | Prepared_claim (claim, balance) ->
    prepared_hash
      "octra:private_transition:claim"
      [
        string_of_int claim.claim_output_id;
        claim.claim_cipher;
        balance.current_cipher;
        balance.next_cipher;
      ]

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
    with _ ->
      error "malformed_transaction"
        (op ^ ": malformed encrypted_data")
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
      let payload = {
        new_pubkey_b64;
        aes_kat;
        old_bound_pubkey_b64 = Yojson.Safe.Util.(member "old_bound_pubkey" json |> to_string_option);
        old_bound_cipher = Yojson.Safe.Util.(member "old_bound_cipher" json |> to_string_option);
        source_cipher_hash = Yojson.Safe.Util.(member "source_cipher_hash" json |> to_string_option);
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
        legacy_zero_reset =
          Yojson.Safe.Util.(member "legacy_zero_reset" json |> to_bool_option) = Some true;
      } in
      if key_switch_migration_count payload > 1 then
        error "key_switch_rejected" "key_switch migration modes are mutually exclusive"
      else
        Ok payload
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

let private_verifier = "pvac_private_preverify"

let private_op tx =
  match tx.T.op_type with
  | T.EncryptOp -> Some Private_encrypt
  | T.DecryptOp -> Some Private_decrypt
  | T.StealthOp -> Some Private_stealth
  | T.ClaimOp -> Some Private_claim
  | _ -> None

let private_account ledger address =
  match Ledger.find_opt ledger address with
  | None -> Private_account_missing
  | Some account ->
    account.Ledger.encrypted_balance
    |> Option.value ~default:"0"
    |> digest
    |> fun encrypted_hash -> Private_account encrypted_hash

let private_claim_source ledger tx =
  let open Lwt.Syntax in
  match tx.T.op_type with
  | T.ClaimOp ->
    begin
      match parse_claim tx.T.encrypted_data with
      | Error _ -> Lwt.return Private_claim_invalid
      | Ok claim ->
        let output_id = claim.SC.output_id in
        let* output = Ledger.get_stealth_output_by_id ledger output_id in
        begin
          match output with
          | None -> Lwt.return (Private_claim_missing output_id)
          | Some value ->
            let output_hash =
              Ledger_types.stealth_output_to_yojson value
              |> Yojson.Safe.to_string
              |> digest
            in
            Lwt.return (Private_claim_output (output_id, output_hash))
        end
    end
  | _ -> Lwt.return Private_claim_none

let private_source ~result_policy ledger tx =
  let open Lwt.Syntax in
  match private_op tx with
  | None ->
    Lwt.return
      (error
         "private_artifact_rejected"
         "private artifact operation is not supported")
  | Some source_op ->
    let source_account = private_account ledger tx.T.from in
    let* source_key = Ledger.get_pvac_pubkey ledger tx.T.from in
    let source_key_hash = Option.map PR.key_hash source_key in
    let* source_claim = private_claim_source ledger tx in
    Lwt.return_ok {
      source_tx_hash = T.hash tx;
      source_op;
      source_account;
      source_key_hash;
      source_claim;
      source_policy = result_policy;
      source_verifier = private_verifier;
    }

let private_source_equal left right =
  String.equal left.source_tx_hash right.source_tx_hash
  && left.source_op = right.source_op
  && left.source_account = right.source_account
  && left.source_key_hash = right.source_key_hash
  && left.source_claim = right.source_claim
  && left.source_policy = right.source_policy
  && String.equal left.source_verifier right.source_verifier

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

let key_hash_of_pubkey = function
  | Some blob -> PR.key_hash blob
  | None -> "none"

let load_key_switch_snapshot ledger tx =
  let open Lwt.Syntax in
  match current_cipher ledger tx.T.from "key_switch_rejected" with
  | Error failure -> Lwt.return_error failure
  | Ok snapshot_cipher ->
    let* snapshot_pubkey = Ledger.get_pvac_pubkey ledger tx.T.from in
    Lwt.return_ok { snapshot_cipher; snapshot_pubkey }

let load_pk_blob ledger addr tag op =
  let open Lwt.Syntax in
  let* pk_opt = Ledger.get_pvac_pubkey ledger addr in
  match pk_opt with
  | None ->
    Lwt.return
      (error tag
        (op ^ ": no pvac pubkey registered")
        ~user_reason:(op ^ ": bound zero proof failed: no pvac pubkey registered"))
  | Some blob -> Lwt.return (Ok blob)

let load_pk_blob_plain ledger addr missing_tag missing_reason =
  let open Lwt.Syntax in
  let* pk_opt = Ledger.get_pvac_pubkey ledger addr in
  match pk_opt with
  | None -> Lwt.return (error missing_tag missing_reason)
  | Some blob -> Lwt.return (Ok blob)

let with_loaded_pk blob bad_tag verify =
  match FB.load_pubkey_result blob with
  | Error e -> error bad_tag e
  | Ok pk -> verify pk

let guard_private_input cipher tag op =
  match FB.check_private_input cipher with
  | Error e -> error tag e ~user_reason:(op ^ ": " ^ e)
  | Ok () -> Ok ()

let guard_refresh_source cipher =
  match FB.check_refresh_source cipher with
  | Error e -> error "key_switch_rejected" e
  | Ok () -> Ok ()

let encrypt_balance_plan
    ?(worker_priority = Compute_pool.Required)
    result_policy
    blob
    current
    (payload : encrypt_payload) =
  Proof_pool.run ~priority:worker_priority (fun () ->
    match FB.load_pubkey_result blob with
    | Error e ->
      error "bad_zero_proof"
        ("encrypt: " ^ e)
        ~user_reason:("encrypt: bound zero proof failed: " ^ e)
    | Ok pk ->
      match FB.decode_cipher payload.cipher with
      | Error e -> error "encrypt_balance_failed" e
      | Ok delta ->
        match
          FB.deposit_with_pubkey
            ~result_policy
            pk
            ~current_cipher:(Some current)
            ~delta_cipher:delta
        with
        | Error e -> error "encrypt_balance_failed" e
        | Ok next_cipher -> Ok { current_cipher = current; next_cipher })

let verify_encrypt
    worker_priority
    result_policy
    blob
    current
    amount
    (payload : encrypt_payload) =
  let open Lwt.Syntax in
  match guard_private_input current "encrypt_balance_failed" "encrypt" with
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    let* verified =
      VW.verify_encrypt_classified_with_priority
        worker_priority
        ~pubkey:blob
        ~cipher:payload.cipher
        ~amount
        ~proof:payload.zero_proof
        ~commitment:payload.amount_commitment
        ~blinding:payload.blinding
    in
    begin
      match verified with
      | Error worker_failure ->
        Lwt.return
          (private_worker_error
             "bad_zero_proof"
             "encrypt: "
             "encrypt: bound zero proof failed: "
             worker_failure)
      | Ok () ->
        encrypt_balance_plan
          ~worker_priority
          result_policy
          blob
          current
          payload
    end

let prepare_encrypt_plan
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  match parse_encrypt tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok payload ->
    let* blob_result = load_pk_blob ledger tx.T.from "bad_zero_proof" "encrypt" in
    match blob_result with
    | Error e -> Lwt.return (Error e)
    | Ok blob ->
      match current_cipher ledger tx.T.from "encrypt_balance_failed" with
      | Error e -> Lwt.return (Error e)
      | Ok current ->
        match guard_private_input current "encrypt_balance_failed" "encrypt" with
        | Error e -> Lwt.return (Error e)
        | Ok () -> encrypt_balance_plan result_policy blob current payload

let encrypt_plan
    ?(worker_priority = Compute_pool.Required)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  match parse_encrypt tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok payload ->
    let* blob_result = load_pk_blob ledger tx.T.from "bad_zero_proof" "encrypt" in
    match blob_result with
    | Error e -> Lwt.return (Error e)
    | Ok blob ->
      match current_cipher ledger tx.T.from "encrypt_balance_failed" with
      | Error e -> Lwt.return (Error e)
      | Ok current ->
        verify_encrypt
          worker_priority
          result_policy
          blob
          current
          tx.T.amount
          payload

let max_decrypt_amount = Z.of_string "1000000000000"

let decrypt_balance_plan
    ?(worker_priority = Compute_pool.Required)
    result_policy
    blob
    current
    (payload : decrypt_payload) =
  Proof_pool.run ~priority:worker_priority (fun () ->
    match FB.load_pubkey_result blob with
    | Error e ->
      error "bad_zero_proof"
        ("decrypt: " ^ e)
        ~user_reason:("decrypt: bound zero proof failed: " ^ e)
    | Ok pk ->
      match FB.decode_cipher payload.cipher with
      | Error e ->
        error "decrypt_cipher_failed" ("cannot decode delta cipher: " ^ e)
      | Ok delta ->
        match
          FB.withdraw_with_pubkey
            ~result_policy
            pk
            ~current_cipher:(Some current)
            ~delta_cipher:delta
        with
        | Error e ->
          error "decrypt_cipher_failed" ("encrypted balance subtract failed: " ^ e)
        | Ok next_cipher ->
          Ok { current_cipher = current; next_cipher })

let verify_decrypt
    worker_priority
    result_policy
    blob
    current
    amount
    (payload : decrypt_payload) =
  let open Lwt.Syntax in
  match guard_private_input current "decrypt_cipher_failed" "decrypt" with
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    begin
      match payload.range_proof_balance with
      | None ->
        Lwt.return
          (error "bad_range_proof_balance"
            "decrypt: range_proof_balance required: proves remaining encrypted balance >= 0"
            ~user_reason:"decrypt: range_proof_balance required: proves remaining encrypted balance >= 0")
      | Some range_proof ->
        let* verified =
          VW.verify_encrypt_classified_with_priority
            worker_priority
            ~pubkey:blob
            ~cipher:payload.cipher
            ~amount
            ~proof:payload.zero_proof
            ~commitment:payload.amount_commitment
            ~blinding:payload.blinding
        in
        begin
          match verified with
          | Error worker_failure ->
            Lwt.return
              (private_worker_error
                 "bad_zero_proof"
                 "decrypt: "
                 "decrypt: bound zero proof failed: "
                 worker_failure)
          | Ok () ->
            let* plan =
              decrypt_balance_plan
                ~worker_priority
                result_policy
                blob
                current
                payload
            in
            begin
              match plan with
              | Error _ as e -> Lwt.return e
              | Ok plan ->
                let* range =
                  VW.verify_range_classified_with_priority
                    worker_priority
                    ~pubkey:blob
                    ~cipher:plan.next_cipher
                    ~proof:range_proof
                in
                begin
                  match range with
                  | Ok () -> Lwt.return (Ok plan)
                  | Error (VW.Proof_rejected _) ->
                    Lwt.return
                      (error "bad_range_proof_balance"
                         "decrypt: range_proof_balance verification failed: remaining balance may be negative"
                         ~user_reason:"decrypt: range_proof_balance verification failed: remaining balance may be negative")
                  | Error worker_failure ->
                    Lwt.return
                      (private_worker_error
                         "bad_range_proof_balance"
                         "decrypt range: "
                         "decrypt range: "
                         worker_failure)
                end
            end
        end
    end

let prepare_decrypt_plan
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
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
      let* blob_result = load_pk_blob ledger tx.T.from "bad_zero_proof" "decrypt" in
      match blob_result with
      | Error e -> Lwt.return (Error e)
      | Ok blob ->
        match current_cipher ledger tx.T.from "decrypt_cipher_failed" with
        | Error e -> Lwt.return (Error e)
        | Ok current ->
          match guard_private_input current "decrypt_cipher_failed" "decrypt" with
          | Error e -> Lwt.return (Error e)
          | Ok () -> decrypt_balance_plan result_policy blob current payload

let decrypt_plan
    ?(worker_priority = Compute_pool.Required)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
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
      let* blob_result = load_pk_blob ledger tx.T.from "bad_zero_proof" "decrypt" in
      match blob_result with
      | Error e -> Lwt.return (Error e)
      | Ok blob ->
        match current_cipher ledger tx.T.from "decrypt_cipher_failed" with
        | Error e -> Lwt.return (Error e)
        | Ok current ->
          verify_decrypt
            worker_priority
            result_policy
            blob
            current
            tx.T.amount
            payload

let current_plan ledger addr tag plan =
  match current_cipher ledger addr tag with
  | Error e -> Error e
  | Ok current when String.equal current plan.current_cipher -> Ok ()
  | Ok _ -> error tag "encrypted balance changed during proof verification"

let apply_encrypt_plan ledger tx plan =
  match current_plan ledger tx.T.from "encrypt_balance_failed" plan with
    | Error e -> Lwt.return (Error e)
    | Ok () ->
      let total_cost = Z.add tx.T.amount tx.T.ou in
      match Ledger.debit ledger tx.T.from total_cost tx.T.nonce with
      | Error err -> Lwt.return (error "insufficient_balance" err)
      | Ok () ->
        (match Ledger.update_enc_balance ledger tx.T.from plan.next_cipher with
        | Ok () -> Lwt.return (Ok plan)
        | Error e -> failwith (Printf.sprintf "encrypt update_enc_balance: %s" e))

let apply_encrypt
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  let* plan = encrypt_plan ~result_policy ledger tx in
  match plan with
  | Error e -> Lwt.return (Error e)
  | Ok plan -> apply_encrypt_plan ledger tx plan

let apply_decrypt_plan ledger tx plan =
  match current_plan ledger tx.T.from "decrypt_cipher_failed" plan with
    | Error e -> Lwt.return (Error e)
    | Ok () ->
      match Ledger.debit ledger tx.T.from tx.T.ou tx.T.nonce with
      | Error err -> Lwt.return (error "insufficient_balance" err)
      | Ok () ->
        match Ledger.credit ledger tx.T.from tx.T.amount with
        | Error err -> Lwt.return (error "supply_violation" err)
        | Ok () ->
          (match Ledger.update_enc_balance ledger tx.T.from plan.next_cipher with
          | Ok () -> Lwt.return (Ok plan)
          | Error e -> failwith (Printf.sprintf "decrypt update_enc_balance: %s" e))

let apply_decrypt
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  let* plan = decrypt_plan ~result_policy ledger tx in
  match plan with
  | Error e -> Lwt.return (Error e)
  | Ok plan -> apply_decrypt_plan ledger tx plan

let legacy_public_replay_amount = function
  | Some replay when replay.Pvac_legacy_public_replay.can_public_migrate ->
    (match replay.public_net with
    | Some amount when Z.sign amount > 0 -> Ok amount
    | Some _ -> Error "legacy public replay produced an empty balance"
    | None -> Error "legacy public replay did not produce a public balance")
  | Some replay -> Error replay.reason
  | None -> Error "legacy public migration requires node public-history replay"

let legacy_audit_commitment = function
  | Some replay ->
    (match replay.Pvac_legacy_public_replay.audit_class, replay.commitment_net with
    | Pvac_legacy_public_replay.Poisoned, _ -> Error replay.reason
    | _, Some commitment -> Ok commitment
    | _, None -> Error "legacy commitment migration requires reconstructable commitment history")
  | None -> Error "legacy commitment migration requires node history audit"

let history_migration_required status old_pubkey =
  match status.PM.cipher_class with
  | PM.Legacy_hfhe -> Ok true
  | PM.V3 ->
    begin
      match old_pubkey with
      | None -> Error "encrypted balance has no pvac pubkey"
      | Some blob ->
        Result.map not (FB.blob_supports_alias_rejection blob)
    end
  | PM.Empty
  | PM.Foreign
  | PM.Malformed _ -> Ok false

let history_migration_reason status =
  if status.PM.needs_legacy_public_replay then
    status.reason
  else
    "pvac pubkey proof circuit requires public or commitment history migration"

let verify_legacy_public_migration new_pubkey amount payload =
  match payload.new_cipher, payload.new_zero_proof, payload.amount_commitment, payload.amount_blinding with
  | Some new_cipher, Some new_zero_proof, Some amount_commitment, Some amount_blinding ->
    begin
      match PM.classify_cipher new_cipher with
    | PM.V3 ->
      VW.verify_encrypt
        ~pubkey:new_pubkey
        ~cipher:new_cipher
        ~amount
        ~proof:new_zero_proof
        ~commitment:amount_commitment
        ~blinding:amount_blinding
    | _ ->
      Lwt.return_error "new encrypted balance must be v3 key-bound"
    end
  | _ ->
    Lwt.return_error
      "legacy public migration requires new_cipher, new_zero_proof, amount_commitment and amount_blinding"

let verify_legacy_commitment_migration new_pubkey commitment payload =
  match payload.new_cipher, payload.new_zero_proof with
  | Some new_cipher, Some new_zero_proof ->
    if not (FB.cipher_is_wrapped_scalar new_cipher) then
      Lwt.return_error
        "legacy commitment migration requires a wrapped scalar new cipher"
    else
      begin
        match PM.classify_cipher new_cipher with
      | PM.V3 ->
        VW.verify_claim
          ~pubkey:new_pubkey
          ~cipher:new_cipher
          ~proof:new_zero_proof
          ~commitment
      | _ ->
        Lwt.return_error
          "legacy commitment migration requires a v3 key-bound new cipher"
      end
  | _ ->
    Lwt.return_error
      "legacy commitment migration requires new_cipher and new_zero_proof"

let verify_legacy_zero_reset new_pubkey payload =
  match payload.old_bound_pubkey_b64, payload.old_bound_cipher, payload.old_zero_proof with
  | Some _, _, _
  | _, Some _, _
  | _, _, Some _ ->
    Lwt.return_error
      "legacy zero reset must not include old ciphertext binding fields"
  | None, None, None ->
    match payload.new_cipher, payload.new_zero_proof, payload.amount_commitment, payload.amount_blinding with
    | Some new_cipher, Some new_zero_proof, Some amount_commitment, Some amount_blinding ->
      begin
        match PM.classify_cipher new_cipher with
      | PM.V3 ->
        VW.verify_encrypt
          ~pubkey:new_pubkey
          ~cipher:new_cipher
          ~amount:Z.zero
          ~proof:new_zero_proof
          ~commitment:amount_commitment
          ~blinding:amount_blinding
      | _ ->
        Lwt.return_error
          "legacy zero reset requires a v3 key-bound new cipher"
      end
    | _ ->
      Lwt.return_error
        "legacy zero reset requires new_cipher, new_zero_proof, amount_commitment and amount_blinding"

let key_switch_value ~old_key_hash ~new_key_hash ~new_pubkey ~new_cipher
    ~source_cipher =
  Ok { old_key_hash; new_key_hash; new_pubkey; new_cipher; source_cipher }

let verify_new_key new_pubkey label verify =
  let open Lwt.Syntax in
  let* result = verify new_pubkey in
  match result with
  | Error e ->
    Lwt.return (error "key_switch_rejected" (label ^ e))
  | Ok () ->
    Lwt.return_ok ()

let prepared_legacy_zero_reset payload =
  match
    payload.old_bound_pubkey_b64,
    payload.old_bound_cipher,
    payload.old_zero_proof
  with
  | Some _, _, _
  | _, Some _, _
  | _, _, Some _ ->
    error
      "key_switch_rejected"
      "legacy zero reset must not include old ciphertext binding fields"
  | None, None, None ->
    match
      payload.new_cipher,
      payload.new_zero_proof,
      payload.amount_commitment,
      payload.amount_blinding
    with
    | Some new_cipher, Some _, Some _, Some _ ->
      begin
        match PM.classify_cipher new_cipher with
        | PM.V3 -> Ok new_cipher
        | _ ->
          error
            "key_switch_rejected"
            "legacy zero reset requires a v3 key-bound new cipher"
      end
    | _ ->
      error
        "key_switch_rejected"
        "legacy zero reset requires new_cipher, new_zero_proof, amount_commitment and amount_blinding"

let prepared_legacy_commitment payload =
  match payload.new_cipher, payload.new_zero_proof with
  | Some new_cipher, Some _ when FB.cipher_is_wrapped_scalar new_cipher ->
    begin
      match PM.classify_cipher new_cipher with
      | PM.V3 -> Ok new_cipher
      | _ ->
        error
          "key_switch_rejected"
          "legacy commitment migration requires a v3 key-bound new cipher"
    end
  | Some _, Some _ ->
    error
      "key_switch_rejected"
      "legacy commitment migration requires a wrapped scalar new cipher"
  | _ ->
    error
      "key_switch_rejected"
      "legacy commitment migration requires new_cipher and new_zero_proof"

let prepared_legacy_public payload =
  match
    payload.new_cipher,
    payload.new_zero_proof,
    payload.amount_commitment,
    payload.amount_blinding
  with
  | Some new_cipher, Some _, Some _, Some _ ->
    begin
      match PM.classify_cipher new_cipher with
      | PM.V3 -> Ok new_cipher
      | _ ->
        error
          "key_switch_rejected"
          "new encrypted balance must be v3 key-bound"
    end
  | _ ->
    error
      "key_switch_rejected"
      "legacy public migration requires new_cipher, new_zero_proof, amount_commitment and amount_blinding"

let prepared_bound_migration old_pk current_cipher payload =
  match payload.source_cipher_hash with
  | None ->
    error
      "key_switch_rejected"
      "encrypted balance migration requires source_cipher_hash"
  | Some source when not (String.equal source (digest current_cipher)) ->
    error
      "key_switch_rejected"
      "encrypted balance changed before key switch verification"
  | Some _ ->
    begin
      match guard_refresh_source current_cipher with
      | Error _ as result -> result
      | Ok () ->
        match
          old_pk,
          payload.new_cipher,
          payload.old_zero_proof,
          payload.new_zero_proof,
          payload.amount_commitment
        with
        | Some _, Some new_cipher, Some _, Some _, Some _
            when FB.cipher_is_wrapped_scalar new_cipher ->
          Ok new_cipher
        | Some _, Some _, Some _, Some _, Some _ ->
          error
            "key_switch_rejected"
            "new encrypted balance must be a wrapped scalar"
        | _ ->
          error
            "key_switch_rejected"
            "encrypted balance migration requires new_cipher, old_zero_proof, new_zero_proof and amount_commitment"
    end

let verify_key_switch_plan
    ?legacy_public_replay
    ?snapshot
    ?(worker_priority = Compute_pool.Required)
    ledger
    tx =
  let open Lwt.Syntax in
  match parse_key_switch tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok payload ->
    if payload.aes_kat <> PR.expected_kat () then
      Lwt.return
        (error "key_switch_rejected" "AES KAT mismatch - incompatible key")
    else begin
      let* new_pubkey_result =
        Proof_pool.run ~priority:worker_priority (fun () ->
          match PR.decode_b64 payload.new_pubkey_b64 with
          | Error e -> Error e
          | Ok raw -> PR.canonicalize_blob raw)
      in
      match new_pubkey_result with
      | Error e -> Lwt.return (error "key_switch_rejected" e)
      | Ok new_pubkey ->
          let* old_pk =
            match snapshot with
            | Some value -> Lwt.return value.snapshot_pubkey
            | None -> Ledger.get_pvac_pubkey ledger tx.T.from
          in
          let old_key_hash = key_hash_of_pubkey old_pk in
          let new_key_hash = PR.key_hash new_pubkey in
          let current =
            match snapshot with
            | Some value -> Ok value.snapshot_cipher
            | None -> current_cipher ledger tx.T.from "key_switch_rejected"
          in
          match current with
          | Error e -> Lwt.return (Error e)
          | Ok current_cipher ->
            let* migration_status =
              Proof_pool.run
                ~priority:worker_priority
                (fun () -> PM.status_of_cipher current_cipher)
            in
            let* history_required_result =
              Proof_pool.run
                ~priority:worker_priority
                (fun () ->
                  history_migration_required migration_status old_pk)
            in
            match history_required_result with
            | Error reason ->
              Lwt.return (error "key_switch_rejected" reason)
            | Ok history_required ->
            if payload.legacy_zero_reset && not history_required then
              Lwt.return (error "key_switch_rejected" "legacy zero reset requires a legacy hfhe balance")
            else if migration_status.can_key_switch then
              Lwt.return
                (key_switch_value ~old_key_hash ~new_key_hash ~new_pubkey
                  ~new_cipher:None ~source_cipher:current_cipher)
            else if history_required then
              if payload.legacy_zero_reset then
                  let* checked =
                    verify_new_key new_pubkey "legacy zero reset failed: "
                    (fun pubkey -> verify_legacy_zero_reset pubkey payload)
                in
                (match checked, payload.new_cipher with
                | Error e, _ -> Lwt.return (Error e)
                | Ok (), Some new_cipher ->
                  Lwt.return
                    (key_switch_value ~old_key_hash ~new_key_hash ~new_pubkey
                      ~new_cipher:(Some new_cipher) ~source_cipher:current_cipher)
                | Ok (), None ->
                  Lwt.return (error "key_switch_rejected" "legacy zero reset lost new cipher"))
              else if payload.legacy_commitment_migration then
                (match legacy_audit_commitment legacy_public_replay with
                | Error e -> Lwt.return (error "key_switch_rejected" e)
                | Ok commitment ->
                  let* checked =
                    verify_new_key new_pubkey "legacy commitment migration failed: "
                      (fun pubkey ->
                        verify_legacy_commitment_migration
                          pubkey
                          commitment
                          payload)
                  in
                  (match checked, payload.new_cipher with
                  | Error e, _ -> Lwt.return (Error e)
                  | Ok (), Some new_cipher ->
                    Lwt.return
                      (key_switch_value ~old_key_hash ~new_key_hash ~new_pubkey
                        ~new_cipher:(Some new_cipher) ~source_cipher:current_cipher)
                  | Ok (), None ->
                    Lwt.return (error "key_switch_rejected" "legacy commitment migration lost new cipher")))
              else if payload.legacy_ct_migration then
                Lwt.return
                  (error "key_switch_rejected"
                    "legacy ciphertext rebinding is not statement preserving; use public or commitment history migration")
              else if payload.legacy_public_migration then
                match legacy_public_replay_amount legacy_public_replay with
                | Error e -> Lwt.return (error "key_switch_rejected" e)
                | Ok amount ->
                  let* checked =
                    verify_new_key new_pubkey "legacy public migration failed: "
                      (fun pubkey ->
                        verify_legacy_public_migration pubkey amount payload)
                  in
                  (match checked, payload.new_cipher with
                  | Error e, _ -> Lwt.return (Error e)
                  | Ok (), Some new_cipher ->
                    Lwt.return
                      (key_switch_value ~old_key_hash ~new_key_hash ~new_pubkey
                        ~new_cipher:(Some new_cipher) ~source_cipher:current_cipher)
                  | Ok (), None ->
                    Lwt.return (error "key_switch_rejected" "legacy public migration lost new cipher"))
              else
                Lwt.return
                  (error
                    "key_switch_rejected"
                    (history_migration_reason migration_status))
            else if not migration_status.can_v3_migrate then
              Lwt.return (error "key_switch_rejected" migration_status.reason)
            else
              (match payload.source_cipher_hash with
              | None ->
                Lwt.return
                  (error "key_switch_rejected"
                    "encrypted balance migration requires source_cipher_hash")
              | Some source when
                  not (String.equal source
                    Digestif.SHA256.(digest_string current_cipher |> to_hex)) ->
                Lwt.return
                  (error "key_switch_rejected"
                    "encrypted balance changed before key switch verification")
              | Some _ ->
                match guard_refresh_source current_cipher with
                | Error e -> Lwt.return (Error e)
                | Ok () ->
                match old_pk, payload.new_cipher, payload.old_zero_proof, payload.new_zero_proof, payload.amount_commitment with
                | Some old_blob, Some new_cipher, Some old_zero_proof, Some new_zero_proof, Some amount_commitment ->
                  if not (FB.cipher_is_wrapped_scalar new_cipher) then
                    Lwt.return
                      (error "key_switch_rejected"
                        "new encrypted balance must be a wrapped scalar")
                  else
                    let* old_checked =
                      VW.verify_key_switch_claim_classified_with_priority
                        worker_priority
                        ~pubkey:old_blob
                        ~cipher:current_cipher
                        ~proof:old_zero_proof
                        ~commitment:amount_commitment
                    in
                    let* checked =
                      match old_checked with
                      | Error worker_failure ->
                        Lwt.return
                          (key_switch_worker_error
                             "old encrypted balance proof failed: "
                             worker_failure)
                      | Ok () ->
                        let* new_checked =
                          VW.verify_claim_classified_with_priority
                            worker_priority
                            ~pubkey:new_pubkey
                            ~cipher:new_cipher
                            ~proof:new_zero_proof
                            ~commitment:amount_commitment
                        in
                        begin
                          match new_checked with
                          | Error worker_failure ->
                            Lwt.return
                              (key_switch_worker_error
                                 "new encrypted balance proof failed: "
                                 worker_failure)
                          | Ok () ->
                            Lwt.return_ok ()
                        end
                    in
                    (match checked with
                    | Error e -> Lwt.return (Error e)
                    | Ok () ->
                      Lwt.return
                        (key_switch_value ~old_key_hash ~new_key_hash ~new_pubkey
                          ~new_cipher:(Some new_cipher) ~source_cipher:current_cipher))
                | _ ->
                  Lwt.return
                    (error "key_switch_rejected"
                      "encrypted balance migration requires new_cipher, old_zero_proof, new_zero_proof and amount_commitment"))
    end

let prepare_key_switch_plan_uncached ledger tx =
  let open Lwt.Syntax in
  match parse_key_switch tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok payload when payload.aes_kat <> PR.expected_kat () ->
    Lwt.return
      (error "key_switch_rejected" "AES KAT mismatch - incompatible key")
  | Ok payload ->
    let* new_pubkey_result =
      Proof_pool.run (fun () ->
        match PR.decode_b64 payload.new_pubkey_b64 with
        | Error e -> Error e
        | Ok raw -> PR.canonicalize_blob raw)
    in
    begin
      match new_pubkey_result with
      | Error e -> Lwt.return (error "key_switch_rejected" e)
      | Ok new_pubkey ->
        let* old_pk = Ledger.get_pvac_pubkey ledger tx.T.from in
        let old_key_hash =
          match old_pk with
          | Some blob -> PR.key_hash blob
          | None -> "none"
        in
        let new_key_hash = PR.key_hash new_pubkey in
        match current_cipher ledger tx.T.from "key_switch_rejected" with
        | Error e -> Lwt.return (Error e)
        | Ok current_cipher ->
          let* status =
            Proof_pool.run (fun () -> PM.status_of_cipher current_cipher)
          in
          let* history_required_result =
            Proof_pool.run
              (fun () -> history_migration_required status old_pk)
          in
          begin
            match history_required_result with
            | Error reason ->
              Lwt.return (error "key_switch_rejected" reason)
            | Ok history_required ->
          let prepared =
            if payload.legacy_zero_reset
              && not history_required
            then
              error
                "key_switch_rejected"
                "legacy zero reset requires a legacy hfhe balance"
            else if status.can_key_switch then
              Ok None
            else if history_required then
              if payload.legacy_zero_reset then
                Result.map Option.some (prepared_legacy_zero_reset payload)
              else if payload.legacy_commitment_migration then
                Result.map Option.some (prepared_legacy_commitment payload)
              else if payload.legacy_ct_migration then
                error
                  "key_switch_rejected"
                  "legacy ciphertext rebinding is not statement preserving; use public or commitment history migration"
              else if payload.legacy_public_migration then
                Result.map Option.some (prepared_legacy_public payload)
              else
                error
                  "key_switch_rejected"
                  (history_migration_reason status)
            else if not status.can_v3_migrate then
              error "key_switch_rejected" status.reason
            else
              Result.map Option.some
                (prepared_bound_migration old_pk current_cipher payload)
          in
          Lwt.return
            (Result.map
               (fun new_cipher ->
                 {
                   old_key_hash;
                   new_key_hash;
                   new_pubkey;
                   new_cipher;
                   source_cipher = current_cipher;
                 })
               prepared)
          end
    end

let prepare_key_switch_plan ledger tx =
  let open Lwt.Syntax in
  if key_switch_requests_legacy_audit tx then
    prepare_key_switch_plan_uncached ledger tx
  else
    let* key = key_switch_cache_key ledger tx in
    match Hashtbl.find_opt key_switch_cache key with
    | Some plan -> Lwt.return_ok plan
    | None -> prepare_key_switch_plan_uncached ledger tx

let key_switch_plan ?legacy_public_replay ledger tx =
  let open Lwt.Syntax in
  let* result = verify_key_switch_plan ?legacy_public_replay ledger tx in
  match result with
  | Error _ -> Lwt.return result
  | Ok _ when key_switch_requests_legacy_audit tx -> Lwt.return result
  | Ok plan ->
    let* key = key_switch_cache_key ledger tx in
    remember_key_switch_plan key plan;
    Lwt.return result

let preverify_key_switch_artifact
    ?(worker_priority = Compute_pool.Speculative)
    ledger
    tx =
  let open Lwt.Syntax in
  if key_switch_requests_legacy_audit tx then
    Lwt.return
      (error
         "key_switch_artifact_rejected"
         "legacy key switch requires history-bound verification")
  else
    let* snapshot_result = load_key_switch_snapshot ledger tx in
    match snapshot_result with
    | Error failure -> Lwt.return_error failure
    | Ok snapshot ->
      let artifact_source = {
        source_cipher = snapshot.snapshot_cipher;
        source_key_hash = key_hash_of_pubkey snapshot.snapshot_pubkey;
      } in
      let artifact_tx_hash = T.hash tx in
      let* result =
        verify_key_switch_plan ~snapshot ~worker_priority ledger tx
      in
      begin
        match result with
        | Ok artifact_plan ->
          Lwt.return_ok
            (Verified_key_switch { artifact_tx_hash; artifact_plan })
        | Error artifact_failure when
            key_switch_failure_retryable artifact_failure ->
          Lwt.return_error artifact_failure
        | Error artifact_failure ->
          Lwt.return_ok
            (Rejected_key_switch {
               artifact_tx_hash;
               artifact_source;
               artifact_failure;
             })
      end

let bind_key_switch_artifact ledger tx artifact =
  let open Lwt.Syntax in
  let artifact_tx_hash =
    match artifact with
    | Verified_key_switch verified -> verified.artifact_tx_hash
    | Rejected_key_switch rejected -> rejected.artifact_tx_hash
  in
  if not (String.equal artifact_tx_hash (T.hash tx)) then
    Lwt.return
      (Key_switch_artifact_invalid {
         tag = "key_switch_artifact_rejected";
         reason = "key switch artifact transaction mismatch";
         user_reason = "key switch artifact transaction mismatch";
       })
  else
    let* snapshot_result = load_key_switch_snapshot ledger tx in
    match snapshot_result with
    | Error _ -> Lwt.return Key_switch_source_changed
    | Ok snapshot ->
      let current_cipher = snapshot.snapshot_cipher in
      let current_key_hash = key_hash_of_pubkey snapshot.snapshot_pubkey in
      begin
        match artifact with
        | Verified_key_switch verified ->
          let plan = verified.artifact_plan in
          if
            String.equal current_cipher plan.source_cipher
            && String.equal current_key_hash plan.old_key_hash
          then begin
            let* cache_key = key_switch_cache_key ledger tx in
            remember_key_switch_plan cache_key plan;
            Lwt.return (Key_switch_bound (Prepared_key_switch plan))
          end
          else
            Lwt.return Key_switch_source_changed
        | Rejected_key_switch rejected ->
          if
            String.equal current_cipher rejected.artifact_source.source_cipher
            && String.equal
                 current_key_hash
                 rejected.artifact_source.source_key_hash
          then
            Lwt.return
              (Key_switch_artifact_invalid rejected.artifact_failure)
          else
            Lwt.return Key_switch_source_changed
      end

let key_switch_plan_for_apply ?legacy_public_replay ledger tx =
  let open Lwt.Syntax in
  if key_switch_requests_legacy_audit tx then
    key_switch_plan ?legacy_public_replay ledger tx
  else
    let* key = key_switch_cache_key ledger tx in
    match Hashtbl.find_opt key_switch_cache key with
    | Some plan -> Lwt.return_ok plan
    | None -> key_switch_plan ?legacy_public_replay ledger tx

let apply_key_switch_plan ledger tx (plan : key_switch_plan) =
  let open Lwt.Syntax in
  match current_cipher ledger tx.T.from "key_switch_rejected" with
    | Error failure ->
      Lwt.return (Key_switch_rejected { failure; consume_nonce = false })
    | Ok current when not (String.equal current plan.source_cipher) ->
      let failure = {
        tag = "key_switch_rejected";
        reason = "encrypted balance changed during key switch verification";
        user_reason = "encrypted balance changed during key switch verification";
      } in
      Lwt.return (Key_switch_rejected { failure; consume_nonce = false })
    | Ok _ ->
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

let apply_key_switch ?legacy_public_replay ledger tx =
  let open Lwt.Syntax in
  let* plan = key_switch_plan_for_apply ?legacy_public_replay ledger tx in
  match plan with
  | Error failure ->
    Lwt.return (Key_switch_rejected { failure; consume_nonce = false })
  | Ok plan -> apply_key_switch_plan ledger tx plan

let prepare_stealth_plan
    ?(worker_priority = Compute_pool.Required)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  match parse_stealth tx.T.encrypted_data with
  | Error e -> Lwt.return (Error e)
  | Ok ptd ->
    let* blob_result =
      load_pk_blob_plain ledger tx.T.from
        "no_pvac_pubkey"
        "sender has no registered PVAC pubkey"
    in
    match blob_result with
    | Error e -> Lwt.return (Error e)
    | Ok blob ->
      match current_cipher ledger tx.T.from "encrypted_balance_update_failed" with
      | Error e -> Lwt.return (Error e)
      | Ok _ when not (FB.cipher_is_wrapped_scalar ptd.PT.delta_cipher) ->
        Lwt.return
          (error "bad_delta_cipher"
            "stealth delta cipher must be a wrapped scalar")
      | Ok current ->
        match guard_private_input current "encrypted_balance_update_failed" "stealth" with
        | Error e -> Lwt.return (Error e)
        | Ok () ->
          Proof_pool.run ~priority:worker_priority (fun () ->
            with_loaded_pk blob "bad_pvac_pubkey" (fun pk ->
              match FB.decode_cipher ptd.PT.delta_cipher with
              | Error e ->
                error "bad_delta_cipher" ("cannot decode delta cipher: " ^ e)
              | Ok delta ->
                match
                  FB.withdraw_with_pubkey
                    ~result_policy
                    pk
                    ~current_cipher:(Some current)
                    ~delta_cipher:delta
                with
                | Error e ->
                  error
                    "encrypted_balance_update_failed"
                    ("sender encrypted balance update: " ^ e)
                | Ok next_cipher ->
                  Ok {
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
                  }))

let stealth_plan
    ?(worker_priority = Compute_pool.Required)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  let* plan =
    prepare_stealth_plan ~worker_priority ~result_policy ledger tx
  in
  match plan with
  | Error _ as result -> Lwt.return result
  | Ok plan ->
    match parse_stealth tx.T.encrypted_data with
    | Error e -> Lwt.return (Error e)
    | Ok ptd ->
      let* blob_result =
        load_pk_blob_plain ledger tx.T.from
          "no_pvac_pubkey"
          "sender has no registered PVAC pubkey"
      in
      begin
        match blob_result with
        | Error e -> Lwt.return (Error e)
        | Ok blob ->
          let* commitment =
            Proof_pool.run ~priority:worker_priority (fun () ->
              with_loaded_pk blob "bad_pvac_pubkey" (fun pk ->
                if
                  FB.verify_commitment
                    pk
                    ptd.PT.delta_cipher
                    ptd.PT.commitment
                then
                  Ok ()
                else
                  error
                    "bad_commitment"
                    "delta cipher commitment verification failed"))
          in
          begin
            match commitment with
            | Error _ as result -> Lwt.return result
            | Ok () -> Lwt.return_ok plan
          end
      end

let range_status prefix = function
  | Ok () -> Ok true
  | Error (VW.Proof_rejected _) -> Ok false
  | Error worker_failure ->
    private_worker_error
      "bad_range_proof"
      prefix
      prefix
      worker_failure

let stealth_inline_range
    ?(worker_priority = Compute_pool.Required)
    ledger
    tx
    plan =
  let open Lwt.Syntax in
  let* blob_result =
    load_pk_blob_plain ledger tx.T.from
      "no_pvac_pubkey"
      "sender has no registered PVAC pubkey"
  in
  match blob_result with
  | Error e -> Lwt.return (Error e)
  | Ok blob ->
    let* delta =
      VW.verify_range_classified_with_priority
        worker_priority
        ~pubkey:blob
        ~cipher:plan.stealth_delta_cipher
        ~proof:plan.stealth_range_proof_delta
    in
    begin
      match range_status "stealth delta range: " delta with
      | Error failure -> Lwt.return_error failure
      | Ok delta_ok ->
        let* balance =
          VW.verify_range_classified_with_priority
            worker_priority
            ~pubkey:blob
            ~cipher:plan.stealth_next_cipher
            ~proof:plan.stealth_range_proof_balance
        in
        begin
          match range_status "stealth balance range: " balance with
          | Error failure -> Lwt.return_error failure
          | Ok balance_ok -> Lwt.return_ok { delta_ok; balance_ok }
        end
    end

let stealth_accept_range range =
  if not range.delta_ok then
    error "bad_range_proof_delta" "delta range proof failed: delta must be non-negative"
  else if not range.balance_ok then
    error "bad_range_proof_balance" "balance range proof failed: sender may have insufficient balance"
  else
    Ok ()

let stealth_binding
    ?(worker_priority = Compute_pool.Required)
    ledger
    tx
    plan =
  let open Lwt.Syntax in
  match parse_stealth tx.T.encrypted_data with
    | Error e -> Lwt.return (Error e)
    | Ok ptd ->
      if String.length ptd.PT.send_zero_proof = 0 then
        Lwt.return
          (error "bad_send_binding"
            "send_zero_proof does not bind delta_cipher to amount_commitment: send_zero_proof required but missing")
      else
        let* blob_result =
          load_pk_blob_plain ledger tx.T.from
            "no_pvac_pubkey"
            "sender has no registered PVAC pubkey"
        in
        match blob_result with
        | Error e -> Lwt.return (Error e)
        | Ok blob ->
          let* verified =
            VW.verify_claim_classified_with_priority
              worker_priority
              ~pubkey:blob
              ~cipher:plan.stealth_delta_cipher
              ~proof:ptd.PT.send_zero_proof
              ~commitment:plan.stealth_amount_commitment
          in
          begin
            match verified with
            | Ok () -> Lwt.return_ok ()
            | Error (VW.Proof_rejected reason) ->
              Lwt.return
                (error "bad_send_binding"
                  ("send_zero_proof does not bind delta_cipher to amount_commitment: " ^ reason))
            | Error worker_failure ->
              Lwt.return
                (private_worker_error
                   "bad_send_binding"
                   "stealth binding: "
                   "stealth binding: "
                   worker_failure)
          end

let prepare_claim_plan ledger tx =
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
        | Ok () when not (FB.cipher_is_wrapped_scalar claim.SC.claim_cipher) ->
          Lwt.return
            (error "bad_claim_cipher"
              "claim cipher must be a wrapped scalar")
        | Ok () ->
          Lwt.return_ok {
            claim_output_id = claim.SC.output_id;
            claim_cipher = claim.SC.claim_cipher;
          }

let claim_plan
    ?(worker_priority = Compute_pool.Required)
    ledger
    tx =
  let open Lwt.Syntax in
  let* plan = prepare_claim_plan ledger tx in
  match plan with
  | Error _ as result -> Lwt.return result
  | Ok plan ->
    match parse_claim tx.T.encrypted_data with
    | Error e -> Lwt.return (Error e)
    | Ok claim ->
      let* so_opt = Ledger.get_stealth_output_by_id ledger claim.SC.output_id in
      begin
        match so_opt with
        | None ->
          Lwt.return
            (error "output_not_found"
              (Printf.sprintf "stealth output %d not found" claim.SC.output_id))
        | Some so ->
          let* blob_result =
            load_pk_blob_plain ledger tx.T.from
              "no_pvac_pubkey"
              "claimant has no registered PVAC pubkey"
          in
          begin
            match blob_result with
            | Error e -> Lwt.return (Error e)
            | Ok blob ->
              let* commitment =
                Proof_pool.run ~priority:worker_priority (fun () ->
                  with_loaded_pk blob "bad_pvac_pubkey" (fun pk ->
                    if
                      FB.verify_commitment
                        pk
                        claim.SC.claim_cipher
                        claim.SC.commitment
                    then
                      Ok ()
                    else
                      error
                        "bad_commitment"
                        "claim cipher commitment verification failed"))
              in
              begin
                match commitment with
                | Error _ as result -> Lwt.return result
                | Ok () ->
                  let* verified =
                    VW.verify_claim_classified_with_priority
                      worker_priority
                      ~pubkey:blob
                      ~cipher:claim.SC.claim_cipher
                      ~proof:claim.SC.zero_proof
                      ~commitment:so.Ledger_types.amount_commitment
                  in
                  begin
                    match verified with
                    | Error (VW.Proof_rejected reason) ->
                      Lwt.return
                        (error
                           "bad_zero_proof"
                           ("bound zero proof verification failed: " ^ reason))
                    | Error worker_failure ->
                      Lwt.return
                        (private_worker_error
                           "bad_zero_proof"
                           "claim proof: "
                           "claim proof: "
                           worker_failure)
                    | Ok () -> Lwt.return_ok plan
                  end
              end
          end
      end

let claim_balance_plan
    ?(worker_priority = Compute_pool.Required)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx
    plan =
  let open Lwt.Syntax in
  let* blob_result =
    load_pk_blob_plain ledger tx.T.from
      "no_pvac_pubkey"
      "claimant has no registered PVAC pubkey"
  in
  match blob_result with
  | Error e -> Lwt.return (Error e)
  | Ok blob ->
    match current_cipher ledger tx.T.from "encrypted_balance_update_failed" with
    | Error e -> Lwt.return (Error e)
    | Ok current ->
      match guard_private_input current "encrypted_balance_update_failed" "claim" with
      | Error e -> Lwt.return (Error e)
      | Ok () ->
        Proof_pool.run ~priority:worker_priority (fun () ->
          with_loaded_pk blob "bad_pvac_pubkey" (fun pk ->
            match FB.decode_cipher plan.claim_cipher with
            | Error e ->
              error "bad_claim_cipher" ("cannot decode claim cipher: " ^ e)
            | Ok delta ->
              match
                FB.deposit_with_pubkey
                  ~result_policy
                  pk
                  ~current_cipher:(Some current)
                  ~delta_cipher:delta
              with
              | Error e ->
                error "encrypted_balance_update_failed" ("claimant encrypted balance update: " ^ e)
              | Ok next_cipher ->
                Ok { current_cipher = current; next_cipher }))

let verify_private
    ?(worker_priority = Compute_pool.Required)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  match tx.T.op_type with
  | T.EncryptOp ->
    let* result =
      encrypt_plan ~worker_priority ~result_policy ledger tx
    in
    Lwt.return
      (result
       |> Result.map (fun plan -> Prepared_encrypt plan)
       |> Result.map_error private_reject)
  | T.DecryptOp ->
    let* result =
      decrypt_plan ~worker_priority ~result_policy ledger tx
    in
    Lwt.return
      (result
       |> Result.map (fun plan -> Prepared_decrypt plan)
       |> Result.map_error private_reject)
  | T.StealthOp ->
    let* plan =
      stealth_plan ~worker_priority ~result_policy ledger tx
    in
    begin
      match plan with
      | Error failure -> Lwt.return_error (private_reject failure)
      | Ok plan ->
        let* range =
          stealth_inline_range ~worker_priority ledger tx plan
        in
        begin
          match range with
          | Error failure -> Lwt.return_error (private_reject failure)
          | Ok range ->
            begin
              match stealth_accept_range range with
              | Error failure -> Lwt.return_error (private_reject failure)
              | Ok () ->
                let* binding =
                  stealth_binding ~worker_priority ledger tx plan
                in
                Lwt.return
                  (binding
                   |> Result.map (fun () -> Prepared_stealth plan)
                   |> Result.map_error private_reject)
            end
        end
    end
  | T.ClaimOp ->
    let* claim = claim_plan ~worker_priority ledger tx in
    begin
      match claim with
      | Error failure ->
        Lwt.return_error
          (private_reject ~preverify_reason:failure.tag failure)
      | Ok claim ->
        let* balance =
          claim_balance_plan
            ~worker_priority
            ~result_policy
            ledger
            tx
            claim
        in
        Lwt.return
          (balance
           |> Result.map (fun balance -> Prepared_claim (claim, balance))
           |> Result.map_error private_reject)
    end
  | _ ->
    let private_error = {
      tag = "private_artifact_rejected";
      reason = "private artifact operation is not supported";
      user_reason = "private artifact operation is not supported";
    } in
    Lwt.return_error (private_reject private_error)

let preverify_private_artifact
    ?(worker_priority = Compute_pool.Speculative)
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx =
  let open Lwt.Syntax in
  let* source = private_source ~result_policy ledger tx in
  match source with
  | Error failure -> Lwt.return_error failure
  | Ok private_source ->
    let* result =
      verify_private ~worker_priority ~result_policy ledger tx
    in
    begin
      match result with
      | Ok private_prepared ->
        Lwt.return_ok
          (Verified_private { private_source; private_prepared })
      | Error private_rejection
        when private_failure_retryable private_rejection.private_error ->
        Lwt.return_error private_rejection.private_error
      | Error private_rejection ->
        Lwt.return_ok
          (Rejected_private { private_source; private_rejection })
    end

let bind_private_artifact
    ?(result_policy = Private_result_policy.Recoverable)
    ledger
    tx
    artifact =
  let open Lwt.Syntax in
  let artifact_source =
    match artifact with
    | Verified_private verified -> verified.private_source
    | Rejected_private rejected -> rejected.private_source
  in
  let* current = private_source ~result_policy ledger tx in
  match current with
  | Error private_error ->
    Lwt.return
      (Private_artifact_invalid (private_reject private_error))
  | Ok current when not (private_source_equal artifact_source current) ->
    Lwt.return Private_source_changed
  | Ok _ ->
    begin
      match artifact with
      | Verified_private verified ->
        Lwt.return (Private_bound verified.private_prepared)
      | Rejected_private rejected ->
        Lwt.return
          (Private_artifact_invalid rejected.private_rejection)
    end