(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module FB = Crypto.FheBalance
module P = Pvac_verify_protocol

let proof_pubkey blob =
  match FB.load_pubkey_result blob with
  | Error error -> Error error
  | Ok pubkey when not (FB.pubkey_supports_alias_rejection pubkey) ->
    Error "pvac pubkey proof circuit lacks alias rejection"
  | Ok pubkey -> Ok pubkey

let decode_commitment encoded =
  try
    let value = Base64.decode_exn encoded |> Bytes.of_string in
    if Bytes.length value = 32 then Ok value
    else Error "commitment must be 32 bytes"
  with _ ->
    Error "invalid commitment"

let decode_range_proof encoded =
  let prefix_len = FB.range_proof_prefix_len in
  if
    String.length encoded <= prefix_len
    || String.sub encoded 0 prefix_len <> FB.range_proof_prefix
  then
    Error "invalid range proof"
  else
    try
      let raw =
        String.sub encoded prefix_len (String.length encoded - prefix_len)
        |> Base64.decode_exn
        |> Bytes.of_string
      in
      Ok raw
    with _ ->
      Error "invalid range proof"

let verify_circle_proof pubkey cipher kind proof amount_commitment =
  match kind with
  | P.Circle_none ->
    if proof = "" && amount_commitment = "" then Ok ()
    else Error "unexpected circle proof fields"
  | P.Circle_zero ->
    begin
      match FB.decode_zero_proof proof with
      | Error _ -> Error "invalid zero proof"
      | Ok value ->
        if Pvac_ffi.verify_zero pubkey cipher value then Ok ()
        else Error "zero proof verification failed"
    end
  | P.Circle_bound_zero ->
    begin
      match FB.decode_zero_proof proof, decode_commitment amount_commitment with
      | Error _, _ -> Error "invalid bound zero proof"
      | _, Error error -> Error error
      | Ok value, Ok commitment ->
        if Pvac_ffi.verify_zero_bound pubkey cipher value commitment then Ok ()
        else Error "bound zero proof verification failed"
    end
  | P.Circle_range ->
    begin
      match decode_range_proof proof, decode_commitment amount_commitment with
      | Error error, _ -> Error error
      | _, Error error -> Error error
      | Ok value, Ok commitment ->
        if Pvac_ffi.verify_range_bound pubkey cipher value commitment then Ok ()
        else Error "bound range proof verification failed"
    end

let verify_circle_cell (value : P.circle_cell) =
  if not (Pvac_verify_policy.ciphertext_allowed value.cipher) then
    Error "ciphertext exceeds verifier resource policy"
  else if
    value.proof <> ""
    && not (Pvac_verify_policy.proof_allowed value.proof)
  then
    Error "proof exceeds verifier resource policy"
  else
    match
      proof_pubkey value.pubkey,
      FB.decode_cipher value.cipher,
      decode_commitment value.ciphertext_commitment
    with
    | Error error, _, _ -> Error error
    | _, Error error, _ -> Error error
    | _, _, Error error -> Error error
    | Ok pubkey, Ok cipher, Ok expected_commitment ->
      if not (Pvac_verify_policy.shape_allowed (Pvac_ffi.cipher_shape cipher)) then
        Error "ciphertext exceeds verifier resource policy"
      else
        let actual_commitment = Pvac_ffi.commit_ct pubkey cipher in
        if not (Bytes.equal actual_commitment expected_commitment) then
          Error "ciphertext commitment mismatch"
        else
          verify_circle_proof
            pubkey
            cipher
            value.proof_kind
            value.proof
            value.amount_commitment

let execute request =
  match request with
  | P.Ping ->
    Ok ()
  | P.Encrypt value ->
    begin
      match proof_pubkey value.pubkey with
      | Error error -> Error error
      | Ok pubkey ->
        FB.verify_encrypt_proof
          pubkey
          value.cipher
          value.amount
          value.proof
          value.commitment
          value.blinding
    end
  | P.Claim value ->
    begin
      match proof_pubkey value.pubkey with
      | Error error -> Error error
      | Ok pubkey ->
        FB.verify_claim_amount_v5
          pubkey
          value.cipher
          value.proof
          value.commitment
    end
  | P.Key_switch_claim value ->
    begin
      match proof_pubkey value.pubkey with
      | Error error -> Error error
      | Ok pubkey ->
        FB.verify_key_switch_claim_amount
          pubkey
          value.cipher
          value.proof
          value.commitment
    end
  | P.Range value ->
    begin
      match proof_pubkey value.pubkey with
      | Error error -> Error error
      | Ok pubkey ->
        if FB.verify_range pubkey value.cipher value.proof then Ok ()
        else Error "range proof verification failed"
    end
  | P.Zero value ->
    begin
      match proof_pubkey value.pubkey with
      | Error error -> Error error
      | Ok pubkey ->
        if FB.verify_zero pubkey value.cipher value.proof then Ok ()
        else Error "zero proof verification failed"
    end
  | P.Range_bound value ->
    begin
      match
        proof_pubkey value.pubkey,
        FB.decode_cipher value.cipher
      with
      | Error error, _ -> Error error
      | _, Error error -> Error error
      | Ok pubkey, Ok cipher ->
        begin
          try
            let prefix_len = FB.range_proof_prefix_len in
            if
              String.length value.proof <= prefix_len
              || String.sub value.proof 0 prefix_len
                 <> FB.range_proof_prefix
            then
              Error "invalid range proof"
            else
              let proof =
                String.sub
                  value.proof
                  prefix_len
                  (String.length value.proof - prefix_len)
                |> Base64.decode_exn
                |> Bytes.of_string
              in
              let commitment =
                Base64.decode_exn value.commitment
                |> Bytes.of_string
              in
              if Bytes.length commitment <> 32 then
                Error "amount commitment must be 32 bytes"
              else if
                Pvac_ffi.verify_range_bound
                  pubkey
                  cipher
                  proof
                  commitment
              then
                Ok ()
              else
                Error "bound range proof verification failed"
          with _ ->
            Error "invalid bound range proof"
        end
    end
  | P.Circle_cell value ->
    verify_circle_cell value

let response request =
  let hash = P.request_hash request in
  match execute request with
  | Ok () ->
    P.{ request_hash = hash; accepted = true; reason = "" }
  | Error reason ->
    P.{ request_hash = hash; accepted = false; reason }