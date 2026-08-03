(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let error_json message =
  `Assoc [
    "ok", `Bool false;
    "class", `String "rejected";
    "error", `String message;
  ]

let unavailable_json message =
  `Assoc [
    "ok", `Bool false;
    "class", `String "unavailable";
    "error", `String message;
  ]

let value_json value =
  `Assoc [
    "ok", `Bool true;
    "value", value;
  ]

module Hfhe_policy = Circle_wasm_hfhe_policy
module VW = Pvac_verify_worker

let verifier_lane = Mutex.create ()

let with_verifier_lane f =
  if not (Mutex.try_lock verifier_lane) then
    Error "hfhe verifier busy"
  else
    Fun.protect
      ~finally:(fun () -> Mutex.unlock verifier_lane)
      f

let verification_value = function
  | Ok () -> Ok true
  | Error (VW.Proof_rejected _) -> Ok false
  | Error failure -> Error (VW.verification_failure_message failure)

let require_string fields key =
  match List.assoc_opt key fields with
  | Some (`String value) ->
    Ok value
  | _ ->
    Error ("missing " ^ key)

let decode_b64_bounded
    ~encoded_allowed
    ~raw_allowed
    ~label
    value =
  if not (encoded_allowed value) then
    Error (label ^ " exceeds resource policy")
  else
    try
      let raw = Base64.decode_exn value in
      if raw_allowed raw then Ok raw
      else Error (label ^ " exceeds resource policy")
    with _ ->
      Error ("invalid " ^ label)

let require_pubkey fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      match
        decode_b64_bounded
          ~encoded_allowed:Hfhe_policy.pubkey_encoded_allowed
          ~raw_allowed:Hfhe_policy.pubkey_raw_allowed
          ~label:key
          value
      with
      | Error _ as e ->
        e
      | Ok raw ->
        Crypto.FheBalance.load_pubkey_result raw
    end

let require_seckey fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      match
        decode_b64_bounded
          ~encoded_allowed:Hfhe_policy.seckey_encoded_allowed
          ~raw_allowed:Hfhe_policy.seckey_raw_allowed
          ~label:key
          value
      with
      | Error _ as e ->
        e
      | Ok raw ->
        begin
          try
            Ok (Pvac_ffi.deserialize_seckey (Bytes.of_string raw))
          with e ->
            Error (Printexc.to_string e)
        end
    end

let require_amount fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      try
        let amount = Z.of_string value in
        if Z.sign amount < 0 || Z.gt amount Denomination.max_supply then
          Error ("invalid " ^ key)
        else
          Ok amount
      with _ ->
        Error ("invalid " ^ key)
    end

let require_int64 fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      try
        Ok (Int64.of_string value)
      with _ ->
        Error ("invalid " ^ key)
    end

let require_blinding fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    if not (Hfhe_policy.commitment_allowed value) then
      Error (key ^ " exceeds resource policy")
    else begin
      try
        let raw = Base64.decode_exn value in
        let bytes = Bytes.of_string raw in
        if Bytes.length bytes <> 32 then
          Error (key ^ " must decode to 32 bytes")
        else
          Ok bytes
      with e ->
        Error (Printexc.to_string e)
    end

let require_seed fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    if not (Hfhe_policy.seed_allowed value) then
      Error (key ^ " exceeds resource policy")
    else
      require_blinding fields key

let decode_cipher_bounded value =
  if not (Hfhe_policy.ciphertext_allowed value) then
    Error "ciphertext exceeds resource policy"
  else
    Crypto.FheBalance.decode_cipher value

let decode_verifier_cipher value =
  if not (Hfhe_policy.verifier_ciphertext_allowed value) then
    Error "ciphertext exceeds verifier resource policy"
  else match decode_cipher_bounded value with
  | Error _ as e ->
    e
  | Ok cipher ->
    begin
      try
        if Hfhe_policy.verifier_shape_allowed (Pvac_ffi.cipher_shape cipher) then
          Ok cipher
        else
          Error "ciphertext exceeds verifier resource policy"
      with _ ->
        Error "ciphertext exceeds verifier resource policy"
    end

let decode_zero_proof_bounded value =
  if not (Hfhe_policy.proof_allowed value) then
    Error "proof exceeds resource policy"
  else
    Crypto.FheBalance.decode_zero_proof value

let decode_range_proof_bounded value =
  let open Crypto.FheBalance in
  if not (Hfhe_policy.proof_allowed value) then
    Error "proof exceeds resource policy"
  else if String.length value <= range_proof_prefix_len
       || String.sub value 0 range_proof_prefix_len <> range_proof_prefix
  then
    Error "invalid range proof"
  else
    try
      let encoded =
        String.sub
          value
          range_proof_prefix_len
          (String.length value - range_proof_prefix_len) in
      Ok (Bytes.of_string (Base64.decode_exn encoded))
    with _ ->
      Error "invalid range proof"

let verify_zero_bounded pk ciphertext proof =
  match decode_verifier_cipher ciphertext, decode_zero_proof_bounded proof with
  | Ok cipher, Ok _ ->
    VW.verify_zero_sync_classified
      ~pubkey:(Bytes.to_string (Pvac_ffi.serialize_pubkey pk))
      ~cipher:(Crypto.FheBalance.encode_cipher cipher)
      ~proof
    |> verification_value
  | _ ->
    Ok false

let verify_range_bounded pk ciphertext proof amount_commitment =
  match
    decode_verifier_cipher ciphertext,
    decode_range_proof_bounded proof,
    require_blinding ["amount_commitment", `String amount_commitment] "amount_commitment"
  with
  | Ok cipher, Ok range_proof, Ok commitment ->
    VW.verify_range_bound_sync_classified
      ~pubkey:(Bytes.to_string (Pvac_ffi.serialize_pubkey pk))
      ~cipher:(Crypto.FheBalance.encode_cipher cipher)
      ~proof:
        (Crypto.FheBalance.range_proof_prefix
         ^ Base64.encode_exn (Bytes.to_string range_proof))
      ~commitment:(Base64.encode_exn (Bytes.to_string commitment))
    |> verification_value
  | _ ->
    Ok false

let verify_bound_bounded pk ciphertext proof amount_commitment =
  match
    decode_verifier_cipher ciphertext,
    decode_zero_proof_bounded proof,
    require_blinding ["amount_commitment", `String amount_commitment] "amount_commitment"
  with
  | Ok cipher, Ok _, Ok commitment ->
    VW.verify_claim_sync_classified
      ~pubkey:(Bytes.to_string (Pvac_ffi.serialize_pubkey pk))
      ~cipher:(Crypto.FheBalance.encode_cipher cipher)
      ~proof
      ~commitment:(Base64.encode_exn (Bytes.to_string commitment))
    |> verification_value
  | _ ->
    Ok false

let run_action fields =
  let open Crypto.FheBalance in
  match List.assoc_opt "action" fields with
  | Some (`String "encrypt_value_seeded") ->
    begin
      match require_pubkey fields "pubkey_b64", require_seckey fields "seckey_b64", require_int64 fields "amount", require_seed fields "seed_b64" with
      | Ok pk, Ok sk, Ok amount, Ok seed ->
        value_json (`String (encode_cipher (Pvac_ffi.enc_value_seeded pk sk amount seed)))
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _
      | _, _, _, Error e ->
        error_json e
    end
  | Some (`String "encrypt_zero_seeded") ->
    begin
      match require_pubkey fields "pubkey_b64", require_seckey fields "seckey_b64", require_seed fields "seed_b64" with
      | Ok pk, Ok sk, Ok seed ->
        value_json (`String (encode_cipher (Pvac_ffi.enc_zero_seeded pk sk seed)))
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "decrypt_value") ->
    begin
      match require_pubkey fields "pubkey_b64", require_seckey fields "seckey_b64", require_string fields "ciphertext" with
      | Ok pk, Ok sk, Ok ciphertext ->
        begin
          match decode_cipher_bounded ciphertext with
          | Ok ct ->
            value_json (`String (Int64.to_string (Pvac_ffi.dec_value pk sk ct)))
          | Error e ->
            error_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "cipher_add") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "lhs_ciphertext", require_string fields "rhs_ciphertext" with
      | Ok pk, Ok lhs_ciphertext, Ok rhs_ciphertext ->
        begin
          match decode_cipher_bounded lhs_ciphertext, decode_cipher_bounded rhs_ciphertext with
          | Ok lhs, Ok rhs ->
            value_json (`String (encode_cipher (Pvac_ffi.ct_add pk lhs rhs)))
          | Error e, _
          | _, Error e ->
            error_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "cipher_sub") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "lhs_ciphertext", require_string fields "rhs_ciphertext" with
      | Ok pk, Ok lhs_ciphertext, Ok rhs_ciphertext ->
        begin
          match decode_cipher_bounded lhs_ciphertext, decode_cipher_bounded rhs_ciphertext with
          | Ok lhs, Ok rhs ->
            value_json (`String (encode_cipher (Pvac_ffi.ct_sub pk lhs rhs)))
          | Error e, _
          | _, Error e ->
            error_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "cipher_scale") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_int64 fields "factor" with
      | Ok pk, Ok ciphertext, Ok factor ->
        begin
          match decode_cipher_bounded ciphertext with
          | Ok ct ->
            value_json (`String (encode_cipher (Pvac_ffi.ct_scale pk ct factor)))
          | Error e ->
            error_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "cipher_add_const") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_int64 fields "amount" with
      | Ok pk, Ok ciphertext, Ok amount ->
        begin
          match decode_cipher_bounded ciphertext with
          | Ok ct ->
            value_json (`String (encode_cipher (Pvac_ffi.ct_add_const pk ct amount 0L)))
          | Error e ->
            error_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "cipher_sub_const") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_int64 fields "amount" with
      | Ok pk, Ok ciphertext, Ok amount ->
        begin
          match decode_cipher_bounded ciphertext with
          | Ok ct ->
            value_json (`String (encode_cipher (Pvac_ffi.ct_sub_const pk ct amount)))
          | Error e ->
            error_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "pedersen_commit") ->
    begin
      match require_int64 fields "amount", require_blinding fields "blinding_b64" with
      | Ok amount, Ok blinding ->
        let commitment = compute_amount_commitment amount blinding in
        value_json (`String (Base64.encode_exn (Bytes.to_string commitment)))
      | Error e, _
      | _, Error e ->
        error_json e
    end
  | Some (`String "commit_cipher") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext" with
      | Ok pk, Ok ciphertext ->
        if not (Hfhe_policy.ciphertext_allowed ciphertext) then
          error_json "ciphertext exceeds resource policy"
        else begin
          match commit pk ciphertext with
          | Some value -> value_json (`String value)
          | None -> error_json "commit failed"
        end
      | Error e, _
      | _, Error e ->
        error_json e
    end
  | Some (`String "bound_commitment") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_amount fields "amount" with
      | Ok pk, Ok ciphertext, Ok amount ->
        if not (Hfhe_policy.ciphertext_allowed ciphertext) then
          error_json "ciphertext exceeds resource policy"
        else begin
          match compute_bound_commitment pk ciphertext amount with
          | Some value -> value_json (`String value)
          | None -> error_json "bound commitment failed"
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "verify_zero") ->
    begin
      match
        require_pubkey fields "pubkey_b64",
        require_string fields "ciphertext",
        require_string fields "proof"
      with
      | Ok pk, Ok ciphertext, Ok proof ->
        begin
          match
            with_verifier_lane
              (fun () -> verify_zero_bounded pk ciphertext proof)
          with
          | Ok value -> value_json (`Bool value)
          | Error e -> unavailable_json e
        end
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "verify_range") ->
    begin
      match
        require_pubkey fields "pubkey_b64",
        require_string fields "ciphertext",
        require_string fields "proof",
        require_string fields "amount_commitment"
      with
      | Ok pk, Ok ciphertext, Ok proof, Ok amount_commitment ->
        begin
          match
            with_verifier_lane
              (fun () ->
                verify_range_bounded
                  pk
                  ciphertext
                  proof
                  amount_commitment)
          with
          | Ok value -> value_json (`Bool value)
          | Error e -> unavailable_json e
        end
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _
      | _, _, _, Error e ->
        error_json e
    end
  | Some (`String "verify_bound") ->
    begin
      match
        require_pubkey fields "pubkey_b64",
        require_string fields "ciphertext",
        require_string fields "proof",
        require_string fields "amount_commitment"
      with
      | Ok pk, Ok ciphertext, Ok proof, Ok amount_commitment ->
        begin
          match
            with_verifier_lane
              (fun () ->
                verify_bound_bounded
                  pk
                  ciphertext
                  proof
                  amount_commitment)
          with
          | Ok value -> value_json (`Bool value)
          | Error e -> unavailable_json e
        end
      | Error e, _, _, _
      | _, Error e, _, _
      | _, _, Error e, _
      | _, _, _, Error e ->
        error_json e
    end
  | Some (`String action) ->
    error_json ("unsupported action: " ^ action)
  | _ ->
    error_json "missing action"

let call_json input_json =
  let output_json =
    if not (Hfhe_policy.request_allowed input_json) then
      error_json "hfhe request exceeds resource policy"
    else try
      match Yojson.Safe.from_string input_json with
      | `Assoc fields ->
        run_action fields
      | _ ->
        error_json "invalid payload"
    with _ ->
      unavailable_json "hfhe backend exception"
  in
  Yojson.Safe.to_string output_json

let ensure_registered () = ()

let () =
  Callback.register "octra_circle_wasm_host_hfhe_call_json" call_json