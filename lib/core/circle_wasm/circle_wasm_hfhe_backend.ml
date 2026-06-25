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


let error_json message =
  `Assoc [
    "ok", `Bool false;
    "error", `String message;
  ]

let value_json value =
  `Assoc [
    "ok", `Bool true;
    "value", value;
  ]

let require_string fields key =
  match List.assoc_opt key fields with
  | Some (`String value) ->
    Ok value
  | _ ->
    Error ("missing " ^ key)

let require_pubkey fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      try
        let raw = Base64.decode_exn value in
        Crypto.FheBalance.load_pubkey_result raw
      with e ->
        Error (Printexc.to_string e)
    end

let require_seckey fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      try
        let raw = Base64.decode_exn value in
        Ok (Pvac_ffi.deserialize_seckey (Bytes.of_string raw))
      with e ->
        Error (Printexc.to_string e)
    end

let require_amount fields key =
  match require_string fields key with
  | Error _ as e ->
    e
  | Ok value ->
    begin
      try
        Ok (Z.of_string value)
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
    begin
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
  require_blinding fields key

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
          match decode_cipher ciphertext with
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
          match decode_cipher lhs_ciphertext, decode_cipher rhs_ciphertext with
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
          match decode_cipher lhs_ciphertext, decode_cipher rhs_ciphertext with
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
          match decode_cipher ciphertext with
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
          match decode_cipher ciphertext with
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
          match decode_cipher ciphertext with
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
        begin
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
        begin
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
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_string fields "proof" with
      | Ok pk, Ok ciphertext, Ok proof ->
        value_json (`Bool (verify_zero pk ciphertext proof))
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "verify_range") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_string fields "proof" with
      | Ok pk, Ok ciphertext, Ok proof ->
        value_json (`Bool (verify_range pk ciphertext proof))
      | Error e, _, _
      | _, Error e, _
      | _, _, Error e ->
        error_json e
    end
  | Some (`String "verify_bound") ->
    begin
      match require_pubkey fields "pubkey_b64", require_string fields "ciphertext", require_string fields "proof", require_string fields "amount_commitment" with
      | Ok pk, Ok ciphertext, Ok proof, Ok amount_commitment ->
        begin
          match verify_claim_amount_v5 pk ciphertext proof amount_commitment with
          | Ok () -> value_json (`Bool true)
          | Error _ -> value_json (`Bool false)
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
    try
      match Yojson.Safe.from_string input_json with
      | `Assoc fields ->
        run_action fields
      | _ ->
        error_json "invalid payload"
    with e ->
      error_json (Printexc.to_string e)
  in
  Yojson.Safe.to_string output_json

let ensure_registered () = ()

let () =
  Callback.register "octra_circle_wasm_host_hfhe_call_json" call_json