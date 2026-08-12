(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let z_to_yojson z = `String (Z.to_string z)
let z_of_yojson = function
  | `String s -> Ok (Z.of_string s)
  | _ -> Error "Invalid Z JSON"

module Base58 = struct
  let alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let encode input =
    let int_of_bytes bytes =
      Bytes.fold_left
        (fun acc byte -> Z.(shift_left acc 8 + of_int (Char.code byte)))
        Z.zero bytes
    in
    let input_bytes = Bytes.of_string input in
    let n = int_of_bytes input_bytes in
    let rec convert acc n =
      if Z.equal n Z.zero then acc
      else
        let q, r = Z.ediv_rem n (Z.of_int 58) in
        convert (alphabet.[Z.to_int r] :: acc) q
    in
    let count_leading_zeros s =
      let rec aux i =
        if i < String.length s && s.[i] = '\x00' then aux (i + 1) else i
      in
      aux 0
    in
    let prefix_zeros = count_leading_zeros input in
    let base58_str = convert [] n |> List.to_seq |> String.of_seq in
    (String.make prefix_zeros '1') ^ base58_str
end

module Address = struct
  let is_valid_address addr =
    if not (String.starts_with ~prefix:"oct" addr) then false
    else if String.length addr <> 47 then false
    else
      let base58_part = String.sub addr 3 (String.length addr - 3) in
      let is_valid_base58_char c =
        let valid_chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz" in
        String.contains valid_chars c
      in
      String.for_all is_valid_base58_char base58_part

  let address_from_pubkey pub_b64 =
    try
      let pub_raw = Base64.decode_exn pub_b64 in
      let hash = Digestif.SHA256.digest_string pub_raw in
      let base58_hash = Base58.encode (Digestif.SHA256.to_raw_string hash) in
      let padded = if String.length base58_hash < 44 then
        String.make (44 - String.length base58_hash) '1' ^ base58_hash
      else base58_hash in
      "oct" ^ padded
    with _ -> ""

  let verify_address_pubkey addr pubkey =
    let expected = address_from_pubkey pubkey in
    addr = expected
end

module WalletKey = struct
  let verify_privkey_for_address addr priv_b64 =
    try
      let priv = Base64.decode_exn priv_b64 in
      match Mirage_crypto_ec.Ed25519.priv_of_octets priv with
      | Error _ -> false
      | Ok sk ->
        let pk = Mirage_crypto_ec.Ed25519.pub_of_priv sk in
        let pub_b64 = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets pk) in
        Address.address_from_pubkey pub_b64 = addr
    with _ -> false
end

module FheBalance = struct
  let prefix = "hfhe_v1|"
  let prefix_len = 8

  let pvac_default_params : Pvac_ffi.params Lazy.t = lazy (
    Pvac_ffi.default_params ()
  )

  let aes_kat_hex_cache : string Lazy.t = lazy (
    let kat_bytes = Pvac_ffi.aes_kat () in
    let hex_buf = Buffer.create 32 in
    Bytes.iter (fun byte ->
      Buffer.add_string hex_buf (Printf.sprintf "%02x" (Char.code byte))
    ) kat_bytes;
    Buffer.contents hex_buf
  )

  let aes_kat_hex () = Lazy.force aes_kat_hex_cache

  let derive_pvac_keys priv_b64 =
    let priv_raw = Base64.decode_exn priv_b64 in
    let wallet_bytes = Bytes.of_string priv_raw in
    let params = Lazy.force pvac_default_params in
    Pvac_ffi.keygen_from_seed params wallet_bytes

  let is_fhe_cipher s =
    String.length s > prefix_len && String.sub s 0 prefix_len = prefix

  let encode_cipher ct =
    let blob = Pvac_ffi.serialize_cipher ct in
    prefix ^ Base64.encode_exn (Bytes.to_string blob)

  let encode_cipher_public ct =
    let blob = Pvac_ffi.serialize_cipher_public ct in
    prefix ^ Base64.encode_exn (Bytes.to_string blob)

  let decode_cipher s =
    if not (is_fhe_cipher s) then Error "not FHE cipher"
    else try
      let b64 = String.sub s prefix_len (String.length s - prefix_len) in
      let raw = Base64.decode_exn b64 in
      Ok (Pvac_ffi.deserialize_cipher (Bytes.of_string raw))
    with e -> Error (Printexc.to_string e)

  let public_cipher s =
    if s = "0" || s = "" || not (is_fhe_cipher s) then s
    else
      match decode_cipher s with
      | Ok ct -> encode_cipher_public ct
      | Error _ -> "0"

  let cipher_has_key_bound_material s =
    match decode_cipher s with
    | Error e -> Error e
    | Ok ct -> Ok (Pvac_ffi.cipher_has_key_bound_material ct)

  let cipher_is_key_bound_extension legacy_s bound_s =
    match decode_cipher legacy_s, decode_cipher bound_s with
    | Ok legacy, Ok bound -> Ok (Pvac_ffi.cipher_is_key_bound_extension legacy bound)
    | Error e, _ | _, Error e -> Error e

  let make_seed ~tx_hash ~epoch_id ~purpose =
    let buf = Printf.sprintf "OCTRA_FHE_SEED_V1|%s|%d|%s" tx_hash epoch_id purpose in
    let hash = Digestif.SHA256.(digest_string buf |> to_raw_string) in
    Bytes.of_string hash

  let enc_amount pk sk amount ~tx_hash ~epoch_id ~purpose =
    let seed = make_seed ~tx_hash ~epoch_id ~purpose in
    Pvac_ffi.enc_value_seeded pk sk (Int64.of_string (Z.to_string amount)) seed

  let enc_zero pk sk ~tx_hash ~epoch_id =
    let seed = make_seed ~tx_hash ~epoch_id ~purpose:"zero" in
    Pvac_ffi.enc_zero_seeded pk sk seed

  let dec_amount pk sk ct =
    let v = Pvac_ffi.dec_value pk sk ct in
    Z.of_int64 v

  let deposit pk sk ~current_cipher ~amount ~tx_hash ~epoch_id =
    let delta = enc_amount pk sk amount ~tx_hash ~epoch_id ~purpose:"deposit" in
    match current_cipher with
    | None | Some "0" | Some "" ->
      Ok (encode_cipher delta)
    | Some s when not (is_fhe_cipher s) ->
      Ok (encode_cipher delta)
    | Some s ->
      (match decode_cipher s with
       | Error e -> Error ("decode current: " ^ e)
       | Ok curr ->
         let result = Pvac_ffi.ct_add pk curr delta in
         Ok (encode_cipher result))

  let withdraw pk sk ~current_cipher ~amount ~tx_hash ~epoch_id =
    match current_cipher with
    | None | Some "0" | Some "" -> Error "no encrypted balance"
    | Some s ->
      (match decode_cipher s with
       | Error e -> Error ("decode current: " ^ e)
       | Ok curr ->
         let delta = enc_amount pk sk amount ~tx_hash ~epoch_id ~purpose:"withdraw" in
         let result = Pvac_ffi.ct_sub pk curr delta in
         Ok (encode_cipher result))

  let get_balance pk sk cipher_str =
    if cipher_str = "0" || cipher_str = "" then Ok Z.zero
    else match decode_cipher cipher_str with
    | Error e -> Error e
    | Ok ct -> Ok (dec_amount pk sk ct)

  let load_pubkey blob =
    try Pvac_ffi.deserialize_pubkey (Bytes.of_string blob)
    with e ->
      failwith (Printf.sprintf "load_pubkey failed: %s" (Printexc.to_string e))

  let load_pubkey_result blob =
    try Ok (Pvac_ffi.deserialize_pubkey (Bytes.of_string blob))
    with e -> Error (Printf.sprintf "load_pubkey failed: %s" (Printexc.to_string e))

  let pubkey_supports_alias_rejection pk =
    Pvac_ffi.pubkey_supports_alias_rejection pk

  let blob_supports_alias_rejection blob =
    Result.map pubkey_supports_alias_rejection (load_pubkey_result blob)

  let cipher_base_layers cipher_str =
    if cipher_str = "0" || cipher_str = "" then Ok 0
    else
      match decode_cipher cipher_str with
      | Error e -> Error e
      | Ok cipher ->
        (try Ok (Pvac_ffi.cipher_base_layers cipher)
         with e -> Error (Printexc.to_string e))

  let cipher_is_wrapped_scalar cipher_str =
    match decode_cipher cipher_str with
    | Error _ -> false
    | Ok cipher -> Pvac_ffi.cipher_is_wrapped_scalar cipher

  let private_input_base_limit = 6
  let refresh_source_base_limit = 8

  let encode_private_result result_policy cipher =
    match result_policy with
    | Private_result_policy.Legacy -> Ok (encode_cipher cipher)
    | Private_result_policy.Recoverable ->
      try
        let layers = Pvac_ffi.cipher_base_layers cipher in
        if layers > private_input_base_limit then
          Error
            (Printf.sprintf
              "encrypted balance result exceeds recoverable limit (%d base layers)"
              layers)
        else
          Ok (encode_cipher cipher)
      with e ->
        Error ("encrypted balance result shape failed: " ^ Printexc.to_string e)

  let check_private_input cipher_str =
    match cipher_base_layers cipher_str with
    | Error e -> Error e
    | Ok layers when layers > private_input_base_limit ->
      Error
        (Printf.sprintf
          "encrypted balance compact refresh required (%d base layers)"
          layers)
    | Ok _ -> Ok ()

  let check_refresh_source cipher_str =
    match cipher_base_layers cipher_str with
    | Error e -> Error e
    | Ok layers when layers > refresh_source_base_limit ->
      Error
        (Printf.sprintf
          "encrypted balance refresh source exceeds %d base layers"
          refresh_source_base_limit)
    | Ok _ -> Ok ()

  let pubkey_is_key_bound_extension legacy_blob bound_blob =
    match load_pubkey_result legacy_blob, load_pubkey_result bound_blob with
    | Ok legacy, Ok bound -> Ok (Pvac_ffi.pubkey_is_key_bound_extension legacy bound)
    | Error e, _ | _, Error e -> Error e

  let deposit_with_pubkey
      ?(result_policy = Private_result_policy.Recoverable)
      pk
      ~current_cipher
      ~delta_cipher =
    match current_cipher with
    | None | Some "0" | Some "" ->
      encode_private_result result_policy delta_cipher
    | Some s when not (is_fhe_cipher s) ->
      Error (Printf.sprintf "legacy cipher format (not hfhe_v1): %s"
               (if String.length s > 20 then String.sub s 0 20 ^ "..." else s))
    | Some s ->
      (match decode_cipher s with
       | Error e -> Error ("decode current: " ^ e)
       | Ok curr ->
         encode_private_result
           result_policy
           (Pvac_ffi.ct_add pk curr delta_cipher))

  let withdraw_with_pubkey
      ?(result_policy = Private_result_policy.Recoverable)
      pk
      ~current_cipher
      ~delta_cipher =
    match current_cipher with
    | None | Some "0" | Some "" -> Error "no encrypted balance"
    | Some s when not (is_fhe_cipher s) ->
      Error (Printf.sprintf "legacy cipher format (not hfhe_v1): %s"
               (if String.length s > 20 then String.sub s 0 20 ^ "..." else s))
    | Some s ->
      (match decode_cipher s with
       | Error e -> Error ("decode current: " ^ e)
       | Ok curr ->
         encode_private_result
           result_policy
           (Pvac_ffi.ct_sub pk curr delta_cipher))

  let verify_commitment pk cipher_str expected_b64 =
    match decode_cipher cipher_str with
    | Error _ -> false
    | Ok ct ->
      let hash = Pvac_ffi.commit_ct pk ct in
      let actual = Base64.encode_exn (Bytes.to_string hash) in
      String.equal actual expected_b64

  let compute_bound_commitment pk cipher_str amount =
    match decode_cipher cipher_str with
    | Error _ -> None
    | Ok ct ->
      let ct_hash = Pvac_ffi.commit_ct pk ct in
      let amount_i64 = Z.to_int64 amount in
      let amount_le = Bytes.create 8 in
      Bytes.set_int64_le amount_le 0 amount_i64;
      let buf = Bytes.to_string ct_hash ^ Bytes.to_string amount_le in
      let bound = Digestif.SHA256.(digest_string buf |> to_raw_string) in
      Some (Base64.encode_exn bound)

  let verify_bound_commitment pk cipher_str amount expected_b64 =
    match compute_bound_commitment pk cipher_str amount with
    | None -> false
    | Some actual -> String.equal actual expected_b64

  let cipher_valid s =
    s = "0" || s = "" || is_fhe_cipher s

  let commit pk cipher_str =
    match decode_cipher cipher_str with
    | Error _ -> None
    | Ok ct ->
      let hash = Pvac_ffi.commit_ct pk ct in
      Some (Base64.encode_exn (Bytes.to_string hash))

  let range_proof_prefix = "rp_v1|"
  let range_proof_prefix_len = 6

  let encode_range_proof rp =
    let blob = Pvac_ffi.serialize_range_proof rp in
    range_proof_prefix ^ Base64.encode_exn (Bytes.to_string blob)

  let decode_range_proof s =
    if String.length s <= range_proof_prefix_len
       || String.sub s 0 range_proof_prefix_len <> range_proof_prefix
    then Error "not a range proof"
    else try
      let b64 = String.sub s range_proof_prefix_len (String.length s - range_proof_prefix_len) in
      let raw = Base64.decode_exn b64 in
      Ok (Pvac_ffi.deserialize_range_proof (Bytes.of_string raw))
    with e -> Error (Printexc.to_string e)

  let zero_proof_prefix = "zkzp_v2|"
  let zero_proof_prefix_len = 8

  let encode_zero_proof zp =
    let blob = Pvac_ffi.serialize_zero_proof zp in
    zero_proof_prefix ^ Base64.encode_exn (Bytes.to_string blob)

  let decode_zero_proof s =
    if String.length s <= zero_proof_prefix_len
       || String.sub s 0 zero_proof_prefix_len <> zero_proof_prefix
    then Error "not a zero proof"
    else try
      let b64 = String.sub s zero_proof_prefix_len (String.length s - zero_proof_prefix_len) in
      let raw = Base64.decode_exn b64 in
      if String.length raw < 50 then Error "zero proof too small to be valid"
      else Ok (Pvac_ffi.deserialize_zero_proof (Bytes.of_string raw))
    with e -> Error (Printexc.to_string e)

  let verify_zero pk cipher_str zero_proof_str =
    match decode_cipher cipher_str, decode_zero_proof zero_proof_str with
    | Ok ct, Ok zp -> Pvac_ffi.verify_zero pk ct zp
    | _ -> false

  let encode_agg_range_proof arp =
    let blob = Pvac_ffi.serialize_agg_range_proof arp in
    range_proof_prefix ^ Base64.encode_exn (Bytes.to_string blob)

  let encode_bound_range_proof proof =
    let blob = Pvac_ffi.serialize_bound_range_proof proof in
    range_proof_prefix ^ Base64.encode_exn (Bytes.to_string blob)

  let verify_range_any pk cipher_str range_proof_str =
    if String.length range_proof_str <= range_proof_prefix_len
       || String.sub range_proof_str 0 range_proof_prefix_len <> range_proof_prefix
    then false
    else try
      let b64 = String.sub range_proof_str range_proof_prefix_len
                  (String.length range_proof_str - range_proof_prefix_len) in
      let raw = Base64.decode_exn b64 in
      match decode_cipher cipher_str with
      | Ok ct -> Pvac_ffi.verify_range_any pk ct (Bytes.of_string raw)
      | Error _ -> false
    with _ -> false

  let verify_range pk cipher_str range_proof_str =
    verify_range_any pk cipher_str range_proof_str

  let ct_sub_encoded pk cipher_a_str cipher_b_str =
    match decode_cipher cipher_a_str, decode_cipher cipher_b_str with
    | Ok a, Ok b -> Ok (encode_cipher (Pvac_ffi.ct_sub pk a b))
    | Error e, _ | _, Error e -> Error e

  let verify_claim_amount_with verifier pk claim_cipher_str zero_proof_str amount_commitment_b64 =
    match decode_cipher claim_cipher_str, decode_zero_proof zero_proof_str with
    | Ok ct, Ok zp ->
      (try
         let commitment_bytes = Bytes.of_string (Base64.decode_exn amount_commitment_b64) in
         if Bytes.length commitment_bytes <> 32 then
           Error "amount_commitment must be 32 bytes"
         else if verifier pk ct zp commitment_bytes then Ok ()
         else Error "bound zero proof verification failed"
       with e -> Error ("bad amount_commitment: " ^ Printexc.to_string e))
    | Error e, _ -> Error ("bad claim cipher: " ^ e)
    | _, Error e -> Error ("bad zero proof: " ^ e)

  let verify_claim_amount_v5 =
    verify_claim_amount_with Pvac_ffi.verify_zero_bound

  let verify_key_switch_claim_amount =
    verify_claim_amount_with Pvac_ffi.verify_zero_bound_key_switch

  let verify_encrypt_proof pk cipher_str amount zero_proof_str amount_commitment_b64 blinding_b64 =
    try
      if Z.sign amount < 0 || Z.compare amount (Z.of_string "9223372036854775807") > 0 then
        Error "amount out of int64 range"
      else
      let blinding = Bytes.of_string (Base64.decode_exn blinding_b64) in
      if Bytes.length blinding <> 32 then
        Error "blinding must be 32 bytes"
      else
        let expected_commitment = Pvac_ffi.pedersen_commit_amount (Z.to_int64 amount) blinding in
        let actual_commitment = Bytes.of_string (Base64.decode_exn amount_commitment_b64) in
        if Bytes.length actual_commitment <> 32 then
          Error "amount_commitment must be 32 bytes"
        else if not (Bytes.equal expected_commitment actual_commitment) then
          Error "amount commitment mismatch: pedersen_commit(amount, blinding) != amount_commitment"
        else
          match decode_cipher cipher_str, decode_zero_proof zero_proof_str with
          | Ok ct, Ok zp ->
            if not (Pvac_ffi.cipher_is_wrapped_scalar ct) then
              Error "amount cipher must be a wrapped scalar"
            else if Pvac_ffi.verify_zero_bound pk ct zp actual_commitment then Ok ()
            else Error "bound zero proof verification failed"
          | Error e, _ -> Error ("bad cipher: " ^ e)
          | _, Error e -> Error ("bad zero proof: " ^ e)
    with e -> Error ("verify_encrypt_proof: " ^ Printexc.to_string e)

  let compute_amount_commitment (amount : int64) (blinding : bytes) =
    Pvac_ffi.pedersen_commit_amount amount blinding
end

module PrivateTransferV2 = struct
  type t = {
    version : int;
    delta_cipher : string;
    commitment : string;
    amount : Z.t;
  }

  let to_json t =
    `Assoc [
      "version", `Int t.version;
      "delta_cipher", `String t.delta_cipher;
      "commitment", `String t.commitment;
      "amount", `String (Z.to_string t.amount);
    ]

  let of_json = function
    | `Assoc fields ->
      let get k = List.assoc_opt k fields in
      let int_k k = match get k with Some (`Int i) -> Ok i | _ -> Error ("missing " ^ k) in
      let str_k k = match get k with Some (`String s) -> Ok s | _ -> Error ("missing " ^ k) in
      let amount_of = function
        | Some (`String s) -> (try Ok (Z.of_string s) with _ -> Error "invalid amount")
        | Some (`Int n) -> Ok (Z.of_int n)
        | _ -> Error "missing amount"
      in
      (match int_k "version", str_k "delta_cipher", str_k "commitment", amount_of (get "amount") with
       | Ok version, Ok delta_cipher, Ok commitment, Ok amount ->
         Ok { version; delta_cipher; commitment; amount }
       | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e ->
         Error ("PrivateTransferV2: " ^ e))
    | _ -> Error "PrivateTransferV2: expected object"
end

module StealthAddress = struct
  let stealth_tag_domain = "OCTRA_STEALTH_TAG_V1"

  let ed25519_pub_to_x25519 ed25519_pub =
    if String.length ed25519_pub <> 32 then
      Error "Ed25519 pubkey must be 32 bytes"
    else
      let p = Z.(sub (shift_left one 255) (of_int 19)) in

      let y_bytes = Bytes.of_string ed25519_pub in
      let b31 = Char.code (Bytes.get y_bytes 31) in
      Bytes.set y_bytes 31 (Char.chr (b31 land 0x7F));

      let y = ref Z.zero in
      for i = 31 downto 0 do
        y := Z.(add (shift_left !y 8) (of_int (Char.code (Bytes.get y_bytes i))))
      done;
      let y_val = Z.erem !y p in

      let one_plus_y = Z.erem (Z.add Z.one y_val) p in
      let one_minus_y = Z.erem (Z.sub (Z.add p Z.one) y_val) p in

      let inv = Z.powm one_minus_y Z.(sub p (of_int 2)) p in
      let u = Z.erem (Z.mul one_plus_y inv) p in

      let result = Bytes.create 32 in
      let u_ref = ref u in
      for i = 0 to 31 do
        Bytes.set result i (Char.chr (Z.to_int (Z.logand !u_ref (Z.of_int 0xFF))));
        u_ref := Z.shift_right !u_ref 8
      done;
      Ok (Bytes.to_string result)

  let ed25519_sk_to_x25519 ed25519_sk =
    if String.length ed25519_sk <> 32 then
      Error "Ed25519 secret key must be 32 bytes"
    else
      let h = Digestif.SHA512.(digest_string ed25519_sk |> to_raw_string) in
      let s = Bytes.of_string (String.sub h 0 32) in

      Bytes.set s 0 (Char.chr (Char.code (Bytes.get s 0) land 248));
      Bytes.set s 31 (Char.chr ((Char.code (Bytes.get s 31) land 127) lor 64));
      Ok (Bytes.to_string s)

  let ed25519_pub_to_view_pubkey ed25519_pub_b64 =
    let pub_raw = Base64.decode_exn ed25519_pub_b64 in
    match ed25519_pub_to_x25519 pub_raw with
    | Ok x -> Some (Base64.encode_exn x)
    | Error _ -> None

  let derive_view_keypair priv_b64 =
    let priv_raw = Base64.decode_exn priv_b64 in
    match ed25519_sk_to_x25519 priv_raw with
    | Error e -> failwith ("X25519 view key derivation failed: " ^ e)
    | Ok view_sk ->
      match Mirage_crypto_ec.X25519.secret_of_octets view_sk with
      | Ok (_secret, pub_str) -> (view_sk, pub_str)
      | Error _ -> failwith "X25519 secret_of_octets failed"

  let generate_ephemeral () =
    let (secret, pub_str) = Mirage_crypto_ec.X25519.gen_key () in
    (Mirage_crypto_ec.X25519.secret_to_octets secret, pub_str)

  let ecdh_shared_secret our_sk_bytes their_pub_bytes =
    match Mirage_crypto_ec.X25519.secret_of_octets our_sk_bytes with
    | Error _ -> Error "invalid X25519 secret key"
    | Ok (secret, _pub) ->
      (match Mirage_crypto_ec.X25519.key_exchange secret their_pub_bytes with
       | Error _ -> Error "X25519 key exchange failed"
       | Ok raw_shared ->
         Ok (Digestif.SHA256.(digest_string raw_shared |> to_raw_string)))

  let compute_stealth_tag shared_secret =
    let h = Digestif.SHA256.(digest_string (shared_secret ^ stealth_tag_domain) |> to_raw_string) in
    String.sub h 0 16

  let stealth_tag_to_hex tag =
    let buf = Buffer.create 32 in
    String.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) tag;
    Buffer.contents buf

  let stealth_tag_of_hex hex =
    let value = function
      | '0' .. '9' as ch -> Some (Char.code ch - Char.code '0')
      | 'a' .. 'f' as ch -> Some (Char.code ch - Char.code 'a' + 10)
      | 'A' .. 'F' as ch -> Some (Char.code ch - Char.code 'A' + 10)
      | _ -> None
    in
    let len = String.length hex in
    if len mod 2 <> 0 then None
    else
      let raw = Bytes.create (len / 2) in
      let rec decode index =
        if index = len then Some (Bytes.unsafe_to_string raw)
        else
          match value hex.[index], value hex.[index + 1] with
          | Some high, Some low ->
            Bytes.set raw (index / 2) (Char.chr ((high lsl 4) lor low));
            decode (index + 2)
          | _ -> None
      in
      decode 0

  let encrypt_stealth_amount shared_secret amount =
    let key = Mirage_crypto.AES.GCM.of_secret (String.sub shared_secret 0 32) in
    let nonce = Mirage_crypto_rng.generate 12 in
    let plaintext = Z.to_string amount in
    let ct_with_tag = Mirage_crypto.AES.GCM.authenticate_encrypt ~key ~nonce plaintext in
    Base64.encode_exn (nonce ^ ct_with_tag)

  let decrypt_stealth_amount shared_secret enc_b64 =
    try
      let raw = Base64.decode_exn enc_b64 in
      if String.length raw < 12 then Error "encrypted amount too short"
      else
        let nonce = String.sub raw 0 12 in
        let ct_with_tag = String.sub raw 12 (String.length raw - 12) in
        let key = Mirage_crypto.AES.GCM.of_secret (String.sub shared_secret 0 32) in
        match Mirage_crypto.AES.GCM.authenticate_decrypt ~key ~nonce ct_with_tag with
        | None -> Error "AES-GCM auth failed"
        | Some plain -> (try Ok (Z.of_string plain) with _ -> Error "invalid amount string")
    with e -> Error (Printexc.to_string e)

  let validator_amount_domain = "OCTRA_VALIDATOR_AMOUNT_V1"

  let validator_ecdh_shared validator_view_sk sender_eph_pub_b64 =
    let eph_pub = Base64.decode_exn sender_eph_pub_b64 in
    match ecdh_shared_secret validator_view_sk eph_pub with
    | Error e -> Error ("validator ECDH failed: " ^ e)
    | Ok raw_shared ->

      Ok (Digestif.SHA256.(digest_string (raw_shared ^ validator_amount_domain) |> to_raw_string))

  let decrypt_amount_for_validator validator_view_sk sender_eph_pub_b64 enc_amount_b64 =
    match validator_ecdh_shared validator_view_sk sender_eph_pub_b64 with
    | Error e -> Error e
    | Ok shared -> decrypt_stealth_amount shared enc_amount_b64

  let encrypt_amount_for_validator sender_eph_sk validator_view_pub_b64 amount =
    let view_pub = Base64.decode_exn validator_view_pub_b64 in
    match ecdh_shared_secret sender_eph_sk view_pub with
    | Error e -> Error ("sender ECDH failed: " ^ e)
    | Ok raw_shared ->
      let shared = Digestif.SHA256.(digest_string (raw_shared ^ validator_amount_domain) |> to_raw_string) in
      Ok (encrypt_stealth_amount shared amount)

  let claim_secret_domain = "OCTRA_CLAIM_SECRET_V1"
  let claim_bind_domain = "OCTRA_CLAIM_BIND_V1"

  let compute_claim_secret shared_secret =
    Digestif.SHA256.(digest_string (shared_secret ^ claim_secret_domain) |> to_raw_string)

  let compute_claim_pub claim_secret recipient_addr =
    Digestif.SHA256.(digest_string (claim_secret ^ recipient_addr ^ claim_bind_domain) |> to_raw_string)

  let verify_claim_secret ~claim_secret_hex ~claimer_addr ~stored_claim_pub_hex =
    if String.length claim_secret_hex <> 64 || String.length stored_claim_pub_hex <> 64 then false
    else
      match stealth_tag_of_hex claim_secret_hex,
            stealth_tag_of_hex stored_claim_pub_hex with
      | Some claim_secret, Some _ ->
        let computed = compute_claim_pub claim_secret claimer_addr in
        let computed_hex = stealth_tag_to_hex computed in
        String.equal
          (String.lowercase_ascii computed_hex)
          (String.lowercase_ascii stored_claim_pub_hex)
      | _ -> false
end

module PrivateTransferV3 = struct
  type t = {
    version : int;
    delta_cipher : string;
    commitment : string;
    eph_pub : string;
    stealth_tag : string;
    enc_amount : string;
    enc_amount_for_validator : string;
    amount : Z.t;
    claim_pub : string;
  }

  let to_json t =
    let base = [
      "version", `Int t.version;
      "delta_cipher", `String t.delta_cipher;
      "commitment", `String t.commitment;
      "eph_pub", `String t.eph_pub;
      "stealth_tag", `String t.stealth_tag;
      "enc_amount", `String t.enc_amount;
      "enc_amount_for_validator", `String t.enc_amount_for_validator;
    ] in

    let base = if Z.gt t.amount Z.zero
      then base @ ["amount", `String (Z.to_string t.amount)]
      else base in
    let fields = if String.length t.claim_pub > 0
      then base @ ["claim_pub", `String t.claim_pub]
      else base in
    `Assoc fields

  let of_json = function
    | `Assoc fields ->
      let get k = List.assoc_opt k fields in
      let int_k k = match get k with Some (`Int i) -> Ok i | _ -> Error ("missing " ^ k) in
      let str_k k = match get k with Some (`String s) -> Ok s | _ -> Error ("missing " ^ k) in
      let opt_str k = match get k with Some (`String s) -> s | _ -> "" in
      let opt_amount = function
        | Some (`String s) -> (try Z.of_string s with _ -> Z.zero)
        | Some (`Int n) -> Z.of_int n
        | _ -> Z.zero
      in
      (match int_k "version", str_k "delta_cipher", str_k "commitment",
             str_k "eph_pub", str_k "stealth_tag", str_k "enc_amount" with
       | Ok version, Ok delta_cipher, Ok commitment, Ok eph_pub, Ok stealth_tag, Ok enc_amount ->
         let enc_amount_for_validator = opt_str "enc_amount_for_validator" in
         let amount = opt_amount (get "amount") in
         let claim_pub = opt_str "claim_pub" in
         Ok { version; delta_cipher; commitment; eph_pub; stealth_tag; enc_amount;
              enc_amount_for_validator; amount; claim_pub }
       | Error e, _, _, _, _, _ | _, Error e, _, _, _, _
       | _, _, Error e, _, _, _ | _, _, _, Error e, _, _
       | _, _, _, _, Error e, _ | _, _, _, _, _, Error e ->
         Error ("PrivateTransferV3: " ^ e))
    | _ -> Error "PrivateTransferV3: expected object"
end

module StealthClaimData = struct
  type t = {
    output_id : int;
    claim_cipher : string;
    amount : Z.t;
    enc_amount_for_validator : string;
    commitment : string;
    claim_secret : string;
    eph_pub : string;
  }

  let to_json t =
    let base = [
      "output_id", `Int t.output_id;
      "claim_cipher", `String t.claim_cipher;
      "commitment", `String t.commitment;
    ] in

    let base = if Z.gt t.amount Z.zero
      then base @ ["amount", `String (Z.to_string t.amount)]
      else base in
    let base = if String.length t.enc_amount_for_validator > 0
      then base @ ["enc_amount_for_validator", `String t.enc_amount_for_validator]
      else base in
    let base = if String.length t.eph_pub > 0
      then base @ ["eph_pub", `String t.eph_pub]
      else base in
    let fields = if String.length t.claim_secret > 0
      then base @ ["claim_secret", `String t.claim_secret]
      else base in
    `Assoc fields

  let of_json = function
    | `Assoc fields ->
      let get k = List.assoc_opt k fields in
      let int_k k = match get k with Some (`Int i) -> Ok i | _ -> Error ("missing " ^ k) in
      let str_k k = match get k with Some (`String s) -> Ok s | _ -> Error ("missing " ^ k) in
      let opt_str k = match get k with Some (`String s) -> s | _ -> "" in
      let opt_amount = function
        | Some (`String s) -> (try Z.of_string s with _ -> Z.zero)
        | Some (`Int n) -> Z.of_int n
        | _ -> Z.zero
      in
      (match int_k "output_id", str_k "claim_cipher", str_k "commitment" with
       | Ok output_id, Ok claim_cipher, Ok commitment ->
         let amount = opt_amount (get "amount") in
         let enc_amount_for_validator = opt_str "enc_amount_for_validator" in
         let claim_secret = opt_str "claim_secret" in
         let eph_pub = opt_str "eph_pub" in
         Ok { output_id; claim_cipher; amount; enc_amount_for_validator;
              commitment; claim_secret; eph_pub }
       | Error e, _, _ | _, Error e, _ | _, _, Error e ->
         Error ("StealthClaimData: " ^ e))
    | _ -> Error "StealthClaimData: expected object"
end

module PrivateTransferV4 = struct
  type t = {
    version : int;
    delta_cipher : string;
    commitment : string;
    range_proof_delta : string;
    range_proof_balance : string;
    eph_pub : string;
    stealth_tag : string;
    enc_amount : string;
    claim_pub : string;
    amount_commitment : string;
    send_zero_proof : string;

  }

  let to_json t =
    `Assoc [
      "version", `Int t.version;
      "delta_cipher", `String t.delta_cipher;
      "commitment", `String t.commitment;
      "range_proof_delta", `String t.range_proof_delta;
      "range_proof_balance", `String t.range_proof_balance;
      "eph_pub", `String t.eph_pub;
      "stealth_tag", `String t.stealth_tag;
      "enc_amount", `String t.enc_amount;
      "claim_pub", `String t.claim_pub;
      "amount_commitment", `String t.amount_commitment;
      "send_zero_proof", `String t.send_zero_proof;
    ]

  let of_json = function
    | `Assoc fields ->
      let get k = List.assoc_opt k fields in
      let int_k k = match get k with Some (`Int i) -> Ok i | _ -> Error ("missing " ^ k) in
      let str_k k = match get k with Some (`String s) -> Ok s | _ -> Error ("missing " ^ k) in
      let str_opt k = match get k with Some (`String s) -> s | _ -> "" in
      (match int_k "version", str_k "delta_cipher", str_k "commitment",
             str_k "range_proof_delta", str_k "range_proof_balance",
             str_k "eph_pub", str_k "stealth_tag",
             str_k "enc_amount", str_k "claim_pub",
             str_k "amount_commitment" with
       | Ok version, Ok delta_cipher, Ok commitment,
         Ok range_proof_delta, Ok range_proof_balance,
         Ok eph_pub, Ok stealth_tag, Ok enc_amount, Ok claim_pub,
         Ok amount_commitment ->
         let send_zero_proof = str_opt "send_zero_proof" in
         Ok { version; delta_cipher; commitment;
              range_proof_delta; range_proof_balance;
              eph_pub; stealth_tag; enc_amount; claim_pub;
              amount_commitment; send_zero_proof }
       | Error e, _, _, _, _, _, _, _, _, _
       | _, Error e, _, _, _, _, _, _, _, _
       | _, _, Error e, _, _, _, _, _, _, _
       | _, _, _, Error e, _, _, _, _, _, _
       | _, _, _, _, Error e, _, _, _, _, _
       | _, _, _, _, _, Error e, _, _, _, _
       | _, _, _, _, _, _, Error e, _, _, _
       | _, _, _, _, _, _, _, Error e, _, _
       | _, _, _, _, _, _, _, _, Error e, _
       | _, _, _, _, _, _, _, _, _, Error e ->
         Error ("PrivateTransferV4: " ^ e))
    | _ -> Error "PrivateTransferV4: expected object"
end

module StealthClaimV5 = struct
  type t = {
    version : int;
    output_id : int;
    claim_cipher : string;
    commitment : string;
    claim_secret : string;
    zero_proof : string;
  }

  let to_json t =
    `Assoc [
      "version", `Int t.version;
      "output_id", `Int t.output_id;
      "claim_cipher", `String t.claim_cipher;
      "commitment", `String t.commitment;
      "claim_secret", `String t.claim_secret;
      "zero_proof", `String t.zero_proof;
    ]

  let of_json = function
    | `Assoc fields ->
      let get k = List.assoc_opt k fields in
      let int_k k = match get k with Some (`Int i) -> Ok i | _ -> Error ("missing " ^ k) in
      let str_k k = match get k with Some (`String s) -> Ok s | _ -> Error ("missing " ^ k) in
      (match int_k "version", int_k "output_id", str_k "claim_cipher",
             str_k "commitment", str_k "claim_secret", str_k "zero_proof" with
       | Ok version, Ok output_id, Ok claim_cipher, Ok commitment,
         Ok claim_secret, Ok zero_proof ->
         Ok { version; output_id; claim_cipher; commitment; claim_secret;
              zero_proof }
       | Error e, _, _, _, _, _ | _, Error e, _, _, _, _
       | _, _, Error e, _, _, _ | _, _, _, Error e, _, _
       | _, _, _, _, Error e, _ | _, _, _, _, _, Error e ->
         Error ("StealthClaimV5: " ^ e))
    | _ -> Error "StealthClaimV5: expected object"
end

let is_octra_address s =
  String.length s = 47 && String.sub s 0 3 = "oct"

module Wallet = struct
  type t = { priv : string; pub : string; address : string }

  let ensure path =
    if Sys.file_exists path then
      let json = Yojson.Safe.from_file path in
      let open Yojson.Safe.Util in
      {
        priv = json |> member "priv" |> to_string;
        pub = json |> member "pub" |> to_string;
        address = json |> member "address" |> to_string;
      }
    else
      let rec gen_loop n =
        if n <= 0 then failwith "failed to generate valid address in 100 attempts"
        else
          let sk, pk = Mirage_crypto_ec.Ed25519.generate () in
          let priv_s = Base64.encode_exn (Mirage_crypto_ec.Ed25519.priv_to_octets sk) in
          let pub_s = Base64.encode_exn (Mirage_crypto_ec.Ed25519.pub_to_octets pk) in
          let address = Address.address_from_pubkey pub_s in
          if is_octra_address address then begin
            let json = `Assoc [
                "priv", `String priv_s;
                "pub", `String pub_s;
                "address", `String address
              ] in
            Yojson.Safe.to_file path json;
            { priv = priv_s; pub = pub_s; address }
          end else gen_loop (n - 1)
      in
      gen_loop 100
end