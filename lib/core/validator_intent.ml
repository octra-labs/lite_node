(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type identity = {
  chain_id : string;
  address : string;
  pubkey : string;
  bonded_epoch : int64;
}

type t = { identity : identity; signature : string }

let message identity =
  Octra_net.Hash_domain.hash_encoded "octra:validator_exit_intent" (fun buffer ->
    Octra_net.Oce1.put_string buffer identity.chain_id;
    Octra_net.Oce1.put_string buffer identity.address;
    Octra_net.Oce1.put_string buffer identity.pubkey;
    Octra_net.Oce1.put_u64 buffer identity.bonded_epoch)

let id value =
  Digestif.SHA256.digest_string (message value.identity) |> Digestif.SHA256.to_hex

let public_key identity =
  match Base64.decode identity.pubkey with
  | Ok raw when String.length raw = 32
                && Base64.encode_exn raw = identity.pubkey
                && identity.chain_id <> ""
                && String.length identity.chain_id <= 128
                && identity.bonded_epoch >= 0L
                && Crypto.Address.verify_address_pubkey identity.address identity.pubkey ->
    Ok raw
  | _ -> Error "invalid validator exit identity"

let create identity ~privkey =
  let ( let* ) = Result.bind in
  let* public = public_key identity in
  match Base64.decode privkey with
  | Error _ -> Error "invalid validator exit signing key"
  | Ok raw ->
    match Mirage_crypto_ec.Ed25519.priv_of_octets raw with
    | Error _ -> Error "invalid validator exit signing key"
    | Ok key ->
      let actual =
        Mirage_crypto_ec.Ed25519.pub_of_priv key
        |> Mirage_crypto_ec.Ed25519.pub_to_octets
      in
      if actual <> public then Error "validator exit signing key differs"
      else
        Ok { identity; signature = Mirage_crypto_ec.Ed25519.sign ~key (message identity) }

let encode value =
  let identity = value.identity in
  `Assoc [
    "standard", `String "octra-validator-exit-intent";
    "chain_id", `String identity.chain_id;
    "address", `String identity.address;
    "pubkey", `String identity.pubkey;
    "bonded_epoch", `String (Int64.to_string identity.bonded_epoch);
    "signature", `String (Base64.encode_exn value.signature);
  ] |> Yojson.Safe.to_string

let decode raw =
  let ( let* ) = Result.bind in
  if String.length raw > 4_096 then Error "validator exit intent exceeds limit"
  else
    try
      match Yojson.Safe.from_string raw with
      | `Assoc fields when List.length fields = 6 ->
        let names = List.map fst fields |> List.sort_uniq String.compare in
        if names <> ["address"; "bonded_epoch"; "chain_id"; "pubkey"; "signature"; "standard"]
        then Error "validator exit intent fields differ"
        else
          let get name =
            match List.assoc name fields with
            | `String value -> value
            | _ -> failwith "field type"
          in
          let epoch = get "bonded_epoch" in
          let bonded_epoch = Int64.of_string epoch in
          if get "standard" <> "octra-validator-exit-intent"
             || Int64.to_string bonded_epoch <> epoch then
            Error "invalid validator exit intent format"
          else
            let identity = {
              chain_id = get "chain_id";
              address = get "address";
              pubkey = get "pubkey";
              bonded_epoch;
            } in
            let* public = public_key identity in
            let encoded = get "signature" in
            begin
            match Base64.decode encoded with
            | Ok signature when String.length signature = 64
                                && Base64.encode_exn signature = encoded
                                && Octra_consensus.C_hash.verify_ed25519
                                     ~pubkey_raw:public ~msg:(message identity) ~signature ->
              Ok { identity; signature }
            | _ -> Error "invalid validator exit intent signature"
            end
      | _ -> Error "invalid validator exit intent object"
    with _ -> Error "invalid validator exit intent encoding"

let applies expected value =
  let actual = value.identity in
  if actual.chain_id <> expected.chain_id || actual.address <> expected.address
     || actual.pubkey <> expected.pubkey then
    Error "validator exit intent identity differs"
  else if actual.bonded_epoch > expected.bonded_epoch then
    Error "validator exit intent registration is ahead"
  else Ok (actual.bonded_epoch = expected.bonded_epoch)