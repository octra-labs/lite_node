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


type relay_identity = {
  relay_pub_b64 : string;
  relay_pk : Mirage_crypto_ec.Ed25519.pub;
}

let relay_identity ledger relay_id =
  if not (Octra_core.Crypto.Address.is_valid_address relay_id) then
    Error ("invalid_circle_relay_id", "relay_id must be a valid octra address")
  else
    match Octra_core.Ledger.get_public_key ledger relay_id with
    | None ->
      Error ("circle_relay_pubkey_missing", "relay_id has no registered public key")
    | Some relay_pub_b64 ->
      if not (Octra_core.Crypto.Address.verify_address_pubkey relay_id relay_pub_b64) then
        Error ("invalid_circle_relay_id", "relay public key does not match relay_id")
      else
        match Base64.decode relay_pub_b64 with
        | Error _ ->
          Error ("circle_relay_pubkey_invalid", "relay public key is not valid base64")
        | Ok relay_pub_raw ->
          match Mirage_crypto_ec.Ed25519.pub_of_octets relay_pub_raw with
          | Error _ ->
            Error ("circle_relay_pubkey_invalid", "relay public key is not a valid ed25519 key")
          | Ok relay_pk ->
            Ok {
              relay_pub_b64;
              relay_pk;
            }

let verify_signature relay_pk signature subject =
  match Base64.decode signature with
  | Error _ ->
    Error ("circle_relay_signature_invalid", "relay signature is not valid base64")
  | Ok relay_sig ->
    if Mirage_crypto_ec.Ed25519.verify ~key:relay_pk ~msg:subject relay_sig then
      Ok ()
    else
      Error ("circle_relay_signature_invalid", "relay signature verification failed")

let verify_claim_signature ledger circle_id (claim : Octra_core.Circles.relay_claim) =
  match relay_identity ledger claim.relay_id with
  | Error _ as e -> e
  | Ok relay ->
    Circle_transport_subject.relay_claim_subject ~circle_id ~claim
    |> verify_signature relay.relay_pk claim.signature

let verify_ingress_signature ledger circle_id (payload : Octra_core.Circles.ingress_commit_payload) =
  match relay_identity ledger payload.relay_id with
  | Error _ as e -> e
  | Ok relay ->
    Circle_transport_subject.ingress_payload_subject ~circle_id payload
    |> verify_signature relay.relay_pk payload.signature