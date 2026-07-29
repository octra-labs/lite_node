(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type relay_identity = {
  relay_pub_b64 : string;
  relay_pub_raw : string;
}

let relay_identity ledger relay_id =
  if not (Crypto.Address.is_valid_address relay_id) then
    Error ("invalid_circle_relay_id", "relay_id must be a valid octra address")
  else
    match Ledger.get_public_key ledger relay_id with
    | None ->
      Error ("circle_relay_pubkey_missing", "relay_id has no registered public key")
    | Some relay_pub_b64 ->
      if not (Crypto.Address.verify_address_pubkey relay_id relay_pub_b64) then
        Error ("invalid_circle_relay_id", "relay public key does not match relay_id")
      else
        match Base64.decode relay_pub_b64 with
        | Error _ ->
          Error ("circle_relay_pubkey_invalid", "relay public key is not valid base64")
        | Ok relay_pub_raw ->
          if not (Octra_ed25519.safe relay_pub_raw) then
            Error ("circle_relay_pubkey_invalid", "relay public key is not a valid ed25519 key")
          else
            Ok {
              relay_pub_b64;
              relay_pub_raw;
            }

let verify_signature relay_pub_raw signature subject =
  match Base64.decode signature with
  | Error _ ->
    Error ("circle_relay_signature_invalid", "relay signature is not valid base64")
  | Ok relay_sig ->
    if Octra_ed25519.verify ~pub:relay_pub_raw ~msg:subject relay_sig then
      Ok ()
    else
      Error ("circle_relay_signature_invalid", "relay signature verification failed")

let verify_claim_signature ledger circle_id (claim : Circles.relay_claim) =
  match relay_identity ledger claim.relay_id with
  | Error _ as e -> e
  | Ok relay ->
    Circle_transport_subject.relay_claim_subject ~circle_id ~claim
    |> verify_signature relay.relay_pub_raw claim.signature

let verify_cancel_signature ledger circle_id (cancel : Circles.relay_cancel) =
  match relay_identity ledger cancel.relay_id with
  | Error _ as e -> e
  | Ok relay ->
    Circle_transport_subject.relay_cancel_subject ~circle_id ~cancel
    |> verify_signature relay.relay_pub_raw cancel.signature

let verify_ingress_signature ledger circle_id (payload : Circles.ingress_commit_payload) =
  match relay_identity ledger payload.relay_id with
  | Error _ as e -> e
  | Ok relay ->
    Circle_transport_subject.ingress_payload_subject ~circle_id payload
    |> verify_signature relay.relay_pub_raw payload.signature