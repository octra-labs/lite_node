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


let handshake_message = "OCTRA_HANDSHAKE"

let sign msg priv_b64 =
  match Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn priv_b64) with
  | Ok sk -> Base64.encode_exn (Mirage_crypto_ec.Ed25519.sign ~key:sk msg)
  | Error _ -> failwith "Invalid private key"

let verify msg signature_b64 pub_b64 =
  match Mirage_crypto_ec.Ed25519.pub_of_octets (Base64.decode_exn pub_b64) with
  | Ok pk -> Mirage_crypto_ec.Ed25519.verify ~key:pk ~msg (Base64.decode_exn signature_b64)
  | Error _ -> false