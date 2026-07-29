(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let handshake_message = "OCTRA_HANDSHAKE"

let sign msg priv_b64 =
  match Mirage_crypto_ec.Ed25519.priv_of_octets (Base64.decode_exn priv_b64) with
  | Ok sk -> Base64.encode_exn (Mirage_crypto_ec.Ed25519.sign ~key:sk msg)
  | Error _ -> failwith "Invalid private key"

let verify msg signature_b64 pub_b64 =
  try
    Octra_ed25519.verify
      ~pub:(Base64.decode_exn pub_b64)
      ~msg
      (Base64.decode_exn signature_b64)
  with _ -> false