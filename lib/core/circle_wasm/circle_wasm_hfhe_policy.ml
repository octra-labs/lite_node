(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let max_request_bytes = 16 * 1024 * 1024
let max_pubkey_encoded_bytes = 7 * 1024 * 1024
let max_pubkey_raw_bytes = 5_000_000
let max_seckey_encoded_bytes = 2048
let max_seckey_raw_bytes = 1024
let max_ciphertext_encoded_bytes = 1024 * 1024
let max_verifier_ciphertext_encoded_bytes =
  Pvac_verify_policy.max_ciphertext_encoded_bytes
let max_proof_encoded_bytes = Pvac_verify_policy.max_proof_encoded_bytes
let max_commitment_encoded_bytes = 64
let max_seed_encoded_bytes = 64
let max_base_layers = Pvac_verify_policy.max_base_layers
let max_layers = Pvac_verify_policy.max_layers
let max_edges = Pvac_verify_policy.max_edges

let encoded_size_allowed ~max_bytes value =
  String.length value > 0 && String.length value <= max_bytes

let raw_size_allowed ~max_bytes value =
  String.length value > 0 && String.length value <= max_bytes

let request_allowed value =
  encoded_size_allowed ~max_bytes:max_request_bytes value

let pubkey_encoded_allowed value =
  encoded_size_allowed ~max_bytes:max_pubkey_encoded_bytes value

let pubkey_raw_allowed value =
  raw_size_allowed ~max_bytes:max_pubkey_raw_bytes value

let seckey_encoded_allowed value =
  encoded_size_allowed ~max_bytes:max_seckey_encoded_bytes value

let seckey_raw_allowed value =
  raw_size_allowed ~max_bytes:max_seckey_raw_bytes value

let ciphertext_allowed value =
  encoded_size_allowed ~max_bytes:max_ciphertext_encoded_bytes value

let verifier_ciphertext_allowed value =
  Pvac_verify_policy.ciphertext_allowed value

let proof_allowed value =
  Pvac_verify_policy.proof_allowed value

let commitment_allowed value =
  encoded_size_allowed ~max_bytes:max_commitment_encoded_bytes value

let seed_allowed value =
  encoded_size_allowed ~max_bytes:max_seed_encoded_bytes value

let verifier_shape_allowed shape =
  Pvac_verify_policy.shape_allowed shape