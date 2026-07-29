(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val max_request_bytes : int
val max_pubkey_encoded_bytes : int
val max_pubkey_raw_bytes : int
val max_seckey_encoded_bytes : int
val max_seckey_raw_bytes : int
val max_ciphertext_encoded_bytes : int
val max_verifier_ciphertext_encoded_bytes : int
val max_proof_encoded_bytes : int
val max_commitment_encoded_bytes : int
val max_seed_encoded_bytes : int
val max_base_layers : int
val max_layers : int
val max_edges : int

val request_allowed : string -> bool
val pubkey_encoded_allowed : string -> bool
val pubkey_raw_allowed : string -> bool
val seckey_encoded_allowed : string -> bool
val seckey_raw_allowed : string -> bool
val ciphertext_allowed : string -> bool
val verifier_ciphertext_allowed : string -> bool
val proof_allowed : string -> bool
val commitment_allowed : string -> bool
val seed_allowed : string -> bool
val verifier_shape_allowed : Pvac_ffi.cipher_shape -> bool