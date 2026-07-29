(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val max_ciphertext_encoded_bytes : int
val max_proof_encoded_bytes : int
val max_proof_raw_bytes : int
val max_base_layers : int
val max_layers : int
val max_edges : int

val ciphertext_allowed : string -> bool
val proof_allowed : string -> bool
val shape_allowed : Pvac_ffi.cipher_shape -> bool