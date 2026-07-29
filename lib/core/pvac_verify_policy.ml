(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let max_ciphertext_encoded_bytes = 96 * 1024
let max_proof_encoded_bytes = 64 * 1024
let max_proof_raw_bytes = (max_proof_encoded_bytes / 4) * 3
let max_base_layers = 2
let max_layers = 8
let max_edges = 512

let encoded_allowed limit value =
  String.length value > 0 && String.length value <= limit

let ciphertext_allowed value =
  encoded_allowed max_ciphertext_encoded_bytes value

let proof_allowed value =
  encoded_allowed max_proof_encoded_bytes value

let shape_allowed shape =
  shape.Pvac_ffi.slots = 1
  && shape.c0 = 1
  && shape.layers > 0
  && shape.layers <= max_layers
  && shape.edges <= max_edges
  && shape.base_layers > 0
  && shape.base_layers <= max_base_layers