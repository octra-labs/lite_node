(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

external groth16_verify_bn254 : bytes -> bytes -> bytes -> bool
  = "caml_zk_groth16_verify_bn254"