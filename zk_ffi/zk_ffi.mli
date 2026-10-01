(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

exception Backend_unavailable of string

val groth16_verify_bn254 : bytes -> bytes -> bytes -> bool
val initialize : unit -> unit