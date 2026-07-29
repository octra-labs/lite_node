(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type t

type error

val error_message : error -> string
val empty : t
val of_env : (string -> string option) -> (t, error) result
val keys : t -> Program_attestation.key list
val fingerprint : t -> string
val config_hash : t -> string option