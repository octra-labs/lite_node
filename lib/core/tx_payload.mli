(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type limits = {
  encrypted_data_len : int;
  message_len : int;
  message_zkp_len : int;
  message_validator_len : int;
  message_program_len : int;
}

val standard : limits
val message_limit : limits -> Transaction.t -> int
val encrypted_data_limit : limits -> Transaction.t -> int
val size_ok : limits:limits -> Transaction.t -> bool
val admit : ?limits:limits -> Transaction.t -> (unit, string) result
val decode :
  ?limits:limits ->
  Yojson.Safe.t ->
  (Transaction.t, string) result