(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val canonical :
  C_types.reward_source ->
  C_types.reward_source

val validate :
  C_types.reward_source ->
  (C_types.reward_source, string) result

val encode_into :
  Buffer.t ->
  C_types.reward_source ->
  unit

val decode_from :
  Octra_net.Oce1.cursor ->
  C_types.reward_source

val to_yojson :
  C_types.reward_source ->
  Yojson.Safe.t

val of_yojson :
  Yojson.Safe.t ->
  (C_types.reward_source, string) result