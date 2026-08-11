(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val max_single_bytes : int
val max_batch_bytes : int

val single :
  Octra_core.Rpc.response ->
  Yojson.Safe.t

val batch_item :
  count:int ->
  Octra_core.Rpc.response ->
  Yojson.Safe.t

val response_bytes :
  Yojson.Safe.t ->
  int