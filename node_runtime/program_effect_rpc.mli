(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val save_abi :
  save_abi:(string -> string -> unit Lwt.t) ->
  Yojson.Safe.t ->
  rpc_result Lwt.t