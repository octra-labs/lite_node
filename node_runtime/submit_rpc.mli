(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

type validate =
  Octra_core.Transaction.t ->
  (string, string * string) result

val submit :
  validate:validate ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val submit_batch :
  validate:validate ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val staging_remove :
  find_tx:(string -> Octra_core.Transaction.t option) ->
  remove_tx:(string -> bool) ->
  notify:(unit -> unit) ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val private_transfer :
  validate:validate ->
  Yojson.Safe.t ->
  rpc_result Lwt.t