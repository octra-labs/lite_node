(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type rpc_result = (Yojson.Safe.t, Octra_core.Rpc.rpc_error) result

val public_key :
  bft_mode:bool ->
  register:(string -> string -> unit) ->
  Yojson.Safe.t ->
  rpc_result Lwt.t

val pvac_pubkey :
  existing:(string -> string option Lwt.t) ->
  kat_mismatch:(addr:string -> got:string -> expected:string -> unit) ->
  Yojson.Safe.t ->
  rpc_result Lwt.t