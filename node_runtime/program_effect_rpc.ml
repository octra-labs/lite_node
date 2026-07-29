(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

module Rpc = Octra_core.Rpc

type rpc_result = (Yojson.Safe.t, Rpc.rpc_error) result

let save_abi ~save_abi params =
  match Rpc.require_address params 0 "address" with
  | Error e ->
    Lwt.return (Error e)
  | Ok addr ->
    match Rpc.require_string params 1 "abi" with
    | Error e ->
      Lwt.return (Error e)
    | Ok abi ->
      let open Lwt.Syntax in
      let* () = save_abi addr abi in
      Lwt.return (Ok Rpc_view.program_abi_saved)