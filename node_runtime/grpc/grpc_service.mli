(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

type call =
  Rpc_http.meta ->
  Octra_core.Rpc.request ->
  Octra_core.Rpc.response Lwt.t

type reply = {
  body : string option;
  status : Grpc_status.t;
  rpc_method : string option;
}

val paths : string list
val submit_path : string

val reply :
  ?body:string ->
  ?rpc_method:string ->
  Grpc_status.t ->
  reply

val invoke :
  ?submit:call ->
  call:call ->
  meta:Rpc_http.meta ->
  path:string ->
  bytes ->
  reply Lwt.t