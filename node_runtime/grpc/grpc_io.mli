(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

val serve :
  config:H2.Config.t ->
  request_handler:H2.Server_connection.request_handler ->
  error_handler:H2.Server_connection.error_handler ->
  advance:(unit -> unit) ->
  Lwt_unix.file_descr ->
  unit Lwt.t